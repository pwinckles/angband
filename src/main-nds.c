/**
 * \file main-nds.c
 * \brief Main file for playing on the Nintendo DS
 *
 * Copyright (c) 2010 Nick McConnell
 *
 * Many of the routines are based on (or lifted directly from) brettk's
 * excellent NethackDS: http://frodo.dyn.gno.org/~brettk/NetHackDS
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include <nds.h>
#include <fat.h>

// for uid hack move later
#include <unistd.h>
#include <pwd.h>

#include "angband.h"
#include "buildid.h"
#include "main.h"
#include "init.h"
#include "ui-input.h"
#include "ui-game.h"
#include "ui-prefs.h"
#include "ui-term.h"
#include "savefile.h"

/* DS includes */
#include "nds/nds_debug.h"
#include "nds/nds_errfont.h"
#include "nds/nds_main.h"
#include "nds/nds_gfx.h"
#include "nds/nds_io.h"
#include "nds/nds_win.h"

int console_enabled = 0;
int was_console_layer_visible = 0;
int lid_closed = 0;
int power_state = 0;
int have_error = 0;

// TODO the following functions might be better in a different file

void nds_show_debug_console() {
  was_console_layer_visible = REG_DISPCNT & DISPLAY_BG0_ACTIVE;

  REG_DISPCNT_SUB |= DISPLAY_BG0_ACTIVE;

  console_enabled = 1;
}

void nds_hide_debug_console() {
  if (! was_console_layer_visible) {
    REG_DISPCNT_SUB ^= DISPLAY_BG0_ACTIVE;
  }

  console_enabled = 0;
}

/*
 * Key interrupt handler.  Right now, I only use this to toggle the console
 * layer on and off. Triggered by pressing START + SELECT.
 */
void keys_interrupt_handler() {
  lid_closed = keysCurrent() & KEY_LID;

  if (! console_enabled) {
    nds_show_debug_console();
  } else {
    nds_hide_debug_console();
  }
}

/*
 * Right now, the main thing we do here is check for the lid state, so we
 * can power on/off as appropriate.
 */
void vsync_handler() {
  switch (power_state) {
  case POWER_STATE_ON:
    if (lid_closed) {
      systemSleep();
      power_state = POWER_STATE_ASLEEP;
    }

    break;

  case POWER_STATE_ASLEEP:
    if (! lid_closed) {
      power_state = POWER_STATE_ON;
    }

    break;

  default:
    power_state = POWER_STATE_ON;

    break;
  }
}

void nds_error() {
  have_error = 1;
}

/*
 * Registers the following interrupt handlers:
 *
 * - START + SELECT: Toggle debug console
 * - LID: Toggle sleep mode
 */
void enable_interrupts() {
  irqEnable(IRQ_KEYS);

  irqSet(IRQ_VBLANK, vsync_handler);
  irqSet(IRQ_KEYS, keys_interrupt_handler);

  REG_KEYCNT |= 0x8000 | 0x4000 | KEY_SELECT | KEY_START | KEY_LID;
}









u16* subfont_rgb_bin = (u16*)(0x06018400);
u16* subfont_bgr_bin = (u16*)(0x0601C400);
u16* top_font_bin;
u16* btm_font_bin;
u16* tiles_bin;/* = (u16*)0x06020400; */

#define NDS_MAPPABLE_MASK	(KEY_A | KEY_B | KEY_X | KEY_Y | KEY_START | KEY_SELECT)
#define NDS_MODIFIER_MASK	(KEY_L | KEY_R)
#define NDS_BUTTON_MASK		(NDS_MAPPABLE_MASK | NDS_MODIFIER_MASK)
#define NDS_NUM_MAPPABLE	6      /* A, B, X, Y, Select, Start */
#define NDS_NUM_MODIFIER	2      /* R, L */
#define NDS_CMD_LENGTH		16     /* max. 15 keys/button + null terminator */

/*[mappable]*2^[mods] things to map commands to, [cmd_length] chars per command */
byte nds_btn_cmds[NDS_NUM_MAPPABLE << NDS_NUM_MODIFIER][NDS_CMD_LENGTH];

/* make sure there's something there to start with */
byte btn_defaults[] = {
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
  'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'z'};

const s16 mappables[] = { KEY_A, KEY_B, KEY_X, KEY_Y, KEY_SELECT, KEY_START };
const s16 modifiers[] = { KEY_L, KEY_R };

s16 nds_buttons_to_btnid(u16 kd, u16 kh) {
	if (!(kd & NDS_MAPPABLE_MASK)) return -1;
	u16 i, mods = 0;
	for (i=0;i<NDS_NUM_MODIFIER;i++) {
		if (kh & modifiers[i]) mods |= (1 << i);
	}
	for (i=0;i<NDS_NUM_MAPPABLE;i++) {
		if (kd & mappables[i]) return i + NDS_NUM_MAPPABLE * (mods);
	}
	return -1;
}

#define total_tiles_used 512	/*hack, guess  */

#define DEF_TILES_PER_ROW       32

/* don't change these */
#define TILE_WIDTH 3
#define TILE_HEIGHT 8
#define c1(a,i)		(RGB15((a[i]>>3),(a[i+1]>>3),(a[i+2]>>3)))
#define c2(a,i)		(RGB15((a[i+2]>>3),(a[i+1]>>3),(a[i]>>3)))
#define TILE_BUFFER_SIZE		(TILE_WIDTH*TILE_HEIGHT*(total_tiles_used+1)*2)

/*
 * Extra data to associate with each "window"
 *
 * Each "window" is represented by a "term_data" structure, which
 * contains a "term" structure, which contains a pointer (t->data)
 * back to the term_data structure.
 */

typedef struct term_data term_data;

struct term_data {
  term t;
  
  byte rows;
  byte cols;
  
  int tile_height;
  int tile_width; 
};



/*
 * Number of "term_data" structures to support XXX XXX XXX
 *
 * You MUST support at least one "term_data" structure, and the
 * game will currently use up to eight "term_data" structures if
 * they are available.
 *
 * If only one "term_data" structure is supported, then a lot of
 * the things that would normally go into a "term_data" structure
 * could be made into global variables instead.
 */
#define MAX_TERM_DATA 1


/*
 * An array of "term_data" structures, one for each "sub-window"
 */
static term_data data[MAX_TERM_DATA];

/*
 * Colour data
 */

u16b color_data[] = {
	RGB15(  0,  0,  0), 		/* COLOUR_DARK */
	RGB15( 31, 31, 31), 		/* COLOUR_WHITE */
	RGB15( 15, 15, 15), 		/* COLOUR_SLATE */
	RGB15( 31, 15,  0),		/* COLOUR_ORANGE */ 
	RGB15( 23,  0,  0), 		/* COLOUR_RED */
	RGB15(  0, 15,  9), 		/* COLOUR_GREEN */
	RGB15(  0,  0, 31), 		/* COLOUR_BLUE */
	RGB15( 15,  9,  0), 		/* COLOUR_UMBER */
	RGB15(  9,  9,  9), 		/* COLOUR_L_DARK */
	RGB15( 23, 23, 23), 		/* COLOUR_L_WHITE */
	RGB15( 31,  0, 31), 		/* COLOUR_VIOLET */
	RGB15( 31, 31,  0), 		/* COLOUR_YELLOW */
	RGB15( 31,  0,  0), 		/* COLOUR_L_RED */
	RGB15(  0, 31,  0), 		/* COLOUR_L_GREEN */
	RGB15(  0, 31, 31), 		/* COLOUR_L_BLUE */
	RGB15( 23, 15,  9)		/* COLOUR_L_UMBER */
	/* TODO fill in more colors z-color.c */
};



/*** Function hooks needed by "Term" ***/


/*
 * Init a new "term"
 *
 * This function should do whatever is necessary to prepare a new "term"
 * for use by the "term.c" package.  This may include clearing the window,
 * preparing the cursor, setting the font/colors, etc.  Usually, this
 * function does nothing, and the "init_xxx()" function does it all.
 */
static void Term_init_nds(term *t) {
	term_data *td = (term_data*)(t->data);

	/* XXX XXX XXX */
}



/*
 * Nuke an old "term"
 *
 * This function is called when an old "term" is no longer needed.  It should
 * do whatever is needed to clean up before the program exits, such as wiping
 * the screen, restoring the cursor, fixing the font, etc.  Often this function
 * does nothing and lets the operating system clean up when the program quits.
 */
static void Term_nuke_nds(term *t) {
	term_data *td = (term_data*)(t->data);

	/* XXX XXX XXX */
}

void nds_check_buttons(u16b kd, u16b kh) {
  s16b btn = nds_buttons_to_btnid(kd,kh);
  if (btn == -1) return;
  byte *cmd = &nds_btn_cmds[btn][0];
  while (*cmd != 0) {
    put_key_event(*(cmd++));
  }
}

/*
 * All event handling 
 */
u16b *ebuf = (u16b*)(&BG_GFX[256*192]);
/* store the queue just past mainscreen display data */
u16b ebuf_read = 0, ebuf_write = 0;
byte nds_updated = 0;	/* windows that have been updated and should be redrawn */

bool has_event() {
  return ((ebuf[ebuf_read] & EVENT_SET) || (ebuf_read < ebuf_write));
  /* read < write should never happen without EVENT_SET, but */
  /* just in case... */
}

u16b get_event() {
  if (!has_event()) return 0;
  u16b r = ebuf[ebuf_read];
  ebuf[ebuf_read] = 0;
  ebuf_read++;
  if (ebuf_read > ebuf_write) {
    ebuf_write++;
    if (ebuf_write >= MAX_EBUF) ebuf_write = 0;
  }
  if (ebuf_read >= MAX_EBUF) ebuf_read = 0;
  return r;
}

void put_key_event(byte c) {
  ebuf[ebuf_write++] = EVENT_SET | (u16)c;
  if (ebuf_write >= MAX_EBUF) ebuf_write = 0;
}

void put_mouse_event(byte x, byte y) {
  ebuf[ebuf_write++] = EVENT_SET | MEVENT_FLAG | (u16b)x | (((u16b)y) << 7);
  if (ebuf_write >= MAX_EBUF) ebuf_write = 0;
}

void do_vblank() {
  swiWaitForVBlank();
  
  /* --------------------------- */
  /*  Handle the arrow buttons */
  scanKeys();
  u32b kd = keysDown();
  u32b kh = keysHeld();
  /* order of keys: Right, Left, Up, Down */
  /* map keys to dirs, depends on order of keys in nds/input.h */
  /*  and order of directions in ndir & sdir in decl.c */
  /*const s8 k2d[] = {	// indexes into ndir/sdir, 10 = end of string = '\0' */
  /* 10, 4, 0, 10, 2, 3, 1, 10, 6, 5, 7	// no working combinations >= 11 */
  /*}; */
  const byte k2d[] = {'6','4','8','2','3','7','9','1'  };
  /* only do stuff if a key was pressed last frame */
  if (kd & (KEY_RIGHT | KEY_LEFT | KEY_UP | KEY_DOWN)) {
    u16b dirs_down = 0;
    int i;
    if (kh & KEY_LEFT) dirs_down++;
    if (kh & KEY_RIGHT) dirs_down++;
    if (kh & KEY_UP) dirs_down++;
    if (kh & KEY_DOWN) dirs_down++;
    if (dirs_down == 1 && !(kh & (KEY_R | KEY_L))) {
      for (i = 0; i < 4; i++) {
        if (kh & (1 << (i + 4))) {
          put_key_event(k2d[i]);
        }
      }
    } else if (dirs_down == 2 && (kh & (KEY_R | KEY_L))) {
      for (i = 0; i < 4; i++) {
        if (kh & (1 << (i + 4))) {
          put_key_event(k2d[i + 4]);
        }
      }
    }
  }
  
  /* --------------------------- */
  /*  Check for button macros */
  nds_check_buttons(kd, kh);
  
  /* --------------------------- */
  /*  Check for typing on the touchscreen kbd */
  // TODO code of checking keyboard
}

/*
 * An event handler XXX XXX XXX
 *
 * You may need an event handler, which can be used by both
 * by the "TERM_XTRA_BORED" and "TERM_XTRA_EVENT" entries in
 * the "Term_xtra_xxx()" function, and also to wait for the
 * user to perform whatever user-interface operation is needed
 * to request the start of a new game or the loading of an old
 * game, both of which should launch the "play_game()" function.
 */
static errr CheckEvents(bool wait) {
  u16b e = 0;

  do_vblank();

  if (!wait && !has_event()) return (1);

  while (!e) {
    // TODO need to get touch events in here
    e = get_event();

    do_vblank();
  }

  /* Mouse */
  if (IS_MEVENT(e)) {
    // TODO handle keyboard
//    handle_touch(EVENT_X(e) + 1, EVENT_Y(e), 1, true);
  } else if ((EVENT_C(e) & 0x7F) == 0) {
    /* Undefined */
    return (1);
  } else {
    /* Key */
    INFO_PRINT("Change:%c", EVENT_C(e));
    Term_keypress('\n', 0);
//    Term_keypress(EVENT_C(e), 0);
  }

  return (0);
}


/*
 * Do a "special thing" to the current "term"
 *
 * This function must react to a large number of possible arguments, each
 * corresponding to a different "action request" by the "ui-term.c" package,
 * or by the application itself.
 *
 * The "action type" is specified by the first argument, which must be a
 * constant of the form "TERM_XTRA_*" as given in "term.h", and the second
 * argument specifies the "information" for that argument, if any, and will
 * vary according to the first argument.
 *
 * In general, this function should return zero if the action is successfully
 * handled, and non-zero if the action is unknown or incorrectly handled.
 */
static errr Term_xtra_nds(int n, int v) {
  term_data *td = (term_data*)(Term->data);
  
  /* Analyze */
  switch (n) {
    case TERM_XTRA_EVENT: {
      INFO_PRINT("Xevent[v=%d]", v);
      /*
       * Process some pending events
       */
      return (CheckEvents(v));
    }
      
    case TERM_XTRA_FLUSH: {
      INFO_PRINT("Xflush[v=%d]", v);
      /*
       * Flush all pending events
       */
      while (!CheckEvents(false));

      return (0);
    }
      
    case TERM_XTRA_CLEAR: {
      INFO_PRINT("Xclear[v=%d]", v);
      /*
       * Clear the entire window
       */
      int x, y;
      u32b vram_offset;
      u16b *fb = BG_GFX;

      for (y = 0; y < 24; y++) {
        for (x = 0; x < 80; x++) {
          vram_offset = (y & 0x1F)*8*256+x*3;

          byte xx,yy;
          for (yy=0;yy<8;yy++) {
            for (xx = 0; xx < 3; xx++) {
              fb[yy * 256 + xx + vram_offset] = 0;
            }
          }
        }
      }

      return (0);
    }
      
    case TERM_XTRA_SHAPE: {
      INFO_PRINT("Xshape[v=%d]", v);
      /*
       * Set the cursor visibility XXX XXX XXX
       *
       * This action should change the visibility of the cursor,
       * if possible, to the requested value (0=off, 1=on)
       *
       * This action is optional, but can improve both the
       * efficiency (and attractiveness) of the program.
       */

	    return (0);
    }
      
    case TERM_XTRA_FROSH: {
      return (0);
    }
      
    case TERM_XTRA_FRESH: {
      INFO_PRINT("Xfresh[v=%d]", v);
      return (0);
    }
      
    case TERM_XTRA_NOISE: {
      INFO_PRINT("Xnoise[v=%d]", v);
      /*
       * Make a noise XXX XXX XXX
       *
       * This action should produce a "beep" noise.
       *
       * This action is optional, but convenient.
       */

      return (0);
    }
      
    case TERM_XTRA_BORED: {
      INFO_PRINT("Xbored[v=%d]", v);
      /*
       * Handle random events when bored
       */
      return (CheckEvents(0));
    }
      
    case TERM_XTRA_REACT: {
      INFO_PRINT("Xreact[v=%d]", v);
      /*
       * React to global changes XXX XXX XXX
       *
       * For example, this action can be used to react to
       * changes in the global "color_table[256][4]" array.
       *
       * This action is optional, but can be very useful for
       * handling "color changes" and the "arg_sound" and/or
       * "arg_graphics" options.
       */

      return (0);
    }
      
    case TERM_XTRA_ALIVE: {
      INFO_PRINT("Xalive[v=%d]", v);
      /*
       * Change the "hard" level XXX XXX XXX
       *
       * This action is used if the program changes "aliveness"
       * by being either "suspended" (v=0) or "resumed" (v=1)
       * This action is optional, unless the computer uses the
       * same "physical screen" for multiple programs, in which
       * case this action should clean up to let other programs
       * use the screen, or resume from such a cleaned up state.
       *
       * This action is currently only used by "main-gcu.c",
       * on UNIX machines, to allow proper "suspending".
       */

      return (0);
    }
      
    case TERM_XTRA_LEVEL: {
      INFO_PRINT("Xlevel[v=%d]", v);
      /*
       * Change the "soft" level XXX XXX XXX
       *
       * This action is used when the term window changes "activation"
       * either by becoming "inactive" (v=0) or "active" (v=1)
       *
       * This action can be used to do things like activate the proper
       * font / drawing mode for the newly active term window.  This
       * action should NOT change which window has the "focus", which
       * window is "raised", or anything like that.
       *
       * This action is optional if all the other things which depend
       * on what term is active handle activation themself, or if only
       * one "term_data" structure is supported by this file.
       */

      return (0);
    }
      
    case TERM_XTRA_DELAY: {
      INFO_PRINT("Xdelay[v=%d]", v);
      /*
       * Delay for some milliseconds
       */
      int i;
      for (i = 0; i < v; i++) {
        swiWaitForVBlank();
      }

      return (0);
    }
  }

  INFO_PRINT("Xunknown[n=%d,v=%d]", n, v);
  /* Unknown or Unhandled action */
  return (1);
}


/*
 * Display the cursor
 */
static errr Term_curs_nds(int x, int y) {
  INFO_PRINT("curs[x=%d,y=%d]", x, y);

  u32b vram_offset = (y - 1) * TILE_HEIGHT * 256 + x * TILE_WIDTH + 8 * 256;
  byte xx, yy;
  for (xx = 0; xx < TILE_WIDTH; xx++) {
      BG_GFX[xx + vram_offset] = RGB15(31, 31, 0)| BIT(15);
      BG_GFX[256 * (TILE_HEIGHT-1) + xx + vram_offset] = RGB15(31, 31, 0)| BIT(15);
  }
  for (yy = 0; yy < TILE_HEIGHT; yy++) {
      BG_GFX[yy * 256 + vram_offset] = RGB15(31, 31, 0)| BIT(15);
      BG_GFX[yy * 256 + TILE_WIDTH - 1 + vram_offset] = RGB15(31, 31, 0)| BIT(15);
  }

  /* Success */
  return (0);
}


void draw_char(byte x, byte y, wchar_t c) {
  u32b vram_offset = (y & 0x1F) * TILE_WIDTH * 256 + x * TILE_HEIGHT;
  u32b tile_offset = c * 24;
  u16b* fb = BG_GFX;
  const u16b* chardata = top_font_bin;
  // TODO what is this doing?
  if (y & 32) {
    fb = &BG_GFX_SUB[16 * 1024];
    chardata = btm_font_bin;
  }
  byte xx, yy;
  for (yy = 0; yy < TILE_HEIGHT; yy++) {
    for (xx = 0; xx < TILE_WIDTH; xx++) {
      fb[yy * 256 + xx + vram_offset]
          = chardata[yy * 3 + xx + tile_offset] | BIT(15);
    }
  }
}

void draw_color_char(byte x, byte y, wchar_t c, byte clr) {
  u32b vram_offset = (y & 0x1F) * TILE_HEIGHT * 256 + x * TILE_WIDTH;
  u32b tile_offset = c * 24;
  u16b* fb = BG_GFX;
  const u16b* chardata = top_font_bin;
  // TODO what is this doing?
  if (y & 32) {
    fb = &BG_GFX_SUB[16*1024];
    chardata = btm_font_bin;
  }
  byte xx, yy;
  u16b val;
  u16b fgc = color_data[clr & 0xF];
  for (yy = 0; yy < TILE_HEIGHT; yy++) {
    for (xx = 0;xx < TILE_WIDTH; xx++) {
      val = (chardata[yy * 3 + xx + tile_offset]);
      fb[yy * 256 + xx + vram_offset] = (val & fgc) | BIT(15);
    }
  }
}

/*
 * Erase some characters
 *
 * This function should erase "n" characters starting at (x,y).
 *
 * You may assume "valid" input if the window is properly sized.
 */
static errr Term_wipe_nds(int x, int y, int n) {
//  INFO_PRINT("wipe[x=%d,y=%d,n=%d]", x, y, n);

  term_data *td = (term_data*)(Term->data);

  int i;

  /* Draw a blank */
  for (i = 0; i < n; i++) {
    draw_color_char(x + i, y, 0, 0);
  }

  /* Success */
  return (0);
}


/*
 * Draw some text on the screen
 *
 * This function should actually display an array of characters
 * starting at the given location, using the given "attribute",
 * and using the given string of characters, which contains
 * exactly "n" characters and which is NOT null-terminated.
 *
 * You may assume "valid" input if the window is properly sized.
 *
 * You must be sure that the string, when written, erases anything
 * (including any visual cursor) that used to be where the text is
 * drawn.  On many machines this happens automatically, on others,
 * you must first call "Term_wipe_xxx()" to clear the area.
 *
 * In color environments, you should activate the color contained
 * in "color_data[a & 0x0F]", if needed, before drawing anything.
 *
 * You may ignore the "attribute" if you are only supporting a
 * monochrome environment, since this routine is normally never
 * called to display "black" (invisible) text, including the
 * default "spaces", and all other colors should be drawn in
 * the "normal" color in a monochrome environment.
 *
 * Note that if you have changed the "attr_blank" to something
 * which is not black, then this function must be able to draw
 * the resulting "blank" correctly.
 *
 * Note that this function must correctly handle "black" text if
 * the "always_text" flag is set, if this flag is not set, all the
 * "black" text will be handled by the "Term_wipe_xxx()" hook.
 */
static errr Term_text_nds(int x, int y, int n, byte a, const wchar_t *cp) {
//  INFO_PRINT("text[x=%d,y=%d,n=%d,a=%c,cp=%s]", x, y, n, a, cp);

  int i;
  
  /* Do nothing if the string is null */
  if (!cp || !*cp) return (-1);
  
  /* Put the characters directly */
  for (i = 0; i < n; i++) {
    /* Check it's the right attr */
    if ((x + i < Term->wid) && (Term->scr->a[y][x + i] == a)) {
      /* Put the char */
      draw_color_char(x + i, y, cp[i], a);
    } else {
      break;
    }
  }
  /* Success */
  return (0);
}


//void draw_tile(byte x, byte y, u16b tile) {
//  u32b vram_offset = (y & 0x7F) * TILE_HEIGHT * 256 + x * TILE_WIDTH +
//    8 * 256,
//    tile_offset = (tile & 0x7FFF) * TILE_WIDTH * TILE_HEIGHT;
//  u16b* fb = BG_GFX;
//  byte xx, yy;
//  for (yy = 0; yy < TILE_HEIGHT; yy++) {
//    for (xx = 0; xx < TILE_WIDTH; xx++) {
//      fb[yy * 256 + xx + vram_offset] =
//          tiles_bin[yy * TILE_WIDTH + xx + tile_offset] | BIT(15);
//    }
//  }
//}

/*
 * Draw some attr/char pairs on the screen
 *
 * This routine should display the given "n" attr/char pairs at
 * the given location (x,y).  This function is only used if one
 * of the flags "always_pict" or "higher_pict" is defined.
 *
 * You must be sure that the attr/char pairs, when displayed, will
 * erase anything (including any visual cursor) that used to be at
 * the given location.  On many machines this is automatic, but on
 * others, you must first call "Term_wipe_xxx(x, y, 1)".
 *
 * With the "higher_pict" flag, this function can be used to allow
 * the display of "pseudo-graphic" pictures, for example, by using
 * the attr/char pair as an encoded index into a pixmap of special
 * "pictures".
 *
 * With the "always_pict" flag, this function can be used to force
 * every attr/char pair to be drawn by this function, which can be
 * very useful if this file can optimize its own display calls.
 *
 * This function is often associated with the "arg_graphics" flag.
 *
 * This function is only used if one of the "higher_pict" and/or
 * "always_pict" flags are set.
 */
static errr Term_pict_nds(int x, int y, int n, const byte *ap, const wchar_t *cp,
                          const int *tap, const int *tcp) {
  INFO_PRINT("pict[x=%d,y=%d,n=%d,ap=%s,cp=%s]", x, y, n, ap, cp);

  term_data *td = (term_data*)(Term->data);
  u16b tile_number = DEF_TILES_PER_ROW * (*ap - 0x80) + (*cp - 0x80);
  /* XXX XXX XXX */

  int i;

  /* Put the characters directly */
  for (i = 0; i < n, *cp; i++) {
    if ((x + i < Term->wid) && (*cp != '\0')) {
//      draw_tile(x + i, y, tile_number);
    } else {
      break;
    }
  }
  /* Success */
  return (0);
}


/*** Internal Functions ***/


/*
 * Instantiate a "term_data" structure
 *
 * This is one way to prepare the "term_data" structures and to
 * "link" the various informational pieces together.
 *
 * This function assumes that every window should be 80x24 in size
 * (the standard size) and should be able to queue 256 characters.
 * Technically, only the "main screen window" needs to queue any
 * characters, but this method is simple.  One way to allow some
 * variation is to add fields to the "term_data" structure listing
 * parameters for that window, initialize them in the "init_xxx()"
 * function, and then use them in the code below.
 *
 * Note that "activation" calls the "Term_init_xxx()" hook for
 * the "term" structure, if needed.
 */
static void term_data_link(int i) {
  term_data *td = &data[i];
  
  term *t = &td->t;
  
  /* Initialize the term */
  term_init(t, 85, 24, 256);
  
  /* Choose "soft" or "hard" cursor XXX XXX XXX */
  /* A "soft" cursor must be explicitly "drawn" by the program */
  /* while a "hard" cursor has some "physical" existance and is */
  /* moved whenever text is drawn on the screen.  See "term.c". */
  t->soft_cursor = true;
  
  /* Use "Term_pict()" for all attr/char pairs XXX XXX XXX */
  /* See the "Term_pict_xxx()" function above. */
  /* td->t->always_pict = true; */
  
  /* Use "Term_pict()" for some attr/char pairs XXX XXX XXX */
  /* See the "Term_pict_xxx()" function above. */
  t->higher_pict = true;
  
  /* Use "Term_text()" even for "black" text XXX XXX XXX */
  /* See the "Term_text_xxx()" function above. */
  /* t->always_text = true; */
  
  /* Ignore the "TERM_XTRA_BORED" action XXX XXX XXX */
  /* This may make things slightly more efficient. */
  t->never_bored = true;
  
  /* Ignore the "TERM_XTRA_FROSH" action XXX XXX XXX */
  /* This may make things slightly more efficient. */
  /* td->t->never_frosh = true; */
  
  /* Prepare the init/nuke hooks */
  t->init_hook = Term_init_nds;
  t->nuke_hook = Term_nuke_nds;
  
  /* Prepare the template hooks */
  t->xtra_hook = Term_xtra_nds;
  t->curs_hook = Term_curs_nds;
  t->wipe_hook = Term_wipe_nds;
  t->text_hook = Term_text_nds;
  t->pict_hook = Term_pict_nds;

  /* Remember where we came from */
  t->data = (void*)(td);
  
  /* Activate it */
  Term_activate(t);
}



/*
 * Initialization function
 */
errr init_nds(void) {
  /* Initialize globals */
  
  /* Initialize "term_data" structures */
  
  int i;
  bool none = true;
  
  term_data *td;

  /* Main window */
  td = &data[0];
  memset(td, 0, sizeof(term_data));
  td->rows = 24;
  td->cols = 85; // PAW: originally 37. why?
  td->tile_height = TILE_HEIGHT;
  td->tile_width = TILE_WIDTH;
        
   /* Create windows (backwards!) */
  for (i = MAX_TERM_DATA - 1; i >= 0; i--) {
    /* Link */
    term_data_link(i);
    none = false;

    /* Set global pointer */
    angband_term[0] = Term;
  }
  
  if (none) return (1);
  
  /* Success */
  return (0);
}


/*
 * Init some stuff
 *
 * This function is used to keep the "path" variable off the stack.
 */
static void init_stuff(void) {
  char path[1024];

  /* Prepare the path */
  strcpy(path, "/angband/lib/");

  /* Prepare the filepaths */
  init_file_paths(path, path, path);

  /* Hack */
  //strcpy(savefile, "/angband/lib/save/PLAYER");
}

void nds_init_fonts() {
  /* the font is now compiled in as ds_subfont for error reporting purposes */
  /* ds_subfont contains the bgr version */
  /*subfont_bgr_bin = &ds_subfont[0]; */
  u16b i;
  u16b t,t2;
  for (i=0;i<8*3*256;i++) {
    t = ds_subfont[i];
    t2 = t & 0x8000;
    t2 |= (t & 0x001f)<<10;
    t2 |= (t & 0x03e0);
    t2 |= (t & 0x7c00)>>10;
    subfont_bgr_bin[i] = t;
    subfont_rgb_bin[i] = t2;
  }
  top_font_bin = subfont_rgb_bin;
  btm_font_bin = subfont_bgr_bin;
}

/* if you are calling this function, not much should be happening after */
/* since it clobbers the font pointers */
void nds_fatal_err(const char* msg) {
  DEBUG_PRINT("Fatal error: %s\n", msg);
}

void nds_init_buttons() {
  u16b i, j;
  for (i = 0; i < (NDS_NUM_MAPPABLE << NDS_NUM_MODIFIER); i++) {
     for (j = 0; j < NDS_CMD_LENGTH; j++) {
      nds_btn_cmds[i][j] = 0;
     }
  }
  /* Set defaults */
  for (i = 0; i < (NDS_NUM_MAPPABLE << NDS_NUM_MODIFIER); i++) {
    nds_btn_cmds[i][0] = btn_defaults[i];
  }
  return;
}

void swap_font(bool bottom) 
{
  if (!bottom) 
    {
      if (top_font_bin == subfont_rgb_bin) top_font_bin = subfont_bgr_bin;
      else top_font_bin = subfont_rgb_bin;
    } 
  else 
    {
      if (btm_font_bin == subfont_rgb_bin) btm_font_bin = subfont_bgr_bin;
      else btm_font_bin = subfont_rgb_bin;
    }
}

void nds_raw_print(const char* str) {
  static u16b x=0,y=32;
  while (*str) 
    {
      draw_char(x,y,(u8)(*(str++)));
      x++;
      if (x > 78) 
	{
	  x = 0;
	  y++;
	  if (y > 34) y = 32;
	}
    }
  draw_char(x,y,219);
  fflush(0);
}

/*
 * Display warning message (see "z-util.c")
 */
static void hook_plog(const char *str) {
  /* Warning */
  if (str)
    {
      nds_raw_print(str);
    }
}

void nds_exit(int code) {
  u16b i;
  for (i = 0; i < 60; i++) {
    nds_updated = 0xFF;
    do_vblank();	/* wait 1 sec. */
  }
  systemShutDown();
}

/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(const char *str) {
  int i, j;
  
  
  /* Give a warning */
  if (str)
    {
      nds_fatal_err(str);
    }

  /* Bail */
  nds_exit(0);
}

/*
 * Main function
 *
 * This function must do a lot of stuff.
 */
int main(int argc, char *argv[]) {
  /* Initialize the machine itself  */

  enable_interrupts();

  // TODO perhaps change later -- MODE_0 DISPLAY_0 is probably fine
  videoSetMode(MODE_5_2D | DISPLAY_BG2_ACTIVE);

  vramSetBankA(VRAM_A_MAIN_BG);       /* BG2, event buf, fonts */
  vramSetBankB(VRAM_B_MAIN_BG);       /* for storage (tileset) */
  vramSetBankB(VRAM_C_SUB_BG);
  vramSetBankD(VRAM_D_MAIN_BG);       /* for storage (tileset) */
  vramSetBankE(VRAM_E_LCD);	      /* for storage (WIN_TEXT) */
  vramSetBankF(VRAM_F_LCD);           /* for storage (WIN_TEXT) */

  REG_BG2CNT = BG_BMP16_256x256;
  REG_BG2PA = 1<<8;
  REG_BG2PB = 0;
  REG_BG2PC = 0;
  REG_BG2PD = 1<<8;
  REG_BG2Y = 0;
  REG_BG2X = 0;

  consoleDemoInit();
  INFO_PRINT("Debug testing!\n");
//  nds_show_debug_console();

  Keyboard *kbd = keyboardDemoInit();
//  keyboardShow();

  // TODO handler for key events
//  kbd->OnKeyPressed = OnKeyPressed;

  nds_init_fonts();
  
  swiWaitForVBlank();
  
  if (!fatInitDefault()) {
      nds_fatal_err("\nError initializing FAT drivers.\n");
      nds_fatal_err("Make sure the game is patched with the correct DLDI.\n");
      nds_fatal_err(" (see https://www.chishm.com/DLDI/ for more info).\n");
      nds_fatal_err("\n\nUnable to access filesystem.\nCannot continue.\n");
      return 1;
  }
  
  swiWaitForVBlank();
  
  chdir("/angband");

  nds_init_buttons();

  /* Activate hooks */
  plog_aux = hook_plog;
  quit_aux = hook_quit;

  /* Initialize the windows */
  if (init_nds()) quit("Oops!");

  swiWaitForVBlank();

  /* XXX XXX XXX */
  ANGBAND_SYS = "nds";
  
  /* Initialize some stuff */
  init_stuff();
//
//  draw_tile(2, 2, 5);
//  draw_tile(4, 2, 15);
//  draw_tile(6, 2, 25);
//  draw_tile(8, 2, 35);

  INFO_PRINT("Before init_display()\n");
  init_display();
  INFO_PRINT("Before init_display()\n");

  INFO_PRINT("Before init_angband()\n");
  /* Initialize */
  init_angband();
  INFO_PRINT("After init_angband()\n");

  swiWaitForVBlank();

//      for (int i = 0; i < 50; i++) {
//        draw_tile(i % 10, i / 10, i + 600);
//      }
  /* Wait for response */

  INFO_PRINT("Before pause_line()\n");
  pause_line(Term);
  INFO_PRINT("After pause_line()\n");

  INFO_PRINT("Before play_game()\n");
  /* Play the game */
  play_game(true);
  INFO_PRINT("After play_game()\n");

  /* Free resources */
  cleanup_angband();

  /* Quit */
  quit(NULL);
  
  /* Exit */
  return (0);
}

// Hacks for unsupported functions moves somewhere else eventually
double sqrt(double x) {
  return f32tofloat(sqrtf32(floattof32(x)));
}

uid_t getuid() {
  return 0;
}

struct passwd *getpwuid(uid_t uid) {
  return 0;
}

struct passwd *getpwnam(const char *name) {
  return 0;
}