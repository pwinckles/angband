/**
 * Copied from https://github.com/fancypantalons/NetHack/blob/NetHackDS/sys/nds/arm9/include/nds_debug.h
 */

#ifndef _NDS_DEBUG_H_
#define _NDS_DEBUG_H_

#define DEBUG_PRINT(fmt, ...) nds_debug_print(__FILE__, __LINE__, fmt, ##__VA_ARGS__)
#define INFO_PRINT(fmt, ...) nds_info_print(fmt, ##__VA_ARGS__)

void nds_debug_print(char *file, int line, char *fmt, ...);
void nds_info_print(char *fmt, ...);

#endif
