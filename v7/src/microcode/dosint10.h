/* -*-C-*-

$Id: dosint10.h,v 1.3 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <dos.h>
#include <stdlib.h>

#define _0B(b7,b6,b5,b4,b3,b2,b1,b0) ((b7<<7)|(b6<<6)|(b5<<5)|(b4<<4)|(b3<<3)|(b2<<2)|(b1<<1)|(b0))
#define UNINITIALIZED -1

int map_ansi_sys_color_to_bios_attribute (int iANSIcode);

void bios_initialize_variables (int, int);
void bios_uninitialize_variables (void);

void bios__scroll_up_rectangle (int iBlankAttribute, int iLines,
  int iUpperLeftX, int iUpperLeftY,
  int iBottomRightX, int iBottomRightY);
void bios__scroll_down_rectangle (int iBlankAttribute, int iLines,
  int iUpperLeftX, int iUpperLeftY,
  int iBottomRightX, int iBottomRightY);
void bios__set_cursor_position (int iPageNumber, int iColumn, int iRow);
void bios__write_char_with_attribute (char cChar, int iPageNumber,
  int iAttribute, int iRepeatCount);
void bios__teletype_output_char (char cChar, int iPageNumber,
  int iGraphicsModeForegroundColor);
void bios__set_video_mode (int iModeNumber);
void bios__set_cursor_size (int iBlinkMode, int iTopScan, int iBottomScan);

void bios_clear_line (int iCol, int iRow, int iFirstUnusedX);
void bios_clear_region (int iUpperLeftX, int iUpperLeftY,
  int iBottomRightX, int iBottomRightY);
void bios_clear_screen (void);
void bios_get_cursor_position (int *x, int *y);
void bios_write_string_attributed (char *pString, long lLength, int iColumn,
  int iRow, int iSingleAttribute);

extern void asm_bios__write_string_attr (unsigned long lAttributeMode,
  unsigned long lPageNumber, unsigned long lSingleAttribute,
  unsigned long lCharCount, unsigned long lColumn, unsigned long lRow,
  unsigned long pString);

extern int tty_x_size;
extern int tty_y_size;
extern unsigned long RealModeBufferParagraph;
extern char * pRealModeBuffer;
extern void pc_gestalt_screen_x_size (void);
extern void pc_gestalt_screen_y_size (void);

