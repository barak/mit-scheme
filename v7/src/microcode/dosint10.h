/* -*-C-*-

$Id: dosint10.h,v 1.2 1992/10/17 22:22:32 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

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

