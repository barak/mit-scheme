/* -*-C-*-

$Id: dosint10.c,v 1.5 1992/10/17 22:54:55 jinx Exp $

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

/*
  Scheme primitives for faster video on PC's by using BIOS Int. 0x10
  (pshuang@martigny, June 1992).

  Naming conventions: bios__* are simply interfaces to the raw BIOS
  calls, bios_* are slightly cooked interfaces, e.g. they do not have
  the exact same parameters that the BIOS calls document.

  The coordinates (0,0) correspond to the upper-left corner.
*/

#include "scheme.h"
#include "prims.h"
#include "msdos.h"
#include "dosint10.h"
#include "osio.h"
#include "ostty.h"

/**********************************************************************/

#define MAP map_ansi_sys_color_to_bios_attribute

#define DEFAULT_FOREGROUND 37	/* White */
#define DEFAULT_BACKGROUND 40	/* Black */

static int 
map_ansi_sys_color_to_bios_attribute (int iANSIcode)
{
  /*
    ANSI.SYS color mappings (ISO 6429 standard) to video memory attributes:

    (Notes: Bit 7 == Blink; Foreground bits with bit 3 low map similar
    to Background bits, except that 0b0111 maps to Light Gray; background
    colors in ANSI.SYS specified with foreground+10.)

      ANSI.SYS    Background (bits 6-4)  Foreground (bits 3-0, bit 3 high)
      ==========  =====================  =================================
      30 Black    000                    Dark Gray
      31 Red      100                    Light Red
      32 Green    010                    Light Green
      33 Yellow   110 (listed as Brown)  Yellow
      34 Blue     001                    Light Blue
      35 Magenta  101                    Light Magenta
      36 Cyan     011                    Light Cyan
      37 White    111                    White
  */

  switch (iANSIcode)
    {
      case 30:
      case 40: return 0x00;
      case 31:
      case 41: return _0B(0,0,0,0,0,1,0,0);
      case 32:
      case 42: return _0B(0,0,0,0,0,0,1,0);
      case 33:
      case 43: return _0B(0,0,0,0,0,1,1,0);
      case 34:
      case 44: return _0B(0,0,0,0,0,0,0,1);
      case 35:
      case 45: return _0B(0,0,0,0,0,1,0,1);
      case 36:
      case 46: return _0B(0,0,0,0,0,0,1,1);
      case 37:
      case 47: return _0B(0,0,0,0,0,1,1,1);
    }
  return 0x00;
    /* covers case of atoi parse failure returning 0 */
}

static int bios_initialized_p = 0;
static int display_columns = UNINITIALIZED;
static int display_rows = UNINITIALIZED;
  /* 0-based values, initialized from variables in dostty.c */
static int foreground_attribute = UNINITIALIZED;
static int background_attribute = UNINITIALIZED;
static int normal_video = UNINITIALIZED;
static int reverse_video = UNINITIALIZED;

static void 
bios_initialize_variables (int foreground, int background)
{
  char* psTemp;

  if (display_columns == UNINITIALIZED)
    display_columns = (tty_x_size - 1);

  if (display_rows == UNINITIALIZED)
    display_rows = (tty_y_size - 1);

  foreground_attribute = (MAP (foreground));
  background_attribute = (MAP (background));
  normal_video = (foreground_attribute | background_attribute);
  reverse_video = ((foreground_attribute << 4) | (background_attribute >> 4));
  return;
}

static int
stdout_is_console_p (void)
{
  Tchannel output = (OS_tty_output_channel ());
  if ((OS_channel_type (output)) != channel_type_terminal)
    return (0);
  return (1);
}

static void 
bios_uninitialize_variables (void)
{
  display_columns = UNINITIALIZED;
  display_rows = UNINITIALIZED;
  foreground_attribute = UNINITIALIZED;
  background_attribute = UNINITIALIZED;
  normal_video = UNINITIALIZED;
  reverse_video = UNINITIALIZED;
  return;
}

static void 
bios__scroll_up_rectangle (int iBlankAttribute, int iLines,
			   int iUpperLeftX, int iUpperLeftY,
			   int iBottomRightX, int iBottomRightY)
{
  union REGS regs;

  regs.h.ah = 0x06;
  regs.h.al = iLines;
  regs.h.bh = iBlankAttribute;
  regs.h.ch = iUpperLeftY;
  regs.h.cl = iUpperLeftX;
  regs.h.dh = iBottomRightY;
  regs.h.dl = iBottomRightX;
  int10h (&regs, &regs);
  return;
}

static void 
bios__scroll_down_rectangle (int iBlankAttribute, int iLines,
			     int iUpperLeftX, int iUpperLeftY,
			     int iBottomRightX, int iBottomRightY)
{
  union REGS regs;

  regs.h.ah = 0x07;
  regs.h.al = iLines;
  regs.h.bh = iBlankAttribute;
  regs.h.ch = iUpperLeftY;
  regs.h.cl = iUpperLeftX;
  regs.h.dh = iBottomRightY;
  regs.h.dl = iBottomRightX;
  int10h (&regs, &regs);
  return;
}

static void 
bios__set_cursor_position (int iPageNumber, int iColumn, int iRow)
{
  union REGS regs;

  regs.h.ah = 0x02;
  regs.h.bh = iPageNumber;
  regs.h.dh = iRow;
  regs.h.dl = iColumn;
  int10h (&regs, &regs);
  return;
}

static void 
bios__write_char_with_attribute (char cChar, int iPageNumber,
				 int iAttribute, int iRepeatCount)
     /* Note: no special characters are recognized */
{
  union REGS regs;

  regs.h.ah = 0x09;
  regs.h.al = cChar;
  regs.h.bh = iPageNumber;
  regs.h.bl = iAttribute;
  regs.x.cx = iRepeatCount;
  int10h (&regs, &regs);
  return;
}

static void 
bios__teletype_output_char (char cChar, int iPageNumber,
			    int iGraphicsModeForegroundColor)
  /* Note: CR/LF/BS/BEL recognized */
{
  union REGS regs;

  regs.h.ah = 0x0E;
  regs.h.al = cChar;
  regs.h.bh = iPageNumber;
  regs.h.bl = iGraphicsModeForegroundColor;
  int10h (&regs, &regs);
  return;
}

static void 
bios__set_video_mode (int iModeNumber)
{
  union REGS regs;

  regs.h.ah = 0x00;
  regs.h.al = iModeNumber;
  int10h (&regs, &regs);
  return;
}

static void
bios__set_cursor_size (int iBlinkMode, int iTopScan, int iBottomScan)
{
  union REGS regs;

  regs.h.ah = 0x01;
  regs.h.ch = (((iBlinkMode & _0B(0,0,0,0,0,0,1,1)) << 5)
	       | (iTopScan & _0B(0,0,0,1,1,1,1,1)));
  regs.h.cl = (iBottomScan & _0B(0,0,0,1,1,1,1,1));
  int10h (&regs, &regs);
  return;
}

static void 
bios_clear_line (int iCol, int iRow, int iFirstUnusedX)
{
  bios__scroll_up_rectangle (normal_video, 0, iCol, iRow, iFirstUnusedX, iRow);
  return;
}

static void
bios_clear_region (int iUpperLeftX, int iUpperLeftY,
		   int iBottomRightX, int iBottomRightY)
{
  bios__scroll_up_rectangle (normal_video, 0,
			     iUpperLeftX, iUpperLeftY,
			     iBottomRightX, iBottomRightY);
  return;
}

static void
bios_clear_screen (void)
{
  bios__scroll_up_rectangle (normal_video, 0x00,
			     0, 0, display_columns, display_rows);
  return;
}

static void
bios_get_cursor_position (int * x, int * y)
{
  union REGS regs;

  regs.h.ah = 0x03;
  regs.h.bh = 0x00; /* page number */
  int10h (&regs, &regs);
  *y = regs.h.dh;
  *x = regs.h.dl;
  return;
}

static void
bios_write_string_attributed (char * pString, long lLength, int iColumn,
			      int iRow, int iSingleAttribute)
{
  strncpy (pRealModeBuffer, pString, lLength);
  asm_bios__write_string_attr (1, 0, iSingleAttribute, lLength,
			       iColumn, iRow,
			       ((RealModeBufferParagraph << 16) + 0));
  return;
}

/* This does no argument checking. */

void
bios__set_text_size (int al, int bl)
{
  union REGS regs;

  regs.h.ah = 0x11;
  regs.h.al = al;
  regs.h.bl = bl;
  int10h (&regs, &regs);
  return;
}

DEFINE_PRIMITIVE ("BIOS:CAN-USE-BIOS?", Prim_bios_p, 0, 0,
		  "#T if BIOS operations can be used, otherwise #F. ()")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN ((stdout_is_console_p ()) ? SHARP_T : SHARP_F);
}

DEFINE_PRIMITIVE ("BIOS:BEEP", Prim_bios_beep, 0, 0,
		  "Produce a Beep [feep]. ()")
{
  PRIMITIVE_HEADER (0);
  if (!bios_initialized_p)
    error_external_return ();
  bios__teletype_output_char ('\007', 0, 0x00);
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:CLEAR-LINE!", Prim_bios_clear_line, 3, 3,
		  "Clear a line in the console. (column row first-unused)")
{
  PRIMITIVE_HEADER (3);
  if (!bios_initialized_p)
    error_external_return ();
  bios_clear_line ((arg_integer (1)), (arg_integer (2)), (arg_integer (3)));
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:CLEAR-RECTANGLE!", Prim_bios_clear_rectangle, 5, 5,
		  "Clear a rectangle in the console. (xl xu yl yu hl)")
{
  PRIMITIVE_HEADER (5);
  if (!bios_initialized_p)
    error_external_return ();
  bios_clear_region ((arg_integer (1)), (arg_integer (3)),
		     (arg_integer (2)), (arg_integer (4)));
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:CLEAR-SCREEN!", Prim_bios_clear_screen, 0, 0,
		  "Clear the whole console. ()")
{
  PRIMITIVE_HEADER (0);
  if (!bios_initialized_p)
    error_external_return ();
  bios_clear_screen ();
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:INITIALIZE!", Prim_bios_enter, 2, 2,
		  "Initialize bios internal state. (foreground background)")
{
  int foreground, background;
  PRIMITIVE_HEADER (2);
  if ((! (stdout_is_console_p ())) || bios_initialized_p)
    error_external_return ();
  bios_initialize_variables ((arg_integer_in_range (1, 30, 38)),
			     (arg_integer_in_range (2, 40, 48)));
  bios_clear_screen ();
  bios__set_cursor_position (0, 0, display_rows);
  bios_initialized_p = 1;
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:EXIT!", Prim_bios_exit, 0, 0,
		  "Clean up bios internal state. ()")
{
  PRIMITIVE_HEADER (0);
  if (!bios_initialized_p)
    error_external_return ();
  bios__set_cursor_position (0, 0, display_rows);
  bios_uninitialize_variables ();
  bios_initialized_p = 0;
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:SCROLL-LINES-DOWN!", Prim_bios_scroll_lines_down, 5, 5,
		  "Scroll a console rectangle down. (xl xu yl yu delta)")
{
  PRIMITIVE_HEADER (5);
  bios__scroll_down_rectangle (normal_video, (arg_integer (5)),
			       (arg_integer (1)), (arg_integer (3)),
			       (arg_integer (2)), (arg_integer (4)));
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:SCROLL-LINES-UP!", Prim_bios_scroll_lines_up, 5, 5,
		  "Scroll a console rectangle up. (xl xu yl yu delta)")
{
  PRIMITIVE_HEADER (5);
  bios__scroll_up_rectangle (normal_video, (arg_integer (5)),
			     (arg_integer (1)), (arg_integer (3)),
			     (arg_integer (2)), (arg_integer (4)));
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:WRITE-CURSOR!", Prim_bios_write_cursor, 2, 2,
		  "Draw the cursor on the console. (x y)")
{
  PRIMITIVE_HEADER (2);
  bios__set_cursor_position (0, (arg_integer (1)), (arg_integer (2)));
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:WRITE-CHAR!", Prim_bios_write_char, 2, 2,
		  "Write a character on the console. (char highlight)")
{
  PRIMITIVE_HEADER (2);
  if (BOOLEAN_ARG (2))
  {
    int x, y;

    bios_get_cursor_position (&x, &y);
    bios__scroll_up_rectangle (reverse_video, 1, x, y, x, y);
    bios__teletype_output_char ((arg_ascii_char (1)), 0, 0);
  }
  else
    bios__teletype_output_char ((arg_ascii_char (1)), 0, 0);
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:WRITE-SUBSTRING!", Prim_bios_write_substring, 4, 4,
		  "Write a string on the console. (str start end highlight)")
{
  int x, y;
  long start, end;
  PRIMITIVE_HEADER (4);
  start = (arg_integer (2));
  end = (arg_integer (3));
  if (start > end)
    PRIMITIVE_RETURN (SHARP_F);
  bios_get_cursor_position (&x, &y);
  bios_write_string_attributed (((STRING_ARG (1)) + start), (end - start),
				x, y,
				((BOOLEAN_ARG (4))
				 ? reverse_video
				 : normal_video));
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:SET-VIDEO-MODE!", Prim_bios_set_video_mode, 1, 1,
		  "Set the video display mode for the console. (mode)")
{
  PRIMITIVE_HEADER (1);
  if ((! (stdout_is_console_p ())) || bios_initialized_p)
    error_external_return ();
  bios__set_video_mode (arg_integer (1));
  pc_gestalt_screen_x_size ();
  pc_gestalt_screen_y_size ();
  bios_clear_screen ();
  bios__set_cursor_position (0, 0, display_rows);
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:SET-CURSOR-SIZE!", Prim_bios_set_cursor_size, 3, 3,
		  "Set the cursor's shape and size. (blinkmode top bottom)")
     /* blinkmode: 0 = normal, 1 = invisible, 2 = erratic, 3 = slow */
{
  PRIMITIVE_HEADER (3);
  if (! (stdout_is_console_p ()))
    error_external_return ();
  bios__set_cursor_size ((arg_integer (1)),
			 (arg_integer (2)),
			 (arg_integer (3)));
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("BIOS:SET-TEXT-SIZE!", Prim_bios_set_text_size, 2, 2,
		  "Diddle the character generator's blocks. (al bl)")
{
  int al, bl;
  PRIMITIVE_HEADER (2);
  if ((! (stdout_is_console_p ())) || bios_initialized_p)
    error_external_return ();
  al = (arg_integer_in_range (1, 0x0, 0x20));
  bl = (arg_integer (2));
  if (((al & 0x8) != 0) || ((al & 0x7) == 0) || ((al & 0x7) > 4))
    error_bad_range_arg (1);
  bios__set_text_size (al, bl);
  PRIMITIVE_RETURN (SHARP_T);
}
