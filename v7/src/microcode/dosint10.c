/* -*-C-*-

$Id: dosint10.c,v 1.2 1992/09/15 20:35:42 jinx Exp $

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

  The coordinates (0x00,0x00) is the upper-left corner.
*/

#include "dosint10.h"
#include "scheme.h"
#include "prims.h"

#define DEBUG_WRITE(message) \
  {\
    FILE *debug_file = fopen("\\tmp\\debug.fil","ab");\
    fputs(message,debug_file);\
    fputs("\n",debug_file);\
    fclose(debug_file);\
  }

/**********************************************************************/

#define MAP map_ansi_sys_color_to_bios_attribute

int map_ansi_sys_color_to_bios_attribute(int iANSIcode)
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

int DISPLAY_COLUMNS = UNINITIALIZED;
int DISPLAY_ROWS = UNINITIALIZED;
  /* 0-based values, initialized from variables in dostty.c */
int FOREGROUND_ATTRIBUTE = UNINITIALIZED;
int BACKGROUND_ATTRIBUTE = UNINITIALIZED;
int NORMAL_VIDEO = UNINITIALIZED;
int REVERSE_VIDEO = UNINITIALIZED;

extern unsigned long RealModeBufferParagraph;
extern char *pRealModeBuffer;

void 
bios_initialize_variables (void)
/*
  If valid environment variables exist, use values. Otherwise
  queries BIOS for parameters.
*/
{
  char* psTemp;

  if (DISPLAY_COLUMNS == UNINITIALIZED)
    DISPLAY_COLUMNS = tty_x_size-1;

  if (DISPLAY_ROWS == UNINITIALIZED)
    DISPLAY_ROWS = tty_y_size-1;

  if (FOREGROUND_ATTRIBUTE == UNINITIALIZED)
    {
      psTemp = getenv("EDWIN_FOREGROUND");
      if (NULL == psTemp)
        {
          FOREGROUND_ATTRIBUTE = _0B(0,0,0,0,0,1,1,1); /* White */
        }
      else
        {
          if (atoi(psTemp) == 0)
            FOREGROUND_ATTRIBUTE = _0B(0,0,0,0,0,1,1,1); /* White */
          else
            FOREGROUND_ATTRIBUTE = MAP(atoi(psTemp));
        }
    }

  if (BACKGROUND_ATTRIBUTE == UNINITIALIZED)
    {
      psTemp = getenv("EDWIN_BACKGROUND");
      if (NULL == psTemp)
        {
          BACKGROUND_ATTRIBUTE = _0B(0,0,0,0,0,0,0,0) << 4; /* Black */
        }
      else
        {
          if (atoi(psTemp) == 0)
            BACKGROUND_ATTRIBUTE = _0B(0,0,0,0,0,0,0,0) << 4; /* Black */
          else
            BACKGROUND_ATTRIBUTE = MAP(atoi(psTemp)) << 4;
        }
    }

  NORMAL_VIDEO = (FOREGROUND_ATTRIBUTE | BACKGROUND_ATTRIBUTE);
  REVERSE_VIDEO = ((FOREGROUND_ATTRIBUTE << 4) | (BACKGROUND_ATTRIBUTE >> 4));
  return;
}

void bios_uninitialize_variables(void)
{
  DISPLAY_COLUMNS = UNINITIALIZED;
  DISPLAY_ROWS = UNINITIALIZED;
  FOREGROUND_ATTRIBUTE = UNINITIALIZED;
  BACKGROUND_ATTRIBUTE = UNINITIALIZED;
  NORMAL_VIDEO = UNINITIALIZED;
  REVERSE_VIDEO = UNINITIALIZED;
}

void bios__scroll_up_rectangle(int iBlankAttribute, int iLines,
  int iUpperLeftX, int iUpperLeftY,
  int iBottomRightX, int iBottomRightY)
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x06;
  rIn.h.al = iLines;
  rIn.h.bh = iBlankAttribute;
  rIn.h.ch = iUpperLeftY;
  rIn.h.cl = iUpperLeftX;
  rIn.h.dh = iBottomRightY;
  rIn.h.dl = iBottomRightX;
  int86(0x10,&rIn,&rOut);
}

void bios__scroll_down_rectangle(int iBlankAttribute, int iLines,
  int iUpperLeftX, int iUpperLeftY,
  int iBottomRightX, int iBottomRightY)
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x07;
  rIn.h.al = iLines;
  rIn.h.bh = iBlankAttribute;
  rIn.h.ch = iUpperLeftY;
  rIn.h.cl = iUpperLeftX;
  rIn.h.dh = iBottomRightY;
  rIn.h.dl = iBottomRightX;
  int86(0x10,&rIn,&rOut);
}

void bios__set_cursor_position(int iPageNumber, int iColumn, int iRow)
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x02;
  rIn.h.bh = iPageNumber;
  rIn.h.dh = iRow;
  rIn.h.dl = iColumn;
  int86(0x10,&rIn,&rOut);
}

void bios__write_char_with_attribute(char cChar, int iPageNumber,
  int iAttribute, int iRepeatCount)
  /* Note: no special characters are recognized */
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x09;
  rIn.h.al = cChar;
  rIn.h.bh = iPageNumber;
  rIn.h.bl = iAttribute;
  rIn.x.cx = iRepeatCount;

  int86(0x10,&rIn,&rOut);
}

void bios__teletype_output_char(char cChar, int iPageNumber,
  int iGraphicsModeForegroundColor)
  /* Note: CR/LF/BS/BEL recognized */
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x0E;
  rIn.h.al = cChar;
  rIn.h.bh = iPageNumber;
  rIn.h.bl = iGraphicsModeForegroundColor;

  int86(0x10,&rIn,&rOut);
}

void bios__set_video_mode(int iModeNumber)
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x00;
  rIn.h.al = iModeNumber;
  int86(0x10,&rIn,&rOut);
}

void bios__set_cursor_size(int iBlinkMode, int iTopScan, int iBottomScan)
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x01;
  rIn.h.ch = (((iBlinkMode & _0B(0,0,0,0,0,0,1,1)) << 5)
	      | (iTopScan & _0B(0,0,0,1,1,1,1,1)));
  rIn.h.cl = (iBottomScan & _0B(0,0,0,1,1,1,1,1));
  int86(0x10,&rIn,&rOut);
}

void bios_clear_line(int iCol, int iRow, int iFirstUnusedX)
{
  bios__scroll_up_rectangle(NORMAL_VIDEO, 0, iCol, iRow, iFirstUnusedX, iRow);
}

void bios_clear_region(int iUpperLeftX, int iUpperLeftY,
  int iBottomRightX, int iBottomRightY)
{
  bios__scroll_up_rectangle(NORMAL_VIDEO, 0,
    iUpperLeftX, iUpperLeftY, iBottomRightX, iBottomRightY);
}

void bios_clear_screen(void)
{
  bios__scroll_up_rectangle(NORMAL_VIDEO, 0x00,
    0, 0, DISPLAY_COLUMNS, DISPLAY_ROWS);
}

void bios_get_cursor_position(int *x, int *y)
{
  union REGS rIn;
  union REGS rOut;

  rIn.h.ah = 0x03;
  rIn.h.bh = 0x00; /* page number */
  int86(0x10,&rIn,&rOut);
  *y = rOut.h.dh;
  *x = rOut.h.dl;
}

void bios_write_string_attributed(char *pString, long lLength, int iColumn,
  int iRow, int iSingleAttribute)
{
  strncpy(pRealModeBuffer,pString,lLength);
  asm_bios__write_string_attr(1,0,iSingleAttribute,lLength,
    iColumn,iRow,((RealModeBufferParagraph << 16) + 0));
}

#ifdef USE_MAIN

int main()
{
 char BIGSTR[] = \
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ2þþþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ4þþþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ6þþþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ8þþþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ10þþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ12þþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ14þþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ16þþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ18þþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ20þþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ22þþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "12345678911234567892123456789312345678941234567895123456789612345678971234567898"
    "þþ24þþþþþ1þþþþþþþþþ2þþþþþþþþþ3þþþþþþþþþ4þþþþþþþþþ5þþþþþþþþþ6þþþþþþþþþ7þþþþþþþþþ8"
    "1234567891123456789212345678931234567894123456789512345678961234567897123456789";

  int i,j,k;

  printf(BIGSTR);
  bios_initialize();

  for (j=0; j<=24; j++)
    {
      for (i=0; i<=79; i++)
        {
          bios__set_cursor_position(0x00,i,j);
          bios__write_char_with_attribute(i+j,0x00,_0B(0,0,0,1,1,1,0,0),0x01);
        }
    }

  for (i=0; i<=24; i++)
  {
    bios__scroll_down_rectangle(i*4, 1, 0, 0, 39, 24);
    bios__scroll_up_rectangle(i*4, 1, 40, 0, 79, 24);
  }

  for (i=0; i<=24; i=i+2)
  {
    bios_clear_line(i);
  }

  for (i=0; i<=255; i++)
  {
    bios__teletype_output_char(i,0x00,0x00);
  }

  {
    char *pcTemp;
    pcTemp = BIGSTR;
    while (0x00 != *pcTemp)
      {
        bios__teletype_output_char(*pcTemp,0x00,0x00);
        pcTemp++;
      }
  }

  printf("\n%s",BIGSTR);
  for (i=5; i<75; i=i+4)
  {
    bios_clear_region(i,5,i+2,20);
  }

}

#endif /* USE_MAIN */

DEFINE_PRIMITIVE("BIOS:BEEP", Prim_bios_beep, 0, 0, 0)
{
  PRIMITIVE_HEADER(0);
  bios__teletype_output_char('\007',0,0x00);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:CLEAR-LINE!", Prim_bios_clear_line, 3, 3, 0)
{
  PRIMITIVE_HEADER(3);
  bios_clear_line(arg_integer(1),arg_integer(2),arg_integer(3));
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:CLEAR-RECTANGLE!", Prim_bios_clear_rectangle, 5, 5, 0)
  /* xl xu yl yu highlight */
{
  PRIMITIVE_HEADER(5);
  bios_clear_region(arg_integer(1),arg_integer(3),arg_integer(2),arg_integer(4));
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:CLEAR-SCREEN!", Prim_bios_clear_screen, 0, 0, 0)
{
  PRIMITIVE_HEADER(0);
  bios_clear_screen();
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:DISCARD!", Prim_bios_discard, 0, 0, 0)
{
  PRIMITIVE_HEADER(0);
  bios_uninitialize_variables();
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:ENTER!", Prim_bios_enter, 0, 0, 0)
{
  PRIMITIVE_HEADER(0);
  bios_initialize_variables ();
  bios_clear_screen();
  bios__set_cursor_position(0,0,DISPLAY_ROWS);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:EXIT!", Prim_bios_exit, 0, 0, 0)
{
  PRIMITIVE_HEADER(0);
  bios__set_cursor_position(0,0,DISPLAY_ROWS);
  PRIMITIVE_RETURN(SHARP_T);
}

/*
  flush!, modeline-event!, and discretionary-flush have no meaning
  for BIOS output, no corresponding primitives have been defined.
*/

DEFINE_PRIMITIVE("BIOS:SCROLL-LINES-DOWN!", Prim_bios_scroll_lines_down, 5, 5, 0)
  /* xl xu yl yu amount */
{
  PRIMITIVE_HEADER(5);
  bios__scroll_down_rectangle(NORMAL_VIDEO,arg_integer(5),
    arg_integer(1),arg_integer(3),arg_integer(2),arg_integer(4));
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:SCROLL-LINES-UP!", Prim_bios_scroll_lines_up, 5, 5, 0)
{
  PRIMITIVE_HEADER(5);
  bios__scroll_up_rectangle(NORMAL_VIDEO,arg_integer(5),
    arg_integer(1),arg_integer(3),arg_integer(2),arg_integer(4));
  PRIMITIVE_RETURN(SHARP_T);
}

/*
  console-wrap-update! has no meaning for BIOS output, no primitive defined.
*/

DEFINE_PRIMITIVE("BIOS:WRITE-CHAR!", Prim_bios_write_char, 2, 2, 0)
  /* char highlight */
{
  PRIMITIVE_HEADER(2);
  if (BOOLEAN_ARG(2))
    {
      int x,y;
      bios_get_cursor_position(&x,&y);
      bios__scroll_up_rectangle(REVERSE_VIDEO,1,x,y,x,y);
      bios__teletype_output_char(arg_ascii_char(1),0,0);
    }
  else
    bios__teletype_output_char(arg_ascii_char(1),0,0);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:WRITE-CURSOR!", Prim_bios_write_cursor, 2, 2, 0)
{
  PRIMITIVE_HEADER(2);
  bios__set_cursor_position(0,arg_integer(1),arg_integer(2));
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:WRITE-SUBSTRING!", Prim_bios_write_substring, 4, 4, 0)
  /* string start end highlight */
{
  long start, end;
  PRIMITIVE_HEADER(4);

  start = arg_integer (2);
  end = arg_integer (3);
  if (start > end)
    PRIMITIVE_RETURN(SHARP_F);

  {
    int x, y;

    bios_get_cursor_position (&x, &y);
    bios_write_string_attributed (((STRING_ARG(1)) + start), (end - start),
                                  x, y,
                                  ((BOOLEAN_ARG(4))
                                   ? REVERSE_VIDEO
                                   : NORMAL_VIDEO));
  }

  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:SET-VIDEO-MODE!", Prim_bios_set_video_mode, 1, 1, 0)
{
  PRIMITIVE_HEADER(1);
  bios__set_video_mode(arg_integer(1));
  pc_gestalt_screen_x_size();
  pc_gestalt_screen_y_size();
  bios_initialize_variables();
  bios_clear_screen();
  bios__set_cursor_position(0,0,DISPLAY_ROWS);
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE("BIOS:SET-CURSOR-SIZE!",Prim_bios_set_cursor_size, 3, 3, 0)
{
  extern void pc_gestalt_x_size(void);
  extern void pc_gestalt_y_size(void);

  PRIMITIVE_HEADER(3);
  bios__set_cursor_size(arg_integer(1),arg_integer(2),arg_integer(3));
  PRIMITIVE_RETURN(SHARP_T);
}
