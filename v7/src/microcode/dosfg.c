/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosfg.c,v 1.2 1992/05/08 19:05:26 mhwu Exp $

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

/* Zortech 'Flash Graphics' primitives */

#include "scheme.h"
#include "prims.h"
#include <fg.h>

#define ARG_FG_COORD	arg_nonnegative_integer

/* Internal state to emulate Starbase behaviour */
typedef struct fg_state_struct
{
  fg_color_t color;
  fg_color_t background;
  fg_coord_t x;
  fg_coord_t y;
  fg_box_t clip;
  int mode;
  int line_type;
  int mask;
  int rotation;
} fg_state_t;

static fg_state_t current;

/* This makes sure that the variables will not be changed unless
   both args are of the right type.
 */

#define Get_Scheme_Coordinates(xp, x, yp, y)		\
do							\
{ fg_coord_t _tmp_x, _tmp_y;				\
  _tmp_x = (ARG_FG_COORD(xp));				\
  _tmp_y = (ARG_FG_COORD(yp));				\
  (x) = _tmp_x, (y) = _tmp_y;				\
} while (0)


DEFINE_PRIMITIVE ("FG-OPEN", Prim_fg_open, 0, 0,
"Initializes the graphics display.")
{
  PRIMITIVE_HEADER (0);
  {
    if (fg_init() != 0)
    {
      current.color	= FG_WHITE;
      current.background= FG_BLACK;
      current.line_type	= FG_LINE_SOLID;
      current.mode 	= FG_MODE_SET;
      current.rotation	= FG_ROT0;
      current.mask	= (int) (~0);
      current.x 	= (fg_coord_t) 0;
      current.y 	= (fg_coord_t) 0;
      
      fg_box_cpy(current.clip, fg.displaybox);

      { int i;
	for (i=1000000L; i > 0; i--);
      }

      PRIMITIVE_RETURN (SHARP_T);
    }
    else
      PRIMITIVE_RETURN (SHARP_F);
  }
}

DEFINE_PRIMITIVE ("FG-CLOSE", Prim_fg_close, 0, 0,
"Closes the graphics device.")
{
  PRIMITIVE_HEADER (0);
  fg_term();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-CLEAR", Prim_fg_clear, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  fg_fillbox(current.background, current.mode, current.mask, fg.displaybox);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-DRAW-POINT", Prim_fg_draw_point, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  { 
    Get_Scheme_Coordinates(1, current.x, 2, current.y);

    if (fg_pt_inbox(current.clip, current.x, current.y))
      fg_drawdot(current.color, current.mode, current.mask,
		 current.x, current.y);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-MOVE-CURSOR", Prim_fg_move_cursor, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {     
    Get_Scheme_Coordinates(1, current.x, 2, current.y);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-DRAG-CURSOR", Prim_fg_drag_cursor, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  { fg_coord_t x, y;
    fg_line_t line, cline;

    Get_Scheme_Coordinates(1, x, 2, y);

    fg_make_line(line, current.x, current.y, x, y);
    (void) fg_lineclip(current.clip, line, cline);
    fg_drawline(current.color, current.mode, current.mask,
		current.line_type, cline);
    current.x = x, current.y = y;
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-DRAW-LINE", Prim_fg_draw_line, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  { fg_coord_t x, y;
    fg_line_t line, cline;

    Get_Scheme_Coordinates(1, x, 2, y);
    Get_Scheme_Coordinates(3, current.x, 4, current.y);

    fg_make_line(line, x, y, current.x, current.y);
    (void) fg_lineclip(current.clip, line, cline);
    fg_drawline(current.color, current.mode, current.mask,
		current.line_type, cline);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-SET-LINE-STYLE", Prim_fg_set_line_style, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  current.line_type = (int) arg_index_integer(1, FG_LINE_MAX);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-SET-DRAWING-MODE", Prim_fg_set_drawing_mode, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  { int mode = (int) arg_index_integer(1, 2);
    
    current.mode = (mode == 0) ? FG_MODE_SET : FG_MODE_XOR;
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-DEVICE-COORDINATES", Prim_fg_device_coordinates, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  { SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 4, true));

    VECTOR_SET (result, 0, long_to_integer(fg.displaybox[FG_X1]));
    VECTOR_SET (result, 1, long_to_integer(fg.displaybox[FG_Y1]));
    VECTOR_SET (result, 2, long_to_integer(fg.displaybox[FG_X2])); 
    VECTOR_SET (result, 3, long_to_integer(fg.displaybox[FG_Y2]));

    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("FG-RESET-CLIP-RECTANGLE", Prim_fg_reset_clip_rectangle,
		  0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    fg_box_cpy(current.clip, fg.displaybox);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-SET-CLIP-RECTANGLE", Prim_fg_set_clip_rectangle, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  { fg_coord_t x1, y1, x2, y2;
    
    Get_Scheme_Coordinates(1, x1, 2, y1);
    Get_Scheme_Coordinates(1, x2, 2, y2);

    if (fg_pt_inbox(fg.displaybox, x1, y1) == 0)
      error_bad_range_arg(1);
    if (fg_pt_inbox(fg.displaybox, x1, y1) == 0)
      error_bad_range_arg(3);
    
    current.clip[FG_X1] = x1;
    current.clip[FG_Y1] = y1;
    current.clip[FG_X2] = x2;
    current.clip[FG_Y2] = y2;
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-DRAW-TEXT", Prim_fg_draw_text, 3, 3,
  "(FG-DRAW-TEXT DEVICE X Y STRING)")
{
  PRIMITIVE_HEADER (3);
  {
    fg_coord_t x, y;
    unsigned char * string = STRING_ARG(3);

    Get_Scheme_Coordinates(1, x, 2, y);
    fg_puts(current.color, current.mode, current.mask,
	    current.rotation, x, y, string, current.clip);
  }    
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-SET-TEXT-ROTATION", Prim_fg_set_text_rotation, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    long angle = ((long) (arg_real_number (1))) % 360;
    int path_style = FG_ROT0;

    if ((angle > 315) || (angle <=  45))
      path_style = FG_ROT0;
    else if ((angle > 45) && (angle <= 135))
      path_style = FG_ROT90;
    else if ((angle > 135) && (angle <= 225))
      path_style = FG_ROT180;
    else if ((angle > 225) && (angle <= 315))
      path_style = FG_ROT270;
    
    current.rotation = path_style;
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-COLOR-MAP-SIZE", Prim_fg_color_map_size, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (fg.ncolormap));
}

DEFINE_PRIMITIVE ("FG-DEFINE-COLOR", Prim_fg_define_color, 4, 4,
  "(FG-DEFINE-COLOR COLOR-INDEX RED GREEN BLUE)")
{
  PRIMITIVE_HEADER (4);
  {
    fg_color_t index, r, g, b;
    
    index = arg_index_integer(1, fg.ncolormap);
    r = arg_real_number(2);
    g = arg_real_number(3);
    b = arg_real_number(4);

    fg_setpalette(index, r, g, b);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FG-SET-LINE-COLOR", Prim_fg_set_line_color, 1, 1,
  "(FG-SET-LINE-COLOR COLOR-INDEX)")
{
  PRIMITIVE_HEADER (1);
  current.color = arg_index_integer(1, fg.ncolormap);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#if 0
/* Graphics Screen Dump */

static void print_graphics ();

UNDEFINE_PRIMITIVE ("FG-WRITE-IMAGE-FILE", Prim_fg_write_image_file, 3, 3,
  "(FG-WRITE-IMAGE-FILE DEVICE FILENAME INVERT?)\n\
Write a file containing an image of the DEVICE's screen, in a format\n\
suitable for printing on an HP laserjet printer.\n\
If INVERT? is not #F, invert black and white in the output.")
{
  PRIMITIVE_HEADER (3);
  print_graphics ((SB_DEVICE_ARG (1)), (STRING_ARG (2)), (BOOLEAN_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static char rasres[] = "\033*t100R";
static char rastop[] = "\033&l2E";
static char raslft[] = "\033&a2L";
static char rasbeg[] = "\033*r0A";
static char raslen[] = "\033*b96W";
static char rasend[] = "\033*rB";

static int
inquire_cmap_mask (fildes)
     int fildes;
{
  int cmap_size = (inquire_cmap_size (fildes));
  return
    (((cmap_size >= 0) && (cmap_size < 8))
     ? ((1 << cmap_size) - 1)
     : (-1));
}

static int
open_dumpfile (dumpname)
  char * dumpname;
{
  int dumpfile = (creat (dumpname, 0666));
  if (dumpfile == (-1))
    {
      fprintf (stderr, "\nunable to create graphics dump file.");
      fflush (stderr);
      error_external_return ();
    }
  dumpfile = (open (dumpname, OUTINDEV));
  if (dumpfile == (-1))
    {
      fprintf (stderr, "\nunable to open graphics dump file.");
      fflush (stderr);
      error_external_return ();
    }
  return (dumpfile);
}

static void
print_graphics (descriptor, dumpname, inverse_p)
     int descriptor;
     char * dumpname;
     int inverse_p;
{
  int dumpfile = (open_dumpfile (dumpname));
  write (dumpfile, rasres, (strlen (rasres)));
  write (dumpfile, rastop, (strlen (rastop)));
  write (dumpfile, raslft, (strlen (raslft)));
  write (dumpfile, rasbeg, (strlen (rasbeg)));
  {
    fast unsigned char mask = (inquire_cmap_mask (descriptor));
    int col;
    for (col = (1024 - 16); (col >= 0); col = (col - 16))
      {
	unsigned char pixdata [(16 * 768)];
	{
	  fast unsigned char * p = (& (pixdata [0]));
	  fast unsigned char * pe = (& (pixdata [sizeof (pixdata)]));
	  while (p < pe)
	    (*p++) = '\0';
	}
	dcblock_read (descriptor, col, 0, 16, 768, pixdata, 0);
	{
	  int x;
	  for (x = (16 - 1); (x >= 0); x -= 1)
	    {
	      unsigned char rasdata [96];
	      fast unsigned char * p = (& (pixdata [x]));
	      fast unsigned char * r = rasdata;
	      int n;
	      for (n = 0; (n < 96); n += 1)
		{
		  fast unsigned char c = 0;
		  int nn;
		  for (nn = 0; (nn < 8); nn += 1)
		    {
		      c <<= 1;
		      if (((* p) & mask) != 0)
			c |= 1;
		      p += 16;
		    }
		  (*r++) = (inverse_p ? (~ c) : c);
		}
	      write (dumpfile, raslen, (strlen (raslen)));
	      write (dumpfile, rasdata, 96);
	    }
	}
      }
  }
  write (dumpfile, rasend, (strlen (rasend)));
  close (dumpfile);
  return;
}
#endif
