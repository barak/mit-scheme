/* -*-C-*-

$Id: starbase.c,v 1.8 2002/11/20 19:46:14 cph Exp $

Copyright (c) 1989, 1990, 1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* Starbase graphics for HP 9000 machines. */

#include "scheme.h"
#include "prims.h"
#include <starbase.c.h>

static void
set_vdc_extent (descriptor, xmin, ymin, xmax, ymax)
     int descriptor;
     float xmin, ymin, xmax, ymax;
{
  vdc_extent (descriptor, xmin, ymin, (0.0), xmax, ymax, (0.0));
  clip_indicator (descriptor, CLIP_TO_VDC);
  clear_control (descriptor, CLEAR_VDC_EXTENT);
  return;
}

static void
set_line_color_index (descriptor, color_index)
     int descriptor;
     long color_index;
{
  line_color_index (descriptor, color_index);
  text_color_index (descriptor, color_index);
  perimeter_color_index (descriptor, color_index);
  fill_color_index (descriptor, color_index);
  return;
}

static int
inquire_cmap_size (fildes)
     int fildes;
{
  float physical_limits [2][3];
  float resolution [3];
  float p1 [3];
  float p2 [3];
  int cmap_size;
  inquire_sizes (fildes, physical_limits, resolution, p1, p2, (& cmap_size));
  return (cmap_size);
}

#define SB_DEVICE_ARG(arg) (arg_nonnegative_integer (arg))

DEFINE_PRIMITIVE ("STARBASE-OPEN-DEVICE", Prim_starbase_open_device, 2, 2,
  "(STARBASE-OPEN-DEVICE DEVICE-NAME DRIVER-NAME)")
{
  PRIMITIVE_HEADER (2);
  gerr_print_control (NO_ERROR_PRINTING);
  {
    int descriptor = (gopen ((STRING_ARG (1)), OUTDEV, (STRING_ARG (2)), 0));
    gerr_print_control (PRINT_ERRORS);
    if (descriptor == (-1))
      PRIMITIVE_RETURN (SHARP_F);
    set_vdc_extent (descriptor, (-1.0), (-1.0), (1.0), (1.0));
    mapping_mode (descriptor, DISTORT);
    set_line_color_index (descriptor, 1);
    line_type (descriptor, 0);
    drawing_mode (descriptor, 3);
    text_alignment
      (descriptor, TA_NORMAL_HORIZONTAL, TA_NORMAL_VERTICAL, (0.0), (0.0));
    interior_style (descriptor, INT_HOLLOW, 1);
    PRIMITIVE_RETURN (long_to_integer (descriptor));
  }
}

DEFINE_PRIMITIVE ("STARBASE-CLOSE-DEVICE", Prim_starbase_close_device, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  gclose (SB_DEVICE_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-FLUSH", Prim_starbase_flush, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  make_picture_current (SB_DEVICE_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-CLEAR", Prim_starbase_clear, 1, 1,
  "(STARBASE-CLEAR DEVICE)\n\
Clear the graphics section of the screen.\n\
Uses the Starbase CLEAR_VIEW_SURFACE procedure.")
{
  PRIMITIVE_HEADER (1);
  clear_view_surface (SB_DEVICE_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-DRAW-POINT", Prim_starbase_draw_point, 3, 3,
  "(STARBASE-DRAW-POINT DEVICE X Y)\n\
Draw one point at the given coordinates.\n\
Subsequently move the graphics cursor to those coordinates.\n\
Uses the starbase procedures `move2d' and `draw2d'.")
{
  PRIMITIVE_HEADER (3);
  {
    int descriptor = (SB_DEVICE_ARG (1));
    fast float x = (arg_real_number (2));
    fast float y = (arg_real_number (3));
    move2d (descriptor, x, y);
    draw2d (descriptor, x, y);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-MOVE-CURSOR", Prim_starbase_move_cursor, 3, 3,
  "(STARBASE-MOVE-CURSOR DEVICE X Y)\n\
Move the graphics cursor to the given coordinates.\n\
Uses the starbase procedure `move2d'.")
{
  PRIMITIVE_HEADER (3);
  move2d ((SB_DEVICE_ARG (1)), (arg_real_number (2)), (arg_real_number (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-DRAG-CURSOR", Prim_starbase_drag_cursor, 3, 3,
  "(STARBASE-DRAG-CURSOR DEVICE X Y)\n\
Draw a line from the graphics cursor to the given coordinates.\n\
Subsequently move the graphics cursor to those coordinates.\n\
Uses the starbase procedure `draw2d'.")
{
  PRIMITIVE_HEADER (3);
  draw2d ((SB_DEVICE_ARG (1)), (arg_real_number (2)), (arg_real_number (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-DRAW-LINE", Prim_starbase_draw_line, 5, 5,
  "(STARBASE-DRAW-LINE DEVICE X-START Y-START X-END Y-END)\n\
Draw a line from the start coordinates to the end coordinates.\n\
Subsequently move the graphics cursor to the end coordinates.\n\
Uses the starbase procedures `move2d' and `draw2d'.")
{
  PRIMITIVE_HEADER (5);
  {
    int descriptor = (SB_DEVICE_ARG (1));
    fast float x_start = (arg_real_number (2));
    fast float y_start = (arg_real_number (3));
    fast float x_end = (arg_real_number (4));
    fast float y_end = (arg_real_number (5));
    move2d (descriptor, x_start, y_start);
    draw2d (descriptor, x_end, y_end);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-SET-LINE-STYLE", Prim_starbase_set_line_style, 2, 2,
  "(STARBASE-SET-LINE-STYLE DEVICE STYLE)\n\
Changes the line drawing style.\n\
The STYLE argument is an integer in the range 0-7 inclusive.\n\
See the description of the starbase procedure `line_type'.")
{
  PRIMITIVE_HEADER (2);
  line_type ((SB_DEVICE_ARG (1)), (arg_index_integer (2, 8)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-SET-DRAWING-MODE", Prim_starbase_set_drawing_mode, 2, 2,
  "(STARBASE-SET-DRAWING-MODE DEVICE MODE)\n\
Changes the replacement rule used when drawing.\n\
The MODE argument is an integer in the range 0-15 inclusive.\n\
See the description of the starbase procedure `drawing_mode'.")
{
  PRIMITIVE_HEADER (2);
  drawing_mode ((SB_DEVICE_ARG (1)), (arg_index_integer (2, 16)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-DEVICE-COORDINATES", Prim_starbase_device_coordinates, 1, 1, 0)
{
  float physical_limits [2][3];
  float resolution [3];
  float p1 [3];
  float p2 [3];
  int cmap_size;
  PRIMITIVE_HEADER (1);
  inquire_sizes
    ((SB_DEVICE_ARG (1)), physical_limits, resolution, p1, p2, (& cmap_size));
  {
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 4, true));
    VECTOR_SET (result, 0, (FLOAT_TO_FLONUM (physical_limits[0][0])));
    VECTOR_SET (result, 1, (FLOAT_TO_FLONUM (physical_limits[0][1])));
    VECTOR_SET (result, 2, (FLOAT_TO_FLONUM (physical_limits[1][0])));
    VECTOR_SET (result, 3, (FLOAT_TO_FLONUM (physical_limits[1][1])));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("STARBASE-SET-VDC-EXTENT", Prim_starbase_set_vdc_extent, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  set_vdc_extent
    ((SB_DEVICE_ARG (1)),
     (arg_real_number (2)),
     (arg_real_number (3)),
     (arg_real_number (4)),
     (arg_real_number (5)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-RESET-CLIP-RECTANGLE", Prim_starbase_reset_clip_rectangle, 1, 1,
  "(STARBASE-RESET-CLIP-RECTANGLE DEVICE)\n\
Undo the clip rectangle.  Subsequently, clipping is not affected by it.")
{
  PRIMITIVE_HEADER (1);
  {
    int descriptor = (SB_DEVICE_ARG (1));
    clip_indicator (descriptor, CLIP_TO_VDC);
    clear_control (descriptor, CLEAR_VDC_EXTENT);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-SET-CLIP-RECTANGLE", Prim_starbase_set_clip_rectangle, 5, 5,
  "(STARBASE-SET-CLIP-RECTANGLE X-LEFT Y-BOTTOM X-RIGHT Y-TOP)\n\
Restrict the graphics drawing primitives to the area in the given rectangle.")
{
  PRIMITIVE_HEADER (5);
  {
    int descriptor = (SB_DEVICE_ARG (1));
    fast float x_left = (arg_real_number (2));
    fast float y_bottom = (arg_real_number (3));
    fast float x_right = (arg_real_number (4));
    fast float y_top = (arg_real_number (5));
    clip_rectangle (descriptor, x_left, x_right, y_bottom, y_top);
    clip_indicator (descriptor, CLIP_TO_RECT);
    clear_control (descriptor, CLEAR_CLIP_RECTANGLE);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-DRAW-TEXT", Prim_starbase_draw_text, 4, 4,
  "(STARBASE-DRAW-TEXT DEVICE X Y STRING)")
{
  PRIMITIVE_HEADER (4);
  text2d
    ((SB_DEVICE_ARG (1)),
     (arg_real_number (2)),
     (arg_real_number (3)),
     (STRING_ARG (4)),
     VDC_TEXT,
     FALSE);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-SET-TEXT-HEIGHT", Prim_starbase_set_text_height, 2, 2,
  "(STARBASE-SET-TEXT-HEIGHT DEVICE HEIGHT)")
{
  PRIMITIVE_HEADER (2);
  character_height ((SB_DEVICE_ARG (1)), (arg_real_number (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-SET-TEXT-ASPECT", Prim_starbase_set_text_aspect, 2, 2,
  "(STARBASE-SET-TEXT-ASPECT DEVICE ASPECT)")
{
  PRIMITIVE_HEADER (2);
  character_expansion_factor ((SB_DEVICE_ARG (1)), (arg_real_number (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-SET-TEXT-SLANT", Prim_starbase_set_text_slant, 2, 2,
  "(STARBASE-SET-TEXT-SLANT DEVICE SLANT)")
{
  PRIMITIVE_HEADER (2);
  character_slant ((SB_DEVICE_ARG (1)), (arg_real_number (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-SET-TEXT-ROTATION", Prim_starbase_set_text_rotation, 2, 2,
  "(STARBASE-SET-TEXT-ROTATION DEVICE ANGLE)")
{
  PRIMITIVE_HEADER (2);
  {
    fast float angle = (arg_real_number (2));
    fast int path_style;
    if ((angle > 315.0) || (angle <=  45.0))
      path_style = PATH_RIGHT;
    else if ((angle > 45.0) && (angle <= 135.0))
      path_style = PATH_DOWN;
    else if ((angle > 135.0) && (angle <= 225.0))
      path_style = PATH_LEFT;
    else if ((angle > 225.0) && (angle <= 315.0))
      path_style = PATH_UP;
    text_path ((SB_DEVICE_ARG (1)), path_style);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-COLOR-MAP-SIZE", Prim_starbase_color_map_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (inquire_cmap_size (SB_DEVICE_ARG (1))));
}

DEFINE_PRIMITIVE ("STARBASE-DEFINE-COLOR", Prim_starbase_define_color, 5, 5,
  "(STARBASE-DEFINE-COLOR DEVICE COLOR-INDEX RED GREEN BLUE)\n\
COLOR-INDEX must be a valid index for the current device's color map.\n\
RED, GREEN, and BLUE must be numbers between 0 and 1 inclusive.\n\
Changes the color map, defining COLOR-INDEX to be the given RGB color.")
{
  int descriptor;
  float colors [1][3];
  PRIMITIVE_HEADER (5);
  descriptor = (SB_DEVICE_ARG (1));
  (colors [0] [0]) = (arg_real_number (3));
  (colors [0] [1]) = (arg_real_number (4));
  (colors [0] [2]) = (arg_real_number (5));
  define_color_table
    (descriptor,
     (arg_index_integer (2, (inquire_cmap_size (descriptor)))),
     1,
     colors);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("STARBASE-SET-LINE-COLOR", Prim_starbase_set_line_color, 2, 2,
  "(STARBASE-SET-LINE-COLOR DEVICE COLOR-INDEX)\n\
COLOR-INDEX must be a valid index for the current device's color map.\n\
Changes the color used for drawing most things.\n\
Does not take effect until the next starbase output operation.")
{
  int descriptor;
  PRIMITIVE_HEADER (2);
  descriptor = (SB_DEVICE_ARG (1));
  set_line_color_index
    (descriptor, (arg_index_integer (2, (inquire_cmap_size (descriptor)))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Graphics Screen Dump */

static void print_graphics ();

DEFINE_PRIMITIVE ("STARBASE-WRITE-IMAGE-FILE", Prim_starbase_write_image_file, 3, 3,
  "(STARBASE-WRITE-IMAGE-FILE DEVICE FILENAME INVERT?)\n\
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
