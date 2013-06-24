/* -*-C-*-

$Id: sgraph_a.c,v 1.21 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

#include "scheme.h"
#include "prims.h"
#include "sgraph.h"
#include "array.h"
#include "x11.h"

#define SB_DEVICE_ARG(arg) (arg_nonnegative_integer (arg))

#ifndef STARBASE_COLOR_TABLE_START
#define STARBASE_COLOR_TABLE_START 0
#endif

#ifndef STARBASE_COLOR_TABLE_SIZE
#define STARBASE_COLOR_TABLE_SIZE 16
#endif

float Color_Table [STARBASE_COLOR_TABLE_SIZE] [3];

static void
arg_plotting_box (arg_number, plotting_box)
     int arg_number;
     float * plotting_box;
{
  fast SCHEME_OBJECT object;
  fast int i;
  TOUCH_IN_PRIMITIVE ((ARG_REF (arg_number)), object);
  for (i = 0; (i < 4); i += 1)
    {
      if (! (PAIR_P (object)))
	error_wrong_type_arg (arg_number);
      {
	fast SCHEME_OBJECT number = (PAIR_CAR (object));
	if (! (REAL_P (number)))
	  error_wrong_type_arg (arg_number);
	if (! (real_number_to_double_p (number)))
	  error_bad_range_arg (arg_number);
	(plotting_box [i]) = (real_number_to_double (number));
      }
      TOUCH_IN_PRIMITIVE ((PAIR_CDR (object)), object);
    }
  if (!EMPTY_LIST_P (object))
    error_wrong_type_arg (arg_number);
  return;
}

DEFINE_PRIMITIVE ("XPLOT-ARRAY-0", 
		  Prim_xplot_array_0, 6, 6, 
		  "(XPLOT-ARRAY-0 WINDOW ARRAY BOX OFFSET SCALE FILL)")
{
  SCHEME_OBJECT array;
  float plotting_box [4];
  REAL offset, scale;
  PRIMITIVE_HEADER (6);
  { 
    struct xwindow * xw = (x_window_arg (1));
    CHECK_ARG (2, ARRAY_P);
    array = (ARG_REF (2));
    arg_plotting_box (3, plotting_box);
    offset = (arg_real (4));	/* arg_real is defined in array.h */
    scale = (arg_real (5));
    XPlot_C_Array_With_Offset_Scale
      (xw,
       (ARRAY_CONTENTS (array)),
       (ARRAY_LENGTH (array)),
       plotting_box,
       (arg_index_integer (6, 2)),
       offset,
       scale);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

/* The following are taken from x11graphics.c 
 */

struct gw_extra
{
  float x_left;
  float x_right;
  float y_bottom;
  float y_top;
  float x_slope;
  float y_slope;
  int x_cursor;
  int y_cursor;
};

#define XW_EXTRA(xw) ((struct gw_extra *) ((xw) -> extra))

#define XW_X_LEFT(xw) ((XW_EXTRA (xw)) -> x_left)
#define XW_X_RIGHT(xw) ((XW_EXTRA (xw)) -> x_right)
#define XW_Y_BOTTOM(xw) ((XW_EXTRA (xw)) -> y_bottom)
#define XW_Y_TOP(xw) ((XW_EXTRA (xw)) -> y_top)
#define XW_X_SLOPE(xw) ((XW_EXTRA (xw)) -> x_slope)
#define XW_Y_SLOPE(xw) ((XW_EXTRA (xw)) -> y_slope)
#define XW_X_CURSOR(xw) ((XW_EXTRA (xw)) -> x_cursor)
#define XW_Y_CURSOR(xw) ((XW_EXTRA (xw)) -> y_cursor)

#define ROUND_FLOAT(flonum)						\
  ((int) (((flonum) >= 0.0) ? ((flonum) + 0.5) : ((flonum) - 0.5)))

static int
xmake_x_coord (xw, virtual_device_x)
     struct xwindow * xw;
     float virtual_device_x;
{
  float device_x = ((XW_X_SLOPE (xw)) * (virtual_device_x - (XW_X_LEFT (xw))));
  return (ROUND_FLOAT (device_x));
}

static int
xmake_y_coord (xw, virtual_device_y)
     struct xwindow * xw;
     float virtual_device_y;
{
  float device_y =
    ((XW_Y_SLOPE (xw)) * (virtual_device_y - (XW_Y_BOTTOM (xw))));
  return (((XW_Y_SIZE (xw)) - 1) + (ROUND_FLOAT (device_y)));
}

XPlot_C_Array_With_Offset_Scale (xw, Array, Length, Plotting_Box, 
				 fill_with_lines, Offset, Scale)
     struct xwindow * xw;
     float *Plotting_Box;
     long Length;
     int fill_with_lines;	/* plots filled with lines from 0 to y(t) */
     REAL *Array, Scale, Offset;
{
  float box_x_min = Plotting_Box[0];
  float box_y_min = Plotting_Box[1];
  float box_x_max = Plotting_Box[2];
  float box_y_max = Plotting_Box[3];
  float Box_Length = box_x_max - box_x_min;
  float Box_Height = box_y_max - box_y_min;
  long i;
  float        v_d_clipped_offset;
  fast float   v_d_x, v_d_y, v_d_x_increment; /* virtual device coordinates */
  fast int    x, y, clipped_offset; /* X window coordinates */
  fast int    internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
  
  v_d_x = box_x_min;		/* horizontal starting point */
  v_d_x_increment = ((float) Box_Length/Length);
  
  if (fill_with_lines == 0)
    {				/* plot just the points */
      for (i = 0; i < Length; i++)
	{
	  x =     (xmake_x_coord (xw, v_d_x));
	  v_d_y = ((float) (Offset + (Scale * Array[i])));
	  y =     (xmake_y_coord (xw, v_d_y));
	  
	  XDrawPoint
	    ((XW_DISPLAY (xw)),
	     (XW_WINDOW (xw)),
	     (XW_NORMAL_GC (xw)),
	     (internal_border_width + x),
	     (internal_border_width + y));
	  
	  v_d_x = v_d_x + v_d_x_increment;

	  /* Can not use INTEGERS x+x_increment because x_increment
	     may round to 0 and we'll never move the cursor from
	     starting point.  Also Array[i+skip] will not work well,
	     say 1024 points for 1000 places => last 24 are chopped
	     whereas the costly loop we do, downsamples in between
	     more gracefully.  i.e. loop over v_d_coordinates - in
	     floats. */
	}
    }
  else
    {				/* fill with lines */
      v_d_clipped_offset = min( max(box_y_min, ((float) Offset)), box_y_max);
      clipped_offset     = (xmake_y_coord (xw, v_d_clipped_offset));
      /* The above allows us to 
	 fill with vertical bars from the zero-line  to the graphed point y(x)
	 and never go outside box. 
	 */
      for (i = 0; i < Length; i++)
	{
	  x =     (xmake_x_coord (xw, v_d_x));
	  v_d_y = ((float) (Offset + (Scale * Array[i])));
	  y =     (xmake_y_coord (xw, v_d_y));
	  
	  XDrawLine
	    ((XW_DISPLAY (xw)),
	     (XW_WINDOW (xw)),
	     (XW_NORMAL_GC (xw)),
	     (internal_border_width + x),
	     (internal_border_width + clipped_offset),
	     (internal_border_width + x),
	     (internal_border_width + y));
	  
	  v_d_x = v_d_x + v_d_x_increment;
	}
    }
}

/* plot-array-0 is suffixed -0   in case we need more versions of array plot */

DEFINE_PRIMITIVE ("PLOT-ARRAY-0", 
		  Prim_plot_array_0, 6, 6, 
		  "(PLOT-ARRAY-0 DEVICE ARRAY BOX OFFSET SCALE FILL)")
{
  SCHEME_OBJECT array;
  float plotting_box [4];
  REAL offset, scale;
  int device;
  PRIMITIVE_HEADER (6);
  device = (SB_DEVICE_ARG (1));
  
  CHECK_ARG (2, ARRAY_P);
  array = (ARG_REF (2));
  arg_plotting_box (3, plotting_box);
  offset = (arg_real (4));	/* arg_real is defined in array.h */
  scale = (arg_real (5));
  Plot_C_Array_With_Offset_Scale
    (device,
     (ARRAY_CONTENTS (array)),
     (ARRAY_LENGTH (array)),
     plotting_box,
     (arg_index_integer (6, 2)),
     offset,
     scale);
  PRIMITIVE_RETURN
    (cons ((double_to_flonum ((double) (offset))),
	   (cons ((double_to_flonum ((double) (scale))),
		  EMPTY_LIST))));
}

Plot_C_Array_With_Offset_Scale (device, Array, Length, Plotting_Box, 
				fill_with_lines, Offset, Scale)
     int device; 
     float *Plotting_Box; long Length;
     int fill_with_lines;	/* plots filled with lines from 0 to y(t) */
     REAL *Array, Scale, Offset;
{
  float box_x_min = Plotting_Box[0];
  float box_y_min=Plotting_Box[1];
  float box_x_max = Plotting_Box[2];
  float box_y_max = Plotting_Box[3];
  float Box_Length = box_x_max - box_x_min;
  float Box_Height = box_y_max - box_y_min;
  fast float x_position, y_position, index_inc, clipped_offset;
  long i;

  index_inc = ((float) Box_Length/Length);
  x_position = box_x_min;
  if (fill_with_lines == 0)
    {				/* plot just the points */
      for (i = 0; i < Length; i++)
	{
	  y_position = ((float) (Offset + (Scale * Array[i])));
	  move2d(device, x_position, y_position);
	  draw2d(device, x_position, y_position);
	  x_position = x_position + index_inc;
	}
    }
  else
    {				/* fill with lines */
      clipped_offset = min( max(box_y_min, ((float) Offset)), box_y_max);
      /* Fill from zero-line but do not go outside box,
	 (Don't bother with starbase clipping)
	 */
      for (i = 0; i < Length; i++)
	{
	  y_position = ((float) (Offset + (Scale * Array[i])));
	  move2d(device, x_position, clipped_offset);
	  draw2d(device, x_position, y_position);
	  x_position = x_position + index_inc;
	}
    }
  make_picture_current(device);
}

DEFINE_PRIMITIVE ("POLYGON2D", Prim_polygon2d, 2,2, 0)
{
  float clist [TWICE_MAX_NUMBER_OF_CORNERS];
  int count;
  fast SCHEME_OBJECT object;
  int device; 
  PRIMITIVE_HEADER (2); 
  
  device = (SB_DEVICE_ARG (1));
  CHECK_ARG (2, PAIR_P);
  count = 0;
    
  TOUCH_IN_PRIMITIVE ((ARG_REF (2)), object);
  while (PAIR_P (object))
  {
    fast SCHEME_OBJECT number = (PAIR_CAR (object));
    if (! (REAL_P (number)))
      error_wrong_type_arg (2);
    if (! (real_number_to_double_p (number)))
      error_bad_range_arg (2);
    (clist [count]) = (real_number_to_double (number));
    count += 1;
    if (count == (TWICE_MAX_NUMBER_OF_CORNERS - 2))
      error_bad_range_arg (2);
    TOUCH_IN_PRIMITIVE ((PAIR_CDR (object)), object);
  }
  if (!EMPTY_LIST_P (object))
    error_wrong_type_arg (2);

  (clist [count]) = (clist [0]);
  (clist [count + 1]) = (clist [1]);
  polygon2d (device, clist, ((long) ((count + 2) / 2)), 0);
  make_picture_current (device);
  PRIMITIVE_RETURN (UNSPECIFIC);
}


DEFINE_PRIMITIVE ("BOX-MOVE", Prim_box_move, 3,3, 0)
{
  int device;
  float From_Box[4];		/* x_min, y_min, x_max, y_max */
  float To_Box[4];
  float x_source, y_source, x_dest, y_dest, x_length, y_length;
  PRIMITIVE_HEADER (3);
  device = (SB_DEVICE_ARG (1));  
  arg_plotting_box (2, From_Box);
  arg_plotting_box (3, To_Box);
  x_source = From_Box[0]; y_source = From_Box[3];
  x_dest   =   To_Box[0]; y_dest   =   To_Box[3];
  /* notice convention of matrix row, column! */
  y_length = From_Box[3] - From_Box[1] + 1;
  x_length = From_Box[2] - From_Box[0] + 1;
  if ((y_length != (To_Box[3]-To_Box[1]+1)) ||
      (x_length != (To_Box[2]-To_Box[0]+1)))
    error_bad_range_arg (3);
  block_move
    (device,
     x_source, y_source,
     ((int) x_length), ((int) y_length),
     x_dest, y_dest);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Image Drawing (halftoning)
   HG = Hard Grey levels (i.e. output device greys)
   SG = Soft Grey levels (i.e. simulated grey levels)
   There are 3 methods: PSAM, OD, BN (see below)
   There are also the old 16-color drawing routines. */

/* PSAM (Pulse-Surface-Area Modulation) works only for 2 HG grey
   levels.  It maps 1 pxl to a square of 16 pxls.  The distribution of
   on/off pxls in the square gives 16 grey levels.  It's the most
   efficient for B&W monitors, but see below for better quality
   drawing using OD and BN.  Halftoning using OD and BN works for any
   number of grey levels, and there are many methods available (see
   below).

   IMAGE-PSAM-ATXY-WMM fixed magnification 1pxl->16pxls Draw line
   (width 4) by line.  Pdata space needed = (4 * ncols * 16).  The
   following 2 primitives simply take in arguments, and allocate
   space, They call C_image_psam_atxy_wmm to do the actual drawing. */

DEFINE_PRIMITIVE ("IMAGE-PSAM-ATXY-WMM", Prim_image_psam_atxy_wmm, 6,6, 0)
{
  int device;
  long nrows, ncols;
  REAL * Array;
  PRIMITIVE_HEADER (6);
  device = (SB_DEVICE_ARG (1));
  arg_image (2, (&nrows), (&ncols), (&Array));
  Primitive_GC_If_Needed (BYTES_TO_WORDS (16 * ncols));
  C_image_psam_atxy_wmm
    (device, Array,
     ((unsigned char *) Free),
     nrows,
     ncols,
     ((float) (arg_real (3))),
     ((float) (arg_real (4))),
     (arg_real (5)),
     (arg_real (6)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("IMAGE-PSAM-ATXY-WOMM", Prim_image_psam_atxy_womm, 6,6, 0)
{
  int device;
  long nrows, ncols;
  REAL * Array;
  PRIMITIVE_HEADER (6);
  device = (SB_DEVICE_ARG (1));
  arg_image (2, (&nrows), (&ncols), (&Array));
  Primitive_GC_If_Needed (BYTES_TO_WORDS (16 * ncols));
  C_image_psam_atxy_womm
    (device, Array,
     ((unsigned char *) Free),
     nrows,
     ncols,
     ((float) (arg_real (3))),
     ((float) (arg_real (4))),
     (arg_real (5)),
     (arg_real (6)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("IMAGE-HT-OD-ATXY-WMM", Prim_image_ht_od_atxy_wmm, 8,8, 0)
{
  int device;
  long nrows, ncols;
  REAL * Array;
  PRIMITIVE_HEADER (8);
  device = (SB_DEVICE_ARG (1));
  arg_image (2, (&nrows), (&ncols), (&Array));
  Primitive_GC_If_Needed (BYTES_TO_WORDS (ncols));
  C_image_ht_od_atxy_wmm
    (device, Array,
     ((unsigned char *) Free),
     nrows,
     ncols,
     ((float) (arg_real (3))),
     ((float) (arg_real (4))),
     (arg_real (5)),
     (arg_real (6)),
     (arg_integer_in_range (7, 1, 257)),
     (arg_integer_in_range (8, 0, 8)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("IMAGE-HT-BN-ATXY-WMM", Prim_image_ht_bn_atxy_wmm, 8,8, 0)
{
  int device;
  long nrows, ncols;
  REAL * Array;
  unsigned char * pdata;
  float ** er_rows;
  PRIMITIVE_HEADER (8);
  device = (SB_DEVICE_ARG (1));
  arg_image (2, (&nrows), (&ncols), (&Array));
  Primitive_GC_If_Needed
    (BYTES_TO_WORDS
     (/* pdata */
      ncols +
      /* er_rows header */
      (3 * (sizeof (float *))) +
      /* er_rows data */
      (3 * (ncols + 4) * (sizeof (float)))));
  pdata = ((unsigned char *) Free);
  er_rows = ((float **) (pdata + ncols));
  (er_rows [0]) = ((float *) (er_rows + 3));
  (er_rows [1]) = ((er_rows [0]) + (ncols + 4));
  (er_rows [2]) = ((er_rows [1]) + (ncols + 4));
  C_image_ht_bn_atxy_wmm
    (device, Array,
     pdata,
     nrows,
     ncols,
     ((float) (arg_real (3))),
     ((float) (arg_real (4))),
     (arg_real (5)),
     (arg_real (6)),
     (arg_integer_in_range (7, 1, 257)),
     (arg_nonnegative_integer (8)),
     er_rows);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define MINTEGER long

DEFINE_PRIMITIVE ("IMAGE-HT-IBN-ATXY-WMM", Prim_image_ht_ibn_atxy_wmm, 9,9, 0)
{
  int device;
  long nrows, ncols;
  REAL * Array;
  unsigned char * pdata;
  MINTEGER ** er_rows;
  PRIMITIVE_HEADER (9);
  device = (SB_DEVICE_ARG (1));
  arg_image (2, (&nrows), (&ncols), (&Array));
  Primitive_GC_If_Needed
    (BYTES_TO_WORDS
     (/* pdata */
      ncols +
      /* er_rows header */
      (3 * (sizeof (MINTEGER *))) +
      /* er_rows data */
      (3 * (ncols + 4) * (sizeof (MINTEGER)))));
  pdata = ((unsigned char *) Free);
  er_rows = ((MINTEGER **) (pdata + ncols));
  (er_rows [0]) = ((MINTEGER *) (er_rows + 3));
  (er_rows [1]) = (er_rows [0]) + (ncols + 4);
  (er_rows [2]) = (er_rows [1]) + (ncols + 4);
  C_image_ht_ibn_atxy_wmm
    (device, Array,
     pdata,
     nrows,
     ncols,
     ((float) (arg_real (3))),
     ((float) (arg_real (4))),
     (arg_real (5)),
     (arg_real (6)),
     (arg_integer_in_range (7, 1, 257)),
     (arg_index_integer (8, 3)),
     er_rows,
     (arg_integer_in_range
      (9, 1, ((1 << ((8 * (sizeof (MINTEGER))) - 2)) / 64))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* THE FOLLOWING 3 ROUTINES ARE THE OLD 16-color drawing routines
   they also do magnification. */

/* color_table entries 0 and 1 are not used */
/* Just like in array-plotting,
   Use Min,Max and Offset,Scale s.t. values map into [2,15] */

#define SCREEN_BACKGROUND_COLOR 0
#define MINIMUM_INTENSITY_INDEX 2
#define MAXIMUM_INTENSITY_INDEX 15

/* ARGS = (device image x_at y_at magnification)
   magnification can be 1, 2, or 3 */

DEFINE_PRIMITIVE ("DRAW-MAGNIFY-IMAGE-AT-XY", Prim_draw_magnify_image_at_xy, 5, 5, 0)
{
  int device;
  long nrows, ncols, Length;
  REAL * Array;
  long Magnification;
  REAL Offset, Scale;
  REAL Array_Min, Array_Max;
  long nmin, nmax;
  PRIMITIVE_HEADER (5);
  device = (SB_DEVICE_ARG (1));
  arg_image (2, (&nrows), (&ncols), (&Array));
  Magnification = (arg_integer_in_range (5, 1, 101));
  Length = (nrows * ncols);
  {
    C_Array_Find_Min_Max (Array, Length, &nmin, &nmax);
    Array_Min = (Array [nmin]);
    Array_Max = (Array [nmax]);
    /* Do not use colors 0 and 1 */
    Find_Offset_Scale_For_Linear_Map
      (Array_Min, Array_Max, 2.0, 15.0, &Offset, &Scale);
    Primitive_GC_If_Needed (BYTES_TO_WORDS (Magnification * ncols));
    Image_Draw_Magnify_N_Times_With_Offset_Scale
      (device, Array,
       ((unsigned char *) Free),
       nrows,
       ncols,
       ((float) (arg_real (3))),
       ((float) (arg_real (4))),
       Offset,
       Scale,
       Magnification);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("DRAW-MAGNIFY-IMAGE-AT-XY-WITH-MIN-MAX",
		  Prim_draw_magnify_image_at_xy_with_min_max, 7,7, 0)
{
  int device;
  long nrows, ncols;
  REAL * Array;
  REAL Offset, Scale;
  long Magnification;
  PRIMITIVE_HEADER (7);
  device = (SB_DEVICE_ARG (1));
  arg_image (2, (&nrows), (&ncols), (&Array));
  Magnification = (arg_integer_in_range (5, 1, 101));
  /* Do not use colors 0 and 1 */
  Find_Offset_Scale_For_Linear_Map
    ((arg_real (6)), (arg_real (7)), 2.0, 15.0, &Offset, &Scale);
  Primitive_GC_If_Needed (BYTES_TO_WORDS (Magnification * ncols));
  Image_Draw_Magnify_N_Times_With_Offset_Scale
    (device, Array,
     ((unsigned char *) Free),
     nrows,
     ncols,
     ((float) (arg_real (3))),
     ((float) (arg_real (4))),
     Offset,
     Scale,
     Magnification);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DRAW-MAGNIFY-IMAGE-AT-XY-ONLY-BETWEEN-MIN-MAX",
		  Prim_draw_magnify_image_at_xy_only_between_min_max, 7,7, 0)
{
  int device;
  long nrows, ncols;
  REAL * Array;
  REAL Offset, Scale;
  long Magnification;
  PRIMITIVE_HEADER (7);
  device = (SB_DEVICE_ARG (1));
  arg_image (2, (&nrows), (&ncols), (&Array));
  Magnification = (arg_integer_in_range (5, 1, 101));
  /* Do not use colors 0 and 1 */
  Find_Offset_Scale_For_Linear_Map
    ((arg_real (6)), (arg_real (7)), 2.0, 15.0, &Offset, &Scale);
  Primitive_GC_If_Needed (BYTES_TO_WORDS (Magnification * ncols));
  Image_Draw_Magnify_N_Times_With_Offset_Scale_Only
    (device, Array,
     ((unsigned char *) Free),
     nrows,
     ncols,
     ((float) (arg_real (3))),
     ((float) (arg_real (4))),
     Offset,
     Scale,
     Magnification);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Below are the real drawing routines */

/* ht = halftoning
   od = ordered-dither (dispersed dot), Ulichney terminology
   bn = blue noise (also called: minimized average error)
   psam = pulse surface area modulation
   Also, there are the old drawing routines for 16 colors, which are basically
   fixed threshold ordered dither. */

/* The macro Adjust_Value_Wmm is used by most drawing routines.
   The macro Adjust_Value_Womm is used only by psam-atxy-womm.
   REAL value, newvalue, ngreys_min, ngreys_max, Vmin,Vmax, offset,scale;
   offset, scale must be such as to map (min,max)
   into (ngreys_min,ngreys_max) */

#define Adjust_Value_Wmm(value, newvalue, ngreys_min, ngreys_max, Vmin, Vmax, offset, scale) \
{									\
  if (value >= Vmax)							\
    newvalue = ngreys_max;						\
  else if (value <= Vmin)						\
    newvalue = ngreys_min;						\
  else									\
    newvalue = offset + (value * scale);				\
}

#define Adjust_Value_Womm(value, newvalue, ngreys_min, ngreys_max, Vmin, Vmax, offset, scale) \
{									\
  if (value >= Vmax)							\
    newvalue = ngreys_min;						\
  else if (value <= Vmin)						\
    newvalue = ngreys_min;						\
  else									\
    newvalue = offset + (value * scale);				\
}

#define Round_REAL(x) ((long) ((x >= 0) ? (x+.5) : (x-.5)))

/* Ordered Dither MASKS
   A mask is a SQUARE matrix of threshold values,
   that is effectively replicated periodically all over the image.

   ht_od_table[][0] --->  int SG;               number of soft greys
   ht_od_table[][1] --->   int SGarray_nrows;
    nrows=ncols i.e. square matrix of threshold values
   ht_od_table[][2+i] ----> int SGarray[36];
    threshold values with range [0,SG).

   ATTENTION: Currently, the LARGEST SGarray is 6X6 MATRIX
  */

static int ht_od_table[8][2+36] =
{ {2,1, 1},			/* fixed threshold at halfpoint */
    /* this one and following 4 come from Ulichney p.135 */
  {3,2, 1,2,2,1},
  {5,3, 2,3,2, 4,1,4, 2,3,2},
  {9,4, 1,8,2,7, 5,3,6,4, 2,7,1,8, 6,4,5,3},
  {17,5, 2,16,3,13,2, 10,6,11,7,10, 4,14,1,15,4, 12,8,9,5,12, 2,16,3,13,2},
  {33,6, 1,30,8,28,2,29, 17,9,24,16,18,10, 5,25,3,32,6,26, 21,13,19,11,22,14,
     2,29,7,27,1,30, 18,10,23,15,17,9},
    /* this one and following 1 come from Jarvis,Judice,Ninke: CGIP 5, p.23 */
  {4,2, 0,2,3,1},
  {17,4, 0,8,2,10, 12,4,14,6, 3,11,1,9, 15,7,13,5}
};
#define HT_OD_TABLE_MAX_INDEX 7

/* ordered dither
   pdata must have length ncols
   HG= Hardware Grey levels (output pixel values 0,HG-1)
   ODmethod is index for ht_od method
   */

C_image_ht_od_atxy_wmm (device, Array, pdata, nrows,ncols, x_at,y_at, Min,Max,
			HG,ODmethod)
     int device; 
     REAL Array[], Min,Max;
     unsigned char *pdata;
     int nrows,ncols,HG,ODmethod;
     float x_at,y_at;
{ int i,j, SG, *SGarray, SGarray_nrows, dither, pixel, array_index;
  REAL    REAL_pixel, value, offset,scale, HG1_SG;
  /* static int ht_od_table[][]; */
  /* void Find_Offset_Scale_For_Linear_Map(); */

  if (ODmethod>HT_OD_TABLE_MAX_INDEX)
    error_external_return ();
  SG = ht_od_table[ODmethod][0];
  SGarray_nrows = ht_od_table[ODmethod][1]; /* nrows=ncols   */
  SGarray = &(ht_od_table[ODmethod][2]);    /* square matrix */

  HG1_SG = ((REAL) ((HG-1)*SG));
  Find_Offset_Scale_For_Linear_Map
    (Min, Max, 0.0, HG1_SG,  &offset, &scale); /* HG output greys */
  array_index=0;
  for (i=0; i<nrows; i++)
  { for (j=0; j<ncols; j++)
    { value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, HG1_SG, Min,Max, offset,scale);
      /* Turn into integer--- integer arithmetic gives speed */
      pixel = ((long) REAL_pixel);
      /* this special case is necessary to avoid ouput_pxl greater than.. */
      if (pixel == HG1_SG) pixel = pixel-1;
      dither = SGarray[ (i%SGarray_nrows)*SGarray_nrows + (j%SGarray_nrows) ];
      /* integer division */ }
      pdata[j] = ((unsigned char) ((pixel + SG - dither) / SG));
    block_write(device, x_at, (y_at-((float) i)), ncols, 1, pdata, 0);
  }
}

/* Blue Noise (minimized average error)
   pdata must have length ncols
   HG= Hardware Grey levels (output pixel values 0,HG-1)
   BNmethod is index for ht_bn method

   er_rows[][] should be 3 arrays of integers, of length (ncols+2*ER_C),
   which store previous errors, (ALLOCATED STORAGE)
   ER_R is number of error rows, (currently 3)
   ER_C is number of extra columns (to the left and to the right)
   of each er_row, they always contain ZEROS and serve to simplify the
   error summing process, i.e. we don't have to check for i,j bounds
   at edges, (no conditionals in the sum loop).  Also, the code handles
   all cases in a uniform manner (for better explanation get PAS
   halftoning notes). */

C_image_ht_bn_atxy_wmm (device, Array, pdata, nrows, ncols, x_at, y_at,
			Min, Max, HG, BNmethod, er_rows)
     int device;
     REAL Array [], Min, Max;
     unsigned char * pdata;
     int nrows, ncols;
     int HG, BNmethod;
     float x_at, y_at;
     float ** er_rows;
{
  if (BNmethod == 0)
    C_image_ht_bn_atxy_wmm_0_
      (device, Array, pdata, nrows, ncols, x_at, y_at, Min, Max, HG, er_rows);
  else if (BNmethod == 1)
    C_image_ht_bn_atxy_wmm_1_
      (device, Array, pdata, nrows, ncols, x_at, y_at, Min, Max, HG, er_rows);
  else if (BNmethod == 2)
    C_image_ht_bn_atxy_wmm_2_
      (device, Array, pdata, nrows, ncols, x_at, y_at, Min, Max, HG, er_rows);
  else
    {
      fprintf (stderr, "\nHT_BN methods 0,1,2 only\n");
      fflush (stderr);
    }
}

/* the following 3 routines are identical,
   except for the mask weight numbers in computing ersum,
   the sole reason for this duplication is speed (if any) */

/* FLOYD-STEINBERG-75 */
C_image_ht_bn_atxy_wmm_0_ (device, Array, pdata, nrows, ncols, x_at, y_at,
			   Min, Max, HG, er_rows)
     int device;
     REAL Array[], Min,Max;
     unsigned char *pdata;
     int nrows,ncols,HG;
     float x_at,y_at,  **er_rows;
{
  int i, j, m, array_index;
  int row_offset, col_offset, INT_pixel;
  REAL REAL_pixel, value, offset,scale, HG1_2;
  float ersum, weight, pixel;
  static int
    ER_R = 3,
    ER_R1 = 2,
    ER_C = 2,
    ER_C1 = 1;

  /* initialize error rows */
  for (i = 0; (i < ER_R); i += 1)
    for (j = 0; (j < (ncols + (2 * ER_C))); j += 1)
      (er_rows [i] [j]) = 0.0;
  /* notice this is REAL number */
  HG1_2 = ((REAL) ((HG - 1) * 2));
  /* HG output greys */
  Find_Offset_Scale_For_Linear_Map (Min, Max, 0.0, HG1_2, &offset, &scale);
  array_index = 0;
  for (i = 0; (i < nrows); i += 1)
    {
      for (j = 0; (j < ncols); j += 1)
	{
	  ersum =
	    (((1.0 / 16.0) * (er_rows [ER_R1 - 1] [ER_C + j - 1])) +
	     ((5.0 / 16.0) * (er_rows [ER_R1 - 1] [ER_C + j])) +
	     ((3.0 / 16.0) * (er_rows [ER_R1 - 1] [ER_C + j + 1])) +
	     ((7.0 / 16.0) * (er_rows [ER_R1] [ER_C + j - 1])));
	  /* this encodes the FLOYD-STEINBERG-75 mask for computing
	     the average error correction */
	  value = (Array [array_index++]);
	  Adjust_Value_Wmm
	    (value, REAL_pixel, 0.0, HG1_2, Min, Max, offset, scale);
	  /* corrected intensity */
	  pixel = (((float) REAL_pixel) + ersum);
	  /* the (long) does truncation, this corresponds to "IF J>R/2 R 0" */
	  INT_pixel = ((long) ((pixel + 1) / 2.0));
	  /* output pixel to be painted */
	  (pdata [j]) = ((unsigned char) INT_pixel);
	  /* error estimate */
	  (er_rows [ER_R1] [ER_C + j]) = ((pixel / 2.0) - ((float) INT_pixel));
	}
      /* paint a row */
      block_write
	(device, x_at, (y_at - ((float) i)), ncols, 1, pdata, 0);
      /* rotate rows */
      {
	float * temp = (er_rows [0]);
	(er_rows [0]) = (er_rows [1]);
	(er_rows [1]) = (er_rows [2]);
	(er_rows [2]) = temp;
      }
      /* initialize (clean up) the new error row */
      for (m = ER_C; (m < ncols); m += 1)
	(er_rows [2] [m]) = 0.0;
    }
}

/* JARVIS-JUDICE-NINKE-76 mask */
C_image_ht_bn_atxy_wmm_1_ (device, Array, pdata, nrows,ncols, x_at,y_at,
			   Min,Max, HG, er_rows)
     int device;
     REAL Array[], Min,Max;
     unsigned char *pdata;
     int nrows,ncols,HG;
     float x_at,y_at,  **er_rows;
{ int i,j, m, array_index;
  int row_offset, col_offset, INT_pixel;
  REAL    REAL_pixel, value, offset,scale, HG1_2;
  float ersum, weight, pixel, *temp;
  static int ER_R=3, ER_R1=2, ER_C=2, ER_C1=1;

  /* initialize error rows */
  for (i=0;i<ER_R;i++)
    for (j=0;j<ncols+(2*ER_C);j++)
      er_rows[i][j] = 0.0;
  HG1_2 = ((REAL) ((HG-1)*2));	/* notice this is REAL number */
  /* HG output greys */
  Find_Offset_Scale_For_Linear_Map
    (Min, Max, 0.0, HG1_2,  &offset, &scale);
  array_index=0;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      ersum =
	((1.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(-2)+j] +
	 (3.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(-1)+j] +
	 (5.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(0)+j] +
	 (3.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(1)+j] +
	 (1.0/48.0)*er_rows[ER_R1+(-2)][ER_C+(2)+j] +
	 (3.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(-2)+j] +
	 (5.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(-1)+j] +
	 (7.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(0)+j] +
	 (5.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(1)+j] +
	 (3.0/48.0)*er_rows[ER_R1+(-1)][ER_C+(2)+j] +
	 (5.0/48.0)*er_rows[ER_R1+(0)][ER_C+(-2)+j] +
	 (7.0/48.0)*er_rows[ER_R1+(0)][ER_C+(-1)+j]);
      /* this encodes the JARVIS-JUDICE-NINKE-76 mask
	 for computating the average error correction */
      value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, HG1_2, Min,Max, offset,scale);
      /* */
      pixel = ((float) REAL_pixel) + ersum; /*     corrected intensity */
      /* the (long) does truncation, this corresponds to "IF J>R/2 R 0" */
      INT_pixel = ((long) ((pixel + 1) / 2.0));
      pdata[j] = ((unsigned char) INT_pixel); /* output pixel to be painted */
      /* error estimate */
      er_rows[ER_R1][ER_C +j] = (pixel/2.0) - ((float) INT_pixel);
    }
    /* paint a row */
    block_write(device, x_at, (y_at-((float) i)), ncols, 1, pdata, 0);
    temp = er_rows[0];		/* rotate rows */
    er_rows[0] = er_rows[1];
    er_rows[1] = er_rows[2];
    er_rows[2] = temp;
    /* initialize (clean up) the new error row */
    for (m=ER_C;m<ncols;m++) er_rows[2][m]=0.0;
  }
}

/* STUCKI-81 mask */
C_image_ht_bn_atxy_wmm_2_ (device, Array, pdata, nrows,ncols, x_at,y_at,
			   Min,Max, HG, er_rows)
     int device;
     REAL Array[], Min,Max;
     unsigned char *pdata;
     int nrows,ncols,HG;
     float x_at,y_at,  **er_rows;
{ int i,j, m, array_index;
  int row_offset, col_offset, INT_pixel;
  REAL    REAL_pixel, value, offset,scale, HG1_2;
  float ersum, weight, pixel, *temp;
  static int ER_R=3, ER_R1=2, ER_C=2, ER_C1=1;

  for (i=0;i<ER_R;i++)
    for (j=0;j<ncols+(2*ER_C);j++)
      er_rows[i][j] = 0.0;
  HG1_2 = ((REAL) ((HG-1)*2));
  Find_Offset_Scale_For_Linear_Map(Min, Max, 0.0, HG1_2,  &offset, &scale);
  array_index=0;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      ersum =
	((1.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(-2)+j] +
	 (2.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(-1)+j] +
	 (4.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(0)+j] +
	 (2.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(1)+j] +
	 (1.0/42.0)*er_rows[ER_R1+(-2)][ER_C+(2)+j] +
	 (2.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(-2)+j] +
	 (4.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(-1)+j] +
	 (8.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(0)+j] +
	 (4.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(1)+j] +
	 (2.0/42.0)*er_rows[ER_R1+(-1)][ER_C+(2)+j] +
	 (4.0/42.0)*er_rows[ER_R1+(0)][ER_C+(-2)+j] +
	 (8.0/42.0)*er_rows[ER_R1+(0)][ER_C+(-1)+j]);
      /* this encodes the STUCKI-81 mask
	 for computating the average error correction */
      value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, HG1_2, Min,Max, offset,scale);
      /* */
      pixel = ((float) REAL_pixel) + ersum; /* corrected intensity */
      /* the (long) does truncation, this corresponds to "IF J>R/2 R 0" */
      INT_pixel = ((long) ((pixel + 1) / 2.0));
      pdata[j] = ((unsigned char) INT_pixel); /* output pixel to be painted */
      /*  error estimate */
      er_rows[ER_R1][ER_C +j] = (pixel/2.0) - ((float) INT_pixel);
    }
    block_write (device, x_at, (y_at-((float) i)), ncols, 1, pdata, 0);
    temp = er_rows[0];		/* rotate rows */
    er_rows[0] = er_rows[1];
    er_rows[1] = er_rows[2];
    er_rows[2] = temp;
    /* initialize (clean up) the new error row */
    for (m=ER_C;m<ncols;m++)
      er_rows[2][m]=0.0;
  }
}

/* INTEGER BLUE NOISE
   pdata must have length ncols
   HG= Hardware Grey levels (output pixel values 0,HG-1)
   BNmethod is index for ht_ibn method

   IBN = integer blue noise
   uses integer arithmetic for speed, but also has different effect
   depending on the scaling of the integer intensities and error-corrections.
   A scale of PREC_SCALE=4 gives a very clear picture, with EDGE-INHANCEMENT.
   */

/*
  ht_ibn_table[][0] --->  int BN;               sum of error weights
  ht_ibn_table[][1] --->   int BNentries;       number of weight entries
  ht_ibn_table[][2+i+0,1,2] ----> int row_offset,col_offset,weight;
  */

static int ht_ibn_table[3][2+(3*12)] =
{ {16,4,  -1,-1,1, -1,0,5,  -1,1,3,  0,-1,7},
  {48,12, -2,-2,1, -2,-1,3, -2,0,5, -2,1,3, -2,2,1,
          -1,-2,3, -1,-1,5, -1,0,7, -1,1,5, -1,2,3,
           0,-2,5,  0,-1,7},
  {42,12, -2,-2,1, -2,-1,2, -2,0,4, -2,1,2, -2,2,1,
          -1,-2,2, -1,-1,4, -1,0,8, -1,1,4, -1,2,2,
           0,-2,4,  0,-1,8}
};

/*
  er_rows[][] should be 3 arrays of integers, of length (ncols+2*ER_C),
  which store previous errors, (ALLOCATED STORAGE)
  ER_R is number of error rows, (currently 3)
  ER_C is number of extra columns (to the left and right) of each er_row,
  they always contain ZEROS and serve to simplify the error summing process,
  i.e. we don't have to check for i,j bounds at edges
  (no conditionals in the sum loop).
  Also, the code handles all cases in a uniform manner.
  (for better explanation get pas halftoning notes) */

C_image_ht_ibn_atxy_wmm (device, Array, pdata, nrows,ncols, x_at,y_at, Min,Max,
			 HG,BNmethod, er_rows, PREC_SCALE)
     int device; 
     REAL Array[], Min,Max;
     unsigned char *pdata;
     int nrows,ncols,HG,BNmethod;
     MINTEGER   **er_rows, PREC_SCALE;
     float x_at,y_at;
{ int i,j, m, BNentries, array_index, row_offset, col_offset;
  MINTEGER  BN, ersum, weight, PREC_2, PREC, *temp, pixel;
  /* PREC is a scale factor that varies the precision in ersum
     -- using integer arithmetic for speed */
  REAL REAL_pixel, value, offset,scale, HG1_2_PREC;
  static int ER_R=3, ER_R1=2, ER_C=2, ER_C1=1;

  for (i=0;i<ER_R;i++)
    for (j=0;j<ncols+(2*ER_C);j++) er_rows[i][j] = 0;
  BN = ((MINTEGER) ht_ibn_table[BNmethod][0]);
  BNentries = ht_ibn_table[BNmethod][1];
  HG1_2_PREC = ((REAL) PREC_SCALE);
  /* HG1_2_PREC = ((REAL) ( (1<<( 8*(sizeof(MINTEGER))-1 )) / BN)); */
  /* max_intensity   maps to  (max_integer/BN), so that */
  /* neither ersum*BN nor (max_intensity + ersum) overflow */
  PREC_2 = ((MINTEGER) HG1_2_PREC) / ((MINTEGER) (HG-1));
  PREC   = PREC_2 / 2;
  Find_Offset_Scale_For_Linear_Map
    (Min, Max, 0.0, HG1_2_PREC, &offset, &scale);
  array_index=0;
  for (i=0;i<nrows;i++) {
    for (j=0;j<ncols;j++) {
      ersum=0;
      for (m=0;m<(3*BNentries); m=m+3)
	{
	  row_offset = ht_ibn_table[BNmethod][2+m+0]; /* should be 0,1,2 */
	  col_offset = ht_ibn_table[BNmethod][2+m+1];
	  weight = ((MINTEGER) ht_ibn_table[BNmethod][2+m+2]);
	  ersum += weight * er_rows[ER_R1+row_offset][ER_C +j+ col_offset];
	}
      ersum = ersum / BN;
      value = Array[array_index++];
      Adjust_Value_Wmm
	(value, REAL_pixel, 0.0, HG1_2_PREC, Min,Max, offset,scale);
      pixel = ((MINTEGER) REAL_pixel);
      pixel = pixel + ersum;	/* corrected intensity */
      ersum = ((pixel + PREC) / PREC_2);
      pdata[j] = ((unsigned char) ersum);
      er_rows[ER_R1][ER_C +j] = pixel - (PREC_2*ersum);
    }
    block_write (device, x_at, (y_at-((float) i)), ncols, 1, pdata, 0);
    temp = er_rows[0];		/* rotate rows */
    er_rows[0] = er_rows[1];
    er_rows[1] = er_rows[2];
    er_rows[2] = temp;
    for (m=0;m<(ncols+(2*ER_C));m++) er_rows[2][m]=0;
  }
}

/* PSAM drawing (see scheme primitives definition for description)
   Pdata must be (16 * ncols) bytes in size. */

C_image_psam_atxy_wmm(device, Array, pdata, nrows, ncols, x_origin, y_origin,
		      Min,Max)
     int device;
     REAL Array[], Min,Max;
     unsigned char *pdata; /* pdata should have length 16*4*ncols */
     long nrows, ncols;
     float x_origin, y_origin;
{ register long i,j, i4;
  register long array_index, pdata_index;
  long ncols4 = 4 * ncols;
  long color_index;
  REAL REAL_pixel, value, offset,scale;

  Find_Offset_Scale_For_Linear_Map
    (Min, Max, 0.0, 15.0,  &offset, &scale); /* 16 grey levels */
  
  array_index=0;    i4=0;
  for (i=0; i<nrows; i++) 
  { pdata_index = 0;
    for (j=0; j<ncols; j++) 
    { value = Array[array_index++];
      Adjust_Value_Wmm(value, REAL_pixel, 0.0, 15.0, Min,Max, offset,scale);
      color_index = ((long) (REAL_pixel + .5));	/* integer between 0 and 15 */
      /* */
      my_write_dither(pdata, pdata_index, ncols4, color_index);
      /* dependency between this and my_write_dither */
      pdata_index = pdata_index + 4;
    }
    block_write(device, x_origin, y_origin-i4, ncols4, 4, pdata, 0);
    i4 = i4+4;
  }
  /* A(i,j) --> Array[i*ncols + j] */
}

/* Same as above, except use Adjust_Value_Womm.
 */
C_image_psam_atxy_womm(device, Array, pdata, nrows, ncols, x_origin, y_origin,
		       Min,Max)
     int device;
     REAL Array[], Min,Max;
     unsigned char *pdata; /* pdata should have length 16*4*ncols */
     long nrows, ncols;
     float x_origin, y_origin;
{ register long i,j, i4;
  register long array_index, pdata_index;
  long ncols4 = 4*ncols;
  long color_index;
  REAL REAL_pixel, value, offset,scale;
  
  Find_Offset_Scale_For_Linear_Map
    (Min, Max, 0.0, 15.0,  &offset, &scale); /* 16 grey levels */
  array_index=0;    i4=0;
  for (i=0; i<nrows; i++) 
  { pdata_index = 0;
    for (j=0; j<ncols; j++) 
    { value = Array[array_index++];
      Adjust_Value_Womm(value, REAL_pixel, 0.0, 15.0, Min,Max, offset,scale);
      /* ONLY DIFFERENCE WITH PREVIOUS ONE */
      color_index = ((long) (REAL_pixel + .5));	/* integer between 0 and 15 */
      /* */
      my_write_dither(pdata, pdata_index, ncols4, color_index);
      /* dependency between this and my_write_dither */
      pdata_index = pdata_index + 4;
    }
    block_write(device, x_origin, y_origin-i4, ncols4, 4, pdata, 0);
    i4 = i4+4;
  }
  /* A(i,j) --> Array[i*ncols + j] */
}

/* psam dither[11] is left out, { 1,1,0,1, 1,1,1,0, 0,1,1,0, 1,0,1,1 } */

/* The following routine writes a 4x4 dither cell
   in 4 consecutive rows of pdata. It assumes a lot about
   pdata and the other args passed to it. READ carefully.
   Designed TO BE USED BY C_image_psam_atxy_wmm
*/

my_write_dither(pdata, pdata_row_index, ncols , color_index)
     unsigned char *pdata;
     long pdata_row_index, ncols;
     long color_index; /* should be 0 to 15 */
{ static unsigned char dither_table[16][16] =
    {{ 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0 },
       { 0,0,0,0, 0,1,0,0, 0,0,0,0, 0,0,0,0 },
       { 0,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,0 },
       { 0,0,0,0, 0,1,1,0, 0,0,1,0, 0,0,0,0 },
       { 0,0,0,0, 0,1,1,0, 0,1,1,0, 0,0,0,0 },
       { 1,0,0,0, 0,1,1,0, 0,1,1,0, 0,0,0,0 },
       { 1,0,0,0, 0,1,1,0, 0,1,1,0, 0,0,0,1 },
       { 1,0,0,1, 0,1,1,0, 0,1,1,0, 0,0,0,1 },
       { 1,0,0,1, 0,1,1,0, 0,1,1,0, 1,0,0,1 },
       { 1,1,0,1, 0,1,1,0, 0,1,1,0, 1,0,0,1 },
       { 1,1,0,1, 1,1,1,0, 0,1,1,0, 1,0,0,1 },
       { 1,1,0,1, 1,1,1,0, 0,1,1,1, 1,0,1,1 },
       { 1,1,0,1, 1,1,1,0, 1,1,1,1, 1,0,1,1 },
       { 1,1,1,1, 1,1,1,0, 1,1,1,1, 1,0,1,1 },
       { 1,1,1,1, 1,1,1,0, 1,1,1,1, 1,1,1,1 },
       { 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1 }};
  long i, row_start,m;
  long dither_index;		/* do not mix up the counters, indexes */
  dither_index=0;
  for (i=0;i<4;i++) { row_start = pdata_row_index + (i*ncols);
		      for (m=row_start; m<row_start+4; m++) 
			pdata[m] = dither_table[color_index][dither_index++]; }
}

/* Below are the OLD DRAWING ROUTINES for 16 color monitors.
   In effect they are fixed threshold, with 16 HG levels.
   The only difference is they also do magnification by replicating pixels.
   */

/* Image_Draw_Magnify_N_Times : N^2 in area
 */
Image_Draw_Magnify_N_Times_With_Offset_Scale
  (device, Array, pdata, nrows, ncols, x_origin,y_origin,Offset,Scale,N)
     int device;
     REAL Array[], Offset, Scale;
     unsigned char *pdata;
     long nrows, ncols, N;
     float x_origin, y_origin;
{ fast long i,j,m;
  fast long array_index;
  long ncolsN= N * ncols;
  long nrowsN= N * nrows;
  fast unsigned char pixel;
  fast REAL REAL_pixel;

  array_index = 0;
  for (i = 0; i < nrowsN;)	/* note that i is NOT incremented here */
  { for (j = 0; j < ncolsN;)	/* note that j is NOT incremented here */
    { REAL_pixel = Offset + (Array[array_index++] * Scale);
      if (REAL_pixel > 15.0)
	pixel = MAXIMUM_INTENSITY_INDEX;
      else if (REAL_pixel < 2.0)
	pixel = MINIMUM_INTENSITY_INDEX;
      else
	pixel = ((unsigned char) (Round_REAL(REAL_pixel)));
      for (m=0; m<N; m++) { pdata[j] = pixel;
			    j++; }
    }
    for (m=0; m<N; m++) {
      block_write(device, x_origin, y_origin-i, ncolsN, 1, pdata, 0);
      i++; }
    /* A(i,j) --> Array[i*ncols + j] */
  }
}

/* Image_Draw_Magnify_N_Times_Only : N^2 in area
   This procedure throws away (i.e. maps to SCREEN_BACKGROUND_COLOR)
   all values outside the range given by Offset,Scale.
   */
Image_Draw_Magnify_N_Times_With_Offset_Scale_Only
  (device, Array, pdata, nrows, ncols, x_origin, y_origin, Offset, Scale, N)
     int device;
     REAL Array[], Offset, Scale;
     unsigned char *pdata;
     long nrows, ncols, N;
     float x_origin, y_origin;
{ fast long i,j,m;
  fast long array_index;
  long ncolsN= N * ncols;
  long nrowsN= N * nrows;
  fast unsigned char pixel;
  fast REAL REAL_pixel;

  array_index = 0;
  for (i=0; i<nrowsN;)	/* note that i is NOT incremented here */
  { for (j=0; j<ncolsN;)	/* note that j is NOT incremented here */
    { REAL_pixel = Offset + (Array[array_index++] * Scale);
      if (REAL_pixel > 15.0)
	pixel = SCREEN_BACKGROUND_COLOR;
      else if (REAL_pixel < 2.0)
	pixel = SCREEN_BACKGROUND_COLOR;
      else
	pixel = ((unsigned char) (Round_REAL(REAL_pixel)));
      for (m=0; m<N; m++)
      {	pdata[j] = pixel;
	j++; }
    }
    for (m=0; m<N; m++) {
      block_write(device, x_origin, y_origin - i, ncolsN, 1, pdata, 0);
      i++; }
    /* A(i,j) --> Array[i*ncols + j] */
  }
}

/* Grey Level Manipulations */

DEFINE_PRIMITIVE ("NEW-COLOR", Prim_new_color, 5,5, 0)
{
  int device;
  long index;
  PRIMITIVE_HEADER (5);
  device = (SB_DEVICE_ARG (1));
  index =
    (arg_integer_in_range
     (2, STARBASE_COLOR_TABLE_START, STARBASE_COLOR_TABLE_SIZE));
  inquire_color_table
    (device,
     STARBASE_COLOR_TABLE_START,
     STARBASE_COLOR_TABLE_SIZE,
     Color_Table);
  (Color_Table [index] [0]) =
    (arg_real_in_range (3, ((double) 0), ((double) 1)));
  (Color_Table [index] [1]) =
    (arg_real_in_range (4, ((double) 0), ((double) 1)));
  (Color_Table [index] [2]) =
    (arg_real_in_range (5, ((double) 0), ((double) 1)));
  define_color_table
    (device,
     STARBASE_COLOR_TABLE_START,
     STARBASE_COLOR_TABLE_SIZE,
     Color_Table);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("INQUIRE-COLOR", Prim_inquire_color, 2,2, 0)
{
  int device, index;
  PRIMITIVE_HEADER (2);
  device = (SB_DEVICE_ARG (1));
  index =
    (arg_integer_in_range
     (2, STARBASE_COLOR_TABLE_START, STARBASE_COLOR_TABLE_SIZE));
  inquire_color_table
    (device,
     STARBASE_COLOR_TABLE_START,
     STARBASE_COLOR_TABLE_SIZE,
     Color_Table);
  PRIMITIVE_RETURN
    (cons ((double_to_flonum ((double) (Color_Table[index][0]))),
	   (cons ((double_to_flonum ((double) (Color_Table[index][1]))),
		  (cons ((double_to_flonum ((double) (Color_Table[index][2]))),
			 EMPTY_LIST))))));
}

DEFINE_PRIMITIVE ("READ-COLORS-FROM-FILE", Prim_read_colors_from_file, 2,2, 0)
{
  int device;
  long i;
  FILE * fp;
  PRIMITIVE_HEADER (2);
  device = (SB_DEVICE_ARG (1));
  CHECK_ARG (2, STRING_P);

  fp = (fopen (((char *) (STRING_LOC ((ARG_REF (2)), 0))), "r"));
  if (fp == ((FILE *) 0))
    error_bad_range_arg (2);
  if (feof (fp))
    {
      fprintf (stderr, "\nColor Datafile is empty!\n");
      error_external_return ();
    }
  for (i = 0; (i < STARBASE_COLOR_TABLE_SIZE); i += 1)
    fscanf (fp, "%f %f %f\n",
	    (& (Color_Table [i] [0])),
	    (& (Color_Table [i] [1])),
	    (& (Color_Table [i] [2])));
  if ((fclose (fp)) != 0)
    error_external_return ();
  define_color_table
    (device,
     STARBASE_COLOR_TABLE_START,
     STARBASE_COLOR_TABLE_SIZE,
     Color_Table);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SAVE-COLORS-IN-FILE", Prim_save_colors_in_file, 2,2, 0)
{
  int device;
  long i;
  FILE * fp;
  PRIMITIVE_HEADER (2);
  device = (SB_DEVICE_ARG (1));
  CHECK_ARG (2, STRING_P);
  fp = (fopen (((char *) (STRING_LOC ((ARG_REF (2)), 0))), "w"));
  if (fp == ((FILE *) 0))
    error_bad_range_arg (2);
  inquire_color_table
    (device,
     STARBASE_COLOR_TABLE_START,
     STARBASE_COLOR_TABLE_SIZE,
     Color_Table);
  for (i = 0; (i < STARBASE_COLOR_TABLE_SIZE); i += 1)
    fprintf (fp, "%f %f %f\n",
	     (Color_Table [i] [0]),
	     (Color_Table [i] [1]),
	     (Color_Table [i] [2]));
  if ((fclose (fp)) != 0)
    error_external_return ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}
