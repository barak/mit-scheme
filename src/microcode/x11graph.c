/* -*-C-*-

$Id: x11graph.c,v 1.47 2008/01/30 20:02:23 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* Simple graphics for X11 */

#include "scheme.h"
#include "prims.h"
#include "x11.h"

#define RESOURCE_NAME "schemeGraphics"
#define RESOURCE_CLASS "SchemeGraphics"
#define DEFAULT_GEOMETRY "512x384+0+0"

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

#define X_COORDINATE(virtual_device_x, xw, direction)			\
  (((XW_X_SLOPE (xw)) == FLT_MAX)					\
   ? ((direction <= 0) ? 0 : ((int) ((XW_X_SIZE (xw)) - 1)))		\
   : (ROUND_FLOAT							\
      (((XW_X_SLOPE (xw)) * (virtual_device_x - (XW_X_LEFT (xw)))))))

#define Y_COORDINATE(virtual_device_y, xw, direction)			\
  (((XW_Y_SLOPE (xw)) == FLT_MAX)					\
   ? ((direction <= 0) ? ((int) ((XW_Y_SIZE (xw)) - 1)) : 0)		\
   : (((int) ((XW_Y_SIZE (xw)) - 1))					\
      + (ROUND_FLOAT							\
	 ((XW_Y_SLOPE (xw)) * (virtual_device_y - (XW_Y_BOTTOM (xw)))))))

#define X_LENGTH(virtual_length, xw)					\
  (((XW_X_SLOPE (xw)) == 0.0)						\
   ? 0									\
   : ((XW_X_SLOPE (xw)) == FLT_MAX)					\
   ? ((int) ((XW_X_SIZE (xw)) - 1))					\
   : (ROUND_FLOAT ((fabs (XW_X_SLOPE (xw))) * (virtual_length))))

#define Y_LENGTH(virtual_length, xw)					\
  (((XW_Y_SLOPE (xw)) == 0.0)						\
   ? 0									\
   : ((XW_Y_SLOPE (xw)) == FLT_MAX)					\
   ? ((int) ((XW_Y_SIZE (xw)) - 1))					\
   : (ROUND_FLOAT ((fabs (XW_Y_SLOPE (xw))) * (virtual_length))))

static int
arg_x_coordinate (unsigned int arg, struct xwindow * xw, int direction)
{
  return (X_COORDINATE (((float) (arg_real_number (arg))), xw, direction));
}

static int
arg_y_coordinate (unsigned int arg, struct xwindow * xw, int direction)
{
  return (Y_COORDINATE (((float) (arg_real_number (arg))), xw, direction));
}

static SCHEME_OBJECT
x_coordinate_map (struct xwindow * xw, unsigned int x)
{
  return
    (FLOAT_TO_FLONUM
     ((((XW_X_SLOPE (xw)) == 0.0) || ((XW_X_SLOPE (xw)) == FLT_MAX))
      ? (XW_X_LEFT (xw))
      : ((((float) x) / (XW_X_SLOPE (xw))) + (XW_X_LEFT (xw)))));
}

static SCHEME_OBJECT
y_coordinate_map (struct xwindow * xw, unsigned int y)
{
  return
    (FLOAT_TO_FLONUM
     ((((XW_Y_SLOPE (xw)) == 0.0) || ((XW_Y_SLOPE (xw)) == FLT_MAX))
      ? (XW_Y_BOTTOM (xw))
      : (((((float) y) - ((XW_Y_SIZE (xw)) - 1)) / (XW_Y_SLOPE (xw)))
	 + (XW_Y_BOTTOM (xw)))));
}

static void
set_clip_rectangle (struct xwindow * xw,
		    int x_left,
		    int y_bottom,
		    int x_right,
		    int y_top)
{
  XRectangle rectangles [1];
  Display * display = (XW_DISPLAY (xw));
  unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
  if (x_left > x_right)
    {
      unsigned int x = x_left;
      x_left = x_right;
      x_right = x;
    }
  if (y_top > y_bottom)
    {
      unsigned int y = y_top;
      y_top = y_bottom;
      y_bottom = y;
    }
  {
    unsigned int width = ((x_right + 1) - x_left);
    unsigned int height = ((y_bottom + 1) - y_top);
    (XW_CLIP_X (xw)) = x_left;
    (XW_CLIP_Y (xw)) = y_top;
    (XW_CLIP_WIDTH (xw)) = width;
    (XW_CLIP_HEIGHT (xw)) = height;
    ((rectangles[0]) . x) = x_left;
    ((rectangles[0]) . y) = y_top;
    ((rectangles[0]) . width) = width;
    ((rectangles[0]) . height) = height;
  }
  XSetClipRectangles
    (display,
     (XW_NORMAL_GC (xw)),
     internal_border_width,
     internal_border_width,
     rectangles, 1, Unsorted);
  XSetClipRectangles
    (display,
     (XW_REVERSE_GC (xw)),
     internal_border_width,
     internal_border_width,
     rectangles, 1, Unsorted);
}

static void
reset_clip_rectangle (struct xwindow * xw)
{
  set_clip_rectangle
    (xw, 0, ((XW_Y_SIZE (xw)) - 1), ((XW_X_SIZE (xw)) - 1), 0);
}

static void
reset_virtual_device_coordinates (struct xwindow * xw)
{
  /* Note that the expression ((XW_c_SIZE (xw)) - 1) guarantees that
     both limits of the device coordinates will be inside the window. */
  (XW_X_SLOPE (xw))
    = (((XW_X_RIGHT (xw)) == (XW_X_LEFT (xw)))
       ? FLT_MAX
       : ((XW_X_SIZE (xw)) <= 1)
       ? 0.0
       : (((float) ((XW_X_SIZE (xw)) - 1))
	  / ((XW_X_RIGHT (xw)) - (XW_X_LEFT (xw)))));
  (XW_Y_SLOPE (xw))
    = (((XW_Y_BOTTOM (xw)) == (XW_Y_TOP (xw)))
       ? FLT_MAX
       : ((XW_Y_SIZE (xw)) <= 1)
       ? 0.0
       : (((float) ((XW_Y_SIZE (xw)) - 1))
	  / ((XW_Y_BOTTOM (xw)) - (XW_Y_TOP (xw)))));
  reset_clip_rectangle (xw);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-VDC-EXTENT", Prim_x_graphics_set_vdc_extent,
		  5, 5,
  "(X-GRAPHICS-SET-VDC-EXTENT WINDOW X-MIN Y-MIN X-MAX Y-MAX)\n\
Set the virtual device coordinates to the given values.")
{
  PRIMITIVE_HEADER (5);
  {
    struct xwindow * xw = (x_window_arg (1));
    float x_left = (arg_real_number (2));
    float y_bottom = (arg_real_number (3));
    float x_right = (arg_real_number (4));
    float y_top = (arg_real_number (5));
    (XW_X_LEFT (xw)) = x_left;
    (XW_Y_BOTTOM (xw)) = y_bottom;
    (XW_X_RIGHT (xw)) = x_right;
    (XW_Y_TOP (xw)) = y_top;
    reset_virtual_device_coordinates (xw);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-VDC-EXTENT", Prim_x_graphics_vdc_extent, 1, 1, 0)
{
  PRIMITIVE_HEADER (5);
  {
    struct xwindow * xw = (x_window_arg (1));
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 4, true));
    VECTOR_SET (result, 0, (double_to_flonum ((double) (XW_X_LEFT (xw)))));
    VECTOR_SET (result, 1, (double_to_flonum ((double) (XW_Y_BOTTOM (xw)))));
    VECTOR_SET (result, 2, (double_to_flonum ((double) (XW_X_RIGHT (xw)))));
    VECTOR_SET (result, 3, (double_to_flonum ((double) (XW_Y_TOP (xw)))));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("X-GRAPHICS-RESET-CLIP-RECTANGLE", Prim_x_graphics_reset_clip_rectangle, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  reset_clip_rectangle (x_window_arg (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-CLIP-RECTANGLE",
		  Prim_x_graphics_set_clip_rectangle, 5, 5,
  "(X-GRAPHICS-SET-CLIP-RECTANGLE WINDOW X-LEFT Y-BOTTOM X-RIGHT Y-TOP)\n\
Set the clip rectangle to the given coordinates.")
{
  PRIMITIVE_HEADER (5);
  {
    struct xwindow * xw = (x_window_arg (1));
    set_clip_rectangle
      (xw,
       (arg_x_coordinate (2, xw, -1)),
       (arg_y_coordinate (3, xw, -1)),
       (arg_x_coordinate (4, xw, 1)),
       (arg_y_coordinate (5, xw, 1)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
process_event (struct xwindow * xw, XEvent * event)
{
}

static void
reconfigure (struct xwindow * xw, unsigned int width, unsigned int height)
{
  unsigned int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  unsigned int x_size = ((width < extra) ? 0 : (width - extra));
  unsigned int y_size = ((height < extra) ? 0 : (height - extra));
  if ((x_size != (XW_X_SIZE (xw))) || (y_size != (XW_Y_SIZE (xw))))
    {
      (XW_X_SIZE (xw)) = x_size;
      (XW_Y_SIZE (xw)) = y_size;
      reset_virtual_device_coordinates (xw);
      XClearWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
    }
}

DEFINE_PRIMITIVE ("X-GRAPHICS-RECONFIGURE", Prim_x_graphics_reconfigure, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  reconfigure ((x_window_arg (1)),
	       (arg_ulong_integer (2)),
	       (arg_ulong_integer (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
wm_set_size_hint (struct xwindow * xw, int geometry_mask, int x, int y)
{
  unsigned int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  XSizeHints * size_hints = (XAllocSizeHints ());
  if (size_hints == 0)
    error_external_return ();
  (size_hints -> flags) =
    (PResizeInc | PMinSize | PBaseSize
     | (((geometry_mask & XValue) && (geometry_mask & YValue))
	? USPosition : PPosition)
     | (((geometry_mask & WidthValue) && (geometry_mask & HeightValue))
	? USSize : PSize));
  (size_hints -> x) = x;
  (size_hints -> y) = y;
  (size_hints -> width) = ((XW_X_SIZE (xw)) + extra);
  (size_hints -> height) = ((XW_Y_SIZE (xw)) + extra);
  (size_hints -> width_inc) = 1;
  (size_hints -> height_inc) = 1;
  (size_hints -> min_width) = extra;
  (size_hints -> min_height) = extra;
  (size_hints -> base_width) = extra;
  (size_hints -> base_height) = extra;
  XSetWMNormalHints ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), size_hints);
  XFree ((caddr_t) size_hints);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-OPEN-WINDOW", Prim_x_graphics_open_window, 3, 3,
  "(X-GRAPHICS-OPEN-WINDOW DISPLAY GEOMETRY SUPPRESS-MAP?)\n\
Open a window on DISPLAY using GEOMETRY.\n\
If GEOMETRY is false map window interactively.\n\
If third argument SUPPRESS-MAP? is true, do not map the window immediately.")
{
  PRIMITIVE_HEADER (3);
  {
    struct xdisplay * xd = (x_display_arg (1));
    Display * display = (XD_DISPLAY (xd));
    struct drawing_attributes attributes;
    struct xwindow_methods methods;
    XSetWindowAttributes wattributes;
    const char * resource_name = RESOURCE_NAME;
    const char * resource_class = RESOURCE_CLASS;
    int map_p;

    x_decode_window_map_arg
      ((ARG_REF (3)), (&resource_name), (&resource_class), (&map_p));
    x_default_attributes
      (display, resource_name, resource_class, (&attributes));
    (wattributes . background_pixel) = (attributes . background_pixel);
    (wattributes . border_pixel) = (attributes . border_pixel);
    (wattributes . backing_store) = Always;
    (methods . deallocator) = 0;
    (methods . event_processor) = process_event;
    (methods . x_coordinate_map) = x_coordinate_map;
    (methods . y_coordinate_map) = y_coordinate_map;
    (methods . update_normal_hints) = 0;
    {
      unsigned int extra = (2 * (attributes . internal_border_width));
      int x_pos = (-1);
      int y_pos = (-1);
      int x_size = 512;
      int y_size = 384;
      int geometry_mask =
	(XGeometry (display, (DefaultScreen (display)),
		    (((ARG_REF (2)) == SHARP_F)
		     ? (x_get_default
			(display, resource_name, resource_class,
			 "geometry", "Geometry", 0))
		     : (STRING_ARG (2))),
		    DEFAULT_GEOMETRY, (attributes . border_width),
		    1, 1, extra, extra,
		    (&x_pos), (&y_pos), (&x_size), (&y_size)));
      Window window =
	(XCreateWindow
	 (display,
	  (RootWindow (display, (DefaultScreen (display)))),
	  x_pos, y_pos, (x_size + extra), (y_size + extra),
	  (attributes . border_width),
	  CopyFromParent, CopyFromParent, CopyFromParent,
	  (CWBackPixel | CWBorderPixel | CWBackingStore),
	  (&wattributes)));
      if (window == 0)
	error_external_return ();
      {
	struct xwindow * xw =
	  (x_make_window
	   (xd, window, x_size, y_size, (&attributes), (&methods),
	    (sizeof (struct gw_extra))));
	(XW_X_LEFT (xw)) = ((float) (-1));
	(XW_X_RIGHT (xw)) = ((float) 1);
	(XW_Y_BOTTOM (xw)) = ((float) (-1));
	(XW_Y_TOP (xw)) = ((float) 1);
	reset_virtual_device_coordinates (xw);
	(XW_X_CURSOR (xw)) = 0;
	(XW_Y_CURSOR (xw)) = 0;
	wm_set_size_hint (xw, geometry_mask, x_pos, y_pos);
	xw_set_wm_input_hint (xw, 0);
	xw_set_wm_name (xw, "scheme-graphics");
	xw_set_wm_icon_name (xw, "scheme-graphics");
	XSelectInput (display, window, StructureNotifyMask);
	xw_make_window_map (xw, resource_name, resource_class, map_p);
	PRIMITIVE_RETURN (XW_TO_OBJECT (xw));
      }
    }
  }
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-LINE", Prim_x_graphics_draw_line, 5, 5,
  "(X-GRAPHICS-DRAW-LINE WINDOW X-START Y-START X-END Y-END)\n\
Draw a line from the start coordinates to the end coordinates.\n\
Subsequently move the graphics cursor to the end coordinates.")
{
  PRIMITIVE_HEADER (5);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int new_x_cursor = (arg_x_coordinate (4, xw, 0));
    unsigned int new_y_cursor = (arg_y_coordinate (5, xw, 0));
    unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
    XDrawLine
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       (XW_NORMAL_GC (xw)),
       (internal_border_width + (arg_x_coordinate (2, xw, 0))),
       (internal_border_width + (arg_y_coordinate (3, xw, 0))),
       (internal_border_width + new_x_cursor),
       (internal_border_width + new_y_cursor));
    (XW_X_CURSOR (xw)) = new_x_cursor;
    (XW_Y_CURSOR (xw)) = new_y_cursor;
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-MOVE-CURSOR", Prim_x_graphics_move_cursor, 3, 3,
  "(X-GRAPHICS-MOVE-CURSOR WINDOW X Y)\n\
Move the graphics cursor to the given coordinates.")
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    (XW_X_CURSOR (xw)) = (arg_x_coordinate (2, xw, 0));
    (XW_Y_CURSOR (xw)) = (arg_y_coordinate (3, xw, 0));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAG-CURSOR", Prim_x_graphics_drag_cursor, 3, 3,
  "(X-GRAPHICS-DRAG-CURSOR WINDOW X Y)\n\
Draw a line from the graphics cursor to the given coordinates.\n\
Subsequently move the graphics cursor to those coordinates.")
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int new_x_cursor = (arg_x_coordinate (2, xw, 0));
    unsigned int new_y_cursor = (arg_y_coordinate (3, xw, 0));
    unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
    XDrawLine
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       (XW_NORMAL_GC (xw)),
       (internal_border_width + (XW_X_CURSOR (xw))),
       (internal_border_width + (XW_Y_CURSOR (xw))),
       (internal_border_width + new_x_cursor),
       (internal_border_width + new_y_cursor));
    (XW_X_CURSOR (xw)) = new_x_cursor;
    (XW_Y_CURSOR (xw)) = new_y_cursor;
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-POINT", Prim_x_graphics_draw_point, 3, 3,
  "(X-GRAPHICS-DRAW-POINT WINDOW X Y)\n\
Draw one point at the given coordinates.\n\
Subsequently move the graphics cursor to those coordinates.")
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
    XDrawPoint
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       (XW_NORMAL_GC (xw)),
       (internal_border_width + (arg_x_coordinate (2, xw, 0))),
       (internal_border_width + (arg_y_coordinate (3, xw, 0))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-ARC", Prim_x_graphics_draw_arc, 8, 8,
  "(X-GRAPHICS-DRAW-ARC WINDOW X Y RADIUS-X RADIUS-Y START-ANGLE SWEEP-ANGLE FILL?)\n\
Draw an arc at the given coordinates, with given X and Y radii.\n\
START-ANGLE and SWEEP-ANGLE are in degrees, anti-clocwise.\n\
START-ANGLE is from 3 o'clock, and SWEEP-ANGLE is relative to the START-ANGLE\n\
If FILL? is true, the arc is filled.")
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
    float  virtual_device_x = arg_real_number (2);
    float  virtual_device_y = arg_real_number (3);
    float  radius_x = arg_real_number (4);
    float  radius_y = arg_real_number (5);
    float  angle_start = arg_real_number (6);
    float  angle_sweep = arg_real_number (7);

    /* we assume a virtual coordinate system with X increasing left to
     * right and Y increasing top to bottom.  If we are wrong then we
     * have to flip the axes and adjust the angles */

    int x1 = (X_COORDINATE (virtual_device_x - radius_x,  xw, 0));
    int x2 = (X_COORDINATE (virtual_device_x + radius_x,  xw, 0));
    int y1 = (Y_COORDINATE (virtual_device_y + radius_y,  xw, 0));
    int y2 = (Y_COORDINATE (virtual_device_y - radius_y,  xw, 0));
    int width, height;
    int angle1 = ((int)(angle_start * 64)) % (64*360);
    int angle2 = ((int)(angle_sweep * 64));
    if (angle1 < 0)
      angle1 = (64*360) + angle1;
    /* angle1 is now 0..359 */
    if (x2<x1) { /* x-axis flip */
      int t=x1; x1=x2; x2=t;
      if (angle1 < 64*180)
	angle1 = 64*180 - angle1;
      else
	angle1 = 64*540 - angle1;
      angle2 = -angle2;
    }
    if (y2<y1) { /* y-axis flip */
      int t=y1; y1=y2; y2=t;
      angle1 = 64*360 - angle1;
      angle2 = -angle2;
    }
    width  = x2 - x1;
    height = y2 - y1;
    if (ARG_REF(8) == SHARP_F)
      XDrawArc
	((XW_DISPLAY (xw)),
	 (XW_WINDOW (xw)),
	 (XW_NORMAL_GC (xw)),
	 (internal_border_width + x1),
	 (internal_border_width + y1),
	 width, height,  angle1, angle2);
    else
      XFillArc
	((XW_DISPLAY (xw)),
	 (XW_WINDOW (xw)),
	 (XW_NORMAL_GC (xw)),
	 (internal_border_width + x1),
	 (internal_border_width + y1),
	 width, height,  angle1, angle2);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/**************   TEST PROGRAM FOR X-GRAPHICS-DRAW-ARC  *****************
(define g (make-graphics-device))

(define (test dx dy a1 a2)
  (let ((x .3)
	(y .4)
	(r .2))
    (define (fx a) (+ x (* r (cos (* a (asin 1) 1/90)))))
    (define (fy a) (+ y (* r (sin (* a (asin 1) 1/90)))))
    (graphics-set-coordinate-limits g (- dx) (- dy) dx dy)
    (graphics-operation g 'set-foreground-color "black")
    (graphics-clear g)

    (graphics-draw-text g   0   0 ".")

    (graphics-draw-line g  -1   0 1 0)
    (graphics-draw-line g   0  -1 0 1)
    (graphics-draw-line g   0   0 1 1)
    (graphics-draw-text g  .5   0 "+X")
    (graphics-draw-text g -.5   0 "-X")
    (graphics-draw-text g   0  .5 "+Y")
    (graphics-draw-text g   0 -.5 "-Y")

    ;; The grey wedge is that that 10 degrees of the arc.
    (graphics-operation g 'set-foreground-color "grey")
    (graphics-operation g 'draw-arc x y r r a1 a2 #T)
    (graphics-operation g 'set-foreground-color "black")
    (graphics-operation g 'draw-arc x y r r a1 (+ a2 (if (< a2 0) 10 -10)) #T)

    (graphics-operation g 'set-foreground-color "red")
    (graphics-draw-text g x y ".O")

    (let ((b1 (min a1 (+ a1 a2)))
	  (b2 (max a1 (+ a1 a2))))
      (do ((a b1 (+ a 5)))
	  ((> a b2))
	(graphics-draw-text g (fx a) (fy a) ".")))

    (graphics-draw-text g (fx a1) (fy a1) ".Start")
    (graphics-draw-text g (fx (+ a1 a2)) (fy (+ a1 a2)) ".End")))

;; Test axes
(test  1  1  30 90)
(test -1  1  30 90)
(test  1 -1  30 90)
(test -1 -1  30 90)

;; Test angles
(test  1  1  30 90)
(test  1  1  30 -90)
(test  1  1  -30 90)
(test  1  1  -30 -90)
 ***********************************************************************/

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-STRING", Prim_x_graphics_draw_string, 4, 4,
  "(X-GRAPHICS-DRAW-STRING WINDOW X Y STRING)\n\
Draw characters in the current font at the given coordinates, with\n\
transparent background.")
{
  PRIMITIVE_HEADER (4);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
    char * s = (STRING_ARG (4));
    XDrawString
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       (XW_NORMAL_GC (xw)),
       (internal_border_width + (arg_x_coordinate (2, xw, 0))),
       (internal_border_width + (arg_y_coordinate (3, xw, 0))),
       s,
       (STRING_LENGTH (ARG_REF (4))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-IMAGE-STRING",
		  Prim_x_graphics_draw_image_string, 4, 4,
  "(X-GRAPHICS-DRAW-IMAGE-STRING WINDOW X Y STRING)\n\
Draw characters in the current font at the given coordinates, with\n\
solid background.")
{
  PRIMITIVE_HEADER (4);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
    char * s = (STRING_ARG (4));
    XDrawImageString
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       (XW_NORMAL_GC (xw)),
       (internal_border_width + (arg_x_coordinate (2, xw, 0))),
       (internal_border_width + (arg_y_coordinate (3, xw, 0))),
       s,
       (STRING_LENGTH (ARG_REF (4))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-FUNCTION", Prim_x_graphics_set_function, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned int function = (arg_ulong_index_integer (2, 16));
    XSetFunction (display, (XW_NORMAL_GC (xw)), function);
    XSetFunction (display, (XW_REVERSE_GC (xw)), function);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static XPoint *
floating_vector_point_args (struct xwindow * xw,
			    unsigned int x_index,
			    unsigned int y_index,
			    unsigned int * return_n_points)
{
  SCHEME_OBJECT x_vector = (ARG_REF (x_index));
  SCHEME_OBJECT y_vector = (ARG_REF (y_index));
  unsigned int n_points;

  if (!FLONUM_P (x_vector))
    error_wrong_type_arg (x_index);
  if (!FLONUM_P (y_vector))
    error_wrong_type_arg (y_index);
  n_points = (FLOATING_VECTOR_LENGTH (x_vector));
  if (n_points != (FLOATING_VECTOR_LENGTH (y_vector)))
    error_bad_range_arg (x_index);
  {
    XPoint * points = (dstack_alloc (n_points * (sizeof (XPoint))));
    double * scan_x = (FLOATING_VECTOR_LOC (x_vector, 0));
    double * end_x = (FLOATING_VECTOR_LOC (x_vector, n_points));
    double * scan_y = (FLOATING_VECTOR_LOC (y_vector, 0));
    XPoint * scan_points = points;
    unsigned int border = (XW_INTERNAL_BORDER_WIDTH (xw));
    while (scan_x < end_x)
      {
	(scan_points -> x) = (border + (X_COORDINATE ((*scan_x++), xw, 0)));
	(scan_points -> y) = (border + (X_COORDINATE ((*scan_y++), xw, 0)));
	scan_points += 1;
      }
    (*return_n_points) = n_points;
    return (points);
  }
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-POINTS", Prim_x_graphics_draw_points, 3, 3,
  "(X-GRAPHICS-DRAW-POINTS WINDOW X-VECTOR Y-VECTOR)\n\
Draw multiple points.")
{
  PRIMITIVE_HEADER (3);
  {
    void * position = dstack_position;
    struct xwindow * xw = (x_window_arg (1));
    unsigned int n_points;
    XPoint * points = (floating_vector_point_args (xw, 2, 3, (&n_points)));
    while (n_points > 0)
      {
	unsigned int this_send = ((n_points <= 4093) ? n_points : 4093);
	n_points -= this_send;
	XDrawPoints ((XW_DISPLAY (xw)),
		     (XW_WINDOW (xw)),
		     (XW_NORMAL_GC (xw)),
		     points,
		     this_send,
		     CoordModeOrigin);
	points += this_send;
      }
    dstack_set_position (position);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-LINES", Prim_x_graphics_draw_lines, 3, 3,
  "(X-GRAPHICS-DRAW-LINES WINDOW X-VECTOR Y-VECTOR)\n\
Draw multiple lines.")
{
  PRIMITIVE_HEADER (3);
  {
    void * position = dstack_position;
    struct xwindow * xw = (x_window_arg (1));
    unsigned int n_points;
    XPoint * points = (floating_vector_point_args (xw, 2, 3, (&n_points)));
    while (n_points > 0)
      {
	unsigned int this_send = ((n_points <= 2047) ? n_points : 2047);
	n_points -= this_send;
	XDrawLines ((XW_DISPLAY (xw)),
		    (XW_WINDOW (xw)),
		    (XW_NORMAL_GC (xw)),
		    points,
		    this_send,
		    CoordModeOrigin);
	points += (this_send - 1);
      }
    dstack_set_position (position);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-FILL-STYLE", Prim_x_graphics_set_fill_style, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned int fill_style = (arg_ulong_index_integer (2, 4));
    XSetFillStyle (display, (XW_NORMAL_GC (xw)), fill_style);
    XSetFillStyle (display, (XW_REVERSE_GC (xw)), fill_style);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-LINE-STYLE", Prim_x_graphics_set_line_style, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned int style = (arg_ulong_index_integer (2, 3));
    XSetLineAttributes
      (display, (XW_NORMAL_GC (xw)), 0, style, CapButt, JoinMiter);
    XSetLineAttributes
      (display, (XW_REVERSE_GC (xw)), 0, style, CapButt, JoinMiter);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-DASHES", Prim_x_graphics_set_dashes, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    char * dash_list = (STRING_ARG (3));
    unsigned int dash_list_length = (STRING_LENGTH (ARG_REF (3)));
    unsigned int dash_offset = (arg_ulong_index_integer (2, dash_list_length));
    XSetDashes
      (display, (XW_NORMAL_GC (xw)), dash_offset, dash_list, dash_list_length);
    XSetDashes
      (display, (XW_REVERSE_GC (xw)), dash_offset, dash_list,
       dash_list_length);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-COPY-AREA", Prim_x_graphics_copy_area, 8, 8, 0)
{
  PRIMITIVE_HEADER (7);
  {
    struct xwindow * source_xw = x_window_arg (1);
    struct xwindow * destination_xw = x_window_arg (2);
    unsigned int source_internal_border_width
      = (XW_INTERNAL_BORDER_WIDTH (source_xw));
    unsigned int destination_internal_border_width
      = (XW_INTERNAL_BORDER_WIDTH (destination_xw));
    Display *source_display = XW_DISPLAY (source_xw);
    Display *destination_display = XW_DISPLAY (destination_xw);
    if (source_display != destination_display)
      error_bad_range_arg (2);
    XCopyArea (source_display,
	       (XW_WINDOW (source_xw)),
	       (XW_WINDOW (destination_xw)),
	       (XW_NORMAL_GC (source_xw)),
	       (source_internal_border_width
		+ (arg_x_coordinate (3, source_xw, -1))),
	       (source_internal_border_width
		+ (arg_y_coordinate (4, source_xw, 1))),
	       (X_LENGTH ((arg_real_number (5)), source_xw)),
	       (Y_LENGTH ((arg_real_number (6)), source_xw)),
	       (destination_internal_border_width
		+ (arg_x_coordinate (7, destination_xw, -1))),
	       (destination_internal_border_width
		+ (arg_y_coordinate (8, destination_xw, 1))));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

static XPoint *
x_polygon_vector_arg (struct xwindow * xw, unsigned int argno)
{
  SCHEME_OBJECT vector = (VECTOR_ARG (argno));
  unsigned long length = (VECTOR_LENGTH (vector));
  unsigned int border = (XW_INTERNAL_BORDER_WIDTH (xw));
  if ((length % 2) != 0)
    error_bad_range_arg (argno);
  {
    XPoint * result = (x_malloc ((length / 2) * (sizeof (XPoint))));
    XPoint * scan_result = result;
    SCHEME_OBJECT * scan = (& (VECTOR_REF (vector, 0)));
    SCHEME_OBJECT * end = (scan + length);
    SCHEME_OBJECT coord;
    while (scan < end)
      {
	coord = (*scan++);
	if (! ((REAL_P (coord)) && (real_number_to_double_p (coord))))
	  error_bad_range_arg (argno);
	(scan_result -> x)
	  = (border
	     + (X_COORDINATE ((real_number_to_double (coord)), xw, 0)));
	coord = (*scan++);
	if (! ((REAL_P (coord)) && (real_number_to_double_p (coord))))
	  error_bad_range_arg (argno);
	(scan_result -> y)
	  = (border
	     + (Y_COORDINATE ((real_number_to_double (coord)), xw, 0)));
	scan_result += 1;
      }
    return (result);
  }
}

DEFINE_PRIMITIVE ("X-GRAPHICS-FILL-POLYGON", Prim_x_graphics_fill_polygon, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = x_window_arg (1);
    XPoint * points = (x_polygon_vector_arg (xw, 2));
    unsigned long length = VECTOR_LENGTH (VECTOR_ARG (2));
    XFillPolygon ((XW_DISPLAY (xw)),
		  (XW_WINDOW (xw)),
		  (XW_NORMAL_GC (xw)),
		  points,
		  (length / 2),
		  Nonconvex,
		  CoordModeOrigin);
    free (points);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

static int
find_pixmap_format (Display * dpy, int depth, XPixmapFormatValues * format)
{
  XPixmapFormatValues * pixmap_formats;
  int n_pixmap_formats;
  XPixmapFormatValues * scan_pixmap_formats;
  XPixmapFormatValues * end_pixmap_formats;

  pixmap_formats = (XListPixmapFormats (dpy, (&n_pixmap_formats)));
  if (pixmap_formats == 0)
    return (0);
  scan_pixmap_formats = pixmap_formats;
  end_pixmap_formats = (pixmap_formats + n_pixmap_formats);
  while (1)
    {
      if (scan_pixmap_formats >= end_pixmap_formats)
	return (0);
      if ((scan_pixmap_formats -> depth) == depth)
	{
	  (*format) = (*scan_pixmap_formats);
	  XFree (pixmap_formats);
	  return (1);
	}
      scan_pixmap_formats += 1;
    }
}

DEFINE_PRIMITIVE ("X-CREATE-IMAGE", Prim_x_create_image, 3, 3,
  "(window width height)\n\
Creates and returns an XImage object, of dimensions WIDTH by HEIGHT.\n\
WINDOW is used to set the Display, Visual, and Depth characteristics.\n\
The image is created by calling XCreateImage.")
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    Window window = (XW_WINDOW (xw));
    Display * dpy = (XW_DISPLAY (xw));
    unsigned int width = (arg_ulong_integer (2));
    unsigned int height = (arg_ulong_integer (3));
    XWindowAttributes attrs;
    XPixmapFormatValues pixmap_format;
    unsigned int bits_per_line;
    unsigned int bitmap_pad;
    unsigned int bytes_per_line;

    XGetWindowAttributes (dpy, window, (&attrs));
    if (!find_pixmap_format (dpy, (attrs . depth), (&pixmap_format)))
      error_external_return ();
    bits_per_line = ((pixmap_format . bits_per_pixel) * width);
    bitmap_pad = (pixmap_format . scanline_pad);
    if ((bits_per_line % bitmap_pad) != 0)
      bits_per_line += (bitmap_pad - (bits_per_line % bitmap_pad));
    bytes_per_line = ((bits_per_line + (CHAR_BIT - 1)) / CHAR_BIT);
    PRIMITIVE_RETURN
      (X_IMAGE_TO_OBJECT
       (XCreateImage
	(dpy,
	 (DefaultVisualOfScreen (attrs . screen)),
	 (attrs . depth),
	 ZPixmap,
	 0,
	 ((char *) (x_malloc (height * bytes_per_line))),
	 width,
	 height,
	 bitmap_pad,
	 bytes_per_line)));
  }
}

DEFINE_PRIMITIVE ("X-BYTES-INTO-IMAGE", Prim_x_bytes_into_image, 2, 2,
  "(vector image)\n\
VECTOR is a vector or vector-8b of pixel values stored in row-major\n\
order; it must have the same number of pixels as IMAGE.\n\
These pixels are written onto IMAGE by repeated calls to XPutPixel.\n\
This procedure is equivalent to calling X-SET-PIXEL-IN-IMAGE for each\n\
pixel in VECTOR.")
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT vector = (ARG_REF (1));
    XImage * image = (XI_IMAGE (x_image_arg (2)));
    unsigned long width = (image -> width);
    unsigned long height = (image -> height);
    if (STRING_P (vector))
      {
	unsigned char * vscan;
	unsigned long x;
	unsigned long y;

	if ((STRING_LENGTH (vector)) != (width * height))
	  error_bad_range_arg (1);
	vscan = (STRING_BYTE_PTR (vector));
	for (y = 0; (y < height); y += 1)
	  for (x = 0; (x < width); x += 1)
	    XPutPixel (image, x, y, ((unsigned long) (*vscan++)));
      }
    else if (VECTOR_P (vector))
      {
	unsigned long vlen;
	SCHEME_OBJECT * vscan;
	SCHEME_OBJECT * vend;
	unsigned long x;
	unsigned long y;

	vlen = (VECTOR_LENGTH (vector));
	if (vlen != (width * height))
	  error_bad_range_arg (1);
	vscan = (VECTOR_LOC (vector, 0));
	vend = (VECTOR_LOC (vector, vlen));
	while (vscan < vend)
	  {
	    SCHEME_OBJECT elt = (*vscan++);
	    if (! ((INTEGER_P (elt)) && (integer_to_ulong_p (elt))))
	      error_bad_range_arg (1);
	  }
	vscan = (VECTOR_LOC (vector, 0));
	for (y = 0; (y < height); y += 1)
	  for (x = 0; (x < width); x += 1)
	    XPutPixel (image, x, y, (integer_to_ulong (*vscan++)));
      }
    else
      error_wrong_type_arg (1);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("X-GET-PIXEL-FROM-IMAGE", Prim_x_get_image_pixel, 3, 3,
  "(image x y)\n\
The value of pixel (X,Y) of IMAGE is returned as an integer.\n\
This is accomplished by calling XGetPixel.")
{
  PRIMITIVE_HEADER (3);
  {
    XImage * image = (XI_IMAGE (x_image_arg (1)));
    PRIMITIVE_RETURN
      (ulong_to_integer
       (XGetPixel (image,
		   (arg_index_integer (2, (image -> width))),
		   (arg_index_integer (3, (image -> height))))));
  }
}

DEFINE_PRIMITIVE ("X-SET-PIXEL-IN-IMAGE", Prim_x_set_image_pixel, 4, 4,
  "(image x y pixel-value)\n\
The pixel (X,Y) of IMAGE is modified to contain PIXEL-VALUE.\n\
This is accomplished by calling XPutPixel.")
{
  PRIMITIVE_HEADER (4);
  {
    XImage * image = (XI_IMAGE (x_image_arg (1)));
    XPutPixel (image,
	       (arg_index_integer (2, (image -> width))),
	       (arg_index_integer (3, (image -> height))),
	       (arg_ulong_integer (4)));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("X-DESTROY-IMAGE", Prim_x_destroy_image, 1, 1,
  "(image)\n\
IMAGE is deallocated by calling XDestroyImage.")
{
  PRIMITIVE_HEADER (1);
  {
    struct ximage * xi = (x_image_arg (1));
    XDestroyImage (XI_IMAGE (xi));
    deallocate_x_image (xi);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("X-DISPLAY-IMAGE", Prim_x_display_image, 8, 8,
  "(image image-xoff image-yoff window window_xoff window_yoff width height)\n\
IMAGE is drawn on WINDOW by calling XPutImage.")
{
  PRIMITIVE_HEADER (8);
  {
    XImage * image = (XI_IMAGE (x_image_arg (1)));
    unsigned int image_width = (image -> width);
    unsigned int image_height = (image -> height);
    unsigned int x_offset = (arg_ulong_index_integer (2, image_width));
    unsigned int y_offset = (arg_ulong_index_integer (3, image_height));
    struct xwindow * xw = (x_window_arg (4));
    XPutImage
      ((XW_DISPLAY (xw)),(XW_WINDOW (xw)),(XW_NORMAL_GC (xw)),
       image, x_offset, y_offset,
       (arg_x_coordinate (5, xw, -1)),
       (arg_y_coordinate (6, xw, 1)),
       (arg_index_integer (7, ((image_width - x_offset) + 1))),
       (arg_index_integer (8, ((image_height - y_offset) + 1))));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("X-READ-IMAGE", Prim_x_read_image, 8, 8,
  "(image image-xoff image-yoff window window_xoff window_yoff width height)\n\
Reads the specified rectangle of WINDOW into IMAGE by calling XGetSubImage.")
{
  /* Called with Image, X-offset in image, Y-offset in image,
     Window, X-offset in window, Y-offset in window,
     Width, Height */
  PRIMITIVE_HEADER (8);
  { struct ximage * xi = x_image_arg (1);
    long XImageOffset = arg_integer(2);
    long YImageOffset = arg_integer(3);
    struct xwindow * xw = x_window_arg(4);
    long XWindowOffset = arg_integer(5);
    long YWindowOffset = arg_integer(6);
    long Width = arg_integer(7);
    long Height = arg_integer(8);

    XGetSubImage(XW_DISPLAY(xw), XW_WINDOW(xw), XWindowOffset, YWindowOffset,
		 Width, Height, -1, ZPixmap,
		 XI_IMAGE(xi), XImageOffset, YImageOffset);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-DEPTH", Prim_x_window_depth, 1, 1,
  "(window)\n\
Returns the pixel depth of WINDOW as an integer.")
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XWindowAttributes attrs;
    XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&attrs));
    PRIMITIVE_RETURN (long_to_integer (attrs . depth));
  }
}

DEFINE_PRIMITIVE ("X-GRAPHICS-MAP-X-COORDINATE", Prim_x_graphics_map_x_coordinate, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int xp = (arg_ulong_integer (2));
    int bx = (xp - (XW_INTERNAL_BORDER_WIDTH (xw)));
    PRIMITIVE_RETURN
      (x_coordinate_map
       (xw,
	((bx < 0) ? 0
	 : (bx >= (XW_X_SIZE (xw))) ? ((XW_X_SIZE (xw)) - 1)
	 : bx)));
  }
}

DEFINE_PRIMITIVE ("X-GRAPHICS-MAP-Y-COORDINATE", Prim_x_graphics_map_y_coordinate, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int yp = (arg_ulong_integer (2));
    int by = (yp - (XW_INTERNAL_BORDER_WIDTH (xw)));
    PRIMITIVE_RETURN
      (y_coordinate_map
       (xw,
	((by < 0) ? 0
	 : (by >= (XW_Y_SIZE (xw))) ? ((XW_Y_SIZE (xw)) - 1)
	 : by)));
  }
}
