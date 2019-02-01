/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of an x11 plugin for MIT/GNU Scheme.

This plugin is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This plugin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this plugin; if not, write to the Free Software Foundation,
Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

*/

/* Simple graphics for X11 */

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

struct xwindow_graphics
{
  struct xwindow xw;
  struct gw_extra extra;
};

#define XW_EXTRA(xw) (& (((struct xwindow_graphics *) xw) -> extra))

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

static float
x_coordinate_map (struct xwindow * xw, unsigned int x)
{
  return
    ((((XW_X_SLOPE (xw)) == 0.0) || ((XW_X_SLOPE (xw)) == FLT_MAX))
     ? (XW_X_LEFT (xw))
     : ((((float) x) / (XW_X_SLOPE (xw))) + (XW_X_LEFT (xw))));
}

static float
y_coordinate_map (struct xwindow * xw, unsigned int y)
{
  return
    ((((XW_Y_SLOPE (xw)) == 0.0) || ((XW_Y_SLOPE (xw)) == FLT_MAX))
     ? (XW_Y_BOTTOM (xw))
     : (((((float) y) - ((XW_Y_SIZE (xw)) - 1)) / (XW_Y_SLOPE (xw)))
	+ (XW_Y_BOTTOM (xw))));
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

void
x_graphics_set_vdc_extent (struct xwindow * xw,
			   float x_left, float y_bottom,
			   float x_right, float y_top)
{
  (XW_X_LEFT (xw)) = x_left;
  (XW_Y_BOTTOM (xw)) = y_bottom;
  (XW_X_RIGHT (xw)) = x_right;
  (XW_Y_TOP (xw)) = y_top;
  reset_virtual_device_coordinates (xw);
}

void
x_graphics_vdc_extent (struct xwindow * xw, float * results)
{
  results[0] = (XW_X_LEFT (xw));
  results[1] = (XW_Y_BOTTOM (xw));
  results[2] = (XW_X_RIGHT (xw));
  results[3] = (XW_Y_TOP (xw));
}

void
x_graphics_reset_clip_rectangle (struct xwindow * xw)
{
  reset_clip_rectangle (xw);
}

void
x_graphics_set_clip_rectangle (struct xwindow * xw,
			       float x_left, float y_bottom,
			       float x_right, float y_top)
{
  set_clip_rectangle (xw,
		      (X_COORDINATE (x_left, xw, -1)),
		      (Y_COORDINATE (y_bottom, xw, -1)),
		      (X_COORDINATE (x_right, xw, 1)),
		      (Y_COORDINATE (y_top, xw, 1)));
}

static void
process_event (struct xwindow * xw, XEvent * event)
{
}

void
x_graphics_reconfigure (struct xwindow * xw,
			unsigned int width, unsigned int height)
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

static void
wm_set_size_hint (struct xwindow * xw, int geometry_mask, int x, int y)
{
  unsigned int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  XSizeHints * size_hints = (XAllocSizeHints ());
  if (size_hints == 0)
    {
      fprintf (stderr, "\nXAllocSizeHints failed!\n");
      fflush (stderr);
      return;
    }
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

struct xwindow *
x_graphics_open_window (struct xdisplay * xd,
			char * geometry,
			const char * resource_name,
			const char * resource_class,
			int map_p)
{
  Display * display = (XD_DISPLAY (xd));
  struct drawing_attributes attributes;
  struct xwindow_methods methods;
  XSetWindowAttributes wattributes;

  if (resource_name == NULL) resource_name = RESOURCE_NAME;
  if (resource_class == NULL) resource_class = RESOURCE_CLASS;

  if (0 != x_default_attributes (display, resource_name, resource_class,
				 (&attributes)))
    return (NULL);
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
		  ((geometry == NULL)
		   ? (x_get_default
		      (display, resource_name, resource_class,
		       "geometry", "Geometry", 0))
		   : geometry),
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
      return (NULL);
    {
      struct xwindow * xw;
      xw =
	(x_make_window
	 (xd, window, x_size, y_size, (&attributes), (&methods),
	  (sizeof (struct xwindow_graphics))));
      (XW_X_LEFT (xw)) = ((float) (-1));
      (XW_X_RIGHT (xw)) = ((float) 1);
      (XW_Y_BOTTOM (xw)) = ((float) (-1));
      (XW_Y_TOP (xw)) = ((float) 1);
      reset_virtual_device_coordinates (xw);
      (XW_X_CURSOR (xw)) = 0;
      (XW_Y_CURSOR (xw)) = 0;
      wm_set_size_hint (xw, geometry_mask, x_pos, y_pos);
      if ((0 != xw_set_wm_input_hint (xw, 0))
	  || (0 != xw_set_wm_name (xw, "scheme-graphics"))
	  || (0 != xw_set_wm_icon_name (xw, "scheme-graphics"))
	  /* || (0 != XSelectInput (display, window, StructureNotifyMask))
	     The above fails with BadRequest but may have always done
	     so.  The umodule did not check the return code. */
	  || (0 != xw_make_window_map (xw, resource_name, resource_class,
				       map_p)))
	{
	  x_close_window (xw);
	  return (NULL);
	}
      return (xw);
    }
  }
}

void
x_graphics_draw_line (struct xwindow * xw,
		      float x_start, float y_start, float x_end, float y_end)
{
  unsigned int new_x_cursor = (X_COORDINATE (x_end, xw, 0));
  unsigned int new_y_cursor = (Y_COORDINATE (y_end, xw, 0));
  unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
  XDrawLine
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (XW_NORMAL_GC (xw)),
     (internal_border_width + (X_COORDINATE (x_start, xw, 0))),
     (internal_border_width + (Y_COORDINATE (y_start, xw, 0))),
     (internal_border_width + new_x_cursor),
     (internal_border_width + new_y_cursor));
  (XW_X_CURSOR (xw)) = new_x_cursor;
  (XW_Y_CURSOR (xw)) = new_y_cursor;
}

void
x_graphics_move_cursor (struct xwindow * xw, float x, float y)
{
  (XW_X_CURSOR (xw)) = (X_COORDINATE (x, xw, 0));
  (XW_Y_CURSOR (xw)) = (Y_COORDINATE (y, xw, 0));
}

void
x_graphics_drag_cursor (struct xwindow * xw, float x, float y)
{
  unsigned int new_x_cursor = (X_COORDINATE (x, xw, 0));
  unsigned int new_y_cursor = (Y_COORDINATE (y, xw, 0));
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

void
x_graphics_draw_point (struct xwindow * xw, float x, float y)
{
  unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
  XDrawPoint
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (XW_NORMAL_GC (xw)),
     (internal_border_width + (X_COORDINATE (x, xw, 0))),
     (internal_border_width + (Y_COORDINATE (y, xw, 0))));
}

void
x_graphics_draw_arc (struct xwindow * xw,
		     float virtual_device_x,
		     float virtual_device_y,
		     float radius_x,
		     float radius_y,
		     float angle_start,
		     float angle_sweep,
		     int fill_p)
{
  unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));

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
  if (!fill_p)
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

void
x_graphics_draw_string (struct xwindow * xw,
			float x, float y, char * string)
{
  unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
  XDrawString
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (XW_NORMAL_GC (xw)),
     (internal_border_width + (X_COORDINATE (x, xw, 0))),
     (internal_border_width + (Y_COORDINATE (y, xw, 0))),
     string,
     strlen (string));
}

void
x_graphics_draw_image_string (struct xwindow * xw,
			      float x, float y, char * string)
{
  unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
  XDrawImageString
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (XW_NORMAL_GC (xw)),
     (internal_border_width + (X_COORDINATE (x, xw, 0))),
     (internal_border_width + (Y_COORDINATE (y, xw, 0))),
     string,
     strlen (string));
}

int
x_graphics_set_function (struct xwindow * xw, unsigned int function)
{
  Display * display = (XW_DISPLAY (xw));
  if (function >= 16)
    return (1);
  XSetFunction (display, (XW_NORMAL_GC (xw)), function);
  XSetFunction (display, (XW_REVERSE_GC (xw)), function);
  return (0);
}

static void
transform_points (struct xwindow * xw,
		  double * x_vector, double * y_vector,
		  unsigned int n_points,
		  XPoint * points)
{
  double * scan_x = x_vector;
  double * end_x = x_vector + n_points;
  double * scan_y = y_vector;
  XPoint * scan_points = points;
  unsigned int border = (XW_INTERNAL_BORDER_WIDTH (xw));
  while (scan_x < end_x)
    {
      (scan_points -> x) = (border + (X_COORDINATE ((*scan_x++), xw, 0)));
      (scan_points -> y) = (border + (X_COORDINATE ((*scan_y++), xw, 0)));
      scan_points += 1;
    }
}

void
x_graphics_draw_points (struct xwindow * xw,
			double * x_vector, double * y_vector,
			unsigned int n_points, XPoint * points)
{
  transform_points (xw, x_vector, y_vector, n_points, points);
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
}

void
x_graphics_draw_lines (struct xwindow * xw,
		       double * x_vector, double * y_vector,
		       unsigned int n_points, XPoint * points)
{
  transform_points (xw, x_vector, y_vector, n_points, points);
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
}

int
x_graphics_set_fill_style (struct xwindow * xw, unsigned int fill_style)
{
  Display * display = (XW_DISPLAY (xw));
  if (fill_style >= 4)
    return (0);
  XSetFillStyle (display, (XW_NORMAL_GC (xw)), fill_style);
  XSetFillStyle (display, (XW_REVERSE_GC (xw)), fill_style);
  return (1);
}

int
x_graphics_set_line_style (struct xwindow * xw, unsigned int style)
{
  Display * display = (XW_DISPLAY (xw));
  if (style >= 3)
    return (0);
  XSetLineAttributes
    (display, (XW_NORMAL_GC (xw)), 0, style, CapButt, JoinMiter);
  XSetLineAttributes
    (display, (XW_REVERSE_GC (xw)), 0, style, CapButt, JoinMiter);
  return (1);
}

int
x_graphics_set_dashes (struct xwindow * xw, int dash_offset,
		       char * dash_list, int dash_list_length)
{
  Display * display = (XW_DISPLAY (xw));
  if (dash_offset >= dash_list_length)
    return (0);
  XSetDashes
    (display, (XW_NORMAL_GC (xw)), dash_offset, dash_list, dash_list_length);
  XSetDashes
    (display, (XW_REVERSE_GC (xw)), dash_offset, dash_list, dash_list_length);
  return (1);
}

int
x_graphics_copy_area (struct xwindow * source_xw,
		      struct xwindow * destination_xw,
		      int source_x, int source_y,
		      int width, int height,
		      int dest_x, int dest_y)
{
  unsigned int source_internal_border_width
    = (XW_INTERNAL_BORDER_WIDTH (source_xw));
  unsigned int destination_internal_border_width
    = (XW_INTERNAL_BORDER_WIDTH (destination_xw));
  Display *source_display = XW_DISPLAY (source_xw);
  Display *destination_display = XW_DISPLAY (destination_xw);
  if (source_display != destination_display)
    return (0);
  XCopyArea (source_display,
	     (XW_WINDOW (source_xw)),
	     (XW_WINDOW (destination_xw)),
	     (XW_NORMAL_GC (source_xw)),
	     (source_internal_border_width
	      + (X_COORDINATE (source_x, source_xw, -1))),
	     (source_internal_border_width
	      + (Y_COORDINATE (source_y, source_xw, 1))),
	     (X_LENGTH (width, source_xw)),
	     (Y_LENGTH (height, source_xw)),
	     (destination_internal_border_width
	      + (X_COORDINATE (dest_x, destination_xw, -1))),
	     (destination_internal_border_width
	      + (Y_COORDINATE (dest_y, destination_xw, 1))));
  return (1);
}

void
transform_polygon_points (struct xwindow * xw, double * vector, int length,
			  XPoint * result)
{
  unsigned int border = (XW_INTERNAL_BORDER_WIDTH (xw));
  {
    XPoint * scan_result = result;
    double * scan = vector;
    double * end = (scan + length);
    double coord;
    while (scan < end)
      {
	coord = (*scan++);
	(scan_result -> x) = (border + (X_COORDINATE (coord, xw, 0)));
	coord = (*scan++);
	(scan_result -> y) = (border + (Y_COORDINATE (coord, xw, 0)));
	scan_result += 1;
      }
  }
}

void
x_graphics_fill_polygon (struct xwindow * xw,
			 double * vector, unsigned int length,
			 XPoint * points)
{
  transform_polygon_points (xw, vector, length, points);
  XFillPolygon ((XW_DISPLAY (xw)),
		(XW_WINDOW (xw)),
		(XW_NORMAL_GC (xw)),
		points,
		(length / 2),
		Nonconvex,
		CoordModeOrigin);
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

struct ximage *
x_create_image (struct xwindow * xw, uint width, uint height)
{
  Window window = (XW_WINDOW (xw));
  Display * dpy = (XW_DISPLAY (xw));
  XWindowAttributes attrs;
  XPixmapFormatValues pixmap_format;
  unsigned int bits_per_line;
  unsigned int bitmap_pad;
  unsigned int bytes_per_line;
  char * data;

  XGetWindowAttributes (dpy, window, (&attrs));
  if (!find_pixmap_format (dpy, (attrs . depth), (&pixmap_format)))
    return (NULL);
  bits_per_line = ((pixmap_format . bits_per_pixel) * width);
  bitmap_pad = (pixmap_format . scanline_pad);
  if ((bits_per_line % bitmap_pad) != 0)
    bits_per_line += (bitmap_pad - (bits_per_line % bitmap_pad));
  bytes_per_line = ((bits_per_line + (CHAR_BIT - 1)) / CHAR_BIT);
  data = malloc (height * bytes_per_line);
  if (data == NULL)
    return (NULL);
  return (allocate_x_image
	  (XCreateImage
	   (dpy,
	    (DefaultVisualOfScreen (attrs . screen)),
	    (attrs . depth),
	    ZPixmap,
	    0,
	    data,
	    width,
	    height,
	    bitmap_pad,
	    bytes_per_line)));
}

int
x_bytes_into_image (unsigned char * vector, int length, struct ximage *ximage)
{
  XImage * image = (XI_IMAGE (ximage));
  unsigned long width = (image -> width);
  unsigned long height = (image -> height);
  unsigned char * vscan;
  unsigned long x;
  unsigned long y;
  if (length != (width * height))
    return (1);
  vscan = vector;
  for (y = 0; (y < height); y += 1)
    for (x = 0; (x < width); x += 1)
      XPutPixel (image, x, y, ((unsigned long) (*vscan++)));
  return (0);
}

long
x_get_pixel_from_image (struct ximage * xi, int x, int y)
{
  XImage * image = (XI_IMAGE (xi));
  if ((x >= (image -> width))
      || (y >= (image -> height)))
    return (-1);
  return (XGetPixel (image, x, y));
}

int
x_set_pixel_in_image (struct ximage * xi, int x, int y, unsigned long pixel)
{
  XImage * image = (XI_IMAGE (xi));
  if ((x >= (image -> width))
      || (y >= (image -> height)))
    return (0);
  XPutPixel (image, x, y, pixel);
  return (1);
}

void
x_destroy_image (struct ximage * xi)
{
  XDestroyImage (XI_IMAGE (xi));
  deallocate_x_image (xi);
}

int
x_display_image (struct ximage * xi,
		 unsigned int x_offset, unsigned int y_offset,
		 struct xwindow * xw,
		 unsigned int window_xoff, unsigned int window_yoff,
		 unsigned int width, unsigned int height)
{
  XImage * image = (XI_IMAGE (xi));
  unsigned int image_width = (image -> width);
  unsigned int image_height = (image -> height);
  if ((x_offset >= image_width)
      || (y_offset >= image_height)
      || (width >= ((image_width - x_offset) + 1))
      || (height >= ((image_height - y_offset) + 1)))
    return (0);
  XPutImage
      ((XW_DISPLAY (xw)),(XW_WINDOW (xw)),(XW_NORMAL_GC (xw)),
       image, x_offset, y_offset,
       (X_COORDINATE (window_xoff, xw, -1)),
       (Y_COORDINATE (window_yoff, xw, 1)),
       width, height);
  return (1);
}


void
x_read_image (struct ximage * xi,
	      long XImageOffset, long YImageOffset,
	      struct xwindow * xw,
	      long XWindowOffset, long YWindowOffset,
	      long Width, long Height)
{
  XGetSubImage(XW_DISPLAY(xw), XW_WINDOW(xw), XWindowOffset, YWindowOffset,
	       Width, Height, -1, ZPixmap,
	       XI_IMAGE(xi), XImageOffset, YImageOffset);
}

int
x_window_depth (struct xwindow * xw)
{
  XWindowAttributes attrs;
  XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&attrs));
  return (attrs . depth);
}

float
x_graphics_map_x_coordinate (struct xwindow * xw, int signed_xp)
{
  unsigned int xp = ((signed_xp < 0) ? 0 : ((unsigned int) signed_xp));
  int bx = (xp - (XW_INTERNAL_BORDER_WIDTH (xw)));
  return (x_coordinate_map (xw,
			    ((bx < 0) ? 0
			     : (bx >= (XW_X_SIZE (xw))) ? ((XW_X_SIZE (xw)) - 1)
			     : bx)));
}

float
x_graphics_map_y_coordinate (struct xwindow * xw, int signed_yp)
{
  unsigned int yp = ((signed_yp < 0) ? 0 : ((unsigned int) signed_yp));
  int by = (yp - (XW_INTERNAL_BORDER_WIDTH (xw)));
  return (y_coordinate_map (xw,
			    ((by < 0) ? 0
			     : (by >= (XW_Y_SIZE (xw)))
			     ? ((XW_Y_SIZE (xw)) - 1)
			     : by)));
}
