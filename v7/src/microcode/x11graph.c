
/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11graph.c,v 1.17 1991/10/02 21:16:39 jinx Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

/* Simple graphics for X11 */

#include "scheme.h"
#include "prims.h"
#include "x11.h"

#define RESOURCE_NAME "scheme-graphics"
#define DEFAULT_RESOURCE_CLASS "SchemeGraphics"
#define DEFAULT_RESOURCE_NAME "schemeGraphics"
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

static unsigned int
DEFUN (arg_x_coordinate, (arg, xw),
       unsigned int arg AND
       struct xwindow * xw)
{
  float virtual_device_x = (arg_real_number (arg));
  float device_x = ((XW_X_SLOPE (xw)) * (virtual_device_x - (XW_X_LEFT (xw))));
  return (ROUND_FLOAT (device_x));
}

static unsigned int
DEFUN (arg_y_coordinate, (arg, xw),
       unsigned int arg AND
       struct xwindow * xw)
{
  float virtual_device_y = (arg_real_number (arg));
  float device_y =
    ((XW_Y_SLOPE (xw)) * (virtual_device_y - (XW_Y_BOTTOM (xw))));
  return (((int) ((XW_Y_SIZE (xw)) - 1)) + (ROUND_FLOAT (device_y)));
}

static SCHEME_OBJECT
DEFUN (x_coordinate_map, (xw, x), struct xwindow * xw AND unsigned int x)
{
  return
    (FLOAT_TO_FLONUM ((((float) x) / (XW_X_SLOPE (xw))) + (XW_X_LEFT (xw))));
}

static SCHEME_OBJECT
DEFUN (y_coordinate_map, (xw, y), struct xwindow * xw AND unsigned int y)
{
  return
    (FLOAT_TO_FLONUM
     (((((float) y) - ((XW_Y_SIZE (xw)) - 1)) / (XW_Y_SLOPE (xw)))
        + (XW_Y_BOTTOM (xw))));
}

static void
DEFUN (set_clip_rectangle, (xw, x_left, y_bottom, x_right, y_top),
       struct xwindow * xw AND
       unsigned int x_left AND
       unsigned int y_bottom AND
       unsigned int x_right AND
       unsigned int y_top)
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
DEFUN (reset_clip_rectangle, (xw), struct xwindow * xw)
{
  set_clip_rectangle
    (xw, 0, ((XW_Y_SIZE (xw)) - 1), ((XW_X_SIZE (xw)) - 1), 0);
}

static void
DEFUN (reset_virtual_device_coordinates, (xw), struct xwindow * xw)
{
  /* Note that the expression ((XW_c_SIZE (xw)) - 1) guarantees that
     both limits of the device coordinates will be inside the window. */
  (XW_X_SLOPE (xw)) =
    (((float) ((XW_X_SIZE (xw)) - 1)) /
     ((XW_X_RIGHT (xw)) - (XW_X_LEFT (xw))));
  (XW_Y_SLOPE (xw)) =
    (((float) ((XW_Y_SIZE (xw)) - 1)) /
     ((XW_Y_BOTTOM (xw)) - (XW_Y_TOP (xw))));
  reset_clip_rectangle (xw);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-VDC-EXTENT", Prim_x_graphics_set_vdc_extent, 5, 5,
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

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-CLIP-RECTANGLE", Prim_x_graphics_set_clip_rectangle, 5, 5,
  "(X-GRAPHICS-SET-CLIP-RECTANGLE WINDOW X-LEFT Y-BOTTOM X-RIGHT Y-TOP)\n\
Set the clip rectangle to the given coordinates.")
{
  PRIMITIVE_HEADER (5);
  {
    struct xwindow * xw = (x_window_arg (1));
    set_clip_rectangle
      (xw,
       (arg_x_coordinate (2, xw)),
       (arg_y_coordinate (3, xw)),
       (arg_x_coordinate (4, xw)),
       (arg_y_coordinate (5, xw)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
DEFUN (process_event, (xw, event),
       struct xwindow * xw AND
       XEvent * event)
{
  switch (event -> type)
    {
    case ConfigureNotify:
      {
	unsigned int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
	unsigned int x_size = (((event -> xconfigure) . width) - extra);
	unsigned int y_size = (((event -> xconfigure) . height) - extra);
	if ((x_size != (XW_X_SIZE (xw))) || (y_size != (XW_Y_SIZE (xw))))
	  {
	    (XW_X_SIZE (xw)) = x_size;
	    (XW_Y_SIZE (xw)) = y_size;
	    reset_virtual_device_coordinates (xw);
	    XClearWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
	  }
      }
      break;
    }
}

static void
DEFUN (wm_set_size_hint, (xw, geometry_mask, x, y),
       struct xwindow  * xw AND
       int geometry_mask AND
       int x AND
       int y)
{
  unsigned int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  XSizeHints size_hints;
  (size_hints . flags) =
    (PResizeInc
     | PMinSize
     | (((geometry_mask & XValue) && (geometry_mask & YValue))
	? USPosition : PPosition)
     | (((geometry_mask & WidthValue) && (geometry_mask & HeightValue))
	? USSize : PSize));
  (size_hints . x) = x;
  (size_hints . y) = y;
  (size_hints . width) = ((XW_X_SIZE (xw)) + extra);
  (size_hints . height) = ((XW_Y_SIZE (xw)) + extra);
  (size_hints . width_inc) = 1;
  (size_hints . height_inc) = 1;
  (size_hints . min_width) = extra;
  (size_hints . min_height) = extra;
  XSetNormalHints ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&size_hints));
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
    x_default_attributes (display, RESOURCE_NAME, (&attributes));
    (wattributes . background_pixel) = (attributes . background_pixel);
    (wattributes . border_pixel) = (attributes . border_pixel);
    (wattributes . backing_store) = Always;
    (methods . deallocator) = 0;
    (methods . event_processor) = process_event;
    (methods . x_coordinate_map) = x_coordinate_map;
    (methods . y_coordinate_map) = y_coordinate_map;
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
			(display, RESOURCE_NAME, "geometry", "Geometry", 0))
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
	XStoreName (display, window, "scheme-graphics");
	XSetIconName (display, window, "scheme-graphics");
	XSelectInput (display, window, StructureNotifyMask);
	if ((ARG_REF (3)) == SHARP_F)
	  {
	    XClassHint *class_hint = XAllocClassHint ();

	    if (class_hint == NULL)
	      error_external_return ();
	    class_hint->res_class = DEFAULT_RESOURCE_CLASS;
	    class_hint->res_name = DEFAULT_RESOURCE_NAME;
	    XSetClassHint (display, window, class_hint);
	    XFree ((caddr_t) class_hint);
	    XMapWindow (display, window);
	    XFlush (display);
	  }
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
    unsigned int new_x_cursor = (arg_x_coordinate (4, xw));
    unsigned int new_y_cursor = (arg_y_coordinate (5, xw));
    unsigned int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
    XDrawLine
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       (XW_NORMAL_GC (xw)),
       (internal_border_width + (arg_x_coordinate (2, xw))),
       (internal_border_width + (arg_y_coordinate (3, xw))),
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
    (XW_X_CURSOR (xw)) = (arg_x_coordinate (2, xw));
    (XW_Y_CURSOR (xw)) = (arg_y_coordinate (3, xw));
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
    unsigned int new_x_cursor = (arg_x_coordinate (2, xw));
    unsigned int new_y_cursor = (arg_y_coordinate (3, xw));
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
       (internal_border_width + (arg_x_coordinate (2, xw))),
       (internal_border_width + (arg_y_coordinate (3, xw))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-STRING", Prim_x_graphics_draw_string, 4, 4,
  "(X-GRAPHICS-DRAW-STRING WINDOW X Y STRING)\n\
Draw characters in the current font at the given coordinates.")
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
       (internal_border_width + (arg_x_coordinate (2, xw))),
       (internal_border_width + (arg_y_coordinate (3, xw))),
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
    unsigned int function = (arg_index_integer (2, 16));
    XSetFunction (display, (XW_NORMAL_GC (xw)), function);
    XSetFunction (display, (XW_REVERSE_GC (xw)), function);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-FILL-STYLE", Prim_x_graphics_set_fill_style, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned int fill_style = (arg_index_integer (2, 4));
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
    unsigned int style = (arg_index_integer (2, 3));
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
    unsigned int dash_offset = (arg_index_integer (2, dash_list_length));
    XSetDashes
      (display, (XW_NORMAL_GC (xw)), dash_offset, dash_list, dash_list_length);
    XSetDashes
      (display, (XW_REVERSE_GC (xw)), dash_offset, dash_list,
       dash_list_length);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-CREATE-IMAGE", Prim_x_create_image, 3, 3,
  "Arguments: Window, width, height\n\
Returns:   A Scheme image\n\
\n\
The window is used to find the Display, Visual, and Depth\n\
information needed to crate an XImage structure.")
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    Window window = (XW_WINDOW (xw));
    Display * dpy = (XW_DISPLAY (xw));
    unsigned int width = (arg_nonnegative_integer (2));
    unsigned int height = (arg_nonnegative_integer (3));
    unsigned int bitmap_pad = (BitmapPad (dpy));
    unsigned int byte_pad = (bitmap_pad / CHAR_BIT);
    unsigned int bytes_per_line =
      (((width + (byte_pad - 1)) / byte_pad) * byte_pad);
    XWindowAttributes attrs;
    XGetWindowAttributes (dpy, window, (&attrs));
    PRIMITIVE_RETURN
      (X_IMAGE_TO_OBJECT
       (XCreateImage
	(dpy,
	 (DefaultVisualOfScreen (attrs . screen)),
	 (attrs . depth),
	 ZPixmap,
	 0,
	 ((char *)
	  (x_malloc (height
		     * bytes_per_line
		     * ((((attrs . depth) - 1) / 8) + 1)))),
	 width,
	 height,
	 bitmap_pad,
	 bytes_per_line)));
  }
}

DEFINE_PRIMITIVE ("X-BYTES-INTO-IMAGE", Prim_x_bytes_into_image, 2, 2,
  "Stick the bytes from the vector-8b (first arg) into the x_image (second arg).")
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT vector = ARG_REF (1);
    XImage * image = XI_IMAGE (x_image_arg (2));
    char * image_scan;
    unsigned long width = (image -> width);
    unsigned long height = (image -> height);
    int x, y;

    if (! (STRING_P (vector)))
      error_wrong_type_arg (1);
    if (STRING_LENGTH(vector) != (width * height))
      error_bad_range_arg (1);

    image_scan = ((char *) STRING_LOC (vector, 0));
    for (y = 0; y < height; y++)
      for (x = 0; x < width; x++)
	XPutPixel (image, x, y, ((unsigned long) *image_scan++));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE("X-GET-PIXEL-FROM-IMAGE", Prim_x_get_image_pixel, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    XImage * image = (XI_IMAGE (x_image_arg (1)));
    PRIMITIVE_RETURN
      (long_to_integer
       (XGetPixel (image,
		   (arg_index_integer (2, (image -> width))),
		   (arg_index_integer (3, (image -> height))))));
  }
}

DEFINE_PRIMITIVE("X-SET-PIXEL-IN-IMAGE", Prim_x_set_image_pixel, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  {
    XImage * image = (XI_IMAGE (x_image_arg (1)));
    XPutPixel (image,
	       (arg_index_integer (2, (image -> width))),
	       (arg_index_integer (3, (image -> height))),
	       (arg_integer (4)));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("X-DESTROY-IMAGE", Prim_x_destroy_image, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct ximage * xi = (x_image_arg (1));
    XDestroyImage (XI_IMAGE (xi));
    deallocate_x_image (xi);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("X-DISPLAY-IMAGE", Prim_x_display_image, 8, 8, 0)
{
  /* Called with Image, X-offset in image, Y-offset in image,
     Window, X-offset in window, Y-offset in window,
     Width, Height */
  PRIMITIVE_HEADER (8);
  {
    XImage * image = (XI_IMAGE (x_image_arg (1)));
    unsigned int image_width = (image -> width);
    unsigned int image_height = (image -> height);
    unsigned int x_offset = (arg_index_integer (2, image_width));
    unsigned int y_offset = (arg_index_integer (3, image_height));
    struct xwindow * xw = (x_window_arg (4));
    XPutImage
      ((XW_DISPLAY (xw)),(XW_WINDOW (xw)),(XW_NORMAL_GC (xw)),
       image, x_offset, y_offset,
       (arg_x_coordinate (5, xw)),
       (arg_y_coordinate (6, xw)),
       (arg_index_integer (7, ((image_width - x_offset) + 1))),
       (arg_index_integer (8, ((image_height - y_offset) + 1))));
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("X-READ-IMAGE", Prim_x_read_image, 8, 8, 0)
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

DEFINE_PRIMITIVE ("X-WINDOW-DEPTH", Prim_x_window_depth, 1, 1, 0)
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
    unsigned int xp = (arg_nonnegative_integer (2));
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
    unsigned int yp = (arg_nonnegative_integer (2));
    int by = (yp - (XW_INTERNAL_BORDER_WIDTH (xw)));
    PRIMITIVE_RETURN
      (y_coordinate_map
       (xw,
	((by < 0) ? 0
	 : (by >= (XW_Y_SIZE (xw))) ? ((XW_Y_SIZE (xw)) - 1)
	 : by)));
  }
}
