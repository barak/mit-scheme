/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11graph.c,v 1.3 1989/06/27 10:10:01 cph Rel $

Copyright (c) 1989 Massachusetts Institute of Technology

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
#include "string.h"
#include "x11.h"

#define RESOURCE_NAME "scheme-graphics"
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

#define FLONUM_ARG(argno, target)					\
{									\
  fast Pointer argument;						\
  fast long fixnum_value;						\
									\
  argument = (ARG_REF (argno));						\
  switch (OBJECT_TYPE (argument))					\
    {									\
    case TC_FIXNUM:							\
      FIXNUM_VALUE (argument, fixnum_value);				\
      target = ((float) fixnum_value);					\
      break;								\
									\
    case TC_BIG_FLONUM:							\
      target = ((float) (Get_Float (argument)));			\
      break;								\
									\
    default:								\
      error_wrong_type_arg (argno);					\
    }									\
}

#define ROUND_FLOAT(flonum)						\
  ((int) (((flonum) >= 0.0) ? ((flonum) + 0.5) : ((flonum) - 0.5)))

static int
arg_x_coordinate (arg, xw)
     int arg;
     struct xwindow * xw;
{
  float virtual_device_x;
  float device_x;

  FLONUM_ARG (arg, virtual_device_x);
  device_x = ((XW_X_SLOPE (xw)) * (virtual_device_x - (XW_X_LEFT (xw))));
  return (ROUND_FLOAT (device_x));
}

static int
arg_y_coordinate (arg, xw)
     int arg;
     struct xwindow * xw;
{
  float virtual_device_y;
  float device_y;

  FLONUM_ARG (arg, virtual_device_y);
  device_y = ((XW_Y_SLOPE (xw)) * (virtual_device_y - (XW_Y_BOTTOM (xw))));
  return (((XW_Y_SIZE (xw)) - 1) + (ROUND_FLOAT (device_y)));
}

static void
set_clip_rectangle (xw, x_left, y_bottom, x_right, y_top)
     struct xwindow * xw;
     int x_left;
     int y_bottom;
     int x_right;
     int y_top;
{
  XRectangle rectangles [1];
  Display * display = (XW_DISPLAY (xw));
  int internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));

  if (x_left > x_right)
    {
      int x = x_left;
      x_left = x_right;
      x_right = x;
    }
  if (y_top > y_bottom)
    {
      int y = y_top;
      y_top = y_bottom;
      y_bottom = y;
    }
  ((rectangles [0]) . x) = x_left;
  ((rectangles [0]) . y) = y_top;
  ((rectangles [0]) . width) = ((x_right + 1) - x_left);
  ((rectangles [0]) . height) = ((y_bottom + 1) - y_top);
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
  return;
}

static void
reset_clip_rectangle (xw)
     struct xwindow * xw;
{
  set_clip_rectangle
    (xw, 0, ((XW_Y_SIZE (xw)) - 1), ((XW_X_SIZE (xw)) - 1), 0);
  return;
}

static void
reset_virtual_device_coordinates (xw)
     struct xwindow * xw;
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
  return;
}

static void
process_event (xw, event)
     struct xwindow * xw;
     XEvent * event;
{
  switch (event -> type)
    {
    case ConfigureNotify:
      if (x_debug) fprintf (stderr, "\nX event: ConfigureNotify\n");
      {
	int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
	int x_size = (((event -> xconfigure) . width) - extra);
	int y_size = (((event -> xconfigure) . height) - extra);
	if ((x_size != (XW_X_SIZE (xw))) || (y_size != (XW_Y_SIZE (xw))))
	  {
	    (XW_X_SIZE (xw)) = x_size;
	    (XW_Y_SIZE (xw)) = y_size;
	    reset_virtual_device_coordinates (xw);
	    (XW_EVENT_FLAGS (xw)) |= EVENT_FLAG_RESIZED;
	    XClearWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
	  }
      }
      break;

    case MapNotify:
      if (x_debug) fprintf (stderr, "\nX event: MapNotify\n");
      (XW_VISIBLE_P (xw)) = 1;
      break;

    case UnmapNotify:
      if (x_debug) fprintf (stderr, "\nX event: UnmapNotify\n");
      (XW_VISIBLE_P (xw)) = 0;
      break;

    case CirculateNotify:
      if (x_debug) fprintf (stderr, "\nX event: CirculateNotify\n");
      break;

    case CreateNotify:
      if (x_debug) fprintf (stderr, "\nX event: CreateNotify\n");
      break;

    case DestroyNotify:
      if (x_debug) fprintf (stderr, "\nX event: DestroyNotify\n");
      break;

    case GravityNotify:
      if (x_debug) fprintf (stderr, "\nX event: GravityNotify\n");
      break;

    case ReparentNotify:
      if (x_debug) fprintf (stderr, "\nX event: ReparentNotify\n");
      break;

    default:
      if (x_debug) fprintf (stderr, "\nX event: %d", (event -> type));
      break;
    }
  return;
}

static void
process_events (xw)
     struct xwindow * xw;
{
  XEvent event;
  while (xw_dequeue_event (xw, (& event)))
    process_event (xw, (& event));
  return;
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-VDC-EXTENT", Prim_x_graphics_set_vdc_extent, 5, 5,
  "(X-GRAPHICS-SET-VDC-EXTENT WINDOW X-MIN Y-MIN X-MAX Y-MAX)\n\
Set the virtual device coordinates to the given values.")
{
  struct xwindow * xw;
  float x_left;
  float y_bottom;
  float x_right;
  float y_top;
  PRIMITIVE_HEADER (5);

  xw = (WINDOW_ARG (1));
  FLONUM_ARG (2, x_left);
  FLONUM_ARG (3, y_bottom);
  FLONUM_ARG (4, x_right);
  FLONUM_ARG (5, y_top);
  process_events (xw);
  (XW_X_LEFT (xw)) = x_left;
  (XW_Y_BOTTOM (xw)) = y_bottom;
  (XW_X_RIGHT (xw)) = x_right;
  (XW_Y_TOP (xw)) = y_top;
  reset_virtual_device_coordinates (xw);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-VDC-EXTENT", Prim_x_graphics_vdc_extent, 1, 1, 0)
{
  struct xwindow * xw;
  Pointer result;
  PRIMITIVE_HEADER (5);

  xw = (WINDOW_ARG (1));
  process_events (xw);
  result = (allocate_marked_vector (TC_VECTOR, 4, true));
  User_Vector_Set (result, 0, (Allocate_Float ((double) (XW_X_LEFT (xw)))));
  User_Vector_Set (result, 1, (Allocate_Float ((double) (XW_Y_BOTTOM (xw)))));
  User_Vector_Set (result, 2, (Allocate_Float ((double) (XW_X_RIGHT (xw)))));
  User_Vector_Set (result, 3, (Allocate_Float ((double) (XW_Y_TOP (xw)))));
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-RESET-CLIP-RECTANGLE", Prim_x_graphics_reset_clip_rectangle, 1, 1, 0)
{
  struct xwindow * xw;
  PRIMITIVE_HEADER (1);

  xw = (WINDOW_ARG (1));
  process_events (xw);
  reset_clip_rectangle (xw);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-CLIP-RECTANGLE", Prim_x_graphics_set_clip_rectangle, 5, 5,
  "(X-GRAPHICS-SET-CLIP-RECTANGLE WINDOW X-LEFT Y-BOTTOM X-RIGHT Y-TOP)\n\
Set the clip rectangle to the given coordinates.")
{
  struct xwindow * xw;
  int x_left;
  int y_bottom;
  int x_right;
  int y_top;
  PRIMITIVE_HEADER (5);

  xw = (WINDOW_ARG (1));
  process_events (xw);
  x_left = (arg_x_coordinate (2, xw));
  y_bottom = (arg_y_coordinate (3, xw));
  x_right = (arg_x_coordinate (4, xw));
  y_top = (arg_y_coordinate (5, xw));
  set_clip_rectangle (xw, x_left, y_bottom, x_right, y_top);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
wm_set_size_hint (xw, flags, x, y)
     struct xwindow  * xw;
     long flags;
     int x, y;
{
  int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  XSizeHints size_hints;

  (size_hints . flags) = (PResizeInc | PMinSize | flags);
  (size_hints . x) = x;
  (size_hints . y) = y;
  (size_hints . width) = ((XW_X_SIZE (xw)) + extra);
  (size_hints . height) = ((XW_Y_SIZE (xw)) + extra);
  (size_hints . width_inc) = 1;
  (size_hints . height_inc) = 1;
  (size_hints . min_width) = extra;
  (size_hints . min_height) = extra;
  XSetNormalHints ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (& size_hints));
  return;
}

#define MAKE_GC(gc, fore, back)						\
{									\
  XGCValues gcv;							\
									\
  (gcv . font) = fid;							\
  (gcv . foreground) = (fore);						\
  (gcv . background) = (back);						\
  (gc) =								\
    (XCreateGC (display,						\
		window,							\
		(GCFont | GCForeground | GCBackground),			\
		(& gcv)));						\
}

DEFINE_PRIMITIVE ("X-GRAPHICS-OPEN-WINDOW", Prim_x_graphics_open_window, 3, 3,
  "(X-GRAPHICS-OPEN-WINDOW DISPLAY GEOMETRY SUPPRESS-MAP?)\n\
Open a window on DISPLAY using GEOMETRY.\n\
If GEOMETRY is false map window interactively.\n\
If third argument SUPPRESS-MAP? is true, do not map the window immediately.")
{
  Display * display;
  int screen_number;
  char * name;
  struct drawing_attributes attributes;
  int border_width;
  int internal_border_width;
  int extra;
  int x_pos;
  int y_pos;
  int x_size;
  int y_size;
  Window window;
  long flags;
  struct xwindow * xw;
  PRIMITIVE_HEADER (3);

  display = (DISPLAY_ARG (1));
  screen_number = (DefaultScreen (display));
  name = "scheme-graphics";
  x_default_attributes (display, RESOURCE_NAME, (& attributes));
  border_width = (attributes . border_width);
  internal_border_width = (attributes . internal_border_width);
  extra = (2 * internal_border_width);
  x_pos = (-1);
  y_pos = (-1);
  x_size = 512;
  y_size = 384;
  {
    char * geometry;
    int result;

    geometry =
      (((ARG_REF (2)) == SHARP_F)
       ? (x_get_default
	  (display, RESOURCE_NAME, "geometry", "Geometry", ((char *) 0)))
       : (STRING_ARG (2)));
    result =
      (XGeometry (display, screen_number, geometry,
		  DEFAULT_GEOMETRY, border_width,
		  1, 1, extra, extra,
		  (& x_pos), (& y_pos), (& x_size), (& y_size)));
    flags = 0;
    flags |=
      (((result & XValue) && (result & YValue)) ? USPosition : PPosition);
    flags |=
      (((result & WidthValue) && (result & HeightValue)) ? USSize : PSize);
  }

  /* Open the window with the given arguments. */
  {
    XSetWindowAttributes wattributes;

    (wattributes . background_pixel) = (attributes . background_pixel);
    (wattributes . border_pixel) = (attributes . border_pixel);
    (wattributes . backing_store) = Always;
    window =
      (XCreateWindow
       (display,
	(RootWindow (display, screen_number)),
	x_pos, y_pos, (x_size + extra), (y_size + extra), border_width,
	CopyFromParent, CopyFromParent, CopyFromParent,
	(CWBackPixel | CWBorderPixel | CWBackingStore),
	(& wattributes)));
  }
  if (window == ((Window) 0))
    error_external_return ();

  xw =
    (x_make_window
     (display,
      window,
      x_size,
      y_size,
      (& attributes),
      (sizeof (struct gw_extra)),
      ((void (*) ()) 0)));
  (XW_X_LEFT (xw)) = ((float) (-1));
  (XW_X_RIGHT (xw)) = ((float) 1);
  (XW_Y_BOTTOM (xw)) = ((float) (-1));
  (XW_Y_TOP (xw)) = ((float) 1);
  reset_virtual_device_coordinates (xw);
  (XW_X_CURSOR (xw)) = 0;
  (XW_Y_CURSOR (xw)) = 0;

  XSelectInput (display, window, StructureNotifyMask);
  wm_set_size_hint (xw, flags, x_pos, y_pos);
  XStoreName (display, window, name);
  XSetIconName (display, window, name);

  if ((ARG_REF (3)) == SHARP_F)
    {
      (XW_VISIBLE_P (xw)) = 1;
      XMapWindow (display, window);
      XFlush (display);
    }

  PRIMITIVE_RETURN (x_window_to_object (xw));
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-LINE", Prim_x_graphics_draw_line, 5, 5,
  "(X-GRAPHICS-DRAW-LINE WINDOW X-START Y-START X-END Y-END)\n\
Draw a line from the start coordinates to the end coordinates.\n\
Subsequently move the graphics cursor to the end coordinates.")
{
  struct xwindow * xw;
  int new_x_cursor;
  int new_y_cursor;
  int internal_border_width;
  PRIMITIVE_HEADER (5);

  xw = (WINDOW_ARG (1));
  new_x_cursor = (arg_x_coordinate (4, xw));
  new_y_cursor = (arg_y_coordinate (5, xw));
  internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
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
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-MOVE-CURSOR", Prim_x_graphics_move_cursor, 3, 3,
  "(X-GRAPHICS-MOVE-CURSOR WINDOW X Y)\n\
Move the graphics cursor to the given coordinates.")
{
  struct xwindow * xw;
  PRIMITIVE_HEADER (3);

  xw = (WINDOW_ARG (1));
  (XW_X_CURSOR (xw)) = (arg_x_coordinate (2, xw));
  (XW_Y_CURSOR (xw)) = (arg_y_coordinate (3, xw));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAG-CURSOR", Prim_x_graphics_drag_cursor, 3, 3,
  "(X-GRAPHICS-DRAG-CURSOR WINDOW X Y)\n\
Draw a line from the graphics cursor to the given coordinates.\n\
Subsequently move the graphics cursor to those coordinates.")
{
  struct xwindow * xw;
  int new_x_cursor;
  int new_y_cursor;
  int internal_border_width;
  PRIMITIVE_HEADER (3);

  xw = (WINDOW_ARG (1));
  new_x_cursor = (arg_x_coordinate (2, xw));
  new_y_cursor = (arg_y_coordinate (3, xw));
  internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
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
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-POINT", Prim_x_graphics_draw_point, 3, 3,
  "(X-GRAPHICS-DRAW-POINT WINDOW X Y)\n\
Draw one point at the given coordinates.\n\
Subsequently move the graphics cursor to those coordinates.")
{
  struct xwindow * xw;
  int internal_border_width;
  PRIMITIVE_HEADER (3);

  xw = (WINDOW_ARG (1));
  internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
  XDrawPoint
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (XW_NORMAL_GC (xw)),
     (internal_border_width + (arg_x_coordinate (2, xw))),
     (internal_border_width + (arg_y_coordinate (3, xw))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DRAW-STRING", Prim_x_graphics_draw_string, 4, 4,
  "(X-GRAPHICS-DRAW-STRING WINDOW X Y STRING)\n\
Draw characters in the current font at the given coordinates.")
{
  struct xwindow * xw;
  int internal_border_width;
  char * s;
  PRIMITIVE_HEADER (4);

  xw = (WINDOW_ARG (1));
  internal_border_width = (XW_INTERNAL_BORDER_WIDTH (xw));
  s = (STRING_ARG (4));
  XDrawString
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (XW_NORMAL_GC (xw)),
     (internal_border_width + (arg_x_coordinate (2, xw))),
     (internal_border_width + (arg_y_coordinate (3, xw))),
     s,
     (string_length (ARG_REF (4))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-FUNCTION", Prim_x_graphics_set_function, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  int function;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  function = (arg_index_integer (2, 16));
  XSetFunction (display, (XW_NORMAL_GC (xw)), function);
  XSetFunction (display, (XW_REVERSE_GC (xw)), function);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-FILL-STYLE", Prim_x_graphics_set_fill_style, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  int fill_style;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  fill_style = (arg_index_integer (2, 4));
  XSetFillStyle (display, (XW_NORMAL_GC (xw)), fill_style);
  XSetFillStyle (display, (XW_REVERSE_GC (xw)), fill_style);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-LINE-STYLE", Prim_x_graphics_set_line_style, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  int style;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  style = (arg_index_integer (2, 3));
  XSetLineAttributes
    (display, (XW_NORMAL_GC (xw)), 0, style, CapButt, JoinMiter);
  XSetLineAttributes
    (display, (XW_REVERSE_GC (xw)), 0, style, CapButt, JoinMiter);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-SET-DASHES", Prim_x_graphics_set_dashes, 3, 3, 0)
{
  struct xwindow * xw;
  Display * display;
  int dash_offset;
  char * dash_list;
  int dash_list_length;
  PRIMITIVE_HEADER (3);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  dash_list = (STRING_ARG (3));
  dash_list_length = (string_length (ARG_REF (3)));
  dash_offset = (arg_index_integer (2, dash_list_length));
  XSetDashes
    (display, (XW_NORMAL_GC (xw)), dash_offset, dash_list, dash_list_length);
  XSetDashes
    (display, (XW_REVERSE_GC (xw)), dash_offset, dash_list, dash_list_length);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-PROCESS-EVENTS", Prim_x_graphics_process_events, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  process_events (WINDOW_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
