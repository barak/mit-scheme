/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11term.c,v 1.4 1989/04/25 03:52:54 cph Exp $

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

/* X11 terminal for Edwin. */

#include "scheme.h"
#include "prims.h"
#include "string.h"

#define UNSPECIFIC (Make_Non_Pointer (TC_TRUE, 1))

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

#define RESOURCE_NAME "edwin"
#define DEFAULT_GEOMETRY "80x40+0+0"

static char *
xterm_malloc (size)
     int size;
{
  char * result;
  extern char * malloc ();

  result = (malloc (size));
  if (result == ((char *) 0))
    error_external_return ();
  return (result);
}

static char *
xterm_realloc (ptr, size)
     char * ptr;
     int size;
{
  char * result;
  extern char * realloc ();

  result = (realloc (ptr, size));
  if (result == ((char *) 0))
    error_external_return ();
  return (result);
}

struct allocation_table
{
  char ** items;
  int length;
};

static int
allocate_table_index (table, item)
     struct allocation_table * table;
     char * item;
{
  char ** items = (table -> items);
  int length = (table -> length);
  int i;

  if (length == 0)
    {
      int new_length = 4;
      char ** new_items =
	((char **) (xterm_malloc ((sizeof (char *)) * new_length)));
      (new_items [0]) = item;
      for (i = 1; (i < new_length); i += 1)
	(new_items [i]) = ((char *) 0);
      (table -> items) = new_items;
      (table -> length) = new_length;
      return (0);
    }
  for (i = 0; (i < length); i += 1)
    if ((items [i]) == ((char *) 0))
      {
	(items [i]) = item;
	return (i);
      }
  {
    int new_length = (length * 2);
    char ** new_items =
      ((char **) (xterm_realloc (items, ((sizeof (char *)) * new_length))));
    (new_items [length]) = item;
    for (i = (length + 1); (i < new_length); i += 1)
      (new_items [i]) = ((char *) 0);
    (table -> items) = new_items;
    (table -> length) = new_length;
  }
  return (length);
}

#define DEF_ALLOCATION_ARG(name, result_type, result)			\
static result_type							\
name (arg, table)							\
     int arg;								\
     struct allocation_table * table;					\
{									\
  fast Pointer object = (ARG_REF (arg));				\
									\
  if (! (FIXNUM_P (object)))						\
    error_wrong_type_arg (arg);						\
  if (! (FIXNUM_NEGATIVE_P (object)))					\
    {									\
      fast int length = (table -> length);				\
      fast char ** items = (table -> items);				\
      fast int index = (UNSIGNED_FIXNUM_VALUE (object));		\
      if ((index < length) && ((items [index]) != ((char *) 0)))	\
	return (result);						\
    }									\
  error_bad_range_arg (arg);						\
  /* NOTREACHED */							\
}

DEF_ALLOCATION_ARG (allocation_item_arg, char *, (items [index]))
DEF_ALLOCATION_ARG (allocation_index_arg, int, index)

static struct allocation_table display_table;
static struct allocation_table xterm_table;

struct xterm
{
  Display * display;
  Window window;

  /* Dimensions of the window, in characters.  Valid character
     coordinates are nonnegative integers strictly less than these
     limits. */
  int x_size;
  int y_size;

  /* Position of the cursor, in character coordinates. */
  int cursor_x;
  int cursor_y;

  /* Width of the internal border, in pixels. */
  int internal_border_width;

  /* Character map of the window's contents.  See `XTERM_CHAR_LOC' for
     the address arithmetic. */
  char * character_map;

  /* Bit map of the window's highlighting. */
  char * highlight_map;

  /* The primary font, and its dimensions in pixels. */
  XFontStruct * font;

  /* The graphics contexts used to draw characters and the cursor. */
  GC normal_gc;
  GC reverse_gc;
  GC cursor_gc;

  /* Commonly used pixel values. */
  unsigned long background_pixel;
  unsigned long foreground_pixel;
  unsigned long cursor_pixel;
  unsigned long border_pixel;
  unsigned long mouse_pixel;

  int event_flags;

  /* Nonzero iff this window is visible (mapped and unobscured). */
  char visible_p;

  /* Nonzero iff the cursor is drawn on the window. */
  char cursor_visible_p;
};

#define DISPLAY_ARG(arg)						\
  ((Display *) (allocation_item_arg (arg, (& display_table))))

#define XTERM_ARG(arg)							\
  ((struct xterm *) (allocation_item_arg (arg, (& xterm_table))))

#define XTERM_CHAR_INDEX(xt, x, y) ((y * (xt -> x_size)) + x)
#define XTERM_CHAR_LOC(xt, index) ((xt -> character_map) + index)
#define XTERM_CHAR(xt, index) (* (XTERM_CHAR_LOC (xt, index)))
#define XTERM_HL_LOC(xt, index) ((xt -> highlight_map) + index)
#define XTERM_HL(xt, index) (* (XTERM_HL_LOC (xt, index)))

#define XTERM_X_PIXEL(xt, x)						\
  ((x * (FONT_WIDTH (xt -> font))) + (xt -> internal_border_width))

#define XTERM_Y_PIXEL(xt, y)						\
  ((y * (FONT_HEIGHT (xt -> font))) + (xt -> internal_border_width))

#define XTERM_HL_GC(xt, hl) (hl ? (xt -> reverse_gc) : (xt -> normal_gc))

#define HL_ARG(arg) arg_index_integer (arg, 2)

#define FONT_WIDTH(f)	((f -> max_bounds) . width)
#define FONT_HEIGHT(f)	((f -> ascent) + (f -> descent))
#define FONT_BASE(f)    (f -> ascent)

#define EVENT_FLAG_RESIZED	0x01

#define XTERM_DRAW_CHARS(xt, x, y, s, n, gc)				\
  XDrawImageString							\
    ((xt -> display),							\
     (xt -> window),							\
     gc,								\
     (XTERM_X_PIXEL (xt, x)),						\
     ((XTERM_Y_PIXEL (xt, y)) + (FONT_BASE (xt -> font))),		\
     s,									\
     n)

#define WITH_CURSOR_PRESERVED(xt, expression, body)			\
{									\
  if ((expression) && (xt -> cursor_visible_p))				\
    {									\
      (xt -> cursor_visible_p) = 0;					\
      body;								\
      xterm_draw_cursor (xt);						\
    }									\
  else									\
    body;								\
}

static void
xterm_erase_cursor (xt)
     struct xterm * xt;
{
  fast int x, y, index;

  if (! (xt -> visible_p))
    return;

  x = (xt -> cursor_x);
  y = (xt -> cursor_y);
  index = (XTERM_CHAR_INDEX (xt, x, y));
  XTERM_DRAW_CHARS
    (xt, x, y, (XTERM_CHAR_LOC (xt, index)), 1,
     (XTERM_HL_GC (xt, (XTERM_HL (xt, index)))));
  (xt -> cursor_visible_p) = 0;
  return;
}

static void
xterm_draw_cursor (xt)
     struct xterm * xt;
{
  fast int x, y;

  if (! (xt -> visible_p))
    return;

  /* Need option here to draw cursor as outline box when this xterm is
     not the one that input is going to. */
  x = (xt -> cursor_x);
  y = (xt -> cursor_y);
  XTERM_DRAW_CHARS (xt, x, y,
		    (XTERM_CHAR_LOC (xt, (XTERM_CHAR_INDEX (xt, x, y)))),
		    1,
		    (xt -> cursor_gc));
  (xt -> cursor_visible_p) = 1;
  return;
}

static struct xterm *
xterm_window_to_xt (window)
     Window window;
{
  int length = (xterm_table . length);
  struct xterm ** items = ((struct xterm **) (xterm_table . items));
  int i;
  struct xterm * xt;

  for (i = 0; (i < length); i += 1)
    {
      xt = (items [i]);
      if ((xt -> window) == window)
	return (xt);
    }
  return ((struct xterm *) 0);
}

static int
x_io_error_handler (display)
     Display * display;
{
  fprintf (stderr, "\nX IO Error\n");
  error_external_return ();
}

static int
x_error_handler (display, error_event)
     Display * display;
     XErrorEvent * error_event;
{
  char buffer [2048];

  XGetErrorText (display, (error_event -> error_code),
		 (& buffer), (sizeof (buffer)));
  fprintf (stderr, "\nX Error: %s\n", buffer);
  fprintf (stderr, "         Request code: %d\n",
	   (error_event -> request_code));
  fprintf (stderr, "         Error serial: %x\n", (error_event -> serial));
  error_external_return ();
}

static void
xterm_wm_set_size_hint (xt, flags, x, y)
     struct xterm * xt;
     long flags;
     int x, y;
{
  Window window = (xt -> window);
  int extra = (2 * (xt -> internal_border_width));
  XFontStruct * font = (xt -> font);
  int fwidth = (FONT_WIDTH (font));
  int fheight = (FONT_HEIGHT (font));
  XSizeHints size_hints;

  (size_hints . flags) = (PResizeInc | PMinSize | flags);
  (size_hints . x) = x;
  (size_hints . y) = y;
  (size_hints . width) = (((xt -> x_size) * fwidth) + extra);
  (size_hints . height) = (((xt -> y_size) * fheight) + extra);
  (size_hints . width_inc) = fwidth;
  (size_hints . height_inc) = fheight;
  (size_hints . min_width) = extra;
  (size_hints . min_height) = extra;
  XSetNormalHints ((xt -> display), window, (& size_hints));
  return;
}

static unsigned long
xterm_decode_color (display, color_map, color_name, default_color)
     Display * display;
     Colormap color_map;
     char * color_name;
     unsigned long default_color;
{
  XColor cdef;

  if ((strcmp (color_name, "black")) == 0)
    return (BlackPixel (display, (DefaultScreen (display))));
  if ((strcmp (color_name, "white")) == 0)
    return (WhitePixel (display, (DefaultScreen (display))));
  if (DisplayCells (display, (DefaultScreen (display))) <= 2)
    return (default_color);
  if ((XParseColor (display, color_map, color_name, (& cdef))) &&
      (XAllocColor (display, color_map, (& cdef))))
    return (cdef . pixel);
  return (default_color);
}

static unsigned long
xterm_default_color (display, color_map, property_name, default_color)
     Display * display;
     Colormap color_map;
     char * property_name;
     unsigned long default_color;
{
  char * color_name;

  color_name = (XGetDefault (display, RESOURCE_NAME, property_name));
  if (color_name == ((char *) 0))
    return (default_color);
  return (xterm_decode_color (display, color_map, color_name, default_color));
}

static Display *
xterm_close_window (index)
     int index;
{
  struct xterm * xt;
  Display * display;
  Window window;

  xt = ((struct xterm *) ((xterm_table . items) [index]));
  ((struct xterm *) ((xterm_table . items) [index])) = ((struct xterm *) 0);
  display = (xt -> display);
  free (xt -> character_map);
  free (xt -> highlight_map);
  XFreeFont (display, (xt -> font));
  XDestroyWindow (display, (xt -> window));
  free (xt);
  return (display);
}

static void
xterm_close_display (index)
     int index;
{
  Display * display;

  display = ((Display *) ((display_table . items) [index]));
  ((Display *) ((display_table . items) [index])) = ((Display *) 0);
  {
    struct xterm ** items = ((struct xterm **) (xterm_table . items));
    int length = (xterm_table . length);
    int i;

    for (i = 0; (i < length); i += 1)
      {
	struct xterm * xt = (items [i]);
	if ((xt != ((struct xterm *) 0)) &&
	    ((xt -> display) == display))
	  (void) xterm_close_window (i);
      }
  }
  XCloseDisplay (display);
  return;
}

static void
xterm_dump_rectangle (xt, x, y, width, height)
     struct xterm * xt;
     int x, y, width, height;
{
  XFontStruct * font = (xt -> font);
  int fwidth = (FONT_WIDTH (font));
  int fheight = (FONT_HEIGHT (font));
  int border = (xt -> internal_border_width);
  char * character_map = (xt -> character_map);
  char * highlight_map = (xt -> highlight_map);
  int x_start = ((x - border) / fwidth);
  int y_start = ((y - border) / fheight);
  int x_end = ((((x + width) - border) + (fwidth - 1)) / fwidth);
  int y_end = ((((y + height) - border) + (fheight - 1)) / fheight);
  int x_width = (x_end - x_start);
  int xi, yi;

  if (x_end > (xt -> x_size)) x_end = (xt -> x_size);
  if (y_end > (xt -> y_size)) y_end = (xt -> y_size);
  if (x_start < x_end)
    {
      for (yi = y_start; (yi < y_end); yi += 1)
	{
	  int index = (XTERM_CHAR_INDEX (xt, 0, yi));
	  char * line_char = (& (character_map [index]));
	  char * line_hl = (& (highlight_map [index]));
	  int xi = x_start;
	  while (1)
	    {
	      int hl = (line_hl [xi]);
	      int i = (xi + 1);
	      while ((i < x_end) && ((line_hl [i]) == hl))
		i += 1;
	      XTERM_DRAW_CHARS (xt, xi, yi,
				(& (line_char [xi])), (i - xi),
				(XTERM_HL_GC (xt, hl)));
	      if (i == x_end)
		break;
	      xi = i;
	    }
	}
      if ((xt -> cursor_visible_p) &&
	  ((x_start <= (xt -> cursor_x)) && ((xt -> cursor_x) < x_end)) &&
	  ((y_start <= (xt -> cursor_y)) && ((xt -> cursor_y) < y_end)))
	xterm_draw_cursor (xt);
    }
  return;
}

static int xterm_debug = 0;

DEFINE_PRIMITIVE ("XTERM-DEBUG", Prim_xterm_debug, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  xterm_debug = ((ARG_REF (1)) != SHARP_F);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-OPEN-DISPLAY", Prim_xterm_open_display, 1, 1, 0)
{
  Display * display;
  int index;
  PRIMITIVE_HEADER (1);

  display =
    (XOpenDisplay (((ARG_REF (1)) == SHARP_F) ? NULL : (STRING_ARG (1))));
  if (display == NULL)
    PRIMITIVE_RETURN (SHARP_F);

  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_handler);
  PRIMITIVE_RETURN
    (MAKE_UNSIGNED_FIXNUM
     (allocate_table_index ((& display_table), ((char *) display))));
}

DEFINE_PRIMITIVE ("XTERM-CLOSE-DISPLAY", Prim_xterm_close_display, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  xterm_close_display (allocation_index_arg (1, (& display_table)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-CLOSE-ALL-DISPLAYS", Prim_xterm_close_all_displays, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  
  {
    Display ** items = ((Display **) (display_table . items));
    int length = (display_table . length);
    int i;

    for (i = 0; (i < length); i += 1)
      if ((items [i]) != ((Display *) 0))
	xterm_close_display (i);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define MAKE_MAP(map, size, fill)					\
{									\
  char * MAKE_MAP_scan;							\
  char * MAKE_MAP_end;							\
									\
  map = (xterm_malloc (size));						\
  MAKE_MAP_scan = (& (map [0]));					\
  MAKE_MAP_end = (MAKE_MAP_scan + size);				\
  while (MAKE_MAP_scan < MAKE_MAP_end)					\
    (*MAKE_MAP_scan++) = fill;						\
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

DEFINE_PRIMITIVE ("XTERM-OPEN-WINDOW", Prim_xterm_open_window, 3, 3,
  "(xterm-open-window display geometry suppress-map?)")
{
  Display * display;
  int screen_number;
  XFontStruct * font;
  Font fid;
  int fwidth;
  int fheight;
  int border_width;
  int x_pos;
  int y_pos;
  int x_size;
  int y_size;
  char * name;
  int internal_border_width;
  int extra;
  unsigned long foreground_pixel;
  unsigned long background_pixel;
  unsigned long border_pixel;
  unsigned long cursor_pixel;
  unsigned long mouse_pixel;
  Window window;
  long flags;
  char * character_map;
  char * highlight_map;
  GC normal_gc;
  GC reverse_gc;
  GC cursor_gc;
  struct xterm * xt;
  PRIMITIVE_HEADER (3);

  display = (DISPLAY_ARG (1));
  screen_number = (DefaultScreen (display));
  {
    char * font_name;

    font_name = (XGetDefault (display, RESOURCE_NAME, "BodyFont"));
    if (font_name == ((char *) 0))
      font_name = "9x15";
    font = (XLoadQueryFont (display, font_name));
    if (font == ((XFontStruct *) 0))
      error_external_return ();
  }
  fid = (font -> fid);
  fwidth = (FONT_WIDTH (font));
  fheight = (FONT_HEIGHT (font));
  x_pos = (-1);
  y_pos = (-1);
  x_size = 80;
  y_size = 24;
  name = "edwin";
  {
    char * s;

    s = (XGetDefault (display, RESOURCE_NAME, "BorderWidth"));
    border_width = ((s == ((char *) 0)) ? 2 : (atoi (s)));
    s = (XGetDefault (display, RESOURCE_NAME, "InternalBorderWidth"));
    internal_border_width = ((s == ((char *) 0)) ? 4 : (atoi (s)));
  }
  extra = (2 * internal_border_width);
  {
    unsigned long white_pixel = (WhitePixel (display, screen_number));
    unsigned long black_pixel = (BlackPixel (display, screen_number));
    Colormap color_map = (DefaultColormap (display, screen_number));

    background_pixel =
      (xterm_default_color (display, color_map, "Background", white_pixel));
    foreground_pixel =
      (xterm_default_color (display, color_map, "Foreground", black_pixel));
    border_pixel =
      (xterm_default_color (display, color_map, "Border", black_pixel));
    cursor_pixel =
      (xterm_default_color (display, color_map, "Cursor", black_pixel));
    mouse_pixel =
      (xterm_default_color (display, color_map, "Mouse", black_pixel));
  }
  {
    char * geometry;
    int result;

    geometry =
      (((ARG_REF (2)) == SHARP_F)
       ? (XGetDefault (display, RESOURCE_NAME, "Geometry"))
       : (STRING_ARG (2)));
    result = 
      (XGeometry (display, screen_number, geometry,
		  DEFAULT_GEOMETRY, border_width,
		  fwidth, fheight, extra, extra,
		  (& x_pos), (& y_pos), (& x_size), (& y_size)));
    flags = 0;
    flags |=
      (((result & XValue) && (result & YValue)) ? USPosition : PPosition);
    flags |=
      (((result & WidthValue) && (result & HeightValue)) ? USSize : PSize);
  }
  {
    int map_size = (x_size * y_size);
    MAKE_MAP (character_map, map_size, ' ');
    MAKE_MAP (highlight_map, map_size, 0);
  }
  window =
    (XCreateSimpleWindow
     (display, (RootWindow (display, screen_number)),
      x_pos, y_pos, ((x_size * fwidth) + extra), ((y_size * fheight) + extra),
      border_width, border_pixel, background_pixel));
  if (window == ((Window) 0))
    error_external_return ();
  MAKE_GC (normal_gc, foreground_pixel, background_pixel);
  MAKE_GC (reverse_gc, background_pixel, foreground_pixel);
  MAKE_GC (cursor_gc, background_pixel, cursor_pixel);

  xt = ((struct xterm *) (xterm_malloc (sizeof (struct xterm))));
  (xt -> display) = display;
  (xt -> window) = window;
  (xt -> x_size) = x_size;
  (xt -> y_size) = y_size;
  (xt -> cursor_x) = 0;
  (xt -> cursor_y) = 0;
  (xt -> internal_border_width) = internal_border_width;
  (xt -> character_map) = character_map;
  (xt -> highlight_map) = highlight_map;
  (xt -> font) = font;
  (xt -> normal_gc) = normal_gc;
  (xt -> reverse_gc) = reverse_gc;
  (xt -> cursor_gc) = cursor_gc;
  (xt -> background_pixel) = background_pixel;
  (xt -> foreground_pixel) = foreground_pixel;
  (xt -> border_pixel) = border_pixel;
  (xt -> cursor_pixel) = cursor_pixel;
  (xt -> mouse_pixel) = mouse_pixel;
  (xt -> visible_p) = 0;
  (xt -> cursor_visible_p) = 0;
  (xt -> event_flags) = 0;

  XSelectInput
    (display, window,
     (KeyPressMask | ExposureMask |
      ButtonPressMask | ButtonReleaseMask |
      StructureNotifyMask | FocusChangeMask |
      PointerMotionHintMask | ButtonMotionMask |
      LeaveWindowMask | EnterWindowMask));
  xterm_wm_set_size_hint (xt, flags, x_pos, y_pos);
  XStoreName (display, window, name);
  XSetIconName (display, window, name);

  if ((ARG_REF (3)) == SHARP_F)
    {
      (xt -> visible_p) = 1;
      XMapWindow (display, window);
      XFlush (display);
    }

  PRIMITIVE_RETURN
    (MAKE_UNSIGNED_FIXNUM
     (allocate_table_index ((& xterm_table), ((char *) xt))));
}

DEFINE_PRIMITIVE ("XTERM-CLOSE-WINDOW", Prim_xterm_close_window, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  XFlush (xterm_close_window (allocation_index_arg (1, (& xterm_table))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-MAP", Prim_xterm_map, 1, 1, 0)
{
  struct xterm * xt;
  Display * display;
  PRIMITIVE_HEADER (1);

  xt = (XTERM_ARG (1));
  display = (xt -> display);
  (xt -> visible_p) = 1;
  XMapWindow (display, (xt -> window));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-UNMAP", Prim_xterm_unmap, 1, 1, 0)
{
  struct xterm * xt;
  Display * display;
  PRIMITIVE_HEADER (1);

  xt = (XTERM_ARG (1));
  display = (xt -> display);
  (xt -> visible_p) = 0;
  XUnmapWindow (display, (xt -> window));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-X-SIZE", Prim_xterm_x_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer ((XTERM_ARG (1)) -> x_size));
}

DEFINE_PRIMITIVE ("XTERM-Y-SIZE", Prim_xterm_y_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer ((XTERM_ARG (1)) -> y_size));
}

DEFINE_PRIMITIVE ("XTERM-READ-EVENT-FLAGS!", Prim_xterm_read_event_flags, 1, 1, 0)
{
  struct xterm * xt;
  int old;
  PRIMITIVE_HEADER (1);

  xt = (XTERM_ARG (1));
  old = (xt -> event_flags);
  (xt -> event_flags) = 0;
  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (old));
}

DEFINE_PRIMITIVE ("XTERM-BEEP", Prim_xterm_beep, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  XBell (((XTERM_ARG (1)) -> display), 100); /* 100% */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-FLUSH", Prim_xterm_flush, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  XFlush ((XTERM_ARG (1)) -> display);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-WRITE-CURSOR!", Prim_xterm_write_cursor, 3, 3, 0)
{
  fast struct xterm * xt;
  fast int x, y;
  PRIMITIVE_HEADER (3);

  xt = (XTERM_ARG (1));
  x = (arg_index_integer (2, (xt -> x_size)));
  y = (arg_index_integer (3, (xt -> y_size)));
  if (xt -> cursor_visible_p)
    xterm_erase_cursor (xt);
  (xt -> cursor_x) = x;
  (xt -> cursor_y) = y;
  xterm_draw_cursor (xt);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-WRITE-CHAR!", Prim_xterm_write_char, 5, 5, 0)
{
  struct xterm * xt;
  int x, y;
  int c;
  int hl;
  int index;
  char * map_ptr;
  PRIMITIVE_HEADER (5);

  xt = (XTERM_ARG (1));
  x = (arg_index_integer (2, (xt -> x_size)));
  y = (arg_index_integer (3, (xt -> y_size)));
  c = (arg_ascii_char (4));
  hl = (HL_ARG (5));
  index = (XTERM_CHAR_INDEX (xt, x, y));
  map_ptr = (XTERM_CHAR_LOC (xt, index));
  (*map_ptr) = c;
  (XTERM_HL (xt, index)) = hl;
  WITH_CURSOR_PRESERVED
    (xt, ((x == (xt -> cursor_x)) && (y == (xt -> cursor_y))),
     {
       XTERM_DRAW_CHARS (xt, x, y, map_ptr, 1, (XTERM_HL_GC (xt, (xt, hl))));
     });
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-WRITE-SUBSTRING!", Prim_xterm_write_substring, 7, 7, 0)
{
  struct xterm * xt;
  int x, y;
  Pointer string;
  int start, end;
  int hl;
  int length;
  char * string_scan;
  char * string_end;
  int index;
  char * char_start;
  char * char_scan;
  char * hl_scan;
  PRIMITIVE_HEADER (7);

  xt = (XTERM_ARG (1));
  x = (arg_index_integer (2, (xt -> x_size)));
  y = (arg_index_integer (3, (xt -> y_size)));
  CHECK_ARG (4, STRING_P);
  string = (ARG_REF (4));
  end = (arg_index_integer (6, ((string_length (string)) + 1)));
  start = (arg_index_integer (5, (end + 1)));
  hl = (HL_ARG (7));
  length = (end - start);
  if ((x + length) > (xt -> x_size))
    error_bad_range_arg (2);
  string_scan = (string_pointer (string, start));
  string_end = (string_pointer (string, end));
  index = (XTERM_CHAR_INDEX (xt, x, y));
  char_start = (XTERM_CHAR_LOC (xt, index));
  char_scan = char_start;
  hl_scan = (XTERM_HL_LOC (xt, index));
  while (string_scan < string_end)
    {
      (*char_scan++) = (*string_scan++);
      (*hl_scan++) = hl;
    }
  WITH_CURSOR_PRESERVED
    (xt,
     ((y == (xt -> cursor_y)) &&
      ((x <= (xt -> cursor_x)) && ((xt -> cursor_x) < (x + length)))),
     {
       XTERM_DRAW_CHARS (xt, x, y, char_start, length, (XTERM_HL_GC (xt, hl)));
     });
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-CLEAR-RECTANGLE!", Prim_xterm_clear_rectangle, 6, 6, 0)
{
  struct xterm * xt;
  int start_x, start_y, end_x, end_y;
  int hl;
  int x_length;
  PRIMITIVE_HEADER (6);

  xt = (XTERM_ARG (1));
  end_x = (arg_index_integer (3, ((xt -> x_size) + 1)));
  end_y = (arg_index_integer (5, ((xt -> y_size) + 1)));
  start_x = (arg_index_integer (2, (end_x + 1)));
  start_y = (arg_index_integer (4, (end_y + 1)));
  hl = (HL_ARG (6));
  if ((start_x == end_x) || (start_y == end_y))
    goto done;
  x_length = (end_x - start_x);
  {
    int y;
    int index;
    fast char * char_scan;
    fast char * char_end;
    fast char * hl_scan;

    for (y = start_y; (y < end_y) ; (y += 1))
      {
	index = (XTERM_CHAR_INDEX (xt, start_x, y));
	char_scan = (XTERM_CHAR_LOC (xt, index));
	char_end = (char_scan + x_length);
	hl_scan = (XTERM_HL_LOC (xt, index));
	while (char_scan < char_end)
	  {
	    (*char_scan++) = ' ';
	    (*hl_scan++) = hl;
	  }
      }
  }
  WITH_CURSOR_PRESERVED
    (xt,
     (((start_x <= (xt -> cursor_x)) && ((xt -> cursor_x) < end_x)) &&
      ((start_y <= (xt -> cursor_y)) && ((xt -> cursor_y) < end_y))),
     {
       if (hl == 0)
	 XClearArea ((xt -> display), (xt -> window),
		     (XTERM_X_PIXEL (xt, start_x)),
		     (XTERM_Y_PIXEL (xt, start_y)),
		     ((end_x - start_x) * (FONT_WIDTH (xt -> font))),
		     ((end_y - start_y) * (FONT_HEIGHT (xt -> font))),
		     False);
       else
	 {
	   fast int y;
	   GC hl_gc;

	   hl_gc = (XTERM_HL_GC (xt, hl));
	   for (y = start_y; (y < end_y) ; (y += 1))
	     XTERM_DRAW_CHARS
	       (xt,
		start_x,
		y,
		(XTERM_CHAR_LOC (xt, (XTERM_CHAR_INDEX (xt, start_x, y)))),
		x_length,
		hl_gc);
	 }
     });
 done:
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void xterm_process_event ();

DEFINE_PRIMITIVE ("XTERM-READ-CHARS", Prim_xterm_read_chars, 2, 2, 0)
{
  struct xterm * xt;
  Display * display;
  int interval;
  long time_limit;
  char copy_buffer [80];
  int buffer_length;
  int buffer_index;
  char * buffer;
  fast int nbytes;
  int nevents;
  fast char * scan_buffer;
  fast char * scan_copy;
  fast char * end_copy;
  XEvent event;
  KeySym keysym;
  int * status;
  extern long OS_real_time_clock ();
  PRIMITIVE_HEADER (2);

  /* change this to allocate and return an appropriately sized string */
  xt = (XTERM_ARG (1));
  display = (xt -> display);
  interval =
    (((ARG_REF (2)) == SHARP_F) ? (-1) : (arg_nonnegative_integer (2)));
  if (interval >= 0)
    time_limit = ((OS_real_time_clock ()) + interval);
  buffer_length = 4;
  buffer_index = 0;
  buffer = (xterm_malloc (buffer_length));
  scan_buffer = buffer;
  nevents = (XEventsQueued (display, QueuedAfterReading));
  while (1)
    {
      if (nevents == 0)
	{
	  if ((buffer != scan_buffer) ||
	      ((xt -> event_flags) != 0) ||
	      (interval == 0))
	    break;
	  if (interval > 0)
	    {
	      if ((OS_real_time_clock ()) < time_limit)
		{
		  nevents = (XEventsQueued (display, QueuedAfterReading));
		  continue;
		}
	      break;
	    }
	  nevents = 1;
	}
      XNextEvent (display, (& event));
      nevents -= 1;
      if ((event . type) != KeyPress)
	{
	  xterm_process_event (& event);
	  continue;
	}
      status = ((int *) 0);
      nbytes =
	(XLookupString ((& event),
			(& (copy_buffer [0])),
			(sizeof (copy_buffer)),
			(& keysym),
			status));
      if ((IsFunctionKey (keysym)) ||
	  (IsCursorKey (keysym)) ||
	  (IsKeypadKey (keysym)) ||
	  (IsMiscFunctionKey (keysym)))
	continue;
      if (((event . xkey . state) & Mod1Mask) != 0)
	(copy_buffer [0]) |= 0x80;
      if (nbytes > (buffer_length - buffer_index))
	{
	  buffer_length *= 2;
	  buffer = (xterm_realloc (buffer, buffer_length));
	  scan_buffer = (buffer + buffer_index);
	}
      scan_copy = (& (copy_buffer [0]));
      end_copy = (scan_copy + nbytes);
      while (scan_copy < end_copy)
	(*scan_buffer++) = (*scan_copy++);
      buffer_index = (scan_buffer - buffer);
    }
  /* If we got characters, return them */
  if (buffer != scan_buffer)
    PRIMITIVE_RETURN (memory_to_string (buffer_index, buffer));
  /* If we're in a read with timeout, and we stopped before the
     timeout was finished, return the amount remaining. */
  if (interval > 0)
    interval = (time_limit - (OS_real_time_clock ()));
  if (interval <= 0)
    PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (interval));
}

static void
xterm_process_event (event)
     XEvent * event;
{
  struct xterm * ext;

  ext = (xterm_window_to_xt ((event -> xany) . window));
  switch (event -> type)
    {
    case ConfigureNotify:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: ConfigureNotify\n");
      if (ext != ((struct xterm *) 0))
	{
	  XFontStruct * font = (ext -> font);
	  int extra = (2 * (ext -> internal_border_width));
	  int x_size =
	    ((((event -> xconfigure) . width) - extra) / (FONT_WIDTH (font)));
	  int y_size =
	    ((((event -> xconfigure) . height) - extra) / (FONT_HEIGHT (font)));
	  if ((x_size != (ext -> x_size)) || (y_size != (ext -> y_size)))
	    {
	      int map_size = (x_size * y_size);
	      (ext -> x_size) = x_size;
	      (ext -> y_size) = y_size;
	      (ext -> event_flags) |= EVENT_FLAG_RESIZED;
	      free (ext -> character_map);
	      free (ext -> highlight_map);
	      MAKE_MAP ((ext -> character_map), map_size, ' ');
	      MAKE_MAP ((ext -> highlight_map), map_size, 0);
	      xterm_wm_set_size_hint (ext, 0, 0, 0);
	    }
	}
      break;

    case MapNotify:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: MapNotify\n");
      (ext -> visible_p) = 1;
      break;

    case UnmapNotify:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: UnmapNotify\n");
      if (ext != ((struct xterm *) 0))
	(ext -> visible_p) = 0;
      break;

    case Expose:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: Expose\n");
      if (ext != ((struct xterm *) 0))
	xterm_dump_rectangle (ext,
			      ((event -> xexpose) . x),
			      ((event -> xexpose) . y),
			      ((event -> xexpose) . width),
			      ((event -> xexpose) . height));
      break;

    case GraphicsExpose:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: GraphicsExpose\n");
      if (ext != ((struct xterm *) 0))
	xterm_dump_rectangle (ext,
			      ((event -> xgraphicsexpose) . x),
			      ((event -> xgraphicsexpose) . y),
			      ((event -> xgraphicsexpose) . width),
			      ((event -> xgraphicsexpose) . height));
      break;

    case NoExpose:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: NoExpose\n");
      break;

    case EnterNotify:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: EnterNotify\n");
      break;

    case LeaveNotify:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: LeaveNotify\n");
      break;

    case FocusIn:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: FocusIn\n");
      break;

    case FocusOut:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: FocusOut\n");
      break;

    case MotionNotify:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: MotionNotify\n");
      break;

    case ButtonPress:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: ButtonPress\n");
      break;

    case ButtonRelease:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: ButtonRelease\n");
      break;

    default:
      if (xterm_debug) fprintf (stderr, "\nXTerm event: %d", (event -> type));
      break;
    }
  return;
}
