/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11base.c,v 1.6 1989/07/26 04:14:06 cph Rel $

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

/* Common X11 support. */

#include "scheme.h"
#include "prims.h"
#include "x11.h"

char *
x_malloc (size)
     int size;
{
  char * result;
  extern char * malloc ();

  result = (malloc (size));
  if (result == ((char *) 0))
    error_external_return ();
  return (result);
}

char *
x_realloc (ptr, size)
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

int
x_allocate_table_index (table, item)
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
	((char **) (x_malloc ((sizeof (char *)) * new_length)));
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
      ((char **) (x_realloc (items, ((sizeof (char *)) * new_length))));
    (new_items [length]) = item;
    for (i = (length + 1); (i < new_length); i += 1)
      (new_items [i]) = ((char *) 0);
    (table -> items) = new_items;
    (table -> length) = new_length;
  }
  return (length);
}

#define DEF_ALLOCATION_ARG(name, result_type, result)			\
result_type								\
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

DEF_ALLOCATION_ARG (x_allocation_item_arg, char *, (items [index]))
DEF_ALLOCATION_ARG (x_allocation_index_arg, int, index)

struct allocation_table x_display_table;
struct allocation_table x_window_table;

int x_debug = 0;

DEFINE_PRIMITIVE ("X-DEBUG", Prim_x_debug, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  x_debug = ((ARG_REF (1)) != SHARP_F);
  PRIMITIVE_RETURN (UNSPECIFIC);
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
		 buffer, (sizeof (buffer)));
  fprintf (stderr, "\nX Error: %s\n", buffer);
  fprintf (stderr, "         Request code: %d\n",
	   (error_event -> request_code));
  fprintf (stderr, "         Error serial: %x\n", (error_event -> serial));
  error_external_return ();
}

unsigned long
x_decode_color (display, color_map, color_name, default_color)
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

char *
x_get_default (display, resource_name, property_name, class_name, sdefault)
     Display * display;
     char * resource_name;
     char * property_name;
     char * class_name;
     char * sdefault;
{
  char * result;

  result = (XGetDefault (display, resource_name, property_name));
  if (result != ((char *) 0))
    return (result);
  result = (XGetDefault (display, resource_name, class_name));
  if (result != ((char *) 0))
    return (result);
  return (sdefault);
}

unsigned long
x_default_color (display, resource_name, property_name, class_name,
		 default_color)
     Display * display;
     char * resource_name;
     char * property_name;
     char * class_name;
     unsigned long default_color;
{
  char * color_name =
    (x_get_default
     (display, resource_name, property_name, class_name, ((char *) 0)));
  if (color_name == ((char *) 0))
    return (default_color);
  return
    (x_decode_color
     (display,
      (DefaultColormap (display, (DefaultScreen (display)))),
      color_name,
      default_color));
}

void
x_set_mouse_colors (display, mouse_cursor, mouse_pixel, background_pixel)
     Display * display;
     Cursor mouse_cursor;
     unsigned long mouse_pixel;
     unsigned long background_pixel;
{
  Colormap color_map = (DefaultColormap (display, (DefaultScreen (display))));
  XColor mouse_color;
  XColor background_color;

  (mouse_color . pixel) = mouse_pixel;
  XQueryColor (display, color_map, (& mouse_color));
  (background_color . pixel) = background_pixel;
  XQueryColor (display, color_map, (& background_color));
  XRecolorCursor
    (display, mouse_cursor, (& mouse_color), (& background_color));
  return;
}

void
x_default_attributes (display, resource_name, attributes)
     Display * display;
     char * resource_name;
     struct drawing_attributes * attributes;
{
  int screen_number = (DefaultScreen (display));

  (attributes -> font) =
    (XLoadQueryFont
     (display,
      (x_get_default
       (display, resource_name, "font", "Font", "9x15"))));
  if ((attributes -> font) == ((XFontStruct *) 0))
    error_external_return ();
  {
    char * s =
      (x_get_default
       (display, resource_name, "borderWidth", "BorderWidth", ((char *) 0)));
    (attributes -> border_width) = ((s == ((char *) 0)) ? 2 : (atoi (s)));
  }
  {
    char * s =
      (x_get_default
       (display, resource_name,
	"internalBorder", "BorderWidth", ((char *) 0)));
    (attributes -> internal_border_width) =
      ((s == ((char *) 0)) ? (attributes -> border_width) : (atoi (s)));
  }
  {
    unsigned long white_pixel = (WhitePixel (display, screen_number));
    unsigned long black_pixel = (BlackPixel (display, screen_number));
    unsigned long foreground_pixel;

    (attributes -> background_pixel) =
      (x_default_color
       (display, resource_name, "background", "Background", white_pixel));
    foreground_pixel =
      (x_default_color
       (display, resource_name, "foreground", "Foreground", black_pixel));
    (attributes -> foreground_pixel) = foreground_pixel;
    (attributes -> border_pixel) =
      (x_default_color
       (display, resource_name,
	"borderColor", "BorderColor", foreground_pixel));
    (attributes -> cursor_pixel) =
      (x_default_color
       (display, resource_name,
	"cursorColor", "Foreground", foreground_pixel));
    (attributes -> mouse_pixel) =
      (x_default_color
       (display, resource_name,
	"pointerColor", "Foreground", foreground_pixel));
  }
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

struct xwindow *
x_make_window (display, window, x_size, y_size, attributes, extra, deallocator)
     Display * display;
     Window window;
     int x_size;
     int y_size;
     struct drawing_attributes * attributes;
     int extra;
     void (* deallocator) ();
{
  GC normal_gc;
  GC reverse_gc;
  GC cursor_gc;
  struct xwindow * xw;
  Font fid = ((attributes -> font) -> fid);
  unsigned long foreground_pixel = (attributes -> foreground_pixel);
  unsigned long background_pixel = (attributes -> background_pixel);
  Cursor mouse_cursor = (XCreateFontCursor (display, XC_left_ptr));

  MAKE_GC (normal_gc, foreground_pixel, background_pixel);
  MAKE_GC (reverse_gc, background_pixel, foreground_pixel);
  MAKE_GC (cursor_gc, background_pixel, (attributes -> cursor_pixel));
  x_set_mouse_colors
    (display, mouse_cursor, (attributes -> mouse_pixel), background_pixel);
  XDefineCursor (display, window, mouse_cursor);

  xw = ((struct xwindow *) (x_malloc (sizeof (struct xwindow))));
  (XW_DISPLAY (xw)) = display;
  (XW_WINDOW (xw)) = window;
  (XW_X_SIZE (xw)) = x_size;
  (XW_Y_SIZE (xw)) = y_size;
  (xw -> attributes) = (* attributes);
  (XW_NORMAL_GC (xw)) = normal_gc;
  (XW_REVERSE_GC (xw)) = reverse_gc;
  (XW_CURSOR_GC (xw)) = cursor_gc;
  (XW_MOUSE_CURSOR (xw)) = mouse_cursor;
  ((xw -> events) . head) = ((struct event_queue_element *) 0);
  ((xw -> events) . tail) = ((struct event_queue_element *) 0);
  (XW_EVENT_FLAGS (xw)) = 0;
  (XW_VISIBLE_P (xw)) = 0;

  if (extra > 0)
    (xw -> extra) = ((char *) (x_malloc (extra)));
  (xw -> deallocator) = deallocator;
  return (xw);
}

Pointer
x_window_to_object (xw)
     struct xwindow * xw;
{
  return
    (MAKE_UNSIGNED_FIXNUM
     (x_allocate_table_index ((& x_window_table), ((char *) xw))));
}

struct xwindow *
x_window_to_xw (window)
     Window window;
{
  int length = (x_window_table . length);
  struct xwindow ** items = ((struct xwindow **) (x_window_table . items));
  int i;
  struct xwindow * xw;

  for (i = 0; (i < length); i += 1)
    {
      xw = (items [i]);
      if ((XW_WINDOW (xw)) == window)
	return (xw);
    }
  return ((struct xwindow *) 0);
}

Display *
x_close_window (index)
     int index;
{
  struct xwindow * xw;
  Display * display;

  xw = ((struct xwindow *) ((x_window_table . items) [index]));
  ((x_window_table . items) [index]) = 0;
  display = (XW_DISPLAY (xw));
  {
    void (* deallocator) () = (xw -> deallocator);
    if (deallocator != ((void (*) ()) 0))
      (* deallocator) (xw);
  }
  {
    XFontStruct * font = (XW_FONT (xw));
    if (font != ((XFontStruct *) 0))
      XFreeFont (display, font);
  }
  XDestroyWindow (display, (XW_WINDOW (xw)));
  free (xw);
  return (display);
}

void
x_close_display (index)
     int index;
{
  Display * display;

  display = ((Display *) ((x_display_table . items) [index]));
  ((x_display_table . items) [index]) = 0;
  {
    struct xwindow ** items = ((struct xwindow **) (x_window_table . items));
    int length = (x_window_table . length);
    int i;

    for (i = 0; (i < length); i += 1)
      {
	struct xwindow * xw = (items [i]);
	if ((xw != ((struct xwindow *) 0)) &&
	    ((XW_DISPLAY (xw)) == display))
	  (void) x_close_window (i);
      }
  }
  XCloseDisplay (display);
  return;
}

static void
x_enqueue_event (events, event)
     struct event_queue * events;
     XEvent * event;
{
  struct event_queue_element * element;

  element =
    ((struct event_queue_element *)
     (x_malloc (sizeof (struct event_queue_element))));
  (element -> event) = (* event);
  (element -> next) = ((struct event_queue_element *) 0);
  if ((events -> head) == ((struct event_queue_element *) 0))
    (events -> head) = element;
  else
    ((events -> tail) -> next) = element;
  (events -> tail) = element;
  return;
}

static int
x_dequeue_event (events, event)
     struct event_queue * events;
     XEvent * event;
{
  struct event_queue_element * element;

  element = (events -> head);
  if (element == ((struct event_queue_element *) 0))
    return (0);
  (* event) = (element -> event);
  (events -> head) = (element -> next);
  free (element);
  return (1);
}

void
xw_enqueue_event (xw, event)
     struct xwindow * xw;
     XEvent * event;
{
  x_enqueue_event ((& (xw -> events)), event);
  return;
}

int
xw_dequeue_event (xw, event)
     struct xwindow * xw;
     XEvent * event;
{
  if (x_dequeue_event ((& (xw -> events)), event))
    return (1);
  x_distribute_events (XW_DISPLAY (xw));
  return (x_dequeue_event ((& (xw -> events)), event));
}

void
x_distribute_events (display)
     Display * display;
{
  int nevents;
  XEvent event;
  struct xwindow * exw;

  nevents = (XEventsQueued (display, QueuedAfterReading));
  while (nevents > 0)
    {
      XNextEvent (display, (& event));
      nevents -= 1;

      exw = (x_window_to_xw ((event . xany) . window));
      if (exw == ((struct xwindow *) 0))
	continue;
      xw_enqueue_event (exw, (& event));
    }
  return;
}

void
xw_wait_for_window_event (xw, event)
     struct xwindow * xw;
     XEvent * event;
{
  Display * display = (XW_DISPLAY (xw));
  Window window = (XW_WINDOW (xw));
  struct xwindow * exw;

  while (1)
    {
      XNextEvent (display, event);

      exw = (x_window_to_xw ((event -> xany) . window));
      if (exw == xw)
	{
	  x_distribute_events (display);
	  break;
	}
      if (exw != ((struct xwindow *) 0))
	xw_enqueue_event (exw, event);
    }
  return;
}

DEFINE_PRIMITIVE ("X-WINDOW-READ-EVENT-FLAGS!", Prim_x_window_read_event_flags, 1, 1, 0)
{
  struct xwindow * xw;
  int old;
  PRIMITIVE_HEADER (1);

  xw = (WINDOW_ARG (1));
  old = (XW_EVENT_FLAGS (xw));
  (XW_EVENT_FLAGS (xw)) = 0;
  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (old));
}

DEFINE_PRIMITIVE ("X-OPEN-DISPLAY", Prim_x_open_display, 1, 1, 0)
{
  Display * display;
  int index;
  PRIMITIVE_HEADER (1);

  display =
    (XOpenDisplay (((ARG_REF (1)) == SHARP_F) ? NULL : (STRING_ARG (1))));
  if (display == NULL)
    PRIMITIVE_RETURN (SHARP_F);

  /* This only needs to be done once for this process, but it doesn't
     hurt to run it every time we open the display. */
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_handler);

  PRIMITIVE_RETURN
    (MAKE_UNSIGNED_FIXNUM
     (x_allocate_table_index ((& x_display_table), ((char *) display))));
}

DEFINE_PRIMITIVE ("X-CLOSE-DISPLAY", Prim_x_close_display, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  x_close_display (x_allocation_index_arg (1, (& x_display_table)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-CLOSE-ALL-DISPLAYS", Prim_x_close_all_displays, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  
  {
    Display ** items = ((Display **) (x_display_table . items));
    int length = (x_display_table . length);
    int i;

    for (i = 0; (i < length); i += 1)
      if ((items [i]) != ((Display *) 0))
	x_close_display (i);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-CLOSE-WINDOW", Prim_x_close_window, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  XFlush (x_close_window (x_allocation_index_arg (1, (& x_window_table))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-X-SIZE", Prim_x_window_x_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (XW_X_SIZE (WINDOW_ARG (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-Y-SIZE", Prim_x_window_y_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (XW_Y_SIZE (WINDOW_ARG (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-MAP", Prim_x_window_map, 1, 1, 0)
{
  struct xwindow * xw;
  Display * display;
  PRIMITIVE_HEADER (1);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  (XW_VISIBLE_P (xw)) = 1;
  XMapWindow (display, (XW_WINDOW (xw)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-UNMAP", Prim_x_window_unmap, 1, 1, 0)
{
  struct xwindow * xw;
  Display * display;
  PRIMITIVE_HEADER (1);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  (XW_VISIBLE_P (xw)) = 0;
  XUnmapWindow (display, (XW_WINDOW (xw)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-BEEP", Prim_x_window_beep, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  XBell ((XW_DISPLAY (WINDOW_ARG (1))), 100); /* 100% */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-CLEAR", Prim_x_window_clear, 1, 1, 0)
{
  struct xwindow * xw;
  PRIMITIVE_HEADER (1);

  xw = (WINDOW_ARG (1));
  XClearWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-FLUSH", Prim_x_window_flush, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  XFlush (XW_DISPLAY (WINDOW_ARG (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-GET-DEFAULT", Prim_x_window_get_default, 3, 3, 0)
{
  char * result;
  PRIMITIVE_HEADER (3);

  result =
    (XGetDefault
     ((XW_DISPLAY (WINDOW_ARG (1))), (STRING_ARG (2)), (STRING_ARG (3))));
  PRIMITIVE_RETURN
    ((result == ((char *) 0))
     ? SHARP_F
     : (C_String_To_Scheme_String (result)));
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-FOREGROUND-COLOR", Prim_x_window_set_foreground_color, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  unsigned long foreground_pixel;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  foreground_pixel =
    (x_decode_color
     (display,
      (DefaultColormap (display, (DefaultScreen (display)))),
      (STRING_ARG (2)),
      (XW_FOREGROUND_PIXEL (xw))));
  (XW_FOREGROUND_PIXEL (xw)) = foreground_pixel;
  XSetForeground (display, (XW_NORMAL_GC (xw)), foreground_pixel);
  XSetBackground (display, (XW_REVERSE_GC (xw)), foreground_pixel);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-BACKGROUND-COLOR", Prim_x_window_set_background_color, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  unsigned long background_pixel;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  background_pixel =
    (x_decode_color
     (display,
      (DefaultColormap (display, (DefaultScreen (display)))),
      (STRING_ARG (2)),
      (XW_BACKGROUND_PIXEL (xw))));
  (XW_BACKGROUND_PIXEL (xw)) = background_pixel;
  XSetWindowBackground (display, (XW_WINDOW (xw)), background_pixel);
  XSetBackground (display, (XW_NORMAL_GC (xw)), background_pixel);
  XSetForeground (display, (XW_REVERSE_GC (xw)), background_pixel);
  XSetForeground (display, (XW_CURSOR_GC (xw)), background_pixel);
  x_set_mouse_colors
    (display, (XW_MOUSE_CURSOR (xw)), (XW_MOUSE_PIXEL (xw)), background_pixel);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-BORDER-COLOR", Prim_x_window_set_border_color, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  unsigned long border_pixel;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  border_pixel =
    (x_decode_color
     (display,
      (DefaultColormap (display, (DefaultScreen (display)))),
      (STRING_ARG (2)),
      (XW_BORDER_PIXEL (xw))));
  (XW_BORDER_PIXEL (xw)) = border_pixel;
  XSetWindowBorder (display, (XW_WINDOW (xw)), border_pixel);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-CURSOR-COLOR", Prim_x_window_set_cursor_color, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  unsigned long cursor_pixel;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  cursor_pixel =
    (x_decode_color
     (display,
      (DefaultColormap (display, (DefaultScreen (display)))),
      (STRING_ARG (2)),
      (XW_CURSOR_PIXEL (xw))));
  (XW_CURSOR_PIXEL (xw)) = cursor_pixel;
  XSetBackground (display, (XW_CURSOR_GC (xw)), cursor_pixel);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-MOUSE-COLOR", Prim_x_window_set_mouse_color, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  unsigned long mouse_pixel;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  mouse_pixel =
    (x_decode_color
     (display,
      (DefaultColormap (display, (DefaultScreen (display)))),
      (STRING_ARG (2)),
      (XW_MOUSE_PIXEL (xw))));
  (XW_MOUSE_PIXEL (xw)) = mouse_pixel;
  x_set_mouse_colors
    (display, (XW_MOUSE_CURSOR (xw)), mouse_pixel, (XW_BACKGROUND_PIXEL (xw)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-MOUSE-SHAPE", Prim_x_window_set_mouse_shape, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  Window window;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  window = (XW_WINDOW (xw));
  {
    Cursor old_cursor = (XW_MOUSE_CURSOR (xw));
    Cursor mouse_cursor =
      (XCreateFontCursor
       (display, (2 * (arg_index_integer (2, (XC_num_glyphs / 2))))));
    x_set_mouse_colors
      (display,
       mouse_cursor,
       (XW_MOUSE_PIXEL (xw)),
       (XW_BACKGROUND_PIXEL (xw)));
    (XW_MOUSE_CURSOR (xw)) = mouse_cursor;
    XDefineCursor (display, window, mouse_cursor);
    XFreeCursor (display, old_cursor);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-FONT", Prim_x_window_set_font, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  XFontStruct * font;
  Font fid;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  font = (XLoadQueryFont (display, (STRING_ARG (2))));
  if (font == ((XFontStruct *) 0))
    PRIMITIVE_RETURN (SHARP_F);
  XFreeFont (display, (XW_FONT (xw)));
  (XW_FONT (xw)) = font;
  fid = (font -> fid);
  XSetFont (display, (XW_NORMAL_GC (xw)), fid);
  XSetFont (display, (XW_REVERSE_GC (xw)), fid);
  XSetFont (display, (XW_CURSOR_GC (xw)), fid);
  PRIMITIVE_RETURN (SHARP_T);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-BORDER-WIDTH", Prim_x_window_set_border_width, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  int border_width;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  border_width = (arg_nonnegative_integer (2));
  (XW_BORDER_WIDTH (xw)) = border_width;
  XSetWindowBorderWidth (display, (XW_WINDOW (xw)), border_width);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-INTERNAL-BORDER-WIDTH", Prim_x_window_set_internal_border_width, 2, 2, 0)
{
  struct xwindow * xw;
  Display * display;
  int internal_border_width;
  int extra;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  internal_border_width = (arg_nonnegative_integer (2));
  (XW_INTERNAL_BORDER_WIDTH (xw)) = internal_border_width;
  extra = (2 * internal_border_width);
  XResizeWindow
    (display,
     (XW_WINDOW (xw)),
     ((XW_X_SIZE (xw)) + extra),
     ((XW_Y_SIZE (xw)) + extra));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-SIZE", Prim_x_window_set_size, 3, 3, 0)
{
  struct xwindow * xw;
  int extra;
  PRIMITIVE_HEADER (3);

  xw = (WINDOW_ARG (1));
  extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  XResizeWindow
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     ((arg_nonnegative_integer (2)) + extra),
     ((arg_nonnegative_integer (3)) + extra));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-POSITION", Prim_x_window_set_position, 3, 3, 0)
{
  struct xwindow * xw;
  Display * display;
  int screen_number;
  PRIMITIVE_HEADER (3);

  xw = (WINDOW_ARG (1));
  display = (XW_DISPLAY (xw));
  screen_number = (DefaultScreen (display));
  XMoveWindow
    ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (arg_fixnum (2)), (arg_fixnum (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
