/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11base.c,v 1.9 1990/07/22 06:35:42 jinx Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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
#include "ux.h"
#include "x11.h"

char *
x_malloc (size)
     int size;
{
  char * result;

  result = (UX_malloc (size));
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

  result = (UX_realloc (ptr, size));
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
  fast SCHEME_OBJECT object = (ARG_REF (arg));				\
									\
  if (! (FIXNUM_P (object)))						\
    error_wrong_type_arg (arg);						\
  if (! (FIXNUM_NEGATIVE_P (object)))					\
    {									\
      fast int length = (table -> length);				\
      fast char ** items = (table -> items);				\
      fast int index = (UNSIGNED_FIXNUM_TO_LONG (object));		\
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
x_make_window (display, window, x_size, y_size, attributes, extra, deallocator, event_proc)
     Display * display;
     Window window;
     int x_size;
     int y_size;
     struct drawing_attributes * attributes;
     int extra;
     void (* deallocator) ();
     void (* event_proc) ();
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
  (xw -> event_proc) = event_proc;
  return (xw);
}

SCHEME_OBJECT
x_window_to_object (xw)
     struct xwindow * xw;
{
  return
    (LONG_TO_UNSIGNED_FIXNUM
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

int
x_window_to_xw_index (window)
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
	return (i);
    }
  return (-1);
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

static struct event_queue global_x_event_queue;

Boolean
x_process_events()
{
  Display ** displays;
  Display * display;
  int length;
  int i;
  Boolean any_events_p = false;

  displays = ((Display **) (x_display_table . items));
  length = (x_display_table . length);
  for (i = 0; (i < length); ++i) {
    if ((display = displays [i]) != ((Display *) 0)) {
      any_events_p = x_distribute_events (display) || any_events_p;
    }
  }
  return (any_events_p);
}

static void
x_enqueue_event (events, event)
     struct event_queue * events;
     XEvent * event;
{
  struct event_queue_element * element;
  struct event_queue_element * global_element;

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

  global_element =
    ((struct event_queue_element *)
     (x_malloc (sizeof (struct event_queue_element))));
  (global_element -> event) = (* event);
  (global_element -> next) = ((struct event_queue_element *) 0);
  if ((global_x_event_queue . head) == ((struct event_queue_element *) 0))
    (global_x_event_queue . head) = global_element;
  else
    ((global_x_event_queue . tail) -> next) = global_element;
  (global_x_event_queue . tail) = global_element;

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

int 
x_dequeue_global_event (event)
     XEvent * event;
{
  (void) x_process_events();
  if (x_dequeue_event ((& global_x_event_queue), event)) {
    return (1);
  }
  return (x_dequeue_event ((& global_x_event_queue), event));
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
  (void) x_distribute_events (XW_DISPLAY (xw));
  return (x_dequeue_event ((& (xw -> events)), event));
}

Boolean
x_distribute_events (display)
     Display * display;
{
  int nevents;
  XEvent * event;
  struct xwindow * exw;
  Boolean any_events_p;

  nevents = (XEventsQueued (display, QueuedAfterReading));
  any_events_p = (nevents ? true : false);
  while (nevents > 0)
    {
      event = (XEvent *) (x_malloc (sizeof (XEvent)));
      XNextEvent (display, (event));
      nevents -= 1;

      exw = (x_window_to_xw ((event -> xany) . window));
      if (exw == ((struct xwindow *) 0))
	continue;
      (exw->event_proc)(exw, (event));
      xw_enqueue_event (exw, (event));
    }
  return (any_events_p);
}

void
xw_wait_for_window_event (xw)
     struct xwindow * xw;
{
  Display * display = (XW_DISPLAY (xw));
  struct xwindow * exw;
  XEvent event_s;
  XEvent * event;

  event &event_s;

  while (1)
    {
      XNextEvent (display, event);

      exw = (x_window_to_xw ((event -> xany) . window));
      if (exw != ((struct xwindow *) 0)) {
	(exw->event_proc)(exw, event);
	xw_enqueue_event (exw, event);
	if (exw == xw)
	  {
	    (void) x_distribute_events (display);
	    break;
	  }
      }
    }
  return;
}

static int * x_select_mask;
static int x_select_mask_size;
static int x_max_file_descriptor;

int
copy_x_select_mask (mask)
     int ** mask;
{
  int i;
  
  *mask = (int *) x_malloc (x_select_mask_size * sizeof (int));
  for (i = 0; i < x_select_mask_size; i++) {
    (*mask) [i] = (x_select_mask) [i];
  }
  return (x_max_file_descriptor);
}

/* Note that because of the conditional use of select here we can't
   depend on x_wait_for_event() actually waiting for an event. The
   return value will tell you if an event actually was processed */

Boolean
x_wait_for_event ()
{
  int * select_mask;
  int max_filedesc;
  Boolean any_events_p;

  any_events_p = x_process_events ();

#ifdef HAVE_SELECT
  if (! any_events_p) {
    max_filedesc = copy_x_select_mask(&select_mask);
    UX_select ((1 +max_filedesc), select_mask, 0, 0, 0);
    any_events_p = x_process_events ();
  }
#endif  /* HAVE_SELECT */

  return (any_events_p);
}

#define MAKE_EVENT(event_type, window_index, extra)			\
(cons (LONG_TO_UNSIGNED_FIXNUM (event_type),				\
       cons (((window_index < 0) ?					\
	      SHARP_F :							\
	      LONG_TO_UNSIGNED_FIXNUM (window_index)),			\
	     extra)))

int
check_button (button)
     int button;
{
  switch (button)
    {
    case Button1: return (0);
    case Button2: return (1);
    case Button3: return (2);
    case Button4: return (3);
    case Button5: return (4);
    default: return (-1);
    }
}

static SCHEME_OBJECT
x_event_to_scheme_event (event)
     XEvent * event;
{
  struct xwindow * exw;
  int xw_index;

  xw_index = x_window_to_xw_index ((event -> xany) . window);
  exw = (struct xwindow *) (x_window_table . items) [xw_index];
  switch (event -> type) {

  case ConfigureNotify:
    return (MAKE_EVENT (EVENT_TYPE_CONFIGURE, xw_index, EMPTY_LIST));
    break;

  case MapNotify:
    return (MAKE_EVENT (EVENT_TYPE_MAP, xw_index, EMPTY_LIST));
    break;

  case UnmapNotify:
    return (MAKE_EVENT (EVENT_TYPE_UNMAP, xw_index, EMPTY_LIST));
    break;

  case Expose:
    return (MAKE_EVENT (EVENT_TYPE_EXPOSE, xw_index, EMPTY_LIST));
    break;

  case GraphicsExpose:
    return (MAKE_EVENT (EVENT_TYPE_GRAPHICS_EXPOSE, xw_index, EMPTY_LIST));
    break;

  case KeyPress:
    {
      char copy_buffer [80];
      KeySym keysym;

      XLookupString ((& (event -> xkey)),
		     (& (copy_buffer [0])),
		     (sizeof (copy_buffer)),
		     (& keysym),
		     ((XComposeStatus *) 0));
      return (MAKE_EVENT (EVENT_TYPE_KEY_PRESS,
			  xw_index,
			  cons(char_pointer_to_string (& (copy_buffer [0])),
			       EMPTY_LIST)));
      break;
    }

  case ButtonPress:
    {
      int button = (check_button ((event -> xbutton) . button));
      int pointer_x = ((event -> xbutton) . x);
      int pointer_y = ((event -> xbutton) . y);
      return
	(MAKE_EVENT (EVENT_TYPE_BUTTON_DOWN,
		     xw_index,
		     cons (long_to_integer (button),
			   cons (LONG_TO_UNSIGNED_FIXNUM (pointer_x),
				 cons (LONG_TO_UNSIGNED_FIXNUM (pointer_y),
				       EMPTY_LIST)))));
    }
    break;

  case ButtonRelease:
    {
      int button = (check_button ((event -> xbutton) . button));
      int pointer_x = ((event -> xbutton) . x);
      int pointer_y = ((event -> xbutton) . y);
      return
	(MAKE_EVENT (EVENT_TYPE_BUTTON_UP,
		     xw_index,
		     cons (long_to_integer (button),
			   cons (LONG_TO_UNSIGNED_FIXNUM (pointer_x),
				 cons (LONG_TO_UNSIGNED_FIXNUM (pointer_y),
				       EMPTY_LIST)))));
    }
    break;

  case NoExpose:
    return (MAKE_EVENT (EVENT_TYPE_NO_EXPOSE, xw_index, EMPTY_LIST));
    break;

  case EnterNotify:
    return (MAKE_EVENT (EVENT_TYPE_ENTER, xw_index, EMPTY_LIST));
    break;

  case LeaveNotify:
    return (MAKE_EVENT (EVENT_TYPE_LEAVE, xw_index, EMPTY_LIST));
    break;

  case FocusIn:
    return (MAKE_EVENT (EVENT_TYPE_FOCUS_IN, xw_index, EMPTY_LIST));
    break;

  case FocusOut:
    return (MAKE_EVENT (EVENT_TYPE_FOCUS_OUT, xw_index, EMPTY_LIST));
    break;

  case MotionNotify:
    {
      int pointer_x = ((event -> xbutton) . x);
      int pointer_y = ((event -> xbutton) . y);
      return
	(MAKE_EVENT (EVENT_TYPE_MOTION,
		     xw_index,
		     cons (LONG_TO_UNSIGNED_FIXNUM (pointer_x),
			   cons (LONG_TO_UNSIGNED_FIXNUM (pointer_y),
				 EMPTY_LIST))));
    }
    return (MAKE_EVENT (EVENT_TYPE_MOTION, xw_index, EMPTY_LIST));
    break;

  default:
    return (MAKE_EVENT (EVENT_TYPE_UNKNOWN, xw_index, EMPTY_LIST));
    break;
  }
}

DEFINE_PRIMITIVE ("X-PROCESS-EVENTS", Prim_x_process_events, 0, 0,
"Process any pending X events. Does not wait.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (x_process_events ()));
}

/* X-WAIT-FOR-EVENT-ON-WINDOW should be supplemented to accept a 
   time out argument */

DEFINE_PRIMITIVE ("X-WAIT-FOR-EVENT-ON-WINDOW",
		  Prim_x_wait_for_event_on_window, 1, 1,
"Wait for an X event for the X-WINDOW-INDEX argument.")
{
  PRIMITIVE_HEADER (1);
  xw_wait_for_window_event (WINDOW_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* X-WAIT-FOR-EVENT should be supplemented to accept a time out argument */

DEFINE_PRIMITIVE ("X-WAIT-FOR-EVENT", Prim_x_wait_for_event, 0, 0, 
"Wait for an X event. It is possible that this procedure will return\n\
even though there there are no X events. The return value will tell\n\
you if there were actually any events processed.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (x_wait_for_event ()));
}

DEFINE_PRIMITIVE ("X-WINDOW-READ-EVENT-FLAGS!", Prim_x_window_read_event_flags, 1, 1, 0)
{
  struct xwindow * xw;
  int old;
  PRIMITIVE_HEADER (1);

  xw = (WINDOW_ARG (1));
  old = (XW_EVENT_FLAGS (xw));
  (XW_EVENT_FLAGS (xw)) = 0;
  PRIMITIVE_RETURN (long_to_integer (old));
}

DEFINE_PRIMITIVE ("X-DEQUEUE-GLOBAL-EVENT", Prim_x_dequeue_global_event, 0, 0,
"Returns an list representing a single event from the global X event queue.\n\
The list is of the form (EVENT-TYPE X-WINDOW-INDEX . EXTRA) where EXTRA is\n\
dependent on the EVENT-TYPE.")
{
  XEvent event;
  int any_events;
  PRIMITIVE_HEADER (0);

  any_events = x_dequeue_global_event (& event);
  if (!any_events) {
    PRIMITIVE_RETURN (EMPTY_LIST);
  }
  PRIMITIVE_RETURN (x_event_to_scheme_event (& event));
}
  
DEFINE_PRIMITIVE ("X-RETURN-EVENT-QUEUE", Prim_x_return_event_queue, 0, 0,
  "Returns an list of all events (in order) from the global X event queue \n\
and flushes the queue. Each event on the list is of the form \n\
(EVENT-TYPE X-WINDOW-INDEX . EXTRA) where EXTRA is dependent on the \n\
EVENT-TYPE.")
{
  XEvent event;
  int any_events;
  SCHEME_OBJECT event_list;
  SCHEME_OBJECT event_list_tail;
  SCHEME_OBJECT new_event;
  PRIMITIVE_HEADER (0);

  any_events = x_dequeue_global_event (& event);
  if (!any_events) {
    return (EMPTY_LIST);
  }
  event_list = cons (x_event_to_scheme_event (& event), EMPTY_LIST);
  event_list_tail = event_list;
  while (any_events = x_dequeue_global_event (& event)) {
    new_event = cons (x_event_to_scheme_event (& event), EMPTY_LIST);
    SET_PAIR_CDR (event_list_tail, new_event);
    event_list_tail = new_event;
  }
  PRIMITIVE_RETURN (event_list);
}
  

DEFINE_PRIMITIVE ("X-OPEN-DISPLAY", Prim_x_open_display, 1, 1, 0)
{
  Display * display;
  int display_file_descriptor;
  PRIMITIVE_HEADER (1);

  display =
    (XOpenDisplay (((ARG_REF (1)) == SHARP_F) ? NULL : (STRING_ARG (1))));
  if (display == NULL)
    PRIMITIVE_RETURN (SHARP_F);

  /* This only needs to be done once for this process, but it doesn't
     hurt to run it every time we open the display. */
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_handler);

  display_file_descriptor = ConnectionNumber (display);

  if (! x_select_mask_size) {
    x_select_mask_size = 1;
    x_select_mask = (int *) x_malloc (x_select_mask_size * sizeof (int));
  }
      

  if (display_file_descriptor > x_max_file_descriptor) {

    int new_select_mask_size;
    
    x_max_file_descriptor = display_file_descriptor;
    new_select_mask_size = 1 + (x_max_file_descriptor / BITS_PER_INT);
    if (new_select_mask_size > x_select_mask_size) {
      x_select_mask = (int *) x_realloc (x_select_mask,
					  new_select_mask_size * sizeof (int));
      x_select_mask_size = new_select_mask_size;
    }
    SET_X_SELECT_MASK (display_file_descriptor);
  }
      
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM
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
  PRIMITIVE_RETURN (long_to_integer (XW_X_SIZE (WINDOW_ARG (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-Y-SIZE", Prim_x_window_y_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_Y_SIZE (WINDOW_ARG (1))));
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
     : (char_pointer_to_string (result)));
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
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (arg_integer (2)),
     (arg_integer (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-PIXEL-COORD->CHAR-COORD",
		  Prim_window_pixel_coord_to_char_coord,
		  2,
		  2,
"Takes an X window and a pair (cons) of x and y pixel coordinates \n\
and returns a pair of x and y character coordinates appropriate \n\
for the current font associated with that window.")
{
  struct xwindow * xw;
  SCHEME_OBJECT coord_list;
  SCHEME_OBJECT x_coord;
  SCHEME_OBJECT y_coord;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  coord_list = (PAIR_ARG (2));
  x_coord = (PAIR_CAR (coord_list));
  y_coord = (PAIR_CDR (coord_list));
  if (!((INTEGER_P (x_coord)) && (INTEGER_P (y_coord)))) {
    error_wrong_type_arg (2);
  }
  PRIMITIVE_RETURN
    (cons (long_to_integer (XTERM_X_CHARACTER (xw, integer_to_long (x_coord))),
	   long_to_integer (XTERM_Y_CHARACTER (xw, integer_to_long (y_coord)))));
}

DEFINE_PRIMITIVE ("X-WINDOW-CHAR-COORD->PIXEL-COORD",
		  Prim_window_char_coord_to_pixel_coord,
		  2,
		  2,
"Takes an X window and a pair (cons) of x and y character coordinates \n\
and returns a pair of x and y pixel coordinates appropriate \n\
for the current font associated with that window.")
{
  struct xwindow * xw;
  SCHEME_OBJECT coord_list;
  SCHEME_OBJECT x_coord;
  SCHEME_OBJECT y_coord;
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  coord_list = (PAIR_ARG (2));
  x_coord = (PAIR_CAR (coord_list));
  y_coord = (PAIR_CDR (coord_list));
  if (!((INTEGER_P (x_coord)) && (INTEGER_P (y_coord)))) {
    error_wrong_type_arg (2);
  }
  PRIMITIVE_RETURN
    (cons (long_to_integer (XTERM_X_PIXEL (xw, integer_to_long (x_coord))),
	   long_to_integer (XTERM_Y_PIXEL (xw, integer_to_long (y_coord)))));
}

DEFINE_PRIMITIVE ("X-SET-WINDOW-NAME", Prim_x_set_window_name, 2, 2,
"Set the window name.")
{
  struct xwindow * xw;

  PRIMITIVE_HEADER (2);
  xw = WINDOW_ARG (1);
  XStoreName (XW_DISPLAY (xw), XW_WINDOW (xw), STRING_ARG (2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-SET-ICON-NAME", Prim_x_set_icon_name, 2, 2,
"Set the window icon name.")
{
  struct xwindow * xw;

  PRIMITIVE_HEADER (2);
  xw = WINDOW_ARG (1);
  XSetIconName (XW_DISPLAY (xw), XW_WINDOW (xw), STRING_ARG (2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
