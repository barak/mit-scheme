/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11base.c,v 1.33 1992/02/09 03:48:11 cph Exp $

Copyright (c) 1989-92 Massachusetts Institute of Technology

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
#include "uxselect.h"
#include "x11.h"

int x_debug = 0;
static int initialization_done = 0;

#define INITIALIZE_ONCE()						\
{									\
  if (!initialization_done)						\
    initialize_once ();							\
}

static void EXFUN (initialize_once, (void));

PTR
DEFUN (x_malloc, (size), unsigned int size)
{
  PTR result = (UX_malloc (size));
  if (result == 0)
    error_external_return ();
  return (result);
}

PTR
DEFUN (x_realloc, (ptr, size), PTR ptr AND unsigned int size)
{
  PTR result = (UX_realloc (ptr, size));
  if (result == 0)
    error_external_return ();
  return (result);
}

struct allocation_table
{
  PTR * items;
  int length;
};

static struct allocation_table x_display_table;
static struct allocation_table x_window_table;
static struct allocation_table x_image_table;
static struct allocation_table x_visual_table;
static struct allocation_table x_colormap_table;

static void
DEFUN (allocation_table_initialize, (table), struct allocation_table * table)
{
  (table -> length) = 0;
}

static unsigned int
DEFUN (allocate_table_index, (table, item),
       struct allocation_table * table AND
       PTR item)
{
  unsigned int length = (table -> length);
  unsigned int new_length;
  PTR * items = (table -> items);
  PTR * new_items;
  PTR * scan;
  PTR * end;
  if (length == 0)
    {
      new_length = 4;
      new_items = (x_malloc ((sizeof (PTR)) * new_length));
    }
  else
    {
      scan = items;
      end = (scan + length);
      while (scan < end)
	if ((*scan++) == 0)
	  {
	    (*--scan) = item;
	    return (scan - items);
	  }
      new_length = (length * 2);
      new_items = (x_realloc (items, ((sizeof (PTR)) * new_length)));
    }
  scan = (new_items + length);
  end = (new_items + new_length);
  (*scan++) = item;
  while (scan < end)
    (*scan++) = 0;
  (table -> items) = new_items;
  (table -> length) = new_length;
  return (length);
}

static PTR
DEFUN (allocation_item_arg, (arg, table),
       unsigned int arg AND
       struct allocation_table * table)
{
  unsigned int index = (arg_index_integer (arg, (table -> length)));
  PTR item = ((table -> items) [index]);
  if (item == 0)
    error_bad_range_arg (arg);
  return (item);
}

struct xdisplay *
DEFUN (x_display_arg, (arg), unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_display_table)));
}

struct xwindow *
DEFUN (x_window_arg, (arg), unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_window_table)));
}

struct ximage *
DEFUN (x_image_arg, (arg), unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_image_table)));
}

struct xvisual *
DEFUN (x_visual_arg, (arg), unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_visual_table)));
}

struct xcolormap *
DEFUN (x_colormap_arg, (arg), unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_colormap_table)));
}

static int
DEFUN (x_io_error_handler, (display), Display * display)
{
  fprintf (stderr, "\nX IO Error\n");
  error_external_return ();
}

static int
DEFUN (x_error_handler, (display, error_event),
       Display * display AND
       XErrorEvent * error_event)
{
  char buffer [2048];
  XGetErrorText (display, (error_event -> error_code),
		 buffer, (sizeof (buffer)));
  fprintf (stderr, "\nX Error: %s\n", buffer);
  fprintf (stderr, "         Request code: %d\n",
	   (error_event -> request_code));
  fprintf (stderr, "         Error serial: %x\n", (error_event -> serial));
  fflush (stderr);
  error_external_return ();
}

static int
DEFUN (x_decode_color, (display, color_map, color_name, color_return),
       Display * display AND
       Colormap color_map AND
       char * color_name AND
       unsigned long * color_return)
{
  XColor cdef;
  if ((XParseColor (display, color_map, color_name, (&cdef)))
      && (XAllocColor (display, color_map, (&cdef))))
    {
      (*color_return) = (cdef . pixel);
      return (1);
    }
  return (0);
}

static unsigned long
DEFUN (arg_color, (arg, display),
       unsigned int arg AND
       Display * display)
{
  unsigned long result;
  if (! (x_decode_color
	 (display,
	  (DefaultColormap (display, (DefaultScreen (display)))),
	  (STRING_ARG (arg)),
	  (&result))))
    error_bad_range_arg (arg);
  return (result);
}

static void
DEFUN (x_set_mouse_colors,
       (display, mouse_cursor, mouse_pixel, background_pixel),
       Display * display AND
       Cursor mouse_cursor AND
       unsigned long mouse_pixel AND
       unsigned long background_pixel)
{
  Colormap color_map = (DefaultColormap (display, (DefaultScreen (display))));
  XColor mouse_color;
  XColor background_color;
  (mouse_color . pixel) = mouse_pixel;
  XQueryColor (display, color_map, (&mouse_color));
  (background_color . pixel) = background_pixel;
  XQueryColor (display, color_map, (&background_color));
  XRecolorCursor (display, mouse_cursor, (&mouse_color), (&background_color));
}

char *
DEFUN (x_get_default,
       (display, resource_name, resource_class,
	property_name, property_class, sdefault),
       Display * display AND
       char * resource_name AND
       char * resource_class AND
       char * property_name AND
       char * property_class AND
       char * sdefault)
{
  char * result = (XGetDefault (display, resource_name, property_name));
  if (result != 0)
    return (result);
  result = (XGetDefault (display, resource_class, property_name));
  if (result != 0)
    return (result);
  result = (XGetDefault (display, resource_name, property_class));
  if (result != 0)
    return (result);
  result = (XGetDefault (display, resource_class, property_class));
  if (result != 0)
    return (result);
  return (sdefault);
}

static unsigned long
DEFUN (x_default_color,
       (display, resource_class, resource_name,
	property_name, property_class, default_color),
       Display * display AND
       char * resource_name AND
       char * resource_class AND
       char * property_name AND
       char * property_class AND
       unsigned long default_color)
{
  char * color_name =
    (x_get_default
     (display, resource_name, resource_class,
      property_name, property_class, 0));
  unsigned long result;
  return
    (((color_name != 0)
      && (x_decode_color
	  (display,
	   (DefaultColormap (display, (DefaultScreen (display)))),
	   color_name,
	   (&result))))
     ? result
     : default_color);
}

void
DEFUN (x_default_attributes, 
       (display, resource_name, resource_class, attributes),
       Display * display AND
       char * resource_name AND
       char * resource_class AND
       struct drawing_attributes * attributes)
{
  int screen_number = (DefaultScreen (display));
  (attributes -> font) =
    (XLoadQueryFont
     (display,
      (x_get_default
       (display, resource_name, resource_class,
	"font", "Font", "9x15"))));
  if ((attributes -> font) == 0)
    error_external_return ();
  {
    char * s =
      (x_get_default
       (display, resource_name, resource_class,
	"borderWidth", "BorderWidth", 0));
    (attributes -> border_width) = ((s == 0) ? 2 : (atoi (s)));
  }
  {
    char * s =
      (x_get_default
       (display, resource_name, resource_class,
	"internalBorder", "BorderWidth", 0));
    (attributes -> internal_border_width) =
      ((s == 0) ? (attributes -> border_width) : (atoi (s)));
  }
  {
    unsigned long white_pixel = (WhitePixel (display, screen_number));
    unsigned long black_pixel = (BlackPixel (display, screen_number));
    unsigned long foreground_pixel;
    (attributes -> background_pixel) =
      (x_default_color
       (display, resource_class, resource_name,
	"background", "Background", white_pixel));
    foreground_pixel =
      (x_default_color
       (display, resource_class, resource_name,
	"foreground", "Foreground", black_pixel));
    (attributes -> foreground_pixel) = foreground_pixel;
    (attributes -> border_pixel) =
      (x_default_color
       (display, resource_class, resource_name,
	"borderColor", "BorderColor", foreground_pixel));
    (attributes -> cursor_pixel) =
      (x_default_color
       (display, resource_class, resource_name,
	"cursorColor", "Foreground", foreground_pixel));
    (attributes -> mouse_pixel) =
      (x_default_color
       (display, resource_class, resource_name,
	"pointerColor", "Foreground", foreground_pixel));
  }
}

#define MAKE_GC(gc, fore, back)						\
{									\
  XGCValues gcv;							\
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
DEFUN (x_make_window, (xd, window, x_size, y_size, attributes, methods, extra),
       struct xdisplay * xd AND
       Window window AND
       int x_size AND
       int y_size AND
       struct drawing_attributes * attributes AND
       struct xwindow_methods * methods AND
       unsigned int extra)
{
  GC normal_gc;
  GC reverse_gc;
  GC cursor_gc;
  struct xwindow * xw;
  Display * display = (XD_DISPLAY (xd));
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
  XSelectInput (display, window, 0);
  xw =
    (x_malloc (((sizeof (struct xwindow)) - (sizeof (xw -> extra))) + extra));
  (XW_ALLOCATION_INDEX (xw)) = (allocate_table_index ((&x_window_table), xw));
  (XW_XD (xw)) = xd;
  (XW_WINDOW (xw)) = window;
  (XW_X_SIZE (xw)) = x_size;
  (XW_Y_SIZE (xw)) = y_size;
  (XW_CLIP_X (xw)) = 0;
  (XW_CLIP_Y (xw)) = 0;
  (XW_CLIP_WIDTH (xw)) = x_size;
  (XW_CLIP_HEIGHT (xw)) = y_size;
  (xw -> attributes) = (*attributes);
  (xw -> methods) = (*methods);
  (XW_NORMAL_GC (xw)) = normal_gc;
  (XW_REVERSE_GC (xw)) = reverse_gc;
  (XW_CURSOR_GC (xw)) = cursor_gc;
  (XW_MOUSE_CURSOR (xw)) = mouse_cursor;
  (XW_EVENT_MASK (xw)) = 0;
  return (xw);
}

static struct xwindow *
DEFUN (x_window_to_xw, (window), Window window)
{
  struct xwindow ** scan = ((struct xwindow **) (x_window_table . items));
  struct xwindow ** end = (scan + (x_window_table . length));
  while (scan < end)
    {
      struct xwindow * xw = (*scan++);
      if ((XW_WINDOW (xw)) == window)
	return (xw);
    }
  return (0);
}
extern void x_destroy_image ();

static void
DEFUN (x_close_window, (xw), struct xwindow * xw)
{
  Display * display = (XW_DISPLAY (xw));
  ((x_window_table . items) [XW_ALLOCATION_INDEX (xw)]) = 0;
  {
    x_deallocator_t deallocator = (XW_DEALLOCATOR (xw));
    if (deallocator != 0)
      (*deallocator) (xw);
  }
  {
    XFontStruct * font = (XW_FONT (xw));
    if (font != 0)
      XFreeFont (display, font);
  }
  XDestroyWindow (display, (XW_WINDOW (xw)));
  free (xw);
}

static void
DEFUN (x_close_display, (xd), struct xdisplay * xd)
{
  struct xwindow ** scan = ((struct xwindow **) (x_window_table . items));
  struct xwindow ** end = (scan + (x_window_table . length));
  while (scan < end)
    {
      struct xwindow * xw = (*scan++);
      if ((xw != 0) && ((XW_XD (xw)) == xd))
	x_close_window (xw);
    }
  ((x_display_table . items) [XD_ALLOCATION_INDEX (xd)]) = 0;
  XCloseDisplay (XD_DISPLAY (xd));
}

static void
DEFUN_VOID (x_close_all_displays)
{
  struct xdisplay ** scan = ((struct xdisplay **) (x_display_table . items));
  struct xdisplay ** end = (scan + (x_display_table . length));
  while (scan < end)
    {
      struct xdisplay * xd = (*scan++);
      if (xd != 0)
	x_close_display (xd);
    }
}

static void
DEFUN (xw_set_class_hint, (xw, name, class),
       struct xwindow * xw AND
       CONST char * name AND
       CONST char * class)
{
  XClassHint * class_hint = (XAllocClassHint ());
  if (class_hint == 0)
    error_external_return ();
  /* This structure is misdeclared, so cast the args. */
  (class_hint -> res_name) = ((char *) name);
  (class_hint -> res_class) = ((char *) class);
  XSetClassHint ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), class_hint);
  XFree ((caddr_t) class_hint);
}

void
DEFUN (xw_set_wm_input_hint, (xw, input_hint),
       struct xwindow * xw AND
       int input_hint)
{
  XWMHints * hints = (XAllocWMHints ());
  if (hints == 0)
    error_external_return ();
  (hints -> flags) = InputHint;
  (hints -> input) = (input_hint != 0);
  XSetWMHints ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), hints);
  XFree ((caddr_t) hints);
}

void
DEFUN (xw_set_wm_name, (xw, name), struct xwindow * xw AND CONST char * name)
{
  XTextProperty property;
  if ((XStringListToTextProperty (((char **) (&name)), 1, (&property))) == 0)
    error_external_return ();
  XSetWMName ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&property));
}

void
DEFUN (xw_set_wm_icon_name, (xw, name),
       struct xwindow * xw AND
       CONST char * name)
{
  XTextProperty property;
  if ((XStringListToTextProperty (((char **) (&name)), 1, (&property))) == 0)
    error_external_return ();
  XSetWMIconName ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&property));
}

void
DEFUN (xw_make_window_map, (xw, resource_name, resource_class, map_arg),
       struct xwindow * xw AND
       CONST char * resource_name AND
       CONST char * resource_class AND
       SCHEME_OBJECT map_arg)
{
  int map_p = 0;
  if (map_arg == SHARP_F)
    map_p = 1;
  else if ((PAIR_P (map_arg))
	   && (STRING_P (PAIR_CAR (map_arg)))
	   && (STRING_P (PAIR_CDR (map_arg))))
    {
      resource_class = ((CONST char *) (STRING_LOC ((PAIR_CDR (map_arg)), 0)));
      resource_name = ((CONST char *) (STRING_LOC ((PAIR_CAR (map_arg)), 0)));
      map_p = 1;
    }
  xw_set_class_hint (xw, resource_name, resource_class);
  if (map_p)
    {
      XMapWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
      XFlush (XW_DISPLAY (xw));
    }
}

static void
DEFUN (xw_process_event, (xw, event),
       struct xwindow * xw AND
       XEvent * event)
{
  if (x_debug)
    {
      char * type_name;
      fprintf (stderr, "\nX event: ");
      switch (event -> type)
	{
	case ButtonPress:	type_name = "ButtonPress"; break;
	case ButtonRelease:	type_name = "ButtonRelease"; break;
	case CirculateNotify:	type_name = "CirculateNotify"; break;
	case CreateNotify:	type_name = "CreateNotify"; break;
	case DestroyNotify:	type_name = "DestroyNotify"; break;
	case EnterNotify:	type_name = "EnterNotify"; break;
	case Expose:		type_name = "Expose"; break;
	case FocusIn:		type_name = "FocusIn"; break;
	case FocusOut:		type_name = "FocusOut"; break;
	case GraphicsExpose:	type_name = "GraphicsExpose"; break;
	case GravityNotify:	type_name = "GravityNotify"; break;
	case KeyPress:		type_name = "KeyPress"; break;
	case KeyRelease:	type_name = "KeyRelease"; break;
	case LeaveNotify:	type_name = "LeaveNotify"; break;
	case MapNotify:		type_name = "MapNotify"; break;
	case MappingNotify:	type_name = "MappingNotify"; break;
	case MotionNotify:	type_name = "MotionNotify"; break;
	case NoExpose:		type_name = "NoExpose"; break;
	case ReparentNotify:	type_name = "ReparentNotify"; break;
	case UnmapNotify:	type_name = "UnmapNotify"; break;
	case ConfigureNotify:
	  {
	    fprintf (stderr, "ConfigureNotify; width = %d, height = %d",
		     ((event -> xconfigure) . width),
		     ((event -> xconfigure) . height));
	    goto debug_done;
	  }
	case ClientMessage:
	  {
	    struct xdisplay * xd = (XW_XD (xw));
	    if ((((event -> xclient) . message_type) == (XD_WM_PROTOCOLS (xd)))
		&& (((event -> xclient) . format) == 32))
	      {
		if (((Atom) (((event -> xclient) . data . l) [0]))
		    == (XD_WM_DELETE_WINDOW (xd)))
		  type_name = "WM_DELETE_WINDOW";
		else if (((Atom) (((event -> xclient) . data . l) [0]))
			 == (XD_WM_TAKE_FOCUS (xd)))
		  type_name = "WM_TAKE_FOCUS";
		else
		  type_name = "WM_PROTOCOLS";
	      }
	    else
	      {
		fprintf (stderr,
			 "ClientMessage; message_type = 0x%x, format = %d",
			 ((event -> xclient) . message_type),
			 ((event -> xclient) . format));
		goto debug_done;
	      }
	  }
	  break;
	default:		type_name = 0; break;
	}
      if (type_name != 0)
	fprintf (stderr, "%s", type_name);
      else
	fprintf (stderr, "%d", (event -> type));
    debug_done:
      fprintf (stderr, "\n");
      fflush (stderr);
    }
  switch (event -> type)
    {
    case MappingNotify:
      switch ((event -> xmapping) . request)
	{
	case MappingKeyboard:
	case MappingModifier:
	  XRefreshKeyboardMapping (& (event -> xmapping));
	  break;
	}
      break;
    }
  (* (XW_EVENT_PROCESSOR (xw))) (xw, event);
}

enum event_type
{
  event_type_button_down,
  event_type_button_up,
  event_type_configure,
  event_type_enter,
  event_type_focus_in,
  event_type_focus_out,
  event_type_key_press,
  event_type_leave,
  event_type_motion,
  event_type_expose,
  event_type_delete_window,
  event_type_map,
  event_type_unmap,
  event_type_take_focus,
  event_type_supremum
};

#define EVENT_MASK_ARG(arg)						\
  (arg_index_integer ((arg), (1 << ((unsigned int) event_type_supremum))))

#define EVENT_ENABLED(xw, type)						\
  (((XW_EVENT_MASK (xw)) & (1 << ((unsigned int) (type)))) != 0)

#define EVENT_0 2
#define EVENT_1 3
#define EVENT_2 4
#define EVENT_3 5

#define EVENT_INTEGER(event, slot, number)				\
  VECTOR_SET ((event), (slot), (long_to_integer (number)))

static SCHEME_OBJECT
DEFUN (make_event_object, (xw, type, extra),
       struct xwindow * xw AND
       enum event_type type AND
       unsigned int extra)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, (2 + extra), 1));
  VECTOR_SET (result, 0, (LONG_TO_UNSIGNED_FIXNUM ((long) type)));
  VECTOR_SET (result, 1, (XW_TO_OBJECT (xw)));
  return (result);
}

static SCHEME_OBJECT
DEFUN (button_event, (xw, event, type),
       struct xwindow * xw AND
       XButtonEvent * event AND
       enum event_type type)
{
  SCHEME_OBJECT result = (make_event_object (xw, type, 4));
  EVENT_INTEGER (result, EVENT_0, (event -> x));
  EVENT_INTEGER (result, EVENT_1, (event -> y));
  {
    SCHEME_OBJECT conversion;
    int button_number;
    switch (event -> button)
      {
      case Button1: button_number = 1; break;
      case Button2: button_number = 2; break;
      case Button3: button_number = 3; break;
      case Button4: button_number = 4; break;
      case Button5: button_number = 5; break;
      default: button_number = 0; break;
      }
    if (button_number) {
      --button_number;
      if ((event -> state) & ShiftMask) {
	button_number += 5;
      }
      if ((event -> state) & ControlMask) {
	button_number += 10;
      }
      if ((event -> state) & Mod1Mask) {
	button_number += 20;
      }
      conversion = (LONG_TO_UNSIGNED_FIXNUM (button_number));
    } else {
      conversion = (SHARP_F);
    }
    VECTOR_SET (result, EVENT_2, conversion);
  }
  EVENT_INTEGER (result, EVENT_3, (event -> time));
  return (result);
}

static XComposeStatus compose_status;

static SCHEME_OBJECT
DEFUN (key_event, (xw, event, type),
       struct xwindow * xw AND
       XKeyEvent * event AND
       enum event_type type)
{
  char copy_buffer [80];
  KeySym keysym;
  int nbytes;

  /* Make ShiftLock modifier not affect keys with other modifiers. */
  if ((event -> state) &
      (ShiftMask || ControlMask
       || Mod1Mask || Mod2Mask || Mod3Mask || Mod4Mask || Mod5Mask))
    {
      if (((event->state) & LockMask) != 0)
	(event->state) -= LockMask;
    }
  nbytes =
    (XLookupString (event,
		    copy_buffer,
		    (sizeof (copy_buffer)),
		    (&keysym),
		    (&compose_status)));
  if (IsModifierKey (keysym))
    return (SHARP_F);
  else
    {
      SCHEME_OBJECT result = (make_event_object (xw, type, 4));
      VECTOR_SET (result, EVENT_0,
		  (memory_to_string (nbytes,
				     ((unsigned char *) copy_buffer))));
      /* Create Scheme bucky bits (kept independent of the character).
	 X has already controlified, so Scheme may choose to ignore
	 the control bucky bit.  */
      {
	long bucky = 0;
	if ((event -> state) & Mod1Mask) /* Meta */
	  bucky |= 1;
	if ((event -> state) & ControlMask) /* Control */
	  bucky |= 2;
	if ((event -> state) & Mod2Mask) /* Super */
	  bucky |= 4;
	if ((event -> state) & Mod3Mask) /* Hyper */
	  bucky |= 8;
	if ((event -> state) & Mod4Mask) /* Top */
	  bucky |= 16;
	VECTOR_SET (result, EVENT_1, (LONG_TO_UNSIGNED_FIXNUM (bucky)));
      }
      /* Move vendor-specific bit from bit 28 (zero-based) to bit 23
	 so that all keysym values will fit in Scheme fixnums.  */
      VECTOR_SET
	(result,
	 EVENT_2,
	 (LONG_TO_UNSIGNED_FIXNUM ((keysym & 0xffffff)
				   | (0x800000 & (keysym >> 5)))));
      EVENT_INTEGER (result, EVENT_3, (event -> time));
      return (result);
    }
}

#define CONVERT_TRIVIAL_EVENT(scheme_name)				\
  if (EVENT_ENABLED (xw, scheme_name))					\
    result = (make_event_object (xw, scheme_name, 0));			\
  break

static SCHEME_OBJECT
DEFUN (x_event_to_object, (event), XEvent * event)
{
  struct xwindow * xw = (x_window_to_xw ((event -> xany) . window));
  SCHEME_OBJECT result = SHARP_F;
  switch (event -> type)
    {
    case KeyPress:
      if (EVENT_ENABLED (xw, event_type_key_press))
	result = (key_event (xw, (& (event -> xkey)), event_type_key_press));
      break;
    case ButtonPress:
      if (EVENT_ENABLED (xw, event_type_button_down))
	result =
	  (button_event (xw, (& (event -> xbutton)), event_type_button_down));
      break;
    case ButtonRelease:
      if (EVENT_ENABLED (xw, event_type_button_up))
	result =
	  (button_event (xw, (& (event -> xbutton)), event_type_button_up));
      break;
    case MotionNotify:
      if (EVENT_ENABLED (xw, event_type_motion))
	{
	  result = (make_event_object (xw, event_type_motion, 2));
	  EVENT_INTEGER (result, EVENT_0, ((event -> xmotion) . x));
	  EVENT_INTEGER (result, EVENT_1, ((event -> xmotion) . y));
	}
      break;
    case ConfigureNotify:
      if (EVENT_ENABLED (xw, event_type_configure))
	{
	  result = (make_event_object (xw, event_type_configure, 2));
	  EVENT_INTEGER (result, EVENT_0, ((event -> xconfigure) . width));
	  EVENT_INTEGER (result, EVENT_1, ((event -> xconfigure) . height));
	}
      break;
    case Expose:
      if (EVENT_ENABLED (xw, event_type_expose))
	{
	  result = (make_event_object (xw, event_type_expose, 4));
	  EVENT_INTEGER (result, EVENT_0, ((event -> xexpose) . x));
	  EVENT_INTEGER (result, EVENT_1, ((event -> xexpose) . y));
	  EVENT_INTEGER (result, EVENT_2, ((event -> xexpose) . width));
	  EVENT_INTEGER (result, EVENT_3, ((event -> xexpose) . height));
	}
      break;
    case GraphicsExpose:
      if (EVENT_ENABLED (xw, event_type_expose))
	{
	  result = (make_event_object (xw, event_type_expose, 4));
	  EVENT_INTEGER (result, EVENT_0, ((event -> xgraphicsexpose) . x));
	  EVENT_INTEGER (result, EVENT_1, ((event -> xgraphicsexpose) . y));
	  EVENT_INTEGER (result, EVENT_2,
			 ((event -> xgraphicsexpose) . width));
	  EVENT_INTEGER (result, EVENT_3,
			 ((event -> xgraphicsexpose) . height));
	}
      break;
    case ClientMessage:
      {
	struct xdisplay * xd = (XW_XD (xw));
	if ((((event -> xclient) . message_type) == (XD_WM_PROTOCOLS (xd)))
	    && (((event -> xclient) . format) == 32))
	  {
	    if (((Atom) (((event -> xclient) . data . l) [0]))
		== (XD_WM_DELETE_WINDOW (xd)))
	      {
		if (EVENT_ENABLED (xw, event_type_delete_window))
		  result =
		    (make_event_object (xw, event_type_delete_window, 0));
	      }
	    else if (((Atom) (((event -> xclient) . data . l) [0]))
		     == (XD_WM_TAKE_FOCUS (xd)))
	      {
		if (EVENT_ENABLED (xw, event_type_take_focus))
		  {
		    result =
		      (make_event_object (xw, event_type_take_focus, 1));
		    EVENT_INTEGER
		      (result, EVENT_0, (((event -> xclient) . data . l) [1]));
		  }
	      }
	  }
      }
      break;
    case EnterNotify: CONVERT_TRIVIAL_EVENT (event_type_enter);
    case LeaveNotify: CONVERT_TRIVIAL_EVENT (event_type_leave);
    case FocusIn: CONVERT_TRIVIAL_EVENT (event_type_focus_in);
    case FocusOut: CONVERT_TRIVIAL_EVENT (event_type_focus_out);
    case MapNotify: CONVERT_TRIVIAL_EVENT (event_type_map);
    case UnmapNotify: CONVERT_TRIVIAL_EVENT (event_type_unmap);
    }
  return (result);
}

/* The use of `XD_CACHED_EVENT' prevents an event from being lost due
   to garbage collection.  First `XD_CACHED_EVENT' is set to hold the
   current event, then the allocations are performed.  If one of them
   fails, the primitive will exit, and when it reenters it will notice
   the cached event and use it.  It is important that this be the only
   entry that reads events -- or else that all other event readers
   cooperate with this strategy.  */

static SCHEME_OBJECT
DEFUN (xd_process_events, (xd, non_block_p),
       struct xdisplay * xd AND
       int non_block_p)
{
  Display * display = (XD_DISPLAY (xd));
  unsigned int events_queued;
  if (XD_CACHED_EVENT_P (xd))
    {
      events_queued = (XEventsQueued (display, QueuedAlready));
      goto restart;
    }
  events_queued =
    (UX_have_select_p ? (XEventsQueued (display, QueuedAlready))
     : non_block_p ? (XEventsQueued (display, QueuedAfterReading))
     : 0);
  while (1)
    {
      XEvent event;
      if (events_queued > 0)
	events_queued -= 1;
      else if (UX_have_select_p)
	switch (UX_select_input ((ConnectionNumber (display)),
				 (!non_block_p)))
	  {
	  case select_input_none:
	    return (SHARP_F);
	  case select_input_other:
	    return (LONG_TO_FIXNUM (-2));
	  case select_input_process_status:
	    return (LONG_TO_FIXNUM (-3));
	  case select_input_interrupt:
	    return (LONG_TO_FIXNUM (-4));
	  case select_input_argument:
	    events_queued = (XEventsQueued (display, QueuedAfterReading));
	    continue;
	  }
      else if (non_block_p)
	return (SHARP_F);
      XNextEvent (display, (&event));
      if ((event . type) == KeymapNotify)
	continue;
      {
	struct xwindow * xw = (x_window_to_xw (event . xany . window));
	if (xw == 0)
	  continue;
	xw_process_event (xw, (&event));
      }
      (XD_CACHED_EVENT (xd)) = event;
      (XD_CACHED_EVENT_P (xd)) = 1;
    restart:
      {
	SCHEME_OBJECT result = (x_event_to_object (&event));
	(XD_CACHED_EVENT_P (xd)) = 0;
	if (result != SHARP_F)
	  return (result);
      }
    }
}

extern XFontStruct * saved_font;

static void
DEFUN_VOID (initialize_once)
{
  allocation_table_initialize (&x_display_table);
  allocation_table_initialize (&x_window_table);
  allocation_table_initialize (&x_image_table);
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_handler);
  add_reload_cleanup (x_close_all_displays);
  saved_font = 0;
  initialization_done = 1;
}

DEFINE_PRIMITIVE ("X-DEBUG", Prim_x_debug, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  x_debug = (BOOLEAN_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-OPEN-DISPLAY", Prim_x_open_display, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  INITIALIZE_ONCE ();
  {
    struct xdisplay * xd = (x_malloc (sizeof (struct xdisplay)));
    (XD_DISPLAY (xd)) =
      (XOpenDisplay (((ARG_REF (1)) == SHARP_F) ? 0 : (STRING_ARG (1))));
    if ((XD_DISPLAY (xd)) == 0)
      {
	free (xd);
	PRIMITIVE_RETURN (SHARP_F);
      }
    (XD_ALLOCATION_INDEX (xd)) =
      (allocate_table_index ((&x_display_table), xd));
    (XD_WM_PROTOCOLS (xd)) =
      (XInternAtom ((XD_DISPLAY (xd)), "WM_PROTOCOLS", False));
    (XD_WM_DELETE_WINDOW (xd)) =
      (XInternAtom ((XD_DISPLAY (xd)), "WM_DELETE_WINDOW", False));
    (XD_WM_TAKE_FOCUS (xd)) =
      (XInternAtom ((XD_DISPLAY (xd)), "WM_TAKE_FOCUS", False));
    (XD_CACHED_EVENT_P (xd)) = 0;
    PRIMITIVE_RETURN (XD_TO_OBJECT (xd));
  }
}

DEFINE_PRIMITIVE ("X-CLOSE-DISPLAY", Prim_x_close_display, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  x_close_display (x_display_arg (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-CLOSE-ALL-DISPLAYS", Prim_x_close_all_displays, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  INITIALIZE_ONCE ();
  x_close_all_displays ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-CLOSE-WINDOW", Prim_x_close_window, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    x_close_window (xw);
    XFlush (display);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-DISPLAY-PROCESS-EVENTS", Prim_x_display_process_events, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (xd_process_events ((x_display_arg (1)), (BOOLEAN_ARG (2))));
}

static void
DEFUN (update_input_mask, (xw), struct xwindow * xw)
{
  {
    long event_mask = 0;

    if (EVENT_ENABLED (xw, event_type_expose))
      event_mask |= ExposureMask;
    if ((EVENT_ENABLED (xw, event_type_configure))
	|| (EVENT_ENABLED (xw, event_type_map))
	|| (EVENT_ENABLED (xw, event_type_unmap)))
      event_mask |= StructureNotifyMask;
    if (EVENT_ENABLED (xw, event_type_button_down))
      event_mask |= ButtonPressMask;
    if (EVENT_ENABLED (xw, event_type_button_up))
      event_mask |= ButtonReleaseMask;
    if (EVENT_ENABLED (xw, event_type_key_press))
      event_mask |= KeyPressMask;
    if (EVENT_ENABLED (xw, event_type_enter))
      event_mask |= EnterWindowMask;
    if (EVENT_ENABLED (xw, event_type_leave))
      event_mask |= LeaveWindowMask;
    if ((EVENT_ENABLED (xw, event_type_focus_in))
	|| (EVENT_ENABLED (xw, event_type_focus_out)))
      event_mask |= FocusChangeMask;
    if (EVENT_ENABLED (xw, event_type_motion))
      event_mask |= PointerMotionMask;
    XSelectInput ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), event_mask);
  }
  {
    struct xdisplay * xd = (XW_XD (xw));
    Atom protocols [2];
    unsigned int n_protocols = 0;

    if (EVENT_ENABLED (xw, event_type_delete_window))
      (protocols[n_protocols++]) = (XD_WM_DELETE_WINDOW (xd));
    if (EVENT_ENABLED (xw, event_type_take_focus))
      (protocols[n_protocols++]) = (XD_WM_TAKE_FOCUS (xd));
    if (n_protocols > 0)
      XSetWMProtocols
	((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&protocols[0]), n_protocols);
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-EVENT-MASK", Prim_x_window_event_mask, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_EVENT_MASK (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-EVENT-MASK", Prim_x_window_set_event_mask, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    (XW_EVENT_MASK (xw)) = (EVENT_MASK_ARG (2));
    update_input_mask (xw);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-OR-EVENT-MASK", Prim_x_window_or_event_mask, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    (XW_EVENT_MASK (xw)) |= (EVENT_MASK_ARG (2));
    update_input_mask (xw);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-ANDC-EVENT-MASK", Prim_x_window_andc_event_mask, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    (XW_EVENT_MASK (xw)) &=~ (EVENT_MASK_ARG (2));
    update_input_mask (xw);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-INPUT-FOCUS", Prim_x_window_set_input_focus, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    XSetInputFocus
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       RevertToParent,
       ((Time) (arg_integer (2))));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-DISPLAY", Prim_x_window_display, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (XD_TO_OBJECT (XW_XD (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-X-SIZE", Prim_x_window_x_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_X_SIZE (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-Y-SIZE", Prim_x_window_y_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_Y_SIZE (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-MAP", Prim_x_window_map, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XMapWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-UNMAP", Prim_x_window_unmap, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XUnmapWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-BEEP", Prim_x_window_beep, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  XBell ((XW_DISPLAY (x_window_arg (1))), 100); /* 100% */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-CLEAR", Prim_x_window_clear, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XClearArea ((XW_DISPLAY (xw)),
		(XW_WINDOW (xw)),
		((XW_CLIP_X (xw)) + (XW_INTERNAL_BORDER_WIDTH (xw))),
		((XW_CLIP_Y (xw)) + (XW_INTERNAL_BORDER_WIDTH (xw))),
		(XW_CLIP_WIDTH (xw)),
		(XW_CLIP_HEIGHT (xw)),
		False);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-DISPLAY-FLUSH", Prim_x_display_flush, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  XFlush (XD_DISPLAY (x_display_arg (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-DISPLAY-SYNC", Prim_x_display_sync, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  XSync ((XD_DISPLAY (x_display_arg (1))), (BOOLEAN_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-DISPLAY-GET-DEFAULT", Prim_x_display_get_default, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    char * result =
      (XGetDefault
       ((XD_DISPLAY (x_display_arg (1))), (STRING_ARG (2)), (STRING_ARG (3))));
    PRIMITIVE_RETURN
      ((result == 0) ? SHARP_F
       : (char_pointer_to_string ((unsigned char *) result)));
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-FOREGROUND-COLOR", Prim_x_window_set_foreground_color, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned long foreground_pixel = (arg_color (2, display));
    (XW_FOREGROUND_PIXEL (xw)) = foreground_pixel;
    XSetForeground (display, (XW_NORMAL_GC (xw)), foreground_pixel);
    XSetBackground (display, (XW_REVERSE_GC (xw)), foreground_pixel);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-BACKGROUND-COLOR", Prim_x_window_set_background_color, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned long background_pixel = (arg_color (2, display));
    (XW_BACKGROUND_PIXEL (xw)) = background_pixel;
    XSetWindowBackground (display, (XW_WINDOW (xw)), background_pixel);
    XSetBackground (display, (XW_NORMAL_GC (xw)), background_pixel);
    XSetForeground (display, (XW_REVERSE_GC (xw)), background_pixel);
    XSetForeground (display, (XW_CURSOR_GC (xw)), background_pixel);
    x_set_mouse_colors
      (display,
       (XW_MOUSE_CURSOR (xw)),
       (XW_MOUSE_PIXEL (xw)),
       background_pixel);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-BORDER-COLOR", Prim_x_window_set_border_color, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned long border_pixel = (arg_color (2, display));
    (XW_BORDER_PIXEL (xw)) = border_pixel;
    XSetWindowBorder (display, (XW_WINDOW (xw)), border_pixel);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-CURSOR-COLOR", Prim_x_window_set_cursor_color, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned long cursor_pixel = (arg_color (2, display));
    (XW_CURSOR_PIXEL (xw)) = cursor_pixel;
    XSetBackground (display, (XW_CURSOR_GC (xw)), cursor_pixel);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-MOUSE-COLOR", Prim_x_window_set_mouse_color, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned long mouse_pixel = (arg_color (2, display));
    (XW_MOUSE_PIXEL (xw)) = mouse_pixel;
    x_set_mouse_colors
      (display,
       (XW_MOUSE_CURSOR (xw)),
       mouse_pixel,
       (XW_BACKGROUND_PIXEL (xw)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-MOUSE-SHAPE", Prim_x_window_set_mouse_shape, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    Window window = (XW_WINDOW (xw));
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
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-FONT", Prim_x_window_set_font, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    XFontStruct * font = (XLoadQueryFont (display, (STRING_ARG (2))));
    if (font == 0)
      PRIMITIVE_RETURN (SHARP_F);
    XFreeFont (display, (XW_FONT (xw)));
    (XW_FONT (xw)) = font;
    {
      Font fid = (font -> fid);
      XSetFont (display, (XW_NORMAL_GC (xw)), fid);
      XSetFont (display, (XW_REVERSE_GC (xw)), fid);
      XSetFont (display, (XW_CURSOR_GC (xw)), fid);
    }
  }
  PRIMITIVE_RETURN (SHARP_T);
}

static SCHEME_OBJECT
DEFUN (convert_char_struct, (char_struct), XCharStruct * char_struct)
{
  SCHEME_OBJECT char_structure;

  if (((char_struct -> lbearing) == 0)
      && ((char_struct -> rbearing) == 0)
      && ((char_struct -> width) == 0)
      && ((char_struct -> ascent) == 0)
      && ((char_struct -> descent) == 0))

    {
      return (SHARP_F);
    }
  char_structure = (allocate_marked_vector (TC_VECTOR, 5, true));

  VECTOR_SET (char_structure, 0, (long_to_integer (char_struct -> lbearing)));
  VECTOR_SET (char_structure, 1, (long_to_integer (char_struct -> rbearing)));
  VECTOR_SET (char_structure, 2, (long_to_integer (char_struct -> width)));
  VECTOR_SET (char_structure, 3, (long_to_integer (char_struct -> ascent)));
  VECTOR_SET (char_structure, 4, (long_to_integer (char_struct -> descent)));
  return (char_structure);
}

XFontStruct * saved_font;

DEFINE_PRIMITIVE ("X-FONT-STRUCTURE", Prim_x_font_structure, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 10, true));
    SCHEME_OBJECT font_name = ARG_REF (2);
    Display * display = (XD_DISPLAY (x_display_arg (1)));

    if (saved_font == 0)
      {
	saved_font = (XLoadQueryFont
		      (display, (char *) (STRING_LOC (font_name, 0))));
	if (saved_font == 0)
	  {
	    PRIMITIVE_RETURN (SHARP_F);
	  }
      }
    /* Handle only 8-bit fonts because of laziness. */
    if (((saved_font -> min_byte1) != 0) || ((saved_font -> max_byte1) != 0))
      {
	XFreeFont (display, saved_font);
	saved_font = 0;
	PRIMITIVE_RETURN (SHARP_F);
      }
    if ((saved_font -> per_char) == NULL)
      {
	VECTOR_SET (result, 6, SHARP_F);
      }
    else
      {
	unsigned int start_index = (saved_font -> min_char_or_byte2);
	unsigned int index;
	unsigned int length = 
	  ((saved_font -> max_char_or_byte2) - start_index + 1);
	SCHEME_OBJECT character_vector =
	  (allocate_marked_vector (TC_VECTOR, length, true));
	for (index = 0; index < length; index++)
	  {
	    VECTOR_SET (character_vector,
			index,
			convert_char_struct ((saved_font -> per_char) + index));
	  }
	VECTOR_SET (result, 6, (long_to_integer (start_index)));
	VECTOR_SET (result, 7, character_vector);
      }
    VECTOR_SET (result, 0, font_name);
    VECTOR_SET (result, 1, (long_to_integer (saved_font -> direction)));
    VECTOR_SET (result, 2, (BOOLEAN_TO_OBJECT
			    ((saved_font -> all_chars_exist) == True)));
    VECTOR_SET (result, 3, (long_to_integer (saved_font -> default_char)));
    VECTOR_SET (result, 4, convert_char_struct (& (saved_font -> min_bounds)));
    VECTOR_SET (result, 5, convert_char_struct (& (saved_font -> max_bounds)));
    VECTOR_SET (result, 8, (long_to_integer (saved_font -> ascent)));
    VECTOR_SET (result, 9, (long_to_integer (saved_font -> descent)));
    XFreeFont (display, saved_font);
    saved_font = 0;
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-BORDER-WIDTH", Prim_x_window_set_border_width, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned int border_width = (arg_nonnegative_integer (2));
    (XW_BORDER_WIDTH (xw)) = border_width;
    XSetWindowBorderWidth (display, (XW_WINDOW (xw)), border_width);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-INTERNAL-BORDER-WIDTH", Prim_x_window_set_internal_border_width, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned int internal_border_width = (arg_nonnegative_integer (2));
    (XW_INTERNAL_BORDER_WIDTH (xw)) = internal_border_width;
    XResizeWindow
      (display,
       (XW_WINDOW (xw)),
       ((XW_X_SIZE (xw)) + (2 * internal_border_width)),
       ((XW_Y_SIZE (xw)) + (2 * internal_border_width)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-SIZE", Prim_x_window_set_size, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
    XResizeWindow
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       ((arg_nonnegative_integer (2)) + extra),
       ((arg_nonnegative_integer (3)) + extra));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-POSITION", Prim_x_window_set_position, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    XMoveWindow
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       (arg_integer (2)),
       (arg_integer (3)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-NAME", Prim_x_window_set_name, 2, 2,
  "Set the name of WINDOW to STRING.")
{
  PRIMITIVE_HEADER (2);
  xw_set_wm_name ((x_window_arg (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-ICON-NAME", Prim_x_window_set_icon_name, 2, 2,
  "Set the icon name of WINDOW to STRING.")
{
  PRIMITIVE_HEADER (2);
  xw_set_wm_icon_name ((x_window_arg (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-CLASS-HINT", Prim_x_window_set_class_hint, 3, 3,
  "Set the class hint of WINDOW to RESOURCE_NAME and RESOURCE_CLASS.")
{
  PRIMITIVE_HEADER (3);
  xw_set_class_hint ((x_window_arg (1)), (STRING_ARG (2)), (STRING_ARG (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-INPUT-HINT", Prim_x_window_set_input_hint, 2, 2,
  "Set the input hint of WINDOW to INPUT.")
{
  PRIMITIVE_HEADER (2);
  xw_set_wm_input_hint ((x_window_arg (1)), (BOOLEAN_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-TRANSIENT-FOR-HINT", Prim_x_window_set_transient_for, 2, 2,
  "Set the transient-for hint of WINDOW to PRIMARY-WINDOW.")
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    struct xwindow * transient_for = (x_window_arg (2));
    if ((xw == transient_for) || ((XW_XD (xw)) != (XW_XD (transient_for))))
      error_bad_range_arg (2);
    XSetTransientForHint
      ((XW_DISPLAY (xw)),
       (XW_WINDOW (xw)),
       (XW_WINDOW (transient_for)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-ICONIFY", Prim_x_window_iconify, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    XIconifyWindow (display, (XW_WINDOW (xw)), (DefaultScreen (display)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-WITHDRAW", Prim_x_window_withdraw, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    XWithdrawWindow (display, (XW_WINDOW (xw)), (DefaultScreen (display)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

unsigned int
DEFUN (allocate_x_image, (image), XImage * image)
{
  struct ximage * xi = (x_malloc (sizeof (struct ximage)));
  unsigned int index = (allocate_table_index ((&x_image_table), xi));
  (XI_ALLOCATION_INDEX (xi)) = index;
  (XI_IMAGE (xi)) = image;
  return (index);
}

void
DEFUN (deallocate_x_image, (xi), struct ximage * xi)
{
  ((x_image_table . items) [XI_ALLOCATION_INDEX (xi)]) = 0;
  free (xi);
}

unsigned int
DEFUN (allocate_x_visual, (visual), Visual * visual)
{
  struct xvisual * xv = (x_malloc (sizeof (struct xvisual)));
  unsigned int index = (allocate_table_index ((&x_visual_table), xv));
  (XV_ALLOCATION_INDEX (xv)) = index;
  (XV_VISUAL (xv)) = visual;
  return (index);
}

void
DEFUN (deallocate_x_visual, (xv), struct xvisual * xv)
{
  ((x_visual_table . items) [XV_ALLOCATION_INDEX (xv)]) = 0;
  free (xv);
}

unsigned int
DEFUN (allocate_x_colormap, (colormap, xd),
       Colormap colormap AND
       struct xdisplay * xd)
{
  struct xcolormap * xcm = (x_malloc (sizeof (struct xcolormap)));
  unsigned int index = (allocate_table_index ((&x_colormap_table), xcm));
  (XCM_ALLOCATION_INDEX (xcm)) = index;
  (XCM_COLORMAP (xcm)) = colormap;
  (XCM_XD (xcm)) = xd;
  return (index);
}

void
DEFUN (deallocate_x_colormap, (xcm), struct xcolormap * xcm)
{
  ((x_colormap_table . items) [XCM_ALLOCATION_INDEX (xcm)]) = 0;
  free (xcm);
}
