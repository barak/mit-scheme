/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11term.c,v 1.9 1990/07/16 21:01:11 markf Exp $

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

/* X11 terminal for Edwin. */

#include "scheme.h"
#include "prims.h"
#include "x11.h"
#include "x11term.h"

#define RESOURCE_NAME "edwin"
#define DEFAULT_GEOMETRY "80x40+0+0"

#define XTERM_DRAW_CHARS(xw, x, y, s, n, gc)				\
  XDrawImageString							\
    ((XW_DISPLAY (xw)),							\
     (XW_WINDOW (xw)),							\
     gc,								\
     (XTERM_X_PIXEL (xw, x)),						\
     ((XTERM_Y_PIXEL (xw, y)) + (FONT_BASE (XW_FONT (xw)))),		\
     s,									\
     n)

#define WITH_CURSOR_PRESERVED(xw, expression, body)			\
{									\
  if ((expression) && (XW_CURSOR_VISIBLE_P (xw)))			\
    {									\
      (XW_CURSOR_VISIBLE_P (xw)) = 0;					\
      body;								\
      xterm_draw_cursor (xw);						\
    }									\
  else									\
    body;								\
}

extern void xterm_erase_cursor ();

void
xterm_erase_cursor (xw)
     struct xwindow * xw;
{
  fast int x, y, index;

  if (! (XW_VISIBLE_P (xw)))
    return;

  x = (XW_CURSOR_X (xw));
  y = (XW_CURSOR_Y (xw));
  index = (XTERM_CHAR_INDEX (xw, x, y));
  XTERM_DRAW_CHARS
    (xw, x, y, (XTERM_CHAR_LOC (xw, index)), 1,
     (XTERM_HL_GC (xw, (XTERM_HL (xw, index)))));
  (XW_CURSOR_VISIBLE_P (xw)) = 0;
  return;
}

extern void xterm_draw_cursor();

void
xterm_draw_cursor (xw)
     struct xwindow * xw;
{
  fast int x, y;

  if (! (XW_VISIBLE_P (xw)))
    return;

  /* Need option here to draw cursor as outline box when this xterm is
     not the one that input is going to. */
  x = (XW_CURSOR_X (xw));
  y = (XW_CURSOR_Y (xw));
  XTERM_DRAW_CHARS (xw, x, y,
		    (XTERM_CHAR_LOC (xw, (XTERM_CHAR_INDEX (xw, x, y)))),
		    1,
		    (XW_CURSOR_GC (xw)));
  (XW_CURSOR_VISIBLE_P (xw)) = 1;
  return;
}

static void
xterm_wm_set_size_hint (xw, flags, x, y)
     struct xwindow * xw;
     long flags;
     int x, y;
{
  Window window = (XW_WINDOW (xw));
  int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  XFontStruct * font = (XW_FONT (xw));
  int fwidth = (FONT_WIDTH (font));
  int fheight = (FONT_HEIGHT (font));
  XSizeHints size_hints;

  (size_hints . flags) = (PResizeInc | PMinSize | flags);
  (size_hints . x) = x;
  (size_hints . y) = y;
  (size_hints . width) = (((XW_X_CSIZE (xw)) * fwidth) + extra);
  (size_hints . height) = (((XW_Y_CSIZE (xw)) * fheight) + extra);
  (size_hints . width_inc) = fwidth;
  (size_hints . height_inc) = fheight;
  (size_hints . min_width) = extra;
  (size_hints . min_height) = extra;
  XSetNormalHints ((XW_DISPLAY (xw)), window, (& size_hints));
  return;
}

static void
xterm_deallocate (xw)
     struct xwindow * xw;
{
  free (XW_CHARACTER_MAP (xw));
  free (XW_HIGHLIGHT_MAP (xw));
  return;
}

static void
xterm_dump_rectangle (xw, x, y, width, height)
     struct xwindow * xw;
     int x, y, width, height;
{
  XFontStruct * font = (XW_FONT (xw));
  int fwidth = (FONT_WIDTH (font));
  int fheight = (FONT_HEIGHT (font));
  int border = (XW_INTERNAL_BORDER_WIDTH (xw));
  char * character_map = (XW_CHARACTER_MAP (xw));
  char * highlight_map = (XW_HIGHLIGHT_MAP (xw));
  int x_start = ((x - border) / fwidth);
  int y_start = ((y - border) / fheight);
  int x_end = ((((x + width) - border) + (fwidth - 1)) / fwidth);
  int y_end = ((((y + height) - border) + (fheight - 1)) / fheight);
  int yi;

  if (x_end > (XW_X_CSIZE (xw))) x_end = (XW_X_CSIZE (xw));
  if (y_end > (XW_Y_CSIZE (xw))) y_end = (XW_Y_CSIZE (xw));
  if (x_start < x_end)
    {
      for (yi = y_start; (yi < y_end); yi += 1)
	{
	  int index = (XTERM_CHAR_INDEX (xw, 0, yi));
	  char * line_char = (& (character_map [index]));
	  char * line_hl = (& (highlight_map [index]));
	  int xi = x_start;
	  while (1)
	    {
	      int hl = (line_hl [xi]);
	      int i = (xi + 1);
	      while ((i < x_end) && ((line_hl [i]) == hl))
		i += 1;
	      XTERM_DRAW_CHARS (xw, xi, yi,
				(& (line_char [xi])), (i - xi),
				(XTERM_HL_GC (xw, hl)));
	      if (i == x_end)
		break;
	      xi = i;
	    }
	}
      if ((XW_CURSOR_VISIBLE_P (xw)) &&
	  ((x_start <= (XW_CURSOR_X (xw))) && ((XW_CURSOR_X (xw)) < x_end)) &&
	  ((y_start <= (XW_CURSOR_Y (xw))) && ((XW_CURSOR_Y (xw)) < y_end)))
	xterm_draw_cursor (xw);
    }
  return;
}

#define MAKE_MAP(map, size, fill)					\
{									\
  char * MAKE_MAP_scan;							\
  char * MAKE_MAP_end;							\
									\
  map = (x_malloc (size));						\
  MAKE_MAP_scan = (& (map [0]));					\
  MAKE_MAP_end = (MAKE_MAP_scan + size);				\
  while (MAKE_MAP_scan < MAKE_MAP_end)					\
    (*MAKE_MAP_scan++) = fill;						\
}

static void xterm_process_event ();

DEFINE_PRIMITIVE ("XTERM-OPEN-WINDOW", Prim_xterm_open_window, 3, 3,
  "(xterm-open-window display geometry suppress-map?)")
{
  Display * display;
  int screen_number;
  struct drawing_attributes attributes;
  XFontStruct * font;
  int fwidth;
  int fheight;
  int border_width;
  int x_pos;
  int y_pos;
  int x_csize;
  int y_csize;
  int x_size;
  int y_size;
  char * name;
  int internal_border_width;
  int extra;
  Window window;
  long flags;
  char * character_map;
  char * highlight_map;
  struct xwindow * xw;
  PRIMITIVE_HEADER (3);

  display = (DISPLAY_ARG (1));
  screen_number = (DefaultScreen (display));
  name = "edwin";
  x_default_attributes (display, RESOURCE_NAME, (& attributes));
  font = (attributes . font);
  border_width = (attributes . border_width);
  internal_border_width = (attributes . internal_border_width);
  fwidth = (FONT_WIDTH (font));
  fheight = (FONT_HEIGHT (font));
  extra = (2 * internal_border_width);
  x_pos = (-1);
  y_pos = (-1);
  x_csize = 80;
  y_csize = 24;
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
		  fwidth, fheight, extra, extra,
		  (& x_pos), (& y_pos), (& x_csize), (& y_csize)));
    flags = 0;
    flags |=
      (((result & XValue) && (result & YValue)) ? USPosition : PPosition);
    flags |=
      (((result & WidthValue) && (result & HeightValue)) ? USSize : PSize);
  }
  {
    int map_size = (x_csize * y_csize);
    MAKE_MAP (character_map, map_size, ' ');
    MAKE_MAP (highlight_map, map_size, 0);
  }
  x_size = (x_csize * fwidth);
  y_size = (y_csize * fheight);
  window =
    (XCreateSimpleWindow
     (display, (RootWindow (display, screen_number)),
      x_pos, y_pos, (x_size + extra), (y_size + extra),
      border_width,
      (attributes . border_pixel),
      (attributes . background_pixel)));
  if (window == ((Window) 0))
    error_external_return ();

  xw =
    (x_make_window
     (display, window, x_size, y_size, (& attributes),
      (sizeof (struct xterm_extra)), xterm_deallocate,
      xterm_process_event));
  (XW_X_CSIZE (xw)) = x_csize;
  (XW_Y_CSIZE (xw)) = y_csize;
  (XW_CURSOR_X (xw)) = 0;
  (XW_CURSOR_Y (xw)) = 0;
  (XW_CHARACTER_MAP (xw)) = character_map;
  (XW_HIGHLIGHT_MAP (xw)) = highlight_map;
  (XW_CURSOR_VISIBLE_P (xw)) = 0;
  (XW_CHAR_BUFFER_INDEX (xw)) = 0;
  (XW_CHAR_BUFFER_LENGTH (xw)) = 4;
  (XW_CHAR_BUFFER (xw)) = (x_malloc (4));

  XSelectInput
    (display, window,
     (KeyPressMask | ExposureMask |
      ButtonPressMask | ButtonReleaseMask |
      StructureNotifyMask | FocusChangeMask |
      PointerMotionHintMask | ButtonMotionMask |
      LeaveWindowMask | EnterWindowMask));
  xterm_wm_set_size_hint (xw, flags, x_pos, y_pos);
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

DEFINE_PRIMITIVE ("XTERM-X-SIZE", Prim_xterm_x_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_X_CSIZE (WINDOW_ARG (1))));
}

DEFINE_PRIMITIVE ("XTERM-Y-SIZE", Prim_xterm_y_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_Y_CSIZE (WINDOW_ARG (1))));
}

DEFINE_PRIMITIVE ("XTERM-SET-SIZE", Prim_xterm_set_size, 3, 3, 0)
{
  struct xwindow * xw;
  int extra;
  XFontStruct * font;
  PRIMITIVE_HEADER (3);
  xw = (WINDOW_ARG (1));
  extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  font = (XW_FONT (xw));
  XResizeWindow
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (((arg_nonnegative_integer (2)) * (FONT_WIDTH (font))) + extra),
     (((arg_nonnegative_integer (3)) * (FONT_HEIGHT (font))) + extra));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-BUTTON", Prim_xterm_button, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_BUTTON (WINDOW_ARG (1))));
}

DEFINE_PRIMITIVE ("XTERM-POINTER-X", Prim_xterm_pointer_x, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_POINTER_X (WINDOW_ARG (1))));
}

DEFINE_PRIMITIVE ("XTERM-POINTER-Y", Prim_xterm_pointer_y, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (XW_POINTER_Y (WINDOW_ARG (1))));
}

DEFINE_PRIMITIVE ("XTERM-WRITE-CURSOR!", Prim_xterm_write_cursor, 3, 3, 0)
{
  fast struct xwindow * xw;
  fast int x, y;
  PRIMITIVE_HEADER (3);

  xw = (WINDOW_ARG (1));
  x = (arg_index_integer (2, (XW_X_CSIZE (xw))));
  y = (arg_index_integer (3, (XW_Y_CSIZE (xw))));
  if (XW_CURSOR_VISIBLE_P (xw))
    xterm_erase_cursor (xw);
  (XW_CURSOR_X (xw)) = x;
  (XW_CURSOR_Y (xw)) = y;
  xterm_draw_cursor (xw);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-WRITE-CHAR!", Prim_xterm_write_char, 5, 5, 0)
{
  struct xwindow * xw;
  int x, y;
  int c;
  int hl;
  int index;
  char * map_ptr;
  PRIMITIVE_HEADER (5);

  xw = (WINDOW_ARG (1));
  x = (arg_index_integer (2, (XW_X_CSIZE (xw))));
  y = (arg_index_integer (3, (XW_Y_CSIZE (xw))));
  c = (arg_ascii_char (4));
  hl = (HL_ARG (5));
  index = (XTERM_CHAR_INDEX (xw, x, y));
  map_ptr = (XTERM_CHAR_LOC (xw, index));
  (*map_ptr) = c;
  (XTERM_HL (xw, index)) = hl;
  WITH_CURSOR_PRESERVED
    (xw, ((x == (XW_CURSOR_X (xw))) && (y == (XW_CURSOR_Y (xw)))),
     {
       XTERM_DRAW_CHARS (xw, x, y, map_ptr, 1, (XTERM_HL_GC (xw, (xw, hl))));
     });
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-WRITE-SUBSTRING!", Prim_xterm_write_substring, 7, 7, 0)
{
  struct xwindow * xw;
  int x, y;
  SCHEME_OBJECT string;
  int start, end;
  int hl;
  int length;
  unsigned char * string_scan;
  unsigned char * string_end;
  int index;
  char * char_start;
  char * char_scan;
  char * hl_scan;
  PRIMITIVE_HEADER (7);
  xw = (WINDOW_ARG (1));
  x = (arg_index_integer (2, (XW_X_CSIZE (xw))));
  y = (arg_index_integer (3, (XW_Y_CSIZE (xw))));
  CHECK_ARG (4, STRING_P);
  string = (ARG_REF (4));
  end = (arg_index_integer (6, ((STRING_LENGTH (string)) + 1)));
  start = (arg_index_integer (5, (end + 1)));
  hl = (HL_ARG (7));
  length = (end - start);
  if ((x + length) > (XW_X_CSIZE (xw)))
    error_bad_range_arg (2);
  string_scan = (STRING_LOC (string, start));
  string_end = (STRING_LOC (string, end));
  index = (XTERM_CHAR_INDEX (xw, x, y));
  char_start = (XTERM_CHAR_LOC (xw, index));
  char_scan = char_start;
  hl_scan = (XTERM_HL_LOC (xw, index));
  while (string_scan < string_end)
    {
      (*char_scan++) = (*string_scan++);
      (*hl_scan++) = hl;
    }
  WITH_CURSOR_PRESERVED
    (xw,
     ((y == (XW_CURSOR_Y (xw))) &&
      ((x <= (XW_CURSOR_X (xw))) && ((XW_CURSOR_X (xw)) < (x + length)))),
     {
       XTERM_DRAW_CHARS (xw, x, y, char_start, length, (XTERM_HL_GC (xw, hl)));
     });
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-CLEAR-RECTANGLE!", Prim_xterm_clear_rectangle, 6, 6, 0)
{
  struct xwindow * xw;
  int start_x, start_y, end_x, end_y;
  int hl;
  int x_length;
  PRIMITIVE_HEADER (6);

  xw = (WINDOW_ARG (1));
  end_x = (arg_index_integer (3, ((XW_X_CSIZE (xw)) + 1)));
  end_y = (arg_index_integer (5, ((XW_Y_CSIZE (xw)) + 1)));
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
	index = (XTERM_CHAR_INDEX (xw, start_x, y));
	char_scan = (XTERM_CHAR_LOC (xw, index));
	char_end = (char_scan + x_length);
	hl_scan = (XTERM_HL_LOC (xw, index));
	while (char_scan < char_end)
	  {
	    (*char_scan++) = ' ';
	    (*hl_scan++) = hl;
	  }
      }
  }
  WITH_CURSOR_PRESERVED
    (xw,
     (((start_x <= (XW_CURSOR_X (xw))) && ((XW_CURSOR_X (xw)) < end_x)) &&
      ((start_y <= (XW_CURSOR_Y (xw))) && ((XW_CURSOR_Y (xw)) < end_y))),
     {
       if (hl == 0)
	 XClearArea ((XW_DISPLAY (xw)), (XW_WINDOW (xw)),
		     (XTERM_X_PIXEL (xw, start_x)),
		     (XTERM_Y_PIXEL (xw, start_y)),
		     ((end_x - start_x) * (FONT_WIDTH (XW_FONT (xw)))),
		     ((end_y - start_y) * (FONT_HEIGHT (XW_FONT (xw)))),
		     False);
       else
	 {
	   fast int y;
	   GC hl_gc;

	   hl_gc = (XTERM_HL_GC (xw, hl));
	   for (y = start_y; (y < end_y) ; (y += 1))
	     XTERM_DRAW_CHARS
	       (xw,
		start_x,
		y,
		(XTERM_CHAR_LOC (xw, (XTERM_CHAR_INDEX (xw, start_x, y)))),
		x_length,
		hl_gc);
	 }
     });
 done:
  PRIMITIVE_RETURN (UNSPECIFIC);
}


DEFINE_PRIMITIVE ("XTERM-READ-CHARS", Prim_xterm_read_chars, 2, 2, 0)
{
  struct xwindow * xw;
  int interval;
  int found_index;
  long time_limit;
  XEvent event;
  extern long OS_real_time_clock ();
  PRIMITIVE_HEADER (2);

  xw = (WINDOW_ARG (1));
  interval =
    (((ARG_REF (2)) == SHARP_F) ? (-1) : (arg_nonnegative_integer (2)));
  if (interval >= 0)
    time_limit = ((OS_real_time_clock ()) + interval);

  x_process_events();
  while (1) {
    if ((XW_CHAR_BUFFER_INDEX (xw) != 0) ||
	(interval == 0)){
      break;
    } else if (interval < 0) {
      x_wait_for_event ();
      break;
    } else if ((OS_real_time_clock ()) >= time_limit) {
      break;
    } else {
      x_process_events();
    }
  }

  /* If we got characters, return them */
  if ((found_index = XW_CHAR_BUFFER_INDEX (xw)) != 0) {
    XW_CHAR_BUFFER_INDEX (xw) = 0;
    PRIMITIVE_RETURN (memory_to_string (found_index,
					XW_CHAR_BUFFER (xw)));
  }
  /* If we're in a read with timeout, and we stopped before the
     timeout was finished, return the amount remaining. */
  if (interval > 0)
    interval = (time_limit - (OS_real_time_clock ()));
  if (interval <= 0)
    PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (long_to_integer (interval));
}

#define min(x,y)	(((x)<(y)) ? (x) : (y))
  
/* This procedure courtesy of Mike Clarkson (mike@ists.ists.ca) */

extern void xterm_dump_contents();

void
xterm_dump_contents (xw, x_start, x_end, y_start, y_end)
     struct xwindow *xw;
     int x_start, x_end, y_start, y_end;
{
  char *character_map = (XW_CHARACTER_MAP (xw));
  char *highlight_map = (XW_HIGHLIGHT_MAP (xw));
  int x_width = (x_end - x_start);
  int xi, yi;

  if (x_end > (XW_X_CSIZE (xw)))
  {
    x_end = (XW_X_CSIZE (xw));
  }
  if (y_end > (XW_Y_CSIZE (xw)))
  {
    y_end = (XW_Y_CSIZE (xw));
  }
  if (x_start < x_end)
  {
    for (yi = y_start; (yi < y_end); yi += 1)
    {
      int index = (XTERM_CHAR_INDEX (xw, 0, yi));
      char * line_char = (& (character_map [index]));
      char * line_hl = (& (highlight_map [index]));
      int xi = x_start;
      while (1)
      {
	int hl = (line_hl [xi]);
	int i = (xi + 1);
	while ((i < x_end) && ((line_hl [i]) == hl))
	{
	  i += 1;
	}
	XTERM_DRAW_CHARS (xw, xi, yi,
			  (& (line_char [xi])), (i - xi),
			  (XTERM_HL_GC (xw, hl)));
	if (i == x_end)
	  break;
	xi = i;
      }
    }
    if ((XW_CURSOR_VISIBLE_P (xw)) &&
	((x_start <= (XW_CURSOR_X (xw))) && ((XW_CURSOR_X (xw)) < x_end)) &&
	((y_start <= (XW_CURSOR_Y (xw))) && ((XW_CURSOR_Y (xw)) < y_end)))
    {
      xterm_draw_cursor (xw);
    }
  }
  return;
}

static XComposeStatus compose_status;

static void
xterm_process_event (exw, event)
     struct xwindow *exw;
     XEvent * event;
{
  switch (event -> type)
    {
    case KeyPress:
      {
	char copy_buffer [80] ;
	int buffer_length;
	int buffer_index;
	char * buffer;
	fast int nbytes;
	fast char * scan_buffer;
	fast char * scan_copy;
	fast char * end_copy;
	KeySym keysym;

	buffer_length = XW_CHAR_BUFFER_LENGTH (exw);
	buffer_index = XW_CHAR_BUFFER_INDEX (exw);
	buffer = XW_CHAR_BUFFER (exw);


	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_KEY_PRESS;
	nbytes =
	  (XLookupString ((& (event -> xkey)),
			  (& (copy_buffer [0])),
			  (sizeof (copy_buffer)),
			  (& keysym),
			  (& compose_status)));
	if (x_debug)
	  {
	    fprintf (stderr, "\nX event: KeyPress, key=%s\n", copy_buffer);
	  }
	if ((IsFunctionKey (keysym)) ||
	    (IsCursorKey (keysym)) ||
	    (IsKeypadKey (keysym)) ||
	    (IsMiscFunctionKey (keysym)))
	  break;
	if (((event -> xkey . state) & Mod1Mask) != 0)
	  (copy_buffer [0]) |= 0x80;
	if (nbytes > (buffer_length - buffer_index))
	  {
	    buffer_length *= 2;
	    buffer = (x_realloc (buffer, buffer_length));
	    XW_CHAR_BUFFER (exw) = buffer;
	    XW_CHAR_BUFFER_LENGTH (exw) = buffer_length;
	  }
	scan_buffer = (buffer + buffer_index);
	scan_copy = (& (copy_buffer [0]));
	end_copy = (scan_copy + nbytes);
	while (scan_copy < end_copy)
	  (*scan_buffer++) = (*scan_copy++);
	XW_CHAR_BUFFER_INDEX (exw) = (scan_buffer - buffer);
	break;
      }
      
    case ConfigureNotify:
      if (x_debug)
      {
	fprintf (stderr, "\nX event: ConfigureNotify\n");
      }
      if (exw != ((struct xwindow *) 0))
      {
	int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (exw)));
	int x_size = (((event -> xconfigure) . width) - extra);
	int y_size = (((event -> xconfigure) . height) - extra);

	if ((x_size != (XW_X_SIZE (exw))) || (y_size != (XW_Y_SIZE (exw))))
	{
	  XFontStruct * font = (XW_FONT (exw));
	  int x_csize = (x_size / (FONT_WIDTH (font)));
	  int y_csize = (y_size / (FONT_HEIGHT (font)));
	  int map_size = (x_csize * y_csize);
	  char * new_char_map;
	  char * new_hl_map;
	  int new_y;
	  fast char * char_scan, * new_char_scan;
	  fast char * char_end;
	  fast char * hl_scan, * new_hl_scan;
	  fast int min_y_csize = min (y_csize, XW_Y_CSIZE(exw));

	  MAKE_MAP (new_char_map, map_size, ' ');
	  MAKE_MAP (new_hl_map, map_size, 0);

	  for (new_y = 0; (new_y < min_y_csize); new_y++ )
	  {
	    char_scan = ((XW_CHARACTER_MAP (exw)) +
			 (new_y * (XW_X_CSIZE (exw))));
	    char_end = (char_scan + min(x_csize, (XW_X_CSIZE (exw))));
	    hl_scan = ((XW_HIGHLIGHT_MAP (exw)) +
		       (new_y * (XW_X_CSIZE (exw))));
	    new_char_scan = new_char_map + (new_y * x_csize);
	    new_hl_scan = new_hl_map + (new_y * x_csize);

	    while (char_scan < char_end)
	    {
	      (*new_char_scan++) = (*char_scan++) ;
	      (*new_hl_scan++) = (*hl_scan++) ;
	    }
	  }

	  (XW_X_SIZE (exw)) = x_size;
	  (XW_Y_SIZE (exw)) = y_size;
	  (XW_X_CSIZE (exw)) = x_csize;
	  (XW_Y_CSIZE (exw)) = y_csize;
	  (XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_RESIZED;
	  (XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_CONFIGURE;
	  free (XW_CHARACTER_MAP (exw));
	  free (XW_HIGHLIGHT_MAP (exw));
	  (XW_CHARACTER_MAP (exw))= new_char_map;
	  (XW_HIGHLIGHT_MAP (exw))= new_hl_map;

	  (void) xterm_dump_contents (exw, 0, 0, x_csize, y_csize);
	  xterm_wm_set_size_hint (exw, 0, 0, 0);
	}
      }
      break;

    case MapNotify:
      if (x_debug) fprintf (stderr, "\nX event: MapNotify\n");
      (XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_MAP;
      (XW_VISIBLE_P (exw)) = 1;
      break;

    case UnmapNotify:
      if (x_debug) fprintf (stderr, "\nX event: UnmapNotify\n");
      if (exw != ((struct xwindow *) 0)) {
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_UNMAP;
	(XW_VISIBLE_P (exw)) = 0;
      }
      break;

    case Expose:
      if (x_debug) fprintf (stderr, "\nX event: Expose\n");
      if (exw != ((struct xwindow *) 0)) {
	xterm_dump_rectangle (exw,
			      ((event -> xexpose) . x),
			      ((event -> xexpose) . y),
			      ((event -> xexpose) . width),
			      ((event -> xexpose) . height));
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_EXPOSE;
      }
      break;

    case GraphicsExpose:
      if (x_debug) fprintf (stderr, "\nX event: GraphicsExpose\n");
      if (exw != ((struct xwindow *) 0)) {
	xterm_dump_rectangle (exw,
			      ((event -> xgraphicsexpose) . x),
			      ((event -> xgraphicsexpose) . y),
			      ((event -> xgraphicsexpose) . width),
			      ((event -> xgraphicsexpose) . height));
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_GRAPHICS_EXPOSE;
      }
      break;

    case ButtonPress:
      {
	int button = (check_button ((event -> xbutton) . button));
	int pointer_x = (XTERM_X_CHARACTER (exw, ((event -> xbutton) . x)));
	int pointer_y = (XTERM_Y_CHARACTER (exw, ((event -> xbutton) . y)));
	if (button == (-1)) break;
	(XW_BUTTON (exw)) = button;
	(XW_POINTER_X (exw)) = pointer_x;
	(XW_POINTER_Y (exw)) = pointer_y;
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_BUTTON_DOWN;
	if (x_debug)
	  fprintf (stderr, "\nX event: ButtonPress: Button=%d, X=%d, Y=%d\n",
		   button, pointer_x, pointer_y);
      }
      break;

    case ButtonRelease:
      {
	int button = (check_button ((event -> xbutton) . button));
	int pointer_x = (XTERM_X_CHARACTER (exw, ((event -> xbutton) . x)));
	int pointer_y = (XTERM_Y_CHARACTER (exw, ((event -> xbutton) . y)));
	if (button == (-1)) break;
	(XW_BUTTON (exw)) = button;
	(XW_POINTER_X (exw)) = pointer_x;
	(XW_POINTER_Y (exw)) = pointer_y;
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_BUTTON_UP;
	if (x_debug)
	  fprintf (stderr, "\nX event: ButtonRelease: Button=%d, X=%d, Y=%d\n",
		   button, pointer_x, pointer_y);
      }
      break;

    case NoExpose:
      if (x_debug) fprintf (stderr, "\nX event: NoExpose\n");
      if (exw != ((struct xwindow *) 0)) {
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_NO_EXPOSE;
      }
      break;

    case EnterNotify:
      if (x_debug) fprintf (stderr, "\nX event: EnterNotify\n");
      if (exw != ((struct xwindow *) 0)) {
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_ENTER;
      }
      break;

    case LeaveNotify:
      if (x_debug) fprintf (stderr, "\nX event: LeaveNotify\n");
      if (exw != ((struct xwindow *) 0)) {
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_LEAVE;
      }
      break;

    case FocusIn:
      if (x_debug) fprintf (stderr, "\nX event: FocusIn\n");
      if (exw != ((struct xwindow *) 0)) {
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_FOCUS_IN;
      }
      break;

    case FocusOut:
      if (x_debug) fprintf (stderr, "\nX event: FocusOut\n");
      if (exw != ((struct xwindow *) 0)) {
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_FOCUS_OUT;
      }
      break;

    case MotionNotify:
      if (x_debug) fprintf (stderr, "\nX event: MotionNotify\n");
      if (exw != ((struct xwindow *) 0)) {
	(XW_EVENT_FLAGS (exw)) |= EVENT_FLAG_MOTION;
      }
      break;

    default:
      if (x_debug) fprintf (stderr, "\nX event: %d", (event -> type));
      break;
    }
  return;
}
