/* -*-C-*-

$Id: x11term.c,v 1.32 2006/02/15 05:09:50 cph Exp $

Copyright 1989,1990,1991,1992,1993,1995 Massachusetts Institute of Technology
Copyright 2000,2005,2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* X11 terminal for Edwin. */

#include "scheme.h"
#include "prims.h"
#include "x11.h"

struct xterm_extra
{
  /* Dimensions of the window, in characters.  Valid character
     coordinates are nonnegative integers strictly less than these
     limits. */
  unsigned int x_size;
  unsigned int y_size;

  /* Position of the cursor, in character coordinates. */
  unsigned int cursor_x;
  unsigned int cursor_y;

  /* Character map of the window's contents.  See `XTERM_CHAR_LOC' for
     the address arithmetic. */
  char * character_map;

  /* Bit map of the window's highlighting. */
  char * highlight_map;

  /* Nonzero iff the cursor is drawn on the window. */
  char cursor_visible_p;

  /* Nonzero iff the cursor should be drawn on the window. */
  char cursor_enabled_p;
};

#define XW_EXTRA(xw) ((struct xterm_extra *) ((xw) -> extra))

#define XW_X_CSIZE(xw) ((XW_EXTRA (xw)) -> x_size)
#define XW_Y_CSIZE(xw) ((XW_EXTRA (xw)) -> y_size)
#define XW_CURSOR_X(xw) ((XW_EXTRA (xw)) -> cursor_x)
#define XW_CURSOR_Y(xw) ((XW_EXTRA (xw)) -> cursor_y)
#define XW_CHARACTER_MAP(xw) ((XW_EXTRA (xw)) -> character_map)
#define XW_HIGHLIGHT_MAP(xw) ((XW_EXTRA (xw)) -> highlight_map)
#define XW_CURSOR_VISIBLE_P(xw) ((XW_EXTRA (xw)) -> cursor_visible_p)
#define XW_CURSOR_ENABLED_P(xw) ((XW_EXTRA (xw)) -> cursor_enabled_p)

#define XTERM_CHAR_INDEX(xw, x, y) (((y) * (XW_X_CSIZE (xw))) + (x))
#define XTERM_CHAR_LOC(xw, index) ((XW_CHARACTER_MAP (xw)) + (index))
#define XTERM_CHAR(xw, index) (* (XTERM_CHAR_LOC (xw, index)))
#define XTERM_HL_LOC(xw, index) ((XW_HIGHLIGHT_MAP (xw)) + (index))
#define XTERM_HL(xw, index) (* (XTERM_HL_LOC (xw, index)))

#define XTERM_HL_GC(xw, hl) (hl ? (XW_REVERSE_GC (xw)) : (XW_NORMAL_GC (xw)))

#define HL_ARG(arg) arg_index_integer (arg, 2)

#define RESOURCE_NAME "schemeTerminal"
#define RESOURCE_CLASS "SchemeTerminal"
#define DEFAULT_GEOMETRY "80x40+0+0"
#define BLANK_CHAR ' '
#define DEFAULT_HL 0

#define XTERM_X_PIXEL(xw, x)						\
  (((x) * (FONT_WIDTH (XW_FONT (xw)))) + (XW_INTERNAL_BORDER_WIDTH (xw)))

#define XTERM_Y_PIXEL(xw, y)						\
  (((y) * (FONT_HEIGHT (XW_FONT (xw)))) + (XW_INTERNAL_BORDER_WIDTH (xw)))

#define XTERM_DRAW_CHARS(xw, x, y, s, n, gc)				\
  XDrawImageString							\
    ((XW_DISPLAY (xw)),							\
     (XW_WINDOW (xw)),							\
     gc,								\
     (XTERM_X_PIXEL (xw, x)),						\
     ((XTERM_Y_PIXEL (xw, y)) + (FONT_BASE (XW_FONT (xw)))),		\
     s,									\
     n)

#define CURSOR_IN_RECTANGLE(xw, x_start, x_end, y_start, y_end)		\
  (((x_start) <= (XW_CURSOR_X (xw)))					\
   && ((XW_CURSOR_X (xw)) < (x_end))					\
   && ((y_start) <= (XW_CURSOR_Y (xw)))					\
   && ((XW_CURSOR_Y (xw)) < (y_end)))

static void
DEFUN (xterm_erase_cursor, (xw), struct xwindow * xw)
{
  if (XW_CURSOR_VISIBLE_P (xw))
    {
      unsigned int x = (XW_CURSOR_X (xw));
      unsigned int y = (XW_CURSOR_Y (xw));
      unsigned int index = (XTERM_CHAR_INDEX (xw, x, y));
      XTERM_DRAW_CHARS
	(xw, x, y,
	 (XTERM_CHAR_LOC (xw, index)),
	 1,
	 (XTERM_HL_GC (xw, (XTERM_HL (xw, index)))));
      (XW_CURSOR_VISIBLE_P (xw)) = 0;
    }
}

static void
DEFUN (xterm_draw_cursor, (xw), struct xwindow * xw)
{
  if ((XW_CURSOR_ENABLED_P (xw)) && (! (XW_CURSOR_VISIBLE_P (xw))))
    {
      unsigned int x = (XW_CURSOR_X (xw));
      unsigned int y = (XW_CURSOR_Y (xw));
      unsigned int index = (XTERM_CHAR_INDEX (xw, x, y));
      int hl = (XTERM_HL (xw, index));
      XTERM_DRAW_CHARS
	(xw, x, y,
	 (XTERM_CHAR_LOC (xw, index)),
	 1,
	 ((hl && ((XW_FOREGROUND_PIXEL (xw)) == (XW_CURSOR_PIXEL (xw))))
	  ? (XW_NORMAL_GC (xw))
	  : (XW_CURSOR_GC (xw))));
      (XW_CURSOR_VISIBLE_P (xw)) = 1;
    }
}

static void
DEFUN (xterm_process_event, (xw, event),
       struct xwindow * xw AND
       XEvent * event)
{
}

static XSizeHints *
DEFUN (xterm_make_size_hints, (font, extra),
       XFontStruct * font AND
       unsigned int extra)
{
  XSizeHints * size_hints = (XAllocSizeHints ());
  if (size_hints == 0)
    error_external_return ();
  (size_hints -> flags) = (PResizeInc | PMinSize | PBaseSize);
  (size_hints -> width_inc) = (FONT_WIDTH (font));
  (size_hints -> height_inc) = (FONT_HEIGHT (font));
  (size_hints -> min_width) = extra;
  (size_hints -> min_height) = extra;
  (size_hints -> base_width) = extra;
  (size_hints -> base_height) = extra;
  return (size_hints);
}

static void
xterm_set_wm_normal_hints (struct xwindow * xw, XSizeHints * size_hints)
{
  XSetWMNormalHints ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), size_hints);
  XFree ((void *) size_hints);
}

static void
xterm_update_normal_hints (struct xwindow * xw)
{
  xterm_set_wm_normal_hints
    (xw,
     (xterm_make_size_hints
      ((XW_FONT (xw)),
       (2 * (XW_INTERNAL_BORDER_WIDTH (xw))))));
}

static void
DEFUN (xterm_deallocate, (xw), struct xwindow * xw)
{
  free (XW_CHARACTER_MAP (xw));
  free (XW_HIGHLIGHT_MAP (xw));
}

static SCHEME_OBJECT
DEFUN (xterm_x_coordinate_map, (xw, x), struct xwindow * xw AND unsigned int x)
{
  return (ulong_to_integer (x / (FONT_WIDTH (XW_FONT (xw)))));
}

static SCHEME_OBJECT
DEFUN (xterm_y_coordinate_map, (xw, y), struct xwindow * xw AND unsigned int y)
{
  return (ulong_to_integer (y / (FONT_HEIGHT (XW_FONT (xw)))));
}

static void
DEFUN (xterm_copy_map_line, (xw, x_start, x_end, y_from, y_to),
       struct xwindow * xw AND
       unsigned int x_start AND
       unsigned int x_end AND
       unsigned int y_from AND
       unsigned int y_to)
{
  {
    char * from_scan =
      (XTERM_CHAR_LOC (xw, (XTERM_CHAR_INDEX (xw, x_start, y_from))));
    char * from_end =
      (XTERM_CHAR_LOC (xw, (XTERM_CHAR_INDEX (xw, x_end, y_from))));
    char * to_scan =
      (XTERM_CHAR_LOC (xw, (XTERM_CHAR_INDEX (xw, x_start, y_to))));
    while (from_scan < from_end)
      (*to_scan++) = (*from_scan++);
  }
  {
    char * from_scan =
      (XTERM_HL_LOC (xw, (XTERM_CHAR_INDEX (xw, x_start, y_from))));
    char * from_end =
      (XTERM_HL_LOC (xw, (XTERM_CHAR_INDEX (xw, x_end, y_from))));
    char * to_scan =
      (XTERM_HL_LOC (xw, (XTERM_CHAR_INDEX (xw, x_start, y_to))));
    while (from_scan < from_end)
      (*to_scan++) = (*from_scan++);
  }
}

static void
DEFUN (xterm_dump_contents, (xw, x_start, x_end, y_start, y_end),
       struct xwindow * xw AND
       unsigned int x_start AND
       unsigned int x_end AND
       unsigned int y_start AND
       unsigned int y_end)
{
  char * character_map = (XW_CHARACTER_MAP (xw));
  char * highlight_map = (XW_HIGHLIGHT_MAP (xw));
  if (x_start < x_end)
    {
      unsigned int yi;
      for (yi = y_start; (yi < y_end); yi += 1)
	{
	  unsigned int index = (XTERM_CHAR_INDEX (xw, 0, yi));
	  char * line_char = (&character_map[index]);
	  char * line_hl = (&highlight_map[index]);
	  unsigned int xi = x_start;
	  while (1)
	    {
	      unsigned int hl = (line_hl[xi]);
	      unsigned int xj = (xi + 1);
	      while ((xj < x_end) && ((line_hl[xj]) == hl))
		xj += 1;
	      XTERM_DRAW_CHARS (xw, xi, yi,
				(&line_char[xi]),
				(xj - xi),
				(XTERM_HL_GC (xw, hl)));
	      if (xj == x_end)
		break;
	      xi = xj;
	    }
	}
      if (CURSOR_IN_RECTANGLE (xw, x_start, x_end, y_start, y_end))
	{
	  (XW_CURSOR_VISIBLE_P (xw)) = 0;
	  xterm_draw_cursor (xw);
	}
    }
}

static void
DEFUN (xterm_dump_rectangle, (xw, x, y, width, height),
       struct xwindow * xw AND
       unsigned int x AND
       unsigned int y AND
       unsigned int width AND
       unsigned int height)
{
  XFontStruct * font = (XW_FONT (xw));
  unsigned int fwidth = (FONT_WIDTH (font));
  unsigned int fheight = (FONT_HEIGHT (font));
  unsigned int border = (XW_INTERNAL_BORDER_WIDTH (xw));
  if (x < border)
    {
      width -= (border - x);
      x = 0;
    }
  else
    x -= border;
  if ((x + width) > (XW_X_SIZE (xw)))
    width = ((XW_X_SIZE (xw)) - x);
  if (y < border)
    {
      height -= (border - y);
      y = 0;
    }
  else
    y -= border;
  if ((y + height) > (XW_Y_SIZE (xw)))
    height = ((XW_Y_SIZE (xw)) - y);
  {
    unsigned int x_start = (x / fwidth);
    unsigned int x_end = (((x + width) + (fwidth - 1)) / fwidth);
    unsigned int y_start = (y / fheight);
    unsigned int y_end = (((y + height) + (fheight - 1)) / fheight);
    if (x_end > (XW_X_CSIZE (xw)))
      x_end = (XW_X_CSIZE (xw));
    if (y_end > (XW_Y_CSIZE (xw)))
      y_end = (XW_Y_CSIZE (xw));
    xterm_dump_contents (xw, x_start, x_end, y_start, y_end);
  }
  XFlush (XW_DISPLAY (xw));
}

#define MIN(x, y) (((x) < (y)) ? (x) : (y))

static void
DEFUN (xterm_reconfigure, (xw, width, height),
       struct xwindow * xw AND
       unsigned int x_csize AND
       unsigned int y_csize)
{
  if ((x_csize != (XW_X_CSIZE (xw))) || (y_csize != (XW_Y_CSIZE (xw))))
    {
      char * new_char_map = (x_malloc (x_csize * y_csize));
      char * new_hl_map = (x_malloc (x_csize * y_csize));
      unsigned int old_x_csize = (XW_X_CSIZE (xw));
      unsigned int min_x_csize = (MIN (x_csize, old_x_csize));
      unsigned int min_y_csize = (MIN (y_csize, (XW_Y_CSIZE (xw))));
      int x_clipped = (old_x_csize - x_csize);
      char * new_scan_char = new_char_map;
      char * new_scan_hl = new_hl_map;
      char * new_end;
      char * old_scan_char = (XW_CHARACTER_MAP (xw));
      char * old_scan_hl = (XW_HIGHLIGHT_MAP (xw));
      char * old_end;
      unsigned int new_y = 0;
      for (; (new_y < min_y_csize); new_y += 1)
	{
	  old_end = (old_scan_char + min_x_csize);
	  while (old_scan_char < old_end)
	    {
	      (*new_scan_char++) = (*old_scan_char++);
	      (*new_scan_hl++) = (*old_scan_hl++);
	    }
	  if (x_clipped < 0)
	    {
	      new_end = (new_scan_char + ((unsigned int) (- x_clipped)));
	      while (new_scan_char < new_end)
		{
		  (*new_scan_char++) = BLANK_CHAR;
		  (*new_scan_hl++) = DEFAULT_HL;
		}
	    }
	  else if (x_clipped > 0)
	    {
	      old_scan_char += ((unsigned int) x_clipped);
	      old_scan_hl += ((unsigned int) x_clipped);
	    }
	}
      for (; (new_y < y_csize); new_y += 1)
	{
	  new_end = (new_scan_char + x_csize);
	  while (new_scan_char < new_end)
	    {
	      (*new_scan_char++) = BLANK_CHAR;
	      (*new_scan_hl++) = DEFAULT_HL;
	    }
	}
      free (XW_CHARACTER_MAP (xw));
      free (XW_HIGHLIGHT_MAP (xw));
      {
	unsigned int x_size = (XTERM_X_PIXEL (xw, x_csize));
	unsigned int y_size = (XTERM_Y_PIXEL (xw, x_csize));
	(XW_X_SIZE (xw)) = x_size;
	(XW_Y_SIZE (xw)) = y_size;
	(XW_CLIP_X (xw)) = 0;
	(XW_CLIP_Y (xw)) = 0;
	(XW_CLIP_WIDTH (xw)) = x_size;
	(XW_CLIP_HEIGHT (xw)) = y_size;
      }
      (XW_X_CSIZE (xw)) = x_csize;
      (XW_Y_CSIZE (xw)) = y_csize;
      (XW_CHARACTER_MAP (xw))= new_char_map;
      (XW_HIGHLIGHT_MAP (xw))= new_hl_map;
      XClearWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
      xterm_dump_contents (xw, 0, 0, x_csize, y_csize);
      xterm_update_normal_hints (xw);
      XFlush (XW_DISPLAY (xw));
    }
}

DEFINE_PRIMITIVE ("XTERM-RECONFIGURE", Prim_xterm_reconfigure, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  xterm_reconfigure ((x_window_arg (1)),
		     (arg_ulong_integer (2)),
		     (arg_ulong_integer (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-DUMP-RECTANGLE", Prim_xterm_dump_rectangle, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  xterm_dump_rectangle ((x_window_arg (1)),
			(arg_ulong_integer (2)),
			(arg_ulong_integer (3)),
			(arg_ulong_integer (4)),
			(arg_ulong_integer (5)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-MAP-X-COORDINATE", Prim_xterm_map_x_coordinate, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int xp = (arg_ulong_integer (2));
    int bx = (xp - (XW_INTERNAL_BORDER_WIDTH (xw)));
    PRIMITIVE_RETURN
      (long_to_integer
       (((bx < 0) ? 0
	 : (bx >= (XW_X_SIZE (xw))) ? ((XW_X_SIZE (xw)) - 1)
	 : bx)
	/ (FONT_WIDTH (XW_FONT (xw)))));
  }
}

DEFINE_PRIMITIVE ("XTERM-MAP-Y-COORDINATE", Prim_xterm_map_y_coordinate, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int yp = (arg_ulong_integer (2));
    int by = (yp - (XW_INTERNAL_BORDER_WIDTH (xw)));
    PRIMITIVE_RETURN
      (long_to_integer
       (((by < 0) ? 0
	 : (by >= (XW_Y_SIZE (xw))) ? ((XW_Y_SIZE (xw)) - 1)
	 : by)
	/ (FONT_HEIGHT (XW_FONT (xw)))));
  }
}

DEFINE_PRIMITIVE ("XTERM-MAP-X-SIZE", Prim_xterm_map_x_size, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    int width =
      ((arg_nonnegative_integer (2)) - (2 * (XW_INTERNAL_BORDER_WIDTH (xw))));
    PRIMITIVE_RETURN
      (ulong_to_integer
       ((width < 0) ? 0 : (width / (FONT_WIDTH (XW_FONT (xw))))));
  }
}

DEFINE_PRIMITIVE ("XTERM-MAP-Y-SIZE", Prim_xterm_map_y_size, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    int height =
      ((arg_nonnegative_integer (2)) - (2 * (XW_INTERNAL_BORDER_WIDTH (xw))));
    PRIMITIVE_RETURN
      (ulong_to_integer
       ((height < 0) ? 0 : (height / (FONT_HEIGHT (XW_FONT (xw))))));
  }
}

DEFINE_PRIMITIVE ("XTERM-OPEN-WINDOW", Prim_xterm_open_window, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    struct xdisplay * xd = (x_display_arg (1));
    Display * display = (XD_DISPLAY (xd));
    struct drawing_attributes attributes;
    struct xwindow_methods methods;
    const char * resource_name = RESOURCE_NAME;
    const char * resource_class = RESOURCE_CLASS;
    int map_p;
    XSizeHints * size_hints;
    int x_pos;
    int y_pos;
    int x_size;
    int y_size;
    unsigned int x_csize;
    unsigned int y_csize;
    Window window;
    struct xwindow * xw;
    unsigned int map_size;

    x_decode_window_map_arg
      ((ARG_REF (3)), (&resource_name), (&resource_class), (&map_p));
    x_default_attributes
      (display, resource_name, resource_class, (&attributes));
    (methods.deallocator) = xterm_deallocate;
    (methods.event_processor) = xterm_process_event;
    (methods.x_coordinate_map) = xterm_x_coordinate_map;
    (methods.y_coordinate_map) = xterm_y_coordinate_map;
    (methods.update_normal_hints) = xterm_update_normal_hints;

    size_hints
      = (xterm_make_size_hints ((attributes.font),
				(2 * (attributes.internal_border_width))));
    XWMGeometry (display,
		 (DefaultScreen (display)),
		 (((ARG_REF (2)) == SHARP_F)
		  ? (x_get_default
		     (display, resource_name, resource_class,
		      "geometry", "Geometry", 0))
		  : (STRING_ARG (2))),
		 DEFAULT_GEOMETRY,
		 (attributes.border_width),
		 size_hints,
		 (&x_pos), (&y_pos), (&x_size), (&y_size),
		 (& (size_hints->win_gravity)));
    x_csize
      = ((x_size - (size_hints->base_width)) / (size_hints->width_inc));
    y_csize
      = ((y_size - (size_hints->base_height)) / (size_hints->height_inc));

    window = (XCreateSimpleWindow
	      (display, (RootWindow (display, (DefaultScreen (display)))),
	       x_pos, y_pos, x_size, y_size,
	       (attributes.border_width),
	       (attributes.border_pixel),
	       (attributes.background_pixel)));
    if (window == 0)
      error_external_return ();

    xw = (x_make_window
	  (xd,
	   window,
	   (x_size - (size_hints->base_width)),
	   (y_size - (size_hints->base_height)),
	   (&attributes),
	   (&methods),
	   (sizeof (struct xterm_extra))));
    (XW_X_CSIZE (xw)) = x_csize;
    (XW_Y_CSIZE (xw)) = y_csize;
    (XW_CURSOR_X (xw)) = 0;
    (XW_CURSOR_Y (xw)) = 0;
    (XW_CURSOR_VISIBLE_P (xw)) = 0;
    (XW_CURSOR_ENABLED_P (xw)) = 1;

    map_size = (x_csize * y_csize);
    (XW_CHARACTER_MAP (xw)) = (x_malloc (map_size));
    memset ((XW_CHARACTER_MAP (xw)), BLANK_CHAR, map_size);
    (XW_HIGHLIGHT_MAP (xw)) = (x_malloc (map_size));
    memset ((XW_CHARACTER_MAP (xw)), DEFAULT_HL, map_size);

    (size_hints->flags) |= PWinGravity;
    xterm_set_wm_normal_hints (xw, size_hints);
    xw_set_wm_input_hint (xw, 1);
    xw_set_wm_name (xw, "scheme-terminal");
    xw_set_wm_icon_name (xw, "scheme-terminal");
    xw_make_window_map (xw, resource_name, resource_class, map_p);
    PRIMITIVE_RETURN (XW_TO_OBJECT (xw));
  }
}

DEFINE_PRIMITIVE ("XTERM-X-SIZE", Prim_xterm_x_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (XW_X_CSIZE (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("XTERM-Y-SIZE", Prim_xterm_y_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (XW_Y_CSIZE (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("XTERM-SET-SIZE", Prim_xterm_set_size, 3, 3, 0)
{
  struct xwindow * xw;
  int extra;
  XFontStruct * font;
  PRIMITIVE_HEADER (3);
  xw = (x_window_arg (1));
  extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  font = (XW_FONT (xw));
  XResizeWindow
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     (((arg_ulong_integer (2)) * (FONT_WIDTH (font))) + extra),
     (((arg_ulong_integer (3)) * (FONT_HEIGHT (font))) + extra));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-ENABLE-CURSOR", Prim_xterm_enable_cursor, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  (XW_CURSOR_ENABLED_P (x_window_arg (1))) = (BOOLEAN_ARG (2));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-ERASE-CURSOR", Prim_xterm_erase_cursor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  xterm_erase_cursor (x_window_arg (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-DRAW-CURSOR", Prim_xterm_draw_cursor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  xterm_draw_cursor (x_window_arg (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-WRITE-CURSOR!", Prim_xterm_write_cursor, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int x = (arg_ulong_index_integer (2, (XW_X_CSIZE (xw))));
    unsigned int y = (arg_ulong_index_integer (3, (XW_Y_CSIZE (xw))));
    if ((x != (XW_CURSOR_X (xw))) || (y != (XW_CURSOR_Y (xw))))
      {
	xterm_erase_cursor (xw);
	(XW_CURSOR_X (xw)) = x;
	(XW_CURSOR_Y (xw)) = y;
      }
    xterm_draw_cursor (xw);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-WRITE-CHAR!", Prim_xterm_write_char, 5, 5, 0)
{
  PRIMITIVE_HEADER (5);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int x = (arg_ulong_index_integer (2, (XW_X_CSIZE (xw))));
    unsigned int y = (arg_ulong_index_integer (3, (XW_Y_CSIZE (xw))));
    int c = (arg_ascii_char (4));
    unsigned int hl = (HL_ARG (5));
    unsigned int index = (XTERM_CHAR_INDEX (xw, x, y));
    char * map_ptr = (XTERM_CHAR_LOC (xw, index));
    (*map_ptr) = c;
    (XTERM_HL (xw, index)) = hl;
    XTERM_DRAW_CHARS (xw, x, y, map_ptr, 1, (XTERM_HL_GC (xw, hl)));
    if (((XW_CURSOR_X (xw)) == x) && ((XW_CURSOR_Y (xw)) == y))
      {
	(XW_CURSOR_VISIBLE_P (xw)) = 0;
	xterm_draw_cursor (xw);
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-WRITE-SUBSTRING!", Prim_xterm_write_substring, 7, 7, 0)
{
  PRIMITIVE_HEADER (7);
  CHECK_ARG (4, STRING_P);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int x = (arg_ulong_index_integer (2, (XW_X_CSIZE (xw))));
    unsigned int y = (arg_ulong_index_integer (3, (XW_Y_CSIZE (xw))));
    SCHEME_OBJECT string = (ARG_REF (4));
    unsigned int end
      = (arg_ulong_index_integer (6, ((STRING_LENGTH (string)) + 1)));
    unsigned int start = (arg_ulong_index_integer (5, (end + 1)));
    unsigned int hl = (HL_ARG (7));
    unsigned int length = (end - start);
    unsigned int index = (XTERM_CHAR_INDEX (xw, x, y));
    if ((x + length) > (XW_X_CSIZE (xw)))
      error_bad_range_arg (2);
    {
      unsigned char * string_scan = (STRING_LOC (string, start));
      unsigned char * string_end = (STRING_LOC (string, end));
      char * char_scan = (XTERM_CHAR_LOC (xw, index));
      char * hl_scan = (XTERM_HL_LOC (xw, index));
      while (string_scan < string_end)
	{
	  (*char_scan++) = (*string_scan++);
	  (*hl_scan++) = hl;
	}
    }
    XTERM_DRAW_CHARS
      (xw, x, y, (XTERM_CHAR_LOC (xw, index)), length, (XTERM_HL_GC (xw, hl)));
    if ((x <= (XW_CURSOR_X (xw))) && ((XW_CURSOR_X (xw)) < (x + length))
	&& (y == (XW_CURSOR_Y (xw))))
      {
	(XW_CURSOR_VISIBLE_P (xw)) = 0;
	xterm_draw_cursor (xw);
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
DEFUN (xterm_clear_rectangle, (xw, x_start, x_end, y_start, y_end, hl),
       struct xwindow * xw AND
       unsigned int x_start AND
       unsigned int x_end AND
       unsigned int y_start AND
       unsigned int y_end AND
       unsigned int hl)
{
  unsigned int x_length = (x_end - x_start);
  unsigned int y;
  for (y = y_start; (y < y_end); y += 1)
    {
      unsigned int index = (XTERM_CHAR_INDEX (xw, x_start, y));
      {
	char * scan = (XTERM_CHAR_LOC (xw, index));
	char * end = (scan + x_length);
	while (scan < end)
	  (*scan++) = BLANK_CHAR;
      }
      {
	char * scan = (XTERM_HL_LOC (xw, index));
	char * end = (scan + x_length);
	while (scan < end)
	  (*scan++) = hl;
      }
    }
  if (hl != 0)
    {
      GC hl_gc = (XTERM_HL_GC (xw, hl));
      for (y = y_start; (y < y_end); y += 1)
	XTERM_DRAW_CHARS
	  (xw, x_start, y,
	   (XTERM_CHAR_LOC (xw, (XTERM_CHAR_INDEX (xw, x_start, y)))),
	   x_length, hl_gc);
    }
  else if ((x_start == 0)
	   && (y_start == 0)
	   && (x_end == (XW_X_CSIZE (xw)))
	   && (y_end == (XW_Y_CSIZE (xw))))
    XClearWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
  else
    XClearArea ((XW_DISPLAY (xw)),
		(XW_WINDOW (xw)),
		(XTERM_X_PIXEL (xw, x_start)),
		(XTERM_Y_PIXEL (xw, y_start)),
		(x_length * (FONT_WIDTH (XW_FONT (xw)))),
		((y_end - y_start) * (FONT_HEIGHT (XW_FONT (xw)))),
		False);
}

DEFINE_PRIMITIVE ("XTERM-CLEAR-RECTANGLE!", Prim_xterm_clear_rectangle, 6, 6, 0)
{
  PRIMITIVE_HEADER (6);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int x_end
      = (arg_ulong_index_integer (3, ((XW_X_CSIZE (xw)) + 1)));
    unsigned int y_end
      = (arg_ulong_index_integer (5, ((XW_Y_CSIZE (xw)) + 1)));
    unsigned int x_start = (arg_ulong_index_integer (2, (x_end + 1)));
    unsigned int y_start = (arg_ulong_index_integer (4, (y_end + 1)));
    unsigned int hl = (HL_ARG (6));
    if ((x_start < x_end) && (y_start < y_end))
      {
	xterm_clear_rectangle (xw, x_start, x_end, y_start, y_end, hl);
	if (CURSOR_IN_RECTANGLE (xw, x_start, x_end, y_start, y_end))
	  {
	    (XW_CURSOR_VISIBLE_P (xw)) = 0;
	    xterm_draw_cursor (xw);
	  }
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
DEFUN (xterm_scroll_lines_up, (xw, x_start, x_end, y_start, y_end, lines),
       struct xwindow * xw AND
       unsigned int x_start AND
       unsigned int x_end AND
       unsigned int y_start AND
       unsigned int y_end AND
       unsigned int lines)
{
  {
    unsigned int y_to = y_start;
    unsigned int y_from = (y_to + lines);
    while (y_from < y_end)
      xterm_copy_map_line (xw, x_start, x_end, (y_from++), (y_to++));
  }
  XCopyArea ((XW_DISPLAY (xw)),
	     (XW_WINDOW (xw)),
	     (XW_WINDOW (xw)),
	     (XW_NORMAL_GC (xw)),
	     (XTERM_X_PIXEL (xw, x_start)),
	     (XTERM_Y_PIXEL (xw, (y_start + lines))),
	     ((x_end - x_start) * (FONT_WIDTH (XW_FONT (xw)))),
	     (((y_end - y_start) - lines) * (FONT_HEIGHT (XW_FONT (xw)))),
	     (XTERM_X_PIXEL (xw, x_start)),
	     (XTERM_Y_PIXEL (xw, y_start)));
}

DEFINE_PRIMITIVE ("XTERM-SCROLL-LINES-UP", Prim_xterm_scroll_lines_up, 6, 6,
  "(XTERM-SCROLL-LINES-UP XTERM X-START X-END Y-START Y-END LINES)\n\
Scroll the contents of the region up by LINES.")
{
  PRIMITIVE_HEADER (6);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int x_end
      = (arg_ulong_index_integer (3, ((XW_X_CSIZE (xw)) + 1)));
    unsigned int x_start = (arg_ulong_index_integer (2, (x_end + 1)));
    unsigned int y_end
      = (arg_ulong_index_integer (5, ((XW_Y_CSIZE (xw)) + 1)));
    unsigned int y_start = (arg_ulong_index_integer (4, (y_end + 1)));
    unsigned int lines = (arg_ulong_index_integer (6, (y_end - y_start)));
    if ((0 < lines) && (x_start < x_end) && (y_start < y_end))
      {
	if (CURSOR_IN_RECTANGLE (xw, x_start, x_end, (y_start + lines), y_end))
	  {
	    xterm_erase_cursor (xw);
	    xterm_scroll_lines_up (xw, x_start, x_end, y_start, y_end, lines);
	    xterm_draw_cursor (xw);
	  }
	else
	  {
	    xterm_scroll_lines_up (xw, x_start, x_end, y_start, y_end, lines);
	    if (CURSOR_IN_RECTANGLE
		(xw, x_start, x_end, y_start, (y_end - lines)))
	      {
		(XW_CURSOR_VISIBLE_P (xw)) = 0;
		xterm_draw_cursor (xw);
	      }
	  }
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
DEFUN (xterm_scroll_lines_down, (xw, x_start, x_end, y_start, y_end, lines),
       struct xwindow * xw AND
       unsigned int x_start AND
       unsigned int x_end AND
       unsigned int y_start AND
       unsigned int y_end AND
       unsigned int lines)
{
  {
    unsigned int y_to = y_end;
    unsigned int y_from = (y_to - lines);
    while (y_from > y_start)
      xterm_copy_map_line (xw, x_start, x_end, (--y_from), (--y_to));
  }
  XCopyArea ((XW_DISPLAY (xw)),
	     (XW_WINDOW (xw)),
	     (XW_WINDOW (xw)),
	     (XW_NORMAL_GC (xw)),
	     (XTERM_X_PIXEL (xw, x_start)),
	     (XTERM_Y_PIXEL (xw, y_start)),
	     ((x_end - x_start) * (FONT_WIDTH (XW_FONT (xw)))),
	     (((y_end - y_start) - lines) * (FONT_HEIGHT (XW_FONT (xw)))),
	     (XTERM_X_PIXEL (xw, x_start)),
	     (XTERM_Y_PIXEL (xw, (y_start + lines))));
}

DEFINE_PRIMITIVE ("XTERM-SCROLL-LINES-DOWN", Prim_xterm_scroll_lines_down, 6, 6,
  "(XTERM-SCROLL-LINES-DOWN XTERM X-START X-END Y-START Y-END LINES)\n\
Scroll the contents of the region down by LINES.")
{
  PRIMITIVE_HEADER (6);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int x_end
      = (arg_ulong_index_integer (3, ((XW_X_CSIZE (xw)) + 1)));
    unsigned int x_start = (arg_ulong_index_integer (2, (x_end + 1)));
    unsigned int y_end
      = (arg_ulong_index_integer (5, ((XW_Y_CSIZE (xw)) + 1)));
    unsigned int y_start = (arg_ulong_index_integer (4, (y_end + 1)));
    unsigned int lines = (arg_ulong_index_integer (6, (y_end - y_start)));
    if ((0 < lines) && (x_start < x_end) && (y_start < y_end))
      {
	if (CURSOR_IN_RECTANGLE (xw, x_start, x_end, y_start, (y_end - lines)))
	  {
	    xterm_erase_cursor (xw);
	    xterm_scroll_lines_down
	      (xw, x_start, x_end, y_start, y_end, lines);
	    xterm_draw_cursor (xw);
	  }
	else
	  {
	    xterm_scroll_lines_down
	      (xw, x_start, x_end, y_start, y_end, lines);
	    if (CURSOR_IN_RECTANGLE
		(xw, x_start, x_end, (y_start + lines), y_end))
	      {
		(XW_CURSOR_VISIBLE_P (xw)) = 0;
		xterm_draw_cursor (xw);
	      }
	  }
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("XTERM-SAVE-CONTENTS", Prim_xterm_save_contents, 5, 5,
  "(XTERM-SAVE-CONTENTS XW X-START X-END Y-START Y-END)\n\
Get the contents of the terminal screen rectangle as a string.\n\
The string contains alternating (CHARACTER, HIGHLIGHT) pairs.\n\
The pairs are organized in row-major order from (X-START, Y-START).")
{
  PRIMITIVE_HEADER (5);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int x_end
      = (arg_ulong_index_integer (3, ((XW_X_CSIZE (xw)) + 1)));
    unsigned int y_end
      = (arg_ulong_index_integer (5, ((XW_Y_CSIZE (xw)) + 1)));
    unsigned int x_start = (arg_ulong_index_integer (2, (x_end + 1)));
    unsigned int y_start = (arg_ulong_index_integer (4, (y_end + 1)));
    unsigned int x_length = (x_end - x_start);
    unsigned int string_length = (2 * x_length * (y_end - y_start));
    SCHEME_OBJECT string = (allocate_string (string_length));
    if (string_length > 0)
      {
	char * string_scan = ((char *) (STRING_LOC (string, 0)));
	unsigned int y;
	for (y = y_start; (y < y_end); y += 1)
	  {
	    unsigned int index = (XTERM_CHAR_INDEX (xw, x_start, y));
	    char * char_scan = (XTERM_CHAR_LOC (xw, index));
	    char * char_end = (char_scan + x_length);
	    char * hl_scan = (XTERM_HL_LOC (xw, index));
	    while (char_scan < char_end)
	      {
		(*string_scan++) = (*char_scan++);
		(*string_scan++) = (*hl_scan++);
	      }
	  }
      }
    PRIMITIVE_RETURN (string);
  }
}

DEFINE_PRIMITIVE ("XTERM-RESTORE-CONTENTS", Prim_xterm_restore_contents, 6, 6,
  "(xterm-restore-contents xterm x-start x-end y-start y-end contents)\n\
Replace the terminal screen rectangle with CONTENTS.\n\
See `XTERM-SCREEN-CONTENTS' for the format of CONTENTS.")
{
  PRIMITIVE_HEADER (6);
  CHECK_ARG (6, STRING_P);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int x_end
      = (arg_ulong_index_integer (3, ((XW_X_CSIZE (xw)) + 1)));
    unsigned int y_end
      = (arg_ulong_index_integer (5, ((XW_Y_CSIZE (xw)) + 1)));
    unsigned int x_start = (arg_ulong_index_integer (2, (x_end + 1)));
    unsigned int y_start = (arg_ulong_index_integer (4, (y_end + 1)));
    unsigned int x_length = (x_end - x_start);
    unsigned int string_length = (2 * x_length * (y_end - y_start));
    SCHEME_OBJECT string = (ARG_REF (6));
    if ((STRING_LENGTH (string)) != string_length)
      error_bad_range_arg (6);
    if (string_length > 0)
      {
	char * string_scan = ((char *) (STRING_LOC (string, 0)));
	unsigned int y;
	for (y = y_start; (y < y_end); y += 1)
	  {
	    unsigned int index = (XTERM_CHAR_INDEX (xw, x_start, y));
	    char * char_scan = (XTERM_CHAR_LOC (xw, index));
	    char * char_end = (char_scan + x_length);
	    char * hl_scan = (XTERM_HL_LOC (xw, index));
	    while (char_scan < char_end)
	      {
		(*char_scan++) = (*string_scan++);
		(*hl_scan++) = (*string_scan++);
	      }
	  }
	xterm_dump_contents (xw, x_start, x_end, y_start, y_end);
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}
