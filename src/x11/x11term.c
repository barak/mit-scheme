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

/* X11 terminal for Edwin. */

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

struct xwindow_term
{
  struct xwindow xw;
  struct xterm_extra extra;
};

#define XW_EXTRA(xw) (& (((struct xwindow_term *) xw) -> extra))

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

void
xterm_erase_cursor (struct xwindow * xw)
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

void
xterm_draw_cursor (struct xwindow * xw)
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
xterm_process_event (struct xwindow * xw, XEvent * event)
{
}

static XSizeHints *
xterm_make_size_hints (XFontStruct * font, unsigned int extra)
{
  XSizeHints * size_hints = (XAllocSizeHints ());
  if (size_hints == 0)
    return (NULL);
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
  XFree (size_hints);
}

static void
xterm_update_normal_hints (struct xwindow * xw)
{
  XSizeHints * hints = (xterm_make_size_hints
			((XW_FONT (xw)),
			 (2 * (XW_INTERNAL_BORDER_WIDTH (xw)))));
  if (hints == NULL)
    return;
  xterm_set_wm_normal_hints (xw, hints);
}

static void
xterm_deallocate (struct xwindow * xw)
{
  free (XW_CHARACTER_MAP (xw));
  free (XW_HIGHLIGHT_MAP (xw));
}

static float
xterm_x_coordinate_map (struct xwindow * xw, unsigned int x)
{
  return (x / (FONT_WIDTH (XW_FONT (xw))));
}

static float
xterm_y_coordinate_map (struct xwindow * xw, unsigned int y)
{
  return (y / (FONT_HEIGHT (XW_FONT (xw))));
}

static void
xterm_copy_map_line (struct xwindow * xw,
		     unsigned int x_start,
		     unsigned int x_end,
		     unsigned int y_from,
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
xterm_dump_contents (struct xwindow * xw,
		     unsigned int x_start,
		     unsigned int x_end,
		     unsigned int y_start,
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

void
xterm_dump_rectangle (struct xwindow * xw,
		      int signed_x,
		      int signed_y,
		      unsigned int width,
		      unsigned int height)
{
  XFontStruct * font = (XW_FONT (xw));
  unsigned int x = ((signed_x < 0) ? 0 : ((unsigned int) signed_x));
  unsigned int y = ((signed_y < 0) ? 0 : ((unsigned int) signed_y));
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

int
xterm_reconfigure (struct xwindow * xw,
		   unsigned int x_csize,
		   unsigned int y_csize)
{
  if ((x_csize != (XW_X_CSIZE (xw))) || (y_csize != (XW_Y_CSIZE (xw))))
    {
      char * new_char_map = (malloc (x_csize * y_csize));
      char * new_hl_map = (malloc (x_csize * y_csize));
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
      if (new_char_map == NULL)	return (1);
      if (new_hl_map == NULL) return (1);
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
  return (0);
}

long
xterm_map_x_coordinate (struct xwindow * xw, int signed_xp)
{
  unsigned int xp = ((signed_xp < 0) ? 0 : ((unsigned int) signed_xp));
  int bx = (xp - (XW_INTERNAL_BORDER_WIDTH (xw)));
  return (((bx < 0) ? 0
	   : (bx >= (XW_X_SIZE (xw))) ? ((XW_X_SIZE (xw)) - 1)
	   : bx)
	  / (FONT_WIDTH (XW_FONT (xw))));
}

long
xterm_map_y_coordinate (struct xwindow * xw, int signed_yp)
{
  unsigned int yp = ((signed_yp < 0) ? 0 : ((unsigned int) signed_yp));
  int by = (yp - (XW_INTERNAL_BORDER_WIDTH (xw)));
  return (((by < 0) ? 0
	   : (by >= (XW_Y_SIZE (xw))) ? ((XW_Y_SIZE (xw)) - 1)
	   : by)
	  / (FONT_HEIGHT (XW_FONT (xw))));
}

unsigned long
xterm_map_x_size (struct xwindow * xw, unsigned int width)
{
  int w = (width - (2 * (XW_INTERNAL_BORDER_WIDTH (xw))));
  return ((w < 0) ? 0 : (w / (FONT_WIDTH (XW_FONT (xw)))));
}

unsigned long
xterm_map_y_size (struct xwindow * xw, unsigned int height)
{
  int h = (height - (2 * (XW_INTERNAL_BORDER_WIDTH (xw))));
  return ((h < 0) ? 0 : (h / (FONT_HEIGHT (XW_FONT (xw)))));
}

struct xwindow *
xterm_open_window (struct xdisplay * xd, char * geometry,
		   const char * resource_name,
		   const char * resource_class,
		   int map_p)
{
  Display * display = (XD_DISPLAY (xd));
  struct drawing_attributes attributes;
  struct xwindow_methods methods;
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
  char * charmap;
  char * hlmap;

  if (resource_name == NULL)
    resource_name = RESOURCE_NAME;
  if (resource_class == NULL)
    resource_class = RESOURCE_CLASS;

  if (0 != x_default_attributes (display, resource_name, resource_class,
				 (&attributes)))
    return (NULL);
  (methods.deallocator) = xterm_deallocate;
  (methods.event_processor) = xterm_process_event;
  (methods.x_coordinate_map) = xterm_x_coordinate_map;
  (methods.y_coordinate_map) = xterm_y_coordinate_map;
  (methods.update_normal_hints) = xterm_update_normal_hints;

  size_hints
    = (xterm_make_size_hints ((attributes.font),
			      (2 * (attributes.internal_border_width))));
  if (size_hints == NULL)
    return (NULL);

  XWMGeometry (display,
	       (DefaultScreen (display)),
	       ((geometry == NULL)
		? (x_get_default (display, resource_name, resource_class,
				  "geometry", "Geometry", 0))
		: geometry),
	       DEFAULT_GEOMETRY,
	       (attributes.border_width),
	       size_hints,
	       (&x_pos), (&y_pos), (&x_size), (&y_size),
	       (& (size_hints->win_gravity)));
  x_csize
    = ((x_size - (size_hints->base_width)) / (size_hints->width_inc));
  y_csize
    = ((y_size - (size_hints->base_height)) / (size_hints->height_inc));

  map_size = (x_csize * y_csize);
  charmap = (malloc (map_size));
  if (charmap == NULL)
    return (NULL);
  hlmap = (malloc (map_size));
  if (hlmap == NULL)
    {
      free (charmap);
      return (NULL);
    }

  window = (XCreateSimpleWindow
	    (display, (RootWindow (display, (DefaultScreen (display)))),
	     x_pos, y_pos, x_size, y_size,
	     (attributes.border_width),
	     (attributes.border_pixel),
	     (attributes.background_pixel)));
  if (window == 0)
    return (NULL);

  xw = (x_make_window
	(xd,
	 window,
	 (x_size - (size_hints->base_width)),
	 (y_size - (size_hints->base_height)),
	 (&attributes),
	 (&methods),
	 (sizeof (struct xwindow_term))));
  (XW_X_CSIZE (xw)) = x_csize;
  (XW_Y_CSIZE (xw)) = y_csize;
  (XW_CURSOR_X (xw)) = 0;
  (XW_CURSOR_Y (xw)) = 0;
  (XW_CURSOR_VISIBLE_P (xw)) = 0;
  (XW_CURSOR_ENABLED_P (xw)) = 1;

  memset (charmap, BLANK_CHAR, map_size);
  (XW_CHARACTER_MAP (xw)) = charmap;
  memset (hlmap, DEFAULT_HL, map_size);
  (XW_HIGHLIGHT_MAP (xw)) = hlmap;

  (size_hints->flags) |= PWinGravity;
  xterm_set_wm_normal_hints (xw, size_hints);
  if ((0 != xw_set_wm_input_hint (xw, 1))
      || (0 != xw_set_wm_name (xw, "scheme-terminal"))
      || (0 != xw_set_wm_icon_name (xw, "scheme-terminal"))
      || (0 != xw_make_window_map (xw, resource_name, resource_class, map_p)))
    {
      x_close_window (xw);
      return (NULL);
    }
  return (xw);
}

unsigned int
xterm_x_size (struct xwindow * xw)
{
  return (XW_X_CSIZE (xw));
}

unsigned int
xterm_y_size (struct xwindow * xw)
{
  return (XW_Y_CSIZE (xw));
}

void
xterm_set_size (struct xwindow * xw, unsigned int width, unsigned int height)
{
  int extra;
  XFontStruct * font;
  extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
#ifdef __APPLE__
  extra += 1;
#endif
  font = (XW_FONT (xw));
  XResizeWindow
    ((XW_DISPLAY (xw)),
     (XW_WINDOW (xw)),
     ((width * (FONT_WIDTH (font))) + extra),
     ((height * (FONT_HEIGHT (font))) + extra));
}

void
xterm_enable_cursor (struct xwindow * xw, int enable_p)
{
  (XW_CURSOR_ENABLED_P (xw)) = enable_p;
}

int
xterm_write_cursor (struct xwindow * xw, unsigned int x, unsigned int y)
{
  if (x >= (XW_X_CSIZE (xw)))
    return (1);
  if (y >= (XW_Y_CSIZE (xw)))
    return (2);
  if ((x != (XW_CURSOR_X (xw))) || (y != (XW_CURSOR_Y (xw))))
    {
      xterm_erase_cursor (xw);
      (XW_CURSOR_X (xw)) = x;
      (XW_CURSOR_Y (xw)) = y;
    }
  xterm_draw_cursor (xw);
  return (0);
}

int
xterm_write_char (struct xwindow * xw, unsigned int x, unsigned int y,
		  int c, unsigned int hl)
{
  unsigned int index;
  char * map_ptr;

  if (x >= (XW_X_CSIZE (xw)))
    return (1);
  if (y >= (XW_Y_CSIZE (xw)))
    return (2);
  if (hl >= 2)
    return (3);
  index = (XTERM_CHAR_INDEX (xw, x, y));
  map_ptr = (XTERM_CHAR_LOC (xw, index));
  (*map_ptr) = c;
  (XTERM_HL (xw, index)) = hl;
  XTERM_DRAW_CHARS (xw, x, y, map_ptr, 1, (XTERM_HL_GC (xw, hl)));
  if (((XW_CURSOR_X (xw)) == x) && ((XW_CURSOR_Y (xw)) == y))
    {
      (XW_CURSOR_VISIBLE_P (xw)) = 0;
      xterm_draw_cursor (xw);
    }
  return (0);
}

int
xterm_write_substring (struct xwindow * xw, unsigned int x, unsigned int y,
		       unsigned char * string,
		       unsigned int start, unsigned int end,
		       unsigned int hl)
{
  unsigned int length, index;

  if (x >= (XW_X_CSIZE (xw)))
    return (1);
  if (y >= (XW_Y_CSIZE (xw)))
    return (2);
  if (start >= (end + 1))
    return (3);
  if (hl >= 2)
    return (4);

  length = (end - start);
  index = (XTERM_CHAR_INDEX (xw, x, y));
  if ((x + length) > (XW_X_CSIZE (xw)))
    return (5);
  {
    unsigned char * string_scan = &string[start];
    unsigned char * string_end = &string[end];
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
  return (0);
}

static void
clear_rectangle (struct xwindow * xw,
		 unsigned int x_start,
		 unsigned int x_end,
		 unsigned int y_start,
		 unsigned int y_end,
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

int
xterm_clear_rectangle (struct xwindow * xw,
		       unsigned int x_start, unsigned int x_end,
		       unsigned int y_start, unsigned int y_end,
		       unsigned int hl)
{
  if (((XW_X_CSIZE (xw)) + 1) <= x_end)
    return (1);
  if (((XW_Y_CSIZE (xw)) + 1) <= y_end)
    return (2);
  if ((x_end + 1) <= x_start)
    return (3);
  if ((y_end + 1) <= y_start)
    return (4);
  if (hl >= 2)
    return (5);
  if ((x_start < x_end) && (y_start < y_end))
    {
      clear_rectangle (xw, x_start, x_end, y_start, y_end, hl);
      if (CURSOR_IN_RECTANGLE (xw, x_start, x_end, y_start, y_end))
	{
	  (XW_CURSOR_VISIBLE_P (xw)) = 0;
	  xterm_draw_cursor (xw);
	}
    }
  return (0);
}

static void
scroll_lines_up (struct xwindow * xw,
		 unsigned int x_start,
		 unsigned int x_end,
		 unsigned int y_start,
		 unsigned int y_end,
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

int
xterm_scroll_lines_up (struct xwindow * xw,
		       unsigned int x_start,
		       unsigned int x_end,
		       unsigned int y_start,
		       unsigned int y_end,
		       unsigned int lines)
{
  if (x_end >= ((XW_X_CSIZE (xw)) + 1))
    return (1);
  if (y_end >= ((XW_Y_CSIZE (xw)) + 1))
    return (2);
  if (x_start >= (x_end + 1))
    return (3);
  if (y_start >= (y_end + 1))
    return (4);
  if (lines >= (y_end - y_start))
    return (5);
  if ((0 < lines) && (x_start < x_end) && (y_start < y_end))
    {
      if (CURSOR_IN_RECTANGLE (xw, x_start, x_end, (y_start + lines), y_end))
	{
	  xterm_erase_cursor (xw);
	  scroll_lines_up (xw, x_start, x_end, y_start, y_end, lines);
	  xterm_draw_cursor (xw);
	}
      else
	{
	  scroll_lines_up (xw, x_start, x_end, y_start, y_end, lines);
	  if (CURSOR_IN_RECTANGLE
	      (xw, x_start, x_end, y_start, (y_end - lines)))
	    {
	      (XW_CURSOR_VISIBLE_P (xw)) = 0;
	      xterm_draw_cursor (xw);
	    }
	}
    }
  return (0);
}

static void
scroll_lines_down (struct xwindow * xw,
		   unsigned int x_start,
		   unsigned int x_end,
		   unsigned int y_start,
		   unsigned int y_end,
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

int
xterm_scroll_lines_down (struct xwindow * xw,
			 unsigned int x_start,
			 unsigned int x_end,
			 unsigned int y_start,
			 unsigned int y_end,
			 unsigned int lines)
{
  if (x_end >= ((XW_X_CSIZE (xw)) + 1))
    return (1);
  if (y_end >= ((XW_Y_CSIZE (xw)) + 1))
    return (2);
  if (x_start >= (x_end + 1))
    return (3);
  if (y_start >= (y_end + 1))
    return (4);
  if (lines >= (y_end - y_start))
    return (5);
  if ((0 < lines) && (x_start < x_end) && (y_start < y_end))
    {
      if (CURSOR_IN_RECTANGLE (xw, x_start, x_end, y_start, (y_end - lines)))
	{
	  xterm_erase_cursor (xw);
	  scroll_lines_down (xw, x_start, x_end, y_start, y_end, lines);
	  xterm_draw_cursor (xw);
	}
      else
	{
	  scroll_lines_down (xw, x_start, x_end, y_start, y_end, lines);
	  if (CURSOR_IN_RECTANGLE
	      (xw, x_start, x_end, (y_start + lines), y_end))
	    {
	      (XW_CURSOR_VISIBLE_P (xw)) = 0;
	      xterm_draw_cursor (xw);
	    }
	}
    }
  return (0);
}

int
xterm_save_contents (struct xwindow * xw,
		     unsigned int x_start,
		     unsigned int x_end,
		     unsigned int y_start,
		     unsigned int y_end,
		     char * contents)
{
  unsigned int x_length;

  if (x_end >= ((XW_X_CSIZE (xw)) + 1))
    return (1);
  if (y_end >= ((XW_Y_CSIZE (xw)) + 1))
    return (2);
  if (x_start >= (x_end + 1))
    return (3);
  if (y_start >= (y_end + 1))
    return (4);
  x_length = (x_end - x_start);

  {
    char * string_scan = contents;
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
  return (0);
}

int
xterm_restore_contents (struct xwindow * xw,
			unsigned int x_start,
			unsigned int x_end,
			unsigned int y_start,
			unsigned int y_end,
			char * contents)
{
  unsigned int x_length;
  unsigned int string_length;

  if (x_end >= ((XW_X_CSIZE (xw)) + 1))
    return (1);
  if (y_end >= ((XW_Y_CSIZE (xw)) + 1))
    return (2);
  if (x_start >= (x_end + 1))
    return (3);
  if (y_start >= (y_end + 1))
    return (4);
  x_length = (x_end - x_start);
  string_length = (2 * x_length * (y_end - y_start));
  if (string_length > 0)
    {
      char * string_scan = contents;
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
  return (0);
}
