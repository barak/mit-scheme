/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11.h,v 1.7 1990/08/16 19:23:35 cph Exp $

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

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include "ansidecl.h"

struct allocation_table
{
  char ** items;
  int length;
};

struct drawing_attributes
{
  /* Width of the borders, in pixels. */
  int border_width;
  int internal_border_width;

  /* The primary font. */
  XFontStruct * font;

  /* Standard pixel values. */
  unsigned long background_pixel;
  unsigned long foreground_pixel;
  unsigned long border_pixel;
  unsigned long cursor_pixel;
  unsigned long mouse_pixel;
};

struct event_queue_element
{
  XEvent event;
  struct event_queue_element * next;
};

struct event_queue
{
  struct event_queue_element * head;
  struct event_queue_element * tail;
};

struct xwindow
{
  Display * display;
  Window window;

  /* Dimensions of the drawing region in pixels. */
  int x_size;
  int y_size;

  /* The clip rectangle. */
  int clip_x;
  int clip_y;
  int clip_width;
  int clip_height;

  struct drawing_attributes attributes;

  /* Standard graphics contexts. */
  GC normal_gc;
  GC reverse_gc;
  GC cursor_gc;

  /* The mouse cursor. */
  Cursor mouse_cursor;

  /* Event queue for this window. */
  struct event_queue events;

  /* Flags that can be set by event handlers. */
  int event_flags;

  /* Additional window-specific data. */
  char * extra;

  /* Deallocation procedure to do window-specific deallocation. */
  void (* deallocator) ();

  /* Procedure to call on each received event (called with the
     xwindow and the event) */
  void (* event_proc) ();

  /* Nonzero iff this window is mapped. */
  char visible_p;
};

extern struct allocation_table x_display_table;
extern struct allocation_table x_window_table;
extern int x_debug;

extern int x_allocate_table_index ();
extern char * x_allocation_item_arg ();
extern int x_allocation_index_arg ();
extern PTR EXFUN (x_malloc, (unsigned int size));
extern PTR EXFUN (x_realloc, (PTR ptr, unsigned int size));
extern unsigned long x_decode_color ();
extern char * x_get_default ();
extern unsigned long x_default_color ();
extern void x_set_mouse_colors ();
extern void x_default_attributes ();
extern struct xwindow * x_make_window ();
extern SCHEME_OBJECT x_window_to_object ();
extern struct xwindow * x_window_to_xw ();
extern Display * x_close_window ();
extern void x_close_display ();
extern void xw_enqueue_event ();
extern int xw_dequeue_event ();
extern int x_distribute_events ();
extern void xw_wait_for_window_event ();
extern int check_button ();
extern int x_process_events ();
extern int x_wait_for_event ();

#define DISPLAY_ARG(arg)						\
  ((Display *) (x_allocation_item_arg (arg, (& x_display_table))))

#define WINDOW_ARG(arg)							\
  ((struct xwindow *) (x_allocation_item_arg (arg, (& x_window_table))))

#define XW_DISPLAY(xw) ((xw) -> display)
#define XW_WINDOW(xw) ((xw) -> window)
#define XW_X_SIZE(xw) ((xw) -> x_size)
#define XW_Y_SIZE(xw) ((xw) -> y_size)
#define XW_CLIP_X(xw) ((xw) -> clip_x)
#define XW_CLIP_Y(xw) ((xw) -> clip_y)
#define XW_CLIP_WIDTH(xw) ((xw) -> clip_width)
#define XW_CLIP_HEIGHT(xw) ((xw) -> clip_height)
#define XW_BORDER_WIDTH(xw) (((xw) -> attributes) . border_width)
#define XW_INTERNAL_BORDER_WIDTH(xw)					\
  (((xw) -> attributes) . internal_border_width)
#define XW_FONT(xw) (((xw) -> attributes) . font)
#define XW_BACKGROUND_PIXEL(xw) (((xw) -> attributes) . background_pixel)
#define XW_FOREGROUND_PIXEL(xw) (((xw) -> attributes) . foreground_pixel)
#define XW_BORDER_PIXEL(xw) (((xw) -> attributes) . border_pixel)
#define XW_CURSOR_PIXEL(xw) (((xw) -> attributes) . cursor_pixel)
#define XW_MOUSE_PIXEL(xw) (((xw) -> attributes) . mouse_pixel)
#define XW_NORMAL_GC(xw) ((xw) -> normal_gc)
#define XW_REVERSE_GC(xw) ((xw) -> reverse_gc)
#define XW_CURSOR_GC(xw) ((xw) -> cursor_gc)
#define XW_MOUSE_CURSOR(xw) ((xw) -> mouse_cursor)
#define XW_EVENT_FLAGS(xw) ((xw) -> event_flags)
#define XW_VISIBLE_P(xw) ((xw) -> visible_p)

#define FONT_WIDTH(f)	(((f) -> max_bounds) . width)
#define FONT_HEIGHT(f)	(((f) -> ascent) + ((f) -> descent))
#define FONT_BASE(f)    ((f) -> ascent)

#define XTERM_X_PIXEL(xw, x)						\
  (((x) * (FONT_WIDTH (XW_FONT (xw)))) + (XW_INTERNAL_BORDER_WIDTH (xw)))

#define XTERM_Y_PIXEL(xw, y)						\
  (((y) * (FONT_HEIGHT (XW_FONT (xw)))) + (XW_INTERNAL_BORDER_WIDTH (xw)))

#define XTERM_X_CHARACTER(xw, x)					\
  (((x) - (XW_INTERNAL_BORDER_WIDTH (xw))) / (FONT_WIDTH (XW_FONT (xw))))

#define XTERM_Y_CHARACTER(xw, y)					\
  (((y) - (XW_INTERNAL_BORDER_WIDTH (xw))) / (FONT_HEIGHT (XW_FONT (xw))))

#define EVENT_TYPE_UNKNOWN		0
#define EVENT_TYPE_RESIZED		1
#define EVENT_TYPE_BUTTON_DOWN		2
#define EVENT_TYPE_BUTTON_UP		3
#define EVENT_TYPE_FOCUS_IN		4
#define EVENT_TYPE_FOCUS_OUT		5
#define EVENT_TYPE_ENTER		6
#define EVENT_TYPE_LEAVE		7
#define EVENT_TYPE_MOTION		8
#define EVENT_TYPE_CONFIGURE		9
#define EVENT_TYPE_MAP			10
#define EVENT_TYPE_UNMAP		11
#define EVENT_TYPE_EXPOSE		12
#define EVENT_TYPE_NO_EXPOSE     	13
#define EVENT_TYPE_GRAPHICS_EXPOSE   	14
#define EVENT_TYPE_KEY_PRESS	   	15

#define EVENT_FLAG_RESIZED		(1 << (EVENT_TYPE_RESIZED - 1))
#define EVENT_FLAG_BUTTON_DOWN		(1 << (EVENT_TYPE_BUTTON_DOWN - 1))
#define EVENT_FLAG_BUTTON_UP		(1 << (EVENT_TYPE_BUTTON_UP - 1))
#define EVENT_FLAG_FOCUS_IN		(1 << (EVENT_TYPE_FOCUS_IN - 1))
#define EVENT_FLAG_FOCUS_OUT		(1 << (EVENT_TYPE_FOCUS_OUT - 1))
#define EVENT_FLAG_ENTER		(1 << (EVENT_TYPE_ENTER - 1))
#define EVENT_FLAG_LEAVE		(1 << (EVENT_TYPE_LEAVE - 1))
#define EVENT_FLAG_MOTION		(1 << (EVENT_TYPE_MOTION - 1))
#define EVENT_FLAG_CONFIGURE		(1 << (EVENT_TYPE_CONFIGURE - 1))
#define EVENT_FLAG_MAP			(1 << (EVENT_TYPE_MAP - 1))
#define EVENT_FLAG_UNMAP		(1 << (EVENT_TYPE_UNMAP - 1))
#define EVENT_FLAG_EXPOSE		(1 << (EVENT_TYPE_EXPOSE - 1))
#define EVENT_FLAG_NO_EXPOSE		(1 << (EVENT_TYPE_NO_EXPOSE - 1))
#define EVENT_FLAG_GRAPHICS_EXPOSE	(1 << (EVENT_TYPE_GRAPHICS_EXPOSE - 1))
#define EVENT_FLAG_KEY_PRESS		(1 << (EVENT_TYPE_KEY_PRESS - 1))

#define BITS_PER_INT 	32	/* this should be somewhere else */

#define SET_X_SELECT_MASK(fd)						\
{									\
  (x_select_mask) [fd / BITS_PER_INT] |= (1 << (fd % BITS_PER_INT));	\
}
