/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/x11.h,v 1.10 1991/07/23 08:16:09 cph Exp $

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

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include "ansidecl.h"

struct xdisplay
{
  unsigned int allocation_index;
  Display * display;
  XEvent cached_event;
  char cached_event_p;
};

#define XD_ALLOCATION_INDEX(xd) ((xd) -> allocation_index)
#define XD_DISPLAY(xd) ((xd) -> display)
#define XD_CACHED_EVENT(xd) ((xd) -> cached_event)
#define XD_CACHED_EVENT_P(xd) ((xd) -> cached_event_p)
#define XD_TO_OBJECT(xd) (LONG_TO_UNSIGNED_FIXNUM (XD_ALLOCATION_INDEX (xd)))

extern struct xdisplay * EXFUN (x_display_arg, (unsigned int arg));

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

#ifdef __STDC__
/* This incomplete type definition is needed because the scope of the
   implicit definition in the following typedefs is incorrect.  */
struct xwindow;
#endif

typedef void EXFUN ((*x_deallocator_t), (struct xwindow *));
typedef void EXFUN ((*x_event_processor_t), (struct xwindow *, XEvent *));
typedef SCHEME_OBJECT EXFUN
  ((*x_coordinate_map_t), (struct xwindow *, unsigned int));

struct xwindow_methods
{
  /* Deallocation procedure to do window-specific deallocation.  */
  x_deallocator_t deallocator;

  /* Procedure to call on each received event.  */
  x_event_processor_t event_processor;

  /* Procedures to map coordinates to Scheme objects. */
  x_coordinate_map_t x_coordinate_map;
  x_coordinate_map_t y_coordinate_map;
};

struct xwindow
{
  unsigned int allocation_index;
  Window window;
  struct xdisplay * xd;

  /* Dimensions of the drawing region in pixels. */
  unsigned int x_size;
  unsigned int y_size;

  /* The clip rectangle. */
  unsigned int clip_x;
  unsigned int clip_y;
  unsigned int clip_width;
  unsigned int clip_height;

  struct drawing_attributes attributes;

  /* Standard graphics contexts. */
  GC normal_gc;
  GC reverse_gc;
  GC cursor_gc;

  /* The mouse cursor. */
  Cursor mouse_cursor;

  struct xwindow_methods methods;

  unsigned long event_mask;

#ifdef __GNUC__
  PTR extra [0];
#else
  PTR extra [1];
#endif
};

#define XW_ALLOCATION_INDEX(xw) ((xw) -> allocation_index)
#define XW_XD(xw) ((xw) -> xd)
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
#define XW_DEALLOCATOR(xw) (((xw) -> methods) . deallocator)
#define XW_EVENT_PROCESSOR(xw) (((xw) -> methods) . event_processor)
#define XW_X_COORDINATE_MAP(xw) (((xw) -> methods) . x_coordinate_map)
#define XW_Y_COORDINATE_MAP(xw) (((xw) -> methods) . y_coordinate_map)
#define XW_EVENT_MASK(xw) ((xw) -> event_mask)

#define XW_TO_OBJECT(xw) (LONG_TO_UNSIGNED_FIXNUM (XW_ALLOCATION_INDEX (xw)))
#define XW_DISPLAY(xw) (XD_DISPLAY (XW_XD (xw)))

#define FONT_WIDTH(f)	(((f) -> max_bounds) . width)
#define FONT_HEIGHT(f)	(((f) -> ascent) + ((f) -> descent))
#define FONT_BASE(f)    ((f) -> ascent)

extern struct xwindow * EXFUN (x_window_arg, (unsigned int arg));

struct ximage
{
  unsigned int allocation_index;
  XImage * image;
};

#define XI_ALLOCATION_INDEX(xi) ((xi) -> allocation_index)
#define XI_IMAGE(xi) ((xi) -> image)
#define X_IMAGE_TO_OBJECT(image)					\
  (LONG_TO_UNSIGNED_FIXNUM (allocate_x_image (image)))

extern struct ximage * EXFUN (x_image_arg, (unsigned int arg));
extern unsigned int EXFUN (allocate_x_image, (XImage * image));
extern void EXFUN (deallocate_x_image, (struct ximage * xi));

struct xvisual
{
  unsigned int allocation_index;
  Visual * visual;
};

#define XV_ALLOCATION_INDEX(xv) ((xv) -> allocation_index)
#define XV_VISUAL(xv) ((xv) -> visual)
#define X_VISUAL_TO_OBJECT(visual)					\
  (LONG_TO_UNSIGNED_FIXNUM (allocate_x_visual (visual)))

extern struct xvisual * EXFUN (x_visual_arg, (unsigned int arg));
extern unsigned int EXFUN (allocate_x_visual, (Visual * visual));
extern void EXFUN (deallocate_x_visual, (struct xvisual * xv));

struct xcolormap
{
  unsigned int allocation_index;
  Colormap colormap;
  struct xdisplay * xd;
};

#define XCM_ALLOCATION_INDEX(xcm) ((xcm) -> allocation_index)
#define XCM_COLORMAP(xcm) ((xcm) -> colormap)
#define XCM_XD(xcm) ((xcm) -> xd)
#define X_COLORMAP_TO_OBJECT(colormap, xd)				\
  (LONG_TO_UNSIGNED_FIXNUM (allocate_x_colormap ((colormap), (xd))))
#define XCM_DISPLAY(xcm) (XD_DISPLAY (XCM_XD (xcm)))

extern struct xcolormap * EXFUN (x_colormap_arg, (unsigned int arg));
extern unsigned int EXFUN
  (allocate_x_colormap, (Colormap colormap, struct xdisplay * xd));
extern void EXFUN (deallocate_x_colormap, (struct xcolormap * xcm));

extern int x_debug;

extern PTR EXFUN (x_malloc, (unsigned int size));
extern PTR EXFUN (x_realloc, (PTR ptr, unsigned int size));

extern char * EXFUN
  (x_get_default,
   (Display * display,
    char * resource_name,
    char * property_name,
    char * class_name,
    char * sdefault));

extern void EXFUN
  (x_default_attributes,
   (Display * display,
    char * resource_name,
    struct drawing_attributes * attributes));

extern struct xwindow * EXFUN
  (x_make_window,
   (struct xdisplay * xd,
    Window window,
    int x_size,
    int y_size,
    struct drawing_attributes * attributes,
    struct xwindow_methods * methods,
    unsigned int extra));
