/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

struct xdisplay
{
  unsigned int allocation_index;
  Display * display;
  unsigned int server_ping_timer;
  Atom wm_protocols;
  Atom wm_delete_window;
  Atom wm_take_focus;
  XEvent cached_event;
  char cached_event_p;

  /* X key events have 8-bit modifier masks, three bits of which are
     defined to be Shift, Lock, and Control, identified with ShiftMask,
     LockMask, and ControlMask; and five bits of which are unspecified
     named only mod1 to mod5.  Which ones mean Meta, Super, Hyper, &c.,
     vary from system to system, however, so, on initializing the display
     record, we grovel through some tables (XGetKeyboardMapping and
     XGetModifierMapping) to find which ones the various modifier
     keysyms are assigned to, and cache them here.

     Scheme knows about Shift, Control, Meta, Super, and Hyper.  Of
     these, only Meta, Super, and Hyper are identified by numbered
     modifier masks.  All other modifiers are ignored. */
  int modifier_mask_meta;
  int modifier_mask_super;
  int modifier_mask_hyper;

  /* The type of window manager we have.  If we move FRAME_OUTER_WINDOW
     to x/y 0/0, some window managers (type A) puts the window manager
     decorations outside the screen and FRAME_OUTER_WINDOW exactly at 0/0.
     Other window managers (type B) puts the window including decorations
     at 0/0, so FRAME_OUTER_WINDOW is a bit below 0/0.
     Record the type of WM in use so we can compensate for type A WMs.  */
  enum
    {
      X_WMTYPE_UNKNOWN,
      X_WMTYPE_A,
      X_WMTYPE_B
    } wm_type;
};

#define XD_ALLOCATION_INDEX(xd) ((xd) -> allocation_index)
#define XD_DISPLAY(xd) ((xd) -> display)
#define XD_SERVER_PING_TIMER(xd) ((xd) -> server_ping_timer)
#define XD_WM_PROTOCOLS(xd) ((xd) -> wm_protocols)
#define XD_WM_DELETE_WINDOW(xd) ((xd) -> wm_delete_window)
#define XD_WM_TAKE_FOCUS(xd) ((xd) -> wm_take_focus)
#define XD_CACHED_EVENT(xd) ((xd) -> cached_event)
#define XD_CACHED_EVENT_P(xd) ((xd) -> cached_event_p)
#define XD_MODIFIER_MASK_SHIFT(xd) (ShiftMask)
#define XD_MODIFIER_MASK_CONTROL(xd) (ControlMask)
#define XD_MODIFIER_MASK_LOCK(xd) (LockMask)
#define XD_MODIFIER_MASK_META(xd) ((xd) -> modifier_mask_meta)
#define XD_MODIFIER_MASK_SUPER(xd) ((xd) -> modifier_mask_super)
#define XD_MODIFIER_MASK_HYPER(xd) ((xd) -> modifier_mask_hyper)
#define XD_WM_TYPE(xd) ((xd) -> wm_type)
#define XD_TO_OBJECT(xd) (LONG_TO_UNSIGNED_FIXNUM (XD_ALLOCATION_INDEX (xd)))

#define X_MODIFIER_MASK_SHIFT_P(modifier_mask, xd) \
  ((modifier_mask) & (XD_MODIFIER_MASK_SHIFT (xd)))
#define X_MODIFIER_MASK_CONTROL_P(modifier_mask, xd) \
  ((modifier_mask) & (XD_MODIFIER_MASK_CONTROL (xd)))
#define X_MODIFIER_MASK_LOCK_P(modifier_mask, xd) \
  ((modifier_mask) & (XD_MODIFIER_MASK_LOCK (xd)))
#define X_MODIFIER_MASK_META_P(modifier_mask, xd) \
  ((modifier_mask) & (XD_MODIFIER_MASK_META (xd)))
#define X_MODIFIER_MASK_SUPER_P(modifier_mask, xd) \
  ((modifier_mask) & (XD_MODIFIER_MASK_SUPER (xd)))
#define X_MODIFIER_MASK_HYPER_P(modifier_mask, xd) \
  ((modifier_mask) & (XD_MODIFIER_MASK_HYPER (xd)))

extern struct xdisplay * x_display_arg (unsigned int arg);

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

/* This incomplete type definition is needed because the scope of the
   implicit definition in the following typedefs is incorrect.  */
struct xwindow;

typedef void (*x_deallocator_t) (struct xwindow *);
typedef void (*x_event_processor_t) (struct xwindow *, XEvent *);
typedef SCHEME_OBJECT (*x_coordinate_map_t)
  (struct xwindow *, unsigned int);
typedef void (*x_update_normal_hints_t) (struct xwindow *);

struct xwindow_methods
{
  /* Deallocation procedure to do window-specific deallocation.  */
  x_deallocator_t deallocator;

  /* Procedure to call on each received event.  */
  x_event_processor_t event_processor;

  /* Procedures to map coordinates to Scheme objects. */
  x_coordinate_map_t x_coordinate_map;
  x_coordinate_map_t y_coordinate_map;

  /* Procedure that is called to inform the window manager of
     adjustments to the window's internal border or font. */
  x_update_normal_hints_t update_normal_hints;
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

  /* Geometry parameters for window-manager decoration window.  */
  int wm_decor_x;
  int wm_decor_y;
  unsigned int wm_decor_pixel_width;
  unsigned int wm_decor_pixel_height;
  unsigned int wm_decor_border_width;

  /* The latest move we made to the window.  Saved so we can
     compensate for type A WMs (see wm_type above).  */
  int expected_x;
  int expected_y;

  /* Nonzero if we have made a move and need to check if the WM placed
     us at the right position.  */
  int check_expected_move_p;

  /* The offset we need to add to compensate for type A WMs.  */
  int move_offset_x;
  int move_offset_y;

#ifdef __GNUC__
  void * extra [0];
#else
  void * extra [1];
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
#define XW_UPDATE_NORMAL_HINTS(xw) (((xw) -> methods) . update_normal_hints)
#define XW_EVENT_MASK(xw) ((xw) -> event_mask)
#define XW_WM_DECOR_X(xw) ((xw) -> wm_decor_x)
#define XW_WM_DECOR_Y(xw) ((xw) -> wm_decor_y)
#define XW_WM_DECOR_PIXEL_WIDTH(xw) ((xw) -> wm_decor_pixel_width)
#define XW_WM_DECOR_PIXEL_HEIGHT(xw) ((xw) -> wm_decor_pixel_height)
#define XW_WM_DECOR_BORDER_WIDTH(xw) ((xw) -> wm_decor_border_width)
#define XW_EXPECTED_X(xw) ((xw) -> expected_x)
#define XW_EXPECTED_Y(xw) ((xw) -> expected_y)
#define XW_CHECK_EXPECTED_MOVE_P(xw) ((xw) -> check_expected_move_p)
#define XW_MOVE_OFFSET_X(xw) ((xw) -> move_offset_x)
#define XW_MOVE_OFFSET_Y(xw) ((xw) -> move_offset_y)

#define XW_TO_OBJECT(xw) (LONG_TO_UNSIGNED_FIXNUM (XW_ALLOCATION_INDEX (xw)))
#define XW_DISPLAY(xw) (XD_DISPLAY (XW_XD (xw)))
#define XW_WM_TYPE(xw) (XD_WM_TYPE (XW_XD (xw)))

#define FONT_WIDTH(f) (((f) -> max_bounds) . width)
#define FONT_HEIGHT(f) (((f) -> ascent) + ((f) -> descent))
#define FONT_BASE(f) ((f) -> ascent)

extern struct xwindow * x_window_arg (unsigned int arg);

struct ximage
{
  unsigned int allocation_index;
  XImage * image;
};

#define XI_ALLOCATION_INDEX(xi) ((xi) -> allocation_index)
#define XI_IMAGE(xi) ((xi) -> image)
#define X_IMAGE_TO_OBJECT(image)					\
  (LONG_TO_UNSIGNED_FIXNUM (allocate_x_image (image)))

extern struct ximage * x_image_arg (unsigned int arg);
extern unsigned int allocate_x_image (XImage * image);
extern void deallocate_x_image (struct ximage * xi);

struct xvisual
{
  unsigned int allocation_index;
  Visual * visual;
};

#define XV_ALLOCATION_INDEX(xv) ((xv) -> allocation_index)
#define XV_VISUAL(xv) ((xv) -> visual)
#define X_VISUAL_TO_OBJECT(visual)					\
  (LONG_TO_UNSIGNED_FIXNUM (allocate_x_visual (visual)))

extern struct xvisual * x_visual_arg (unsigned int arg);
extern unsigned int allocate_x_visual (Visual * visual);
extern void deallocate_x_visual (struct xvisual * xv);

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

extern struct xcolormap * x_colormap_arg (unsigned int arg);
extern unsigned int allocate_x_colormap
  (Colormap colormap, struct xdisplay * xd);
extern void deallocate_x_colormap (struct xcolormap * xcm);

extern int x_debug;

extern void * x_malloc (unsigned int size);
extern void * x_realloc (void * ptr, unsigned int size);

extern char * x_get_default
  (Display * display,
   const char * resource_name,
   const char * resource_class,
   const char * property_name,
   const char * property_class,
   char * sdefault);

extern void x_default_attributes
  (Display * display,
   const char * resource_name,
   const char * resource_class,
   struct drawing_attributes * attributes);

extern struct xwindow * x_make_window
  (struct xdisplay * xd,
   Window window,
   int x_size,
   int y_size,
   struct drawing_attributes * attributes,
   struct xwindow_methods * methods,
   unsigned int extra);

extern void xw_set_wm_input_hint (struct xwindow * xw, int input_hint);
extern void xw_set_wm_name (struct xwindow * xw, const char * name);
extern void xw_set_wm_icon_name (struct xwindow * xw, const char * name);

extern void x_decode_window_map_arg
  (SCHEME_OBJECT map_arg,
   const char ** resource_class,
   const char ** resource_name,
   int * map_p);

extern void xw_make_window_map
  (struct xwindow * xw,
   const char * resource_name,
   const char * resource_class,
   int map_p);
