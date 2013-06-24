/* -*-C-*-

$Id: x11base.c,v 1.96 2007/07/15 22:03:15 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* Common X11 support. */

#include "scheme.h"
#include "prims.h"
#include "ux.h"
#include "uxselect.h"
#include "osio.h"
#include "x11.h"
#include <X11/Xmd.h>
#include <X11/keysym.h>

extern void block_signals (void);
extern void unblock_signals (void);

#ifndef X_DEFAULT_FONT
#  define X_DEFAULT_FONT "fixed"
#endif

int x_debug = 0;
static int initialization_done = 0;
static const char * x_default_font = 0;

#define INITIALIZE_ONCE()						\
{									\
  if (!initialization_done)						\
    initialize_once ();							\
}

static void initialize_once (void);

static void move_window (struct xwindow *, int, int);
static void check_expected_move (struct xwindow *);

void *
x_malloc (unsigned int size)
{
  void * result = (UX_malloc (size));
  if (result == 0)
    error_external_return ();
  return (result);
}

void *
x_realloc (void * ptr, unsigned int size)
{
  void * result = (UX_realloc (ptr, size));
  if (result == 0)
    error_external_return ();
  return (result);
}

/* Allocation Tables */

struct allocation_table
{
  void ** items;
  int length;
};

static struct allocation_table x_display_table;
static struct allocation_table x_window_table;
static struct allocation_table x_image_table;
static struct allocation_table x_visual_table;
static struct allocation_table x_colormap_table;

static void
allocation_table_initialize (struct allocation_table * table)
{
  (table->length) = 0;
}

static unsigned int
allocate_table_index (struct allocation_table * table, void * item)
{
  unsigned int length = (table->length);
  unsigned int new_length;
  void ** items = (table->items);
  void ** new_items;
  void ** scan;
  void ** end;
  if (length == 0)
    {
      new_length = 4;
      new_items = (x_malloc ((sizeof (void *)) * new_length));
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
      new_items = (x_realloc (items, ((sizeof (void *)) * new_length)));
    }
  scan = (new_items + length);
  end = (new_items + new_length);
  (*scan++) = item;
  while (scan < end)
    (*scan++) = 0;
  (table->items) = new_items;
  (table->length) = new_length;
  return (length);
}

static void *
allocation_item_arg (unsigned int arg, struct allocation_table * table)
{
  unsigned int index = (arg_index_integer (arg, (table->length)));
  void * item = ((table->items) [index]);
  if (item == 0)
    error_bad_range_arg (arg);
  return (item);
}

struct xdisplay *
x_display_arg (unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_display_table)));
}

struct xwindow *
x_window_arg (unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_window_table)));
}

static struct xwindow *
x_window_to_xw (Display * display, Window window)
{
  struct xwindow ** scan = ((struct xwindow **) (x_window_table.items));
  struct xwindow ** end = (scan + (x_window_table.length));
  while (scan < end)
    {
      struct xwindow * xw = (*scan++);
      if ((xw != 0)
	  && ((XW_DISPLAY (xw)) == display)
	  && ((XW_WINDOW (xw)) == window))
	return (xw);
    }
  return (0);
}

struct ximage *
x_image_arg (unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_image_table)));
}

unsigned int
allocate_x_image (XImage * image)
{
  struct ximage * xi = (x_malloc (sizeof (struct ximage)));
  unsigned int index = (allocate_table_index ((&x_image_table), xi));
  (XI_ALLOCATION_INDEX (xi)) = index;
  (XI_IMAGE (xi)) = image;
  return (index);
}

void
deallocate_x_image (struct ximage * xi)
{
  ((x_image_table.items) [XI_ALLOCATION_INDEX (xi)]) = 0;
  free (xi);
}

struct xvisual *
x_visual_arg (unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_visual_table)));
}

unsigned int
allocate_x_visual (Visual * visual)
{
  struct xvisual * xv = (x_malloc (sizeof (struct xvisual)));
  unsigned int index = (allocate_table_index ((&x_visual_table), xv));
  (XV_ALLOCATION_INDEX (xv)) = index;
  (XV_VISUAL (xv)) = visual;
  return (index);
}

void
deallocate_x_visual (struct xvisual * xv)
{
  ((x_visual_table.items) [XV_ALLOCATION_INDEX (xv)]) = 0;
  free (xv);
}

struct xcolormap *
x_colormap_arg (unsigned int arg)
{
  INITIALIZE_ONCE ();
  return (allocation_item_arg (arg, (&x_colormap_table)));
}

unsigned int
allocate_x_colormap (Colormap colormap, struct xdisplay * xd)
{
  struct xcolormap * xcm = (x_malloc (sizeof (struct xcolormap)));
  unsigned int index = (allocate_table_index ((&x_colormap_table), xcm));
  (XCM_ALLOCATION_INDEX (xcm)) = index;
  (XCM_COLORMAP (xcm)) = colormap;
  (XCM_XD (xcm)) = xd;
  return (index);
}

void
deallocate_x_colormap (struct xcolormap * xcm)
{
  ((x_colormap_table.items) [XCM_ALLOCATION_INDEX (xcm)]) = 0;
  free (xcm);
}

/* Error Handlers */

static int
x_io_error_handler (Display * display)
{
  fprintf (stderr, "\nX IO Error\n");
  fflush (stderr);
  termination_eof ();
  return (0);
}

typedef struct
{
  char message [2048];
  char terminate_p;
  unsigned char code;
} x_error_info_t;

static x_error_info_t x_error_info;

static int
x_error_handler (Display * display, XErrorEvent * error_event)
{
  (x_error_info.code) = (error_event->error_code);
  XGetErrorText (display,
		 (error_event->error_code),
		 (x_error_info.message),
		 (sizeof (x_error_info.message)));
  if (x_error_info.terminate_p)
    {
      fprintf (stderr, "\nX Error: %s\n", (x_error_info.message));
      fprintf (stderr, "         Request code: %d\n",
	       (error_event->request_code));
      fprintf (stderr, "         Error serial: %lx\n", (error_event->serial));
      fflush (stderr);
      termination_eof ();
    }
  return (0);
}

static void
unbind_x_error_info (void * storage)
{
  x_error_info = (* ((x_error_info_t *) storage));
}

static void *
push_x_error_info (Display * display)
{
  void * handle;
  x_error_info_t * storage;

  XSync (display, False);
  handle = dstack_position;
  storage = (dstack_alloc (sizeof (x_error_info_t)));
  (*storage) = x_error_info;
  ((x_error_info.message) [0]) = '\0';
  (x_error_info.terminate_p) = 0;
  (x_error_info.code) = 0;
  dstack_protect (unbind_x_error_info, storage);
  return (handle);
}

static void
pop_x_error_info (void * handle)
{
  dstack_set_position (handle);
}

static unsigned char
x_error_code (Display * display)
{
  XSync (display, False);
  return (x_error_info.code);
}

static int
any_x_errors_p (Display * display)
{
  return ((x_error_code (display)) != 0);
}

/* Defaults and Attributes */

static int
x_decode_color (Display * display,
		Colormap color_map,
		char * color_name,
		unsigned long * color_return)
{
  XColor cdef;
  if ((XParseColor (display, color_map, color_name, (&cdef)))
      && (XAllocColor (display, color_map, (&cdef))))
    {
      (*color_return) = (cdef.pixel);
      return (1);
    }
  return (0);
}

Colormap
xw_color_map (struct xwindow * xw)
{
  XWindowAttributes a;
  if (! (XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&a))))
    error_external_return ();
  return (a.colormap);
}

static unsigned long
arg_window_color (unsigned int arg, Display * display, struct xwindow * xw)
{
  unsigned long result;
  SCHEME_OBJECT object = (ARG_REF (arg));
  if (INTEGER_P (object))
    {
      if (! (integer_to_ulong_p (object)))
	error_bad_range_arg (arg);
      result = (integer_to_ulong (object));
    }
  else if (! (x_decode_color
	      (display, (xw_color_map (xw)), (STRING_ARG (arg)), (&result))))
    error_bad_range_arg (arg);
  return (result);
}

static void
x_set_mouse_colors (Display * display,
		    Colormap color_map,
		    Cursor mouse_cursor,
		    unsigned long mouse_pixel,
		    unsigned long background_pixel)
{
  XColor mouse_color;
  XColor background_color;
  (mouse_color.pixel) = mouse_pixel;
  XQueryColor (display, color_map, (&mouse_color));
  (background_color.pixel) = background_pixel;
  XQueryColor (display, color_map, (&background_color));
  XRecolorCursor (display, mouse_cursor, (&mouse_color), (&background_color));
}

char *
x_get_default (Display * display,
	       const char * resource_name,
	       const char * resource_class,
	       const char * property_name,
	       const char * property_class,
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
x_default_color (Display * display,
		 const char * resource_name,
		 const char * resource_class,
		 const char * property_name,
		 const char * property_class,
		 unsigned long default_color)
{
  char * color_name
    = (x_get_default (display, resource_name, resource_class,
		      property_name, property_class, 0));
  unsigned long result;
  return
    (((color_name != 0)
      && (x_decode_color (display,
			  (DefaultColormap (display,
					    (DefaultScreen (display)))),
			  color_name,
			  (&result))))
     ? result
     : default_color);
}

void
x_default_attributes (Display * display,
		      const char * resource_name,
		      const char * resource_class,
		      struct drawing_attributes * attributes)
{
  int screen_number = (DefaultScreen (display));
  (attributes->font)
    = (XLoadQueryFont (display,
		       ((x_default_font != 0)
			? x_default_font
			: (x_get_default (display,
					  resource_name, resource_class,
					  "font", "Font",
					  X_DEFAULT_FONT)))));
  if ((attributes->font) == 0)
    error_external_return ();
  {
    char * s
      = (x_get_default (display,
			resource_name, resource_class,
			"borderWidth", "BorderWidth",
			0));
    (attributes->border_width) = ((s == 0) ? 0 : (atoi (s)));
  }
  {
    char * s
      = (x_get_default (display,
			resource_name, resource_class,
			"internalBorder", "BorderWidth",
			0));
    (attributes->internal_border_width)
      = ((s == 0) ? (attributes->border_width) : (atoi (s)));
  }
  {
    unsigned long white_pixel = (WhitePixel (display, screen_number));
    unsigned long black_pixel = (BlackPixel (display, screen_number));
    unsigned long foreground_pixel;
    (attributes->background_pixel)
      = (x_default_color (display,
			  resource_name, resource_class,
			  "background", "Background",
			  white_pixel));
    foreground_pixel
      = (x_default_color (display,
			  resource_name, resource_class,
			  "foreground", "Foreground",
			  black_pixel));
    (attributes->foreground_pixel) = foreground_pixel;
    (attributes->border_pixel)
      = (x_default_color (display,
			  resource_name, resource_class,
			  "borderColor", "BorderColor",
			  foreground_pixel));
    (attributes->cursor_pixel)
      = (x_default_color (display,
			  resource_name, resource_class,
			  "cursorColor", "Foreground",
			  foreground_pixel));
    (attributes->mouse_pixel)
      = (x_default_color (display,
			  resource_name, resource_class,
			  "pointerColor", "Foreground",
			  foreground_pixel));
  }
}

static int
get_wm_decor_geometry (struct xwindow * xw)
{
  Display * display = (XW_DISPLAY (xw));
  Window decor = (XW_WINDOW (xw));
  void * handle = (push_x_error_info (display));
  Window root;
  unsigned int depth;

  {
    Window parent;
    Window * children;
    unsigned int n_children;
    while (1)
      {
	if ((!XQueryTree (display, decor,
			  (&root), (&parent), (&children), (&n_children)))
	    || (any_x_errors_p (display)))
	  {
	    pop_x_error_info (handle);
	    error_external_return ();
	  }
	if (children != 0)
	  XFree (children);
	if (parent == root)
	  break;
	decor = parent;
      }
  }
  if ((!XGetGeometry (display,
		      decor,
		      (&root),
		      (& (XW_WM_DECOR_X (xw))),
		      (& (XW_WM_DECOR_Y (xw))),
		      (& (XW_WM_DECOR_PIXEL_WIDTH (xw))),
		      (& (XW_WM_DECOR_PIXEL_HEIGHT (xw))),
		      (& (XW_WM_DECOR_BORDER_WIDTH (xw))),
		      (&depth)))
      || (any_x_errors_p (display)))
    {
      pop_x_error_info (handle);
      error_external_return ();
    }
  pop_x_error_info (handle);
  /* Return true iff the window has been reparented by the WM.  */
  return (decor != (XW_WINDOW (xw)));
}

/* Open/Close Windows */

#define MAKE_GC(gc, fore, back)						\
{									\
  XGCValues gcv;							\
  (gcv.font) = fid;							\
  (gcv.foreground) = (fore);						\
  (gcv.background) = (back);						\
  (gc) =								\
    (XCreateGC (display,						\
		window,							\
		(GCFont | GCForeground | GCBackground),			\
		(& gcv)));						\
}

struct xwindow *
x_make_window (struct xdisplay * xd,
	       Window window,
	       int x_size,
	       int y_size,
	       struct drawing_attributes * attributes,
	       struct xwindow_methods * methods,
	       unsigned int extra)
{
  GC normal_gc;
  GC reverse_gc;
  GC cursor_gc;
  struct xwindow * xw;
  Display * display = (XD_DISPLAY (xd));
  Font fid = ((attributes->font) -> fid);
  unsigned long foreground_pixel = (attributes->foreground_pixel);
  unsigned long background_pixel = (attributes->background_pixel);
  Cursor mouse_cursor = (XCreateFontCursor (display, XC_left_ptr));
  MAKE_GC (normal_gc, foreground_pixel, background_pixel);
  MAKE_GC (reverse_gc, background_pixel, foreground_pixel);
  MAKE_GC (cursor_gc, background_pixel, (attributes->cursor_pixel));
  x_set_mouse_colors
    (display,
     (DefaultColormap (display, (DefaultScreen (display)))),
     mouse_cursor,
     (attributes->mouse_pixel),
     background_pixel);
  XDefineCursor (display, window, mouse_cursor);
  XSelectInput (display, window, 0);
  xw
    = (x_malloc (((sizeof (struct xwindow)) - (sizeof (xw->extra)))
		 + extra));
  (XW_ALLOCATION_INDEX (xw)) = (allocate_table_index ((&x_window_table), xw));
  (XW_XD (xw)) = xd;
  (XW_WINDOW (xw)) = window;
  (XW_X_SIZE (xw)) = x_size;
  (XW_Y_SIZE (xw)) = y_size;
  (XW_CLIP_X (xw)) = 0;
  (XW_CLIP_Y (xw)) = 0;
  (XW_CLIP_WIDTH (xw)) = x_size;
  (XW_CLIP_HEIGHT (xw)) = y_size;
  (xw->attributes) = (*attributes);
  (xw->methods) = (*methods);
  (XW_NORMAL_GC (xw)) = normal_gc;
  (XW_REVERSE_GC (xw)) = reverse_gc;
  (XW_CURSOR_GC (xw)) = cursor_gc;
  (XW_MOUSE_CURSOR (xw)) = mouse_cursor;
  (XW_EVENT_MASK (xw)) = 0;
  (XW_CHECK_EXPECTED_MOVE_P (xw)) = 0;
  (XW_MOVE_OFFSET_X (xw)) = 0;
  (XW_MOVE_OFFSET_Y (xw)) = 0;
  return (xw);
}

static jmp_buf x_close_window_jmp_buf;

static int
x_close_window_io_error (Display * display)
{
  longjmp (x_close_window_jmp_buf, 1);
  /*NOTREACHED*/
  return (0);
}

static void
x_close_window (struct xwindow * xw)
{
  Display * display = (XW_DISPLAY (xw));
  ((x_window_table.items) [XW_ALLOCATION_INDEX (xw)]) = 0;
  if ((setjmp (x_close_window_jmp_buf)) == 0)
    {
      XSetIOErrorHandler (x_close_window_io_error);
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
      /* Guarantee that the IO error occurs while the IO error handler
	 is rebound, if at all. */
      XFlush (display);
    }
  XSetIOErrorHandler (x_io_error_handler);
  free (xw);
}

/* Initialize/Close Displays */

#define MODIFIER_INDEX_TO_MASK(N) (1 << (N))

/* Grovel through the X server's keycode and modifier mappings to find
   out what we ought to interpret as Meta, Hyper, and Super, based on
   what modifiers are associated with keycodes that are associated with
   keysyms Meta_L, Meta_R, Alt_L, Alt_R, Hyper_L, &c.

   Adapted from GNU Emacs. */

static void
x_initialize_display_modifier_masks (struct xdisplay * xd)
{
  int min_keycode;
  int max_keycode;
  XModifierKeymap * modifier_keymap;
  KeyCode * modifier_to_keycodes_table;
  int keycodes_per_modifier;
  KeySym * keycode_to_keysyms_table;
  int keysyms_per_keycode;

  (XD_MODIFIER_MASK_META (xd)) = 0;
  (XD_MODIFIER_MASK_SUPER (xd)) = 0;
  (XD_MODIFIER_MASK_HYPER (xd)) = 0;

  modifier_keymap = (XGetModifierMapping ((XD_DISPLAY (xd))));
  modifier_to_keycodes_table = (modifier_keymap->modifiermap);
  keycodes_per_modifier = (modifier_keymap->max_keypermod);

  XDisplayKeycodes ((XD_DISPLAY (xd)), (& min_keycode), (& max_keycode));

  keycode_to_keysyms_table
    = (XGetKeyboardMapping ((XD_DISPLAY (xd)),
			    min_keycode,
			    (max_keycode - min_keycode + 1),
			    (& keysyms_per_keycode)));

  /* Go through each of the 8 non-preassigned modifiers, which start at
     3 (Mod1), after Shift, Control, and Lock.  For each modifier, go
     through all of the (non-zero) keycodes attached to it; for each
     keycode, go through all of the keysyms attached to it; check each
     keysym for the modifiers that we're interested in (Meta, Hyper,
     and Super). */

  {
    int modifier_index;

    for (modifier_index = 3; (modifier_index < 8); modifier_index += 1)
      {
        int modifier_mask = (MODIFIER_INDEX_TO_MASK (modifier_index));
        KeyCode * keycodes
	  = (& (modifier_to_keycodes_table
		[modifier_index * keycodes_per_modifier]));

        /* This is a flag specifying whether the modifier has already
           been identified as Meta, which takes precedence over Hyper
           and Super.  (What about precedence between Hyper and
           Super...?  This is GNU Emacs's behaviour.) */
        int modifier_is_meta_p = 0;

        int keycode_index;

        for (keycode_index = 0;
             (keycode_index < keycodes_per_modifier);
             keycode_index += 1)
          {
            KeyCode keycode = (keycodes [keycode_index]);

            if (keycode == 0)
              continue;

            {
              int keysym_index;
              KeySym * keysyms
		= (& (keycode_to_keysyms_table
		      [(keycode - min_keycode) * keysyms_per_keycode]));

              for (keysym_index = 0;
                   (keysym_index < keysyms_per_keycode);
                   keysym_index += 1)
                switch (keysyms [keysym_index])
                  {
                  case XK_Meta_L:
                  case XK_Meta_R:
                  case XK_Alt_L:
                  case XK_Alt_R:
                    modifier_is_meta_p = 1;
                    (XD_MODIFIER_MASK_META (xd)) |= modifier_mask;
                    break;

                  case XK_Hyper_L:
                  case XK_Hyper_R:
                    if (! modifier_is_meta_p)
                      (XD_MODIFIER_MASK_HYPER (xd)) |= modifier_mask;
                    goto next_modifier;

                  case XK_Super_L:
                  case XK_Super_R:
                    if (! modifier_is_meta_p)
                      (XD_MODIFIER_MASK_SUPER (xd)) |= modifier_mask;
                    goto next_modifier;
                  }
            }
          }

      next_modifier:
        continue;
      }
  }

  XFree (((char *) keycode_to_keysyms_table));
  XFreeModifiermap (modifier_keymap);
}

static void
x_close_display (struct xdisplay * xd)
{
  struct xwindow ** scan = ((struct xwindow **) (x_window_table.items));
  struct xwindow ** end = (scan + (x_window_table.length));
  while (scan < end)
    {
      struct xwindow * xw = (*scan++);
      if ((xw != 0) && ((XW_XD (xw)) == xd))
	x_close_window (xw);
    }
  ((x_display_table.items) [XD_ALLOCATION_INDEX (xd)]) = 0;
  XCloseDisplay (XD_DISPLAY (xd));
}

static void
x_close_all_displays (void)
{
  struct xdisplay ** scan = ((struct xdisplay **) (x_display_table.items));
  struct xdisplay ** end = (scan + (x_display_table.length));
  while (scan < end)
    {
      struct xdisplay * xd = (*scan++);
      if (xd != 0)
	x_close_display (xd);
    }
}

/* Window Manager Properties */

static void
xw_set_class_hint (struct xwindow * xw, const char * name, const char * class)
{
  XClassHint * class_hint = (XAllocClassHint ());
  if (class_hint == 0)
    error_external_return ();
  /* This structure is misdeclared, so cast the args. */
  (class_hint->res_name) = ((char *) name);
  (class_hint->res_class) = ((char *) class);
  XSetClassHint ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), class_hint);
  XFree (class_hint);
}

void
xw_set_wm_input_hint (struct xwindow * xw, int input_hint)
{
  XWMHints * hints = (XAllocWMHints ());
  if (hints == 0)
    error_external_return ();
  (hints->flags) = InputHint;
  (hints->input) = (input_hint != 0);
  XSetWMHints ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), hints);
  XFree (hints);
}

void
xw_set_wm_name (struct xwindow * xw, const char * name)
{
  XTextProperty property;
  if ((XStringListToTextProperty (((char **) (&name)), 1, (&property))) == 0)
    error_external_return ();
  XSetWMName ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&property));
}

void
xw_set_wm_icon_name (struct xwindow * xw, const char * name)
{
  XTextProperty property;
  if ((XStringListToTextProperty (((char **) (&name)), 1, (&property))) == 0)
    error_external_return ();
  XSetWMIconName ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&property));
}

void
x_decode_window_map_arg (SCHEME_OBJECT map_arg,
			 const char ** resource_name,
			 const char ** resource_class,
			 int * map_p)
{
  (*map_p) = 0;
  if (map_arg == SHARP_F)
    (*map_p) = 1;
  else if ((PAIR_P (map_arg))
	   && (STRING_P (PAIR_CAR (map_arg)))
	   && (STRING_P (PAIR_CDR (map_arg))))
    {
      (*resource_name) = (STRING_POINTER (PAIR_CAR (map_arg)));
      (*resource_class) = (STRING_POINTER (PAIR_CDR (map_arg)));
      (*map_p) = 1;
    }
  else if ((VECTOR_P (map_arg))
	   && ((VECTOR_LENGTH (map_arg)) == 3)
	   && (BOOLEAN_P (VECTOR_REF (map_arg, 0)))
	   && (STRING_P (VECTOR_REF (map_arg, 1)))
	   && (STRING_P (VECTOR_REF (map_arg, 2))))
    {
      (*resource_name) = (STRING_POINTER (VECTOR_REF (map_arg, 1)));
      (*resource_class) = (STRING_POINTER (VECTOR_REF (map_arg, 2)));
      (*map_p) = (OBJECT_TO_BOOLEAN (VECTOR_REF (map_arg, 0)));
    }
}

void
xw_make_window_map (struct xwindow * xw,
		    const char * resource_name,
		    const char * resource_class,
		    int map_p)
{
  xw_set_class_hint (xw, resource_name, resource_class);
  if (map_p)
    {
      XMapWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
      XFlush (XW_DISPLAY (xw));
    }
}

/* Event Processing */

/* Returns non-zero value if caller should ignore the event.  */

static int
xw_process_event (struct xwindow * xw, XEvent * event)
{
  if (x_debug > 0)
    {
      char * type_name;
      fprintf (stderr, "\nX event on 0x%lx: ", ((event->xany) . window));
      switch (event->type)
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
	case SelectionClear:	type_name = "SelectionClear"; break;
	case SelectionRequest:	type_name = "SelectionRequest"; break;
	case UnmapNotify:	type_name = "UnmapNotify"; break;

	case VisibilityNotify:
	  fprintf (stderr, "VisibilityNotify; state=");
	  switch ((event->xvisibility) . state)
	    {
	    case VisibilityUnobscured:
	      fprintf (stderr, "unobscured");
	      break;
	    case VisibilityPartiallyObscured:
	      fprintf (stderr, "partially-obscured");
	      break;
	    case VisibilityFullyObscured:
	      fprintf (stderr, "fully-obscured");
	      break;
	    default:
	      fprintf (stderr, "%d", ((event->xvisibility) . state));
	      break;
	    }
	  goto debug_done;

	case ConfigureNotify:
	  fprintf (stderr, "ConfigureNotify; x=%d y=%d width=%d height=%d",
		   ((event->xconfigure) . x),
		   ((event->xconfigure) . y),
		   ((event->xconfigure) . width),
		   ((event->xconfigure) . height));
	  goto debug_done;

	case ClientMessage:
	  {
	    struct xdisplay * xd = (XW_XD (xw));
	    if ((((event->xclient) . message_type) == (XD_WM_PROTOCOLS (xd)))
		&& (((event->xclient) . format) == 32))
	      {
		if (((Atom) (((event->xclient) . data . l) [0]))
		    == (XD_WM_DELETE_WINDOW (xd)))
		  type_name = "WM_DELETE_WINDOW";
		else if (((Atom) (((event->xclient) . data . l) [0]))
			 == (XD_WM_TAKE_FOCUS (xd)))
		  type_name = "WM_TAKE_FOCUS";
		else
		  type_name = "WM_PROTOCOLS";
	      }
	    else
	      {
		fprintf (stderr, "ClientMessage; message_type=0x%x format=%d",
			 ((unsigned int) ((event->xclient) . message_type)),
			 ((event->xclient) . format));
		goto debug_done;
	      }
	  }
	  break;
	case PropertyNotify:
	  {
	    fprintf (stderr, "PropertyNotify; atom=%ld time=%ld state=%d",
		     ((event->xproperty) . atom),
		     ((event->xproperty) . time),
		     ((event->xproperty) . state));
	    goto debug_done;
	  }
	case SelectionNotify:
	  {
	    fprintf
	      (stderr, "SelectionNotify; sel=%ld targ=%ld prop=%ld t=%ld",
	       ((event->xselection) . selection),
	       ((event->xselection) . target),
	       ((event->xselection) . property),
	       ((event->xselection) . time));
	    goto debug_done;
	  }
	default:		type_name = 0; break;
	}
      if (type_name != 0)
	fprintf (stderr, "%s", type_name);
      else
	fprintf (stderr, "%d", (event->type));
    debug_done:
      fprintf (stderr, "%s\n",
	       (((event->xany) . send_event) ? "; synthetic" : ""));
      fflush (stderr);
    }
  switch (event->type)
    {
    case MappingNotify:
      switch ((event->xmapping) . request)
	{
	case MappingModifier:
	  x_initialize_display_modifier_masks ((XW_XD (xw)));
	  /* Fall through. */
	case MappingKeyboard:
	  XRefreshKeyboardMapping (& (event->xmapping));
	  break;
	}
      break;
    }
  if (xw != 0)
    {
      switch (event->type)
	{
	case ReparentNotify:
	  get_wm_decor_geometry (xw);
	  /* Perhaps reparented due to a WM restart.  Reset this.  */
	  (XW_WM_TYPE (xw)) = X_WMTYPE_UNKNOWN;
	  break;

	case ConfigureNotify:
	  /* If the window has been reparented, don't check
	     non-synthetic events.  */
	  if ((XW_CHECK_EXPECTED_MOVE_P (xw))
	      && (! ((get_wm_decor_geometry (xw))
		     && (! ((event->xconfigure) . send_event)))))
	    check_expected_move (xw);
	  break;
	}
      (* (XW_EVENT_PROCESSOR (xw))) (xw, event);
    }
  return (0);
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
  event_type_visibility,
  event_type_selection_clear,
  event_type_selection_notify,
  event_type_selection_request,
  event_type_property_notify,
  event_type_supremum
};

#define EVENT_MASK_ARG(arg)						\
  (arg_ulong_index_integer						\
   ((arg), (1 << ((unsigned int) event_type_supremum))))

#define EVENT_ENABLED(xw, type)						\
  (((xw) == 0)								\
   || (((XW_EVENT_MASK (xw)) & (1 << ((unsigned int) (type)))) != 0))

#define EVENT_0 2
#define EVENT_1 3
#define EVENT_2 4
#define EVENT_3 5
#define EVENT_4 6

#define EVENT_INTEGER(event, slot, number)				\
  VECTOR_SET ((event), (slot), (long_to_integer (number)))

#define EVENT_ULONG_INTEGER(event, slot, number)			\
  VECTOR_SET ((event), (slot), (ulong_to_integer (number)))

static SCHEME_OBJECT
make_event_object (struct xwindow * xw,
		   enum event_type type,
		   unsigned int extra)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, (2 + extra), 1));
  VECTOR_SET (result, 0, (LONG_TO_UNSIGNED_FIXNUM ((long) type)));
  VECTOR_SET (result, 1, ((xw == 0) ? SHARP_F : (XW_TO_OBJECT (xw))));
  return (result);
}

/* This handles only the modifier bits that Scheme supports.
   At the moment, these are Control, Meta, Super, and Hyper.
   This might want to change if the character abstraction were ever to
   change, or if the X11 interface were to be changed to use something
   other than Scheme characters to convey key presses. */

static unsigned long
x_modifier_mask_to_bucky_bits (unsigned int mask, struct xdisplay * xd)
{
  unsigned long bucky = 0;
  if (X_MODIFIER_MASK_CONTROL_P (mask, xd)) bucky |= CHAR_BITS_CONTROL;
  if (X_MODIFIER_MASK_META_P    (mask, xd)) bucky |= CHAR_BITS_META;
  if (X_MODIFIER_MASK_SUPER_P   (mask, xd)) bucky |= CHAR_BITS_SUPER;
  if (X_MODIFIER_MASK_HYPER_P   (mask, xd)) bucky |= CHAR_BITS_HYPER;
  return (bucky);
}

/* I'm not sure why we have a function for this. */

static SCHEME_OBJECT
x_key_button_mask_to_scheme (unsigned int x_state)
{
  unsigned long scheme_state = 0;
  if (x_state & ControlMask) scheme_state |= 0x0001;
  if (x_state & Mod1Mask)    scheme_state |= 0x0002;
  if (x_state & Mod2Mask)    scheme_state |= 0x0004;
  if (x_state & Mod3Mask)    scheme_state |= 0x0008;
  if (x_state & ShiftMask)   scheme_state |= 0x0010;
  if (x_state & LockMask)    scheme_state |= 0x0020;
  if (x_state & Mod4Mask)    scheme_state |= 0x0040;
  if (x_state & Mod5Mask)    scheme_state |= 0x0080;
  if (x_state & Button1Mask) scheme_state |= 0x0100;
  if (x_state & Button2Mask) scheme_state |= 0x0200;
  if (x_state & Button3Mask) scheme_state |= 0x0400;
  if (x_state & Button4Mask) scheme_state |= 0x0800;
  if (x_state & Button5Mask) scheme_state |= 0x1000;
  return (ULONG_TO_FIXNUM (scheme_state));
}

static SCHEME_OBJECT
button_event (struct xwindow * xw, XButtonEvent * event, enum event_type type)
{
  SCHEME_OBJECT result = (make_event_object (xw, type, 4));
  EVENT_INTEGER (result, EVENT_0, (event->x));
  EVENT_INTEGER (result, EVENT_1, (event->y));
  VECTOR_SET
    (result, EVENT_2,
     ((((event->button) >= 1) && ((event->button) <= 256))
      ? (ULONG_TO_FIXNUM
	 (((event->button) - 1)
	  | ((x_modifier_mask_to_bucky_bits ((event->state), (XW_XD (xw))))
	     << 8)))
      : SHARP_F));
  EVENT_ULONG_INTEGER (result, EVENT_3, (event->time));
  return (result);
}

static XComposeStatus compose_status;

static SCHEME_OBJECT
key_event (struct xwindow * xw, XKeyEvent * event, enum event_type type)
{
  char copy_buffer [80];
  KeySym keysym;
  int nbytes;
  SCHEME_OBJECT result;

  /* Make ShiftLock modifier not affect keys with other modifiers. */
  if ((event->state)
      & (ShiftMask || ControlMask
	 || Mod1Mask || Mod2Mask || Mod3Mask || Mod4Mask || Mod5Mask))
    {
      if (((event->state) & LockMask) != 0)
	(event->state) &=~ LockMask;
    }
  nbytes
    = (XLookupString (event,
		      copy_buffer,
		      (sizeof (copy_buffer)),
		      (&keysym),
		      (&compose_status)));
  if (keysym == NoSymbol)
    return (SHARP_F);
  /* If the BackSpace keysym is received, and XLookupString has
     translated it into ASCII backspace, substitute ASCII DEL
     instead.  */
  if ((keysym == XK_BackSpace)
      && (nbytes == 1)
      && ((copy_buffer[0]) == '\b'))
    (copy_buffer[0]) = '\177';
  if (IsModifierKey (keysym))
    return (SHARP_F);

  result = (make_event_object (xw, type, 4));
  VECTOR_SET (result, EVENT_0,
	      (memory_to_string (nbytes, ((unsigned char *) copy_buffer))));
  /* Create Scheme bucky bits (kept independent of the character).
     X has already controlified, so Scheme may choose to ignore
     the control bucky bit.  */
  VECTOR_SET (result, EVENT_1,
	      (ULONG_TO_FIXNUM
	       (x_modifier_mask_to_bucky_bits ((event->state),
					       (XW_XD (xw))))));
  VECTOR_SET (result, EVENT_2, (ulong_to_integer (keysym)));
  EVENT_ULONG_INTEGER (result, EVENT_3, (event->time));
  return (result);
}

#define CONVERT_TRIVIAL_EVENT(scheme_name)				\
  if (EVENT_ENABLED (xw, scheme_name))					\
    result = (make_event_object (xw, scheme_name, 0));			\
  break

static SCHEME_OBJECT
x_event_to_object (XEvent * event)
{
  struct xwindow * xw
    = (x_window_to_xw (((event->xany) . display),
		       ((event->xany) . window)));
  SCHEME_OBJECT result = SHARP_F;
  switch (event->type)
    {
    case KeyPress:
      if (EVENT_ENABLED (xw, event_type_key_press))
	result = (key_event (xw, (& (event->xkey)), event_type_key_press));
      break;
    case ButtonPress:
      if (EVENT_ENABLED (xw, event_type_button_down))
	result
	  = (button_event (xw, (& (event->xbutton)), event_type_button_down));
      break;
    case ButtonRelease:
      if (EVENT_ENABLED (xw, event_type_button_up))
	result
	  = (button_event (xw, (& (event->xbutton)), event_type_button_up));
      break;
    case MotionNotify:
      if (EVENT_ENABLED (xw, event_type_motion))
	{
	  result = (make_event_object (xw, event_type_motion, 3));
	  EVENT_INTEGER (result, EVENT_0, ((event->xmotion) . x));
	  EVENT_INTEGER (result, EVENT_1, ((event->xmotion) . y));
	  VECTOR_SET (result, EVENT_2,
                      (x_key_button_mask_to_scheme
                       (((event->xmotion) . state))));
	}
      break;
    case ConfigureNotify:
      if (EVENT_ENABLED (xw, event_type_configure))
	{
	  result = (make_event_object (xw, event_type_configure, 2));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_0, ((event->xconfigure) . width));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_1, ((event->xconfigure) . height));
	}
      break;
    case Expose:
      if (EVENT_ENABLED (xw, event_type_expose))
	{
	  result = (make_event_object (xw, event_type_expose, 5));
	  EVENT_INTEGER (result, EVENT_0, ((event->xexpose) . x));
	  EVENT_INTEGER (result, EVENT_1, ((event->xexpose) . y));
	  EVENT_ULONG_INTEGER (result, EVENT_2, ((event->xexpose) . width));
	  EVENT_ULONG_INTEGER (result, EVENT_3, ((event->xexpose) . height));
	  VECTOR_SET (result, EVENT_4, (LONG_TO_UNSIGNED_FIXNUM (0)));
	}
      break;
    case GraphicsExpose:
      if (EVENT_ENABLED (xw, event_type_expose))
	{
	  result = (make_event_object (xw, event_type_expose, 5));
	  EVENT_INTEGER (result, EVENT_0, ((event->xgraphicsexpose) . x));
	  EVENT_INTEGER (result, EVENT_1, ((event->xgraphicsexpose) . y));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_2, ((event->xgraphicsexpose) . width));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_3, ((event->xgraphicsexpose) . height));
	  VECTOR_SET (result, EVENT_4, (LONG_TO_UNSIGNED_FIXNUM (1)));
	}
      break;
    case ClientMessage:
      {
	struct xdisplay * xd = (XW_XD (xw));
	if ((((event->xclient) . message_type) == (XD_WM_PROTOCOLS (xd)))
	    && (((event->xclient) . format) == 32))
	  {
	    if (((Atom) (((event->xclient) . data . l) [0]))
		== (XD_WM_DELETE_WINDOW (xd)))
	      {
		if (EVENT_ENABLED (xw, event_type_delete_window))
		  result
		    = (make_event_object (xw, event_type_delete_window, 0));
	      }
	    else if (((Atom) (((event->xclient) . data . l) [0]))
		     == (XD_WM_TAKE_FOCUS (xd)))
	      {
		if (EVENT_ENABLED (xw, event_type_take_focus))
		  {
		    result
		      = (make_event_object (xw, event_type_take_focus, 1));
		    EVENT_INTEGER
		      (result, EVENT_0, (((event->xclient) . data . l) [1]));
		  }
	      }
	  }
      }
      break;
    case VisibilityNotify:
      if (EVENT_ENABLED (xw, event_type_visibility))
	{
	  unsigned int state;
	  switch ((event->xvisibility) . state)
	    {
	    case VisibilityUnobscured:
	      state = 0;
	      break;
	    case VisibilityPartiallyObscured:
	      state = 1;
	      break;
	    case VisibilityFullyObscured:
	      state = 2;
	      break;
	    default:
	      state = 3;
	      break;
	    }
	  result = (make_event_object (xw, event_type_visibility, 1));
	  EVENT_ULONG_INTEGER (result, EVENT_0, state);
	}
      break;
    case SelectionClear:
      if (EVENT_ENABLED (xw, event_type_selection_clear))
	{
	  result = (make_event_object (xw, event_type_selection_clear, 2));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_0, ((event->xselectionclear) . selection));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_1, ((event->xselectionclear) . time));
	}
      break;
    case SelectionNotify:
      if (EVENT_ENABLED (xw, event_type_selection_notify))
	{
	  result = (make_event_object (xw, event_type_selection_notify, 5));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_0, ((event->xselection) . requestor));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_1, ((event->xselection) . selection));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_2, ((event->xselection) . target));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_3, ((event->xselection) . property));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_4, ((event->xselection) . time));
	}
      break;
    case SelectionRequest:
      if (EVENT_ENABLED (xw, event_type_selection_request))
	{
	  result = (make_event_object (xw, event_type_selection_request, 5));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_0, ((event->xselectionrequest) . requestor));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_1, ((event->xselectionrequest) . selection));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_2, ((event->xselectionrequest) . target));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_3, ((event->xselectionrequest) . property));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_4, ((event->xselectionrequest) . time));
	}
      break;
    case PropertyNotify:
      if (EVENT_ENABLED (xw, event_type_property_notify))
	{
	  result = (make_event_object (xw, event_type_property_notify, 4));
	  /* Must store window element separately because this window
	     might not have a corresponding XW object.  */
	  EVENT_ULONG_INTEGER
	    (result, EVENT_0, ((event->xproperty) . window));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_1, ((event->xproperty) . atom));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_2, ((event->xproperty) . time));
	  EVENT_ULONG_INTEGER
	    (result, EVENT_3, ((event->xproperty) . state));
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

static void
update_input_mask (struct xwindow * xw)
{
  {
    unsigned long event_mask = 0;
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
      event_mask |= (PointerMotionMask | PointerMotionHintMask);
    if (EVENT_ENABLED (xw, event_type_visibility))
      event_mask |= VisibilityChangeMask;
    if (EVENT_ENABLED (xw, event_type_property_notify))
      event_mask |= PropertyChangeMask;
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

static void
ping_server (struct xdisplay * xd)
{
  /* Periodically ping the server connection to see if it has died.  */
  (XD_SERVER_PING_TIMER (xd)) += 1;
  if ((XD_SERVER_PING_TIMER (xd)) >= 100)
    {
      (XD_SERVER_PING_TIMER (xd)) = 0;
      XNoOp (XD_DISPLAY (xd));
      XFlush (XD_DISPLAY (xd));
    }
}

/* The use of `XD_CACHED_EVENT' prevents an event from being lost due
   to garbage collection.  First `XD_CACHED_EVENT' is set to hold the
   current event, then the allocations are performed.  If one of them
   fails, the primitive will exit, and when it reenters it will notice
   the cached event and use it.  It is important that this be the only
   entry that reads events -- or else that all other event readers
   cooperate with this strategy.  */

static SCHEME_OBJECT
xd_process_events (struct xdisplay * xd, int non_block_p, int use_select_p)
{
  Display * display = (XD_DISPLAY (xd));
  unsigned int events_queued;
  SCHEME_OBJECT result;
  if (x_debug > 1)
    {
      fprintf (stderr, "Enter xd_process_events (%s)\n",
	       (non_block_p ? "non-blocking" : "blocking"));
      fflush (stderr);
    }
  if (!OS_have_select_p)
    use_select_p = 0;
  if (XD_CACHED_EVENT_P (xd))
    {
      events_queued = (XEventsQueued (display, QueuedAlready));
      goto restart;
    }
  if (use_select_p)
    events_queued = (XEventsQueued (display, QueuedAlready));
  else if (non_block_p)
    {
      ping_server (xd);
      events_queued = (XEventsQueued (display, QueuedAfterReading));
    }
  else
    events_queued = 0;
  while (1)
    {
      XEvent event;
      if (events_queued > 0)
	events_queued -= 1;
      else
	{
	  if (use_select_p)
	    switch (UX_select_input ((ConnectionNumber (display)),
				     (!non_block_p)))
	      {
	      case select_input_none:
		result = SHARP_F; goto done;
	      case select_input_other:
		result = (LONG_TO_FIXNUM (-2)); goto done;
	      case select_input_process_status:
		result = (LONG_TO_FIXNUM (-3)); goto done;
	      case select_input_interrupt:
		result = (LONG_TO_FIXNUM (-4)); goto done;
	      case select_input_argument:
		ping_server (xd);
		events_queued = (XEventsQueued (display, QueuedAfterReading));
		continue;
	      }
	  else if (non_block_p)
	    {
	      result = SHARP_F;
	      goto done;
	    }
	  ping_server (xd);
	}
      XNextEvent (display, (&event));
      if ((event.type) == KeymapNotify)
	continue;
      {
	struct xwindow * xw
	  = (x_window_to_xw (display, (event.xany.window)));
	if ((xw == 0)
	    && (! (((event.type) == PropertyNotify)
		   || ((event.type) == SelectionClear)
		   || ((event.type) == SelectionNotify)
		   || ((event.type) == SelectionRequest))))
	  continue;
	if (xw_process_event (xw, (&event)))
	  continue;
      }
      (XD_CACHED_EVENT (xd)) = event;
      (XD_CACHED_EVENT_P (xd)) = 1;
    restart:
      result = (x_event_to_object (&event));
      (XD_CACHED_EVENT_P (xd)) = 0;
      if (result != SHARP_F)
	goto done;
    }
 done:
  if (x_debug > 1)
    {
      fprintf (stderr, "Return from xd_process_events: ");
      if (result == SHARP_F)
	fprintf (stderr, "#f");
      else if (FIXNUM_P (result))
	fprintf (stderr, "%ld", (FIXNUM_TO_LONG (result)));
      else
	fprintf (stderr, "[vector]");
      fprintf (stderr, "\n");
      fflush (stderr);
    }
  return (result);
}

/* Open/Close Primitives */

static void
initialize_once (void)
{
  allocation_table_initialize (&x_display_table);
  allocation_table_initialize (&x_window_table);
  allocation_table_initialize (&x_image_table);
  ((x_error_info.message) [0]) = '\0';
  (x_error_info.terminate_p) = 1;
  (x_error_info.code) = 0;
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_handler);
  add_reload_cleanup (x_close_all_displays);
  initialization_done = 1;
}

DEFINE_PRIMITIVE ("X-DEBUG", Prim_x_debug, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT object = (ARG_REF (1));
    if (object == SHARP_F)
      x_debug = 0;
    else if (UNSIGNED_FIXNUM_P (object))
      x_debug = (UNSIGNED_FIXNUM_TO_LONG (object));
    else
      x_debug = 1;
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-OPEN-DISPLAY", Prim_x_open_display, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  INITIALIZE_ONCE ();
  {
    struct xdisplay * xd = (x_malloc (sizeof (struct xdisplay)));
    /* Added 7/95 by Nick in an attempt to fix problem Hal was having
       with SWAT over PPP (i.e. slow connections).  */
    block_signals ();
    (XD_DISPLAY (xd))
      = (XOpenDisplay (((ARG_REF (1)) == SHARP_F) ? 0 : (STRING_ARG (1))));
    unblock_signals ();
    if ((XD_DISPLAY (xd)) == 0)
      {
	free (xd);
	PRIMITIVE_RETURN (SHARP_F);
      }
    (XD_ALLOCATION_INDEX (xd))
      = (allocate_table_index ((&x_display_table), xd));
    (XD_SERVER_PING_TIMER (xd)) = 0;
    (XD_WM_PROTOCOLS (xd))
      = (XInternAtom ((XD_DISPLAY (xd)), "WM_PROTOCOLS", False));
    (XD_WM_DELETE_WINDOW (xd))
      = (XInternAtom ((XD_DISPLAY (xd)), "WM_DELETE_WINDOW", False));
    (XD_WM_TAKE_FOCUS (xd))
      = (XInternAtom ((XD_DISPLAY (xd)), "WM_TAKE_FOCUS", False));
    (XD_CACHED_EVENT_P (xd)) = 0;
    x_initialize_display_modifier_masks (xd);
    XRebindKeysym ((XD_DISPLAY (xd)), XK_BackSpace, 0, 0,
		   ((unsigned char *) "\177"), 1);
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

DEFINE_PRIMITIVE ("X-DISPLAY-GET-SIZE", Prim_x_display_get_size, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xdisplay * xd = (x_display_arg (1));
    Display * display = (XD_DISPLAY (xd));
    long screen = (arg_nonnegative_integer (2));
    PRIMITIVE_RETURN
      (cons ((ulong_to_integer (DisplayWidth (display, screen))),
	     (ulong_to_integer (DisplayHeight (display, screen)))));
  }
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

DEFINE_PRIMITIVE ("X-SET-DEFAULT-FONT", Prim_x_set_default_font, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xdisplay * xd = (x_display_arg (1));
    Display * display = (XD_DISPLAY (xd));
    const char * name = (STRING_ARG (2));
    XFontStruct * font = (XLoadQueryFont (display, name));
    if (font == 0)
      PRIMITIVE_RETURN (SHARP_F);
    XFreeFont (display, font);
    if (x_default_font != 0)
      OS_free ((void *) x_default_font);
    {
      char * copy = (OS_malloc ((strlen (name)) + 1));
      const char * s1 = name;
      char * s2 = copy;
      while (1)
	{
	  char c = (*s1++);
	  (*s2++) = c;
	  if (c == '\0')
	    break;
	}
      x_default_font = copy;
    }
  }
  PRIMITIVE_RETURN (SHARP_T);
}

/* Event Processing Primitives */

DEFINE_PRIMITIVE ("X-DISPLAY-DESCRIPTOR", Prim_x_display_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer (ConnectionNumber (XD_DISPLAY (x_display_arg (1)))));
}

DEFINE_PRIMITIVE ("X-MAX-REQUEST-SIZE", Prim_x_max_request_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer (XMaxRequestSize (XD_DISPLAY (x_display_arg (1)))));
}

DEFINE_PRIMITIVE ("X-DISPLAY-PROCESS-EVENTS", Prim_x_display_process_events, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xdisplay * xd = (x_display_arg (1));
    SCHEME_OBJECT how = (ARG_REF (2));
    if (how == SHARP_F)
      PRIMITIVE_RETURN (xd_process_events (xd, 0, 1));
    else if (how == (LONG_TO_UNSIGNED_FIXNUM (0)))
      PRIMITIVE_RETURN (xd_process_events (xd, 1, 1));
    else if (how == (LONG_TO_UNSIGNED_FIXNUM (1)))
      PRIMITIVE_RETURN (xd_process_events (xd, 0, 0));
    else
      PRIMITIVE_RETURN (xd_process_events (xd, 1, 0));
  }
}

DEFINE_PRIMITIVE ("X-SELECT-INPUT", Prim_x_select_input, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  XSelectInput ((XD_DISPLAY (x_display_arg (1))),
		(arg_ulong_integer (2)),
		(arg_integer (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-EVENT-MASK", Prim_x_window_event_mask, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (XW_EVENT_MASK (x_window_arg (1))));
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

/* Miscellaneous Primitives */

DEFINE_PRIMITIVE ("X-WINDOW-DISPLAY", Prim_x_window_display, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (XD_TO_OBJECT (XW_XD (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-X-SIZE", Prim_x_window_x_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (XW_X_SIZE (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-Y-SIZE", Prim_x_window_y_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (XW_Y_SIZE (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("X-WINDOW-BEEP", Prim_x_window_beep, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  XBell ((XW_DISPLAY (x_window_arg (1))), 0); /* base value */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-CLEAR", Prim_x_window_clear, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    if (((XW_CLIP_X (xw)) == 0)
	&& ((XW_CLIP_Y (xw)) == 0)
	&& ((XW_CLIP_WIDTH (xw)) == (XW_X_SIZE (xw)))
	&& ((XW_CLIP_HEIGHT (xw)) == (XW_Y_SIZE (xw))))
      XClearWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
    else
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

DEFINE_PRIMITIVE ("X-WINDOW-FLUSH", Prim_x_window_flush, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  XFlush (XW_DISPLAY (x_window_arg (1)));
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
    char * result
      = (XGetDefault ((XD_DISPLAY (x_display_arg (1))),
		      (STRING_ARG (2)),
		      (STRING_ARG (3))));
    PRIMITIVE_RETURN
      ((result == 0)
       ? SHARP_F
       : (char_pointer_to_string (result)));
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-COORDS-ROOT->LOCAL", Prim_x_window_coords_root2local, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT result = (cons (SHARP_F, SHARP_F));
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    int rx = (arg_integer (2));
    int ry = (arg_integer (3));
    int wx;
    int wy;
    Window child;
    if (! (XTranslateCoordinates
	   (display,
	    (RootWindow (display, (DefaultScreen (display)))),
	    (XW_WINDOW (xw)),
	    rx, ry, (&wx), (&wy), (&child))))
      error_bad_range_arg (1);
    SET_PAIR_CAR (result, (long_to_integer (wx)));
    SET_PAIR_CDR (result, (long_to_integer (wy)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-COORDS-LOCAL->ROOT", Prim_x_window_coords_local2root, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT result = (cons (SHARP_F, SHARP_F));
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    int wx = (arg_integer (2));
    int wy = (arg_integer (3));
    int rx;
    int ry;
    Window child;
    if (! (XTranslateCoordinates
	   (display,
	    (XW_WINDOW (xw)),
	    (RootWindow (display, (DefaultScreen (display)))),
	    wx, wy, (&rx), (&ry), (&child))))
      error_bad_range_arg (1);
    SET_PAIR_CAR (result, (long_to_integer (rx)));
    SET_PAIR_CDR (result, (long_to_integer (ry)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-QUERY-POINTER", Prim_x_window_query_pointer, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 5, 1));
    struct xwindow * xw = (x_window_arg (1));
    Window root;
    Window child;
    int root_x;
    int root_y;
    int win_x;
    int win_y;
    unsigned int keys_buttons;
    if (!XQueryPointer ((XW_DISPLAY (xw)),
			(XW_WINDOW (xw)),
			(&root), (&child),
			(&root_x), (&root_y),
			(&win_x), (&win_y),
			(&keys_buttons)))
      PRIMITIVE_RETURN (SHARP_F);
    VECTOR_SET (result, 0, (long_to_integer (root_x)));
    VECTOR_SET (result, 1, (long_to_integer (root_y)));
    VECTOR_SET (result, 2, (long_to_integer (win_x)));
    VECTOR_SET (result, 3, (long_to_integer (win_y)));
    VECTOR_SET (result, 4, (x_key_button_mask_to_scheme (keys_buttons)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-ID", Prim_x_window_id, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (XW_WINDOW (x_window_arg (1))));
}

DEFINE_PRIMITIVE ("X-ID->WINDOW", Prim_x_id_to_window, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw
      = (x_window_to_xw ((XD_DISPLAY (x_display_arg (1))),
			 (arg_ulong_integer (2))));
    PRIMITIVE_RETURN ((xw == 0) ? SHARP_F : (XW_TO_OBJECT (xw)));
  }
}

/* Appearance Control Primitives */

DEFINE_PRIMITIVE ("X-WINDOW-SET-FOREGROUND-COLOR", Prim_x_window_set_foreground_color, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    unsigned long foreground_pixel = (arg_window_color (2, display, xw));
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
    unsigned long background_pixel = (arg_window_color (2, display, xw));
    (XW_BACKGROUND_PIXEL (xw)) = background_pixel;
    XSetWindowBackground (display, (XW_WINDOW (xw)), background_pixel);
    XSetBackground (display, (XW_NORMAL_GC (xw)), background_pixel);
    XSetForeground (display, (XW_REVERSE_GC (xw)), background_pixel);
    XSetForeground (display, (XW_CURSOR_GC (xw)), background_pixel);
    x_set_mouse_colors (display,
			(xw_color_map (xw)),
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
    unsigned long border_pixel = (arg_window_color (2, display, xw));
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
    unsigned long cursor_pixel = (arg_window_color (2, display, xw));
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
    unsigned long mouse_pixel = (arg_window_color (2, display, xw));
    (XW_MOUSE_PIXEL (xw)) = mouse_pixel;
    x_set_mouse_colors (display,
			(xw_color_map (xw)),
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
      Cursor mouse_cursor
	= (XCreateFontCursor
	   (display, (2 * (arg_index_integer (2, (XC_num_glyphs / 2))))));
      x_set_mouse_colors (display,
			  (xw_color_map (xw)),
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
      Font fid = (font->fid);
      XSetFont (display, (XW_NORMAL_GC (xw)), fid);
      XSetFont (display, (XW_REVERSE_GC (xw)), fid);
      XSetFont (display, (XW_CURSOR_GC (xw)), fid);
    }
    if ((XW_UPDATE_NORMAL_HINTS (xw)) != 0)
      (* (XW_UPDATE_NORMAL_HINTS (xw))) (xw);
  }
  PRIMITIVE_RETURN (SHARP_T);
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
    unsigned int internal_border_width = (arg_nonnegative_integer (2));
    (XW_INTERNAL_BORDER_WIDTH (xw)) = internal_border_width;
    if ((XW_UPDATE_NORMAL_HINTS (xw)) != 0)
      (* (XW_UPDATE_NORMAL_HINTS (xw))) (xw);
    XResizeWindow ((XW_DISPLAY (xw)),
		   (XW_WINDOW (xw)),
		   ((XW_X_SIZE (xw)) + (2 * internal_border_width)),
		   ((XW_Y_SIZE (xw)) + (2 * internal_border_width)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* WM Communication Primitives */

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

DEFINE_PRIMITIVE ("X-WINDOW-SET-INPUT-FOCUS", Prim_x_window_set_input_focus, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xwindow * xw = (x_window_arg (1));
    Display * display = (XW_DISPLAY (xw));
    void * handle = (push_x_error_info (display));

    XSetInputFocus (display,
		    (XW_WINDOW (xw)),
		    RevertToParent,
		    ((Time) (arg_ulong_integer (2))));
    if (any_x_errors_p (display))
      {
	pop_x_error_info (handle);
	error_bad_range_arg (1);
      }
    pop_x_error_info (handle);
  }
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

/* WM Control Primitives */

DEFINE_PRIMITIVE ("X-WINDOW-MAP", Prim_x_window_map, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XMapWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
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

/* The following shouldn't be used on top-level windows.  Instead use
   ICONIFY or WITHDRAW.  */
DEFINE_PRIMITIVE ("X-WINDOW-UNMAP", Prim_x_window_unmap, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XUnmapWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-SIZE", Prim_x_window_set_size, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
    XResizeWindow ((XW_DISPLAY (xw)),
		   (XW_WINDOW (xw)),
		   ((arg_ulong_integer (2)) + extra),
		   ((arg_ulong_integer (3)) + extra));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-RAISE", Prim_x_window_raise, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XRaiseWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-LOWER", Prim_x_window_lower, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    XLowerWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-WINDOW-GET-SIZE", Prim_x_window_get_size, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    unsigned int extra;

    get_wm_decor_geometry (xw);
    extra = (2 * (XW_WM_DECOR_BORDER_WIDTH (xw)));
    PRIMITIVE_RETURN
      (cons ((ulong_to_integer ((XW_WM_DECOR_PIXEL_WIDTH (xw)) + extra)),
	     (ulong_to_integer ((XW_WM_DECOR_PIXEL_HEIGHT (xw)) + extra))));
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-GET-POSITION", Prim_x_window_get_position, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    get_wm_decor_geometry (xw);
    PRIMITIVE_RETURN (cons ((long_to_integer (XW_WM_DECOR_X (xw))),
			    (long_to_integer (XW_WM_DECOR_Y (xw)))));
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-SET-POSITION", Prim_x_window_set_position, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  move_window ((x_window_arg (1)),
	       (arg_integer (2)),
	       (arg_integer (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
move_window (struct xwindow * xw, int x, int y)
{
  if ((XW_UPDATE_NORMAL_HINTS (xw)) != 0)
    (* (XW_UPDATE_NORMAL_HINTS (xw))) (xw);
  if ((XW_WM_TYPE (xw)) == X_WMTYPE_A)
    {
      x += (XW_MOVE_OFFSET_X (xw));
      y += (XW_MOVE_OFFSET_Y (xw));
    }
  XMoveWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), x, y);
  if ((XW_WM_TYPE (xw)) == X_WMTYPE_UNKNOWN)
    {
      (XW_EXPECTED_X (xw)) = x;
      (XW_EXPECTED_Y (xw)) = y;
      (XW_CHECK_EXPECTED_MOVE_P (xw)) = 1;
    }
}

static void
check_expected_move (struct xwindow * xw)
{
  if (((XW_WM_DECOR_X (xw)) == (XW_EXPECTED_X (xw)))
      && ((XW_WM_DECOR_Y (xw)) == (XW_EXPECTED_Y (xw))))
    {
      if ((XW_WM_TYPE (xw)) == X_WMTYPE_UNKNOWN)
	(XW_WM_TYPE (xw)) = X_WMTYPE_B;
    }
  else
    {
      (XW_WM_TYPE (xw)) = X_WMTYPE_A;
      (XW_MOVE_OFFSET_X (xw)) = ((XW_EXPECTED_X (xw)) - (XW_WM_DECOR_X (xw)));
      (XW_MOVE_OFFSET_Y (xw)) = ((XW_EXPECTED_Y (xw)) - (XW_WM_DECOR_Y (xw)));
      move_window (xw, (XW_EXPECTED_X (xw)), (XW_EXPECTED_Y (xw)));
    }
  (XW_CHECK_EXPECTED_MOVE_P (xw)) = 0;
}

/* Font Structure Primitive */

#define FONT_STRUCTURE_MAX_CONVERTED_SIZE (10+1 + 256+1 + ((5+1) * (256+2)))
  /* font-structure-words +
     char-struct-vector +
     char-struct-words * maximum-number-possible */

static SCHEME_OBJECT
convert_char_struct (XCharStruct * char_struct)
{
  if (((char_struct->lbearing) == 0)
      && ((char_struct->rbearing) == 0)
      && ((char_struct->width) == 0)
      && ((char_struct->ascent) == 0)
      && ((char_struct->descent) == 0))
    return (SHARP_F);
  {
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 5, true));
    VECTOR_SET (result, 0, (long_to_integer (char_struct->lbearing)));
    VECTOR_SET (result, 1, (long_to_integer (char_struct->rbearing)));
    VECTOR_SET (result, 2, (long_to_integer (char_struct->width)));
    VECTOR_SET (result, 3, (long_to_integer (char_struct->ascent)));
    VECTOR_SET (result, 4, (long_to_integer (char_struct->descent)));
    return (result);
  }
}

static SCHEME_OBJECT
convert_font_struct (SCHEME_OBJECT font_name, XFontStruct * font)
{
  SCHEME_OBJECT result;
  if (font == 0)
    return  SHARP_F;
  /* Handle only 8-bit fonts because of laziness. */
  if (((font->min_byte1) != 0) || ((font->max_byte1) != 0))
    return  SHARP_F;

  result = (allocate_marked_vector (TC_VECTOR, 10, true));
  if ((font->per_char) == 0)
    VECTOR_SET (result, 6, SHARP_F);
  else
    {
      unsigned int start_index = (font->min_char_or_byte2);
      unsigned int length = ((font->max_char_or_byte2) - start_index + 1);
      SCHEME_OBJECT character_vector
	= (allocate_marked_vector (TC_VECTOR, length, true));
      unsigned int index;
      for (index = 0; (index < length); index += 1)
	VECTOR_SET (character_vector,
		    index,
		    (convert_char_struct ((font->per_char) + index)));
      VECTOR_SET (result, 6, (ulong_to_integer (start_index)));
      VECTOR_SET (result, 7, character_vector);
    }
  VECTOR_SET (result, 0, font_name);
  VECTOR_SET (result, 1, (ulong_to_integer (font->direction)));
  VECTOR_SET (result, 2,
	      (BOOLEAN_TO_OBJECT ((font->all_chars_exist) == True)));
  VECTOR_SET (result, 3, (ulong_to_integer (font->default_char)));
  VECTOR_SET (result, 4, (convert_char_struct (& (font->min_bounds))));
  VECTOR_SET (result, 5, (convert_char_struct (& (font->max_bounds))));
  VECTOR_SET (result, 8, (long_to_integer (font->ascent)));
  VECTOR_SET (result, 9, (long_to_integer (font->descent)));

  return  result;
}

DEFINE_PRIMITIVE ("X-FONT-STRUCTURE", Prim_x_font_structure, 2, 2,
		  "(DISPLAY FONT)\n\
FONT is either a font name or a font ID.")
{
  PRIMITIVE_HEADER (2);
  Primitive_GC_If_Needed (FONT_STRUCTURE_MAX_CONVERTED_SIZE);
  {
    SCHEME_OBJECT font_name = (ARG_REF (2));
    Display * display = (XD_DISPLAY (x_display_arg (1)));
    XFontStruct * font = 0;
    bool by_name = STRING_P (font_name);
    SCHEME_OBJECT result;

    if (by_name)
      font = XLoadQueryFont (display, (STRING_POINTER (font_name)));
    else
      font = XQueryFont (display, ((XID) (integer_to_ulong (ARG_REF (2)))));

    if (font == 0)
      PRIMITIVE_RETURN (SHARP_F);

    result = convert_font_struct (font_name, font);

    if (by_name)
      XFreeFont (display, font);
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("X-WINDOW-FONT-STRUCTURE", Prim_x_window_font_structure,
		  1, 1, "(X-WINDOW)\n\
Returns the font-structure for the font currently associated with X-WINDOW.")
{
  XFontStruct *font;
  PRIMITIVE_HEADER (1);
  Primitive_GC_If_Needed (FONT_STRUCTURE_MAX_CONVERTED_SIZE);
  font = XW_FONT (x_window_arg (1));
  PRIMITIVE_RETURN (convert_font_struct (ulong_to_integer (font->fid), font));
}

DEFINE_PRIMITIVE ("X-LIST-FONTS", Prim_x_list_fonts, 3, 3,
		  "(DISPLAY PATTERN LIMIT)\n\
LIMIT is an exact non-negative integer or #F for no limit.\n\
Returns #F or a vector of at least one string.")
{
  PRIMITIVE_HEADER (1);
  {
    int actual_count = 0;
    char ** names
      = (XListFonts ((XD_DISPLAY (x_display_arg (1))),
		     (STRING_ARG (2)),
		     ((FIXNUM_P (ARG_REF (3)))
		      ? (FIXNUM_TO_LONG (ARG_REF (3)))
		      : 1000000),
		     (&actual_count)));
    if (names == 0)
      PRIMITIVE_RETURN (SHARP_F);
    {
      unsigned int words = (actual_count + 1); /* the vector of strings */
      unsigned int i;
      for (i = 0; (i < actual_count); i += 1)
	words += (STRING_LENGTH_TO_GC_LENGTH (strlen (names[i])));
      if (GC_NEEDED_P (words))
	{
	  /* this causes the primitive to be restarted, so deallocate names */
	  XFreeFontNames (names);
	  Primitive_GC (words);
	  /* notreached */
	}
    }
    {
      SCHEME_OBJECT result
	= (allocate_marked_vector (TC_VECTOR, actual_count, false));
      unsigned int i;
      for (i = 0;  (i < actual_count);  i += 1)
	VECTOR_SET (result, i, (char_pointer_to_string (names[i])));
      XFreeFontNames (names);
      PRIMITIVE_RETURN (result);
    }
  }
}

/* Atoms */

DEFINE_PRIMITIVE ("X-INTERN-ATOM", Prim_x_intern_atom, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (ulong_to_integer (XInternAtom ((XD_DISPLAY (x_display_arg (1))),
				    (STRING_ARG (2)),
				    (BOOLEAN_ARG (3)))));
}

DEFINE_PRIMITIVE ("X-GET-ATOM-NAME", Prim_x_get_atom_name, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    struct xdisplay * xd = (x_display_arg (1));
    Display * display = (XD_DISPLAY (xd));
    void * handle = (push_x_error_info (display));
    char * name = (XGetAtomName (display, (arg_ulong_integer (2))));
    unsigned char error_code = (x_error_code (display));
    SCHEME_OBJECT result
      = ((error_code == 0)
	 ? (char_pointer_to_string (name))
	 : (ulong_to_integer (error_code)));
    if (name != 0)
      XFree (name);
    pop_x_error_info (handle);
    PRIMITIVE_RETURN (result);
  }
}

/* Window Properties */

static SCHEME_OBJECT
char_ptr_to_prop_data_32 (const unsigned char * data, unsigned long nitems)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, nitems, 1));
  unsigned long index;
  for (index = 0; (index < nitems); index += 1)
    VECTOR_SET (result, index, (ulong_to_integer (((CARD32 *) data) [index])));
  return (result);
}

static SCHEME_OBJECT
char_ptr_to_prop_data_16 (const unsigned char * data, unsigned long nitems)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, nitems, 1));
  unsigned long index;
  for (index = 0; (index < nitems); index += 1)
    VECTOR_SET (result, index, (ulong_to_integer (((CARD16 *) data) [index])));
  return (result);
}

static const unsigned char *
prop_data_32_to_char_ptr (SCHEME_OBJECT vector, unsigned long * length_return)
{
  unsigned long nitems = (VECTOR_LENGTH (vector));
  unsigned long length = (nitems * 4);
  unsigned char * data = (dstack_alloc (length));
  unsigned long index;
  for (index = 0; (index < nitems); index += 1)
    {
      SCHEME_OBJECT n = (VECTOR_REF (vector, index));
      if (!integer_to_ulong_p (n))
	return (0);
      (((CARD32 *) data) [index]) = (integer_to_ulong (n));
    }
  (*length_return) = length;
  return (data);
}

static const unsigned char *
prop_data_16_to_char_ptr (SCHEME_OBJECT vector, unsigned long * length_return)
{
  unsigned long nitems = (VECTOR_LENGTH (vector));
  unsigned long length = (nitems * 2);
  unsigned char * data = (dstack_alloc (length));
  unsigned long index;
  for (index = 0; (index < nitems); index += 1)
    {
      SCHEME_OBJECT n = (VECTOR_REF (vector, index));
      unsigned long un;
      if (!integer_to_ulong_p (n))
	return (0);
      un = (integer_to_ulong (n));
      if (un >= 65536)
	return (0);
      (((CARD16 *) data) [index]) = un;
    }
  (*length_return) = length;
  return (data);
}

DEFINE_PRIMITIVE ("X-GET-WINDOW-PROPERTY", Prim_x_get_window_property, 7, 7, 0)
{
  PRIMITIVE_HEADER (7);
  {
    Display * display = (XD_DISPLAY (x_display_arg (1)));
    Window window = (arg_ulong_integer (2));
    Atom property = (arg_ulong_integer (3));
    long long_offset = (arg_nonnegative_integer (4));
    long long_length = (arg_nonnegative_integer (5));
    Bool delete = (BOOLEAN_ARG (6));
    Atom req_type = (arg_ulong_integer (7));

    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long bytes_after;
    unsigned char * data;

    if ((XGetWindowProperty (display, window, property, long_offset,
			     long_length, delete, req_type, (&actual_type),
			     (&actual_format), (&nitems), (&bytes_after),
			     (&data)))
	!= Success)
      error_external_return ();
    if (actual_format == 0)
      {
	XFree (data);
	PRIMITIVE_RETURN (SHARP_F);
      }
    if (! ((actual_format == 8)
	   || (actual_format == 16)
	   || (actual_format == 32)))
      error_external_return ();
    {
      SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 4, 1));
      VECTOR_SET (result, 0, (ulong_to_integer (actual_type)));
      VECTOR_SET (result, 1, (long_to_integer (actual_format)));
      VECTOR_SET (result, 2, (ulong_to_integer (bytes_after)));
      VECTOR_SET (result, 3,
		  (((req_type != AnyPropertyType)
		    && (req_type != actual_type))
		   ? SHARP_F
		   : (actual_format == 32)
		   ? (char_ptr_to_prop_data_32 (data, nitems))
		   : (actual_format == 16)
		   ? (char_ptr_to_prop_data_16 (data, nitems))
		   : (memory_to_string (nitems, data))));
      XFree (data);
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("X-CHANGE-PROPERTY", Prim_x_change_property, 7, 7, 0)
{
  PRIMITIVE_HEADER (7);
  {
    Display * display = (XD_DISPLAY (x_display_arg (1)));
    Window window = (arg_ulong_integer (2));
    Atom property = (arg_ulong_integer (3));
    Atom type = (arg_ulong_integer (4));
    int format = (arg_nonnegative_integer (5));
    int mode = (arg_index_integer (6, 3));
    unsigned long dlen = 0;
    const unsigned char * data = 0;
    void * handle;
    unsigned char error_code;

    handle = (push_x_error_info (display));
    switch (format)
      {
      case 8:
	CHECK_ARG (7, STRING_P);
	data = (STRING_BYTE_PTR (ARG_REF (7)));
	dlen = (STRING_LENGTH (ARG_REF (7)));
	break;
      case 16:
	CHECK_ARG (7, VECTOR_P);
	data = (prop_data_16_to_char_ptr ((ARG_REF (7)), (&dlen)));
	if (data == 0)
	  error_bad_range_arg (7);
	break;
      case 32:
	CHECK_ARG (7, VECTOR_P);
	data = (prop_data_32_to_char_ptr ((ARG_REF (7)), (&dlen)));
	if (data == 0)
	  error_bad_range_arg (7);
	break;
      default:
	error_bad_range_arg (5);
	break;
      }
    XChangeProperty (display, window, property, type, format, mode, data, dlen);
    error_code = (x_error_code (display));
    pop_x_error_info (handle);
    PRIMITIVE_RETURN (ulong_to_integer (error_code));
  }
}

DEFINE_PRIMITIVE ("X-DELETE-PROPERTY", Prim_x_delete_property, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  XDeleteProperty ((XD_DISPLAY (x_display_arg (1))),
		   (arg_ulong_integer (2)),
		   (arg_ulong_integer (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Selections */

DEFINE_PRIMITIVE ("X-SET-SELECTION-OWNER", Prim_x_set_selection_owner, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  XSetSelectionOwner ((XD_DISPLAY (x_display_arg (1))),
		      (arg_ulong_integer (2)),
		      (arg_ulong_integer (3)),
		      (arg_ulong_integer (4)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GET-SELECTION-OWNER", Prim_x_get_selection_owner, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (ulong_to_integer (XGetSelectionOwner ((XD_DISPLAY (x_display_arg (1))),
					   (arg_ulong_integer (2)))));
}

DEFINE_PRIMITIVE ("X-CONVERT-SELECTION", Prim_x_convert_selection, 6, 6, 0)
{
  PRIMITIVE_HEADER (6);
  XConvertSelection ((XD_DISPLAY (x_display_arg (1))),
		     (arg_ulong_integer (2)),
		     (arg_ulong_integer (3)),
		     (arg_ulong_integer (4)),
		     (arg_ulong_integer (5)),
		     (arg_ulong_integer (6)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-SEND-SELECTION-NOTIFY", Prim_x_send_selection_notify, 6, 6, 0)
{
  PRIMITIVE_HEADER (6);
  {
    struct xdisplay * xd = (x_display_arg (1));
    Window requestor = (arg_ulong_integer (2));
    XSelectionEvent event;
    (event.type) = SelectionNotify;
    (event.display) = (XD_DISPLAY (xd));
    (event.requestor) = requestor;
    (event.selection) = (arg_ulong_integer (3));
    (event.target) = (arg_ulong_integer (4));
    (event.property) = (arg_ulong_integer (5));
    (event.time) = (arg_ulong_integer (6));
    XSendEvent ((XD_DISPLAY (xd)), requestor, False, 0, ((XEvent *) (&event)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}
