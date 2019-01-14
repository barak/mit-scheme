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

/* Common X11 support. */

#include "x11.h"
#include <setjmp.h>
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

static void check_expected_move (struct xwindow *);

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
      new_items = (malloc ((sizeof (void *)) * new_length));
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
      new_items = (realloc (items, ((sizeof (void *)) * new_length)));
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
allocate_x_image (XImage * image)
{
  struct ximage * xi = (malloc (sizeof (struct ximage)));
  unsigned int index = (allocate_table_index ((&x_image_table), xi));
  (XI_ALLOCATION_INDEX (xi)) = index;
  (XI_IMAGE (xi)) = image;
  return (xi);
}

void
deallocate_x_image (struct ximage * xi)
{
  ((x_image_table.items) [XI_ALLOCATION_INDEX (xi)]) = 0;
  free (xi);
}

struct xvisual *
allocate_x_visual (Visual * visual)
{
  struct xvisual * xv = (malloc (sizeof (struct xvisual)));
  unsigned int index = (allocate_table_index ((&x_visual_table), xv));
  (XV_ALLOCATION_INDEX (xv)) = index;
  (XV_VISUAL (xv)) = visual;
  return (xv);
}

void
x_visual_deallocate (struct xvisual * xv)
{
  ((x_visual_table.items) [XV_ALLOCATION_INDEX (xv)]) = 0;
  free (xv);
}

struct xcolormap *
allocate_x_colormap (Colormap colormap, struct xdisplay * xd)
{
  struct xcolormap * xcm = (malloc (sizeof (struct xcolormap)));
  unsigned int index = (allocate_table_index ((&x_colormap_table), xcm));
  (XCM_ALLOCATION_INDEX (xcm)) = index;
  (XCM_COLORMAP (xcm)) = colormap;
  (XCM_XD (xcm)) = xd;
  return (xcm);
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
    }
  return (0);
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
		const char * color_name,
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

static int
xw_colormap (struct xwindow * xw, Colormap * cm)
{
  XWindowAttributes a;
  if (! (XGetWindowAttributes ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&a))))
    return (0);
  *cm = (a.colormap);
  return (1);
}

static int
color_pixel (char * color, Display * display, struct xwindow * xw,
	     unsigned long * result)
{
  Colormap cm;
  if (! xw_colormap (xw, &cm))
    return (0);
  if (! x_decode_color (display, cm, color, result))
    return (0);
  return (1);
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

const char *
x_get_default (Display * display,
	       const char * resource_name,
	       const char * resource_class,
	       const char * property_name,
	       const char * property_class,
	       const char * sdefault)
{
  const char * result = (XGetDefault (display, resource_name, property_name));
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
  const char * color_name
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

int
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
    return (1);
  {
    const char * s
      = (x_get_default (display,
			resource_name, resource_class,
			"borderWidth", "BorderWidth",
			0));
    (attributes->border_width) = ((s == 0) ? 0 : (atoi (s)));
  }
  {
    const char * s
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
  return (0);
}

static int
get_wm_decor_geometry (struct xwindow * xw)
{
  Display * display = (XW_DISPLAY (xw));
  Window decor = (XW_WINDOW (xw));
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
	    fprintf (stderr, "\nXQueryTree failed!\n");
	    fflush (stderr);
	    return (0);
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
      fprintf (stderr, "\nXGetGeometry failed!\n");
      fflush (stderr);
      return (0);
    }
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
	       unsigned int size)
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
  if (size < (sizeof (struct xwindow)))
    return (NULL);
  xw = (malloc (size));
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

void
x_close_window_internal (struct xwindow * xw)
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

void
x_close_display (struct xdisplay * xd)
{
  struct xwindow ** scan = ((struct xwindow **) (x_window_table.items));
  struct xwindow ** end = (scan + (x_window_table.length));
  while (scan < end)
    {
      struct xwindow * xw = (*scan++);
      if ((xw != 0) && ((XW_XD (xw)) == xd))
	x_close_window_internal (xw);
    }
  ((x_display_table.items) [XD_ALLOCATION_INDEX (xd)]) = 0;
  XCloseDisplay (XD_DISPLAY (xd));
}

void
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

static int
xw_set_class_hint (struct xwindow * xw, const char * name, const char * class)
{
  XClassHint * class_hint = (XAllocClassHint ());
  if (class_hint == 0)
    return (1);
  /* This structure is misdeclared, so cast the args. */
  (class_hint->res_name) = ((char *) name);
  (class_hint->res_class) = ((char *) class);
  XSetClassHint ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), class_hint);
  XFree (class_hint);
  return (0);
}

int
xw_set_wm_input_hint (struct xwindow * xw, int input_hint)
{
  XWMHints * hints = (XAllocWMHints ());
  if (hints == 0)
    return (1);
  (hints->flags) = InputHint;
  (hints->input) = (input_hint != 0);
  XSetWMHints ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), hints);
  XFree (hints);
  return (0);
}

int
xw_set_wm_name (struct xwindow * xw, const char * name)
{
  XTextProperty property;
  if ((XStringListToTextProperty (((char **) (&name)), 1, (&property))) == 0)
    return (1);
  XSetWMName ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&property));
  return (0);
}

int
xw_set_wm_icon_name (struct xwindow * xw, const char * name)
{
  XTextProperty property;
  if ((XStringListToTextProperty (((char **) (&name)), 1, (&property))) == 0)
    return (1);
  XSetWMIconName ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&property));
  return (0);
}

int
x_window_set_input_hint (struct xwindow * xw, int input_hint)
{
  XWMHints * hints = (XAllocWMHints ());
  if (hints == 0)
    return (1);
  (hints->flags) = InputHint;
  (hints->input) = (input_hint != 0);
  XSetWMHints ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), hints);
  XFree (hints);
  return (0);
}

int
x_window_set_name (struct xwindow * xw, const char * name)
{
  XTextProperty property;
  if ((XStringListToTextProperty (((char **) (&name)), 1, (&property))) == 0)
    return (1);
  XSetWMName ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&property));
  return (0);
}

int
x_window_set_icon_name (struct xwindow * xw, const char * name)
{
  XTextProperty property;
  if ((XStringListToTextProperty (((char **) (&name)), 1, (&property))) == 0)
    return (1);
  XSetWMIconName ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), (&property));
  return (0);
}

int
xw_make_window_map (struct xwindow * xw,
		    const char * resource_name,
		    const char * resource_class,
		    int map_p)
{
  int code = xw_set_class_hint (xw, resource_name, resource_class);
  if (code != 0)
    return (code);
  if (map_p)
    {
      XMapWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
      XFlush (XW_DISPLAY (xw));
    }
  return (0);
}

/* Event Processing */

/* Returns non-zero value if caller should ignore the event.  */

#define EVENT_ENABLED(xw, type)					\
  (((XW_EVENT_MASK (xw)) & (1 << ((unsigned int) (type)))) != 0)

static int
xw_process_event (struct xwindow * xw, XEvent * event)
{
  int ignore_p = 0;

  if (x_debug > 0)
    {
      const char * type_name;
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
	  ignore_p = 1;
	  break;

	case ConfigureNotify:
	  /* If the window has been reparented, don't check
	     non-synthetic events.  */
	  if ((XW_CHECK_EXPECTED_MOVE_P (xw))
	      && (! ((get_wm_decor_geometry (xw))
		     && (! ((event->xconfigure) . send_event)))))
	    check_expected_move (xw);
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
		    if (! EVENT_ENABLED (xw, event_type_delete_window))
		      ignore_p = 1;
		  }
		else if (((Atom) (((event->xclient) . data . l) [0]))
			 == (XD_WM_TAKE_FOCUS (xd)))
		  {
		    if (! EVENT_ENABLED (xw, event_type_take_focus))
		      ignore_p = 1;
		  }
	      }
	  }
	  break;
	}
      (* (XW_EVENT_PROCESSOR (xw))) (xw, event);
    }
  return (ignore_p);
}

int
x_event_delete_window_p (struct xwindow * xw, XEvent * event)
{
  struct xdisplay * xd = (XW_XD (xw));
  return ((((event->xclient) . message_type) == (XD_WM_PROTOCOLS (xd)))
	  && (((event->xclient) . format) == 32)
	  && (((Atom) (((event->xclient) . data . l) [0]))
	      == (XD_WM_DELETE_WINDOW (xd))));
}

int
x_event_take_focus_p (struct xwindow * xw, XEvent * event)
{
  struct xdisplay * xd = (XW_XD (xw));
  return ((((event->xclient) . message_type) == (XD_WM_PROTOCOLS (xd)))
	  && (((event->xclient) . format) == 32)
	  && (((Atom) (((event->xclient) . data . l) [0]))
	      == (XD_WM_TAKE_FOCUS (xd))));
}

unsigned long
x_event_take_focus_time (XEvent * event)
{
  return (((event->xclient) . data . l) [1]);
}

/* This handles only the modifier bits that Scheme supports.
   At the moment, these are Control, Meta, Super, and Hyper.
   This might want to change if the character abstraction were ever to
   change, or if the X11 interface were to be changed to use something
   other than Scheme characters to convey key presses. */

/* Copied from microcode/object.h(!): */
#define CHAR_BITS_META 		0x1
#define CHAR_BITS_CONTROL 	0x2
#define CHAR_BITS_SUPER		0x4
#define CHAR_BITS_HYPER		0x8

unsigned long
x_modifier_mask_to_bucky_bits (unsigned int mask, struct xwindow * xw)
{
  struct xdisplay * xd = (XW_XD (xw));
  unsigned long bucky = 0;
  if (X_MODIFIER_MASK_CONTROL_P (mask, xd)) bucky |= CHAR_BITS_CONTROL;
  if (X_MODIFIER_MASK_META_P    (mask, xd)) bucky |= CHAR_BITS_META;
  if (X_MODIFIER_MASK_SUPER_P   (mask, xd)) bucky |= CHAR_BITS_SUPER;
  if (X_MODIFIER_MASK_HYPER_P   (mask, xd)) bucky |= CHAR_BITS_HYPER;
  return (bucky);
}

static XComposeStatus compose_status;

int
x_lookup_string (XKeyEvent * event, char *buffer_return, int bytes_buffer,
		 KeySym * keysym_return)
{
  return (XLookupString (event,
			 buffer_return,
			 bytes_buffer,
			 keysym_return,
			 &compose_status));
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

static int
xd_process_events (struct xdisplay * xd, XEvent * result,
		   struct xwindow ** xw_ret)
{
  Display * display = (XD_DISPLAY (xd));
  unsigned int events_queued;
  XEvent event;
  struct xwindow * xw = NULL;
  int done_p = 1;

  if (x_debug > 1)
    {
      fprintf (stderr, "Enter xd_process_events\n");
      fflush (stderr);
    }
  ping_server (xd);
  events_queued = (XEventsQueued (display, QueuedAfterReading));
  while (0 < events_queued)
    {
      events_queued -= 1;
      XNextEvent (display, (&event));
      if ((event.type) == KeymapNotify)
	continue;
      xw = (x_window_to_xw (display, (event.xany.window)));
      if ((xw == 0)
	  && (! (((event.type) == PropertyNotify)
		 || ((event.type) == SelectionClear)
		 || ((event.type) == SelectionNotify)
		 || ((event.type) == SelectionRequest))))
	continue;
      if (xw_process_event (xw, (&event)))
	continue;
      memcpy (result, &event, sizeof (XEvent));
      *xw_ret = xw;
      done_p = 0;
      break;
    }
  if (x_debug > 1)
    {
      fprintf (stderr, "Return from xd_process_events: %d 0x%lx\n",
	       done_p, ((unsigned long) xw));
      fflush (stderr);
    }
  return (done_p);
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
  initialization_done = 1;
}

void
x_set_debug (int value)
{
  x_debug = value;
}

struct xdisplay *
x_open_display (char * display_name)
{
  INITIALIZE_ONCE ();
  {
    struct xdisplay * xd = (malloc (sizeof (struct xdisplay)));
    /* Added 7/95 by Nick in an attempt to fix problem Hal was having
       with SWAT over PPP (i.e. slow connections).  */
    block_signals ();
    (XD_DISPLAY (xd)) = XOpenDisplay (display_name);
    unblock_signals ();
    if ((XD_DISPLAY (xd)) == 0)
      {
	free (xd);
	return (NULL);
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
    x_initialize_display_modifier_masks (xd);
    XRebindKeysym ((XD_DISPLAY (xd)), XK_BackSpace, 0, 0,
		   ((unsigned char *) "\177"), 1);
    return (xd);
  }
}

void
x_display_get_size (struct xdisplay * xd, long screen, int * results)
{
  Display * display = (XD_DISPLAY (xd));
  results[0] = (DisplayWidth (display, screen));
  results[1] = (DisplayHeight (display, screen));
}

void
x_close_window (struct xwindow * xw)
{
  Display * display = (XW_DISPLAY (xw));
  x_close_window_internal (xw);
  XFlush (display);
}

int
x_set_default_font (struct xdisplay * xd, const char * name)
{
  Display * display = (XD_DISPLAY (xd));
  XFontStruct * font = (XLoadQueryFont (display, name));
  char * copy;
  if (font == 0)
    return (1);
  XFreeFont (display, font);
  if (x_default_font != 0)
    free ((void *)x_default_font);
  copy = (malloc ((strlen (name)) + 1));
  strcpy (copy, name);
  x_default_font = copy;
  return (0);
}

/* Event Processing Primitives */

int
x_display_descriptor (struct xdisplay * xd)
{
  Display * display = (XD_DISPLAY (xd));
  return (ConnectionNumber (display));
}

long
x_max_request_size (struct xdisplay * xd)
{
  Display * display = (XD_DISPLAY (xd));
  return (XMaxRequestSize (display));
}

int
x_display_process_events (struct xdisplay * xd, XEvent * event,
			  struct xwindow **xw_ret)
{
  return (xd_process_events (xd, event, xw_ret));
}

void
x_select_input (struct xdisplay * xd, Window window, long mask)
{
  Display * display = (XD_DISPLAY (xd));
  XSelectInput (display, window, mask);
}

long
x_window_event_mask (struct xwindow * xw)
{
  return (XW_EVENT_MASK (xw));
}

int
x_window_set_event_mask (struct xwindow * xw, long mask)
{
  if (mask >= (1 << ((unsigned int) event_type_supremum)))
    return (0);
  (XW_EVENT_MASK (xw)) = mask;
  update_input_mask (xw);
  return (1);
}

void
x_window_or_event_mask (struct xwindow * xw, long mask)
{
  (XW_EVENT_MASK (xw)) |= mask;
  update_input_mask (xw);
}

void
x_window_andc_event_mask (struct xwindow * xw, long mask)
{
  (XW_EVENT_MASK (xw)) &=~ mask;
  update_input_mask (xw);
}

/* Miscellaneous Primitives */

struct xdisplay *
x_window_display (struct xwindow * xw)
{
  return (XW_XD (xw));
}

long
x_window_screen_number (struct xwindow * xw)
{
  XWindowAttributes attrs;
  XGetWindowAttributes((XW_DISPLAY (xw)), (XW_WINDOW(xw)), &attrs);
  return (XScreenNumberOfScreen(attrs.screen));
}

int
x_window_x_size (struct xwindow * xw)
{
  return (XW_X_SIZE (xw));
}

int
x_window_y_size (struct xwindow * xw)
{
  return (XW_Y_SIZE (xw));
}

void
x_window_beep (struct xwindow * xw)
{
  XBell ((XW_DISPLAY (xw)), 0); /* base value */
}

void
x_window_clear (struct xwindow * xw)
{
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

void
x_display_flush (struct xdisplay * xd)
{
  XFlush (XD_DISPLAY (xd));
}

void
x_window_flush (struct xwindow * xw)
{
  XFlush (XW_DISPLAY (xw));
}

void
x_display_sync (struct xdisplay * xd, Bool discard)
{
  XSync ((XD_DISPLAY (xd)), discard);
}

char *
x_display_get_default (struct xdisplay * xd,
		       char * resource_name,
		       char * class_name)
{
  return (XGetDefault ((XD_DISPLAY (xd)), resource_name, class_name));
}

int
x_window_query_pointer (struct xwindow * xw, int * result)
{
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
    return (0);
  result[0] = root_x;
  result[1] = root_y;
  result[2] = win_x;
  result[3] = win_y;
  result[4] = keys_buttons;
  return (1);
}

unsigned long
x_window_id (struct xwindow * xw)
{
  return (XW_WINDOW (xw));
}

/* Appearance Control Functions */

void
x_window_set_foreground_color_pixel (struct xwindow * xw, unsigned long pixel)
{
  Display * display = (XW_DISPLAY (xw));
  (XW_FOREGROUND_PIXEL (xw)) = pixel;
  XSetForeground (display, (XW_NORMAL_GC (xw)), pixel);
  XSetBackground (display, (XW_REVERSE_GC (xw)), pixel);
}

void
x_window_set_foreground_color_name (struct xwindow * xw, char * color)
{
  Display * display = (XW_DISPLAY (xw));
  unsigned long pixel;
  if (! color_pixel (color, display, xw, &pixel))
    return;
  x_window_set_foreground_color_pixel (xw, pixel);
}

int
x_window_set_background_color_pixel (struct xwindow * xw, unsigned long pixel)
{
  Display * display = (XW_DISPLAY (xw));
  Colormap cm;
  if (! xw_colormap (xw, &cm))
    return (0);
  (XW_BACKGROUND_PIXEL (xw)) = pixel;
  XSetWindowBackground (display, (XW_WINDOW (xw)), pixel);
  XSetBackground (display, (XW_NORMAL_GC (xw)), pixel);
  XSetForeground (display, (XW_REVERSE_GC (xw)), pixel);
  XSetForeground (display, (XW_CURSOR_GC (xw)), pixel);
  x_set_mouse_colors (display, cm,
		      (XW_MOUSE_CURSOR (xw)),
		      (XW_MOUSE_PIXEL (xw)),
		      pixel);
  return (1);
}

void
x_window_set_background_color_name (struct xwindow * xw, char * color)
{
  Display * display = (XW_DISPLAY (xw));
  unsigned long pixel;
  if (! color_pixel (color, display, xw, &pixel))
    return;
  x_window_set_background_color_pixel (xw, pixel);
}

void
x_window_set_border_color_pixel (struct xwindow * xw, unsigned long pixel)
{
  (XW_BORDER_PIXEL (xw)) = pixel;
  XSetWindowBorder ((XW_DISPLAY (xw)), (XW_WINDOW (xw)), pixel);
}

void
x_window_set_border_color_name (struct xwindow * xw, char * color)
{
  Display * display = (XW_DISPLAY (xw));
  unsigned long pixel;
  if (! color_pixel (color, display, xw, &pixel))
    return;
  x_window_set_border_color_pixel (xw, pixel);
}

void
x_window_set_cursor_color_pixel (struct xwindow * xw, unsigned long pixel)
{
  Display * display = (XW_DISPLAY (xw));
  (XW_CURSOR_PIXEL (xw)) = pixel;
  XSetBackground (display, (XW_CURSOR_GC (xw)), pixel);
}

void
x_window_set_cursor_color_name (struct xwindow * xw, char * color)
{
  Display * display = (XW_DISPLAY (xw));
  unsigned long pixel;
  if (! color_pixel (color, display, xw, &pixel))
    return;
  x_window_set_cursor_color_pixel (xw, pixel);
}

int
x_window_set_mouse_color_pixel (struct xwindow * xw, unsigned long pixel)
{
  Display * display = (XW_DISPLAY (xw));
  Colormap cm;
  if (! xw_colormap (xw, &cm))
    return (0);
  (XW_MOUSE_PIXEL (xw)) = pixel;
  x_set_mouse_colors (display, cm,
		      (XW_MOUSE_CURSOR (xw)),
		      pixel,
		      (XW_BACKGROUND_PIXEL (xw)));
  return (1);
}

void
x_window_set_mouse_color_name (struct xwindow * xw, char * color)
{
  Display * display = (XW_DISPLAY (xw));
  unsigned long pixel;
  if (! color_pixel (color, display, xw, &pixel))
    return;
  x_window_set_mouse_color_pixel (xw, pixel);
}

int
x_window_set_mouse_shape (struct xwindow * xw, int shape)
{
  Display * display = (XW_DISPLAY (xw));
  Colormap cm;
  Window window = (XW_WINDOW (xw));
  if (shape >= (XC_num_glyphs / 2))
    return (0);
  if (! xw_colormap (xw, &cm))
    return (0);
  {
    Cursor old_cursor = (XW_MOUSE_CURSOR (xw));
    Cursor mouse_cursor = (XCreateFontCursor (display, (2 * shape)));
    x_set_mouse_colors (display, cm,
			mouse_cursor,
			(XW_MOUSE_PIXEL (xw)),
			(XW_BACKGROUND_PIXEL (xw)));
    (XW_MOUSE_CURSOR (xw)) = mouse_cursor;
    XDefineCursor (display, window, mouse_cursor);
    XFreeCursor (display, old_cursor);
  }
  return (1);
}

int
x_window_set_font (struct xwindow * xw, char * font_name)
{
  Display * display = (XW_DISPLAY (xw));
  XFontStruct * font = XLoadQueryFont (display, font_name);
  if (font == 0)
    return (0);
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
  return (1);
}

void
x_window_set_border_width (struct xwindow * xw, uint border_width)
{
  Display * display = (XW_DISPLAY (xw));
  (XW_BORDER_WIDTH (xw)) = border_width;
  XSetWindowBorderWidth (display, (XW_WINDOW (xw)), border_width);
}

void
x_window_set_internal_border_width (struct xwindow * xw,
				    uint internal_border_width)
{
  (XW_INTERNAL_BORDER_WIDTH (xw)) = internal_border_width;
  if ((XW_UPDATE_NORMAL_HINTS (xw)) != 0)
    (* (XW_UPDATE_NORMAL_HINTS (xw))) (xw);
  XResizeWindow ((XW_DISPLAY (xw)),
		 (XW_WINDOW (xw)),
		 ((XW_X_SIZE (xw)) + (2 * internal_border_width)),
		 ((XW_Y_SIZE (xw)) + (2 * internal_border_width)));
}

/* WM Communication Primitives */

int
x_window_set_input_focus (struct xwindow * xw, Time time)
{
  Display * display = (XW_DISPLAY (xw));
  XSetInputFocus (display, (XW_WINDOW (xw)), RevertToParent, time);
  if (any_x_errors_p (display))
    return (1);
  return (0);
}

/* WM Control Primitives */

void
x_window_map (struct xwindow * xw)
{
  XMapWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
}

void
x_window_iconify (struct xwindow * xw)
{
  Display * display = (XW_DISPLAY (xw));
  XIconifyWindow (display, (XW_WINDOW (xw)), (DefaultScreen (display)));
}

void
x_window_withdraw (struct xwindow * xw)
{
  Display * display = (XW_DISPLAY (xw));
  XWithdrawWindow (display, (XW_WINDOW (xw)), (DefaultScreen (display)));
}

void
x_window_set_size (struct xwindow * xw, int width, int height)
{
  unsigned int extra = (2 * (XW_INTERNAL_BORDER_WIDTH (xw)));
  XResizeWindow ((XW_DISPLAY (xw)),
		 (XW_WINDOW (xw)),
		 width + extra,
		 height + extra);
}

void
x_window_raise (struct xwindow * xw)
{
  XRaiseWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
}

void
x_window_lower (struct xwindow * xw)
{
  XLowerWindow ((XW_DISPLAY (xw)), (XW_WINDOW (xw)));
}

void
x_window_get_size (struct xwindow * xw, int * dimens)
{
  unsigned int extra;

  get_wm_decor_geometry (xw);
  extra = (2 * (XW_WM_DECOR_BORDER_WIDTH (xw)));
  dimens[0] = (XW_WM_DECOR_PIXEL_WIDTH (xw)) + extra;
  dimens[1] = (XW_WM_DECOR_PIXEL_HEIGHT (xw)) + extra;
}

void
x_window_get_position (struct xwindow * xw, int * coord_return)
{
  get_wm_decor_geometry (xw);
  coord_return[0] = (XW_WM_DECOR_X (xw));
  coord_return[1] = (XW_WM_DECOR_Y (xw));
}

void
x_window_set_position (struct xwindow * xw, int x, int y)
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
      x_window_set_position (xw, (XW_EXPECTED_X (xw)), (XW_EXPECTED_Y (xw)));
    }
  (XW_CHECK_EXPECTED_MOVE_P (xw)) = 0;
}

/* Font Structure Primitive */

XFontStruct *
x_font_structure_by_name (struct xdisplay * xd, const char * font_name)
{
  Display * display = XD_DISPLAY (xd);
  return (XLoadQueryFont (display, font_name));
}

XFontStruct *
x_font_structure_by_id (struct xdisplay * xd, XID id)
{
  Display * display = (XD_DISPLAY (xd));
  return (XQueryFont (display, id));
}

void
x_free_font (struct xdisplay * xd, XFontStruct *font)
{
  Display * display = (XD_DISPLAY (xd));
  XFreeFont (display, font);
}

char * *
x_list_fonts (struct xdisplay * xd, char * pattern, long limit,
	      int * actual_count)
{
  return (XListFonts ((XD_DISPLAY (xd)), pattern, limit, actual_count));
}

/* Atoms */

Atom
x_intern_atom (struct xdisplay * xd, const char * name, int soft_p)
{
  return (XInternAtom ((XD_DISPLAY (xd)), name, soft_p));
}

int
x_get_atom_name (struct xdisplay * xd, Atom atom, char * * name_return)
{
  Display * display = (XD_DISPLAY (xd));
  *name_return = (XGetAtomName (display, atom));
  return (x_error_code (display));
}

/* Window Properties */

int
x_get_window_property (struct xdisplay * xd, Window window, Atom property,
		       long long_offset, long long_length, Bool delete,
		       Atom req_type,
		       Atom * actual_type_return, int * actual_format_return,
		       unsigned long * nitems_return,
		       unsigned long * bytes_after_return,
		       unsigned char * * prop_return)
{
    Display * display = (XD_DISPLAY (xd));

    int actual_format;

    if ((XGetWindowProperty (display, window, property, long_offset,
			     long_length, delete, req_type,
			     actual_type_return, (&actual_format),
			     nitems_return, bytes_after_return, prop_return))
	!= Success)
      return (1);
    *actual_format_return = actual_format;
    if (actual_format == 0)
      {
	XFree (*prop_return);
	*prop_return = NULL;
	return (2);
      }
    if (! ((actual_format == 8)
	   || (actual_format == 16)
	   || (actual_format == 32)))
      return (3);
    return (0);
}

int
x_change_property (struct xdisplay * xd, Window window,
		   Atom property, Atom type, int format, int mode,
		   unsigned char * data, unsigned long dlen)
{
    Display * display = (XD_DISPLAY (xd));

    if (mode >= 3)
      return (0);
    XChangeProperty (display, window, property, type, format, mode, data, dlen);
    return (x_error_code (display));
}

void
x_delete_property (struct xdisplay * xd, Window window, Atom property)
{
  XDeleteProperty ((XD_DISPLAY (xd)), window, property);
}

/* Selections */

void
x_set_selection_owner (struct xdisplay * xd, Atom selection,
		       Window owner, Time time)
{
  Display * display = (XD_DISPLAY (xd));
  XSetSelectionOwner (display, selection, owner, time);
}

Window
x_get_selection_owner (struct xdisplay * xd, Atom selection)
{
  return (XGetSelectionOwner ((XD_DISPLAY (xd)), selection));
}

void
x_convert_selection (struct xdisplay * xd, Atom selection, Atom target,
		     Atom property, Window requestor, Time time)
{
  XConvertSelection ((XD_DISPLAY (xd)), selection, target, property,
		     requestor, time);
}

void
x_send_selection_notify (struct xdisplay * xd, Window requestor,
			 Atom selection, Atom target, Atom property, Time time)
{
  Display * display = (XD_DISPLAY (xd));
  XSelectionEvent event;
  (event.type) = SelectionNotify;
  (event.display) = display;
  (event.requestor) = requestor;
  (event.selection) = selection;
  (event.target) = target;
  (event.property) = property;
  (event.time) = time;
  XSendEvent (display, requestor, False, 0, ((XEvent *) (&event)));
}
