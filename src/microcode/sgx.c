/* -*-C-*-

$Id: sgx.c,v 1.13 2007/01/05 21:19:25 cph Exp $

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

/* Simple X graphics for HP 9000 series 300 machines. */

#include <X/Xlib.h>
#include <X/Xhp.h>
#include "scheme.h"
#include "prims.h"
#include "sgraph.h"

static Display * display = NULL;
static Window window = 0;
static char filename [1024] = "";
static int raster_state = 0;

static void close_display ();
static void close_window ();
static void delete_raster ();

#define GUARANTEE_DISPLAY()						\
{									\
  if (display == NULL)							\
    error_external_return ();						\
}

#define GUARANTEE_WINDOW()						\
{									\
  if (window == 0)							\
    error_external_return ();						\
}

#define GUARANTEE_RASTER()						\
{									\
  GUARANTEE_WINDOW ();							\
  if (raster_state == 0)						\
    error_external_return ();						\
}

static int
x_io_error_handler (display)
     Display *display;
{
  fprintf (stderr, "\nX IO Error\n");
  error_external_return ();
}

static int
x_error_handler (display, error_event)
     Display *display;
     XErrorEvent *error_event;
{
  fprintf (stderr, "\nX Error: %s\n",
	   (XErrDescrip (error_event -> error_code)));
  fprintf (stderr, "         Request code: %d\n",
	   (error_event -> request_code));
  fprintf (stderr, "         Request function: %d\n", (error_event -> func));
  fprintf (stderr, "         Request window: %x\n", (error_event -> window));
  fprintf (stderr, "         Error serial: %x\n", (error_event -> serial));
  error_external_return ();
}

DEFINE_PRIMITIVE ("X-GRAPHICS-OPEN-DISPLAY", Prim_x_graphics_open_display, 1, 1,
  "Opens display DISPLAY-NAME.  DISPLAY-NAME may be #F, in which case the\n\
default display is opened (based on the DISPLAY environment\n\
variable).  Returns #T if the open succeeds, #F otherwise.\n\
\n\
This primitive is additionally useful for determining whether the\n\
X server is running on the named display.")
{
  PRIMITIVE_HEADER (1);
  /* Only one display at a time. */
  close_display ();
  /* Grab error handlers. */
  XErrorHandler (x_error_handler);
  XIOErrorHandler (x_io_error_handler);
  display =
    (XOpenDisplay (((ARG_REF (1)) == SHARP_F) ? NULL : (STRING_ARG (1))));
  window = 0;
  (filename [0]) = '\0';
  raster_state = 0;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (display != NULL));
}

DEFINE_PRIMITIVE ("X-GRAPHICS-CLOSE-DISPLAY", Prim_x_graphics_close_display, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  close_display ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
close_display ()
{
  if (display != NULL)
    {
      close_window ();
      XCloseDisplay (display);
      display = NULL;
    }
  return;
}

/* (X-GRAPHICS-OLD-OPEN-WINDOW x y width height border-width)
   Opens a window at the given position, with the given border width,
   on the current display.  If another window was previously opened
   using this primitive, it is closed.  */

DEFINE_PRIMITIVE ("X-GRAPHICS-OLD-OPEN-WINDOW", Prim_x_graphics_old_open_window, 5, 5, 0)
{
  XhpArgItem arglist [7];
  PRIMITIVE_HEADER (5);
  GUARANTEE_DISPLAY ();
  /* Allow only one window open at a time. */
  close_window ();
  /* Open the window with the given arguments. */
  window =
    (XCreateWindow (RootWindow,
		    (arg_nonnegative_integer (1)),
		    (arg_nonnegative_integer (2)),
		    (arg_nonnegative_integer (3)),
		    (arg_nonnegative_integer (4)),
		    (arg_nonnegative_integer (5)),
		    WhitePixmap,
		    BlackPixmap));
  if (window == 0)
    error_external_return ();
  XStoreName (window, "scheme-graphics");
  XFlush ();
  (filename [0]) = '\0';
  raster_state = 0;
  /* Create a starbase device file. */
  if ((XhpFile ((& (filename [0])), window, display)) == 0)
    {
      (filename [0]) = '\0';
      close_window ();
      error_external_return ();
    }
  /* Return the filename so it can be passed to starbase. */
  PRIMITIVE_RETURN (char_pointer_to_string (& (filename [0])));
}

DEFINE_PRIMITIVE ("X-GRAPHICS-CLOSE-WINDOW", Prim_x_graphics_close_window, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  close_window ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
close_window ()
{
  sb_close_device ();
  if ((filename [0]) != '\0')
    {
      XhpDestroy (filename);
      (filename [0]) = '\0';
    }
  if (window != 0)
    {
      delete_raster ();
      XDestroyWindow (window);
      XFlush ();
      window = 0;
    }
  return;
}

DEFINE_PRIMITIVE ("X-GRAPHICS-MAP-WINDOW", Prim_x_graphics_map_window, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  GUARANTEE_WINDOW ();
  XMapWindow (window);
  XFlush ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-UNMAP-WINDOW", Prim_x_graphics_unmap_window, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  GUARANTEE_WINDOW ();
  XUnmapWindow (window);
  XFlush ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-RAISE-WINDOW", Prim_x_graphics_raise_window, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  GUARANTEE_WINDOW ();
  XRaiseWindow (window);
  XFlush ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-LOWER-WINDOW", Prim_x_graphics_lower_window, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  GUARANTEE_WINDOW ();
  XLowerWindow (window);
  XFlush ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-CONFIGURE-WINDOW", Prim_x_graphics_configure_window, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  GUARANTEE_WINDOW ();
  if (raster_state != 0)
    error_external_return ();
  XConfigureWindow
    (window,
     (arg_nonnegative_integer (1)),
     (arg_nonnegative_integer (2)),
     (arg_nonnegative_integer (3)),
     (arg_nonnegative_integer (4)));
  XFlush ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Routines to control the backup raster. */

DEFINE_PRIMITIVE ("X-GRAPHICS-CREATE-RASTER", Prim_x_graphics_create_raster, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  GUARANTEE_WINDOW ();
  delete_raster ();
  XhpRetainWindow (window, XhpCREATE_RASTER);
  XFlush ();
  raster_state = 1;
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DELETE-RASTER", Prim_x_graphics_delete_raster, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  GUARANTEE_WINDOW ();
  delete_raster ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
delete_raster ()
{
  if (raster_state != 0)
    {
      XhpRetainWindow (window, XhpDELETE_RASTER);
      XFlush ();
      raster_state = 0;
    }
  return;
}

DEFINE_PRIMITIVE ("X-GRAPHICS-START-RETAIN", Prim_x_graphics_start_retain, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  GUARANTEE_WINDOW ();
  GUARANTEE_RASTER ();
  XhpRetainWindow (window, XhpSTART_RETAIN);
  XFlush ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-STOP-RETAIN", Prim_x_graphics_stop_retain, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  GUARANTEE_WINDOW ();
  GUARANTEE_RASTER ();
  XhpRetainWindow (window, XhpSTOP_RETAIN);
  XFlush ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}
