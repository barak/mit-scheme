/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/sgx.c,v 1.1 1988/07/15 09:04:39 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

/* Simple X graphics for HP 9000 series 300 machines. */

#include <X/Xlib.h>
#include <X/Xhp.h>
#include "scheme.h"
#include "primitive.h"
#include "flonum.h"
#include "Sgraph.h"

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

/* (X-GRAPHICS-OPEN-DISPLAY display-name)

   Opens the named display.  The name may be #F, in which case the
   default display is opened (based on the DISPLAY environment
   variable).  Returns #T if the open succeeds, #F otherwise.

   This primitive is additionally useful for determining whether the
   X server is running on the named display.  */

DEFINE_PRIMITIVE ("X-GRAPHICS-OPEN-DISPLAY", Prim_x_graphics_open_display, 1)
{
  PRIMITIVE_HEADER (1);

  /* Only one display at a time. */
  close_display ();
  display = (XOpenDisplay (((ARG_REF (1)) == NIL) ? NULL : (STRING_ARG (1))));
  window = 0;
  (filename [0]) = '\0';
  raster_state = 0;
  PRIMITIVE_RETURN ((display == NULL) ? NIL : TRUTH);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-CLOSE-DISPLAY", Prim_x_graphics_close_display, 0)
{
  PRIMITIVE_HEADER (0);

  close_display ();
  PRIMITIVE_RETURN (NIL);
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

/* (X-GRAPHICS-OPEN-WINDOW x y width height border-width)

   Opens a window at the given position, with the given border width,
   on the current display.  If another window was previously opened
   using this primitive, it is closed.  */

DEFINE_PRIMITIVE ("X-GRAPHICS-OPEN-WINDOW", Prim_x_graphics_open_window, 5)
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
  PRIMITIVE_RETURN (C_String_To_Scheme_String (& (filename [0])));
}

DEFINE_PRIMITIVE ("X-GRAPHICS-CLOSE-WINDOW", Prim_x_graphics_close_window, 0)
{
  PRIMITIVE_HEADER (0);

  close_window ();
  PRIMITIVE_RETURN (NIL);
}

static void
close_window ()
{
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

DEFINE_PRIMITIVE ("X-GRAPHICS-MAP-WINDOW", Prim_x_graphics_map_window, 0)
{
  PRIMITIVE_HEADER (0);

  GUARANTEE_WINDOW ();
  XMapWindow (window);
  XFlush ();
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-UNMAP-WINDOW", Prim_x_graphics_unmap_window, 0)
{
  PRIMITIVE_HEADER (0);

  GUARANTEE_WINDOW ();
  XUnmapWindow (window);
  XFlush ();
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-RAISE-WINDOW", Prim_x_graphics_raise_window, 0)
{
  PRIMITIVE_HEADER (0);

  GUARANTEE_WINDOW ();
  XRaiseWindow (window);
  XFlush ();
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-LOWER-WINDOW", Prim_x_graphics_lower_window, 0)
{
  PRIMITIVE_HEADER (0);

  GUARANTEE_WINDOW ();
  XLowerWindow (window);
  XFlush ();
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-CONFIGURE-WINDOW", Prim_x_graphics_configure_window, 4)
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
  PRIMITIVE_RETURN (NIL);
}

/* Routines to control the backup raster. */

DEFINE_PRIMITIVE ("X-GRAPHICS-CREATE-RASTER", Prim_x_graphics_create_raster, 0)
{
  PRIMITIVE_HEADER (0);

  GUARANTEE_WINDOW ();
  delete_raster ();
  XhpRetainWindow (window, XhpCREATE_RASTER);
  XFlush ();
  raster_state = 1;
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DELETE-RASTER", Prim_x_graphics_delete_raster, 0)
{
  PRIMITIVE_HEADER (0);

  GUARANTEE_WINDOW ();
  delete_raster ();
  PRIMITIVE_RETURN (NIL);
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

DEFINE_PRIMITIVE ("X-GRAPHICS-START-RETAIN", Prim_x_graphics_start_retain, 0)
{
  PRIMITIVE_HEADER (0);

  GUARANTEE_WINDOW ();
  GUARANTEE_RASTER ();
  XhpRetainWindow (window, XhpSTART_RETAIN);
  XFlush ();
  PRIMITIVE_RETURN (NIL);
}

DEFINE_PRIMITIVE ("X-GRAPHICS-STOP-RETAIN", Prim_x_graphics_stop_retain, 0)
{
  PRIMITIVE_HEADER (0);

  GUARANTEE_WINDOW ();
  GUARANTEE_RASTER ();
  XhpRetainWindow (window, XhpSTOP_RETAIN);
  XFlush ();
  PRIMITIVE_RETURN (NIL);
}
