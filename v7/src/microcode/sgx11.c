/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/sgx11.c,v 1.2 1989/09/20 23:04:57 cph Rel $

Copyright (c) 1989 Massachusetts Institute of Technology

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

/* Simple X11 graphics for HP 9000 series 300 machines. */

#include <X11/Xlib.h>
#include "scheme.h"
#include "prims.h"
#include "Sgraph.h"

static int
x_io_error_handler (display)
     Display * display;
{
  fprintf (stderr, "\nX IO Error\n");
  error_external_return ();
}

static int
x_error_handler (display, error_event)
     Display * display;
     XErrorEvent * error_event;
{
  char buffer [2048];

  XGetErrorText (display, (error_event -> error_code),
		 (& buffer), (sizeof (buffer)));
  fprintf (stderr, "\nX Error: %s\n", buffer);
  fprintf (stderr, "         Request code: %d\n",
	   (error_event -> request_code));
  fprintf (stderr, "         Error serial: %x\n", (error_event -> serial));
  error_external_return ();
}

DEFINE_PRIMITIVE ("X-GRAPHICS-DISPLAY-NAME", Prim_x_graphics_display_name, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (char_pointer_to_string
     (XDisplayName (((ARG_REF (1)) == SHARP_F) ? NULL : (STRING_ARG (1)))));
}

DEFINE_PRIMITIVE ("X-GRAPHICS-GRAB-ERROR-HANDLERS", Prim_x_graphics_grab_error_handlers, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_handler);
  PRIMITIVE_RETURN (UNSPECIFIC);
}
