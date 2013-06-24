/* -*-C-*-

$Id: sgx11.c,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Simple X11 graphics for HP 9000 series 300 machines. */

#include <X11/Xlib.h>
#include "scheme.h"
#include "prims.h"
#include "sgraph.h"

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
