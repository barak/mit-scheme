/* -*-C-*-

$Id: starbasx.c,v 1.6 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1989, 1990, 1999 Massachusetts Institute of Technology

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

/* Starbase/X11 interface */

#include "scheme.h"
#include "prims.h"
#include "x11.h"
#include <starbase.c.h>

DEFINE_PRIMITIVE ("X11-WINDOW-STARBASE-FILENAME", Prim_x11_window_starbase_filename, 1, 1,
  "Given a window, returns the name of a file which can be opened\n\
as a Starbase graphics device.")
{
  PRIMITIVE_HEADER (1);
  {
    struct xwindow * xw = (x_window_arg (1));
    char * starbase_filename =
      (make_X11_gopen_string ((XW_DISPLAY (xw)), (XW_WINDOW (xw))));
    PRIMITIVE_RETURN
      ((starbase_filename == 0)
       ? SHARP_F
       : (char_pointer_to_string (starbase_filename)));
  }
}
