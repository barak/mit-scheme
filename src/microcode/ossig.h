/* -*-C-*-

$Id: ossig.h,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

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

#ifndef SCM_OSSIG_H
#define SCM_OSSIG_H

#include "os.h"

enum interrupt_handler
{
  interrupt_handler_ignore,
  interrupt_handler_default,
  interrupt_handler_unknown,
  interrupt_handler_terminate,
  interrupt_handler_stop,
  interrupt_handler_interactive,
  interrupt_handler_control_b,
  interrupt_handler_control_g,
  interrupt_handler_control_u,
  interrupt_handler_control_x
};

#endif /* SCM_OSSIG_H */
