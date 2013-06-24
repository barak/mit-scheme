/* -*-C-*-

$Id: ostty.c,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1990, 1999 Massachusetts Institute of Technology

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

#include "ostty.h"
#include "osscheme.h"

static cc_t next_interrupt_char;

void
DEFUN (tty_set_next_interrupt_char, (c), cc_t c)
{
  if (next_interrupt_char == '\0')
    {
      next_interrupt_char = c;
      request_character_interrupt ();
    }
}

cc_t
DEFUN_VOID (OS_tty_next_interrupt_char)
{
  if (next_interrupt_char == '\0')
    error_external_return ();
  {
    /* The interrupt character is mapped here. This provides a
       chance for OS's which have only one interrupt character
       and can't do I/O during an interrupt to request the
       interrupt character in a safe place.
     */
    cc_t result = OS_tty_map_interrupt_char(next_interrupt_char);
    next_interrupt_char = '\0';
    return (result);
  }
}
