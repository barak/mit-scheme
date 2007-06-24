/* -*-C-*-

$Id: ostty.c,v 1.9 2007/04/22 16:31:23 cph Exp $

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

#include "ostty.h"
#include "osscheme.h"
#include "prims.h"

static cc_t next_interrupt_char;

void
tty_set_next_interrupt_char (cc_t c)
{
  if (next_interrupt_char == '\0')
    {
      next_interrupt_char = c;
      request_character_interrupt ();
    }
}

cc_t
OS_tty_next_interrupt_char (void)
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
