/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

#include "intext.h"

extern void preserve_signal_mask (void);

struct interruptable_extent * current_interruptable_extent;

void
initialize_interruptable_extent (void)
{
  current_interruptable_extent = 0;
}

void
reset_interruptable_extent (void)
{
  current_interruptable_extent = 0;
}

struct interruptable_extent *
enter_interruptable_extent (void)
{
  void * position = dstack_position;
  struct interruptable_extent * frame;
  /* Inside the interrupt handler, the signal mask will be different.
     Push a winding frame that will restore it to its current value.
     Do this before any other changes so that the other changes are
     undone before the signal mask is restored (possibly causing
     another interrupt).  */
  preserve_signal_mask ();
  frame = (dstack_alloc (sizeof (struct interruptable_extent)));
  (frame -> position) = position;
  (frame -> interrupted) = 0;
  /* Create a dynamic binding frame but don't assign the new frame to
     it until the setjmp has been done. */
  dstack_bind ((&current_interruptable_extent), current_interruptable_extent);
  return (frame);
}

/* It is possible that two signals arriving close together could both
   set `interrupted'.  This does not matter, because the signal
   handlers haven't done anything at this point, and the net effect is
   to cause the second signal handler to do the longjmp, rather than
   the first.  However, the first signal handler never runs, which may
   be a problem for some applications. */

int
enter_interruption_extent (void)
{
  if ((current_interruptable_extent == 0)
      || (current_interruptable_extent -> interrupted))
    return (0);
  (current_interruptable_extent -> interrupted) = 1;
  return (1);
}

void
exit_interruption_extent (void)
{
  longjmp ((current_interruptable_extent -> control_point), 1);
}
