/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/intext.c,v 1.3 1991/07/05 23:30:46 cph Exp $

Copyright (c) 1990-91 Massachusetts Institute of Technology

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

#include "ansidecl.h"
#include "dstack.h"
#include "intext.h"

extern void EXFUN (preserve_signal_mask, (void));

struct interruptable_extent * current_interruptable_extent;

void
DEFUN_VOID (initialize_interruptable_extent)
{
  current_interruptable_extent = 0;
}

void
DEFUN_VOID (reset_interruptable_extent)
{
  current_interruptable_extent = 0;
}

struct interruptable_extent *
DEFUN_VOID (enter_interruptable_extent)
{
  PTR position = dstack_position;
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
DEFUN_VOID (enter_interruption_extent)
{
  if ((current_interruptable_extent == 0)
      || (current_interruptable_extent -> interrupted))
    return (0);
  (current_interruptable_extent -> interrupted) = 1;
  return (1);
}

void
DEFUN_VOID (exit_interruption_extent)
{
  longjmp ((current_interruptable_extent -> control_point), 1);
}
