/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/intext.h,v 1.2 1991/07/05 23:30:34 cph Exp $

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

#ifndef SCM_INTEXT_H
#define SCM_INTEXT_H

#include "ansidecl.h"
#include "dstack.h"

struct interruptable_extent
{
  PTR position;
  jmp_buf control_point;
  int interrupted;
};

extern struct interruptable_extent * current_interruptable_extent;
extern void EXFUN (initialize_interruptable_extent, (void));
extern void EXFUN (reset_interruptable_extent, (void));
extern struct interruptable_extent * EXFUN
  (enter_interruptable_extent, (void));
extern int EXFUN (enter_interruption_extent, (void));
extern void EXFUN (exit_interruption_extent, (void));

#define INTERRUPTABLE_EXTENT(result, expression)			\
{									\
  struct interruptable_extent * INTERRUPTABLE_EXTENT_frame =		\
    (enter_interruptable_extent ());					\
  if ((setjmp (INTERRUPTABLE_EXTENT_frame -> control_point)) == 0)	\
    {									\
      current_interruptable_extent = INTERRUPTABLE_EXTENT_frame;	\
      (result) = (expression);						\
    }									\
  else									\
    {									\
      errno = EINTR;							\
      (result) = (-1);							\
    }									\
  dstack_set_position (current_interruptable_extent -> position);	\
}

#endif /* SCM_INTEXT_H */
