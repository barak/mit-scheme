/* -*-C-*-

$Id: intext.h,v 1.6 2002/11/20 19:46:09 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

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
  int saved_errno;							\
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
  saved_errno = errno;							\
  dstack_set_position (current_interruptable_extent -> position);	\
  errno = saved_errno;							\
}

#endif /* SCM_INTEXT_H */
