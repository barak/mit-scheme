/* -*-C-*-

$Id: bkpt.c,v 9.37 2007/04/22 16:31:22 cph Exp $

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

/* This file contains breakpoint utilities.
   Disabled when not debugging the interpreter. */

#include "scheme.h"

#ifdef ENABLE_DEBUGGING_TOOLS

#define sp_nil ((struct sp_record *) 0)

sp_record_list SP_List = sp_nil;

extern bool Add_a_Pop_Return_Breakpoint (SCHEME_OBJECT *);

static struct sp_record One_Before =
{
  ((SCHEME_OBJECT *) 0),
  sp_nil
};

bool
Add_a_Pop_Return_Breakpoint (SCHEME_OBJECT * SP)
{
  sp_record_list old = SP_List;
  SP_List = ((sp_record_list) (malloc (sizeof(struct sp_record))));

  if (SP_List == sp_nil)
  {
    fprintf (stderr, "Could not allocate a breakpoint structure\n");
    SP_List = old;
    return false;
  }
  SP_List->sp = SP;
  SP_List->next = old;
  One_Before.next = SP_List;
  return (true);
}

/* A breakpoint can be placed here from a C debugger to examine
   the state of the world. */

extern bool Print_One_Continuation_Frame (SCHEME_OBJECT);

void
Handle_Pop_Return_Break (void)
{
  SCHEME_OBJECT *Old_Stack = stack_pointer;

  printf ("Pop Return Break: SP = %#lx\n", ((unsigned long) stack_pointer));
  (void) (Print_One_Continuation_Frame (GET_RET));
  stack_pointer = Old_Stack;
}

void
Pop_Return_Break_Point (void)
{
  SCHEME_OBJECT * SP = stack_pointer;
  sp_record_list previous = &One_Before;
  sp_record_list this = previous->next; /* = SP_List */

  for ( ;
       this != sp_nil;
       previous = this, this = this->next)
  {
    if (this->sp == SP)
    {
      Handle_Pop_Return_Break ();
      previous->next = this->next;
      break;
    }
  }
  SP_List = One_Before.next;
}

#endif /* ENABLE_DEBUGGING_TOOLS */
