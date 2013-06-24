/* -*-C-*-

$Id: bkpt.c,v 9.30 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* This file contains breakpoint utilities.
   Disabled when not debugging the interpreter. */

#include "scheme.h"

#ifdef ENABLE_DEBUGGING_FLAGS

#define sp_nil ((struct sp_record *) 0)

sp_record_list SP_List = sp_nil;

extern Boolean EXFUN (Add_a_Pop_Return_Breakpoint, (SCHEME_OBJECT *));

static struct sp_record One_Before =
{
  ((SCHEME_OBJECT *) 0),
  sp_nil
};

Boolean
DEFUN (Add_a_Pop_Return_Breakpoint, (SP), SCHEME_OBJECT * SP)
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

extern Boolean EXFUN (Print_One_Continuation_Frame, (SCHEME_OBJECT));

void
DEFUN_VOID (Handle_Pop_Return_Break)
{
  SCHEME_OBJECT *Old_Stack = Stack_Pointer;

  printf ("Pop Return Break: SP = 0x%lx\n", ((long) Stack_Pointer));
  (void) (Print_One_Continuation_Frame (Return));
  Stack_Pointer = Old_Stack;
  return;
}

void
DEFUN_VOID (Pop_Return_Break_Point)
{
  fast SCHEME_OBJECT *SP = Stack_Pointer;
  fast sp_record_list previous = &One_Before;
  fast sp_record_list this = previous->next; /* = SP_List */

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
  return;
}

#else
/* Not ENABLE_DEBUGGING_FLAGS */
#endif
