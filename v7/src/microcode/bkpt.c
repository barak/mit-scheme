/* -*-C-*-

$Id: bkpt.c,v 9.31 2002/07/02 18:37:39 cph Exp $

Copyright (c) 1987-1999, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
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
  SCHEME_OBJECT *Old_Stack = sp_register;

  printf ("Pop Return Break: SP = 0x%lx\n", ((long) sp_register));
  (void) (Print_One_Continuation_Frame (Return));
  sp_register = Old_Stack;
  return;
}

void
DEFUN_VOID (Pop_Return_Break_Point)
{
  SCHEME_OBJECT * SP = sp_register;
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
  return;
}

#else
/* Not ENABLE_DEBUGGING_FLAGS */
#endif
