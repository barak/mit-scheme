/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/bkpt.c,v 9.23 1989/09/20 23:06:19 cph Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* This file contains breakpoint utilities.
   Disabled when not debugging the interpreter. */

#include "scheme.h"

#ifndef ENABLE_DEBUGGING_TOOLS
#include "Error: Not debugging but bkpt.c included"
#endif

sp_record_list SP_List = sp_nil;

extern Boolean Add_a_Pop_Return_Breakpoint();

static struct sp_record One_Before =
{ ((SCHEME_OBJECT *) 0),
  sp_nil
};

Boolean Add_a_Pop_Return_Breakpoint(SP)
SCHEME_OBJECT *SP;
{ sp_record_list old = SP_List;
  SP_List = ((sp_record_list) malloc(sizeof(struct sp_record)));
  if (SP_List == sp_nil)
  { fprintf(stderr, "Could not allocate a breakpoint structure\n");
    SP_List = old;
    return false;
  }
  SP_List->sp = SP;
  SP_List->next = old;
  One_Before.next = SP_List;
  return true;
}

/* This uses register rather than fast because it is invoked
 * very often and would make things too slow.
 */

void Pop_Return_Break_Point()
{ fast SCHEME_OBJECT *SP = Stack_Pointer;
  fast sp_record_list previous = &One_Before;
  fast sp_record_list this = previous->next; /* = SP_List */
  for ( ;
       this != sp_nil;
       previous = this, this = this->next)
    if (this->sp == SP)
    { Handle_Pop_Return_Break();
      previous->next = this->next;
      break;
    }
  SP_List = One_Before.next;
  return;
}

/* A breakpoint can be placed here from a C debugger to examine
   the state of the world. */

extern Boolean Print_One_Continuation_Frame();

Handle_Pop_Return_Break()
{ Boolean ignore;
  SCHEME_OBJECT *Old_Stack = Stack_Pointer;

  printf("Pop Return Break: SP = 0x%x\n", Stack_Pointer);
  ignore = Print_One_Continuation_Frame();
  Stack_Pointer = Old_Stack;
  return;
}
