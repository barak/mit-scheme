/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/intercom.c,v 9.25 1988/08/15 20:49:47 cph Exp $
 *
 * Single-processor simulation of locking, propagating, and
 * communicating stuff.
 */

#include "scheme.h"
#include "prims.h"
#include "locks.h"
#include "zones.h"

#ifndef COMPILE_FUTURES
#include "Error: intercom.c is useless without COMPILE_FUTURES"
#endif

/* (GLOBAL-INTERRUPT LEVEL WORK TEST)

   There are 4 global interrupt levels, level 0 (highest priority)
   being reserved for GC.  See const.h for details of the dist-
   ribution of these bits with respect to local interrupt levels.

   Force all other processors to begin executing WORK (an interrupt
   handler [procedure of two arguments]) provided that TEST returns
   true.  TEST is supplied to allow this primitive to be restarted if it
   is unable to begin because another processor wins the race to
   generate a global interrupt and makes it no longer necessary that
   this processor generate one (TEST receives no arguments).  This
   primitive returns the value of the call to TEST (i.e. non-#!FALSE if
   the interrupt was really generated), and returns only after all other
   processors have begun execution of WORK (or TEST returns false).
*/

DEFINE_PRIMITIVE ("GLOBAL-INTERRUPT", Prim_send_global_interrupt, 3, 3, 0)
{
  long Saved_Zone, Which_Level;
  
  Primitive_3_Args();
  Arg_1_Type(TC_FIXNUM);
  Range_Check(Which_Level, Arg1, 0, 3, ERR_ARG_1_BAD_RANGE);
  Save_Time_Zone(Zone_Global_Int);
  Pop_Primitive_Frame(3);
 Will_Push(CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
  Store_Return(RC_FINISH_GLOBAL_INT);
  Store_Expression(Arg1);
  Save_Cont();
  Push(Arg3);
  Push(STACK_FRAME_HEADER);
 Pushed();
  Restore_Time_Zone();
  PRIMITIVE_ABORT(PRIM_APPLY);
  /*NOTREACHED*/
}

Pointer
Global_Int_Part_2(Which_Level, Do_It)
     Pointer Do_It, Which_Level;
{
  return Do_It;
}

DEFINE_PRIMITIVE ("PUT-WORK", Prim_put_work, 1, 1, 0)
{
  Pointer The_Queue, Queue_Tail, New_Entry;
  Primitive_1_Arg();

  The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
  if (The_Queue == NIL)
  {
    Primitive_GC_If_Needed(4);
    The_Queue = Make_Pointer(TC_LIST, Free);
    Set_Fixed_Obj_Slot(The_Work_Queue, The_Queue);
    *Free++ = NIL;
    *Free++ = NIL;
  }
  else
  {
    Primitive_GC_If_Needed(2);
  }
  Queue_Tail = Vector_Ref(The_Queue, CONS_CDR);
  New_Entry = Make_Pointer(TC_WEAK_CONS, Free);
  *Free++ = Arg1;
  *Free++ = NIL;
  Vector_Set(The_Queue, CONS_CDR, New_Entry);
  if (Queue_Tail == NIL)
  {
    Vector_Set(The_Queue, CONS_CAR, New_Entry);
  }
  else
  {
    Vector_Set(Queue_Tail, CONS_CDR, New_Entry);
  }
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("PUT-WORK-IN-FRONT", Prim_put_work_in_front, 1, 1, 0)
{
  Pointer The_Queue, Queue_Head, New_Entry;
  Primitive_1_Arg();

  The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
  if (The_Queue == NIL)
  { Primitive_GC_If_Needed(4);
    The_Queue = Make_Pointer(TC_LIST, Free);
    Set_Fixed_Obj_Slot(The_Work_Queue, The_Queue);
    *Free++ = NIL;
    *Free++ = NIL;
  }
  else
  {
    Primitive_GC_If_Needed(2);
  }

  Queue_Head = Vector_Ref(The_Queue, CONS_CDR);
  New_Entry = Make_Pointer(TC_WEAK_CONS, Free);
  *Free++ = Arg1;
  *Free++ = Queue_Head;
  Vector_Set(The_Queue, CONS_CAR, New_Entry);
  if (Queue_Head == NIL)
  {
    Vector_Set(The_Queue, CONS_CDR, New_Entry);
  }
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("DRAIN-WORK-QUEUE!", Prim_drain_queue, 0, 0, 0)
{
  Pointer The_Queue;
  Primitive_0_Args();

  The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
  Set_Fixed_Obj_Slot(The_Work_Queue, NIL);
  PRIMITIVE_RETURN((The_Queue != NIL) ?
		   Vector_Ref(The_Queue, CONS_CAR) :
		   NIL);
}

DEFINE_PRIMITIVE ("PEEK-AT-WORK-QUEUE", Prim_peek_queue, 0, 0, 0)
{
  Pointer The_Queue, This_Cons, Last_Cons;
  Primitive_0_Args();

  The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
  if (The_Queue == NIL) return NIL;

  Last_Cons = NIL;
  for (The_Queue = Vector_Ref(The_Queue, CONS_CAR);
       The_Queue != NIL;
       The_Queue = Vector_Ref(The_Queue, CONS_CDR))
  {
    Primitive_GC_If_Needed(2);
    This_Cons = Make_Pointer(TC_LIST, Free);
    *Free++ = Vector_Ref(The_Queue, CONS_CAR);
    *Free++ = Last_Cons;
    Last_Cons = This_Cons;
  }

  PRIMITIVE_RETURN(This_Cons);
}

DEFINE_PRIMITIVE ("GET-WORK", Prim_get_work, 1, 1, 0)
{
  Pointer Get_Work();
  Primitive_1_Arg();

  PRIMITIVE_RETURN(Get_Work(Arg1));
}

Pointer Get_Work(Arg1)
     Pointer Arg1;
{
  Pointer The_Queue, Queue_Head, Result, The_Prim;

  /* This gets this primitive's code which is in the expression register. */
  The_Prim = Fetch_Expression();
  The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
  if (The_Queue != NIL)
  {
    Queue_Head = Vector_Ref(The_Queue, CONS_CAR);
  }
  if ((The_Queue == NIL) || (Queue_Head == NIL))
    if (Arg1 == NIL)
    {
      printf("\nNo work available, but some has been requested!\n");
      Microcode_Termination(TERM_EXIT);
    }
    else
    {
      Pop_Primitive_Frame(1);
     Will_Push(2 * (STACK_ENV_EXTRA_SLOTS + 1) + 1 + CONTINUATION_SIZE);
      Push(NIL);	/* Upon return, no hope if there is no work */
      Push(The_Prim);
      Push(STACK_FRAME_HEADER+1);
      Store_Expression(NIL);
      Store_Return(RC_INTERNAL_APPLY);
      Save_Cont();
      Push(Arg1);
      Push(STACK_FRAME_HEADER);
     Pushed();
      PRIMITIVE_ABORT(PRIM_APPLY);
  }
  Result = Vector_Ref(Queue_Head, CONS_CAR);
  Queue_Head = Vector_Ref(Queue_Head, CONS_CDR);
  Vector_Set(The_Queue, CONS_CAR, Queue_Head);
  if (Queue_Head == NIL)
  {
    Vector_Set(The_Queue, CONS_CDR, NIL);
  }
  return (Result);
}

DEFINE_PRIMITIVE ("AWAIT-SYNCHRONY", Prim_await_sync, 1, 1, 0)
{
  Primitive_1_Arg();

  Arg_1_Type(TC_LIST);
  if (Type_Code(Vector_Ref(Arg1, CONS_CDR)) != TC_FIXNUM)
  {
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  }
  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("N-INTERPRETERS", Prim_n_interps, 0, 0, 0)
{
  Primitive_0_Args();

  PRIMITIVE_RETURN(MAKE_UNSIGNED_FIXNUM(1));
}

DEFINE_PRIMITIVE ("MY-PROCESSOR-NUMBER", Prim_my_proc, 0, 0, 0)
{
  Primitive_0_Args();

  PRIMITIVE_RETURN(MAKE_UNSIGNED_FIXNUM(0));
}

DEFINE_PRIMITIVE ("MY-INTERPRETER-NUMBER", Prim_my_interp_number, 0, 0, 0)
{
  Primitive_0_Args();

  PRIMITIVE_RETURN(MAKE_UNSIGNED_FIXNUM(0));
}

DEFINE_PRIMITIVE ("ZERO-ZONES", Prim_zero_zones, 0, 0, 0)
{
  long i;
  Primitive_0_Args();

#ifdef METERING
  for (i=0; i < Max_Meters; i++)
  {
    Time_Meters[i] = 0;
  }

  Old_Time=Sys_Clock();
#endif
  PRIMITIVE_RETURN(SHARP_T);
}

/* These are really used by GC on a true parallel machine */

DEFINE_PRIMITIVE ("GC-NEEDED?", Prim_gc_needed, 0, 0, 0)
{
  Primitive_0_Args();

  if ((Free + GC_Space_Needed) >= MemTop)
  {
    PRIMITIVE_RETURN(SHARP_T);
  }
  else
  {
    PRIMITIVE_RETURN(NIL);
  }
}

DEFINE_PRIMITIVE ("SLAVE-GC-BEFORE-SYNC", Prim_slave_before, 0, 0, 0)
{
  Primitive_0_Args();

  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("SLAVE-GC-AFTER-SYNC", Prim_slave_after, 0, 0, 0)
{
  Primitive_0_Args();

  PRIMITIVE_RETURN(SHARP_T);
}

DEFINE_PRIMITIVE ("MASTER-GC-BEFORE-SYNC", Prim_master_before, 0, 0, 0)
{
  Primitive_0_Args();

  PRIMITIVE_RETURN(SHARP_T);
}

/* This primitive caches the Scheme object for the garbage collector
   primitive so that it does not have to perform a potentially
   expensive search each time.
*/

DEFINE_PRIMITIVE ("MASTER-GC-LOOP", Prim_master_gc, 1, 1, 0)
{
  static Pointer gc_prim = NIL;
  extern Pointer make_primitive();
  Primitive_1_Arg();

  if (gc_prim == NIL)
  {
    gc_prim = make_primitive("GARBAGE-COLLECT");
  }
  Pop_Primitive_Frame(1);
 Will_Push(STACK_ENV_EXTRA_SLOTS + 2);
  Push(Arg1);
  Push(gc_prim);
  Push(STACK_FRAME_HEADER + 1);
 Pushed();
  PRIMITIVE_ABORT(PRIM_APPLY);
}
