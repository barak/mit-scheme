/* -*-C-*-

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/intercom.c,v 9.27 1989/09/20 23:09:24 cph Exp $
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
  long Which_Level;
  SCHEME_OBJECT work;
  SCHEME_OBJECT test;
  long Saved_Zone;
  PRIMITIVE_HEADER (3);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  Which_Level = (arg_index_integer (1, 4));
  work = (ARG_REF (2));		/* Why is this being ignored? -- CPH */
  test = (ARG_REF (3));
  Save_Time_Zone (Zone_Global_Int);
  Pop_Primitive_Frame (3);
 Will_Push (CONTINUATION_SIZE + STACK_ENV_EXTRA_SLOTS + 1);
  Store_Return (RC_FINISH_GLOBAL_INT);
  Store_Expression (LONG_TO_UNSIGNED_FIXNUM (Which_Level));
  Save_Cont ();
  Push (test);
  Push (STACK_FRAME_HEADER);
 Pushed ();
  Restore_Time_Zone ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
}

SCHEME_OBJECT
Global_Int_Part_2 (Which_Level, Do_It)
     SCHEME_OBJECT Which_Level;
     SCHEME_OBJECT Do_It;
{
  return (Do_It);
}

DEFINE_PRIMITIVE ("PUT-WORK", Prim_put_work, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT queue = (Get_Fixed_Obj_Slot (The_Work_Queue));
    if (queue == EMPTY_LIST)
      {
	queue = (cons (EMPTY_LIST, EMPTY_LIST));
	Set_Fixed_Obj_Slot (The_Work_Queue, queue);
      }
    {
      SCHEME_OBJECT queue_tail = (PAIR_CDR (queue));
      SCHEME_OBJECT new_entry = (cons ((ARG_REF (1)), EMPTY_LIST));
      SET_PAIR_CDR (queue, new_entry);
      if (queue_tail == EMPTY_LIST)
	SET_PAIR_CAR (queue, new_entry);
      else
	SET_PAIR_CDR (queue_tail, new_entry);
    }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PUT-WORK-IN-FRONT", Prim_put_work_in_front, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT queue = (Get_Fixed_Obj_Slot (The_Work_Queue));
    if (queue == EMPTY_LIST)
      {
	queue = (cons (EMPTY_LIST, EMPTY_LIST));
	Set_Fixed_Obj_Slot (The_Work_Queue, queue);
      }
    {
      SCHEME_OBJECT queue_head = (PAIR_CAR (queue));
      SCHEME_OBJECT new_entry = (cons ((ARG_REF (1)), queue_head));
      SET_PAIR_CAR (queue, new_entry);
      if (queue_head == EMPTY_LIST)
	SET_PAIR_CDR (queue, new_entry);
    }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DRAIN-WORK-QUEUE!", Prim_drain_queue, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    SCHEME_OBJECT queue = (Get_Fixed_Obj_Slot (The_Work_Queue));
    Set_Fixed_Obj_Slot (The_Work_Queue, EMPTY_LIST);
    PRIMITIVE_RETURN ((queue != EMPTY_LIST) ? (PAIR_CAR (queue)) : EMPTY_LIST);
  }
}

DEFINE_PRIMITIVE ("PEEK-AT-WORK-QUEUE", Prim_peek_queue, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    fast SCHEME_OBJECT queue = (Get_Fixed_Obj_Slot (The_Work_Queue));
    if (queue == EMPTY_LIST)
      PRIMITIVE_RETURN (EMPTY_LIST);
    /* Reverse the queue and return it.
       (Why is it being reversed? -- cph) */
    {
      fast SCHEME_OBJECT this_pair = (PAIR_CAR (queue));
      fast SCHEME_OBJECT result = EMPTY_LIST;
      while (this_pair != EMPTY_LIST)
	{
	  result = (cons ((PAIR_CAR (this_pair)), result));
	  this_pair = (PAIR_CDR (this_pair));
	}
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("GET-WORK", Prim_get_work, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT thunk = (ARG_REF (1));
    /* This gets this primitive's code which is in the expression register. */
    SCHEME_OBJECT primitive = (Regs [REGBLOCK_PRIMITIVE]);
    SCHEME_OBJECT queue = (Get_Fixed_Obj_Slot (The_Work_Queue));
    SCHEME_OBJECT queue_head =
      ((queue == EMPTY_LIST) ? EMPTY_LIST : (PAIR_CAR (queue)));
    if (queue_head == EMPTY_LIST)
      {
	if (thunk == SHARP_F)
	  {
	    fprintf (stderr,
		     "\nNo work available, but some has been requested!\n");
	    Microcode_Termination (TERM_EXIT);
	  }
	PRIMITIVE_CANONICALIZE_CONTEXT ();
	Pop_Primitive_Frame (1);
      Will_Push ((2 * (STACK_ENV_EXTRA_SLOTS + 1)) + 1 + CONTINUATION_SIZE);
	/* When the thunk returns, call the primitive again.
	   If there's still no work, we lose. */
	Push (SHARP_F);
	Push (primitive);
	Push (STACK_FRAME_HEADER + 1);
	Store_Expression (SHARP_F);
	Store_Return (RC_INTERNAL_APPLY);
	Save_Cont ();
	/* Invoke the thunk. */
	Push (thunk);
	Push (STACK_FRAME_HEADER);
      Pushed ();
	PRIMITIVE_ABORT (PRIM_APPLY);
      }
    {
      SCHEME_OBJECT result = (PAIR_CAR (queue_head));
      queue_head = (PAIR_CDR (queue_head));
      SET_PAIR_CAR (queue, queue_head);
      if (queue_head == EMPTY_LIST)
	SET_PAIR_CDR (queue, EMPTY_LIST);
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("AWAIT-SYNCHRONY", Prim_await_sync, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, PAIR_P);
  if (! (FIXNUM_P (PAIR_CDR (ARG_REF (1)))))
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("N-INTERPRETERS", Prim_n_interps, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
}

DEFINE_PRIMITIVE ("MY-PROCESSOR-NUMBER", Prim_my_proc, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
}

DEFINE_PRIMITIVE ("MY-INTERPRETER-NUMBER", Prim_my_interp_number, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
}

DEFINE_PRIMITIVE ("ZERO-ZONES", Prim_zero_zones, 0, 0, 0)
{
  long i;
  PRIMITIVE_HEADER (0);
#ifdef METERING
  for (i=0; i < Max_Meters; i++)
  {
    Time_Meters[i] = 0;
  }

  Old_Time=Sys_Clock();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* These are really used by GC on a true parallel machine */

DEFINE_PRIMITIVE ("GC-NEEDED?", Prim_gc_needed, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((Free + GC_Space_Needed) >= MemTop));
}

DEFINE_PRIMITIVE ("SLAVE-GC-BEFORE-SYNC", Prim_slave_before, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SLAVE-GC-AFTER-SYNC", Prim_slave_after, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("MASTER-GC-BEFORE-SYNC", Prim_master_before, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("MASTER-GC-LOOP", Prim_master_gc, 1, 1, 0)
{
  static SCHEME_OBJECT gc_prim = SHARP_F;
  extern SCHEME_OBJECT make_primitive ();
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT();
  /* This primitive caches the Scheme object for the garbage collector
     primitive so that it does not have to perform a potentially
     expensive search each time. */
  if (gc_prim == SHARP_F)
    gc_prim = (make_primitive ("GARBAGE-COLLECT"));
  {
    SCHEME_OBJECT argument = (ARG_REF (1));
    Pop_Primitive_Frame (1);
  Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
    Push (argument);
    Push (gc_prim);
    Push (STACK_FRAME_HEADER + 1);
  Pushed ();
    PRIMITIVE_ABORT (PRIM_APPLY);
  }
}
