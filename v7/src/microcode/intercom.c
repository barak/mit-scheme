/*          Hey EMACS, this is -*- C -*- code!                 */

/****************************************************************
*                                                               *
*                         Copyright (c) 1985                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: INTERCOM.C
 * Single-processor simulation of locking, propagating, and
 * communicating stuff.
 */

#include "scheme.h"
#include "primitive.h"
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

Define_Primitive(Prim_Send_Global_Interrupt, 3, "GLOBAL-INTERRUPT")
{ long Saved_Zone, Which_Level;
  
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
  longjmp(*Back_To_Eval, PRIM_APPLY);
}

Pointer Global_Int_Part_2(Which_Level, Do_It)
Pointer Do_It, Which_Level;
{ return Do_It;
}

Define_Primitive(Prim_Put_Work, 1, "PUT-WORK")
{ Pointer The_Queue, Queue_Tail, New_Entry;
  Primitive_1_Arg();

  The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
  if (The_Queue==NIL)
  { Primitive_GC_If_Needed(4);
    The_Queue = Make_Pointer(TC_LIST, Free);
    Set_Fixed_Obj_Slot(The_Work_Queue, The_Queue);
    *Free++ = NIL;
    *Free++ = NIL;
  }
  else Primitive_GC_If_Needed(2);
  Queue_Tail = Vector_Ref(The_Queue, CONS_CDR);
  New_Entry = Make_Pointer(TC_WEAK_CONS, Free);
  *Free++ = Arg1;
  *Free++ = NIL;
  Vector_Set(The_Queue, CONS_CDR, New_Entry);
  if (Queue_Tail==NIL) Vector_Set(The_Queue, CONS_CAR, New_Entry);
  else Vector_Set(Queue_Tail, CONS_CDR, New_Entry);
  return TRUTH;
}

Define_Primitive(Prim_Drain_Queue, 0, "DRAIN-WORK-QUEUE!")
{ Pointer The_Queue;
  Primitive_0_Args();

  The_Queue = Get_Fixed_Obj_Slot(The_Work_Queue);
  Set_Fixed_Obj_Slot(The_Work_Queue, NIL);
  return (The_Queue != NIL) ? Vector_Ref(The_Queue, CONS_CAR) : NIL;
}

Define_Primitive(Prim_Await_Sync, 1, "AWAIT-SYNCHRONY")
{ Primitive_1_Arg();

  Arg_1_Type(TC_LIST);
  if (Type_Code(Vector_Ref(Arg1, CONS_CDR)) != TC_FIXNUM)
    Primitive_Error(ERR_ARG_1_BAD_RANGE);
  return TRUTH;
}

Define_Primitive(Prim_N_Interps, 0, "N-INTERPRETERS")
{ Primitive_0_Args();
  return FIXNUM_0 + 1;
}

Define_Primitive(Prim_My_Proc, 0, "MY-PROCESSOR-NUMBER")
{ Primitive_0_Args();
  return FIXNUM_0;
}

Define_Primitive(Prim_My_Interp_Number, 0, "MY-INTERPRETER-NUMBER")
{ Primitive_0_Args();
  return FIXNUM_0;
}

Define_Primitive(Prim_Zero_Zones, 0, "ZERO-ZONES")
{ long i;
  Primitive_0_Args();
#ifdef METERING
  for (i=0; i < Max_Meters; i++) Time_Meters[i]=0;
  Old_Time=Sys_Clock();
#endif
  return TRUTH;
}

/* These are really used by GC on a true parallel machine */

Define_Primitive(Prim_GC_Needed, 0, "GC-NEEDED?")
{ Primitive_0_Args();
  if ((Free+GC_Space_Needed) >= MemTop) return TRUTH;
  else return NIL;
}

Define_Primitive(Prim_Slave_Before, 0, "SLAVE-GC-BEFORE-SYNC")
{ Primitive_0_Args();
  return TRUTH;
}

Define_Primitive(Prim_Slave_After, 0, "SLAVE-GC-AFTER-SYNC")
{ Primitive_0_Args();
  return TRUTH;
}

Define_Primitive(Prim_Master_Before, 0, "MASTER-GC-BEFORE-SYNC")
{ Primitive_0_Args();
  return TRUTH;
}

Define_Primitive(Prim_Master_GC, 1, "MASTER-GC-LOOP")
{ Primitive_1_Arg();
  Pop_Primitive_Frame(1);
 Will_Push(STACK_ENV_EXTRA_SLOTS + 2);
  Push(Arg1);
  Push(Make_Non_Pointer(TC_PRIMITIVE, PC_GARBAGE_COLLECT));
  Push(STACK_FRAME_HEADER + 1);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
}


