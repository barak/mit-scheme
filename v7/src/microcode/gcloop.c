/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/gcloop.c,v 9.21 1987/01/22 14:26:27 jinx Exp $
 *
 * This file contains the code for the most primitive part
 * of garbage collection.
 *
 */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"

#ifndef butterfly

#define GC_Pointer(Code)					\
Old = Get_Pointer(Temp);					\
Code

#define Setup_Pointer_for_GC(Extra_Code)			\
GC_Pointer(Setup_Pointer(true, Extra_Code))

Pointer *GCLoop(Scan, To_Pointer)
fast Pointer *Scan;
Pointer **To_Pointer;
{ fast Pointer *To, *Old, Temp, *Low_Constant, New_Address;

  To = *To_Pointer;
  Low_Constant = Constant_Space;
  if (GC_Debug)
  { fprintf(stderr, "Starting scan at 0x%08x\n", Scan);
    if (Low_Watch == ((Pointer *) NULL))
    { fprintf(stderr, "Enter low watch range and high watch range: ");
      scanf("%x %x", &Low_Watch, &High_Watch);
    }
  }

  for ( ; Scan != To; Scan++)
  { Temp = *Scan;

    if (GC_Debug)
    { In_Range = (((Scan >= Low_Watch) && (Scan <= High_Watch)) ||
		  ((Free >= Low_Watch) && (Free <= High_Watch)));
      if (In_Range)
	fprintf(stderr,  "0x%08x: %02x|%06x ... ",
	       Scan, Type_Code(Temp), Get_Integer(Temp));
    }

/* GCLoop continues on the next page */

/* GCLoop, continued */

    Switch_by_GC_Type(Temp)
    { case TC_BROKEN_HEART:
        if (Scan == (Get_Pointer(Temp)))
	{ *To_Pointer = To;
	  return Scan;
	}
        fprintf(stderr, "GC: Broken heart in scan.\n");
	Microcode_Termination(TERM_BROKEN_HEART);

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += Get_Integer(Temp);
	if (GC_Debug && In_Range)
	  fprintf(stderr,  "skipping %d cells.", Get_Integer(Temp));
	break;

      case_Non_Pointer:
	if (GC_Debug && In_Range) fprintf(stderr, "not a pointer.");
	break;

      case_compiled_entry_point:
	GC_Pointer(Setup_Internal(true,
				  Transport_Compiled(),
				  Compiled_BH(true, continue)));

      case_Cell:
	Setup_Pointer_for_GC(Transport_Cell());

      case_Pair:
	Setup_Pointer_for_GC(Transport_Pair());

      case_Triple:
	Setup_Pointer_for_GC(Transport_Triple());

      case TC_VARIABLE:
	Setup_Pointer_for_GC(Transport_Variable());	

/* GCLoop continues on the next page */

/* GCLoop, continued */

#ifdef QUADRUPLE
      case_Quadruple:
	Setup_Pointer_for_GC(Transport_Quadruple());
#endif

#ifdef FLOATING_ALIGNMENT
      case TC_BIG_FLONUM:
	Setup_Pointer_for_GC(Transport_Flonum());
#else
      case TC_BIG_FLONUM:
	/* Fall through */
#endif
      case_Vector:
	Setup_Pointer_for_GC(Transport_Vector());

      case TC_FUTURE:
	Setup_Pointer_for_GC(Transport_Future());

      case TC_WEAK_CONS:
	Setup_Pointer_for_GC(Transport_Weak_Cons());

      default:
	fprintf(stderr,
		"GCLoop: Bad type code = 0x%02x\n",
		Type_Code(Temp));
	Invalid_Type_Code();

      }	/* Switch_by_GC_Type */
    if (GC_Debug && In_Range) fprintf(stderr, "\n");
  } /* For loop */
  *To_Pointer = To;
  return To;
} /* GCLoop */

/* Flip into unused heap */

void GCFlip()
{ Pointer *Temp;
  Temp = Unused_Heap;
  Unused_Heap = Heap_Bottom;
  Heap_Bottom = Temp;
  Temp = Unused_Heap_Top;
  Unused_Heap_Top = Heap_Top;
  Heap_Top = Temp;
  Free = Heap_Bottom;
  Set_Mem_Top(Heap_Top - GC_Reserve);
  Weak_Chain = NIL;
}

/* Here is the code which "prunes" objects from weak cons cells.  See
   the picture in gccode.h for a description of the structure built by
   the GC.  This code follows the chain of weak cells (in old space) and
   either updates the new copy's CAR with the relocated version of the
   object, or replaces it with NIL.

   This code could be implemented as a GC daemon, just like
   REHASH-GC-DAEMON, but there is no "good" way of getting Weak_Chain
   to it.  Note that Weak_Chain points to Old Space unless no weak
   conses were found.

   This code should be reimplemented so it does not need to look at both
   old and new space at the same time.  Only the "real" garbage collector
   should be allowed to do that.
*/

void Fix_Weak_Chain()
{ fast Pointer *Old_Weak_Cell, *Scan, Old_Car, Temp, *Old, *Low_Constant;
  Low_Constant = Constant_Space;
  while (Weak_Chain != NIL)
  { Old_Weak_Cell = Get_Pointer(Weak_Chain);
    Scan = Get_Pointer(*Old_Weak_Cell++);
    Weak_Chain = *Old_Weak_Cell;
    Old_Car = *Scan;
    Temp = Make_New_Pointer(Type_Code(Weak_Chain), Old_Car);
    Weak_Chain = Make_New_Pointer(TC_NULL, Weak_Chain);

    switch(GC_Type(Temp))
    { case GC_Non_Pointer:
        *Scan = Temp;
	continue;

      /* Normal pointer types, the broken heart is in the first word.
         Note that most special types are treated normally here.
	 The BH code updates *Scan if the object has been relocated.
	 Otherwise it falls through and we replace it with a full NIL.
	 Eliminating this assignment would keep old data (pl. of datum).
       */

      case GC_Cell:
      case GC_Pair:
      case GC_Triple:
      case GC_Quadruple:
      case GC_Vector:
	Old = Get_Pointer(Old_Car);
	if (Old >= Low_Constant)
	{ *Scan = Temp;
	  continue;
	}
	Normal_BH(false, continue);
	*Scan = NIL;
	continue;

      case GC_Compiled:
	Old = Get_Pointer(Old_Car);
	if (Old >= Low_Constant)
	{ *Scan = Temp;
	  continue;
	}
	Compiled_BH(false, continue);
	*Scan = NIL;
	continue;

      case GC_Special:
      case GC_Undefined:
      default:			/* Non Marked Headers and Broken Hearts */
        fprintf(stderr,
		"\nFix_Weak_Chain: Bad Object: Type = 0x%02x; Datum = %x\n",
		Type_Code(Temp), Datum(Temp));
	Microcode_Termination(TERM_INVALID_TYPE_CODE);
    }
  }
  return;
}

/* Here is the set up for the full garbage collection:

   - First it makes the constant space and stack into one large area
   by "hiding" the gap between them with a non-marked header.
   
   - Then it saves away all the relevant microcode registers into new
   space, making this the root for garbage collection.

   - Then it does the actual garbage collection in 4 steps:
     1) Trace constant space.
     2) Trace objects pointed out by the root and constant space.
     3) Trace the Precious objects, remembering where consing started.
     4) Update all weak pointers.

   - Finally it restores the microcode registers from the copies in
   new space.
*/

void GC()
{ Pointer *Root, *Result, *Check_Value,
  	  The_Precious_Objects, *Root2;

  /* Save the microcode registers so that they can be relocated */
  Terminate_Old_Stacklet();
  Terminate_Constant_Space(Check_Value);

  Root = Free;
  The_Precious_Objects = Get_Fixed_Obj_Slot(Precious_Objects);
  Set_Fixed_Obj_Slot(Precious_Objects, NIL);
  Set_Fixed_Obj_Slot(Lost_Objects_Base, NIL);

  *Free++ = Fixed_Objects;
  *Free++ = Make_Pointer(TC_HUNK3, History);
  *Free++ = Undefined_Externals;
  *Free++ = Get_Current_Stacklet();
  *Free++ = ((Previous_Restore_History_Stacklet == NULL) ?
	     NIL :
	     Make_Pointer(TC_CONTROL_POINT, Previous_Restore_History_Stacklet));
  *Free++ = Current_State_Point;
  *Free++ = Fluid_Bindings;

  /* The 4 step GC */
  Result = GCLoop(Constant_Space, &Free);
  if (Result != Check_Value)
  { fprintf(stderr, "\nGC: Constant Scan ended too early.\n");
    Microcode_Termination(TERM_BROKEN_HEART);
  }
  Result = GCLoop(Root, &Free);
  if (Free != Result)
  { fprintf(stderr, "\nGC-1: Heap Scan ended too early.\n");
    Microcode_Termination(TERM_BROKEN_HEART);
  }
  Root2 = Free;
  *Free++ = The_Precious_Objects;
  Result = GCLoop(Root2, &Free);
  if (Free != Result)
  { fprintf(stderr, "\nGC-2: Heap Scan ended too early.\n");
    Microcode_Termination(TERM_BROKEN_HEART);
  }
  Fix_Weak_Chain();

  /* Make the microcode registers point to the copies in new-space. */
  Fixed_Objects = *Root++;
  Set_Fixed_Obj_Slot(Precious_Objects, *Root2);
  Set_Fixed_Obj_Slot(Lost_Objects_Base, Make_Pointer(TC_ADDRESS, Root2));

  History = Get_Pointer(*Root++);
  Undefined_Externals = *Root++;
  Set_Current_Stacklet(*Root);
  Root += 1;			/* Set_Current_Stacklet is sometimes a No-Op! */
  if (*Root == NIL)
  { Previous_Restore_History_Stacklet = NULL;
    Root += 1;
  }
  else Previous_Restore_History_Stacklet = Get_Pointer(*Root++);
  Current_State_Point = *Root++;
  Fluid_Bindings = *Root++;
  Free_Stacklets = NULL;
  return;
}

/* (GARBAGE_COLLECT SLACK)
      [Primitive number 0x3A]
      Requests a garbage collection leaving the specified amount of slack
      for the top of heap check on the next GC.  The primitive ends by invoking
      the GC daemon process if there is one.
*/

Built_In_Primitive(Prim_Garbage_Collect, 1, "GARBAGE-COLLECT")
{ Pointer GC_Daemon_Proc;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  if (Free > Heap_Top)
  { fprintf(stderr, "\nGC has been delayed too long, and you are truly out of room!\n");
    fprintf(stderr, "Free=0x%x, MemTop=0x%x, Heap_Top=0x%x\n", Free, MemTop, Heap_Top);
    Microcode_Termination(TERM_EXIT);
  }
  GC_Reserve = Get_Integer(Arg1);
  GCFlip();
  Weak_Chain = NULL;
  GC();
  IntCode &= ~INT_GC;
  if (GC_Check(GC_Space_Needed))
  { fprintf(stderr, "\nGC just ended.  The free pointer is at 0x%x, the top of this heap\n",
	   Free);
    fprintf(stderr, "is at 0x%x, and we are trying to cons 0x%x objects.  Dead!\n",
	   MemTop, GC_Space_Needed);
    Microcode_Termination(TERM_EXIT);
  }
  GC_Daemon_Proc = Get_Fixed_Obj_Slot(GC_Daemon);
  if (GC_Daemon_Proc == NIL) return FIXNUM_0 + (MemTop - Free);
  Pop_Primitive_Frame(1);
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+1));
  Store_Return(RC_NORMAL_GC_DONE);
  Store_Expression(FIXNUM_0 + (MemTop - Free));
  Save_Cont();
  Push(GC_Daemon_Proc);
  Push(STACK_FRAME_HEADER);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
  /* The following comment is by courtesy of LINT, your friendly sponsor. */
  /*NOTREACHED*/
}
#endif butterfly

/* (GET_NEXT_CONSTANT)
      [Primitive number 0xE4]
      Returns the next free address in constant space.
*/
Built_In_Primitive(Prim_Get_Next_Constant, 0, "GET-NEXT-CONSTANT")
{ Pointer *Next_Address = Free_Constant+1;
  Primitive_0_Args();
  return Make_Pointer(TC_ADDRESS, Next_Address);
}

/* (GC_TYPE OBJECT)
      [Primitive number 0xBC]
      Returns a fixnum indicating the GC type of the object.  The object
      is NOT touched first.
*/

Built_In_Primitive(Prim_Gc_Type, 1, "GC-TYPE")
{ Primitive_1_Arg(); 
  return Make_Non_Pointer(TC_FIXNUM, GC_Type(Arg1));
}
