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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/memmag.c,v 9.30 1987/06/23 22:00:09 cph Rel $ */

/* Memory management top level.

   The memory management code is spread over 3 files:
   - memmag.c: initialization.
   - gcloop.c: main garbage collector loop.
   - purify.c: constant/pure space hacking.
   There is also a relevant header file, gccode.h.

   The object dumper, fasdump, shares properties and code with the
   memory management utilities.
 */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"

/* Imports */

extern Pointer *GCLoop();

/* Exports */

extern void GCFlip(), GC();
extern void Clear_Memory(), Setup_Memory(), Reset_Memory();

/* 	Memory Allocation, sequential processor:

   ------------------------------------------
   |         Control Stack        ||        |
   |                              \/        |
   ------------------------------------------
   |     Constant + Pure Space    /\        |
   |                              ||        |
   ------------------------------------------
   |                                        |
   |           Heap Space                   |
   ------------------------------------------
  
   Each area has a pointer to its starting address and a pointer to the
   next free cell.  In addition, there is a pointer to the top of the
   useable area of the heap (the heap is subdivided into two areas for
   the purposes of GC, and this pointer indicates the top of the half
   currently in use).

*/

/* Initialize free pointers within areas. Stack_Pointer is
   special: it always points to a cell which is in use. */

void
Clear_Memory (Our_Heap_Size, Our_Stack_Size, Our_Constant_Size)
     int Our_Heap_Size, Our_Stack_Size, Our_Constant_Size;
{
  Heap_Top = (Heap_Bottom + Our_Heap_Size);
  Local_Heap_Base = Heap_Bottom;
  Unused_Heap_Top = (Heap_Bottom + (2 * Our_Heap_Size));
  Set_Mem_Top (Heap_Top - GC_Reserve);
  Free = Heap_Bottom;
  Constant_Top = (Constant_Space + Our_Constant_Size);
  Free_Constant = Constant_Space;
  Set_Pure_Top ();
  Initialize_Stack ();
  return;
}

/* This procedure allocates and divides the total memory. */

void
Setup_Memory(Our_Heap_Size, Our_Stack_Size, Our_Constant_Size)
     int Our_Heap_Size, Our_Stack_Size, Our_Constant_Size;
{
  /* Consistency check 1 */
  if (Our_Heap_Size == 0)
  {
    fprintf(stderr, "Configuration won't hold initial data.\n");
    exit(1);
  }

  /* Allocate */
  Highest_Allocated_Address = 
    Allocate_Heap_Space(Stack_Allocation_Size(Our_Stack_Size) + 
	                (2 * Our_Heap_Size) +
			Our_Constant_Size +
			HEAP_BUFFER_SPACE);

  /* Consistency check 2 */
  if (Heap == NULL)
  {
    fprintf(stderr, "Not enough memory for this configuration.\n");
    exit(1);
  }

  /* Initialize the various global parameters */
  Heap += HEAP_BUFFER_SPACE;
  Initial_Align_Float(Heap);
  Unused_Heap = Heap + Our_Heap_Size;
  Align_Float(Unused_Heap);
  Constant_Space = Heap + 2*Our_Heap_Size;
  Align_Float(Constant_Space);

  /* Consistency check 3 */
  if (((C_To_Scheme(Highest_Allocated_Address)) & TYPE_CODE_MASK) != 0)
  {
    fprintf(stderr,
	    "Largest address does not fit in datum field of Pointer.\n");
    fprintf(stderr,
	    "Allocate less space or re-compile without Heap_In_Low_Memory.\n");
    exit(1);
  }

  Heap_Bottom = Heap;
  Clear_Memory(Our_Heap_Size, Our_Stack_Size, Our_Constant_Size);
  return;
}

/* In this version, this does nothing. */

void
Reset_Memory()
{
  return;
}

/* Utilities for the garbage collector top level.
   The main garbage collector loop is in gcloop.c
*/

/* Flip into unused heap */

void
GCFlip()
{
  Pointer *Temp;

  Temp = Unused_Heap;
  Unused_Heap = Heap_Bottom;
  Heap_Bottom = Temp;
  Temp = Unused_Heap_Top;
  Unused_Heap_Top = Heap_Top;
  Heap_Top = Temp;
  Free = Heap_Bottom;
  Set_Mem_Top(Heap_Top - GC_Reserve);
  Weak_Chain = NIL;
  return;
}

/* Here is the code which "prunes" objects from weak cons cells.  See
   the picture in gccode.h for a description of the structure built by
   the GC.  This code follows the chain of weak cells (in old space) and
   either updates the new copy's CAR with the relocated version of the
   object, or replaces it with NIL.

   Note that this is the only code in the system, besides the inner garbage
   collector, which looks at both old and new space.
*/

Pointer Weak_Chain;

void
Fix_Weak_Chain()
{
  fast Pointer *Old_Weak_Cell, *Scan, Old_Car, Temp, *Old, *Low_Constant;

  Low_Constant = Constant_Space;
  while (Weak_Chain != NIL)
  {
    Old_Weak_Cell = Get_Pointer(Weak_Chain);
    Scan = Get_Pointer(*Old_Weak_Cell++);
    Weak_Chain = *Old_Weak_Cell;
    Old_Car = *Scan;
    Temp = Make_New_Pointer(Type_Code(Weak_Chain), Old_Car);
    Weak_Chain = Make_New_Pointer(TC_NULL, Weak_Chain);

    switch(GC_Type(Temp))
    { case GC_Non_Pointer:
        *Scan = Temp;
	continue;

      case GC_Special:
	if (Type_Code(Temp) != TC_REFERENCE_TRAP)
	{
	  /* No other special type makes sense here. */
	  goto fail;
	}
	if (Datum(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  *Scan = Temp;
	  continue;
	}
	/* Otherwise, it is a pointer.  Fall through */

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
	{
	  *Scan = Temp;
	  continue;
	}
	Normal_BH(false, continue);
	*Scan = NIL;
	continue;

      case GC_Compiled:
	Old = Get_Pointer(Old_Car);
	if (Old >= Low_Constant)
	{
	  *Scan = Temp;
	  continue;
	}
	Compiled_BH(false, continue);
	*Scan = NIL;
	continue;

      case GC_Undefined:
      default:			/* Non Marked Headers and Broken Hearts */
      fail:
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
     3) Trace the precious objects, remembering where consing started.
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
  *Free++ = ((Prev_Restore_History_Stacklet == NULL) ?
	     NIL :
	     Make_Pointer(TC_CONTROL_POINT, Prev_Restore_History_Stacklet));
  *Free++ = Current_State_Point;
  *Free++ = Fluid_Bindings;

  /* The 4 step GC */
  Result = GCLoop(Constant_Space, &Free);
  if (Result != Check_Value)
  {
    fprintf(stderr, "\nGC: Constant Scan ended too early.\n");
    Microcode_Termination(TERM_BROKEN_HEART);
  }
  Result = GCLoop(Root, &Free);
  if (Free != Result)
  {
    fprintf(stderr, "\nGC-1: Heap Scan ended too early.\n");
    Microcode_Termination(TERM_BROKEN_HEART);
  }
  Root2 = Free;
  *Free++ = The_Precious_Objects;
  Result = GCLoop(Root2, &Free);
  if (Free != Result)
  {
    fprintf(stderr, "\nGC-2: Heap Scan ended too early.\n");
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
  {
    Prev_Restore_History_Stacklet = NULL;
    Root += 1;
  }
  else
    Prev_Restore_History_Stacklet = Get_Pointer(*Root++);
  Current_State_Point = *Root++;
  Fluid_Bindings = *Root++;
  Free_Stacklets = NULL;
  return;
}

/* (GARBAGE-COLLECT SLACK)
   Requests a garbage collection leaving the specified amount of slack
   for the top of heap check on the next GC.  The primitive ends by invoking
   the GC daemon if there is one.

   This primitive never returns normally.  It always escapes into
   the interpreter because some of its cached registers (eg. History)
   have changed.
*/

Built_In_Primitive(Prim_Garbage_Collect, 1, "GARBAGE-COLLECT", 0x3A)
{
  Pointer GC_Daemon_Proc;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  if (Free > Heap_Top)
  {
    fprintf(stderr,
	    "\nGC has been delayed too long, and you are out of room!\n");
    fprintf(stderr,
	    "Free = 0x%x; MemTop = 0x%x; Heap_Top = 0x%x\n",
	    Free, MemTop, Heap_Top);
    Microcode_Termination(TERM_NO_SPACE);
  }
  GC_Reserve = Get_Integer(Arg1);
  GCFlip();
  GC();
  IntCode &= ~INT_GC;
  Pop_Primitive_Frame(1);
  GC_Daemon_Proc = Get_Fixed_Obj_Slot(GC_Daemon);
  if (GC_Daemon_Proc == NIL)
  {
   Will_Push(CONTINUATION_SIZE);
    Store_Return(RC_NORMAL_GC_DONE);
    Store_Expression(Make_Unsigned_Fixnum(MemTop - Free));
    Save_Cont();
   Pushed();
    longjmp( *Back_To_Eval, PRIM_POP_RETURN);
    /*NOTREACHED*/
  }
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS+1));
  Store_Return(RC_NORMAL_GC_DONE);
  Store_Expression(Make_Unsigned_Fixnum(MemTop - Free));
  Save_Cont();
  Push(GC_Daemon_Proc);
  Push(STACK_FRAME_HEADER);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
  /* The following comment is by courtesy of LINT, your friendly sponsor. */
  /*NOTREACHED*/
}
