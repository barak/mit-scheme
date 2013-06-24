/* -*-C-*-

$Id: memmag.c,v 9.71 2003/02/14 18:28:20 cph Exp $

Copyright (c) 1987-2000, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* Memory management top level.

   The memory management code is spread over 4 files:
   - memmag.c: initialization.
   - gcloop.c: main garbage collector loop.
   - purify.c: constant/pure space hacking.
   - wabbit.c: alternate garbage collector loop that collects references.
   There is also a relevant header file, gccode.h.

   The object dumper, fasdump, shares properties and code with the
   memory management utilities.
 */

#include "scheme.h"
#include "prims.h"
#include "memmag.h"
#include "gccode.h"

/* Imports */

extern SCHEME_OBJECT * EXFUN (GCLoop, (SCHEME_OBJECT *, SCHEME_OBJECT **));
extern SCHEME_OBJECT * EXFUN
  (wabbit_hunting_gcloop, (SCHEME_OBJECT *, SCHEME_OBJECT **));
extern void EXFUN (wabbit_season, (SCHEME_OBJECT));
extern void EXFUN (duck_season, (SCHEME_OBJECT));
extern void EXFUN (fix_weak_chain_and_hunt_wabbits, (void));
extern void EXFUN (error_unimplemented_primitive, (void));


/* Exports */

extern void
  EXFUN (GCFlip, (void)),
  EXFUN (GC, (void));

extern void
  EXFUN (Clear_Memory, (int, int, int)),
  EXFUN (Setup_Memory, (int, int, int)),
  EXFUN (Reset_Memory, (void));

/* 	Memory Allocation, sequential processor:

oo
   ------------------------------------------ <- fixed boundary (currently)
   |           Heap 2			    |
   |                                        |
   ------------------------------------------ <- boundary moved by purify
   |           Heap 1			    |
   |                                        |
   ------------------------------------------ <- boundary moved by purify
   |     Constant + Pure Space    /\        |
   |                              ||        |
   ------------------------------------------ <- fixed boundary (currently)
   |         Control Stack        ||        |
   |                              \/        |
   ------------------------------------------ <- fixed boundary (currently)
0

   Each area has a pointer to its starting address and a pointer to
   the next free cell (for the stack, it is a pointer to the last cell
   in use).  In addition, there is a pointer to the top of the
   useable area of the heap (the heap is subdivided into two areas for
   the purposes of GC, and this pointer indicates the top of the half
   currently in use).

*/

#define CONSTANT_SPACE_FUDGE	128

/* Initialize free pointers within areas.  sp_register is
   special: it always points to a cell that is in use.  */

static long saved_heap_size, saved_constant_size, saved_stack_size;
extern void EXFUN (reset_allocator_parameters, (void));
extern Boolean EXFUN (update_allocator_parameters, (SCHEME_OBJECT *));

Boolean
DEFUN (update_allocator_parameters, (ctop), SCHEME_OBJECT * ctop)
{
  /* buffer for impurify, etc. */
  SCHEME_OBJECT * nctop = (ctop + CONSTANT_SPACE_FUDGE);
  unsigned long temp;

  if (nctop >= (Highest_Allocated_Address + 1))
    return (FALSE);

  Constant_Top = nctop;
  temp = ((Highest_Allocated_Address - Constant_Top) / 2);
  Heap_Bottom = Constant_Top;
  Heap_Top = (Heap_Bottom + temp);
  Local_Heap_Base = Heap_Bottom;
  Unused_Heap_Bottom = Heap_Top;
  Unused_Heap_Top = Highest_Allocated_Address;
  Free = Heap_Bottom;
  return (TRUE);
}

void
DEFUN_VOID (reset_allocator_parameters)
{
  GC_Reserve = 4500;
  GC_Space_Needed = 0;
  Stack_Bottom = Lowest_Allocated_Address;
  Stack_Top = (Stack_Bottom + (STACK_ALLOCATION_SIZE (saved_stack_size)));
  Constant_Space = Stack_Top;
  Free_Constant = Constant_Space;
  ALIGN_FLOAT (Free_Constant);
  (void) update_allocator_parameters (Free_Constant);
  SET_CONSTANT_TOP ();
  ALIGN_FLOAT (Free);
  SET_MEMTOP (Heap_Top - GC_Reserve);
  INITIALIZE_STACK ();
  STACK_RESET ();
  return;
}

void
DEFUN (Clear_Memory,
       (Our_Heap_Size, Our_Stack_Size, Our_Constant_Size),
       int Our_Heap_Size AND int Our_Stack_Size AND int Our_Constant_Size)
{
  saved_heap_size = Our_Heap_Size;
  saved_constant_size = Our_Constant_Size;
  saved_stack_size = Our_Stack_Size;
  reset_allocator_parameters ();
}

static void 
DEFUN_VOID (failed_consistency_check)
{
  outf_flush_fatal ();
  exit (1);
}

void
DEFUN_VOID (Reset_Memory)
{
  HEAP_FREE (Lowest_Allocated_Address);
  DEALLOCATE_REGISTERS ();
  return;
}

/* This procedure allocates and divides the total memory. */

void
DEFUN (Setup_Memory,
       (Our_Heap_Size, Our_Stack_Size, Our_Constant_Size),
       int Our_Heap_Size AND int Our_Stack_Size AND int Our_Constant_Size)
{
  SCHEME_OBJECT test_value;

  ALLOCATE_REGISTERS ();

  /* Consistency check 1 */
  if (Our_Heap_Size == 0)
  {
    outf_fatal ("Configuration won't hold initial data.\n");
    failed_consistency_check ();
  }

  /* Allocate */
  
  ALLOCATE_HEAP_SPACE (((STACK_ALLOCATION_SIZE (Our_Stack_Size))
			+ (2 * Our_Heap_Size)
			+ Our_Constant_Size),
		       Lowest_Allocated_Address,
		       Highest_Allocated_Address);

  /* Consistency check 2 */
  if (Lowest_Allocated_Address == NULL)
  {
    outf_fatal ("Not enough memory for this configuration.\n");
    failed_consistency_check ();
  }

  /* Consistency check 3 */

  test_value = (MAKE_POINTER_OBJECT (LAST_TYPE_CODE,
				     Highest_Allocated_Address));

  if (((OBJECT_TYPE (test_value)) != LAST_TYPE_CODE) ||
      ((OBJECT_ADDRESS (test_value)) != Highest_Allocated_Address))
  {
    outf_fatal (
        "Largest address does not fit in datum field of object.\n");
    outf_fatal (
        "Allocate less space or re-configure without HEAP_IN_LOW_MEMORY.\n");
    Reset_Memory ();
    failed_consistency_check ();
  }

  Clear_Memory (Our_Heap_Size, Our_Stack_Size, Our_Constant_Size);
  return;
}

/* Utilities for the garbage collector top level.
   The main garbage collector loop is in gcloop.c
*/

/* Flip into unused heap, extending constant space if we are flipping
   to the low heap, and the fudge area has shrunk.
 */

void
DEFUN_VOID (GCFlip)
{
  if (((Constant_Top - Free_Constant) < CONSTANT_SPACE_FUDGE)
      && (Unused_Heap_Bottom < Heap_Bottom)
      && (update_allocator_parameters (Free_Constant)))
    SET_CONSTANT_TOP ();
  else
  {
    SCHEME_OBJECT * temp_bottom, * temp_top;

    temp_bottom = Unused_Heap_Bottom;
    temp_top = Unused_Heap_Top;

    Unused_Heap_Bottom = Heap_Bottom;
    Unused_Heap_Top = Heap_Top;

    Heap_Bottom = temp_bottom;
    Heap_Top = temp_top;

    Free = Heap_Bottom;
  }

  ALIGN_FLOAT (Free);
  SET_MEMTOP (Heap_Top - GC_Reserve);

  Weak_Chain = EMPTY_WEAK_CHAIN;
  return;
}

/* Here is the code which "prunes" objects from weak cons cells.  See
   the picture in gccode.h for a description of the structure built by
   the GC.  This code follows the chain of weak cells (in old space) and
   either updates the new copy's CAR with the relocated version of the
   object, or replaces it with SHARP_F.

   Note that this is the only code in the system, besides the inner garbage
   collector, which looks at both old and new space.
*/

SCHEME_OBJECT Weak_Chain;

void
DEFUN_VOID (Fix_Weak_Chain)
{
  fast SCHEME_OBJECT
    * Old_Weak_Cell, * Scan, Old_Car,
    Temp, * Old, * low_heap;

  low_heap = Constant_Top;
  while (Weak_Chain != EMPTY_WEAK_CHAIN)
  {
    Old_Weak_Cell = (OBJECT_ADDRESS (Weak_Chain));
    Scan = (OBJECT_ADDRESS (*Old_Weak_Cell++));
    Weak_Chain = * Old_Weak_Cell;
    Old_Car = * Scan;
    Temp = (MAKE_OBJECT_FROM_OBJECTS (Weak_Chain, Old_Car));
    Weak_Chain = (OBJECT_NEW_TYPE (TC_NULL, Weak_Chain));

    switch (GC_Type (Temp))
    { case GC_Non_Pointer:
        *Scan = Temp;
	continue;

      case GC_Special:
	if ((OBJECT_TYPE (Temp)) != TC_REFERENCE_TRAP)
	{
	  /* No other special type makes sense here. */
	  goto fail;
	}
	if ((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	{
	  *Scan = Temp;
	  continue;
	}
	/* Otherwise, it is a pointer.  Fall through */

      /* Normal pointer types, the broken heart is in the first word.
         Note that most special types are treated normally here.
	 The BH code updates *Scan if the object has been relocated.
	 Otherwise it falls through and we replace it with a full SHARP_F.
	 Eliminating this assignment would keep old data (pl. of datum).
       */
      case GC_Cell:
      case GC_Pair:
      case GC_Triple:
      case GC_Quadruple:
      case GC_Vector:
	Old = (OBJECT_ADDRESS (Old_Car));
	if (Old < low_heap)
	{
	  *Scan = Temp;
	  continue;
	}
	Normal_BH (false, continue);
	*Scan = SHARP_F;
	continue;

      case GC_Compiled:
	Old = (OBJECT_ADDRESS (Old_Car));
	if (Old < low_heap)
	{
	  *Scan = Temp;
	  continue;
	}
	Compiled_BH (false, { *Scan = Temp; continue; });
	*Scan = SHARP_F;
	continue;

      case GC_Undefined:
	outf_error ("\nFix_Weak_Chain: Clearing bad object 0x%08lx.\n",
		    Temp);
	*Scan = SHARP_F;
	continue;

      default:			/* Non Marked Headers and Broken Hearts */
      fail:
        outf_fatal ("\nFix_Weak_Chain: Bad Object: 0x%08lx.\n",
		    Temp);
	*Scan = SHARP_F;
    }
  }
  return;
}

#ifdef __WIN32__

static void
win32_flush_old_halfspace ()
{
  /* Since we allocated the heap with VirtualAlloc, we can decommit the old
     half-space to tell the VM system that it contains trash.
     Immediately recommitting the region allows the old half-space to be used
     for temporary storage (e.g. by fasdump).
     Note that this is only a win when it prevents paging.  When no paging
     would have happened, we incur the cost of zero-filling the recommitted
     pages.  This can be significant - up to 50% of the time taken to GC, but
     usually somewhat less.

     We are careful to play with pages that are strictly within the old
     half-space, hence the `pagesize' arithmetic.
     */
  long pagesize = 4096;
  void *base =
    ((void*)
     (((DWORD)((char*)Unused_Heap_Bottom + pagesize)) & ~(pagesize-1)));
  DWORD  len =
    ((DWORD)(((char*)Unused_Heap_Top) - ((char*)base))) & ~(pagesize-1);
  VirtualFree (base, len, MEM_DECOMMIT);
  VirtualAlloc (base, len, MEM_COMMIT, PAGE_READWRITE);
}


static BOOL win32_flush_old_halfspace_p = FALSE;

void
win32_advise_end_GC ()
{
  if (win32_flush_old_halfspace_p)
    win32_flush_old_halfspace ();
}
#endif /* __WIN32__ */

DEFINE_PRIMITIVE ("WIN32-FLUSH-OLD-HALFSPACE-AFTER-GC?!", Prim_win32_flush_old_halfspace_after_gc, 1, 1,
		  "(boolean)")
{
  PRIMITIVE_HEADER (1);
#ifdef __WIN32__
  {
    BOOL old = win32_flush_old_halfspace_p;
    win32_flush_old_halfspace_p = (OBJECT_TO_BOOLEAN (ARG_REF (1)));
    PRIMITIVE_RETURN (old ? SHARP_T : SHARP_F);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (SHARP_F);
#endif
}

DEFINE_PRIMITIVE ("WIN32-FLUSH-OLD-HALFSPACE!", Prim_win32_flush_old_halfspace, 0, 0,
		  "()")
{
  PRIMITIVE_HEADER (0);
#ifdef __WIN32__
  win32_flush_old_halfspace ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Here is the set up for the full garbage collection:

   - First it saves away all the relevant microcode registers into new
   space, making this the root for garbage collection.

   - Then it does the actual garbage collection in 4 steps:
     1) Trace the stack and constant space (contiguous).
     2) Trace objects pointed out by the root and constant space.
     3) Trace the precious objects, remembering where consing started.
     4) Update all weak pointers.

   - Finally it restores the microcode registers from the copies in
   new space.
*/

void
DEFUN_VOID (GC)
{
  Boolean hunting_wabbits_p;
  SCHEME_OBJECT
    * Root, * Result, * Check_Value,
    The_Precious_Objects, * Root2;
  SCHEME_OBJECT wabbit_descriptor;
  SCHEME_OBJECT *
    EXFUN ((* transport_loop), (SCHEME_OBJECT *, SCHEME_OBJECT **));

  wabbit_descriptor = (Get_Fixed_Obj_Slot (GC_WABBIT_DESCRIPTOR));
  if ((! (VECTOR_P (wabbit_descriptor)))
      || ((VECTOR_LENGTH (wabbit_descriptor)) != 4)
      || ((VECTOR_REF (wabbit_descriptor, 0)) != SHARP_F)
      || (! (VECTOR_P (VECTOR_REF (wabbit_descriptor, 1))))
      || ((OBJECT_ADDRESS (VECTOR_REF (wabbit_descriptor, 1))) < Constant_Top)
      || (! (VECTOR_P (VECTOR_REF (wabbit_descriptor, 2))))
      || ((OBJECT_ADDRESS (VECTOR_REF (wabbit_descriptor, 2))) < Constant_Top)
      || ((VECTOR_LENGTH (VECTOR_REF (wabbit_descriptor, 2)))
	  < (2 + (2 * (VECTOR_LENGTH (VECTOR_REF (wabbit_descriptor, 1)))))))
  {
    hunting_wabbits_p = false;
    transport_loop = GCLoop;
  }
  else
  {
    hunting_wabbits_p = true;
    transport_loop = wabbit_hunting_gcloop;
  }

  /* Save the microcode registers so that they can be relocated */

  Terminate_Old_Stacklet ();
  SEAL_CONSTANT_SPACE ();
  Check_Value = (CONSTANT_AREA_END ());
  The_Precious_Objects = (Get_Fixed_Obj_Slot (Precious_Objects));
  Set_Fixed_Obj_Slot (Precious_Objects, SHARP_F);
  Set_Fixed_Obj_Slot (Lost_Objects_Base, SHARP_F);

  if (hunting_wabbits_p)
    wabbit_season (wabbit_descriptor);

  Root = Free;
  *Free++ = Fixed_Objects;
  *Free++ = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, history_register));
  *Free++ = Get_Current_Stacklet ();
  *Free++ =
    ((Prev_Restore_History_Stacklet == NULL)
     ? SHARP_F
     : (MAKE_POINTER_OBJECT (TC_CONTROL_POINT,
			     Prev_Restore_History_Stacklet)));
  *Free++ = Current_State_Point;
  *Free++ = Fluid_Bindings;

#ifdef ENABLE_GC_DEBUGGING_TOOLS
  if (gc_objects_referencing != SHARP_F)
    {
      MEMORY_SET
	(gc_objects_referencing, 0,
	 (MAKE_OBJECT
	  (TC_MANIFEST_NM_VECTOR,
	   (OBJECT_DATUM (MEMORY_REF (gc_objects_referencing, 0))))));
      {
	SCHEME_OBJECT * scan = (VECTOR_LOC (gc_objects_referencing, 0));
	SCHEME_OBJECT * end =
	  (VECTOR_LOC (gc_objects_referencing,
		       (VECTOR_LENGTH (gc_objects_referencing))));
	while (scan < end)
	  (*scan++) = SHARP_F;
      }
      *Free++ = gc_objects_referencing;
      gc_objects_referencing_count = 0;
      gc_objects_referencing_scan =
	(VECTOR_LOC (gc_objects_referencing, 1));
      gc_objects_referencing_end =
	(VECTOR_LOC (gc_objects_referencing,
		     (VECTOR_LENGTH (gc_objects_referencing))));
    }
#endif

  /* The 4 step GC */

  Result = ((* transport_loop) ((CONSTANT_AREA_START ()), &Free));
  if (Result != Check_Value)
  {
    outf_fatal ("\nGC: Constant Scan ended too early.\n");
    Microcode_Termination (TERM_BROKEN_HEART);
  }

  Result = ((* transport_loop) (Root, &Free));
  if (Free != Result)
  {
    outf_fatal ("\nGC-1: Heap Scan ended too early.\n");
    Microcode_Termination (TERM_BROKEN_HEART);
  }

  Root2 = Free;
  *Free++ = The_Precious_Objects;
  Result = ((* transport_loop) (Root2, &Free));
  if (Free != Result)
  {
    outf_fatal ("\nGC-2: Heap Scan ended too early.\n");
    Microcode_Termination (TERM_BROKEN_HEART);
  }

#ifdef ENABLE_GC_DEBUGGING_TOOLS
  if (gc_objects_referencing != SHARP_F)
    {
      UPDATE_GC_OBJECTS_REFERENCING ();
      MEMORY_SET
	(gc_objects_referencing, 0,
	 (MAKE_OBJECT
	  (TC_MANIFEST_VECTOR,
	   (OBJECT_DATUM (MEMORY_REF (gc_objects_referencing, 0))))));
      VECTOR_SET (gc_objects_referencing, 0,
		  (LONG_TO_UNSIGNED_FIXNUM (gc_objects_referencing_count)));
      {
	SCHEME_OBJECT * end = gc_objects_referencing_scan;
	Result = (GCLoop ((VECTOR_LOC (gc_objects_referencing, 1)), (&end)));
	if ((end != Result) || (end != gc_objects_referencing_scan))
	  {
	    outf_fatal ("\nGC-3: Heap Scan ended too early.\n");
	    Microcode_Termination (TERM_BROKEN_HEART);
	  }
      }
      gc_objects_referencing = SHARP_F;
      gc_object_referenced = SHARP_F;
    }
#endif

  if (hunting_wabbits_p)
    fix_weak_chain_and_hunt_wabbits ();
  else
    Fix_Weak_Chain ();

  /* Make the microcode registers point to the copies in new-space. */

  Fixed_Objects = *Root++;
  Set_Fixed_Obj_Slot (Precious_Objects, *Root2);
  Set_Fixed_Obj_Slot
    (Lost_Objects_Base, (LONG_TO_UNSIGNED_FIXNUM (ADDRESS_TO_DATUM (Root2))));

  history_register = (OBJECT_ADDRESS (*Root++));

  Set_Current_Stacklet (*Root);
  Root += 1;
  if (*Root == SHARP_F)
  {
    Prev_Restore_History_Stacklet = NULL;
    Root += 1;
  }
  else
    Prev_Restore_History_Stacklet = (OBJECT_ADDRESS (*Root++));
  Current_State_Point = *Root++;
  Fluid_Bindings = *Root++;
  Free_Stacklets = NULL;

  if (hunting_wabbits_p)
  {
    wabbit_descriptor = (Get_Fixed_Obj_Slot (GC_WABBIT_DESCRIPTOR));
    duck_season (wabbit_descriptor);
  }

  COMPILER_TRANSPORT_END ();

#ifdef __WIN32__
  {
    extern void win32_advise_end_GC ();
    win32_advise_end_GC ();
  }
#endif

  CLEAR_INTERRUPT (INT_GC);
  return;
}

/* (GARBAGE-COLLECT SLACK)
   Requests a garbage collection leaving the specified amount of slack
   for the top of heap check on the next GC.  The primitive ends by invoking
   the GC daemon if there is one.

   This primitive never returns normally.  It always escapes into
   the interpreter because some of its cached registers (e.g.
   history_register) have changed.  */

DEFINE_PRIMITIVE ("GARBAGE-COLLECT", Prim_garbage_collect, 1, 1, 0)
{
  extern unsigned long gc_counter;
  SCHEME_OBJECT daemon;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();

  STACK_SANITY_CHECK ("GC");
  if (Free > Heap_Top)
  {
    outf_fatal ("\nGARBAGE-COLLECT: GC has been delayed too long!\n");
    outf_fatal ("Free = 0x%lx; MemTop = 0x%lx; Heap_Top = 0x%lx\n",
	        Free, MemTop, Heap_Top);
    Microcode_Termination (TERM_NO_SPACE);
  }

  GC_Reserve = (arg_nonnegative_integer (1));
  POP_PRIMITIVE_FRAME (1);

  ENTER_CRITICAL_SECTION ("garbage collector");
  run_pre_gc_hooks ();
  gc_counter += 1;
  GCFlip ();
  GC ();
  run_post_gc_hooks ();
  daemon = (Get_Fixed_Obj_Slot (GC_Daemon));

 Will_Push (CONTINUATION_SIZE);
  Store_Return (RC_NORMAL_GC_DONE);
  exp_register = (LONG_TO_UNSIGNED_FIXNUM (MemTop - Free));
  Save_Cont ();
 Pushed ();

  RENAME_CRITICAL_SECTION ("garbage collector daemon");
  if (daemon == SHARP_F)
    PRIMITIVE_ABORT (PRIM_POP_RETURN);
    /*NOTREACHED*/

 Will_Push (2);
  STACK_PUSH (daemon);
  STACK_PUSH (STACK_FRAME_HEADER);
 Pushed ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("GC-TRACE-REFERENCES", Prim_gc_trace_references, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT objects_referencing = (ARG_REF (2));
    if (! ((objects_referencing == SHARP_F)
	   || ((VECTOR_P (objects_referencing))
	       && ((VECTOR_LENGTH (objects_referencing)) >= 1))))
      error_wrong_type_arg (2);
#ifdef ENABLE_GC_DEBUGGING_TOOLS
    gc_object_referenced = (ARG_REF (1));
    gc_objects_referencing = objects_referencing;
#else /* not ENABLE_GC_DEBUGGING_TOOLS */
    error_external_return ();
#endif /* not ENABLE_GC_DEBUGGING_TOOLS */
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

void
DEFUN (check_transport_vector_lossage, (Scan, Saved_Scan, To),
       SCHEME_OBJECT * Scan
       AND SCHEME_OBJECT * Saved_Scan
       AND SCHEME_OBJECT * To)
{
  outf_fatal ("\nBad transport_vector limit:\n");
  outf_fatal ("  limit = 0x%lx\n", ((long) Scan));
  outf_fatal ("  Scan = 0x%lx\n", ((long) Saved_Scan));
  outf_fatal ("  To = 0x%lx\n", ((long) To));
  outf_flush_fatal ();
  abort ();
}
