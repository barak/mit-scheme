/* -*-C-*-

$Id: purify.c,v 9.64 2002/11/20 19:46:14 cph Exp $

Copyright (c) 1988-1999, 2001, 2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* This file contains the code that copies objects into pure
   and constant space. */

#include "scheme.h"
#include "prims.h"
#include "gccode.h"
#include "zones.h"

/* Imports */

extern void EXFUN (GC, (void));
extern SCHEME_OBJECT * EXFUN (GCLoop, (SCHEME_OBJECT *, SCHEME_OBJECT **));

/* This is a copy of GCLoop, with mode handling added, and
   debugging printout removed.
*/

/* Purify modes */

#define	NORMAL_GC	0
#define PURE_COPY	1
#define CONSTANT_COPY	2

#define Purify_Pointer(Code)						\
{									\
  Old = (OBJECT_ADDRESS (Temp));					\
  if ((GC_Mode == CONSTANT_COPY)					\
      && (Old < low_heap))						\
    continue;								\
  Code;									\
}

#define PURIFY_RAW_POINTER(Code)					\
{									\
  Old = (SCHEME_ADDR_TO_ADDR (Temp));					\
  if ((GC_Mode == CONSTANT_COPY)					\
      && (Old < low_heap))						\
    continue;								\
  Code;									\
}

#define Setup_Pointer_for_Purify(Extra_Code)				\
{									\
  Purify_Pointer (Setup_Pointer (false, Extra_Code));			\
}

#define Indirect_BH(In_GC)						\
{									\
  if ((OBJECT_TYPE (* Old)) == TC_BROKEN_HEART)				\
    continue;								\
}

#define Transport_Vector_Indirect()					\
{									\
  Real_Transport_Vector ();						\
  *(OBJECT_ADDRESS (Temp)) = New_Address;				\
}

SCHEME_OBJECT *
DEFUN (purifyloop, (Scan, To_Pointer, GC_Mode),
       fast SCHEME_OBJECT *Scan AND
       SCHEME_OBJECT **To_Pointer AND
       int GC_Mode)
{
  fast SCHEME_OBJECT
    * To, * Old, Temp,
    * low_heap, New_Address;
#ifdef ENABLE_GC_DEBUGGING_TOOLS
  SCHEME_OBJECT object_referencing;
#endif

  To = * To_Pointer;
  low_heap = Constant_Top;
  for ( ; Scan != To; Scan++)
  {
    Temp = * Scan;
#ifdef ENABLE_GC_DEBUGGING_TOOLS
    object_referencing = Temp;
#endif
    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
        if (Scan == (OBJECT_ADDRESS (Temp)))
	{
	  *To_Pointer = To;
	  return Scan;
	}
	sprintf(gc_death_message_buffer,
		"purifyloop: broken heart (0x%lx) in scan",
		Temp);
	gc_death(TERM_BROKEN_HEART, gc_death_message_buffer, Scan, To);
	/*NOTREACHED*/

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	Scan += OBJECT_DATUM (Temp);
	break;

      /* Compiled code relocation. */

      case TC_LINKAGE_SECTION:
      {
	if (GC_Mode == PURE_COPY)
	{
	  gc_death (TERM_COMPILER_DEATH,
		    "purifyloop: linkage section in pure area",
		    Scan, To);
	  /*NOTREACHED*/
	}

	switch (READ_LINKAGE_KIND (Temp))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    /* Assumes that all others are objects of type TC_QUAD without
	       their type codes.
	     */

	    fast long count;

	    Scan++;
	    for (count = (READ_CACHE_LINKAGE_COUNT (Temp));
		 --count >= 0;
		 Scan += 1)
	    {
	      Temp = (* Scan);
	      PURIFY_RAW_POINTER (Setup_Internal (false,
						  TRANSPORT_RAW_TRIPLE (),
						  RAW_BH (false, continue)));
	    }
	    Scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    fast long count;
	    fast char * word_ptr;
	    SCHEME_OBJECT * end_scan;

	    START_OPERATOR_RELOCATION (Scan);
	    count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	    end_scan = (END_OPERATOR_LINKAGE_AREA (Scan, count));

	    while(--count >= 0)
	    {
	      Scan = ((SCHEME_OBJECT *) word_ptr);
	      word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	      EXTRACT_OPERATOR_LINKAGE_ADDRESS (Temp, Scan);
	      PURIFY_RAW_POINTER (Setup_Aligned
				  (false,
				   TRANSPORT_RAW_COMPILED (),
				   RAW_COMPILED_BH (false,
						    goto next_operator)));
	    next_operator:
	      STORE_OPERATOR_LINKAGE_ADDRESS(Temp, Scan);
	    }
	    Scan = end_scan;
	    END_OPERATOR_RELOCATION (Scan);
	    break;
	  }

	  case CLOSURE_PATTERN_LINKAGE_KIND:
	    Scan += (READ_CACHE_LINKAGE_COUNT (Temp));
	    break;

	  default:
	  {
	    gc_death (TERM_EXIT,
		      "purifyloop: Unknown compiler linkage kind.",
		      Scan, Free);
	    /*NOTREACHED*/
	  }
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	fast long count;
	fast char * word_ptr;
	SCHEME_OBJECT * area_end;

	if (GC_Mode == PURE_COPY)
	{
	  gc_death (TERM_COMPILER_DEATH,
		    "purifyloop: manifest closure in pure area",
		    Scan, To);
	  /*NOTREACHED*/
	}

	START_CLOSURE_RELOCATION (Scan);
	Scan += 1;
	count = (MANIFEST_CLOSURE_COUNT (Scan));
	word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	area_end = ((MANIFEST_CLOSURE_END (Scan, count)) - 1);

	while ((--count) >= 0)
	{
	  Scan = ((SCHEME_OBJECT *) (word_ptr));
	  word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	  EXTRACT_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	  PURIFY_RAW_POINTER (Setup_Aligned
			      (false,
			       TRANSPORT_RAW_COMPILED (),
			       RAW_COMPILED_BH (false,
						goto next_closure)));
	next_closure:
	  STORE_CLOSURE_ENTRY_ADDRESS (Temp, Scan);
	}
	Scan = area_end;
	END_CLOSURE_RELOCATION (Scan);
	break;
      }

      case_compiled_entry_point:
	if (GC_Mode != PURE_COPY)
	{
	  Purify_Pointer (Setup_Aligned (false,
					 Transport_Compiled (),
					 Compiled_BH (false,
						      goto after_entry)));
        after_entry:
	  *Scan = Temp;
	}
	break;

      case_Cell:
	Setup_Pointer_for_Purify (Transport_Cell ());
	break;

      case TC_WEAK_CONS:
	Setup_Pointer_for_Purify (Transport_Weak_Cons ());
	break;

      /*
	Symbols, variables, and reference traps cannot be put into
	pure space.  The strings contained in the first two can, on the
	other hand.
       */

      case TC_REFERENCE_TRAP:
	if (((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	    || (GC_Mode == PURE_COPY))
	{
	  /* It is a non pointer. */
	  break;
	}
	goto purify_pair;

      case TC_INTERNED_SYMBOL:
      case TC_UNINTERNED_SYMBOL:
	if (GC_Mode == PURE_COPY)
        {
	  Temp = MEMORY_REF (Temp, SYMBOL_NAME);
	  Purify_Pointer (Setup_Internal (false,
					  Transport_Vector_Indirect (),
					  Indirect_BH (false)));
	  break;
	}

	/* Fall through */

      case_Fasdump_Pair:
      purify_pair:
	Setup_Pointer_for_Purify (Transport_Pair ());
	break;

      case TC_VARIABLE:
      case_Triple:
	Setup_Pointer_for_Purify (Transport_Triple ());
	break;

      case_Quadruple:
	Setup_Pointer_for_Purify (Transport_Quadruple ());
	break;

      case TC_COMPILED_CODE_BLOCK:
	if (GC_Mode == PURE_COPY)
	  break;
	/* fall through */
	
      case TC_BIG_FLONUM:
	Purify_Pointer (Setup_Aligned (false,
				       goto Move_Vector,
				       Normal_BH (false, continue)));
	break;

	/* No need to handle futures specially here, since purifyloop
	   is always invoked after running GCLoop, which will have
	   spliced all spliceable futures unless the GC itself of the
	   GC dameons spliced them, but this should not occur.
	 */

      case TC_FUTURE:
      case TC_ENVIRONMENT:
	if (GC_Mode == PURE_COPY)
	{
	  /* For environments, this should actually do an indirect pair
	     transport of the procedure, at least.
	   */
	  break;
	}
	/* Fall through */

      case_Purify_Vector:
	Setup_Pointer_for_Purify (Transport_Vector ());
	break;

      default:
	GC_BAD_TYPE ("purifyloop", Temp);
	/* Fall Through */

      case_Non_Pointer:
	break;

      } /* Switch_by_GC_Type */
  } /* For loop */

  *To_Pointer = To;
  return (To);

} /* purifyloop */

/* Description of the algorithm for PURIFY:

   Purify increases the size of constant space at the expense of both
   heaps.  A GC-like relocation is performed with the object being
   purified as the root.  The object is copied and relocated from the
   high heap to the area adjacent to constant space.  Then the GC is
   finished after changing the end of constant-space marker.

   In order to make a pure object, the copy process proceeds in two
   halves.  During the first half (which collects the pure part)
   Compiled Code, Environments, Symbols, and Variables (i.e.  things
   whose contents change) are NOT copied.  Then a header is put down
   indicating constant (not pure) area, and then they ARE copied.

   The constant area contains a contiguous set of blocks of the
   following format:

  >>Heap above here<<

                   . (direction of growth)
                   .  ^
                   . / \
                   .  |
                   .  |
        |----------------------|...
        | END   | Total Size M |   . Where END   = TC_FIXNUM
        |----------------------|    .      SNMH  = TC_MANIFEST_SPECIAL_...
        | SNMH  |      1       |    |      CONST = TC_CONSTANT
        |----------------------|    |      PURE  = TC_NULL
        |                      |    |
        |                      |    |
        |    CONSTANT AREA     |    |
        |                      |    |
        |                      |     .
     ...|----------------------|      >  M
    .   | CONST | Pure Size N  |     .
   .    |----------------------|    |
   |    | SNMH  |      1       |    |
   |    |----------------------|    |
   |    |                      |    |
N <     |                      |    |
   |    |      PURE AREA       |    |
   |    |                      |    |
   .    |                      |    .
    .   |----------------------|   .
     ...| PURE  | Total Size M |...
        |----------------------|
        | SNMH  | Pure Size N  |
        |----------------------|

  >>Top of Stack (Stack below here)<<

*/

static void
DEFUN (purify, (object, purify_mode),
       SCHEME_OBJECT object AND Boolean purify_mode)
{
  long length, pure_length;
  SCHEME_OBJECT * new_object, * result;
  extern Boolean EXFUN (update_allocator_parameters, (SCHEME_OBJECT *));

  run_pre_gc_hooks ();
  STACK_SANITY_CHECK ("PURIFY");
  Weak_Chain = EMPTY_WEAK_CHAIN;
  Constant_Top = Free_Constant; 
  new_object = Free_Constant;
  *Free_Constant++ = SHARP_F;	/* Will hold pure space header */
  *Free_Constant++ = object;
  if (! (purify_mode))
    pure_length = 3;
  else
  {
    result = (purifyloop ((new_object + 1), &Free_Constant, PURE_COPY));

    if (result != Free_Constant)
    {
purification_failure:
      outf_fatal ("\nPurify: Pure Copy ended too early.\n");
      Microcode_Termination (TERM_BROKEN_HEART);
    }
    pure_length = ((Free_Constant - new_object) + 1);
  }
  *Free_Constant++ = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  *Free_Constant++ = (MAKE_OBJECT (CONSTANT_PART, pure_length));
  Constant_Top = Free_Constant;
  if (purify_mode)
  {
    result = (purifyloop ((new_object + 1), &Free_Constant, CONSTANT_COPY));
    if (result != Free_Constant)
    {
      outf_fatal ("\nPurify: Pure Copy ended too early.\n");
      Microcode_Termination (TERM_BROKEN_HEART);
    }
  }
  else
  {
    result = (GCLoop ((new_object + 1), &Free_Constant));
    if (result != Free_Constant)
      goto purification_failure;
  }

  length = ((Free_Constant - new_object) - 4);
  *Free_Constant++ = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  *Free_Constant++ = (MAKE_OBJECT (END_OF_BLOCK, (length + 5)));
  *new_object++ =
    (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, pure_length));
  *new_object = (MAKE_OBJECT (PURE_PART, (length + 5)));
  if (! (update_allocator_parameters (Free_Constant)))
    gc_death (TERM_NO_SPACE, "purify: object too large", NULL, NULL);
    /*NOTREACHED*/

  SET_CONSTANT_TOP ();
  ALIGN_FLOAT (Free);
  SET_MEMTOP (Heap_Top - GC_Reserve);
  GC ();
  run_post_gc_hooks ();
}

/* (PRIMITIVE-PURIFY OBJECT PURE? SAFETY-MARGIN)
   Copy an object from the heap into constant space.  This requires
   a spare heap, and is tricky to use -- it should only be used
   through the wrapper provided in the Scheme runtime system.

   To purify an object we just copy it into Pure Space in two
   parts with the appropriate headers and footers.  The actual
   copying is done by purifyloop above.

   Once the copy is complete we run a full GC which handles the
   broken hearts which now point into pure space.  On a
   multiprocessor, this primitive uses the master-gc-loop and it
   should only be used as one would use master-gc-loop i.e. with
   everyone else halted.

   This primitive always "returns" by escaping to the interpreter
   because some of its cached registers (e.g. history_register) have
   changed.  */

DEFINE_PRIMITIVE ("PRIMITIVE-PURIFY", Prim_primitive_purify, 3, 3, 0)
{
  Boolean purify_mode;
  SCHEME_OBJECT object, result, daemon;
  PRIMITIVE_HEADER (3);
  PRIMITIVE_CANONICALIZE_CONTEXT ();

  STACK_SANITY_CHECK ("PURIFY");
  Save_Time_Zone (Zone_Purify);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  CHECK_ARG (2, BOOLEAN_P);
  purify_mode = (BOOLEAN_ARG (2));
  GC_Reserve = (arg_nonnegative_integer (3));

  /* Purify only works from the high heap.
     If in the low heap, tell the runtime system.
   */

  if (Heap_Bottom < Unused_Heap_Bottom)
    PRIMITIVE_RETURN (SHARP_F);

  POP_PRIMITIVE_FRAME (3);

  ENTER_CRITICAL_SECTION ("purify");
  purify (object, purify_mode);
  result = (MAKE_POINTER_OBJECT (TC_LIST, Free));
  Free += 2;
  Free[-2] = SHARP_T;
  Free[-1] = (LONG_TO_UNSIGNED_FIXNUM (MemTop - Free));

 Will_Push (CONTINUATION_SIZE);
  Store_Return (RC_NORMAL_GC_DONE);
  exp_register = result;
  Save_Cont ();
 Pushed ();

  RENAME_CRITICAL_SECTION ("purify daemon");
  daemon = (Get_Fixed_Obj_Slot (GC_Daemon));
  if (daemon == SHARP_F)
    PRIMITIVE_ABORT (PRIM_POP_RETURN);
    /*NOTREACHED*/

 Will_Push (2);
  STACK_PUSH (daemon);
  STACK_PUSH (STACK_FRAME_HEADER);
 Pushed ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  return (0);
}
