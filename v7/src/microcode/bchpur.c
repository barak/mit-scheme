/* -*-C-*-

$Id: bchpur.c,v 9.68 2000/11/28 05:19:05 cph Exp $

Copyright (c) 1987-2000 Massachusetts Institute of Technology

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

/*
 * This file contains the code for primitives dealing with pure
 * and constant space.  Garbage collection to disk version.
 *
 * Poorly implemented:  If there is not enough space, instead of
 * undoing the changes, it crashes.
 * It should be changed to do the job in two passes like the
 * "normal" version.
 */

#include "scheme.h"
#include "prims.h"
#include "bchgcc.h"
#include "zones.h"

/* Purify modes */

#define	NORMAL_GC	0
#define PURE_COPY	1
#define CONSTANT_COPY	2

/* Some utility macros. */

#define relocate_indirect_setup()					\
{									\
  Old = (OBJECT_ADDRESS (Temp));					\
  if (Old < low_heap)							\
    continue;								\
  if (BROKEN_HEART_P (* Old))						\
    continue;								\
  New_Address = (MAKE_BROKEN_HEART (To_Address));			\
}

#define relocate_indirect_end()						\
{									\
  (* (OBJECT_ADDRESS (Temp))) = New_Address;				\
  continue;								\
}

/* A modified copy of GCLoop. */

static SCHEME_OBJECT *
DEFUN (purifyloop, (Scan, To_ptr, To_Address_ptr, purify_mode),
       fast SCHEME_OBJECT * Scan AND
       SCHEME_OBJECT ** To_ptr AND
       SCHEME_OBJECT ** To_Address_ptr AND
       int purify_mode)
{
  fast SCHEME_OBJECT
    * To, * Old, Temp, * low_heap,
    * To_Address, New_Address;

  To = (* To_ptr);
  To_Address = (* To_Address_ptr);
  low_heap = Constant_Top;

  for ( ; Scan != To; Scan++)
  {
    Temp = (* Scan);
    Switch_by_GC_Type (Temp)
    {
      case TC_BROKEN_HEART:
	if (Scan != scan_buffer_top)
	  goto end_purifyloop;
	/* The -1 is here because of the Scan++ in the for header. */
	Scan = ((dump_and_reload_scan_buffer (0, NULL)) - 1);
	continue;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	/* Check whether this bumps over current buffer,
	   and if so we need a new bufferfull. */
	Scan += (OBJECT_DATUM (Temp));
area_skipped:
	if (Scan < scan_buffer_top)
	  break;
	else
	{
	  unsigned long overflow;

	  /* The + & -1 are here because of the Scan++ in the for header. */
	  overflow = ((Scan - scan_buffer_top) + 1);
	  Scan = ((dump_and_reload_scan_buffer
		   ((overflow >> gc_buffer_shift), NULL)
		   + (overflow & gc_buffer_mask)) - 1);
	  break;
	}

      case_compiled_entry_point:
	if (purify_mode == PURE_COPY)
	  break;
	relocate_compiled_entry (false);
	(* Scan) = Temp;
	break;

      case TC_LINKAGE_SECTION:
      {
	if (purify_mode == PURE_COPY)
	  gc_death (TERM_COMPILER_DEATH,
		    "purifyloop: linkage section in pure area",
		    Scan, To);
	  /*NOTREACHED*/
	switch (READ_LINKAGE_KIND (Temp))
	{
	  case REFERENCE_LINKAGE_KIND:
	  case ASSIGNMENT_LINKAGE_KIND:
	  {
	    /* count typeless pointers to quads follow. */

	    fast long count;
	    long max_count, max_here;

	    Scan++;
	    max_here = (scan_buffer_top - Scan);
	    max_count = (READ_CACHE_LINKAGE_COUNT (Temp));
	    while (max_count != 0)
	    {
	      count = ((max_count > max_here) ? max_here : max_count);
	      max_count -= count;
	      for ( ; --count >= 0; Scan += 1)
	      {
		Temp = *Scan;
		relocate_typeless_pointer (copy_quadruple(), 4);
	      }
	      if (max_count != 0)
	      {
		/* We stopped because we needed to relocate too many. */
		Scan = dump_and_reload_scan_buffer(0, NULL);
		max_here = gc_buffer_size;
	      }
	    }
	    /* The + & -1 are here because of the Scan++ in the for header. */
	    Scan -= 1;
	    break;
	  }

	  case OPERATOR_LINKAGE_KIND:
	  case GLOBAL_OPERATOR_LINKAGE_KIND:
	  {
	    /* Operator linkage */

	    fast long count;
	    fast char *word_ptr, *next_ptr;
	    long overflow;

	    word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	    if (! (word_ptr > ((char *) scan_buffer_top)))
	      BCH_START_OPERATOR_RELOCATION (Scan);
	    else
	    {
	      overflow = (word_ptr - ((char *) Scan));
	      extend_scan_buffer (word_ptr, To);
	      BCH_START_OPERATOR_RELOCATION (Scan);
	      word_ptr = (end_scan_buffer_extension (word_ptr));
	      Scan = ((SCHEME_OBJECT *) (word_ptr - overflow));
	    }
	    
	    count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	    overflow = ((END_OPERATOR_LINKAGE_AREA (Scan, count)) -
			scan_buffer_top);

	    for (next_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
		 (--count >= 0);
		 word_ptr = next_ptr,
		 next_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr)))
	    {
	      if (! (next_ptr > ((char *) scan_buffer_top)))
		relocate_linked_operator (false);
	      else
	      {
		extend_scan_buffer (next_ptr, To);
		relocate_linked_operator (false);
		next_ptr = (end_scan_buffer_extension (next_ptr));
		overflow -= gc_buffer_size;
	      }
	    }
	    Scan = (scan_buffer_top + overflow);
	    BCH_END_OPERATOR_RELOCATION (Scan);
	    break;
	  }

	  case CLOSURE_PATTERN_LINKAGE_KIND:
	    Scan += (READ_CACHE_LINKAGE_COUNT (Temp));
	    goto area_skipped;

	  default:
	    gc_death (TERM_EXIT,
		      "purify: Unknown compiler linkage kind.",
		      Scan, Free);
	    /*NOTREACHED*/
	}
	break;
      }

      case TC_MANIFEST_CLOSURE:
      {
	if (purify_mode == PURE_COPY)
	  gc_death (TERM_COMPILER_DEATH,
		    "purifyloop: manifest closure in pure area",
		    Scan, To);
	  /*NOTREACHED*/
      }
      {
	fast long count;
	fast char * word_ptr;
	char * end_ptr;

	Scan += 1;

	/* Is there enough space to read the count? */

	end_ptr = (((char *) Scan) + (2 * (sizeof (format_word))));
	if (end_ptr > ((char *) scan_buffer_top))
	{
	  long dw;

	  extend_scan_buffer (end_ptr, To);
	  BCH_START_CLOSURE_RELOCATION (Scan - 1);
	  count = (MANIFEST_CLOSURE_COUNT (Scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	  dw = (word_ptr - end_ptr);
	  end_ptr = (end_scan_buffer_extension (end_ptr));
	  word_ptr = (end_ptr + dw);
	  Scan = ((SCHEME_OBJECT *) (end_ptr - (2 * (sizeof (format_word)))));
	}
	else
	{
	  BCH_START_CLOSURE_RELOCATION (Scan - 1);
	  count = (MANIFEST_CLOSURE_COUNT (Scan));
	  word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	}
	end_ptr = ((char *) (MANIFEST_CLOSURE_END (Scan, count)));

	for ( ; ((--count) >= 0);
	     (word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr))))
	{
	  if (! ((CLOSURE_ENTRY_END (word_ptr)) > ((char *) scan_buffer_top)))
	    relocate_manifest_closure (false);
	  else
	  {
	    char * entry_end;
	    long de, dw;

	    entry_end = (CLOSURE_ENTRY_END (word_ptr));
	    de = (end_ptr - entry_end);
	    dw = (entry_end - word_ptr);
	    extend_scan_buffer (entry_end, To);
	    relocate_manifest_closure (false);
	    entry_end = (end_scan_buffer_extension (entry_end));
	    word_ptr = (entry_end - dw);
	    end_ptr = (entry_end + de);
	  }
	}
	Scan = ((SCHEME_OBJECT *) (end_ptr));
	BCH_END_CLOSURE_RELOCATION (Scan);
	break;
      }

      case_Cell:
	if (purify_mode == CONSTANT_COPY)
	  break;
	relocate_normal_pointer (copy_cell(), 1);

      case TC_REFERENCE_TRAP:
	if ((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	  break; /* It is a non pointer. */
	goto purify_pair;

      case TC_INTERNED_SYMBOL:
      case TC_UNINTERNED_SYMBOL:
	if (purify_mode == PURE_COPY)
	{
	  Temp = (MEMORY_REF (Temp, SYMBOL_NAME));
	  relocate_indirect_setup ();
	  copy_vector (NULL);
	  relocate_indirect_end ();
	}
	else
	  goto really_purify_pair;

      case_Fasdump_Pair:
      purify_pair:
	if (purify_mode == CONSTANT_COPY)
	  break;
      really_purify_pair:
	relocate_normal_pointer (copy_pair(), 2);

      case TC_WEAK_CONS:
	if (purify_mode == PURE_COPY)
	  break;
	else
	  relocate_normal_pointer (copy_weak_pair(), 2);

      case TC_VARIABLE:
      case_Triple:
	if (purify_mode == CONSTANT_COPY)
	  break;
	relocate_normal_pointer (copy_triple(), 3);

      case_Quadruple:
	if (purify_mode == CONSTANT_COPY)
	  break;
	relocate_normal_pointer (copy_quadruple(), 4);

      case TC_COMPILED_CODE_BLOCK:
	if (purify_mode == PURE_COPY)
	  break;
	goto aligned_vector_relocation;
	
      case TC_BIG_FLONUM:
	if (purify_mode == CONSTANT_COPY)
	  break;
      aligned_vector_relocation:
	relocate_flonum_setup ();
	goto Move_Vector;

      case TC_ENVIRONMENT:
	if (purify_mode == PURE_COPY)
	  break;
	else
	  goto really_purify_vector;

      case_Purify_Vector:
	if (purify_mode == CONSTANT_COPY)
	  break;
      really_purify_vector:
	relocate_normal_setup ();
      Move_Vector:
	copy_vector (NULL);
	relocate_normal_end ();

      case TC_FUTURE:
	if (purify_mode == CONSTANT_COPY)
	  break;
	relocate_normal_setup();
	if (!(Future_Spliceable (Temp)))
	  goto Move_Vector;
	(* Scan) = (Future_Value (Temp));
	Scan -= 1;
	continue;

      default:
	GC_BAD_TYPE ("purifyloop");
	/* Fall Through */

      case_Non_Pointer:
	break;
      }
  }
end_purifyloop:
  (* To_ptr) = To;
  (* To_Address_ptr) = To_Address;
  return (Scan);
}

/* This is not paranoia!
   The two words in the header may overflow the free buffer.
 */

static SCHEME_OBJECT *
DEFUN (purify_header_overflow, (free_buffer), SCHEME_OBJECT * free_buffer)
{
  long delta;
  SCHEME_OBJECT * scan_buffer;

  delta = (free_buffer - free_buffer_top);
  free_buffer = (dump_and_reset_free_buffer (delta, NULL));
  scan_buffer = (dump_and_reload_scan_buffer (0, NULL));
  if ((scan_buffer + delta) != free_buffer)
  {
    gc_death (TERM_EXIT,
	      "purify: scan and free do not meet at the end",
	      (scan_buffer + delta), free_buffer);
    /*NOTREACHED*/
  }
  return (free_buffer);
}

static void
DEFUN (purify, (object, purify_mode),
       SCHEME_OBJECT object AND Boolean purify_mode)
{
  long length, pure_length, delta;
  SCHEME_OBJECT
    * result, * free_buffer_ptr,
    * old_free_const, * block_start,
    * scan_start, * new_free_const, * pending_scan,
    * root, * root2, the_precious_objects,
    * saved_const_top;
  struct saved_scan_state scan_state;
  extern Boolean EXFUN (update_allocator_parameters, (SCHEME_OBJECT *));

  run_pre_gc_hooks ();
  STACK_SANITY_CHECK ("PURIFY");
  initialize_weak_pair_transport (Stack_Bottom);
  free_buffer_ptr = (initialize_free_buffer ());
  Terminate_Old_Stacklet ();
  SEAL_CONSTANT_SPACE ();
  the_precious_objects = (Get_Fixed_Obj_Slot (Precious_Objects));

  Constant_Top = Free_Constant;
  old_free_const = Free_Constant;
  new_free_const = old_free_const;
  block_start = ((SCHEME_OBJECT *) (ALIGN_DOWN_TO_IO_PAGE (old_free_const)));
  delta = (old_free_const - block_start);

  free_buffer_ptr += delta;
  new_free_const += 2;
  * free_buffer_ptr++ = SHARP_F;	/* Pure block header. */
  * free_buffer_ptr++ = object;
  if (free_buffer_ptr >= free_buffer_top)
    free_buffer_ptr =
      (dump_and_reset_free_buffer ((free_buffer_ptr - free_buffer_top), NULL));

  if (! purify_mode)
    pure_length = 3;
  else
  {
    scan_start = ((initialize_scan_buffer (block_start)) + delta);
    result = (purifyloop (scan_start, &free_buffer_ptr,
			  &new_free_const, PURE_COPY));
    if (result != free_buffer_ptr)
      gc_death (TERM_BROKEN_HEART,
		"purify: pure copy ended too early",
		result, free_buffer_ptr);
      /*NOTREACHED*/
    pure_length = ((new_free_const - old_free_const) + 1);
  }

  * free_buffer_ptr++ =
    (purify_mode
     ? (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, new_free_const))
     : (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1)));
  * free_buffer_ptr++ = (MAKE_OBJECT (CONSTANT_PART, pure_length));
  new_free_const += 2;
  if (free_buffer_ptr >= free_buffer_top)
    free_buffer_ptr = (purify_header_overflow (free_buffer_ptr));

  scan_start = ((initialize_scan_buffer (block_start)) + delta);
  if (! purify_mode)
    result = (GCLoop (scan_start, &free_buffer_ptr, &new_free_const));
  else
  {
    SCHEME_OBJECT * pure_area_limit = (new_free_const - 2);

    result = (purifyloop (scan_start, &free_buffer_ptr,
			  &new_free_const, CONSTANT_COPY));
    if ((* result) != (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, pure_area_limit)))
      gc_death (TERM_BROKEN_HEART,
		"purify: constant forwarding ended too early",
		result, free_buffer_ptr);
    * result = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
    result = (GCLoop ((result + 2), &free_buffer_ptr, &new_free_const));
  }

  if (result != free_buffer_ptr)
    gc_death (TERM_BROKEN_HEART, "purify: constant copy ended too early",
	      result, free_buffer_ptr);
    /*NOTREACHED*/

  pending_scan = result;
  new_free_const += 2;
  length = (new_free_const - old_free_const);
  * free_buffer_ptr++ = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  * free_buffer_ptr++ = (MAKE_OBJECT (END_OF_BLOCK, (length - 1)));
  if (free_buffer_ptr >= free_buffer_top)
    free_buffer_ptr =
      (dump_and_reset_free_buffer ((free_buffer_ptr - free_buffer_top),
				   NULL));

  Free_Constant = new_free_const;
  if (! (update_allocator_parameters (Free_Constant)))
    gc_death (TERM_NO_SPACE, "purify: object too large", NULL, NULL);
    /*NOTREACHED*/

  while (! (FLOATING_ALIGNED_P (Free_Constant)))
  {
    *free_buffer_ptr++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));
    Free_Constant++;
  }

  if (Constant_Top > Free_Constant)
  {
    /* This assumes that the distance between the new constant space
       and the new free constant is smaller than a bufferfull.
     */

    long bump = (Constant_Top - Free_Constant);

    *free_buffer_ptr = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR,
				     (bump - 1)));
    free_buffer_ptr += bump;
    if (free_buffer_ptr >= free_buffer_top)
      free_buffer_ptr =
	(dump_and_reset_free_buffer ((free_buffer_ptr - free_buffer_top),
				     NULL));
  }

  while (! (FLOATING_ALIGNED_P (Free)))
  {
    *free_buffer_ptr++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));
    Free++;
  }

  root = Free;
  Free += (GC_relocate_root (&free_buffer_ptr));

  saved_const_top = Constant_Top;
  Constant_Top = old_free_const;

  save_scan_state ((&scan_state), pending_scan);
  set_fixed_scan_area (0, Highest_Allocated_Address);

  result = (GCLoop ((CONSTANT_AREA_START ()), &free_buffer_ptr, &Free));
  if (result != old_free_const)
  {
    outf_fatal ("\n%s (purify): The Constant Space scan ended too early.\n",
		scheme_program_name);
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }

  pending_scan = (restore_scan_state (&scan_state));

  result = (GCLoop (pending_scan, &free_buffer_ptr, &Free));
  if (free_buffer_ptr != result)
  {
    outf_fatal ("\n%s (GC): The Heap scan ended too early.\n",
		scheme_program_name);
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }

  root2 = Free;
  *free_buffer_ptr++ = the_precious_objects;
  Free += (free_buffer_ptr - result);
  if (free_buffer_ptr >= free_buffer_top)
    free_buffer_ptr =
      (dump_and_reset_free_buffer ((free_buffer_ptr - free_buffer_top), NULL));

  result = (GCLoop (result, &free_buffer_ptr, &Free));
  if (free_buffer_ptr != result)
  {
    outf_fatal ("\n%s (GC): The Precious Object scan ended too early.\n",
		scheme_program_name);
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }
  end_transport (NULL);
  fix_weak_chain_1 ();

  /* Load new space into memory carefully to prevent the shared
     buffer from losing any values.
   */

  {
    long counter;

    for (counter = 0; counter < delta; counter++)
      scan_buffer_bottom[counter] = block_start[counter];

    final_reload (block_start, (Free - block_start), "new space");

    for (counter = 0; counter < delta; counter++)
      block_start[counter] = scan_buffer_bottom[counter];
  }
  fix_weak_chain_2 ();

  GC_end_root_relocation (root, root2);

  * old_free_const++ = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR,
				     pure_length));
  * old_free_const = (MAKE_OBJECT (PURE_PART, (length - 1)));
  Constant_Top = saved_const_top;
  SEAL_CONSTANT_SPACE ();
  run_post_gc_hooks ();
  return;
}

/* (PRIMITIVE-PURIFY OBJECT PURE? SAFETY-MARGIN)

   Copy an object from the heap into constant space.  It should only
   be used through the wrapper provided in the Scheme runtime system.

   To purify an object we just copy it into Pure Space in two
   parts with the appropriate headers and footers.  The actual
   copying is done by purifyloop above.

   Once the copy is complete we run a full GC which handles the
   broken hearts which now point into pure space.

   This primitive does not return normally.  It always escapes into
   the interpreter because some of its cached registers (eg. History)
   have changed.  
*/

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

  POP_PRIMITIVE_FRAME (3);

  ENTER_CRITICAL_SECTION ("purify");
  purify (object, purify_mode);
  result = (MAKE_POINTER_OBJECT (TC_LIST, Free));
  Free += 2;
  Free[-2] = SHARP_T;
  Free[-1] = (LONG_TO_UNSIGNED_FIXNUM (MemTop - Free));

 Will_Push (CONTINUATION_SIZE);
  Store_Return (RC_NORMAL_GC_DONE);
  Store_Expression (result);
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
