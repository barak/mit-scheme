/* -*-C-*-

$Id: bchpur.c,v 9.69 2000/12/05 21:23:42 cph Exp $

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

static void EXFUN (purify, (SCHEME_OBJECT, Boolean));
static SCHEME_OBJECT * EXFUN (purify_header_overflow, (SCHEME_OBJECT *));

/* (PRIMITIVE-PURIFY OBJECT PURE? SAFETY-MARGIN)

   Copy an object from the heap into constant space.  It should only
   be used through the wrapper provided in the Scheme runtime system.

   To purify an object we just copy it into Pure Space in two
   parts with the appropriate headers and footers.  The actual
   copying is done by gc_loop.

   Once the copy is complete we run a full GC which handles the
   broken hearts which now point into pure space.

   This primitive does not return normally.  It always escapes into
   the interpreter because some of its cached registers (eg. History)
   have changed.  */

DEFINE_PRIMITIVE ("PRIMITIVE-PURIFY", Prim_primitive_purify, 3, 3, 0)
{
  Boolean pure_p;
  SCHEME_OBJECT object, result, daemon;
  PRIMITIVE_HEADER (3);
  PRIMITIVE_CANONICALIZE_CONTEXT ();

  STACK_SANITY_CHECK ("PURIFY");
  Save_Time_Zone (Zone_Purify);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  CHECK_ARG (2, BOOLEAN_P);
  pure_p = (BOOLEAN_ARG (2));
  GC_Reserve = (arg_nonnegative_integer (3));

  POP_PRIMITIVE_FRAME (3);

  ENTER_CRITICAL_SECTION ("purify");
  purify (object, pure_p);
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
    {
      PRIMITIVE_ABORT (PRIM_POP_RETURN);
      /*NOTREACHED*/
    }

 Will_Push (2);
  STACK_PUSH (daemon);
  STACK_PUSH (STACK_FRAME_HEADER);
 Pushed ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  return (UNSPECIFIC);
}

static void
DEFUN (purify, (object, pure_p), SCHEME_OBJECT object AND Boolean pure_p)
{
  long length;
  long pure_length;
  long delta;
  SCHEME_OBJECT * free_buffer_ptr;
  SCHEME_OBJECT * old_free_const;
  SCHEME_OBJECT * block_start;
  SCHEME_OBJECT * new_free_const;
  SCHEME_OBJECT * pending_scan;
  SCHEME_OBJECT * root;
  SCHEME_OBJECT * root2;
  SCHEME_OBJECT the_precious_objects;

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
  (*free_buffer_ptr++) = SHARP_F;	/* Pure block header. */
  (*free_buffer_ptr++) = object;
  new_free_const += 2;
  if (free_buffer_ptr >= free_buffer_top)
    free_buffer_ptr = (dump_and_reset_free_buffer (free_buffer_ptr, 0));

  if (pure_p)
    {
      gc_loop (((initialize_scan_buffer (block_start)) + delta),
	       (&free_buffer_ptr), (&new_free_const), Constant_Top,
	       PURE_COPY, 1);
      pure_length = ((new_free_const - old_free_const) + 1);
    }
  else
    pure_length = 3;

  (*free_buffer_ptr++)
    = (pure_p
       ? (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, new_free_const))
       : (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1)));
  (*free_buffer_ptr++) = (MAKE_OBJECT (CONSTANT_PART, pure_length));
  new_free_const += 2;
  if (free_buffer_ptr >= free_buffer_top)
    free_buffer_ptr = (purify_header_overflow (free_buffer_ptr));

  {
    SCHEME_OBJECT * scan_start
      = ((initialize_scan_buffer (block_start)) + delta);
    if (pure_p)
      {
	SCHEME_OBJECT * pure_area_limit = (new_free_const - 2);
	SCHEME_OBJECT * result
	  = (gc_loop (scan_start, (&free_buffer_ptr), (&new_free_const),
		      Constant_Top, CONSTANT_COPY, 0));
	if ((*result)
	    != (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, pure_area_limit)))
	  {
	    gc_death (TERM_BROKEN_HEART, "gc_loop ended too early",
		      result, free_buffer_ptr);
	    /*NOTREACHED*/
	  }
	(*result) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
	scan_start = (result + 2);
      }
    pending_scan
      = (gc_loop (scan_start, (&free_buffer_ptr), (&new_free_const),
		  Constant_Top, NORMAL_GC, 1));
  }

  if (result != free_buffer_ptr)
    gc_death (TERM_BROKEN_HEART, "purify: constant copy ended too early",
	      result, free_buffer_ptr);
    /*NOTREACHED*/

  length = (new_free_const + 1 - old_free_const);
  (*free_buffer_ptr++) = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  (*free_buffer_ptr++) = (MAKE_OBJECT (END_OF_BLOCK, length));
  new_free_const += 2;
  if (free_buffer_ptr >= free_buffer_top)
    free_buffer_ptr = (dump_and_reset_free_buffer (free_buffer_ptr, 0));

  Free_Constant = new_free_const;
  if (!update_allocator_parameters (Free_Constant))
    {
      gc_death (TERM_NO_SPACE, "purify: object too large", NULL, NULL);
      /*NOTREACHED*/
    }
  while (!FLOATING_ALIGNED_P (Free_Constant))
    {
      (*free_buffer_ptr++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));
      Free_Constant += 1;
    }
  if (Constant_Top > Free_Constant)
    {
      /* This assumes that the distance between the new constant space
	 and the new free constant is smaller than a bufferful.  */
      long bump = (Constant_Top - Free_Constant);
      (*free_buffer_ptr)
	= (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, (bump - 1)));
      free_buffer_ptr += bump;
      if (free_buffer_ptr >= free_buffer_top)
	free_buffer_ptr = (dump_and_reset_free_buffer (free_buffer_ptr, 0));
    }
  while (!FLOATING_ALIGNED_P (Free))
    {
      (*free_buffer_ptr++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0));
      Free += 1;
    }

  root = Free;
  Free += (GC_relocate_root (&free_buffer_ptr));

  {
    struct saved_scan_state scan_state;
    save_scan_state ((&scan_state), pending_scan);
    set_fixed_scan_area (0, Highest_Allocated_Address);
    {
      SCHEME_OBJECT * result
	= (gc_loop ((CONSTANT_AREA_START ()), (&free_buffer_ptr), (&Free),
		    old_free_const, NORMAL_GC, 0));
      if (result != old_free_const)
	{
	  gc_death (TERM_EXIT, "gc_loop ended too early",
		    result, free_buffer_ptr);
	  /*NOTREACHED*/
	}
    }
    pending_scan = (restore_scan_state (&scan_state));
  }

  pending_scan
    = (gc_loop (pending_scan, (&free_buffer_ptr), (&Free),
		old_free_const, NORMAL_GC, 1));

  root2 = Free;
  (*free_buffer_ptr++) = the_precious_objects;
  Free += 1;
  if (free_buffer_ptr >= free_buffer_top)
    free_buffer_ptr = (dump_and_reset_free_buffer (free_buffer_ptr, 0));

  gc_loop (pending_scan, (&free_buffer_ptr), (&Free),
	   old_free_const, NORMAL_GC, 1);

  end_transport (0);
  fix_weak_chain_1 (old_free_const);

  /* Load new space into memory carefully to prevent the shared
     buffer from losing any values.  */
  {
    unsigned long counter;

    for (counter = 0; (counter < delta); counter += 1)
      (scan_buffer_bottom[counter]) = (block_start[counter]);

    final_reload (block_start, (Free - block_start), "new space");

    for (counter = 0; (counter < delta); counter += 1)
      (block_start[counter]) = (scan_buffer_bottom[counter]);
  }

  fix_weak_chain_2 ();
  GC_end_root_relocation (root, root2);

  (*old_free_const++)
    = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, pure_length));
  (*old_free_const) = (MAKE_OBJECT (PURE_PART, length));
  SEAL_CONSTANT_SPACE ();
  run_post_gc_hooks ();
}

/* This is not paranoia!
   The two words in the header may overflow the free buffer.  */
static SCHEME_OBJECT *
DEFUN (purify_header_overflow, (free_buffer), SCHEME_OBJECT * free_buffer)
{
  long delta = (free_buffer - free_buffer_top);
  free_buffer = (dump_and_reset_free_buffer (free_buffer, 0));
  {
    SCHEME_OBJECT * scan_buffer
      = (dump_and_reload_scan_buffer (scan_buffer_top, 0));
    if ((scan_buffer + delta) != free_buffer)
      {
	gc_death (TERM_EXIT,
		  "purify: scan and free do not meet at the end",
		  (scan_buffer + delta), free_buffer);
	/*NOTREACHED*/
      }
  }
  return (free_buffer);
}
