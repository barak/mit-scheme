/* -*-C-*-

$Id: purutl.c,v 9.54 2003/02/14 18:28:23 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

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

/* Pure/Constant space utilities. */

#include "scheme.h"
#include "prims.h"
#include "gccode.h"
#include "zones.h"
#include "cmpint.h"

#ifdef STDC_HEADERS
#  include <stdlib.h>
#endif

static void
DEFUN (update, (From, To, Was, Will_Be),
       fast SCHEME_OBJECT * From
       AND fast SCHEME_OBJECT * To
       AND fast SCHEME_OBJECT * Was
       AND fast SCHEME_OBJECT * Will_Be)
{
  fast long count;

  for (; From < To; From++)
  {
    if (GC_Type_Special (* From))
    {
      switch (OBJECT_TYPE (* From))
      {
	case TC_MANIFEST_NM_VECTOR:
	  From += (OBJECT_DATUM (* From));
	  break;

	  /* The following two type codes assume that none of the protected
	     objects can be updated.
	     This may be seriously wrong!
	   */
	case TC_LINKAGE_SECTION:
	  switch (READ_LINKAGE_KIND (* From))
	  {
	    case ASSIGNMENT_LINKAGE_KIND:
	    case CLOSURE_PATTERN_LINKAGE_KIND:
	    case REFERENCE_LINKAGE_KIND:
	    {
	      From += (READ_CACHE_LINKAGE_COUNT (* From));
	      break;
	    }

	    case GLOBAL_OPERATOR_LINKAGE_KIND:
	    case OPERATOR_LINKAGE_KIND:
	    {
	      count = (READ_OPERATOR_LINKAGE_COUNT (* From));
	      From = (END_OPERATOR_LINKAGE_AREA (From, count));
	      break;
	    }

	    default:
#ifdef BAD_TYPES_LETHAL
	    {
	      gc_death (TERM_EXIT,
			"Impurify: Unknown compiler linkage kind.",
			From, NULL);
	      /*NOTREACHED*/
	    }
#else /* not BAD_TYPES_LETHAL */
	    outf_error ("\nImpurify: Bad linkage section (0x%lx).\n",
			(* From));
#endif /* BAD_TYPES_LETHAL */
	  }
	  break;

	case TC_MANIFEST_CLOSURE:
	{
	  fast long count;

	  From += 1;
	  count = (MANIFEST_CLOSURE_COUNT (From));
	  From = ((MANIFEST_CLOSURE_END (From, count)) - 1);
	  break;
	}

	default:
	  break;
      }
    }
    else if ((! (GC_Type_Non_Pointer (* From)))
	     && ((OBJECT_ADDRESS (* From)) == Was))
      * From = (MAKE_POINTER_OBJECT (OBJECT_TYPE (* From), Will_Be));
  }
  return;
}

extern SCHEME_OBJECT * EXFUN (find_constant_space_block, (SCHEME_OBJECT *));

long
DEFUN (make_impure, (Object, New_Object),
       SCHEME_OBJECT Object AND SCHEME_OBJECT * New_Object)
{
  fast SCHEME_OBJECT * Obj_Address, * Constant_Address;
  SCHEME_OBJECT * New_Address, * End_Of_Area;
  long Length, Block_Length;
  fast long i;

  /* Calculate size of object to be "impurified".
     Note that this depends on the fact that Compiled Entries CANNOT
     be pure.
   */

  Switch_by_GC_Type (Object)
  {
    case TC_BROKEN_HEART:
    case TC_MANIFEST_NM_VECTOR:
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
    case_Non_Pointer:
#if FALSE
      outf_fatal ("\nImpurify Non-Pointer (0x%lx)\n", Object);
      Microcode_Termination (TERM_NON_POINTER_RELOCATION);
      /* fall through */
#endif
    case TC_BIG_FLONUM:
      return (ERR_ARG_1_WRONG_TYPE);

    case TC_FUTURE:
    case_Vector:
      Length = ((VECTOR_LENGTH (Object)) + 1);
      break;

    case_Quadruple:
      Length = 4;
      break;

    case TC_VARIABLE:
    case_Triple:
      Length = 3;
      break;

    case TC_WEAK_CONS:
    case_Pair:
      Length = 2;
      break;

    case_Cell:
      Length = 1;
      break;

    case TC_LINKAGE_SECTION:
    case TC_MANIFEST_CLOSURE:
    case_compiled_entry_point:
    default:
#ifdef BAD_TYPES_LETHAL
      outf_fatal ("\nImpurify: Bad type code = 0x%02x.\n",
	          OBJECT_TYPE (Object));
      Microcode_Termination (TERM_INVALID_TYPE_CODE);
      /*NOTREACHED*/
#else /* not BAD_TYPES_LETHAL */
      outf_error ("\nImpurify: Bad type code = 0x%02x.\n",
	          OBJECT_TYPE (Object));
      return (ERR_ARG_1_WRONG_TYPE);
#endif /* BAD_TYPES_LETHAL */
  }

  Constant_Address = Free_Constant;

#ifdef FLOATING_ALIGNMENT

  /* Undo ALIGN_FLOAT(Free_Constant) in SET_CONSTANT_TOP (). */

  while ((* (Constant_Address - 1))
	 == (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0)))
    Constant_Address -= 1;

#endif /* FLOATING_ALIGNMENT */

  Obj_Address = (OBJECT_ADDRESS (Object));

  if (! (TEST_CONSTANT_TOP (Constant_Address + Length)))
  {
    /* Make the whole block impure! */

    SCHEME_OBJECT * block = (find_constant_space_block (Obj_Address));

    if (block == ((SCHEME_OBJECT *) NULL))
      return (ERR_IMPURIFY_OUT_OF_SPACE);

    * block = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
    * New_Object = Object;
    return (PRIM_DONE);
  }

  /*
    Add a copy of the object to the last constant block in memory.
   */

  Block_Length = (OBJECT_DATUM (* (Constant_Address - 1)));
  Constant_Address -= 2;
  New_Address = Constant_Address;

  for (i = Length; --i >= 0; )
  {
    *Constant_Address++ = *Obj_Address;
    *Obj_Address++ = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, i));
  }

  *Constant_Address++ = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  *Constant_Address++ = (MAKE_OBJECT (END_OF_BLOCK, Block_Length + Length));
  *(New_Address + 2 - Block_Length) =
    (MAKE_OBJECT (PURE_PART, Block_Length + Length));
  Obj_Address -= Length;
  Free_Constant = Constant_Address;
  SET_CONSTANT_TOP ();

  /* Run through memory relocating pointers to this object, including
   * those in pure areas.
   */

  Terminate_Old_Stacklet ();
  SEAL_CONSTANT_SPACE ();
  End_Of_Area = (CONSTANT_AREA_END ());

  ENTER_CRITICAL_SECTION ("impurify");

  update (Heap_Bottom, Free, Obj_Address, New_Address);
  update ((CONSTANT_AREA_START ()), End_Of_Area, Obj_Address, New_Address);

  EXIT_CRITICAL_SECTION ({});

  * New_Object = (MAKE_POINTER_OBJECT (OBJECT_TYPE (Object), New_Address));
  return (PRIM_DONE);
}

DEFINE_PRIMITIVE ("PRIMITIVE-IMPURIFY", Prim_impurify, 1, 1,
  "(object)\n\
Remove OBJECT from pure space so it can be side effected.\n\
The object is placed in constant space instead if it fits,\n\
otherwise the whole block where it lives in pure space is marked\n\
as being in constant space.")
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT old_object;
    SCHEME_OBJECT new_object;
    TOUCH_IN_PRIMITIVE ((ARG_REF (1)), old_object);
    {
      long result = (make_impure (old_object, (&new_object)));
      if (result != PRIM_DONE)
	signal_error_from_primitive (result);
    }
    PRIMITIVE_RETURN (new_object);
  }
}

SCHEME_OBJECT *
DEFUN (find_constant_space_block, (obj_address),
       fast SCHEME_OBJECT * obj_address)
{
  fast SCHEME_OBJECT * where, * low_constant;

  low_constant = Constant_Space;
  where = (Free_Constant - 1);

  while (where >= low_constant)
  {
#if FALSE
    /* Skip backwards over turds left over by ALIGN_FLOAT */

    /* This should be #ifdef FLOATING_ALIGNMENT, but
       works by serendipity since the padding turds have a
       datum of 0 and are correctly skipped over.
     */

    if (* where = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0)))
    {
      where -= 1;
      continue;
    }
#endif
    where -= (1 + (OBJECT_DATUM (* where)));
    if (where < obj_address)
      return (where + 1);
  }
  return ((SCHEME_OBJECT *) NULL);
}

Boolean
DEFUN (Pure_Test, (obj_address), SCHEME_OBJECT * obj_address)
{
  SCHEME_OBJECT * block;

  block = (find_constant_space_block (obj_address));
  if (block == ((SCHEME_OBJECT *) NULL))
    return (false);
  return
    ((Boolean) (obj_address <= (block + (OBJECT_DATUM (* block)))));
}

DEFINE_PRIMITIVE ("PURE?", Prim_pure_p, 1, 1,
  "Return #T if OBJECT is pure (i.e. it doesn't point to any other object,\n\
or it is in a pure section of the constant space).")
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT object = (ARG_REF (1));
    if ((GC_Type_Non_Pointer (object)) ||
	(GC_Type_Special (object)))
      PRIMITIVE_RETURN (SHARP_T);
    TOUCH_IN_PRIMITIVE (object, object);
    {
      SCHEME_OBJECT * address =
	((GC_Type_Compiled (object))
	 ? (compiled_entry_to_block_address (object))
	 : (OBJECT_ADDRESS (object)));
      PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (ADDRESS_PURE_P (address)));
    }
  }
}

DEFINE_PRIMITIVE ("CONSTANT?", Prim_constant_p, 1, 1,
  "Return #T if OBJECT is in constant space or isn't a pointer.")
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT object = (ARG_REF (1));
    if ((GC_Type_Non_Pointer (object)) || (GC_Type_Special (object)))
      PRIMITIVE_RETURN (SHARP_T);
    TOUCH_IN_PRIMITIVE (object, object);
    PRIMITIVE_RETURN
      (BOOLEAN_TO_OBJECT (ADDRESS_CONSTANT_P (OBJECT_ADDRESS (object))));
  }
}

DEFINE_PRIMITIVE ("GET-NEXT-CONSTANT", Prim_get_next_constant, 0, 0,
  "Return the next free address in constant space.")
{
  SCHEME_OBJECT * next_address = (Free_Constant + 1);
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (ADDRESS_TO_DATUM (next_address)));
}

/* copy_to_constant_space is a microcode utility procedure.
   It takes care of making legal constant space blocks.
   The microcode kills itself if there is not enough constant
   space left.
 */

extern SCHEME_OBJECT *copy_to_constant_space();

SCHEME_OBJECT *
DEFUN (copy_to_constant_space,
       (source, nobjects),
       fast SCHEME_OBJECT *source AND
       long nobjects)
{
  fast long i;
  fast SCHEME_OBJECT * dest;
  SCHEME_OBJECT * result;

  dest = Free_Constant;
  if (!(TEST_CONSTANT_TOP (dest + nobjects + 6)))
  {
    outf_fatal ("copy_to_constant_space: Not enough constant space!\n");
    Microcode_Termination (TERM_NO_SPACE);
  }
  *dest++ = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 3));
  *dest++ = (MAKE_OBJECT (PURE_PART, nobjects + 5));
  *dest++ = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  *dest++ = (MAKE_OBJECT (CONSTANT_PART, 3));
  result = dest;
  for (i = nobjects; --i >= 0; )
    *dest++ = *source++;
  *dest++ = (MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1));
  *dest++ = (MAKE_OBJECT (END_OF_BLOCK, nobjects + 5));
  Free_Constant = dest;
  SET_CONSTANT_TOP ();

  return (result);
}

gc_hook_list  pre_gc_hooks = ((gc_hook_list) NULL);
gc_hook_list post_gc_hooks = ((gc_hook_list) NULL);

static int
DEFUN (add_gc_hook, (cell, hook),
       gc_hook_list * cell AND void EXFUN ((* hook), (void)))
{
  gc_hook_list new = ((gc_hook_list)
		      (malloc (sizeof (struct gc_hook_list_s))));
  if (new == ((gc_hook_list) NULL))
    return (-1);

  new->hook = hook;
  new->next = ((gc_hook_list) NULL);

  while ((* cell) != ((gc_hook_list) NULL))
    cell = (& ((* cell)->next));

  * cell = new;
  return (0);
}

static void
DEFUN (run_gc_hooks, (gc_hooks), gc_hook_list gc_hooks)
{
  while (gc_hooks != ((gc_hook_list) NULL))
  {
    (* (gc_hooks->hook)) ();
    gc_hooks = gc_hooks->next;
  }
  return;
}

int
DEFUN (add_pre_gc_hook, (hook),
       void EXFUN ((* hook), (void)))
{
  return (add_gc_hook ((& pre_gc_hooks), hook));
}

int
DEFUN (add_post_gc_hook, (hook),
       void EXFUN ((* hook), (void)))
{
  return (add_gc_hook ((& post_gc_hooks), hook));
}

void
DEFUN_VOID (run_pre_gc_hooks)
{
  run_gc_hooks (pre_gc_hooks);
  return;
}

void
DEFUN_VOID (run_post_gc_hooks)
{
  run_gc_hooks (post_gc_hooks);
  return;
}
