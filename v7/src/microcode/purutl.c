/* -*-C-*-

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/purutl.c,v 9.39 1990/06/28 18:18:11 jinx Rel $ */

/* Pure/Constant space utilities. */

#include "scheme.h"
#include "prims.h"
#include "gccode.h"
#include "zones.h"

static void
Update(From, To, Was, Will_Be)
     fast SCHEME_OBJECT *From, *To, *Was, *Will_Be;
{
  fast long count;

  for (; From < To; From++)
  {
    if (GC_Type_Special(*From))
    {
      switch(OBJECT_TYPE (*From))
      {
	case TC_MANIFEST_NM_VECTOR:
	  From += OBJECT_DATUM (*From);
	  continue;

	  /* The following two type codes assume that none of the protected
	     objects can be updated.
	     This may be seriously wrong!
	   */
	case TC_LINKAGE_SECTION:
	  if (READ_LINKAGE_KIND(*From) != OPERATOR_LINKAGE_KIND)
	  {
	    From += READ_CACHE_LINKAGE_COUNT(*From);
	    continue;
	  }
	  else
	  {
	    count = READ_OPERATOR_LINKAGE_COUNT(*From);
	    From = END_OPERATOR_LINKAGE_AREA(From, count);
	    continue;
	  }

	case TC_MANIFEST_CLOSURE:
	{
	  fast long count;

	  From += 1;
	  count = (MANIFEST_CLOSURE_COUNT (From));
	  From = (MANIFEST_CLOSURE_END (From, count));
	  continue;
	}

	default:
	  continue;
      }
    }
    if (GC_Type_Non_Pointer(*From))
      continue;
    if (OBJECT_ADDRESS (*From) == Was)
      *From = MAKE_POINTER_OBJECT (OBJECT_TYPE (*From), Will_Be);
  }
  return;
}

long
Make_Impure(Object, New_Object)
     SCHEME_OBJECT Object, *New_Object;
{
  SCHEME_OBJECT *New_Address, *End_Of_Area;
  fast SCHEME_OBJECT *Obj_Address, *Constant_Address;
  long Length, Block_Length;
  fast long i;

  /* Calculate size of object to be "impurified".
     Note that this depends on the fact that Compiled Entries CANNOT
     be pure.
   */

  Switch_by_GC_Type(Object)
  {
    case TC_BROKEN_HEART:
    case TC_MANIFEST_NM_VECTOR:
    case TC_MANIFEST_SPECIAL_NM_VECTOR:
    case_Non_Pointer:
#if false
      fprintf(stderr, "\nImpurify Non-Pointer (0x%lx)\n", Object);
      Microcode_Termination(TERM_NON_POINTER_RELOCATION);
      /* fall through */
#endif
    case TC_BIG_FLONUM:
      return (ERR_ARG_1_WRONG_TYPE);

    case TC_FUTURE:
    case_Vector:
      Length = VECTOR_LENGTH (Object) + 1;
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
      fprintf(stderr, "\nImpurify: Bad type code = 0x%02x.\n",
	      OBJECT_TYPE (Object));
#ifdef BAD_TYPES_LETHAL
      Microcode_Termination(TERM_INVALID_TYPE_CODE);
      /*NOTREACHED*/
#else /* not BAD_TYPES_LETHAL */
      return (ERR_ARG_1_WRONG_TYPE);
#endif /* BAD_TYPES_LETHAL */
  }

  /* Add a copy of the object to the last constant block in memory.
   */

  Constant_Address = Free_Constant;

  Obj_Address = OBJECT_ADDRESS (Object);
  if (!Test_Pure_Space_Top(Constant_Address + Length))
  {
    return (ERR_IMPURIFY_OUT_OF_SPACE);
  }
  Block_Length = OBJECT_DATUM (*(Constant_Address-1));
  Constant_Address -= 2;
  New_Address = Constant_Address;

  for (i = Length; --i >= 0; )
  {
    *Constant_Address++ = *Obj_Address;
    *Obj_Address++ = MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, i);
  }

  *Constant_Address++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *Constant_Address++ = MAKE_OBJECT (END_OF_BLOCK, Block_Length + Length);
  *(New_Address + 2 - Block_Length) =
    MAKE_OBJECT (PURE_PART, Block_Length + Length);
  Obj_Address -= Length;
  Free_Constant = Constant_Address;

  /* Run through memory relocating pointers to this object, including
   * those in pure areas.
   */

  Set_Pure_Top();
  Terminate_Old_Stacklet();
  Terminate_Constant_Space(End_Of_Area);

  ENTER_CRITICAL_SECTION ("impurify");

  Update(Heap_Bottom, Free, Obj_Address, New_Address);
  Update(Constant_Space, End_Of_Area, Obj_Address, New_Address);

  EXIT_CRITICAL_SECTION ({});

  *New_Object = (MAKE_POINTER_OBJECT (OBJECT_TYPE (Object), New_Address));
  return (PRIM_DONE);
}

DEFINE_PRIMITIVE ("PRIMITIVE-IMPURIFY", Prim_impurify, 1, 1,
  "Remove OBJECT from pure space so it can be side effected.\n\
The object is placed in constant space instead.")
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT old_object;
    SCHEME_OBJECT new_object;
    TOUCH_IN_PRIMITIVE ((ARG_REF (1)), old_object);
    {
      fast long result = (Make_Impure (old_object, (&new_object)));
      if (result != PRIM_DONE)
	signal_error_from_primitive (result);
    }
    PRIMITIVE_RETURN (new_object);
  }
}

extern SCHEME_OBJECT * find_constant_space_block();

SCHEME_OBJECT *
find_constant_space_block(obj_address)
     fast SCHEME_OBJECT *obj_address;
{
  fast SCHEME_OBJECT *where, *low_constant;

  low_constant = Constant_Space;
  where = (Free_Constant - 1);

  while (where >= low_constant)
  {
    where -= (1 + OBJECT_DATUM (*where));
    if (where < obj_address)
    {
      return (where + 1);
    }
  }
  return ((SCHEME_OBJECT *) NULL);
}

Boolean
Pure_Test(obj_address)
     SCHEME_OBJECT *obj_address;
{
  SCHEME_OBJECT *block;

  block = find_constant_space_block (obj_address);
  if (block == ((SCHEME_OBJECT *) NULL))
  {
    return (false);
  }
  return
    ((Boolean) (obj_address <= (block + (OBJECT_DATUM (*block)))));
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
      extern SCHEME_OBJECT * compiled_entry_to_block_address ();
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
    if ((GC_Type_Non_Pointer (object)) ||
	(GC_Type_Special (object)))
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
copy_to_constant_space(source, nobjects)
     fast SCHEME_OBJECT *source;
     long nobjects;
{
  fast SCHEME_OBJECT *dest;
  fast long i;
  SCHEME_OBJECT *result;

  dest = Free_Constant;
  if (!Test_Pure_Space_Top(dest + nobjects + 6))
  {
    fprintf(stderr,
	    "copy_to_constant_space: Not enough constant space!\n");
    Microcode_Termination(TERM_NO_SPACE);
  }
  *dest++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 3);
  *dest++ = MAKE_OBJECT (PURE_PART, nobjects + 5);
  *dest++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *dest++ = MAKE_OBJECT (CONSTANT_PART, 3);
  result = dest;
  for (i = nobjects; --i >= 0; )
  {
    *dest++ = *source++;
  }
  *dest++ = MAKE_OBJECT (TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *dest++ = MAKE_OBJECT (END_OF_BLOCK, nobjects + 5);
  Free_Constant = dest;

  return result;
}
