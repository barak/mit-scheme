/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prim.c,v 9.31 1988/08/15 20:52:46 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

/* The leftovers ... primitives that don't seem to belong elsewhere. */

#include "scheme.h"
#include "prims.h"

/* Low level object manipulation */

/* (PRIMITIVE-OBJECT-TYPE OBJECT)
   Returns the type code of OBJECT as an unsigned integer.  */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-TYPE", Prim_prim_obj_type, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (OBJECT_TYPE (ARG_REF (1))));
}

/* (PRIMITIVE-OBJECT-GC-TYPE OBJECT)
   Returns an unsigned integer indicating the GC type of the object.  */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-GC-TYPE", Prim_prim_obj_gc_type, 1, 1, 0)
{
  PRIMITIVE_HEADER (1); 

  PRIMITIVE_RETURN
    (MAKE_SIGNED_FIXNUM (GC_Type_Map [OBJECT_TYPE (ARG_REF (1))]));
}

/* (PRIMITIVE-OBJECT-TYPE? TYPE-CODE OBJECT)
   Return #T if the type code of OBJECT is TYPE-CODE, else #F.  */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-TYPE?", Prim_prim_obj_type_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);

  PRIMITIVE_RETURN
    (((OBJECT_TYPE (ARG_REF (2))) ==
      (arg_index_integer (1, (MAX_TYPE_CODE + 1))))
     ? SHARP_T
     : NIL);
}

/* (PRIMITIVE-OBJECT-DATUM OBJECT)
   Returns the datum part of OBJECT as an unsigned integer. */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-DATUM", Prim_prim_obj_datum, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (OBJECT_DATUM (ARG_REF (1))));
}

/* (MAKE-NON-POINTER-OBJECT NUMBER)
   Converts the unsigned integer NUMBER into a fixnum, by creating an
   object whose type is TC_FIXNUM and whose datum is NUMBER.  */

DEFINE_PRIMITIVE ("MAKE-NON-POINTER-OBJECT", Prim_make_non_pointer_object, 1, 1, 0)
{
  fast long datum;
  PRIMITIVE_HEADER (1);

  datum = (object_to_long ((ARG_REF (1)),
			   ERR_ARG_1_WRONG_TYPE,
			   ERR_ARG_1_BAD_RANGE));
  if ((datum < 0) || (datum > ADDRESS_MASK))
    error_bad_range_arg (1);
  PRIMITIVE_RETURN (MAKE_FIXNUM (datum));
}

/* (PRIMITIVE-OBJECT-SET-TYPE TYPE-CODE OBJECT)
   Returns a new object with TYPE-CODE and the datum part of OBJECT.  */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-SET-TYPE", Prim_prim_obj_set_type, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);

  PRIMITIVE_RETURN
    (Make_New_Pointer ((arg_index_integer (1, (MAX_TYPE_CODE + 1))),
		       (ARG_REF (2))));
}

/* (PRIMITIVE-OBJECT-EQ? OBJECT-1 OBJECT-2)
   Returns #T if the two objects have the same type code and datum.
   Returns #F otherwise.  */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-EQ?", Prim_prim_obj_eq_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);

  PRIMITIVE_RETURN (((ARG_REF (1)) == (ARG_REF (2))) ? SHARP_T : NIL);
}

/* Low level memory references.

   Many primitives can be built out of these, and eventually should be.
   These are extremely unsafe, since there is no consistency checking.
   In particular, they are not gc-safe: You can screw yourself royally
   by using them.  */

/* (PRIMITIVE-OBJECT-REF OBJECT INDEX)
   Fetches the index'ed slot in object.
   Performs no type checking on object.  */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-REF", Prim_prim_obj_ref, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);

  PRIMITIVE_RETURN (Vector_Ref ((ARG_REF (1)), (arg_nonnegative_integer (2))));
}

/* (PRIMITIVE-OBJECT-SET! OBJECT INDEX VALUE)
   Stores value in the index'ed slot in object.
   Performs no type checking on object.  */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-SET!", Prim_prim_obj_set, 3, 3, 0)
{
  fast long index;
  PRIMITIVE_HEADER (3);

  index = (arg_nonnegative_integer (2));
  PRIMITIVE_RETURN
    (Swap_Pointers (Nth_Vector_Loc ((ARG_REF (1)), index), (ARG_REF (3))));
}

/* Safe versions of the object manipulators.
   These touch their arguments, and provide GC safety tests.  */

DEFINE_PRIMITIVE ("OBJECT-TYPE", Prim_object_type, 1, 1, 0)
{
  fast Pointer object;
  PRIMITIVE_HEADER (1);

  Touch_In_Primitive ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (MAKE_UNSIGNED_FIXNUM (OBJECT_TYPE (object)));
}

DEFINE_PRIMITIVE ("OBJECT-GC-TYPE", Prim_object_gc_type, 1, 1, 0)
{
  fast Pointer object;
  PRIMITIVE_HEADER (1); 

  Touch_In_Primitive ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (MAKE_SIGNED_FIXNUM (GC_Type (object)));
}

DEFINE_PRIMITIVE ("OBJECT-TYPE?", Prim_object_type_p, 2, 2, 0)
{
  fast Pointer object;
  PRIMITIVE_HEADER (2);

  Touch_In_Primitive ((ARG_REF (2)), object);
  PRIMITIVE_RETURN
    (((OBJECT_TYPE (object)) ==
      (arg_index_integer (1, (MAX_TYPE_CODE + 1))))
     ? SHARP_T
     : NIL);
}

DEFINE_PRIMITIVE ("OBJECT-DATUM", Prim_object_datum, 1, 1, 0)
{
  fast Pointer object;
  PRIMITIVE_HEADER (1);

  Touch_In_Primitive ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (C_Integer_To_Scheme_Integer (OBJECT_DATUM (object)));
}

DEFINE_PRIMITIVE ("OBJECT-SET-TYPE", Prim_object_set_type, 2, 2, 0)
{
  fast long type_code;
  fast Pointer object;
  PRIMITIVE_HEADER (2);

  type_code = (arg_index_integer (1, (MAX_TYPE_CODE + 1)));
  Touch_In_Primitive ((ARG_REF (2)), object);
  {
    fast long gc_type_code;

    gc_type_code = (GC_Type_Map [type_code]);
    if ((gc_type_code == GC_Undefined) ||
	(! ((gc_type_code == GC_Non_Pointer) ||
	    (gc_type_code == (GC_Type (object))))))
      error_bad_range_arg (1);
  }
  PRIMITIVE_RETURN (Make_New_Pointer (type_code, object));
}

/* (EQ? OBJECT-1 OBJECT-2)
   Returns #T if the two objects have the same type code and datum.
   Returns #F otherwise.
   Touches both arguments.  */

DEFINE_PRIMITIVE ("EQ?", Prim_eq, 2, 2, 0)
{
  fast Pointer object_1;
  fast Pointer object_2;
  PRIMITIVE_HEADER (2);

  Touch_In_Primitive ((ARG_REF (1)), object_1);
  Touch_In_Primitive ((ARG_REF (2)), object_2);
  PRIMITIVE_RETURN ((object_1 == object_2) ? SHARP_T : NIL);
}

/* (NOT OBJECT)
   Returns #T if OBJECT is #F.  Otherwise returns #F.  This is
   the primitive known as NOT, NULL?, and FALSE? in Scheme.
   Touches the argument.  */

DEFINE_PRIMITIVE ("NOT", Prim_not, 1, 1, 0)
{
  fast Pointer object;
  PRIMITIVE_HEADER (1);

  Touch_In_Primitive ((ARG_REF (1)), object);
  PRIMITIVE_RETURN ((object == NIL) ? SHARP_T : NIL);
}

/* Cells */

/* (MAKE-CELL CONTENTS)
   Creates a cell with contents CONTENTS. */

DEFINE_PRIMITIVE ("MAKE-CELL", Prim_make_cell, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  Primitive_GC_If_Needed (1);
  (*Free++) = (ARG_REF (1));
  PRIMITIVE_RETURN (Make_Pointer (TC_CELL, (Free - 1)));
}

/* (CELL? OBJECT)
   Returns #T if OBJECT is a cell, else #F.  */

DEFINE_PRIMITIVE ("CELL?", Prim_cell_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN ((CELL_P (ARG_REF (1))) ? SHARP_T : NIL);
}

/* (CELL-CONTENTS CELL)
   Returns the contents of the cell CELL.  */

DEFINE_PRIMITIVE ("CELL-CONTENTS", Prim_cell_contents, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (Vector_Ref ((CELL_ARG (1)), CELL_CONTENTS));
}

/* (SET-CELL-CONTENTS! CELL OBJECT)
   Stores OBJECT as contents of CELL.
   Returns the previous contents of CELL. */

DEFINE_PRIMITIVE ("SET-CELL-CONTENTS!", Prim_set_cell_contents, 2, 2, 0)
{
  fast Pointer cell;
  fast Pointer object;
  PRIMITIVE_HEADER (2);

  cell = (CELL_ARG (1));
  object = (ARG_REF (2));
  Side_Effect_Impurify (cell, object);
  PRIMITIVE_RETURN
    (Swap_Pointers ((Nth_Vector_Loc (cell, CELL_CONTENTS)), object));
}
