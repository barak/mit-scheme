/* -*-C-*-

$Id: prim.c,v 9.44 2005/06/30 20:04:22 cph Exp $

Copyright 1986,1987,1988,1989,1992,1993 Massachusetts Institute of Technology
Copyright 1996,2004,2005 Massachusetts Institute of Technology

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

/* The leftovers ... primitives that don't seem to belong elsewhere. */

#include "scheme.h"
#include "prims.h"

/* Low level object manipulation */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-TYPE", Prim_prim_obj_type, 1, 1,
  "Return the type code of OBJECT as an unsigned integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (OBJECT_TYPE (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-GC-TYPE", Prim_prim_obj_gc_type, 1, 1,
  "Return an unsigned integer indicating the GC type of the object.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (LONG_TO_FIXNUM (GC_Type_Map [OBJECT_TYPE (ARG_REF (1))]));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-TYPE?", Prim_prim_obj_type_p, 2, 2,
  "Return #T if TYPE-CODE is OBJECT's type code, else #F.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT
     (((long) (OBJECT_TYPE (ARG_REF (2))))
      == (arg_index_integer (1, (MAX_TYPE_CODE + 1)))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-DATUM", Prim_prim_obj_datum, 1, 1,
  "Return the datum part of OBJECT as an unsigned integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (OBJECT_DATUM (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-ADDRESS", Prim_prim_obj_address, 1, 1,
  "Return the address part of OBJECT as an unsigned integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer ((unsigned long) (OBJECT_ADDRESS (ARG_REF (1)))));
}

DEFINE_PRIMITIVE ("MAKE-NON-POINTER-OBJECT", Prim_make_non_ptr_object, 1, 1,
  "(NUMBER)\n\
Convert the unsigned integer NUMBER into a fixnum.\n\
Assert: (= (OBJECT-DATUM (MAKE-NON-POINTER-OBJECT X)) X).")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM
     (arg_index_integer (1, (((unsigned long) 1) << DATUM_LENGTH))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-SET-TYPE", Prim_prim_obj_set_type, 2, 2,
  "Return a new object made from TYPE-CODE and the datum part of OBJECT.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (OBJECT_NEW_TYPE
     ((arg_index_integer (1, (MAX_TYPE_CODE + 1))), (ARG_REF (2))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-EQ?", Prim_prim_obj_eq_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((ARG_REF (1)) == (ARG_REF (2))));
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
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_REF (1)), (arg_nonnegative_integer (2))));
}

/* (PRIMITIVE-OBJECT-SET! OBJECT INDEX VALUE)
   Stores value in the index'ed slot in object.
   Performs no type checking on object.  */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-SET!", Prim_prim_obj_set, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  MEMORY_SET ((ARG_REF (1)), (arg_nonnegative_integer (2)), (ARG_REF (3)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Safe versions of the object manipulators.
   These touch their arguments, and provide GC safety tests.  */

DEFINE_PRIMITIVE ("OBJECT-TYPE", Prim_object_type, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (OBJECT_TYPE (object)));
}

DEFINE_PRIMITIVE ("OBJECT-GC-TYPE", Prim_object_gc_type, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (LONG_TO_FIXNUM (GC_Type (object)));
}

DEFINE_PRIMITIVE ("TYPE->GC-TYPE", Prim_type_to_gc_type, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (LONG_TO_FIXNUM
     (GC_Type_Map [arg_ulong_index_integer (1, (MAX_TYPE_CODE + 1))]));
}

DEFINE_PRIMITIVE ("OBJECT-TYPE?", Prim_object_type_p, 2, 2, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (2);
  TOUCH_IN_PRIMITIVE ((ARG_REF (2)), object);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT
     (((long) (OBJECT_TYPE (object)))
      == (arg_index_integer (1, (MAX_TYPE_CODE + 1)))));
}

DEFINE_PRIMITIVE ("OBJECT-DATUM", Prim_object_datum, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (long_to_integer (OBJECT_DATUM (object)));
}

DEFINE_PRIMITIVE ("OBJECT-SET-TYPE", Prim_object_set_type, 2, 2, 0)
{
  fast long type_code;
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (2);
  type_code = (arg_index_integer (1, (MAX_TYPE_CODE + 1)));
  TOUCH_IN_PRIMITIVE ((ARG_REF (2)), object);
  {
    fast long gc_type_code;

    gc_type_code = (GC_Type_Map [type_code]);
    if ((gc_type_code == GC_Undefined) ||
	(! ((gc_type_code == GC_Non_Pointer) ||
	    (gc_type_code == (GC_Type (object))))))
      error_bad_range_arg (1);
  }
  PRIMITIVE_RETURN (OBJECT_NEW_TYPE (type_code, object));
}

/* (EQ? OBJECT-1 OBJECT-2)
   Returns #T if the two objects have the same type code and datum.
   Returns #F otherwise.
   Touches both arguments.  */

DEFINE_PRIMITIVE ("EQ?", Prim_eq, 2, 2, 0)
{
  fast SCHEME_OBJECT object_1;
  fast SCHEME_OBJECT object_2;
  PRIMITIVE_HEADER (2);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object_1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (2)), object_2);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (object_1 == object_2));
}

/* (NOT OBJECT)
   Returns #T if OBJECT is #F.  Otherwise returns #F.  This is
   the primitive known as NOT and FALSE? in Scheme.
   Touches the argument.  */

DEFINE_PRIMITIVE ("NOT", Prim_not, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (object == SHARP_F));
}

/* (NULL? OBJECT)
   Returns #T if OBJECT is '().  Otherwise returns #F.
   Touches the argument.  */

DEFINE_PRIMITIVE ("NULL?", Prim_null_p, 1, 1, 0)
{
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), object);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (EMPTY_LIST_P (object)));
}

/* Cells */

/* (MAKE-CELL CONTENTS)
   Creates a cell with contents CONTENTS. */

DEFINE_PRIMITIVE ("MAKE-CELL", Prim_make_cell, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  Primitive_GC_If_Needed (1);
  (*Free++) = (ARG_REF (1));
  PRIMITIVE_RETURN (MAKE_POINTER_OBJECT (TC_CELL, (Free - 1)));
}

/* (CELL? OBJECT)
   Returns #T if OBJECT is a cell, else #F.  */

DEFINE_PRIMITIVE ("CELL?", Prim_cell_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (CELL_P (ARG_REF (1))));
}

/* (CELL-CONTENTS CELL)
   Returns the contents of the cell CELL.  */

DEFINE_PRIMITIVE ("CELL-CONTENTS", Prim_cell_contents, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (MEMORY_REF ((CELL_ARG (1)), CELL_CONTENTS));
}

/* (SET-CELL-CONTENTS! CELL OBJECT)
   Stores OBJECT as contents of CELL.
   Returns the previous contents of CELL. */

DEFINE_PRIMITIVE ("SET-CELL-CONTENTS!", Prim_set_cell_contents, 2, 2, 0)
{
  fast SCHEME_OBJECT cell;
  fast SCHEME_OBJECT object;
  PRIMITIVE_HEADER (2);
  cell = (CELL_ARG (1));
  object = (ARG_REF (2));
  SIDE_EFFECT_IMPURIFY (cell, object);
  MEMORY_SET (cell, CELL_CONTENTS, object);
  PRIMITIVE_RETURN (UNSPECIFIC);
}
