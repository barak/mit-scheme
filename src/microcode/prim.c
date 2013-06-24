/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* The leftovers ... primitives that don't seem to belong elsewhere. */

#include "scheme.h"
#include "prims.h"

static unsigned long
arg_type (int arg)
{
  return (arg_ulong_index_integer (arg, (1L << TYPE_CODE_LENGTH)));
}

static unsigned long
arg_datum (int arg)
{
  return (arg_ulong_index_integer (arg, (1L << DATUM_LENGTH)));
}

/* Low level object manipulation */

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-TYPE", Prim_prim_obj_type, 1, 1,
  "Return the type code of OBJECT as an unsigned integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ULONG_TO_FIXNUM (OBJECT_TYPE (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-TYPE?", Prim_prim_obj_type_p, 2, 2,
  "Return #T if TYPE-CODE is OBJECT's type code, else #F.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT ((OBJECT_TYPE (ARG_REF (2))) == (arg_type (1))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-DATUM", Prim_prim_obj_datum, 1, 1,
  "Return the datum part of OBJECT as an unsigned integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (OBJECT_DATUM (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-ADDRESS", Prim_prim_obj_address, 1, 1,
  "(OBJECT)\n\
Return the address part of OBJECT as an unsigned integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer ((unsigned long) (OBJECT_ADDRESS (ARG_REF (1)))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-DATUM->ADDRESS", Prim_prim_datum_to_addr, 1, 1,
  "(DATUM)\n\
Return the memory address corresponding to DATUM.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer ((unsigned long) (DATUM_TO_ADDRESS (arg_datum (1)))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-ADDRESS->DATUM", Prim_prim_addr_to_datum, 1, 1,
  "(ADDRESS)\n\
Return the object datum corresponding to ADDRESS.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer
     (ADDRESS_TO_DATUM ((SCHEME_OBJECT *) (arg_ulong_integer (1)))));
}

DEFINE_PRIMITIVE ("MAKE-NON-POINTER-OBJECT", Prim_make_non_ptr_object, 1, 1,
  "(DATUM)\n\
Convert the unsigned integer DATUM into a fixnum.\n\
Assert: (= (OBJECT-DATUM (MAKE-NON-POINTER-OBJECT X)) X).")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ULONG_TO_FIXNUM (arg_datum (1)));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-SET-TYPE", Prim_prim_obj_set_type, 2, 2,
  "(TYPE OBJECT)\n\
Return a new object made from TYPE and the datum part of OBJECT.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (OBJECT_NEW_TYPE ((arg_type (1)), (ARG_REF (2))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT-EQ?", Prim_prim_obj_eq_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((ARG_REF (1)) == (ARG_REF (2))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-MAKE-OBJECT", Prim_prim_make_obj, 2, 2,
  "(TYPE DATUM)\n\
Return a new object made from TYPE and DATUM.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (MAKE_OBJECT ((arg_type (1)), (arg_datum (2))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-OBJECT->INTEGER", Prim_prim_obj_to_integer, 1, 1,
  "(OBJECT)\n\
Return the integer representation of OBJECT.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("PRIMITIVE-INTEGER->OBJECT", Prim_prim_integer_to_obj, 1, 1,
  "(INTEGER)\n\
Return the object whose representation is INTEGER.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (arg_ulong_integer (1));
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

/* Safe versions of the object manipulators.  */

DEFINE_PRIMITIVE ("OBJECT-TYPE", Prim_object_type, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ULONG_TO_FIXNUM (OBJECT_TYPE (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("OBJECT-GC-TYPE", Prim_object_gc_type, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (LONG_TO_FIXNUM (GC_TYPE_TO_INT (GC_TYPE (ARG_REF (1)))));
}

DEFINE_PRIMITIVE ("TYPE->GC-TYPE", Prim_type_to_gc_type, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (LONG_TO_FIXNUM (GC_TYPE_TO_INT (GC_TYPE_CODE (arg_type (1)))));
}

DEFINE_PRIMITIVE ("OBJECT-TYPE?", Prim_object_type_p, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT ((OBJECT_TYPE (ARG_REF (2))) == (arg_type (1))));
}

DEFINE_PRIMITIVE ("OBJECT-DATUM", Prim_object_datum, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (OBJECT_DATUM (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("OBJECT-SET-TYPE", Prim_object_set_type, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    unsigned long type_code = (arg_type (1));
    SCHEME_OBJECT object = (ARG_REF (2));
    gc_type_t gc_type = (GC_TYPE_CODE (type_code));
    if ((gc_type == GC_UNDEFINED)
	|| ((gc_type != GC_NON_POINTER)
	    && (gc_type != (GC_TYPE (object)))))
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (OBJECT_NEW_TYPE (type_code, object));
  }
}

/* (EQ? OBJECT-1 OBJECT-2)
   Returns #T if the two objects have the same type code and datum.
   Returns #F otherwise.
   Touches both arguments.  */

DEFINE_PRIMITIVE ("EQ?", Prim_eq, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((ARG_REF (1)) == (ARG_REF (2))));
}

/* (NOT OBJECT)
   Returns #T if OBJECT is #F.  Otherwise returns #F.  This is
   the primitive known as NOT and FALSE? in Scheme.
   Touches the argument.  */

DEFINE_PRIMITIVE ("NOT", Prim_not, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((ARG_REF (1)) == SHARP_F));
}

/* (NULL? OBJECT)
   Returns #T if OBJECT is '().  Otherwise returns #F.
   Touches the argument.  */

DEFINE_PRIMITIVE ("NULL?", Prim_null_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (EMPTY_LIST_P (ARG_REF (1))));
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
  SCHEME_OBJECT cell;
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (2);
  cell = (CELL_ARG (1));
  object = (ARG_REF (2));
  MEMORY_SET (cell, CELL_CONTENTS, object);
  PRIMITIVE_RETURN (UNSPECIFIC);
}
