/* -*-C-*-

$Id: hunk.c,v 9.30 2002/11/20 19:46:09 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* Support for Hunk3s (triples) */

#include "scheme.h"
#include "prims.h"

SCHEME_OBJECT
DEFUN (hunk3_cons,
       (cxr0, cxr1, cxr2),
       SCHEME_OBJECT cxr0
       AND SCHEME_OBJECT cxr1
       AND SCHEME_OBJECT cxr2)
{
  Primitive_GC_If_Needed (3);
  (*Free++) = cxr0;
  (*Free++) = cxr1;
  (*Free++) = cxr2;
  return (MAKE_POINTER_OBJECT (TC_HUNK3, (Free - 3)));
}

DEFINE_PRIMITIVE ("HUNK3-CONS", Prim_hunk3_cons, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN (hunk3_cons ((ARG_REF (1)), (ARG_REF (2)), (ARG_REF (3))));
}

DEFINE_PRIMITIVE ("HUNK3-CXR", Prim_hunk3_cxr, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, HUNK3_P);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_REF (1)), (arg_index_integer (2, 3))));
}

DEFINE_PRIMITIVE ("HUNK3-SET-CXR!", Prim_hunk3_set_cxr, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, HUNK3_P);
  {
    fast SCHEME_OBJECT hunk3 = (ARG_REF (1));
    fast long index = (arg_index_integer (2, 3));
    fast SCHEME_OBJECT object = (ARG_REF (3));
    SIDE_EFFECT_IMPURIFY (hunk3, object);
    MEMORY_SET (hunk3, index, object);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define ARG_GC_TRIPLE(arg_number)					\
  (((GC_Type (ARG_REF (arg_number))) == GC_Triple)			\
   ? (ARG_REF (arg_number))						\
   : ((error_wrong_type_arg (arg_number)), ((SCHEME_OBJECT) 0)))

DEFINE_PRIMITIVE ("SYSTEM-HUNK3-CXR0", Prim_sys_h3_0, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_GC_TRIPLE (1)), 0));
}

DEFINE_PRIMITIVE ("SYSTEM-HUNK3-CXR1", Prim_sys_h3_1, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_GC_TRIPLE (1)), 1));
}

DEFINE_PRIMITIVE ("SYSTEM-HUNK3-CXR2", Prim_sys_h3_2, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (MEMORY_REF ((ARG_GC_TRIPLE (1)), 2));
}

DEFINE_PRIMITIVE ("SYSTEM-HUNK3-SET-CXR0!", Prim_sh3_set_0, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT hunk3 = (ARG_GC_TRIPLE (1));
    SCHEME_OBJECT object = (ARG_REF (2));
    SIDE_EFFECT_IMPURIFY (hunk3, object);
    MEMORY_SET (hunk3, 0, object);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SYSTEM-HUNK3-SET-CXR1!", Prim_sh3_set_1, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT hunk3 = (ARG_GC_TRIPLE (1));
    SCHEME_OBJECT object = (ARG_REF (2));
    SIDE_EFFECT_IMPURIFY (hunk3, object);
    MEMORY_SET (hunk3, 1, object);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SYSTEM-HUNK3-SET-CXR2!", Prim_sh3_set_2, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT hunk3 = (ARG_GC_TRIPLE (1));
    SCHEME_OBJECT object = (ARG_REF (2));
    SIDE_EFFECT_IMPURIFY (hunk3, object);
    MEMORY_SET (hunk3, 2, object);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

