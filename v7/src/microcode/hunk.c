/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/hunk.c,v 9.27 1992/01/15 02:27:57 jinx Exp $

Copyright (c) 1987-92 Massachusetts Institute of Technology

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

