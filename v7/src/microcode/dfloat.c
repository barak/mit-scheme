/* -*-C-*-

$Id: dfloat.c,v 1.5 1993/12/05 06:08:10 cph Exp $

Copyright (c) 1991-93 Massachusetts Institute of Technology

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

/* Floating-point vector primitives */

#include "scheme.h"
#include "prims.h"

#define FLOATING_VECTOR_INDEX_ARG(argument_number, vector)		\
  (arg_index_integer ((argument_number), (FLOATING_VECTOR_LENGTH (vector))))

DEFINE_PRIMITIVE ("FLOATING-VECTOR-CONS", Prim_floating_vector_cons, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    long length = (arg_nonnegative_integer (1));
    long length_in_words = (length * FLONUM_SIZE);
    SCHEME_OBJECT result;
    fast double *vect;

    ALIGN_FLOAT (Free);
    Primitive_GC_If_Needed (length_in_words + 1);
    result = (MAKE_POINTER_OBJECT (TC_BIG_FLONUM, Free));
    (*Free++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, length_in_words));
    vect = ((double *) Free);
    while ((length--) > 0) (*vect++) = 0.0;
    Free = ((SCHEME_OBJECT *) vect);
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("FLOATING-VECTOR-REF", Prim_floating_vector_ref, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT vector = (FLOATING_VECTOR_ARG (1));
    Primitive_GC_If_Needed (FLONUM_SIZE + 1);
    PRIMITIVE_RETURN
      (FLOAT_TO_FLONUM
       (FLOATING_VECTOR_REF (vector,
			     (FLOATING_VECTOR_INDEX_ARG (2, vector)))));
  }
}

extern double EXFUN (arg_flonum, (int));

DEFINE_PRIMITIVE ("FLOATING-VECTOR-SET!", Prim_floating_vector_set, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT vector = (FLOATING_VECTOR_ARG (1));
    FLOATING_VECTOR_SET
      (vector,
       (FLOATING_VECTOR_INDEX_ARG (2, vector)),
       (arg_flonum (3)));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FLOATING-VECTOR-LENGTH", Prim_floating_vector_length, 1, 1,
		  0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (LONG_TO_UNSIGNED_FIXNUM
     (FLOATING_VECTOR_LENGTH (FLOATING_VECTOR_ARG (1))));
}
