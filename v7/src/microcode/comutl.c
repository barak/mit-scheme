/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/comutl.c,v 1.2 1987/06/05 16:25:22 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* Compiled Code Utilities */

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"

#define COMPILED_CODE_ADDRESS_P(object)					\
  (((OBJECT_TYPE (object)) == TC_COMPILED_EXPRESSION) ||		\
   ((OBJECT_TYPE (object)) == TC_RETURN_ADDRESS))

Built_In_Primitive (Prim_compiled_code_address_block, 1,
		    "COMPILED-CODE-ADDRESS-BLOCK", 0xB5)
{
  Pointer *address;
  Primitive_1_Arg ();

#ifdef CMPGCFILE
  CHECK_ARG (1, COMPILED_CODE_ADDRESS_P);
  address = (Get_Pointer (Arg1));
  return (Make_Pointer (TC_VECTOR, (Get_Compiled_Block (address))));
#else /* not CMPGCFILE */
  error_external_error ();
#endif /* CMPGCFILE */
}
