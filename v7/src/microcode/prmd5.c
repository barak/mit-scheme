/* -*-C-*-

$Id: prmd5.c,v 1.2 1997/06/09 21:17:11 cph Exp $

Copyright (c) 1997 Massachusetts Institute of Technology

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

/* Interface to MD5 library */

#include "scheme.h"
#include "prims.h"
#include <md5.h>

DEFINE_PRIMITIVE ("MD5", Prim_md5, 1, 1,
  "(STRING)\n\
Generate an MD5 digest of string.\n\
The digest is returned as a 16-byte string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    SCHEME_OBJECT result = (allocate_string (16));
    unsigned char * scan_result = (STRING_LOC (result, 0));
    MD5_CTX context;
    unsigned char * scan_digest = (context . digest);
    unsigned char * end_digest = (scan_digest + 16);
    MD5Init (&context);
    MD5Update ((&context), (STRING_LOC (string, 0)), (STRING_LENGTH (string)));
    MD5Final (&context);
    while (scan_digest < end_digest)
      (*scan_result++) = (*scan_digest++);
    PRIMITIVE_RETURN (result);
  }
}
