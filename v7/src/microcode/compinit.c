/* -*-C-*-

$Id: compinit.c,v 1.3 1993/10/30 03:02:11 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

#include "liarc.h"

#undef DECLARE_COMPILED_CODE
#undef DECLARE_COMPILED_DATA

#define DECLARE_COMPILED_CODE(name, nentries, decl_code, code) do	\
{									\
  extern int EXFUN (decl_code, (void));					\
  extern SCHEME_OBJECT * EXFUN (code,					\
				(SCHEME_OBJECT *, unsigned long));	\
  int result =								\
    (declare_compiled_code (name, nentries, decl_code, code));		\
  if (result != 0)							\
    return (result);							\
} while (0)

#define DECLARE_COMPILED_DATA(name, decl_data, data) do			\
{									\
  extern int EXFUN (decl_data, (void));					\
  extern SCHEME_OBJECT * EXFUN (data, (unsigned long));			\
  int result = (declare_compiled_data (name, decl_data, data));		\
  if (result != 0)							\
    return (result);							\
} while (0)

int
DEFUN_VOID (initialize_compiled_code_blocks)
{
#include "compinit.h"
  return (0);
}
