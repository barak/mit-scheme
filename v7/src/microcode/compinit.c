/* -*-C-*-

$Id: compinit.c,v 1.2 1993/06/09 20:36:38 jawilson Exp $

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

#define DECLARE_COMPILED_CODE(name, decl, code) do			\
{									\
  extern void EXFUN (decl, (void));					\
  extern SCHEME_OBJECT * EXFUN (code, (SCHEME_OBJECT *));		\
  if ((declare_compiled_code (name, decl, code)) == 0)			\
    lose_big_1 ("DECLARE_COMPILED_CODE: duplicate tag", name);		\
} while (0)

extern void EXFUN (lose_big_1, (char *, char *));

void
DEFUN_VOID (initialize_compiled_code_blocks)
{
#include "compinit.h"
  return;
}
