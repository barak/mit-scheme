/* -*-C-*-

$Id: usrdef.h,v 9.41 1993/08/28 05:43:00 gjr Exp $

Copyright (c) 1987-1993 Massachusetts Institute of Technology

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

/* Macros and header for usrdef.c and variants. */

#ifndef SCM_USRDEF_H
#define SCM_USRDEF_H

#include "ansidecl.h"
#include "config.h"
#include "object.h"
#include "errors.h"
#include "prim.h"
#include "prims.h"

extern SCHEME_OBJECT EXFUN ((* (Static_Primitive_Procedure_Table[])), (void));
extern int Static_Primitive_Arity_Table[];
extern int Static_Primitive_Count_Table[];
extern char * Static_Primitive_Name_Table[];
extern char * Static_Primitive_Documentation_Table[];
extern long MAX_STATIC_PRIMITIVE;

extern SCHEME_OBJECT
  EXFUN (declare_primitive, (char *, primitive_procedure_t, int, int, char *));

extern SCHEME_OBJECT
  EXFUN (install_primitive, (char *, primitive_procedure_t, int, int, char *));

extern void
  EXFUN (Microcode_Termination, (int)),
  EXFUN (signal_error_from_primitive, (long));

#endif /* SCM_USRDEF_H */
