/* -*-C-*-

$Id: usrdef.h,v 9.47 2007/01/05 15:33:08 cph Exp $

Copyright (c) 1987-2001 Massachusetts Institute of Technology

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

/* Macros and header for usrdef.c and variants. */

#ifndef SCM_USRDEF_H
#define SCM_USRDEF_H

#include "scheme.h"
#include "prims.h"

extern SCHEME_OBJECT EXFUN ((* (Static_Primitive_Procedure_Table[])), (void));
extern int Static_Primitive_Arity_Table[];
extern int Static_Primitive_Count_Table[];
extern CONST char * Static_Primitive_Name_Table[];
extern CONST char * Static_Primitive_Documentation_Table[];
extern long MAX_STATIC_PRIMITIVE;

extern void
  EXFUN (Microcode_Termination, (int)),
  EXFUN (signal_error_from_primitive, (long));

#endif /* SCM_USRDEF_H */
