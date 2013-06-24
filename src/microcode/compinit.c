/* -*-C-*-

$Id: compinit.c,v 1.6 2003/02/14 18:28:18 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

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
