/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

/* Macros and declarations for the variable lookup code. */

#ifndef SCM_LOOKUP_H
#define SCM_LOOKUP_H

#include "trap.h"

extern long lookup_variable
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);
extern long safe_lookup_variable
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);
extern long variable_unassigned_p
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);
extern long variable_unbound_p
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);
extern long variable_unreferenceable_p
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);
extern long assign_variable
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);
extern long define_variable
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT);
extern long link_variables
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT);
extern long unbind_variable
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);

extern trap_kind_t get_trap_kind (SCHEME_OBJECT);

extern long compiler_cache_lookup
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
extern long compiler_cache_assignment
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
extern long compiler_cache_operator
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);
extern long compiler_cache_global_operator
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned long);

extern SCHEME_OBJECT compiler_var_error
  (SCHEME_OBJECT, SCHEME_OBJECT, unsigned int);

extern long compiler_lookup_trap
  (SCHEME_OBJECT, SCHEME_OBJECT *);
extern long compiler_operator_reference_trap
  (SCHEME_OBJECT, SCHEME_OBJECT *);
extern long compiler_safe_lookup_trap
  (SCHEME_OBJECT, SCHEME_OBJECT *);
extern long compiler_unassigned_p_trap
  (SCHEME_OBJECT, SCHEME_OBJECT *);
extern long compiler_assignment_trap
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *);

#define UNCOMPILED_VARIABLE (MAKE_OBJECT (TC_CONSTANT, 0))

#endif /* not SCM_LOOKUP_H */
