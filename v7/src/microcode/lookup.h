/* -*-C-*-

$Id: lookup.h,v 9.54 2001/08/02 04:30:12 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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
extern long link_variable
  (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT);
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

extern SCHEME_OBJECT compiler_var_error (SCHEME_OBJECT);

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
