/* -*-C-*-

$Id: lookprm.c,v 1.15 2001/08/07 01:26:22 cph Exp $

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

/* This file contains environment manipulation primitives.
   It makes heavy use of procedures in lookup.c */

#include "scheme.h"
#include "prims.h"
#include "locks.h"
#include "trap.h"
#include "lookup.h"

#define STD_LOOKUP(expression)						\
{									\
  long SL_result = (expression);					\
  if (SL_result != PRIM_DONE)						\
    {									\
      if (SL_result == PRIM_INTERRUPT)					\
	signal_interrupt_from_primitive ();				\
      else								\
	signal_error_from_primitive (SL_result);			\
    }									\
}

DEFINE_PRIMITIVE ("LEXICAL-REFERENCE", Prim_lexical_reference, 2, 2,
		  "(ENVIRONMENT SYMBOL)\n
Returns the value of the variable in ENVIRONMENT named SYMBOL.\n
\n
Indistinguishable from evaluating SYMBOL in ENVIRONMENT.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  {
    SCHEME_OBJECT value;
    STD_LOOKUP (lookup_variable ((ARG_REF (1)), (ARG_REF (2)), (&value)));
    PRIMITIVE_RETURN (value);
  }
}

DEFINE_PRIMITIVE ("LOCAL-REFERENCE", Prim_local_reference, 2, 2,
		  "(REFERENCE ENVIRONMENT SYMBOL)\n
Identical to LEXICAL_REFERENCE, here for hysterical reasons.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  {
    SCHEME_OBJECT value;
    STD_LOOKUP (lookup_variable ((ARG_REF (1)), (ARG_REF (2)), (&value)));
    PRIMITIVE_RETURN (value);
  }
}

DEFINE_PRIMITIVE ("LEXICAL-ASSIGNMENT", Prim_lexical_assignment, 3, 3,
		  "(ASSIGNMENT ENVIRONMENT SYMBOL VALUE)\n
Sets the value of the variable in ENVIRONMENT named SYMBOL to VALUE.\n
Returns the previous value.\n
\n
Indistinguishable from evaluating (set! SYMBOL VALUE) in ENVIRONMENT.")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  {
    SCHEME_OBJECT value;
    STD_LOOKUP
      (assign_variable ((ARG_REF (1)), (ARG_REF (2)), (ARG_REF (3)),
			(&value)));
    PRIMITIVE_RETURN (value);
  }
}

DEFINE_PRIMITIVE ("LOCAL-ASSIGNMENT", Prim_local_assignment, 3, 3,
		  "(ENVIRONMENT SYMBOL VALUE)\n
    [Should be called LEXICAL-DEFINE.]\n
\n
If the variable specified by SYMBOL already exists in the\n
lexical ENVIRONMENT, then its value there is changed to VALUE.\n
Otherwise a new binding is created in that environment linking\n
the specified variable to the value.  Returns SYMBOL.\n
\n
Indistinguishable from evaluating (define SYMBOL VALUE) in ENVIRONMENT.")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  STD_LOOKUP (define_variable ((ARG_REF (1)), (ARG_REF (2)), (ARG_REF (3))));
  PRIMITIVE_RETURN (ARG_REF (2));
}

DEFINE_PRIMITIVE ("LEXICAL-UNASSIGNED?", Prim_unassigned_test, 2, 2,
		  "(ENVIRONMENT SYMBOL)\n
Returns #T if the variable corresponding to SYMBOL is bound\n
but has the special UNASSIGNED value in ENVIRONMENT.  Returns\n
#F otherwise.  Does a complete lexical search for SYMBOL\n
starting in ENVIRONMENT.\n
The special form (unassigned? <symbol>) is built on top of this.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  {
    SCHEME_OBJECT value;
    STD_LOOKUP
      (variable_unassigned_p ((ARG_REF (1)), (ARG_REF (2)), (&value)));
    PRIMITIVE_RETURN (value);
  }
}

DEFINE_PRIMITIVE ("LEXICAL-UNBOUND?", Prim_unbound_test, 2, 2,
		  "(ENVIRONMENT SYMBOL)\n
Returns #T if the variable corresponding to SYMBOL has no binding in\n
ENVIRONMENT.  Returns #F otherwise.  Does a complete lexical search\n
for SYMBOL starting in ENVIRONMENT.  The special form (unbound?\n
<symbol>) is built on top of this.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  {
    SCHEME_OBJECT value;
    STD_LOOKUP (variable_unbound_p ((ARG_REF (1)), (ARG_REF (2)), (&value)));
    PRIMITIVE_RETURN (value);
  }
}

DEFINE_PRIMITIVE ("LEXICAL-UNREFERENCEABLE?", Prim_unreferenceable_test, 2, 2,
		  "(ENVIRONMENT SYMBOL)\n
Returns #T if evaluating SYMBOL in ENVIRONMENT would cause a\n
variable lookup error (unbound or unassigned).")
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT value;
    STD_LOOKUP
      (variable_unreferenceable_p ((ARG_REF (1)), (ARG_REF (2)), (&value)));
    PRIMITIVE_RETURN (value);
  }
}

DEFINE_PRIMITIVE ("ENVIRONMENT-LINK-NAME", Prim_environment_link_name, 3, 3,
		  "(ENV1 ENV2 SYMBOL)\n
SYMBOL must be bound in ENV2.  Creates a new binding for SYMBOL in ENV1,\n
such that the bindings in ENV1 and ENV2 share the same value cell.\n
If SYMBOL is already bound in ENV1, the existing binding is modified.")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, ENVIRONMENT_P);
  CHECK_ARG (3, SYMBOL_P);
  STD_LOOKUP
    (link_variables ((ARG_REF (1)), (ARG_REF (3)),
		     (ARG_REF (2)), (ARG_REF (3))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("LINK-VARIABLES", Prim_link_variables, 4, 4,
		  "(TARGET-ENV TARGET-NAME SOURCE-ENV SOURCE-NAME)\n
Define a new binding for TARGET-NAME in TARGET-ENV, which shares its\n
value cell with the binding for SOURCE-NAME in SOURCE-ENV.\n
SOURCE-NAME must be bound in SOURCE-ENV.")
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  CHECK_ARG (3, ENVIRONMENT_P);
  CHECK_ARG (4, SYMBOL_P);
  STD_LOOKUP
    (link_variables ((ARG_REF (1)), (ARG_REF (2)),
		     (ARG_REF (3)), (ARG_REF (4))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("UNBIND-VARIABLE", Prim_unbind_variable, 2, 2,
		  "(ENVIRONMENT SYMBOL)\n
Unbind the variable SYMBOL in ENVIRONMENT.\n
Returns #F if the variable was not previously bound, otherwise #T.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  {
    SCHEME_OBJECT value;
    STD_LOOKUP (unbind_variable ((ARG_REF (1)), (ARG_REF (2)), (&value)));
    PRIMITIVE_RETURN (value);
  }
}
