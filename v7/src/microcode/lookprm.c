/* -*-C-*-

$Id: lookprm.c,v 1.21 2003/02/14 18:28:20 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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
		  "(ENVIRONMENT SYMBOL)\n\
Returns the value of the variable in ENVIRONMENT named SYMBOL.\n\
\n\
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
		  "(REFERENCE ENVIRONMENT SYMBOL)\n\
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
		  "(ASSIGNMENT ENVIRONMENT SYMBOL VALUE)\n\
Sets the value of the variable in ENVIRONMENT named SYMBOL to VALUE.\n\
Returns the previous value.\n\
\n\
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
		  "(ENVIRONMENT SYMBOL VALUE)\n\
    [Should be called LEXICAL-DEFINE.]\n\
\n\
If the variable specified by SYMBOL already exists in the\n\
lexical ENVIRONMENT, then its value there is changed to VALUE.\n\
Otherwise a new binding is created in that environment linking\n\
the specified variable to the value.  Returns SYMBOL.\n\
\n\
Indistinguishable from evaluating (define SYMBOL VALUE) in ENVIRONMENT.")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  STD_LOOKUP (define_variable ((ARG_REF (1)), (ARG_REF (2)), (ARG_REF (3))));
  PRIMITIVE_RETURN (ARG_REF (2));
}

DEFINE_PRIMITIVE ("LEXICAL-REFERENCE-TYPE", Prim_lexical_reference_type, 2, 2,
		  "(ENVIRONMENT SYMBOL)\n\
Returns a index integer indicating the type of object stored in the\n\
binding of SYMBOL within ENVIRONMENT.  The following values are defined:\n\
\n\
0 means unbound\n\
1 means unassigned\n\
2 means a normal binding\n\
3 means a macro binding")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  {
    SCHEME_OBJECT value;
    long result = (lookup_variable ((ARG_REF (1)), (ARG_REF (2)), (&value)));
    switch (result)
      {
      case ERR_UNBOUND_VARIABLE:
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
      case ERR_UNASSIGNED_VARIABLE:
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
      case PRIM_DONE:
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (2));
      case ERR_MACRO_BINDING:
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (3));
      case PRIM_INTERRUPT:
	signal_interrupt_from_primitive ();
	PRIMITIVE_RETURN (UNSPECIFIC);
      default:
	signal_error_from_primitive (result);
	PRIMITIVE_RETURN (UNSPECIFIC);
      }
  }
}

DEFINE_PRIMITIVE ("SAFE-LEXICAL-REFERENCE", Prim_safe_lexical_reference, 2, 2,
		  "(ENVIRONMENT SYMBOL)\n\
Looks up SYMBOL in ENVIRONMENT and returns its value.\n\
If the variable is unbound, signals an error.\n\
If the variable is unassigned or holds a macro transformer,\n\
 returns the appropriate trap object.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, ENVIRONMENT_P);
  CHECK_ARG (2, SYMBOL_P);
  {
    SCHEME_OBJECT value;
    long result = (lookup_variable ((ARG_REF (1)), (ARG_REF (2)), (&value)));
    switch (result)
      {
      case PRIM_DONE:
      case ERR_MACRO_BINDING:
	PRIMITIVE_RETURN (value);
      case ERR_UNASSIGNED_VARIABLE:
	PRIMITIVE_RETURN (UNASSIGNED_OBJECT);
      case PRIM_INTERRUPT:
	signal_interrupt_from_primitive ();
	PRIMITIVE_RETURN (UNSPECIFIC);
      default:
	signal_error_from_primitive (result);
	PRIMITIVE_RETURN (UNSPECIFIC);
      }
  }
}

DEFINE_PRIMITIVE ("LEXICAL-UNASSIGNED?", Prim_unassigned_test, 2, 2,
		  "(ENVIRONMENT SYMBOL)\n\
Returns #T if the variable corresponding to SYMBOL is bound\n\
but has the special UNASSIGNED value in ENVIRONMENT.  Returns\n\
#F otherwise.  Does a complete lexical search for SYMBOL\n\
starting in ENVIRONMENT.\n\
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
		  "(ENVIRONMENT SYMBOL)\n\
Returns #T if the variable corresponding to SYMBOL has no binding in\n\
ENVIRONMENT.  Returns #F otherwise.  Does a complete lexical search\n\
for SYMBOL starting in ENVIRONMENT.")
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
		  "(ENVIRONMENT SYMBOL)\n\
Returns #T if looking up SYMBOL in ENVIRONMENT would cause an error.\n\
Returns #F otherwise.")
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
		  "(ENV1 ENV2 SYMBOL)\n\
SYMBOL must be bound in ENV2.  Creates a new binding for SYMBOL in ENV1,\n\
such that the bindings in ENV1 and ENV2 share the same value cell.\n\
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
		  "(TARGET-ENV TARGET-NAME SOURCE-ENV SOURCE-NAME)\n\
Define a new binding for TARGET-NAME in TARGET-ENV, which shares its\n\
value cell with the binding for SOURCE-NAME in SOURCE-ENV.\n\
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
		  "(ENVIRONMENT SYMBOL)\n\
Unbind the variable SYMBOL in ENVIRONMENT.\n\
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
