/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

/* Format of the SCode representation of programs.  Each of these is
   described in terms of the slots in the data structure. */

#ifndef SCM_SCODE_H
#define SCM_SCODE_H 1

#include "object.h"
#include "sdata.h"

#define access_env memory_ref_0
#define access_name memory_ref_1

#define assignment_name memory_ref_0
#define assignment_value memory_ref_1

#define combination_size vector_length
#define combination_expr vector_ref

#define comment_expression memory_ref_0
#define comment_text memory_ref_1

#define conditional_predicate memory_ref_0
#define conditional_consequent memory_ref_1
#define conditional_alternative memory_ref_2

#define definition_name memory_ref_0
#define definition_value memory_ref_1

#define delay_object memory_ref_0

#define disjunction_predicate memory_ref_0
#define disjunction_alternative memory_ref_1

#define lambda_body memory_ref_0
#define lambda_names memory_ref_1

static inline SCHEME_OBJECT*
lambda_params (SCHEME_OBJECT exp)
{
  return vector_loc (lambda_names (exp), 1);
}

static inline unsigned long
lambda_n_params (SCHEME_OBJECT exp)
{
  return vector_length (lambda_names (exp)) - 1;
}

#define elambda_body memory_ref_0
#define elambda_names memory_ref_1
#define elambda_arg_counts memory_ref_2

static inline unsigned long
elambda_reqs (SCHEME_OBJECT exp)
{
  return (elambda_arg_counts (exp) >> 8) & 0xFF;
}

static inline unsigned long
elambda_opts (SCHEME_OBJECT exp)
{
  return elambda_arg_counts (exp) & 0xFF;
}

static inline unsigned long
elambda_rest (SCHEME_OBJECT exp)
{
  return (elambda_arg_counts (exp) >> 16) & 0x1;
}

#define scode_quote_object memory_ref_0

#define sequence_1 memory_ref_0
#define sequence_2 memory_ref_1

#define variable_name memory_ref_0

static inline bool
variable_safe_p (SCHEME_OBJECT exp)
{
  return memory_ref_1 (exp) == SHARP_F;
}

#endif /* not SCM_SCODE_H */
