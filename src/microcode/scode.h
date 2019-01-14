/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
#define SCM_SCODE_H

/* Here are the definitions of the the executable operations for the
   interpreter.  This file should parallel the file SCODE.SCM in the
   runtime system.  The interpreter dispatches on the type code of a
   pointer to determine what operation to perform.  The format of the
   storage block this points to is described below.  Offsets are the
   number of cells from the location pointed to by the operation. */

/* ALPHABETICALLY LISTED BY TYPE CODE NAME */

/* ACCESS operation: */
#define ACCESS_ENVIRONMENT	0
#define ACCESS_NAME		1

/* ASSIGNMENT operation: */
#define ASSIGN_NAME		0
#define ASSIGN_VALUE		1

/* COMBINATIONS are vector-like: */
#define COMB_VECTOR_HEADER	0
#define COMB_FN_SLOT		1
#define COMB_ARG_1_SLOT		2

/* COMMENT operation: */
#define COMMENT_EXPRESSION	0
#define COMMENT_TEXT		1

/* CONDITIONAL operation (used for COND, IF, AND): */
#define COND_PREDICATE		0
#define COND_CONSEQUENT		1
#define COND_ALTERNATIVE	2

/* DEFINITION operation: */
#define DEFINE_NAME		0
#define DEFINE_VALUE		1

/* DELAY operation: */
#define DELAY_OBJECT		0
#define DELAY_UNUSED		1

/* DISJUNCTION or OR operation: */
#define OR_PREDICATE		0
#define OR_ALTERNATIVE		1

/* EXTENDED_LAMBDA operation:
 * Support for optional parameters and auxiliary local variables.  The
 * Extended Lambda is similar to LAMBDA, except that it has an extra
 * word called the ARG_COUNT.  This contains an 8-bit count of the
 * number of optional arguments, an 8-bit count of the number of
 * required (formal) parameters, and a bit to indicate that additional
 * (rest) arguments are allowed.  The vector of argument names
 * contains, of course, a size count which allows the calculation of
 * the number of auxiliary variables required.  Auxiliary variables
 * are created for any internal DEFINEs which are found at syntax time
 * in the body of a LAMBDA-like special form.
 */

#define ELAMBDA_SCODE      0
#define ELAMBDA_NAMES      1
#define ELAMBDA_ARG_COUNT  2

/* Masks.  The infomation on the number of each type of argument is
 * separated at byte boundaries for easy extraction in the 68000 code.
 */

#define EL_OPTS_MASK		0xFF
#define EL_FORMALS_MASK		0xFF00
#define EL_REST_MASK		0x10000
#define EL_FORMALS_SHIFT	8
#define EL_REST_SHIFT		16

/* Selectors */

#define Get_Body_Elambda(Addr)  (MEMORY_REF (Addr, ELAMBDA_SCODE))
#define Get_Names_Elambda(Addr) (MEMORY_REF (Addr, ELAMBDA_NAMES))
#define Get_Count_Elambda(Addr) (MEMORY_REF (Addr, ELAMBDA_ARG_COUNT))
#define Elambda_Formals_Count(Addr) \
     ((((long) Addr) & EL_FORMALS_MASK) >> EL_FORMALS_SHIFT)
#define Elambda_Opts_Count(Addr) \
     (((long) Addr) & EL_OPTS_MASK)
#define Elambda_Rest_Flag(Addr) \
     ((((long) Addr) & EL_REST_MASK) >> EL_REST_SHIFT)

/* LAMBDA operation:
 * Object representing a LAMBDA expression with a fixed number of
 * arguments.  It consists of a list of the names of the arguments
 * (the first is the name by which the procedure refers to itself) and
 * the SCode for the procedure.
 */

#define LAMBDA_SCODE		0
#define LAMBDA_FORMALS		1

#define GET_LAMBDA_FORMALS(lambda)					\
  (MEMORY_REF ((lambda), LAMBDA_FORMALS))

#define GET_LAMBDA_PARAMETERS(lambda)					\
  (MEMORY_LOC ((GET_LAMBDA_FORMALS (lambda)), (VECTOR_DATA + 1)))

#define GET_LAMBDA_N_PARAMETERS(lambda)					\
  ((VECTOR_LENGTH (GET_LAMBDA_FORMALS (lambda))) - 1)

/* LEXPR
 * Same as LAMBDA (q.v.) except additional arguments are permitted
 * beyond those indicated in the LAMBDA_FORMALS list.
 */

/* SCODE_QUOTE returns itself */
#define SCODE_QUOTE_OBJECT	0
#define SCODE_QUOTE_IGNORED	1

/* SEQUENCE operations */
#define SEQUENCE_1		0
#define SEQUENCE_2		1

/* VARIABLE operation.
 * Corresponds to a variable lookup or variable reference. Contains the
 * symbol referenced
 */
#define VARIABLE_SYMBOL		0

#define GET_VARIABLE_SYMBOL(variable)					\
  (MEMORY_REF ((variable), VARIABLE_SYMBOL))

#endif /* not SCM_SCODE_H */
