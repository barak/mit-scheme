/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/scode.h,v 9.23 1988/08/15 20:54:55 cph Rel $
 *
 * Format of the SCode representation of programs.  Each of these
 * is described in terms of the slots in the data structure.
 *
 */

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

/* COMBINATIONS come in several formats */

/* General combinations are vector-like: */
#define COMB_VECTOR_HEADER	0
#define COMB_FN_SLOT		1
#define COMB_ARG_1_SLOT		2

/* Short non-primitive combinations: */
#define COMB_1_FN		0
#define COMB_1_ARG_1		1

#define COMB_2_FN		0
#define COMB_2_ARG_1		1
#define COMB_2_ARG_2		2

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

#define Get_Body_Elambda(Addr)  (Fast_Vector_Ref(Addr, ELAMBDA_SCODE))
#define Get_Names_Elambda(Addr) (Fast_Vector_Ref(Addr, ELAMBDA_NAMES))
#define Get_Count_Elambda(Addr) (Fast_Vector_Ref(Addr, ELAMBDA_ARG_COUNT))
#define Elambda_Formals_Count(Addr) \
     ((((long) Addr) & EL_FORMALS_MASK) >> EL_FORMALS_SHIFT)
#define Elambda_Opts_Count(Addr) \
     (((long) Addr) & EL_OPTS_MASK)
#define Elambda_Rest_Flag(Addr) \
     ((((long) Addr) & EL_REST_MASK) >> EL_REST_SHIFT)

/* IN-PACKAGE operation: */
#define IN_PACKAGE_ENVIRONMENT	0
#define IN_PACKAGE_EXPRESSION	1

/* LAMBDA operation:
 * Object representing a LAMBDA expression with a fixed number of
 * arguments.  It consists of a list of the names of the arguments
 * (the first is the name by which the procedure refers to itself) and
 * the SCode for the procedure.
 */

#define LAMBDA_SCODE		0
#define LAMBDA_FORMALS		1

/* LEXPR
 * Same as LAMBDA (q.v.) except additional arguments are permitted
 * beyond those indicated in the LAMBDA_FORMALS list.
 */

/* Primitive combinations with 0 arguments are not pointers */

/* Primitive combinations, 1 argument: */
#define PCOMB1_FN_SLOT		0
#define PCOMB1_ARG_SLOT		1

/* Primitive combinations, 2 arguments: */
#define PCOMB2_FN_SLOT		0
#define PCOMB2_ARG_1_SLOT	1
#define PCOMB2_ARG_2_SLOT	2

/* Primitive combinations, 3 arguments are vector-like: */
#define PCOMB3_FN_SLOT		1
#define PCOMB3_ARG_1_SLOT	2
#define PCOMB3_ARG_2_SLOT	3
#define PCOMB3_ARG_3_SLOT	4

/* SCODE_QUOTE returns itself */
#define SCODE_QUOTE_OBJECT	0
#define SCODE_QUOTE_IGNORED	1

/* SEQUENCE operations (two forms: SEQUENCE_2 and SEQUENCE_3) */
#define SEQUENCE_1		0
#define SEQUENCE_2		1
#define SEQUENCE_3		2

/* VARIABLE operation.
 * Corresponds to a variable lookup or variable reference. Contains the
 * symbol referenced, and (if it has been compiled) the frame and
 * offset in the frame in which it was found.  One of these cells is
 * multiplexed by having its type code indicate one of several modes
 * of reference: not yet compiled, local reference, formal reference,
 * auxiliary reference, or global value reference.
 * There are extra definitions in lookup.h.
 */
#define VARIABLE_SYMBOL		0
#define VARIABLE_FRAME_NO	1
#define VARIABLE_OFFSET		2
#define VARIABLE_COMPILED_TYPE	1
