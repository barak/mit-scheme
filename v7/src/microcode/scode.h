/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/scode.h,v 9.21 1987/01/22 14:31:54 jinx Exp $
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

/* Non-primitive combinations are vector-like: */
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

/* COMPILED_CODE_ENTRY operation: */
#define CCE_BYTE_ADDRESS	0

/* CONDITIONAL operation (used for COND, IF, CONJUNCTION): */
#define COND_PREDICATE		0
#define COND_CONSEQUENT		1
#define COND_ALTERNATIVE	2

/* DEFINITION operation: */
#define DEFINE_NAME		0
#define DEFINE_VALUE		1

/* DELAY operation: */
#define DELAY_OBJECT		0
#define DELAY_UNUSED		1

/* DISJUNCTION operation (formerly OR): */
#define OR_PREDICATE		0
#define OR_ALTERNATIVE		1

/* IN-PACKAGE operation: */
#define IN_PACKAGE_ENVIRONMENT	0
#define IN_PACKAGE_EXPRESSION	1

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
