/* Emacs, -*-C-*-an't you guess? */

/****************************************************************
*                                                               *
*                         Copyright (c) 1986                    *
*               Massachusetts Institute of Technology           *
*                                                               *
* This material was developed by the Scheme project at the      *
* Massachusetts Institute of Technology, Department of          *
* Electrical Engineering and Computer Science.  Permission to   *
* copy this software, to redistribute it, and to use it for any *
* purpose is granted, subject to the following restrictions and *
* understandings.                                               *
*                                                               *
* 1. Any copy made of this software must include this copyright *
* notice in full.                                               *
*                                                               *
* 2. Users of this software agree to make their best efforts (a)*
* to return to the MIT Scheme project any improvements or       *
* extensions that they make, so that these may be included in   *
* future releases; and (b) to inform MIT of noteworthy uses of  *
* this software.                                                *
*                                                               *
* 3.  All materials developed as a consequence of the use of    *
* this software shall duly acknowledge such use, in accordance  *
* with the usual standards of acknowledging credit in academic  *
* research.                                                     *
*                                                               *
* 4. MIT has made no warrantee or representation that the       *
* operation of this software will be error-free, and MIT is     *
* under no obligation to provide any services, by way of        *
* maintenance, update, or otherwise.                            *
*                                                               *
* 5.  In conjunction with products arising from the use of this *
* material, there shall be no use of the name of the            *
* Massachusetts Institute of Technology nor of any adaptation   *
* thereof in any advertising, promotional, or sales literature  *
* without prior written consent from MIT in each case.          *
*                                                               *
****************************************************************/

/* File: ERRORS.H
 *
 * Error and termination code declarations.  This must correspond
 * to UTABMD.SCM
 *
 */

/* All error and termination codes must be positive
 * to allow primitives to return either an error code
 * or a primitive flow control value (see CONST.H)
 */

#define ERR_BAD_ERROR_CODE			0x00
#define ERR_UNBOUND_VARIABLE			0x01
#define ERR_UNASSIGNED_VARIABLE			0x02
#define ERR_INAPPLICABLE_OBJECT			0x03
#define ERR_OUT_OF_HASH_NUMBERS			0x04 /* Not generated */
/* #define ERR_ENVIRONMENT_CHAIN_TOO_DEEP	0x05 */
#define ERR_BAD_FRAME				0x06
#define ERR_BROKEN_COMPILED_VARIABLE		0x07
#define ERR_UNDEFINED_USER_TYPE			0x08
#define ERR_UNDEFINED_PRIMITIVE			0x09
#define ERR_EXTERNAL_RETURN			0x0A
#define ERR_EXECUTE_MANIFEST_VECTOR		0x0B
#define ERR_WRONG_NUMBER_OF_ARGUMENTS		0x0C
#define ERR_ARG_1_WRONG_TYPE			0x0D
#define ERR_ARG_2_WRONG_TYPE			0x0E
#define ERR_ARG_3_WRONG_TYPE			0x0F
#define ERR_ARG_1_BAD_RANGE			0x10
#define ERR_ARG_2_BAD_RANGE			0x11
#define ERR_ARG_3_BAD_RANGE			0x12
/* #define ERR_BAD_COMBINATION			0x13 */
/* #define ERR_FASDUMP_OVERFLOW			0x14 */
#define ERR_BAD_INTERRUPT_CODE			0x15 /* Not generated */
/* #define ERR_NO_ERRORS			0x16 */
#define ERR_FASL_FILE_TOO_BIG			0x17
#define ERR_FASL_FILE_BAD_DATA			0x18
#define ERR_IMPURIFY_OUT_OF_SPACE		0x19

/* The following do not exist in the 68000 version */
#define ERR_WRITE_INTO_PURE_SPACE		0x1A
/* #define ERR_LOSING_SPARE_HEAP		0x1B */
#define ERR_NO_HASH_TABLE			0x1C
#define ERR_BAD_SET                             0x1D
#define ERR_ARG_1_FAILED_COERCION      		0x1E
#define ERR_ARG_2_FAILED_COERCION      		0x1F
#define ERR_OUT_OF_FILE_HANDLES			0x20
/* #define ERR_SHELL_DIED			0x21 */

/* Late additions to both 68000 and C world */
#define ERR_ARG_4_BAD_RANGE			0x22
#define ERR_ARG_5_BAD_RANGE			0x23
#define ERR_ARG_6_BAD_RANGE			0x24
#define ERR_ARG_7_BAD_RANGE			0x25
#define ERR_ARG_8_BAD_RANGE			0x26
#define ERR_ARG_9_BAD_RANGE			0x27
#define ERR_ARG_10_BAD_RANGE			0x28
#define ERR_ARG_4_WRONG_TYPE			0x29
#define ERR_ARG_5_WRONG_TYPE			0x2A
#define ERR_ARG_6_WRONG_TYPE			0x2B
#define ERR_ARG_7_WRONG_TYPE			0x2C
#define ERR_ARG_8_WRONG_TYPE			0x2D
#define ERR_ARG_9_WRONG_TYPE			0x2E
#define ERR_ARG_10_WRONG_TYPE			0x2F
#define ERR_INAPPLICABLE_CONTINUATION		0x30
#define ERR_COMPILED_CODE_ERROR			0x31
#define ERR_FLOATING_OVERFLOW			0x32

#define MAX_ERROR				0x32

/* Termination codes: the interpreter halts on these */

#define TERM_HALT				0x00
#define TERM_DISK_RESTORE			0x01
#define TERM_BROKEN_HEART			0x02
#define TERM_NON_POINTER_RELOCATION		0x03
#define TERM_BAD_ROOT				0x04
#define TERM_NON_EXISTENT_CONTINUATION		0x05
#define TERM_BAD_STACK				0x06
#define TERM_STACK_OVERFLOW			0x07
#define TERM_STACK_ALLOCATION_FAILED		0x08
#define TERM_NO_ERROR_HANDLER			0x09
#define TERM_NO_INTERRUPT_HANDLER		0x0A
#define TERM_UNIMPLEMENTED_CONTINUATION		0x0B
#define TERM_EXIT				0x0C
#define TERM_BAD_PRIMITIVE_DURING_ERROR		0x0D
#define TERM_EOF				0x0E
#define TERM_BAD_PRIMITIVE			0x0F
#define TERM_TERM_HANDLER			0x10
#define TERM_END_OF_COMPUTATION			0x11
#define TERM_INVALID_TYPE_CODE                  0x12
#define TERM_COMPILER_DEATH			0x13
#define TERM_GC_OUT_OF_SPACE			0x14
#define TERM_NO_SPACE				0x15
