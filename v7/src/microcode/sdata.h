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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/sdata.h,v 9.21 1987/01/22 14:32:00 jinx Exp $
 *
 * Description of the user data objects.  This should parallel the
 * file SDATA.SCM in the runtime system.
 *
 */

/* Alphabetical order.  Every type of object is described either with a
   comment or with offsets describing locations of various parts. */

/* ADDRESS
 * is a FIXNUM.  It represents a 24-bit address.  Not a pointer type.
 */ 

/* BIG_FIXNUM (bignum).
 * See the file BIGNUM.C
 */

/* BIG_FLONUM (flonum).
 * Implementation dependent format (uses C data type "double").  Pointer
 * to implemetation defined floating point format.
 */

/* BROKEN_HEART.
 * "Forwarding address" used by garbage collector to indicate that an
 * object has been moved to a new location.  These should never be
 * encountered by the interpreter!
 */

/* CELL.
 * An object that points to one other object (extra indirection).
 * Used by the compiler to share objects.
 */
#define CELL_CONTENTS 		0

/* CHARACTER
 * Not currently used.  Intended ultimately to complete the abstraction
 * of strings.  This will probably be removed eventually.
 */

/* CHARACTER_STRING
 * Synonym for 8B_VECTOR.  Used to store strings of characters.  Format
 * consists of the normal non-marked vector header (STRING_HEADER)
 * followed by the number of characters in the string (as a FIXNUM),
 * followed by the characters themselves.
 */
#define STRING_HEADER		0
#define STRING_LENGTH		1
#define STRING_CHARS		2

/* COMPILED_PROCEDURE */
#define COMP_PROCEDURE_ADDRESS	0
#define COMP_PROCEDURE_ENV	1

/* CONTINUATION
 * Pushed on the control stack by the interpreter, each has two parts:
 * the return address within the interpreter (represented as a type
 * code RETURN_ADDRESS and address part RC_xxx), and an expression
 * which was being evaluated at that time (sometimes just used as
 * additional data needed at the return point).  The offsets given
 * here are with respect to the stack pointer as it is located
 * immediately after pushing a continuation (or, of course,
 * immediately before popping it back).
 *
 * HISTORY_SIZE is the size of a RESTORE_HISTORY (or
 * RESTORE_DONT_COPY_HISTORY) continuation.
 */

#define CONTINUATION_EXPRESSION    1
#define CONTINUATION_RETURN_CODE   0
#define CONTINUATION_SIZE          2
#define HISTORY_SIZE		   (CONTINUATION_SIZE + 2)

/* CONTROL_POINT
 * Points to a copy of the control stack at the time a control point is
 * created.  This is the saved state of the interpreter, and can be
 * restored later by APPLYing the control point to an argument (i.e. a
 * throw).  Format is that of an ordinary vector.  They are linked
 * together by using the return code RC_JOIN_STACKLETS.
 */

/* If USE_STACKLETS is defined, then a stack (i.e. control point) is
   actually made from smaller units allocated from the heap and linked
   together.  The format is:

		   0 memory address

             _______________________________________
             |MAN. VECT.| n                        |
           _ _______________________________________
         /   | NM VECT   | m  at GC or when full   |
        |    _______________________________________
        |    |               ...                   |\
        |    |     not yet in use -- garbage       | > m
     n <     _______________________________________/
        |    | Top of Stack, useful contents       | <---Stack_Pointer
        |    _______________________________________
        \    |               ...                   |
         \   |           useful stuff              |
          \_ ________________________________________
                                                     <---Stack_Top
		   infinite memory address

*/

#define STACKLET_LENGTH			0	/* = VECTOR_LENGTH */
#define STACKLET_HEADER_SIZE		2
#define STACKLET_UNUSED_LENGTH		1
#define STACKLET_FREE_LIST_LINK		1	/* If on free list */

/* DELAYED
 * The object returned by a DELAY operation.  Consists initially of a
 * procedure to be APPLYed and environment.  After the FORCE primitive
 * is applied to the object, the result is stored in the DELAYED object
 * and further FORCEs return this same result.  I.e. FORCE memoizes the
 * value of the DELAYED object.  For historical reasons, such an object
 * is called a 'thunk.'
 */
#define THUNK_SNAPPED		0
#define THUNK_VALUE		1
#define THUNK_ENVIRONMENT	0
#define THUNK_PROCEDURE		1

/* ENVIRONMENT
 * Associates identifiers with values.  The identifiers are either from
 * a lambda-binding (as in a procedure call) or a run-time DEFINE (known
 * as an 'auxilliary' binding).  The environment contains three things:
 * the list of identifiers which must be marked DANGEROUS if they are
 * created in this environment (the 'potentially dangerous' list); the
 * A-list associating auxilliary variables with their values; and the
 * values of lambda-bound variables.  The names of the lambda-bound
 * variables are found by looking at the PROCEDURE which is stored in
 * the first formal value slot.  This will contain a LEXPR or LAMBDA
 * object, which contains a list of names associated with the slots in
 * the environment. Notice that the FINGER used in the process of
 * constructing an environment frame is stored in the same place where
 * the potentially dangerous variables will eventually go.
 *
 * There are actually 3 data structures which are used at distinct
 * times to store an environment.  A HEAP_ENVIRONMENT is the format
 * used by the interpreter for a completely formed environment in
 * which variable lookups will occur.  A STACK_COMBINATION is the
 * structure built on the stack to evaluate normal (long)
 * combinations.  It contains a slot for the finger and the
 * combination whose operands are being evaluated.  Only some of the
 * argument slots in a stack-combination are meaningful: those which
 * have already been evaluated (those not "hidden" by the finger).
 * Finally, a STACK_ENVIRONMENT is the format used at Internal_Apply
 * just as an application is about to occur.  This does NOT have slots
 * for auxilliary variables or the potentially dangerous list, since
 * primitives, compiled code, and control points don't need these
 * slots.
 *
 * The "life cycle" of an environment is: (a) it is born on the stack
 * during the evaluation of a COMBINATION as a STACK_COMBINATION; (b)
 * when all of the operands and the operator have been evaluated and
 * stored into this frame, the finger is removed and the function
 * is stored ... the result is a STACK_ENVIRONMENT; (c) finally, if
 * the operator is an interpreted procedure, the frame has is copied
 * onto the heap as a HEAP_ENVIRONMENT (i.e. by adding the two missing
 * slots).  For the optimized combinations (COMBINATION-1, 2, 3 and
 * PCOMB0, 1, 2, 3), the STACK_ENVIRONMENT is used directly, without
 * ever creating a STACK_COMBINATION.
 */

/* ENVIRONMENT, continued */

#define HEAP_ENV_EXTRA_SLOTS	3
	/* Slots over and above those used to store 
	   function (procedure) and its arguments in
           a HEAP environment                        */

#define HEAP_ENV_HEADER		0
#define HEAP_ENV_AUX_SLOT	1
#define HEAP_ENV_P_DANGER	2
#define HEAP_ENV_FUNCTION	3
#define HEAP_ENV_FIRST_ARG	4

#define STACK_ENV_EXTRA_SLOTS   1
#define STACK_ENV_HEADER        0
#define STACK_ENV_FUNCTION      1
#define STACK_ENV_FIRST_ARG     2

#define STACK_COMB_FINGER       0
#define STACK_COMB_FIRST_ARG    1

/* An environment chain always ends in a pointer with type code
   of GLOBAL_ENV.  This will contain an address part which
   either indicates that the lookup should continue on to the
   true global environment, or terminate at this frame. */

#define GO_TO_GLOBAL	0
#define END_OF_CHAIN	1

/* EXTENDED_FIXNUM
 * Not used in the C version.  On the 68000 this is used for 24-bit
 * integers, while FIXNUM is used for 16-bit integers.
 */

/* EXTENDED_LAMBDA
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

/* EXTENDED_PROCEDURE
 * Counterpart to EXTENDED_LAMBDA.  Same format as PROCEDURE.
 */

/* FALSE
 * Alternate name for NULL.  This is the type code of objects which are
 * considered as false for the value of predicates.
 */

/* FIXNUM
 * Small integer.  Fits in the datum portion of a Scheme Pointer.
 */

/* HUNK3
 * User object like a CONS, but with 3 slots rather than 2.
 */
#define HUNK_CXR0		0
#define HUNK_CXR1		1
#define HUNK_CXR2		2

/* INTERNED_SYMBOL
 * A symbol, such as the result of evaluating (QUOTE A).  Some important
 * properties of symbols are that they have a print name, and may be
 * 'interned' so that all instances of a symbol with the same name share
 * a unique object.  The storage pointed to by a symbol includes both
 * the print name (a string) and the value associated with a variable of
 * that name in the global environment.  In looking for the value of a
 * variable in the global environment, the dangerous and potentially
 * dangerous bits are stored in the dangerous bits of these two cells as
 * indicated below.
 */
#define SYMBOL_NAME		0
#define SYMBOL_GLOBAL_VALUE	1
#define GLOBAL_P_DANGER		0
#define GLOBAL_DANGER		1

/* LAMBDA
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

/* LIST
 * Ordinary CONS cell as supplied to a user.  Perhaps this data type is
 * misnamed ... CONS or PAIR would be better.
 */
#define CONS_CAR		0
#define CONS_CDR		1

/* MANIFEST_NM_VECTOR
 * Not a true object, this type code is used to indicate the start of a
 * vector which contains objects other than Scheme pointers.  The
 * address portion indicates the number of cells of non-pointers
 * which follow the header word.  For use primarily in garbage
 * collection to indicate the number of words to copy but not trace.
 */

/* MANIFEST_SPECIAL_NM_VECTOR Similar to MANIFEST_NM_VECTOR but the
 * contents are relocated when loaded by the FALOADer.  This header
 * occurs in pure and constant space to indicate the start of a region
 * which contains Pointers to addresses which are known never to move in
 * the operation of the system.
 */

/* MANIFEST_VECTOR
 * Synonym for NULL, used as first cell in a vector object to indicate
 * how many cells it occupies.  Usage is similar to MANIFEST_NM_VECTOR
 */

/* NON_MARKED_VECTOR
 * User-visible object containing arbitrary bits.  Not currently used.
 * The data portion will always point to a MANIFEST_NM_VECTOR or
 * MANIFEST_SPECIAL_NM_VECTOR specifying the length of the vector.
 */
#define NM_VECTOR_HEADER	0
#define NM_ENTRY_COUNT		1
#define NM_DATA			2
#define NM_HEADER_LENGTH	2

/* NULL
 * The type code used by predicates to test for 'false' and by list
 * operations for testing for the end of a list.
 */

/* PRIMITIVE
 * The data portion contains a number specifying a particular primitive
 * operation to be performed.  An object of type PRIMITIVE can be
 * APPLYed in the same way an object of type PROCEDURE can be. 
 */

/* PRIMITIVE_EXTERNAL
 * Functionally identical to PRIMITIVE.  The distinctions are that a
 * PRIMITIVE is constrained to take no more than 3 arguments, PRIMITIVEs
 * can be formed into more efficient PRIMITIVE-COMBINATIONs by a
 * compiler, and that PRIMITIVE_EXTERNALs are user supplied.
 */

/* PROCEDURE (formerly CLOSURE)
 * Consists of two parts: a LAMBDA expression and the environment
 * in which the LAMBDA was evaluated to yield the PROCEDURE.
 */
#define PROCEDURE_LAMBDA_EXPR	0
#define PROCEDURE_ENVIRONMENT	1

/* RETURN_CODE
 * Represents an address where computation is to continue.  These can be
 * thought of as states in a finite state machine, labels in an assembly
 * language program, or continuations in a formal semantics.  When the
 * interpretation of a single SCode item requires the EVALuation of a
 * subproblem, a RETURN_CODE is left behind indicating where computation
 * continues after the evaluation.
 */

/* STATE_POINT and STATE_SPACE
 * Data structures used to keep track of dynamic wind state.  Both of
 * these are actually ordinary vectors with a special tag in the first
 * user accessible slot.  A STATE_SPACE consists of just a pointer to
 * the current point in that space.  A STATE_POINT contains a
 * procedure to be used when moving through the point (the forward
 * thunk), an alternate procedure to undo the effects of the first
 * (the backward thunk), and the point to which you can move directly
 * from this point.
 */

#define STATE_POINT_HEADER		0
#define STATE_POINT_TAG			1
#define STATE_POINT_BEFORE_THUNK	2
#define STATE_POINT_AFTER_THUNK		3
#define STATE_POINT_NEARER_POINT	4
#define STATE_POINT_DISTANCE_TO_ROOT	5
#define STATE_POINT_SIZE		6

#define STATE_SPACE_HEADER		0
#define STATE_SPACE_TAG			1
#define STATE_SPACE_NEAREST_POINT	2
#define STATE_SPACE_SIZE		3

/* When in RC_MOVE_TO_ADJACENT_POINT in the interpreter, the following
   information is available on the stack (placed there by
   Translate_To_Point
*/
#define TRANSLATE_FROM_POINT		0
#define TRANSLATE_FROM_DISTANCE		1
#define TRANSLATE_TO_POINT		2
#define TRANSLATE_TO_DISTANCE		3

/* TRAP
 * Trap-on-reference object.  Used as a placeholder for a variable's value
 * when special action must be taken at lookup time.  Used to implement
 * fluid variables, active values, etc.
 */

#define TRAP_TAG        0	/* NIL => fluid variable
				   else handler procedure */
#define TRAP_DEFAULT    1	/* Default value of this slot */
#define TRAP_FROB       2	/* For user supplied handlers */
#define TRAP_SIZE	3

/* TRUE
 * The initial binding of the variable T is to an object of this type.
 * This type is the beginnings of a possible move toward a system where
 * predicates check for TRUE / FALSE rather than not-NULL / NULL.
 */

/* UNASSIGNED
 * There are two objects made with a data type of UNASSIGNED.  The first
 * (called the "unassigned object") is a value stored in an environment
 * to indicate that a variable is lambda-bound in that environment but
 * does not yet have an initial value.  The second (called the "unbound
 * object") is stored in the global value slot of a value when it is
 * created, and will therefore be returned when a variable is referenced
 * in an environment where there are no bindings for it.  The numbers
 * here show the data parts corresponding to the two interpretations.
 */

#define UNASSIGNED	0
#define UNBOUND		1

/* UNINTERNED_SYMBOL
 * This indicates that the object is in the format of an INTERNED_SYMBOL
 * but is not interned.
 */

/* VARIABLE
 * Variable reference. Contains the symbol referenced, and (if it has
 * been compiled) the frame and offset in the frame in which it was
 * found.  One of these cells is multiplexed by having its type code
 * indicate one of four modes of reference: not yet compiled, local
 * (formal) reference, auxiliary reference, or global value reference
 */
#define VARIABLE_SYMBOL		0
#define VARIABLE_FRAME_NO	1
#define VARIABLE_OFFSET		2
#define VARIABLE_COMPILED_TYPE	1

/* VECTOR
 * A group of contiguous cells with a header (of type MANIFEST_VECTOR)
 * indicating the length of the group.
 */
#define VECTOR_TYPE		0
#define VECTOR_LENGTH		0
#define VECTOR_DATA		1

/* VECTOR_16B
 * Points to a MANIFEST_NM_VECTOR or MANIFEST_SPECIAL_NM_VECTOR header.
 * The format is described under NON_MARKED_VECTOR.  The contents are to
 * be treated as an array of 16-bit signed or unsigned quantities.  Not
 * currently used, although this may be a useful way to allow users to
 * inspect the internal representation of bignums.
 */

/* VECTOR_1B
 * Similar to VECTOR_16B, but used for a compact representation of an
 * array of booleans.
 */

/* VECTOR_8B
 * An alternate name of CHARACTER_STRING.
 */
