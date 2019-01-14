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

/* Description of the user data objects.  This should parallel the
   file SDATA.SCM in the runtime system.  */

#ifndef SCM_SDATA_H
#define SCM_SDATA_H

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
#define CELL_CONTENTS		0

/* BYTEVECTOR
 * Format consists of the normal non-marked vector header
 * (BYTEVECTOR_HEADER) followed by the number of bytes in the vector
 * (with type-code 0), followed by the bytes themselves.
 */
#define BYTEVECTOR_HEADER	0
#define BYTEVECTOR_LENGTH_INDEX	1
#define BYTEVECTOR_LENGTH_SIZE	1
#define BYTEVECTOR_DATA		2

#define UNICODE_STRING_HEADER 0
#define UNICODE_STRING_LENGTH_INDEX 1
#define UNICODE_STRING_DATA 2

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

/* ENTITY
   A cons of a procedure and something else.
   When invoked, it invokes (tail recurses) into the procedure passing
   the entity and the arguments to it.
 */

#define ENTITY_OPERATOR		0
#define ENTITY_DATA		1

/* ENVIRONMENT
 * Associates identifiers with values.
 * The identifiers are either from a lambda-binding (as in a procedure
 * call) or a incremental (run-time) DEFINE (known as an 'auxilliary'
 * binding).
 * When an environment frame is created, it only contains lambda
 * bindings.  If incremental defines are performed in it, it acquires
 * an extension which contains a list of the auxiliary bindings.
 *
 * Besides the lambda bindings, an environment frame contains a
 * pointer to the procedure which created it.  It is through this
 * procedure that the parent frame is found.
 *
 * An environment frame has three distinct stages in its formation:
 * - A STACK_COMBINATION is the structure built on the stack to
 * evaluate normal (long) combinations.  It contains a slot for the
 * finger and the combination whose operands are being evaluated.
 * Only some of the argument slots in a stack-combination are
 * meaningful: those which have already been evaluated (those not
 * "hidden" by the finger).  This is the first stage.
 * - A STACK_ENVIRONMENT is the format used at Internal_Apply
 * just as an application is about to occur.
 * - An ENVIRONMENT is a real environment frame, containing
 * associations between names and values.  It is the final stage, and
 * corresponds to the structure described above.
 */

#define ENVIRONMENT_HEADER	0
#define ENVIRONMENT_FUNCTION	1
#define ENVIRONMENT_FIRST_ARG	2

#define STACK_ENV_EXTRA_SLOTS   1
#define STACK_ENV_HEADER        0
#define STACK_ENV_FUNCTION      1
#define STACK_ENV_FIRST_ARG     2

#define STACK_COMB_FINGER       0
#define STACK_COMB_FIRST_ARG    1

/* An environment chain always ends in a pointer with type code
   of GLOBAL_ENV.  This will contain an address part which
   either indicates that the lookup should continue on to the
   true global environment, or terminate at this frame.

   We arrange for the global environment to be the same as #F, and the
   end chain to be different by toggling the lowest bit:  */

#define GLOBAL_ENV     (OBJECT_TYPE (SHARP_F))
#define THE_GLOBAL_ENV (MAKE_OBJECT (GLOBAL_ENV, (OBJECT_DATUM (SHARP_F))))
#define THE_NULL_ENV (MAKE_OBJECT (GLOBAL_ENV, ((OBJECT_DATUM (SHARP_F)) ^ 1)))

#define GLOBAL_FRAME_P(frame) ((frame) == THE_GLOBAL_ENV)
#define NULL_FRAME_P(frame) ((frame) == THE_NULL_ENV)
#define PROCEDURE_FRAME_P(frame) ((OBJECT_TYPE (frame)) == TC_ENVIRONMENT)

#define GET_FRAME_PARENT(frame)						\
  (GET_PROCEDURE_ENVIRONMENT (GET_FRAME_PROCEDURE (frame)))

#define GET_FRAME_PROCEDURE(frame)					\
  (MEMORY_REF ((frame), ENVIRONMENT_FUNCTION))

#define SET_FRAME_EXTENSION(frame, extension)				\
  MEMORY_SET ((frame), ENVIRONMENT_FUNCTION, (extension))

#define GET_FRAME_ARG_CELL(frame, index)				\
  (MEMORY_LOC ((frame), (ENVIRONMENT_FIRST_ARG + (index))))

/* Environment extension objects:

   These objects replace the procedure in environment frames when an
   aux slot is desired.  The parent frame is copied into the extension
   so that the "compiled" lookup code does not have to check whether
   the frame has been extended or not.

   Note that for the code to work, ENV_EXTENSION_PARENT_FRAME must be
   equal to PROCEDURE_ENVIRONMENT.

   The following constants are implicitely hard-coded in lookup.c,
   where a new extension object is consed in extend_frame.
 */

#define ENV_EXTENSION_HEADER		0
#define ENV_EXTENSION_PARENT_FRAME	1
#define ENV_EXTENSION_PROCEDURE		2
#define ENV_EXTENSION_COUNT		3
#define ENV_EXTENSION_MIN_SIZE		4

#define EXTENDED_FRAME_P(frame)						\
  (FRAME_EXTENSION_P (GET_FRAME_PROCEDURE (frame)))

#define FRAME_EXTENSION_P VECTOR_P

#define GET_EXTENDED_FRAME_BINDINGS(frame)				\
  (GET_FRAME_EXTENSION_BINDINGS (GET_FRAME_PROCEDURE (frame)))

#define GET_FRAME_EXTENSION_BINDINGS(extension)				\
  ((OBJECT_ADDRESS (extension)) + ENV_EXTENSION_MIN_SIZE)

#define GET_EXTENDED_FRAME_LENGTH(frame)				\
  (GET_FRAME_EXTENSION_LENGTH (GET_FRAME_PROCEDURE (frame)))

#define GET_FRAME_EXTENSION_LENGTH(extension)				\
  (UNSIGNED_FIXNUM_TO_LONG						\
   ((OBJECT_ADDRESS (extension)) [ENV_EXTENSION_COUNT]))

#define SET_EXTENDED_FRAME_LENGTH(frame, length)			\
  (SET_FRAME_EXTENSION_LENGTH ((GET_FRAME_PROCEDURE (frame)), (length)))

#define SET_FRAME_EXTENSION_LENGTH(extension, length)			\
  (((OBJECT_ADDRESS (extension)) [ENV_EXTENSION_COUNT])			\
   = (LONG_TO_UNSIGNED_FIXNUM (length)))

#define GET_MAX_EXTENDED_FRAME_LENGTH(frame)				\
  (GET_MAX_FRAME_EXTENSION_LENGTH (GET_FRAME_PROCEDURE (frame)))

#define GET_MAX_FRAME_EXTENSION_LENGTH(extension)			\
  ((VECTOR_LENGTH (extension)) - (ENV_EXTENSION_MIN_SIZE - 1))

#define GET_EXTENDED_FRAME_PROCEDURE(frame)				\
  (GET_FRAME_EXTENSION_PROCEDURE (GET_FRAME_PROCEDURE (frame)))

#define GET_FRAME_EXTENSION_PROCEDURE(extension)			\
  (MEMORY_REF ((extension), ENV_EXTENSION_PROCEDURE))

#define SET_FRAME_EXTENSION_PROCEDURE(extension, procedure)		\
  MEMORY_SET ((extension), ENV_EXTENSION_PROCEDURE, (procedure))

#define SET_FRAME_EXTENSION_PARENT_FRAME(extension, frame)		\
  MEMORY_SET ((extension), ENV_EXTENSION_PARENT_FRAME, (frame))

/* EXTENDED_FIXNUM
 * Not used in the C version.  On the 68000 this is used for 24-bit
 * integers, while FIXNUM is used for 16-bit integers.
 */

/* EXTENDED_PROCEDURE
 * Type of procedure created by evaluation of EXTENDED_LAMBDA.
 * It's fields are the same as those for PROCEDURE.
 */

/* FALSE
 * Alternate name for NULL.  This is the type code of objects which are
 * considered as false for the value of predicates.
 */

/* FIXNUM
 * Small integer.  Fits in the datum portion of a SCHEME_OBJECT.
 */

/* HUNK3
 * User object like a CONS, but with 3 slots rather than 2.
 */
#define HUNK3_CXR0		0
#define HUNK3_CXR1		1
#define HUNK3_CXR2		2

/* Old code uses these */

#define HUNK_CXR0		HUNK3_CXR0
#define HUNK_CXR1		HUNK3_CXR1
#define HUNK_CXR2		HUNK3_CXR2

/* INTERNED_SYMBOL
 * A symbol, such as the result of evaluating (QUOTE A).  Some
 * important properties of symbols are that they have a print name,
 * and may be 'interned' so that all instances of a symbol with the
 * same name share a unique object.  The storage pointed to by a
 * symbol includes both the print name (a string) and the value cell
 * associated with a variable of that name in the global environment.
 */
#define SYMBOL_NAME		0
#define SYMBOL_GLOBAL_VALUE	1

#define SYMBOL_GLOBAL_VALUE_CELL(symbol)				\
  (MEMORY_LOC ((symbol), SYMBOL_GLOBAL_VALUE))

#define GET_SYMBOL_GLOBAL_VALUE(symbol)					\
  (* (SYMBOL_GLOBAL_VALUE_CELL (symbol)))

#define SET_SYMBOL_GLOBAL_VALUE(symbol, value)				\
  ((* (SYMBOL_GLOBAL_VALUE_CELL (symbol))) = (value))

#define GET_SYMBOL_NAME(symbol) (MEMORY_REF ((symbol), SYMBOL_NAME))

#define SET_SYMBOL_NAME(symbol, name)					\
  MEMORY_SET ((symbol), SYMBOL_NAME, (name))

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

/* PROCEDURE (formerly CLOSURE)
 * Consists of two parts: a LAMBDA expression and the environment
 * in which the LAMBDA was evaluated to yield the PROCEDURE.
 */
#define PROCEDURE_LAMBDA_EXPR	0
#define PROCEDURE_ENVIRONMENT	1

#define GET_PROCEDURE_LAMBDA(procedure)					\
  (MEMORY_REF ((procedure), PROCEDURE_LAMBDA_EXPR))

#define GET_PROCEDURE_ENVIRONMENT(procedure)				\
  (MEMORY_REF ((procedure), PROCEDURE_ENVIRONMENT))

/* QUAD or HUNK4
 * Like a pair but with 4 components.
 */

#define HUNK4_CXR0				0
#define HUNK4_CXR1				1
#define HUNK4_CXR2				2
#define HUNK4_CXR3				3

/* REFERENCE_TRAP
 * Causes the variable lookup code to trap.
 * Used to implement a variety of features.
 * This type code is really the collection of two, done this way for
 * efficiency.  Traps whose datum is less than TRAP_MAX_IMMEDIATE are
 * immediate (not pointers).  The rest are pairs.  The garbage
 * collector deals with them specially.  */

#define TRAP_TAG				0
#define TRAP_EXTRA				1

#define GET_TRAP_TAG(object)						\
  (MEMORY_REF ((object), TRAP_TAG))

#define GET_TRAP_EXTRA(object)						\
  (MEMORY_REF ((object), TRAP_EXTRA))

#define SET_TRAP_EXTRA(object, extra)					\
  MEMORY_SET ((object), TRAP_EXTRA, (extra))

#define GET_TRAP_CACHE GET_TRAP_EXTRA
#define SET_TRAP_CACHE SET_TRAP_EXTRA

#define CACHE_CELL				HUNK3_CXR0
#define CACHE_CLONE				HUNK3_CXR1
#define CACHE_REFERENCES			HUNK3_CXR2

#define CACHE_REFERENCES_LOOKUP			HUNK3_CXR0
#define CACHE_REFERENCES_ASSIGNMENT		HUNK3_CXR1
#define CACHE_REFERENCES_OPERATOR		HUNK3_CXR2


#define GET_CACHE_VALUE(cache)						\
  (MEMORY_REF ((cache), CACHE_CELL))

#define SET_CACHE_VALUE(cache, value)					\
  MEMORY_SET ((cache), CACHE_CELL, (value))

#define GET_CACHE_CLONE(cache)						\
  (MEMORY_REF ((cache), CACHE_CLONE))

#define SET_CACHE_CLONE(cache, clone)					\
  MEMORY_SET ((cache), CACHE_CLONE, (clone))

#define GET_CACHE_REFERENCES_OBJECT(cache)				\
  (MEMORY_REF ((cache), CACHE_REFERENCES))


#define GET_CACHE_REFERENCES(cache, kind)				\
  (MEMORY_LOC ((GET_CACHE_REFERENCES_OBJECT (cache)), (kind)))

#define GET_CACHE_LOOKUP_REFERENCES(cache)				\
  (GET_CACHE_REFERENCES ((cache), CACHE_REFERENCES_LOOKUP))

#define GET_CACHE_ASSIGNMENT_REFERENCES(cache)				\
  (GET_CACHE_REFERENCES ((cache), CACHE_REFERENCES_ASSIGNMENT))

#define GET_CACHE_OPERATOR_REFERENCES(cache)				\
  (GET_CACHE_REFERENCES ((cache), CACHE_REFERENCES_OPERATOR))


#define GET_CACHE_REFERENCE_BLOCK(reference)				\
  (PAIR_CAR (reference))

#define SET_CACHE_REFERENCE_BLOCK(reference, block)			\
  SET_PAIR_CAR (reference, block)

#define GET_CACHE_REFERENCE_OFFSET(reference)				\
  (OBJECT_DATUM (PAIR_CDR (reference)))

#define SET_CACHE_REFERENCE_OFFSET(reference, offset)			\
  (SET_PAIR_CDR ((reference), (LONG_TO_UNSIGNED_FIXNUM (offset))))

/* RETURN_CODE
 * Represents an address where computation is to continue.  These can be
 * thought of as states in a finite state machine, labels in an assembly
 * language program, or continuations in a formal semantics.  When the
 * interpretation of a single SCode item requires the EVALuation of a
 * subproblem, a RETURN_CODE is left behind indicating where computation
 * continues after the evaluation.
 */

/* TRUE
 * The initial binding of the variable T is to an object of this type.
 * This type is the beginnings of a possible move toward a system where
 * predicates check for TRUE / FALSE rather than not-NULL / NULL.
 */

/* UNINTERNED_SYMBOL
 * This indicates that the object is in the format of an INTERNED_SYMBOL
 * but is not interned.
 */

/* VECTOR
 * A group of contiguous cells with a header (of type MANIFEST_VECTOR)
 * indicating the length of the group.
 */
#define VECTOR_DATA		1

/* VECTOR_16B
 * Points to a MANIFEST_NM_VECTOR or MANIFEST_SPECIAL_NM_VECTOR header.
 * The format is described under NON_MARKED_VECTOR.  The contents are to
 * be treated as an array of 16-bit signed or unsigned quantities.  Not
 * currently used.
 */

/* VECTOR_1B
 * Similar to VECTOR_16B, but used for a compact representation of an
 * array of booleans.
 */

/* VECTOR_8B
 * An alternate name of CHARACTER_STRING.
 */

/* COMPLEX
 * System Pair with REAL in CAR and IMAGINARY in CDR
 */

#define COMPLEX_REAL		0
#define COMPLEX_IMAG		1

/* EPHEMERON
 * Similar to a weak pair, but the datum is weakly referenced too.  The
 * key and datum are simultaneously dropped iff the only references to
 * the key go through the datum.  Every ephemeron has extra slots for
 * data structures that the garbage collector needs to implement this,
 * so that the garbage collector need not allocate auxiliary storage.
 */

#define EPHEMERON_MANIFEST	0
#define EPHEMERON_KEY		1
#define EPHEMERON_DATUM		2
#define EPHEMERON_LIST		3
#define EPHEMERON_NEXT		4

#define EPHEMERON_SIZE		5

#define MARKED_EPHEMERON_MANIFEST				\
  (MAKE_OBJECT (TC_MANIFEST_VECTOR, (EPHEMERON_SIZE - 1)))

#define UNMARKED_EPHEMERON_MANIFEST				\
  (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (EPHEMERON_SIZE - 1)))

#endif /* not SCM_SDATA_H */
