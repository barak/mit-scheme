/* -*-C-*-

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/sample.c,v 9.24 1989/09/20 23:11:19 cph Rel $ */

/* This file is intended to help you find out how to write primitives.
   Many concepts needed to write primitives can be found by looking
   at actual primitives in the system.  Hence this file will often
   ask you to look at other files that contain system primitives.  */

/* Files that contain primitives must have the following includes
   near the top of the file.  */
#include "scheme.h"
#include "prims.h"

/* Scheme.h supplies useful macros that are used throughout the
   system, and prims.h supplies macros that are used in defining
   primitives.  */

/* To make a primitive, you must use the macro DEFINE_PRIMITIVE
   with six arguments, followed by the body of C source code
   that you want the primitive to execute.
   The six arguments are:
   1. A string representing the scheme name that you want to identify
      this primitive with.
   2. The name you want to give to this body of code (a C procedure
      name).  By convention, all such names begin with `Prim_'.
   3. The minimum number of arguments that this scheme primitive
      should receive.  Currently this is not implemented and should be
      the same as the maximum number of arguments (or 0 if the maximum
      is the special symbol LEXPR).
   4. The maximum number of arguments that this scheme primitive
      should receive.  If this primitive will take any number of
      arguments, use LEXPR here.
   5. A documentation string, or 0 meaning no documentation.

   The value returned by the body of code following the
   DEFINE_PRIMITIVE is the value of the scheme primitive.  Note that
   this must be a SCHEME_OBJECT (with type tag and datum
   field), and not an arbitrary C object.

   As an example, here is a primitive that takes no arguments and
   always returns SHARP_F (SHARP_F is defined in scheme.h and
   identical to the scheme object #F. SHARP_T is identical to the
   scheme object #T).  */

DEFINE_PRIMITIVE ("RETURN-SHARP-F", Prim_return_sharp_f, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (SHARP_F);
}

/* This will create the primitive RETURN-SHARP-F and when a new Scheme
   is made (with the Makefile properly edited to include this file),
   evaluating (make-primitive-procedure 'return-sharp-f) will return a
   primitive procedure that when called with no arguments, will return
   #F.  */

/* Here's another example that shows the use of the `ARG_REF' macro to
   get one of the arguments to the primitive: */

DEFINE_PRIMITIVE ("IDENTITY", Prim_identity, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (ARG_REF (1));
}

/* Some primitives may have to allocate space on the heap in order
   to return lists or vectors.  There are two things of importance to
   note here.  First, the primitive is responsible for making sure
   that there is enough space on the heap for the new structure that
   is being made.  For instance, in making a PAIR, two words on the
   heap are used, one to point to the CAR, one for CDR.  The macro
   Primitive_GC_If_Needed is supplied to let you check if there is
   room on the heap.  Primitive_GC_If_Needed takes one argument which
   is the amount of space you would like to allocate.  If there is not
   enough space on the heap, a garbage collection happens and
   afterwards the primitive is restarted with the same arguments. The
   second thing to notice is that the primitive is responsible for
   updating Free according to how many words of storage it has used
   up.  Note that the primitive is restarted, not continued, thus any
   side effects must be done after the heap overflow check since
   otherwise they would be done twice.

   A pair is object which has a type TC_LIST and points to the first
   element of the pair.  The macro MAKE_POINTER_OBJECT takes a type
   code and an address or data and returns a scheme object with that
   type code and that address or data.  See scheme.h and the files
   included there for the possible type codes.  The following is the
   equivalent of CONS and takes two arguments and returns the pair
   which contains both arguments. For further examples on heap
   allocation, see the primitives in "list.c", "hunk.c" and
   "vector.c".  */

DEFINE_PRIMITIVE ("NEW-CONS", Prim_new_cons, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (cons ((ARG_REF (1)), (ARG_REF (2))));
}

/* The following primitive takes three arguments and returns a list
   of them.  Note how the CDR of the first two pairs points
   to the next pair.  Also, scheme objects are of type SCHEME_OBJECT
   (defined in object.h).  Note that the result returned can be
   held in a temporary variable even before the contents of the
   object are stored in heap.  */

DEFINE_PRIMITIVE ("WHY-SHOULDNT-THE-NAME-BE-RANDOM?", Prim_utterly_random, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (cons ((ARG_REF (1)),
	   (cons ((ARG_REF (2)),
		  (cons ((ARG_REF (3)),
			 EMPTY_LIST))))));
}

/* Here is a primitive that tries to add 3 to its argument.  */

DEFINE_PRIMITIVE ("3+", Prim_add_3, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer ((arg_integer (1)) + 3));
}
