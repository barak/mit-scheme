/* -*-C-*-

$Id: future.c,v 9.30 2002/11/20 19:46:08 cph Exp $

Copyright (c) 1987, 1988, 1989, 1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* Support code for futures */

#include "scheme.h"
#include "prims.h"
#include "locks.h"

#ifndef COMPILE_FUTURES
#include "Error: future.c is useless without COMPILE_FUTURES"
#endif

/* This is how we support future numbering for external metering */
#ifndef New_Future_Number
#define New_Future_Number() SHARP_F
#else
SCHEME_OBJECT Get_New_Future_Number ();
#endif

/*

A future is a VECTOR starting with <determined?>, <locked?> and
<waiting queue / value>,

where <determined?> is #!false if no value is known yet,
                       #!true if value is known and future can vanish at GC,
                       otherwise value is known, but keep the slot

and where <locked> is #!true if someone wants slot kept for a time.

*/

DEFINE_PRIMITIVE ("TOUCH", Prim_touch, 1, 1, 0)
{
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (1);
  TOUCH_IN_PRIMITIVE ((ARG_REF (1)), result);
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("FUTURE?", Prim_future_p, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (FUTURE_P (ARG_REF (1))));
}

/* Utility setting routine for use by the various test and set if
   equal operators.
*/

long
Set_If_Equal(Base, Offset, New, Wanted)
     SCHEME_OBJECT Base, Wanted, New;
     long Offset;
{
  Lock_Handle lock;
  SCHEME_OBJECT Old_Value, Desired, Remember_Value;
  long success;

  TOUCH_IN_PRIMITIVE(Wanted, Desired);
Try_Again:
  Remember_Value = MEMORY_REF (Base, Offset);
  TOUCH_IN_PRIMITIVE(Remember_Value, Old_Value);
  lock = Lock_Cell(MEMORY_LOC (Base, Offset));
  if (Remember_Value != FAST_MEMORY_REF (Base, Offset))
  {
    Unlock_Cell(lock);
    goto Try_Again;
  }
  if (Old_Value == Desired)
  {
    Do_Store_No_Lock(MEMORY_LOC (Base, Offset), New);
    success = true;
  }
  else
  {
    success = false;
  }
  Unlock_Cell(lock);
  return success;
}

DEFINE_PRIMITIVE ("SET-CAR-IF-EQ?!", Prim_set_car_if_eq, 3, 3,
  "Replace the car of PAIR with NEW-VALUE iff it contains OLD-VALUE.\n\
Return PAIR if so, otherwise return '().")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, PAIR_P);
  {
    fast SCHEME_OBJECT pair = (ARG_REF (1));
    if (Set_If_Equal (pair, CONS_CAR, (ARG_REF (2)), (ARG_REF (3))))
      PRIMITIVE_RETURN (pair);
  }
  PRIMITIVE_RETURN (EMPTY_LIST);
}

DEFINE_PRIMITIVE ("SET-CDR-IF-EQ?!", Prim_set_cdr_if_eq, 3, 3,
  "Replace the cdr of PAIR with NEW-VALUE iff it contains OLD-VALUE.\n\
Return PAIR if so, otherwise return '().")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, PAIR_P);
  {
    fast SCHEME_OBJECT pair = (ARG_REF (1));
    if (Set_If_Equal (pair, CONS_CDR, (ARG_REF (2)), (ARG_REF (3))))
      PRIMITIVE_RETURN (pair);
  }
  PRIMITIVE_RETURN (EMPTY_LIST);
}

/* (VECTOR-SET-IF-EQ?! <Vector> <Offset> <New Value> <Old Value>)
   Replaces the <Offset>th element of <Vector> with <New Value> if it used
   to contain <Old Value>.  The value returned is either <Vector> (if
   the modification takes place) or '() if it does not.
*/
DEFINE_PRIMITIVE ("VECTOR-SET-IF-EQ?!", Prim_vector_set_if_eq, 4, 4,
  "Replace VECTOR's INDEX'th element with NEW-VALUE iff it contains OLD-VALUE.\n\
Return VECTOR if so, otherwise return '().")
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, VECTOR_P);
  {
    fast SCHEME_OBJECT vector = (ARG_REF (1));
    if (Set_If_Equal
	(vector,
	 ((arg_index_integer (2, (VECTOR_LENGTH (vector)))) + 1),
	 (ARG_REF (3)),
	 (ARG_REF (4))))
      PRIMITIVE_RETURN (vector);
  }
  PRIMITIVE_RETURN (EMPTY_LIST);
}

DEFINE_PRIMITIVE ("SET-CXR-IF-EQ?!", Prim_set_cxr_if_eq, 4, 4,
  "Replace HUNK3's INDEX'th element with NEW-VALUE iff it contains OLD-VALUE.\n\
Return HUNK3 if so, otherwise return '().")
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (1, HUNK3_P);
  {
    fast SCHEME_OBJECT hunk3 = (ARG_REF (1));
    if (Set_If_Equal
	(hunk3,
	 ((arg_index_integer (2, 3)) + 1),
	 (ARG_REF (3)),
	 (ARG_REF (4))))
      PRIMITIVE_RETURN (hunk3);
  }
  PRIMITIVE_RETURN (EMPTY_LIST);
}

DEFINE_PRIMITIVE ("FUTURE-SIZE", Prim_future_size, 1, 1,
  "Return the number of elements in FUTURE.\n\
This is similar to SYSTEM-VECTOR-SIZE,\n\
but works only on futures and doesn't touch them.")
{
  PRIMITIVE_HEADER (1)
  CHECK_ARG (1, FUTURE_P);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (VECTOR_LENGTH (ARG_REF (1))));
}

DEFINE_PRIMITIVE ("FUTURE-REF", Prim_future_ref, 2, 2,
  "Return FUTURE's INDEX'th element.\n\
This is similar to SYSTEM-VECTOR-REF,\n\
but works only on futures and doesn't touch them.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, FUTURE_P);
  {
    fast SCHEME_OBJECT future = (ARG_REF (1));
    PRIMITIVE_RETURN
      (VECTOR_REF
       (future, (arg_index_integer (2, (VECTOR_LENGTH (future))))));
  }
}

DEFINE_PRIMITIVE ("FUTURE-SET!", Prim_future_set, 3, 3,
  "Modify FUTURE's INDEX'th element to be VALUE.\n\
This is similar to SYSTEM-VECTOR-SET!,\n\
but works only on futures and doesn't touch them.")
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, FUTURE_P);
  {
    fast SCHEME_OBJECT future = (ARG_REF (1));
    fast long index = (arg_index_integer (2, (VECTOR_LENGTH (future))));
    fast SCHEME_OBJECT result = (VECTOR_REF (future, index));
    VECTOR_SET (future, index, (ARG_REF (3)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("LOCK-FUTURE!", Prim_lock_future, 1, 1,
  "Set the lock flag on FUTURE.\n\
This flag prevents FUTURE from being spliced out by the garbage collector.\n\
If FUTURE is not a future, return #F immediately,\n\
otherwise return #T after the lock has been set.\n\
Will wait as long as necessary for the lock to be set.")
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT future = (ARG_REF (1));
    if (! (FUTURE_P (future)))
      PRIMITIVE_RETURN (SHARP_F);
    while (1)
      {
	if (INTERRUPT_PENDING_P (INT_Mask))
	  signal_interrupt_from_primitive ();
	{
	  fast SCHEME_OBJECT lock;
	  SWAP_POINTERS ((MEMORY_LOC (future, FUTURE_LOCK)), SHARP_T, lock);
	  if (lock == SHARP_F)
	    PRIMITIVE_RETURN (SHARP_T);
	}
	Sleep (CONTENTION_DELAY);
      }
  }
}

DEFINE_PRIMITIVE ("UNLOCK-FUTURE!", Prim_unlock_future, 1, 1,
  "Clear the lock flag on FUTURE.\n\
If FUTURE is not a future, return #F immediately,\n\
otherwise return #T after the lock has been cleared.")
{
  PRIMITIVE_HEADER (1);
  {
    fast SCHEME_OBJECT future = (ARG_REF (1));
    if (! (FUTURE_P (future)))
      PRIMITIVE_RETURN (SHARP_F);
    if (! (Future_Is_Locked (future)))
      error_wrong_type_arg (1);
    MEMORY_SET (future, FUTURE_LOCK, SHARP_F);
    PRIMITIVE_RETURN (SHARP_T);
  }
}

DEFINE_PRIMITIVE ("FUTURE->VECTOR", Prim_future_to_vector, 1, 1,
  "Return a newly-allocated vector containing FUTURE's elements.
If FUTURE is not a future, return #F instead.")
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT future = (ARG_REF (1));
    if (! (FUTURE_P (future)))
      PRIMITIVE_RETURN (SHARP_F);
    {
      long length = (VECTOR_LENGTH (future));
      fast SCHEME_OBJECT * scan_source = (MEMORY_LOC (future, 1));
      fast SCHEME_OBJECT * end_source = (scan_source + length);
      SCHEME_OBJECT result =
	(allocate_marked_vector (TC_VECTOR, length, true));
      fast SCHEME_OBJECT * scan_result = (MEMORY_LOC (result, 1));
      while (scan_source < end_source)
	(*scan_result++) = (MEMORY_FETCH (*scan_source++));
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("NON-TOUCHING-EQ?", Prim_future_eq, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((ARG_REF (1)) == (ARG_REF (2))));
}

/* MAKE-INITIAL-PROCESS is called to create a small stacklet which
 * will just call the specified thunk and then end the computation
 */

DEFINE_PRIMITIVE ("MAKE-INITIAL-PROCESS", Prim_make_initial_process, 1, 1, 0)
{
  SCHEME_OBJECT Result;
  long Useful_Length;
  PRIMITIVE_HEADER (1);

  Result = MAKE_POINTER_OBJECT (TC_CONTROL_POINT, Free);
  Useful_Length = (3 * CONTINUATION_SIZE) + STACK_ENV_EXTRA_SLOTS + 1;

#ifdef USE_STACKLETS

  {
    long Allocated_Length, Waste_Length;

    Allocated_Length = (Useful_Length + STACKLET_SLACK + STACKLET_HEADER_SIZE);
    if (Allocated_Length < Default_Stacklet_Size)
    {
      Allocated_Length = Default_Stacklet_Size;
      Waste_Length = ((Allocated_Length + 1) -
		      (Useful_Length + STACKLET_HEADER_SIZE));
    }
    else
    {
      Waste_Length = (STACKLET_SLACK + 1);
    }
    Primitive_GC_If_Needed(Allocated_Length + 1);
    Free[STACKLET_LENGTH] =
      MAKE_POINTER_OBJECT (TC_MANIFEST_VECTOR, Allocated_Length);
    Free[STACKLET_REUSE_FLAG] = SHARP_T;
    Free[STACKLET_UNUSED_LENGTH] =
      MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, Waste_Length);
    Free += (Allocated_Length + 1) - Useful_Length;
  }

#else /* not USE_STACKLETS */

  Free[STACKLET_LENGTH] =
    MAKE_OBJECT (TC_MANIFEST_VECTOR, Useful_Length + STACKLET_HEADER_SIZE - 1);
  Free[STACKLET_REUSE_FLAG] = SHARP_F;
  Free[STACKLET_UNUSED_LENGTH] = MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, 0);
  Free += STACKLET_HEADER_SIZE;

#endif /* USE_STACKLETS */

  Free[CONTINUATION_EXPRESSION] = LONG_TO_FIXNUM(FETCH_INTERRUPT_MASK());
  Free[CONTINUATION_RETURN_CODE] =
    MAKE_OBJECT (TC_RETURN_CODE, RC_RESTORE_INT_MASK);
  Free += CONTINUATION_SIZE;
  Free[CONTINUATION_EXPRESSION] = SHARP_F;
  Free[CONTINUATION_RETURN_CODE] =
    MAKE_OBJECT (TC_RETURN_CODE, RC_INTERNAL_APPLY);
  Free += CONTINUATION_SIZE;
  *Free++ = STACK_FRAME_HEADER;
  *Free++ = (ARG_REF (1));
  Free[CONTINUATION_EXPRESSION] = (ARG_REF (1)); /* For testing & debugging */
  Free[CONTINUATION_RETURN_CODE] =
    MAKE_OBJECT (TC_RETURN_CODE, RC_END_OF_COMPUTATION);
  Free += CONTINUATION_SIZE;
  PRIMITIVE_RETURN (Result);
}

/*
  Absolutely the cheapest future we can make.  This includes
  the I/O stuff and whatnot.  Notice that the name is required.

  (make-cheap-future orig-code user-proc name)

*/

DEFINE_PRIMITIVE ("MAKE-CHEAP-FUTURE", Prim_make_cheap_future, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    fast SCHEME_OBJECT future = (allocate_marked_vector (TC_FUTURE, 10, true));
    FAST_MEMORY_SET (future, FUTURE_IS_DETERMINED, SHARP_F);
    FAST_MEMORY_SET (future, FUTURE_LOCK, SHARP_F);
    FAST_MEMORY_SET (future, FUTURE_QUEUE, (cons (EMPTY_LIST, EMPTY_LIST)));
    FAST_MEMORY_SET (future, FUTURE_PROCESS, (ARG_REF (1)));
    FAST_MEMORY_SET (future, FUTURE_STATUS, SHARP_T);
    FAST_MEMORY_SET (future, FUTURE_ORIG_CODE, (ARG_REF (2)));
    /* Put the I/O system stuff here. */
    FAST_MEMORY_SET
      (future,
       FUTURE_PRIVATE,
       (make_vector
	(1,
	 (hunk3_cons
	  (SHARP_F,
	   (ARG_REF (3)),
	   (cons ((LONG_TO_UNSIGNED_FIXNUM (0)),
		  (char_pointer_to_string ("")))))),
	 true)));
    FAST_MEMORY_SET (future, FUTURE_WAITING_ON, EMPTY_LIST);
    FAST_MEMORY_SET (future, FUTURE_METERING, (New_Future_Number ()));
    FAST_MEMORY_SET (future, FUTURE_USER, SHARP_F);
    PRIMITIVE_RETURN (future);
  }
}
