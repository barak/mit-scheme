/* -*-C-*-

$Id: futures.h,v 9.32 2003/02/14 18:28:19 cph Exp $

Copyright (c) 1987-1990, 1999, 2002 Massachusetts Institute of Technology

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

/* This file contains macros useful for dealing with futures */

/* NOTES ON FUTURES, derived from the rest of the interpreter code

   ASSUMPTION: The syntaxer is hereby assumed NEVER to generate primitive
   combinations unless the primitive itself is output in the code stream.
   Therefore, we don't have to explicitly check here that the expression
   register has a primitive in it.

   ASSUMPTION: The SYMBOL slot of a VARIABLE does NOT contain a future, nor
   do the cached lexical address slots.

   ASSUMPTION: Environment structure, which is created only by the
   interpreter, never contains FUTUREs on its spine.

   ASSUMPTION: History objects are never created using futures.

   ASSUMPTION: State points, which are created only by the interpreter,
   never contain FUTUREs except possibly as the thunks (which are handled
   by the apply code).

   OPTIMIZATIONS (?):
   After a lot of discussion, we decided that variable reference will check
   whether a value stored in the environment is a determined future which
   is marked spliceable.  If so, it will splice out the future from the
   environment slot to speed up subsequent references.

   EQ? does a normal identity check and only if this fails does it touch the
   arguments.  The same thing does not speed up MEMQ or ASSQ in the normal
   case, so it is omitted there.

   The following are NOT done, but might be useful later
   (1) Splicing on SET! operations
   (2) Splicing at apply and/or primitive apply
   (3) Splicing all arguments when a primitive errors on any of them
   (4) Splicing within the Arg_n_Type macro rather than after longjmping
       to the error handler.

   KNOWN PROBLEMS:
   (1) Garbage collector should be modified to splice out futures.  DONE.

   (2) Purify should be looked at and we should decide what to do about
       purifying an object with a reference to a future (it should probably
       become constant but not pure).

   (3) Look at Impurify and Side-Effect-Impurify to see if futures
       affect them in any way. */

/* Data structure definition */

/* The IS_DETERMINED slot has one of the following type of values:
    #F	if the value is not yet known;
    #T	if the value is known and the garbage collector is free
  	to remove the future object in favor of its value everywhere;
   else	the value is known, but the GC must leave the future object. */

#define FUTURE_VECTOR_HEADER	0
#define FUTURE_IS_DETERMINED	1
#define FUTURE_LOCK             2
#define FUTURE_VALUE		3	/* if known, else */
#define FUTURE_QUEUE		3	/* tasks waiting for value */
#define FUTURE_PROCESS		4
#define FUTURE_STATUS		5
#define FUTURE_ORIG_CODE	6
#define FUTURE_PRIVATE		7
#define FUTURE_WAITING_ON	8
#define FUTURE_METERING		9
#define FUTURE_USER		10

#define Future_Is_Locked(P)						\
  ((MEMORY_REF ((P), FUTURE_LOCK)) != SHARP_F)

#define Future_Has_Value(P)						\
  ((MEMORY_REF ((P), FUTURE_IS_DETERMINED)) != SHARP_F)

#define Future_Value(P)							\
  (MEMORY_REF ((P), FUTURE_VALUE))

#define Future_Spliceable(P)						\
  (((MEMORY_REF ((P), FUTURE_IS_DETERMINED)) == SHARP_T) &&		\
   ((MEMORY_REF ((P), FUTURE_LOCK)) == SHARP_F))

#define Future_Is_Keep_Slot(P)						\
  (! (BOOLEAN_P (MEMORY_REF ((P), FUTURE_IS_DETERMINED))))

#ifndef COMPILE_FUTURES

#define TOUCH_IN_PRIMITIVE(P, To_Where) To_Where = (P)
#define TOUCH_SETUP(object) Microcode_Termination (TERM_TOUCH)
#define Log_Touch_Of_Future(F) {}
#define Call_Future_Logging()
#define Must_Report_References() (false)
#define FUTURE_VARIABLE_SPLICE(P, Offset, Value)

#else /* COMPILE_FUTURES */

/* TOUCH_IN_PRIMITIVE is used by primitives which are not
   strict in an argument but which touch it none the less. */

#define TOUCH_IN_PRIMITIVE(P, To_Where)					\
{									\
  SCHEME_OBJECT Value = (P);						\
  while (FUTURE_P (Value))						\
    {									\
      if (Future_Has_Value (Value))					\
	{								\
	  if (Future_Is_Keep_Slot (Value))				\
	    {								\
	      Log_Touch_Of_Future (Value);				\
	    }								\
	  Value = (Future_Value (Value));				\
	}								\
      else								\
	{								\
	  val_register = Value;						\
	  PRIMITIVE_ABORT (PRIM_TOUCH);					\
	}								\
    }									\
  (To_Where) = Value;							\
}

#define TOUCH_SETUP(object)						\
{									\
  Save_Cont ();								\
 Will_Push (STACK_ENV_EXTRA_SLOTS + 2);					\
  STACK_PUSH (object);							\
  STACK_PUSH (Get_Fixed_Obj_Slot (System_Scheduler));			\
  STACK_PUSH (STACK_FRAME_HEADER + 1);					\
 Pushed ();								\
}

#define FUTURE_VARIABLE_SPLICE(P, Offset, Value)			\
{									\
  while ((FUTURE_P (Value)) && (Future_Spliceable (Value)))		\
    {									\
      Value = (Future_Value (Value));					\
      MEMORY_SET (P, Offset, Value);					\
    }									\
}

#ifdef FUTURE_LOGGING

#define Touched_Futures_Vector() (Get_Fixed_Obj_Slot (Touched_Futures))

#define Logging_On()							\
  ((Valid_Fixed_Obj_Vector ()) && (Touched_Futures_Vector ()))

/* Log_Touch_Of_Future adds the future which was touched to the vector
   of touched futures about which the scheme portion of the system has
   not yet been informed. */

#define Log_Touch_Of_Future(F)						\
{									\
  if (Logging_On ())							\
    {									\
      SCHEME_OBJECT TFV = (Touched_Futures_Vector ());			\
      long Count =							\
	((UNSIGNED_FIXNUM_TO_LONG (VECTOR_REF (TFV, 0))) + 1);		\
      (VECTOR_REF (TFV, 0)) = (LONG_TO_UNSIGNED_FIXNUM (Count));	\
      if (Count < (VECTOR_LENGTH (TFV)))				\
	(VECTOR_REF (TFV, Count)) = (OBJECT_NEW_TYPE (TC_VECTOR, F));	\
    }									\
}

/* Call_Future_Logging calls a user defined scheme routine if the vector
   of touched futures has a nonzero length. */

#define Must_Report_References()					\
  ((Logging_On ()) &&							\
   ((UNSIGNED_FIXNUM_TO_LONG						\
     (VECTOR_REF ((Touched_Futures_Vector ()), 0)))			\
    > 0))

#define Call_Future_Logging()						\
{									\
 Will_Push (STACK_ENV_EXTRA_SLOTS + 2);					\
  STACK_PUSH (Touched_Futures_Vector ());				\
  STACK_PUSH (Get_Fixed_Obj_Slot (Future_Logger));			\
  STACK_PUSH (STACK_FRAME_HEADER + 1);					\
 Pushed ();								\
  (Touched_Futures_Vector ()) = SHARP_F;				\
  goto Apply_Non_Trapping;						\
}

#else /* not FUTURE_LOGGING */

#define Log_Touch_Of_Future(F) {}
#define Call_Future_Logging()
#define Must_Report_References() (false)

#endif /* FUTURE_LOGGING */
#endif /* COMPILE_FUTURES */
