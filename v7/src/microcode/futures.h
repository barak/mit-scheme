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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/futures.h,v 9.23 1987/12/04 22:16:33 jinx Rel $
 *
 * This file contains macros useful for dealing with futures
 */

/* Data structure definition */

/* The IS_DETERMINED slot has one of the following type of values:
 *    #!FALSE if the value is not yet known
 *    #!TRUE  if the value is known and the garbage collector is free
 *            to remove the future object in favor of its value everywhere
 *    else    the value is known, but the GC must leave the future object
*/

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

#define Future_Is_Locked(P)     				\
	(Vector_Ref((P), FUTURE_LOCK) != NIL)

#define Future_Has_Value(P)					\
	(Vector_Ref((P), FUTURE_IS_DETERMINED) != NIL)

#define Future_Value(P)						\
	Vector_Ref((P), FUTURE_VALUE)

#define Future_Spliceable(P)					\
	((Vector_Ref((P), FUTURE_IS_DETERMINED) == TRUTH) &&	\
	 (Vector_Ref((P), FUTURE_LOCK) == NIL))

#define Future_Is_Keep_Slot(P)  				\
((Vector_Ref((P), FUTURE_IS_DETERMINED) != NIL)	&&		\
 (Vector_Ref((P), FUTURE_IS_DETERMINED) != TRUTH))

#ifdef COMPILE_FUTURES

/* Touch_In_Primitive is used by primitives which are not
 * strict in an argument but which touch it none the less.
 */

#define Touch_In_Primitive(P, To_Where)					\
{									\
  Pointer Value;							\
									\
  Value = (P);								\
  while (OBJECT_TYPE(Value) == TC_FUTURE)				\
  {									\
    if (Future_Has_Value(Value))					\
    {									\
      if (Future_Is_Keep_Slot(Value))					\
      {									\
	Log_Touch_Of_Future(Value);					\
      }									\
      Value = Future_Value(Value);					\
    }									\
    else								\
    {									\
      Val = Value;							\
      PRIMITIVE_ABORT(PRIM_TOUCH);					\
    }									\
  }									\
  To_Where = Value;							\
}

#define TOUCH_SETUP(object)						\
{									\
   Save_Cont();								\
  Will_Push(STACK_ENV_EXTRA_SLOTS + 2);					\
   Push(object);							\
   Push(Get_Fixed_Obj_Slot(System_Scheduler));				\
   Push(STACK_FRAME_HEADER + 1);					\
  Pushed();								\
}

/* NOTES ON FUTURES, derived from the rest of the interpreter code */

/* ASSUMPTION: The syntaxer is hereby assumed NEVER to generate primitive
   combinations unless the primitive itself is output in the code stream.
   Therefore, we don't have to explicitly check here that the expression
   register has a primitive in it.

   ASSUMPTION: The SYMBOL slot of a VARIABLE does NOT contain a future, nor
   do the cached lexical address slots.

   ASSUMPTION: History objects are never created using futures.

   ASSUMPTION: State points, which are created only by the interpreter,
   never contain FUTUREs except possibly as the thunks (which are handled
   by the apply code).

*/

/* OPTIMIZATIONS (?):
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
*/

/* KNOWN PROBLEMS:
   (1) Garbage collector should be modified to splice out futures.  DONE.

   (2) Purify should be looked at and we should decide what to do about
       purifying an object with a reference to a future (it should probably
       become constant but not pure).

   (3) Look at Impurify and Side-Effect-Impurify to see if futures
       affect them in any way.
*/

#ifdef FUTURE_LOGGING

#define Touched_Futures_Vector()  Get_Fixed_Obj_Slot(Touched_Futures)

#define Logging_On()							\
(Valid_Fixed_Obj_Vector() && Touched_Futures_Vector())

/* Log_Touch_Of_Future adds the future which was touched to the vector
   of touched futures about which the scheme portion of the system has
   not yet been informed
*/
#define Log_Touch_Of_Future(F)						\
if (Logging_On())							\
{									\
  Pointer TFV;								\
  long Count;								\
									\
  TFV = Touched_Futures_Vector();					\
  Count = Get_Integer(User_Vector_Ref(TFV, 0)) + 1;			\
  User_Vector_Ref(TFV, 0) = MAKE_UNSIGNED_FIXNUM(Count);		\
  if (Count < Vector_Length(TFV))					\
  {									\
    User_Vector_Ref(TFV, Count) = Make_New_Pointer(TC_VECTOR, F);	\
  }									\
}

/* Call_Future_Logging calls a user defined scheme routine if the vector
   of touched futures has a nonzero length.  
*/
#define Must_Report_References()					\
( (Logging_On()) &&							\
   (Get_Integer(User_Vector_Ref(Touched_Futures_Vector(), 0)) > 0))

#define Call_Future_Logging()                                   	\
{									\
 Will_Push(STACK_ENV_EXTRA_SLOTS + 2);			        	\
  Push(Touched_Futures_Vector());                      	        	\
  Push(Get_Fixed_Obj_Slot(Future_Logger));      			\
  Push(STACK_FRAME_HEADER + 1);			 	        	\
 Pushed();								\
  Touched_Futures_Vector() = NIL;                                 	\
  goto Apply_Non_Trapping;						\
}

#else /* not FUTURE_LOGGING */

#define Log_Touch_Of_Future(F) { }
#define Call_Future_Logging()
#define Must_Report_References() (false)

#endif /* FUTURE_LOGGING */

#define FUTURE_VARIABLE_SPLICE(P, Offset, Value)			\
{									\
  while ((OBJECT_TYPE(Value) == TC_FUTURE) && Future_Spliceable(Value))	\
  {									\
    Value = Future_Value(Value);					\
    Vector_Set(P, Offset, Value);					\
  }									\
}

#else /* not COMPILE_FUTURES */

#define Touch_In_Primitive(P, To_Where)		To_Where = (P)
#define TOUCH_SETUP(object)			Microcode_Termination(TERM_TOUCH)
#define Log_Touch_Of_Future(F) { }
#define Call_Future_Logging()
#define Must_Report_References() (false)
#define FUTURE_VARIABLE_SPLICE(P, Offset, Value)

#endif /* COMPILE_FUTURES */
