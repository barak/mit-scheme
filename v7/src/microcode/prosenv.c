/* -*-C-*-

$Id: prosenv.c,v 1.9 1993/07/01 22:29:58 cph Exp $

Copyright (c) 1987-1993 Massachusetts Institute of Technology

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

/* Process-environment primitives. */

#include "scheme.h"
#include "prims.h"
#include "osenv.h"
#include "ostop.h"

/* This primitive is obsolete.
   Left here for a while for compatibility purposes (booting old bands).
 */

DEFINE_PRIMITIVE ("GET-DECODED-TIME", Prim_get_decoded_time, 1, 1,
  "Return a vector with the current decoded time;\n\
arg TAG is used to tag the vector.\n\
The vector's elements are:\n\
  #(TAG second minute hour day month year day-of-week)")
{
  time_t t;
  struct time_structure ts;
  PRIMITIVE_HEADER (1);

  t = (OS_encoded_time ());
  OS_decode_time (t, &ts);
  {
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 8, 1));
    FAST_VECTOR_SET (result, 0, (ARG_REF (1)));
    FAST_VECTOR_SET (result, 1, (long_to_integer (ts . second)));
    FAST_VECTOR_SET (result, 2, (long_to_integer (ts . minute)));
    FAST_VECTOR_SET (result, 3, (long_to_integer (ts . hour)));
    FAST_VECTOR_SET (result, 4, (long_to_integer (ts . day)));
    FAST_VECTOR_SET (result, 5, (long_to_integer (ts . month)));
    FAST_VECTOR_SET (result, 6, (long_to_integer (ts . year)));
    FAST_VECTOR_SET (result, 7, (long_to_integer (ts . day_of_week)));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("ENCODED-TIME", Prim_encoded_time, 0, 0,
  "Return the current time as an integer.")
{
  PRIMITIVE_RETURN (long_to_integer ((long) (OS_encoded_time ())));
}

DEFINE_PRIMITIVE ("DECODE-TIME", Prim_decode_time, 2, 2,
  "Fill a vector with the second argument decoded.\n\
The vector's elements are:\n\
  #(TAG second minute hour day month year day-of-week)")
{
  SCHEME_OBJECT vec;
  struct time_structure ts;
  PRIMITIVE_HEADER (1);

  vec = (VECTOR_ARG (1));
  if ((VECTOR_LENGTH (vec)) != 8)
    error_bad_range_arg (1);
  OS_decode_time (((time_t) (arg_integer (2))), &ts);
  FAST_VECTOR_SET (vec, 1, (long_to_integer (ts . second)));
  FAST_VECTOR_SET (vec, 2, (long_to_integer (ts . minute)));
  FAST_VECTOR_SET (vec, 3, (long_to_integer (ts . hour)));
  FAST_VECTOR_SET (vec, 4, (long_to_integer (ts . day)));
  FAST_VECTOR_SET (vec, 5, (long_to_integer (ts . month)));
  FAST_VECTOR_SET (vec, 6, (long_to_integer (ts . year)));
  FAST_VECTOR_SET (vec, 7, (long_to_integer (ts . day_of_week)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("ENCODE-TIME", Prim_encode_time, 1, 1,
  "Return the file time corresponding to the time structure given.")
{
  SCHEME_OBJECT vec;
  struct time_structure ts;
  PRIMITIVE_HEADER (1);

  vec = (VECTOR_ARG (1));
  if ((VECTOR_LENGTH (vec)) != 8)
    error_bad_range_arg (1);

  ts.second = (integer_to_long (FAST_VECTOR_REF (vec, 1)));
  ts.minute = (integer_to_long (FAST_VECTOR_REF (vec, 2)));
  ts.hour = (integer_to_long (FAST_VECTOR_REF (vec, 3)));
  ts.day = (integer_to_long (FAST_VECTOR_REF (vec, 4)));
  ts.month = (integer_to_long (FAST_VECTOR_REF (vec, 5)));
  ts.year = (integer_to_long (FAST_VECTOR_REF (vec, 6)));
  ts.day_of_week = (integer_to_long (FAST_VECTOR_REF (vec, 7)));
  PRIMITIVE_RETURN (long_to_integer ((long) (OS_encode_time (&ts))));
}

DEFINE_PRIMITIVE ("SYSTEM-CLOCK", Prim_system_clock, 0, 0,
  "Return the current process time in units of milliseconds.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (double_to_integer (OS_process_clock ()));
}

DEFINE_PRIMITIVE ("REAL-TIME-CLOCK", Prim_real_time_clock, 0, 0,
  "Return the current real time in units of milliseconds.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (double_to_integer (OS_real_time_clock ()));
}

DEFINE_PRIMITIVE ("PROCESS-TIMER-CLEAR", Prim_process_timer_clear, 0, 0,
  "Turn off the process timer.")
{
  PRIMITIVE_HEADER (0);
  OS_process_timer_clear ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PROCESS-TIMER-SET", Prim_process_timer_set, 2, 2,
  "Set the process timer.\n\
First arg FIRST says how long to wait until the first interrupt;\n\
second arg INTERVAL says how long to wait between interrupts after that.\n\
Both arguments are in units of milliseconds.")
{
  PRIMITIVE_HEADER (2);
  OS_process_timer_set ((arg_nonnegative_integer (1)),
			(arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("REAL-TIMER-CLEAR", Prim_real_timer_clear, 0, 0,
  "Turn off the real timer.")
{
  PRIMITIVE_HEADER (0);
  OS_real_timer_clear ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("REAL-TIMER-SET", Prim_real_timer_set, 2, 2,
  "Set the real timer.\n\
First arg FIRST says how long to wait until the first interrupt;\n\
second arg INTERVAL says how long to wait between interrupts after that.\n\
Both arguments are in units of milliseconds.")
{
  PRIMITIVE_HEADER (2);
  OS_real_timer_set ((arg_nonnegative_integer (1)),
		     (arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SETUP-TIMER-INTERRUPT", Prim_setup_timer_interrupt, 2, 2,
  "This is an obsolete primitive; use `process-timer-set' instead.")
{
  PRIMITIVE_HEADER (2);
  if (((ARG_REF (1)) == SHARP_F) && ((ARG_REF (2)) == SHARP_F))
    OS_process_timer_clear ();
  else
    {
      unsigned long days = (arg_nonnegative_integer (1));
      unsigned long centisec = (arg_nonnegative_integer (2));
      OS_process_timer_set
	((((days * 24 * 60 * 60 * 100) + centisec) * 10), 0);
    }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WORKING-DIRECTORY-PATHNAME", Prim_working_dir_pathname, 0, 0,
  "Return the current working directory as a string.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string
		    ((unsigned char *) OS_working_dir_pathname ()));
}

DEFINE_PRIMITIVE ("SET-WORKING-DIRECTORY-PATHNAME!", Prim_set_working_dir_pathname, 1, 1,
  "Change the current working directory to NAME.")
{
  PRIMITIVE_HEADER (1);
  OS_set_working_dir_pathname (STRING_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SYSTEM-CALL-ERROR-MESSAGE", Prim_system_call_error_message, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    CONST char * message =
      (OS_error_code_to_message (arg_nonnegative_integer (1)));
    PRIMITIVE_RETURN
      ((message == 0) ? SHARP_F
       : (char_pointer_to_string ((unsigned char *) message)));
  }
}
