/* -*-C-*-

$Id: prosenv.c,v 1.19 2003/02/14 18:28:23 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* Process-environment primitives. */

#include "scheme.h"
#include "prims.h"
#include "osenv.h"
#include "ostop.h"
#include "limits.h"

DEFINE_PRIMITIVE ("ENCODED-TIME", Prim_encoded_time, 0, 0,
  "Return the current time as an integer.")
{
  PRIMITIVE_RETURN (ulong_to_integer ((unsigned long) (OS_encoded_time ())));
}

#define DECODE_TIME_BODY(proc)						\
{									\
  PRIMITIVE_HEADER (2);							\
  {									\
    SCHEME_OBJECT vec = (VECTOR_ARG (1));				\
    unsigned int len = (VECTOR_LENGTH (vec));				\
    struct time_structure ts;						\
    if (! (len >= 10))							\
      error_bad_range_arg (1);						\
    proc (((time_t) (arg_ulong_integer (2))), &ts);			\
    FAST_VECTOR_SET (vec, 1, (ulong_to_integer (ts . second)));		\
    FAST_VECTOR_SET (vec, 2, (ulong_to_integer (ts . minute)));		\
    FAST_VECTOR_SET (vec, 3, (ulong_to_integer (ts . hour)));		\
    FAST_VECTOR_SET (vec, 4, (ulong_to_integer (ts . day)));		\
    FAST_VECTOR_SET (vec, 5, (ulong_to_integer (ts . month)));		\
    FAST_VECTOR_SET (vec, 6, (ulong_to_integer (ts . year)));		\
    FAST_VECTOR_SET (vec, 7, (ulong_to_integer (ts . day_of_week)));	\
    FAST_VECTOR_SET							\
      (vec, 8,								\
       (((ts . daylight_savings_time) < 0)				\
	? SHARP_F							\
	: (long_to_integer (ts . daylight_savings_time))));		\
    FAST_VECTOR_SET							\
      (vec, 9,								\
       (((ts . time_zone) == INT_MAX)					\
	? SHARP_F							\
	: (long_to_integer (ts . time_zone))));				\
  }									\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

DEFINE_PRIMITIVE ("DECODE-TIME", Prim_decode_time, 2, 2,
  "Fill a vector with the second argument decoded.\n\
The vector's elements are:\n\
  #(TAG second minute hour day month year day-of-week dst zone)")
DECODE_TIME_BODY (OS_decode_time)

DEFINE_PRIMITIVE ("DECODE-UTC", Prim_decode_utc, 2, 2,
  "Fill a vector with the second argument decoded.\n\
The vector's elements are:\n\
  #(TAG second minute hour day month year day-of-week dst zone)")
DECODE_TIME_BODY (OS_decode_utc)

DEFINE_PRIMITIVE ("ENCODE-TIME", Prim_encode_time, 1, 1,
  "Return the file time corresponding to the time structure given.")
{
  SCHEME_OBJECT vec;
  unsigned int len;
  struct time_structure ts;
  PRIMITIVE_HEADER (1);

  vec = (VECTOR_ARG (1));
  len = (VECTOR_LENGTH (vec));
  if (! (len >= 8))
    error_bad_range_arg (1);
  (ts . second) = (integer_to_ulong (FAST_VECTOR_REF (vec, 1)));
  (ts . minute) = (integer_to_ulong (FAST_VECTOR_REF (vec, 2)));
  (ts . hour) = (integer_to_ulong (FAST_VECTOR_REF (vec, 3)));
  (ts . day) = (integer_to_ulong (FAST_VECTOR_REF (vec, 4)));
  (ts . month) = (integer_to_ulong (FAST_VECTOR_REF (vec, 5)));
  (ts . year) = (integer_to_ulong (FAST_VECTOR_REF (vec, 6)));
  (ts . day_of_week) = (integer_to_ulong (FAST_VECTOR_REF (vec, 7)));
  (ts . daylight_savings_time)
    = (((len > 8) && (INTEGER_P (FAST_VECTOR_REF (vec, 8))))
       ? (integer_to_long (FAST_VECTOR_REF (vec, 8)))
       : (-1));
  (ts . time_zone)
    = (((len > 9)
	&& (INTEGER_P (FAST_VECTOR_REF (vec, 9)))
	&& (integer_to_ulong_p (FAST_VECTOR_REF (vec, 9))))
       ? (integer_to_ulong (FAST_VECTOR_REF (vec, 9)))
       : INT_MAX);
  PRIMITIVE_RETURN (ulong_to_integer ((unsigned long) (OS_encode_time (&ts))));
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

DEFINE_PRIMITIVE ("PROFILE-TIMER-CLEAR", Prim_profile_timer_clear, 0, 0,
  "Turn off the profile timer.")
{
  PRIMITIVE_HEADER (0);
  OS_profile_timer_clear ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PROFILE-TIMER-SET", Prim_profile_timer_set, 2, 2,
  "Set the profile timer.\n\
First arg FIRST says how long to wait until the first interrupt;\n\
second arg INTERVAL says how long to wait between interrupts after that.\n\
Both arguments are in units of milliseconds.")
{
  PRIMITIVE_HEADER (2);
  OS_profile_timer_set ((arg_nonnegative_integer (1)),
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
