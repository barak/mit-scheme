/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prosenv.c,v 1.1 1990/06/20 19:38:17 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

DEFINE_PRIMITIVE ("GET-DECODED-TIME", Prim_get_decoded_time, 1, 1,
  "Return a vector with the current decoded time;\n\
arg TAG is used to tag the vector.\n\
The vector's elements are:\n\
  #(TAG second minute hour day month year day-of-week)")
{
  struct time_structure ts;
  PRIMITIVE_HEADER (1);
  OS_current_time (&ts);
  {
    SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 8, 1));
    FAST_VECTOR_SET (result, 0, (ARG_REF (1)));
    FAST_VECTOR_SET (result, 1, (ts . second));
    FAST_VECTOR_SET (result, 2, (ts . minute));
    FAST_VECTOR_SET (result, 3, (ts . hour));
    FAST_VECTOR_SET (result, 4, (ts . day));
    FAST_VECTOR_SET (result, 5, (ts . month));
    FAST_VECTOR_SET (result, 6, (ts . year));
    FAST_VECTOR_SET (result, 6, (ts . day_of_week));
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("CURRENT-YEAR", Prim_current_year, 0, 0,
  "This is an obsolete primitive; use `get-decoded-time' instead.")
{
  struct time_structure ts;
  PRIMITIVE_HEADER (0);
  OS_current_time (&ts);
  PRIMITIVE_RETURN (long_to_integer ((ts . year) - 1900));
}

#define DATE_PRIMITIVE(element)						\
{									\
  struct time_structure ts;						\
  PRIMITIVE_HEADER (0);							\
  OS_current_time (&ts);						\
  PRIMITIVE_RETURN (long_to_integer (ts . element));			\
}

DEFINE_PRIMITIVE ("CURRENT-MONTH", Prim_current_month, 0, 0,
  "This is an obsolete primitive; use `get-decoded-time' instead.")
     DATE_PRIMITIVE (month)

DEFINE_PRIMITIVE ("CURRENT-DAY", Prim_current_day, 0, 0,
  "This is an obsolete primitive; use `get-decoded-time' instead.")
     DATE_PRIMITIVE (day)

DEFINE_PRIMITIVE ("CURRENT-HOUR", Prim_current_hour, 0, 0,
  "This is an obsolete primitive; use `get-decoded-time' instead.")
     DATE_PRIMITIVE (hour)

DEFINE_PRIMITIVE ("CURRENT-MINUTE", Prim_current_minute, 0, 0,
  "This is an obsolete primitive; use `get-decoded-time' instead.")
     DATE_PRIMITIVE (minute)

DEFINE_PRIMITIVE ("CURRENT-SECOND", Prim_current_second, 0, 0,
  "This is an obsolete primitive; use `get-decoded-time' instead.")
     DATE_PRIMITIVE (second)

DEFINE_PRIMITIVE ("SYSTEM-CLOCK", Prim_system_clock, 0, 0,
  "Return the current process time in units of milliseconds.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS_process_clock ()));
}

DEFINE_PRIMITIVE ("REAL-TIME-CLOCK", Prim_real_time_clock, 0, 0,
  "Return the current real time in units of milliseconds.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS_real_time_clock ()));
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
  PRIMITIVE_RETURN (char_pointer_to_string (OS_working_dir_pathname ()));
}

DEFINE_PRIMITIVE ("SET-WORKING-DIRECTORY-PATHNAME!", Prim_set_working_dir_pathname, 1, 1,
  "Change the current working directory to NAME.")
{
  PRIMITIVE_HEADER (1);
  OS_set_working_dir_pathname (STRING_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("GET-ENVIRONMENT-VARIABLE", Prim_get_environment_variable, 1, 1,
  "Look up the value of a variable in the user's shell environment.\n\
The argument, a variable name, must be a string.\n\
The result is either a string (the variable's value),\n\
 or #F indicating that the variable does not exist.")
{
  PRIMITIVE_HEADER (1);
  {
    CONST char * variable_value =
      (OS_get_environment_variable (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((variable_value == 0)
       ? SHARP_F
       : (char_pointer_to_string (variable_value)));
  }
}

DEFINE_PRIMITIVE ("CURRENT-USER-NAME", Prim_current_user_name, 0, 0,
  "Return (as a string) the user name of the user running Scheme.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string (OS_current_user_name ()));
}

DEFINE_PRIMITIVE ("CURRENT-USER-HOME-DIRECTORY", Prim_current_user_home_directory, 0, 0,
  "Return the name of the current user's home directory.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    (char_pointer_to_string (OS_current_user_home_directory ()));
}
