/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prosproc.c,v 1.1 1990/06/20 19:38:30 cph Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

/* Primitives for subprocess control. */

#include "scheme.h"
#include "prims.h"
#include "osproc.h"

static int EXFUN (string_vector_p, (SCHEME_OBJECT vector));
static char ** EXFUN (convert_string_vector, (SCHEME_OBJECT vector));

static Tprocess
DEFUN (arg_process, (argument_number), int argument_number)
{
  Tprocess process =
    (arg_index_integer (argument_number, OS_process_table_size));
  switch (OS_process_status (process))
    {
    case process_status_exited:
    case process_status_signalled:
    case process_status_running:
    case process_status_stopped:
      break;
    default:
      error_bad_range_arg (1);
      break;
    }
  return (process);
}

DEFINE_PRIMITIVE ("MAKE-SUBPROCESS", Prim_make_subprocess, 4, 4,
  "Create a subprocess.\n\
First arg FILENAME is the program to run.\n\
Second arg ARGV is a vector of strings to pass to the program as arguments.\n\
Third arg ENV is a vector of strings to pass as the program's environment.\n\
Fourth arg CTTY-TYPE specifies the program's controlling terminal type:\n\
  0 => none; 1 => inherited; 2 => pipe; 3 => PTY.")
{
  PRIMITIVE_HEADER (4);
  CHECK_ARG (2, string_vector_p);
  CHECK_ARG (3, string_vector_p);
  {
    PTR position = dstack_position;
    CONST char * filename = (STRING_ARG (1));
    CONST char ** argv =
      ((CONST char **) (convert_string_vector (ARG_REF (2))));
    char ** env = (convert_string_vector (ARG_REF (3)));
    enum process_ctty_type ctty_type;
    Tprocess process;
    switch (arg_index_integer (4, 4))
      {
      case 0: ctty_type = ctty_type_none; break;
      case 1: ctty_type = ctty_type_inherited; break;
      case 2: ctty_type = ctty_type_pipe; break;
      case 3: ctty_type = ctty_type_pty; break;
      }
    process = (OS_make_subprocess (filename, argv, env, ctty_type));
    dstack_set_position (position);
    PRIMITIVE_RETURN (long_to_integer (process));
  }
}

static int
DEFUN (string_vector_p, (vector), SCHEME_OBJECT vector)
{
  if (! (VECTOR_P (vector)))
    return (0);
  {
    unsigned long length = (VECTOR_LENGTH (vector));
    SCHEME_OBJECT * scan = (VECTOR_LOC (vector, 0));
    SCHEME_OBJECT * end = (scan + length);
    while (scan < end)
      if (! (STRING_P (*scan++)))
	return (0);
  }
  return (1);
}

static char **
DEFUN (convert_string_vector, (vector), SCHEME_OBJECT vector)
{
  unsigned long length = (VECTOR_LENGTH (vector));
  char ** result = (dstack_alloc (length * (sizeof (char *))));
  SCHEME_OBJECT * scan = (VECTOR_LOC (vector, 0));
  SCHEME_OBJECT * end = (scan + length);
  char ** scan_result = result;
  while (scan < end)
    (*scan_result++) = ((char *) (STRING_LOC ((*scan++), 0)));
  return (result);
}

DEFINE_PRIMITIVE ("PROCESS-DELETE", Prim_process_delete, 1, 1,
  "Delete process PROCESS-NUMBER from the process table.\n\
The process may be deleted only if it is exited or stopped.")
{
  PRIMITIVE_HEADER (1);
  {
    Tprocess process = (arg_index_integer (1, OS_process_table_size));
    switch (OS_process_status (process))
      {
      case process_status_free:
	break;
      case process_status_allocated:
      case process_status_exited:
      case process_status_signalled:
	OS_process_deallocate (process);
	break;
      case process_status_running:
      case process_status_stopped:
	error_bad_range_arg (1);
	break;
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PROCESS-TABLE", Prim_process_table, 0, 0,
  "Return a vector of all processes in the process table.")
{
  PRIMITIVE_HEADER (0);
  {
    Tprocess process;
    for (process = 0; (process < OS_process_table_size); process += 1)
      if ((OS_process_status (process)) != process_status_free)
	obstack_grow ((&scratch_obstack), (&process), (sizeof (Tprocess)));
  }
  {
    unsigned int n_processes =
      ((obstack_object_size ((&scratch_obstack))) / (sizeof (Tprocess)));
    if (n_processes == 0)
      PRIMITIVE_RETURN (SHARP_F);
    {
      Tprocess * processes = (obstack_finish (&scratch_obstack));
      Tprocess * scan_processes = processes;
      SCHEME_OBJECT vector =
	(allocate_marked_vector (TC_VECTOR, n_processes, 1));
      SCHEME_OBJECT * scan_vector = (VECTOR_LOC (vector, 0));
      SCHEME_OBJECT * end_vector = (scan_vector + n_processes);
      while (scan_vector < end_vector)
	(*scan_vector++) = (long_to_integer (*scan_processes++));
      obstack_free ((&scratch_obstack), processes);
      PRIMITIVE_RETURN (vector);
    }
  }
}

DEFINE_PRIMITIVE ("PROCESS-ID", Prim_process_id, 1, 1, 
  "Return the process ID of process PROCESS-NUMBER.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (OS_process_id (arg_process (1))));
}

DEFINE_PRIMITIVE ("PROCESS-INPUT", Prim_process_input, 1, 1, 
  "Return the input channel number of process PROCESS-NUMBER.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (OS_process_input (arg_process (1))));
}

DEFINE_PRIMITIVE ("PROCESS-OUTPUT", Prim_process_output, 1, 1, 
  "Return the output channel number of process PROCESS-NUMBER.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (OS_process_output (arg_process (1))));
}

DEFINE_PRIMITIVE ("PROCESS-SYNCHRONOUS?", Prim_process_synchronous_p, 1, 1, 
  "Return #F iff process PROCESS-NUMBER is not synchronous.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (OS_process_synchronous (arg_process (1))));
}

DEFINE_PRIMITIVE ("PROCESS-CTTY-TYPE", Prim_process_ctty_type, 1, 1,
  "Return the controlling terminal type of process PROCESS-NUMBER.\n\
This is a nonnegative integer:\n\
  0 = none; 1 = inherited; 2 = pipe; 3 = PTY.")
{
  PRIMITIVE_HEADER (1);
  switch (OS_process_ctty_type (arg_process (1)))
    {
    case ctty_type_none:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
    case ctty_type_inherited:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
    case ctty_type_pipe:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (2));
    case ctty_type_pty:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (3));
    default:
      error_bad_range_arg (1);
    }
}

DEFINE_PRIMITIVE ("PROCESS-STATUS", Prim_process_status, 1, 1,
  "Return the status of process PROCESS-NUMBER.\n\
This is a nonnegative integer:\n\
  0 = running; 1 = stopped; 2 = exited; 3 = signalled; 4 = unstarted.")
{
  PRIMITIVE_HEADER (1);
  switch (OS_process_status (arg_index_integer (1, OS_process_table_size)))
    {
    case process_status_running:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
    case process_status_stopped:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
    case process_status_exited:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (2));
    case process_status_signalled:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (3));
    case process_status_allocated:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (4));
    default:
      error_bad_range_arg (1);
    }
}

DEFINE_PRIMITIVE ("PROCESS-REASON", Prim_process_reason, 1, 1, 
  "Return the termination reason of process PROCESS-NUMBER.\n\
It is an error if the process is running.\n\
This is a nonnegative integer, which depends on the process's status:\n\
  stopped => the signal that stopped the process;\n\
  exited => the exit code returned by the process;\n\
  signalled => the signal that killed the process.")
{
  PRIMITIVE_HEADER (1);
  {
    Tprocess process = (arg_process (1));
    if ((OS_process_status (process)) == process_status_running)
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (long_to_integer (OS_process_reason (process)));
  }
}

DEFINE_PRIMITIVE ("PROCESS-SIGNAL", Prim_process_signal, 2, 2,
  "Send a signal to process PROCESS-NUMBER; second arg SIGNAL says which one.")
{
  PRIMITIVE_HEADER (2);
  OS_process_send_signal ((arg_process (1)), (arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define PROCESS_SIGNALLING_PRIMITIVE(signaller)				\
{									\
  PRIMITIVE_HEADER (1);							\
  signaller (arg_process (1));						\
  PRIMITIVE_RETURN (UNSPECIFIC);					\
}

DEFINE_PRIMITIVE ("PROCESS-KILL", Prim_process_kill, 1, 1,
  "Kill process PROCESS-NUMBER (in unix: signal SIGKILL).")
     PROCESS_SIGNALLING_PRIMITIVE (OS_process_kill)

DEFINE_PRIMITIVE ("PROCESS-INTERRUPT", Prim_process_interrupt, 1, 1,
  "Interrupt process PROCESS-NUMBER (in unix: signal SIGINT).")
     PROCESS_SIGNALLING_PRIMITIVE (OS_process_interrupt)

DEFINE_PRIMITIVE ("PROCESS-QUIT", Prim_process_quit, 1, 1,
  "Quit process PROCESS-NUMBER (in unix: signal SIGQUIT).")
     PROCESS_SIGNALLING_PRIMITIVE (OS_process_quit)

DEFINE_PRIMITIVE ("PROCESS-STOP", Prim_process_stop, 1, 1,
  "Stop process PROCESS-NUMBER (in unix: signal SIGTSTP).")
     PROCESS_SIGNALLING_PRIMITIVE (OS_process_stop)

DEFINE_PRIMITIVE ("PROCESS-CONTINUE", Prim_process_continue, 1, 1,
  "Continue process PROCESS-NUMBER (in unix: signal SIGCONT).")
     PROCESS_SIGNALLING_PRIMITIVE (OS_process_continue)
