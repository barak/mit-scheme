/* -*-C-*-

$Id: prntio.c,v 1.9 1998/04/14 05:13:55 cph Exp $

Copyright (c) 1993-98 Massachusetts Institute of Technology

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

/* Primitives to do the NT equivalent of Unix select. */

#include <windows.h>
#include "scheme.h"
#include "prims.h"
#include "ntio.h"
#include "nt.h"
#include "ntscreen.h"
#include "ntgui.h"
#include "syscall.h"
#include "ntproc.h"
#include "ostty.h"

extern HANDLE master_tty_window;
extern Tchannel EXFUN (arg_to_channel, (SCHEME_OBJECT, int));

static Tchannel * object_to_channel_vector
  (SCHEME_OBJECT, int, unsigned long *, long *);
static long wait_for_multiple_objects (unsigned long, Tchannel *, long, int);
static long wait_for_multiple_objects_1 (unsigned long, Tchannel *, long, int);

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (arg_channel (1)));
}

DEFINE_PRIMITIVE ("NT:MSGWAITFORMULTIPLEOBJECTS", Prim_nt_msgwaitformultipleobjects, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("NT:WAITFORMULTIPLEOBJECTS", Prim_nt_waitformultipleobjects, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT channel_vector = (VECTOR_ARG (1));
    int blockp = (BOOLEAN_ARG (3));
    unsigned long nc;
    long console_index = (-1);
    Tchannel * channels;
    long result;

    if (BOOLEAN_ARG (2))
      error_bad_range_arg (2);
    transaction_begin ();
    channels
      = (object_to_channel_vector
	 (channel_vector, 1, (&nc), (&console_index)));
    result
      = (wait_for_multiple_objects (nc, channels, console_index, blockp));
    transaction_commit ();
    PRIMITIVE_RETURN (long_to_integer (result));
  }
}

static Tchannel *
object_to_channel_vector (SCHEME_OBJECT channel_vector,
			  int argno,
			  unsigned long * ncp,
			  long * console_index)
{
  unsigned int index = 0;
  Tchannel tty_input_channel = (OS_tty_input_channel ());
  unsigned long nc = (VECTOR_LENGTH (channel_vector));
  Tchannel * channels
    = ((nc == 0) ? 0 : (dstack_alloc (nc * (sizeof (Tchannel)))));
  while (index < nc)
    {
      Tchannel channel
	= (arg_to_channel ((VECTOR_REF (channel_vector, (index))), argno));
      if (channel == tty_input_channel)
	{
	  (*console_index) = index;
	  (channels[index]) = NO_CHANNEL;
	}
      else
	(channels[index]) = channel;
      index += 1;
    }
  (*ncp) = nc;
  return (channels);
}

static long
wait_for_multiple_objects (unsigned long n_channels, Tchannel * channels,
			   long console_index, int blockp)
{
#ifdef TRACE_SCREEN_MSGS
  fprintf
    (trace_file,
     "wait_for_multiple_objects: n_channels=%d console_index=%d blockp=%d\n",
     n_channels, console_index, blockp);
  fflush (trace_file);
  {
    long result
      = (wait_for_multiple_objects_1
	 (n_channels, channels, console_index, blockp));
    fprintf (trace_file, "wait_for_multiple_objects: result=0x%x\n", result);
    fflush (trace_file);
    return (result);
  }
#else
  return
    (wait_for_multiple_objects_1
     (n_channels, channels, console_index, blockp));
#endif
}

static long
wait_for_multiple_objects_1 (unsigned long n_channels, Tchannel * channels,
			     long console_index, int blockp)
{
  while (1)
    {
      if (console_index < 0)
	{
	  if (pending_interrupts_p ())
	    return (-2);
	}
      else if (Screen_PeekEvent (master_tty_window, 0))
	return (console_index);
      else
	{
	  MSG m;
	  while (PeekMessage ((&m), 0, 0, 0, PM_NOREMOVE))
	    {
	      if ((m . message) != WM_SCHEME_INTERRUPT)
		return (console_index);
	      else if (pending_interrupts_p ())
		return (-2);
	      else
		PeekMessage ((&m), 0, 0, 0, PM_REMOVE);
	    }
	}
      {
	unsigned int index;
	for (index = 0; (index < n_channels); index += 1)
	  if ((index != ((unsigned long) console_index))
	      && ((NT_channel_n_read (channels[index])) != (-1)))
	    return (index);
      }
      if (OS_process_any_status_change ())
	return (-3);
      if (!blockp)
	return (-1);
      /* Block waiting for a message to arrive.  The asynchronous
	 interrupt thread guarantees that a message will arrive in a
	 reasonable amount of time.  */
      if ((MsgWaitForMultipleObjects (0, 0, FALSE, INFINITE, QS_ALLINPUT))
	  == WAIT_FAILED)
	NT_error_api_call
	  ((GetLastError ()), apicall_MsgWaitForMultipleObjects);
    }
}

#define PROCESS_CHANNEL_ARG(arg, type, channel)				\
{									\
  if ((ARG_REF (arg)) == SHARP_F)					\
    (type) = process_channel_type_none;					\
  else if ((ARG_REF (arg)) == (LONG_TO_FIXNUM (-1)))			\
    (type) = process_channel_type_inherit;				\
  else									\
    {									\
      (type) = process_channel_type_explicit;				\
      (channel) = (arg_channel (arg));					\
    }									\
}

static void
parse_subprocess_options (int arg, int * hide_windows_p)
{
  SCHEME_OBJECT options = (VECTOR_ARG (arg));
  if ((VECTOR_LENGTH (options)) < 1)
    error_bad_range_arg (arg);
  (*hide_windows_p) = (OBJECT_TO_BOOLEAN (VECTOR_REF (options, 0)));
}

DEFINE_PRIMITIVE ("NT-MAKE-SUBPROCESS", Prim_NT_make_subprocess, 8, 8,
  "(FILENAME CMD-LINE ENV WORK-DIR STDIN STDOUT STDERR OPTIONS)\n\
Create a subprocess.\n\
FILENAME is the program to run.\n\
CMD-LINE a string containing the program's invocation.\n\
ENV is a string to pass as the program's environment;\n\
  #F means inherit Scheme's environment.\n\
WORK-DIR is a string to pass as the program's working directory;\n\
  #F means inherit Scheme's working directory.\n\
STDIN is the input channel for the subprocess.\n\
STDOUT is the output channel for the subprocess.\n\
STDERR is the error channel for the subprocess.\n\
  Each channel arg can take these values:\n\
  #F means none;\n\
  -1 means use the corresponding channel from Scheme;\n\
  otherwise the argument must be a channel.\n\
OPTIONS is a vector of options.")
{
  PRIMITIVE_HEADER (8);
  {
    CONST char * filename = (STRING_ARG (1));
    CONST char * command_line = (STRING_ARG (2));
    CONST char * env = (((ARG_REF (3)) == SHARP_F) ? 0 : (STRING_ARG (3)));
    CONST char * working_directory
      = (((ARG_REF (4)) == SHARP_F) ? 0 : (STRING_ARG (4)));
    enum process_channel_type channel_in_type;
    Tchannel channel_in;
    enum process_channel_type channel_out_type;
    Tchannel channel_out;
    enum process_channel_type channel_err_type;
    Tchannel channel_err;
    int hide_windows_p;

    PROCESS_CHANNEL_ARG (5, channel_in_type, channel_in);
    PROCESS_CHANNEL_ARG (6, channel_out_type, channel_out);
    PROCESS_CHANNEL_ARG (7, channel_err_type, channel_err);
    parse_subprocess_options (8, (&hide_windows_p));
    PRIMITIVE_RETURN
      (long_to_integer
       (NT_make_subprocess
	(filename, command_line, env, working_directory,
	 channel_in_type, channel_in,
	 channel_out_type, channel_out,
	 channel_err_type, channel_err,
	 hide_windows_p)));
  }
}
