/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
extern Tchannel arg_to_channel (SCHEME_OBJECT, int);

static Tchannel * object_to_channel_vector
  (SCHEME_OBJECT, int, unsigned long *, long *);
static long wait_for_multiple_objects (unsigned long, Tchannel *, long, int);
static long wait_for_multiple_objects_1 (unsigned long, Tchannel *, long, int);

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (arg_channel (1)));
}

DEFINE_PRIMITIVE ("WIN32-GUI-TRACE", Prim_win32_gui_trace, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    win32_trace_level = (arg_ulong_integer (1));
    if (win32_trace_file != 0)
      {
	fflush (win32_trace_file);
	fclose (win32_trace_file);
	win32_trace_file = 0;
      }
    if (win32_trace_level > 0)
      win32_trace_file
	= (fopen ((((ARG_REF (2)) == SHARP_F)
		   ? WIN32_TRACE_FILENAME
		   : (STRING_ARG (2))),
		  "w"));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
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
  if (win32_trace_level > 1)
    {
      fprintf (win32_trace_file, "wait_for_multiple_objects: ");
      fprintf (win32_trace_file, "n_channels=%d console_index=%d blockp=%d\n",
	       n_channels, console_index, blockp);
      fflush (win32_trace_file);
    }
  {
    long result
      = (wait_for_multiple_objects_1
	 (n_channels, channels, console_index, blockp));
    if (win32_trace_level > 1)
      {
	fprintf (win32_trace_file, "wait_for_multiple_objects: ");
	fprintf (win32_trace_file, "result=0x%x\n", result);
	fflush (win32_trace_file);
      }
    return (result);
  }
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
      else if (Screen_pending_events_p ())
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
    const char * filename = (STRING_ARG (1));
    const char * command_line = (STRING_ARG (2));
    const char * env = (((ARG_REF (3)) == SHARP_F) ? 0 : (STRING_ARG (3)));
    const char * working_directory
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
