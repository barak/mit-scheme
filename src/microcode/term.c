/* -*-C-*-

$Id: term.c,v 1.24 2007/04/22 16:31:23 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "ostop.h"
#include "osio.h"
#include "osfs.h"
#include "osfile.h"
#include "edwin.h"
#include "option.h"

extern long death_blow;
extern void get_band_parameters (unsigned long *, unsigned long *);

#ifdef __WIN32__
#  define USING_MESSAGE_BOX_FOR_FATAL_OUTPUT
#endif

#ifdef __OS2__
#  define USING_MESSAGE_BOX_FOR_FATAL_OUTPUT
#endif

static void edwin_auto_save (void);
static void delete_temp_files (void);

#define BYTES_TO_BLOCKS(n) (((n) + 1023) / 1024)
#define MIN_HEAP_DELTA	50

#ifndef EXIT_SCHEME
#  define EXIT_SCHEME exit
#endif

#ifdef EXIT_SCHEME_DECLARATIONS
EXIT_SCHEME_DECLARATIONS;
#endif

void
init_exit_scheme (void)
{
#ifdef INIT_EXIT_SCHEME
  INIT_EXIT_SCHEME ();
#endif
}

static void
attempt_termination_backout (int code)
{
  outf_flush_error(); /* NOT flush_fatal */
  if ((WITHIN_CRITICAL_SECTION_P ())
      || (code == TERM_HALT)
      || (! (VECTOR_P (fixed_objects))))
    return;
  {
    SCHEME_OBJECT Term_Vector
      = (VECTOR_REF (fixed_objects, Termination_Proc_Vector));
    if ((! (VECTOR_P (Term_Vector)))
	|| (((long) (VECTOR_LENGTH (Term_Vector))) <= code))
      return;
    {
      SCHEME_OBJECT Handler = (VECTOR_REF (Term_Vector, code));
      if (Handler == SHARP_F)
	return;
     Will_Push (CONTINUATION_SIZE
		+ STACK_ENV_EXTRA_SLOTS
		+ ((code == TERM_NO_ERROR_HANDLER) ? 5 : 4));
      SET_RC (RC_HALT);
      SET_EXP (LONG_TO_UNSIGNED_FIXNUM (code));
      SAVE_CONT ();
      if (code == TERM_NO_ERROR_HANDLER)
	STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (death_blow));
      PUSH_VAL ();		/* Arg 3 */
      PUSH_ENV ();		/* Arg 2 */
      PUSH_EXP ();		/* Arg 1 */
      STACK_PUSH (Handler);	/* The handler function */
      PUSH_APPLY_FRAME_HEADER ((code == TERM_NO_ERROR_HANDLER) ? 4 : 3);
     Pushed ();
      abort_to_interpreter (PRIM_NO_TRAP_APPLY);
    }
  }
}

static void
termination_prefix (int code)
{
  attempt_termination_backout (code);
  OS_restore_external_state ();
  /* TERM_HALT is not an error condition and thus its termination
     message should be considered normal output.  */
  if (code == TERM_HALT)
    {
      if (!option_batch_mode)
	{
	  outf_console ("\n%s.\n", (term_messages[code]));
	  outf_flush_console ();
	}
    }
  else
    {
#ifdef USING_MESSAGE_BOX_FOR_FATAL_OUTPUT
      outf_fatal ("Reason for termination:");
#endif
      outf_fatal ("\n");
      {
	const char * msg = 0;
	if ((code >= 0) && (code <= MAX_TERMINATION))
	  msg = (term_messages[code]);
	if (msg == 0)
	  outf_fatal ("Unknown termination code %#x", code);
	else
	  outf_fatal ("%s", msg);
      }
      if (WITHIN_CRITICAL_SECTION_P ())
	outf_fatal (" within critical section \"%s\"",
		    (CRITICAL_SECTION_NAME ()));
      outf_fatal (".");
#ifndef USING_MESSAGE_BOX_FOR_FATAL_OUTPUT
      outf_fatal ("\n");
#endif
    }
}

static void termination_suffix (int, int, bool) NORETURN;
static void termination_suffix_trace (int) NORETURN;

static void
termination_suffix (int code, int value, bool abnormal_p)
{
#ifdef EXIT_HOOK
  EXIT_HOOK (code, value, abnormal_p);
#endif
  edwin_auto_save ();
  delete_temp_files ();
#ifdef USING_MESSAGE_BOX_FOR_FATAL_OUTPUT
  /* Don't put up message box for ordinary exit.  */
  if (code != TERM_HALT)
#endif
    outf_flush_fatal();
  reset_memory ();
  EXIT_SCHEME (value);
}

static void
termination_suffix_trace (int code)
{
  if (Trace_On_Error)
    {
      outf_error ("\n\n**** Stack trace ****\n\n");
      Back_Trace (ERROR_OUTPUT);
    }
  termination_suffix (code, 1, true);
}

void
Microcode_Termination (int code)
{
  termination_prefix (code);
  termination_suffix_trace (code);
}

void
termination_normal (const int value)
{
  termination_prefix (TERM_HALT);
  termination_suffix (TERM_HALT, value, false);
}

void
termination_init_error (void)
{
  termination_prefix (TERM_EXIT);
  termination_suffix (TERM_EXIT, 1, true);
}

void
termination_end_of_computation (void)
{
  termination_prefix (TERM_END_OF_COMPUTATION);
  Print_Expression (GET_VAL, "Final result");
  outf_console("\n");
  termination_suffix (TERM_END_OF_COMPUTATION, 0, false);
}

void
termination_trap (void)
{
  /* This claims not to be abnormal so that the user will
     not be asked a second time about dumping core. */
  termination_prefix (TERM_TRAP);
  termination_suffix (TERM_TRAP, 1, false);
}

void
termination_no_error_handler (void)
{
  /* This does not print a back trace because the caller printed one. */
  termination_prefix (TERM_NO_ERROR_HANDLER);
  if (death_blow == ERR_FASL_FILE_TOO_BIG)
    {
      unsigned long heap_size;
      unsigned long const_size;
      get_band_parameters ((&heap_size), (&const_size));
      outf_fatal ("Try again with values at least as large as\n");
      outf_fatal ("  --heap %lu\n",
		  (MIN_HEAP_DELTA + (BYTES_TO_BLOCKS (heap_size))));
      outf_fatal ("  --constant %lu\n", (BYTES_TO_BLOCKS (const_size)));
    }
  termination_suffix (TERM_NO_ERROR_HANDLER, 1, true);
}

void
termination_gc_out_of_space (void)
{
  termination_prefix (TERM_GC_OUT_OF_SPACE);
  outf_fatal ("You are out of space at the end of a garbage collection!\n");
  outf_fatal
    ("Free = %#lx; heap_alloc_limit = %#lx; heap_end = %#lx\n",
     ((unsigned long) Free),
     ((unsigned long) heap_alloc_limit),
     ((unsigned long) heap_end));
  outf_fatal ("# words needed = %lu; # words available = %lu\n",
	      gc_space_needed, HEAP_AVAILABLE);
  termination_suffix_trace (TERM_GC_OUT_OF_SPACE);
}

void
termination_eof (void)
{
  Microcode_Termination (TERM_EOF);
}

void
termination_signal (const char * signal_name)
{
  if (signal_name != 0)
    {
      termination_prefix (TERM_SIGNAL);
      outf_fatal ("Killed by %s.\n", signal_name);
    }
  else
    attempt_termination_backout (TERM_SIGNAL);
  termination_suffix_trace (TERM_SIGNAL);
}

static void
edwin_auto_save (void)
{
  static SCHEME_OBJECT position;
  static struct interpreter_state_s new_state;

  position =
    ((VECTOR_P (fixed_objects))
     ? (VECTOR_REF (fixed_objects, FIXOBJ_EDWIN_AUTO_SAVE))
     : EMPTY_LIST);
  while (PAIR_P (position))
    {
      SCHEME_OBJECT entry = (PAIR_CAR (position));
      position = (PAIR_CDR (position));
      if ((PAIR_P (entry))
	  && (GROUP_P (PAIR_CAR (entry)))
	  && (STRING_P (PAIR_CDR (entry)))
	  && ((GROUP_MODIFIED_P (PAIR_CAR (entry))) == SHARP_T))
	{
	  SCHEME_OBJECT group = (PAIR_CAR (entry));
	  char * namestring = (STRING_POINTER (PAIR_CDR (entry)));
	  unsigned long length;
	  unsigned char * start = (GROUP_TEXT (group, (&length)));
	  unsigned char * end = (start + length);
	  unsigned char * gap_start = (start + (GROUP_GAP_START (group)));
	  unsigned char * gap_end = (start + (GROUP_GAP_END (group)));
	  if ((start < gap_start) || (gap_end < end))
	    {
	      bind_interpreter_state (&new_state);
	      if ((setjmp (interpreter_catch_env)) == 0)
		{
		  Tchannel channel;
		  outf_error ("Auto-saving file \"%s\"\n", namestring);
		  outf_flush_error ();
		  channel = (OS_open_output_file (namestring));
		  if (start < gap_start)
		    OS_channel_write (channel, start, (gap_start - start));
		  if (gap_end < end)
		    OS_channel_write (channel, gap_end, (end - gap_end));
		  OS_channel_close (channel);
		}
	      unbind_interpreter_state (&new_state);
	    }
	}
    }
}

static void
delete_temp_files (void)
{
  static SCHEME_OBJECT position;
  static struct interpreter_state_s new_state;

  position =
    ((VECTOR_P (fixed_objects))
     ? (VECTOR_REF (fixed_objects, FIXOBJ_FILES_TO_DELETE))
     : EMPTY_LIST);
  while (PAIR_P (position))
    {
      SCHEME_OBJECT entry = (PAIR_CAR (position));
      position = (PAIR_CDR (position));
      if (STRING_P (entry))
	{
	  bind_interpreter_state (&new_state);
	  if ((setjmp (interpreter_catch_env)) == 0)
	    OS_file_remove (STRING_POINTER (entry));
	  unbind_interpreter_state (&new_state);
	}
    }
}
