/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/term.c,v 1.3 1990/11/01 04:33:17 cph Exp $

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

#include "scheme.h"
#include "ostop.h"

extern long death_blow;
extern char * Term_Messages [];
extern void EXFUN (get_band_parameters, (long * heap_size, long * const_size));
extern void EXFUN (Reset_Memory, (void));

#ifndef EXIT_HOOK
#define EXIT_HOOK()
#endif

#define BYTES_TO_BLOCKS(n) (((n) + 1023) / 1024)
#define MIN_HEAP_DELTA	50

static void
DEFUN (attempt_termination_backout, (code), int code)
{
  if ((WITHIN_CRITICAL_SECTION_P ())
      || (code == TERM_HALT)
      || (! (Valid_Fixed_Obj_Vector ())))
    return;
  {
    SCHEME_OBJECT Term_Vector = (Get_Fixed_Obj_Slot (Termination_Proc_Vector));
    if ((! (VECTOR_P (Term_Vector)))
	|| ((VECTOR_LENGTH (Term_Vector)) <= code))
      return;
    {
      SCHEME_OBJECT Handler = (VECTOR_REF (Term_Vector, code));
      if (Handler == SHARP_F)
	return;
     Will_Push (CONTINUATION_SIZE
		+ STACK_ENV_EXTRA_SLOTS
		+ ((code == TERM_NO_ERROR_HANDLER) ? 5 : 4));
      Store_Return (RC_HALT);
      Store_Expression (LONG_TO_UNSIGNED_FIXNUM (code));
      Save_Cont ();
      if (code == TERM_NO_ERROR_HANDLER)
	STACK_PUSH (LONG_TO_UNSIGNED_FIXNUM (death_blow));
      STACK_PUSH (Val);			/* Arg 3 */
      STACK_PUSH (Fetch_Env ());	/* Arg 2 */
      STACK_PUSH (Fetch_Expression ()); /* Arg 1 */
      STACK_PUSH (Handler);		/* The handler function */
      STACK_PUSH (STACK_FRAME_HEADER
		  + ((code == TERM_NO_ERROR_HANDLER) ? 4 : 3));
     Pushed ();
      abort_to_interpreter (PRIM_NO_TRAP_APPLY);
    }
  }
}

static void
DEFUN (termination_prefix, (code), int code)
{
  attempt_termination_backout (code);
  OS_restore_external_state ();
  putc ('\n', stdout);
  if ((code < 0) || (code > MAX_TERMINATION))
    fprintf (stdout, "Unknown termination code 0x%x", code);
  else
    fputs ((Term_Messages [code]), stdout);
  if ((WITHIN_CRITICAL_SECTION_P ()) && (code != TERM_HALT))
    fprintf (stdout, " within critical section \"%s\"",
	     (CRITICAL_SECTION_NAME ()));
  fputs (".\n", stdout);
}

static void
DEFUN (termination_suffix, (code, value, abnormal_p),
       int code AND int value AND int abnormal_p)
{
  fflush (stdout);
  Reset_Memory ();
  EXIT_HOOK ();
  EXIT_SCHEME (value);
}

static void
DEFUN (termination_suffix_trace, (code), int code)
{
  if (Trace_On_Error)
    {
      fprintf (stdout, "\n\n**** Stack trace ****\n\n");
      Back_Trace (stdout);
    }
  termination_suffix (code, 1, 1);
}

void
DEFUN (Microcode_Termination, (code), int code)
{
  termination_prefix (code);
  termination_suffix_trace (code);
}

void
DEFUN_VOID (termination_normal)
{
  termination_prefix (TERM_HALT);
  termination_suffix (TERM_HALT, 0, 0);
}

void
DEFUN_VOID (termination_end_of_computation)
{
  termination_prefix (TERM_END_OF_COMPUTATION);
  Print_Expression (Val, "Final result");
  putc ('\n', stdout);
  termination_suffix (TERM_END_OF_COMPUTATION, 0, 0);
}

void
DEFUN_VOID (termination_trap)
{
  /* This claims not to be abnormal so that the user will
     not be asked a second time about dumping core. */
  termination_prefix (TERM_TRAP);
  termination_suffix (TERM_TRAP, 1, 0);
}

void
DEFUN_VOID (termination_no_error_handler)
{
  /* This does not print a back trace because the caller printed one. */
  termination_prefix (TERM_NO_ERROR_HANDLER);
  if (death_blow == ERR_FASL_FILE_TOO_BIG)
    {
      long heap_size;
      long const_size;
      get_band_parameters (&heap_size, &const_size);
      fputs ("Try again with values at least as large as\n", stdout);
      fprintf (stdout, "  -heap %d (%d + %d)\n",
	       (MIN_HEAP_DELTA + (BYTES_TO_BLOCKS (heap_size))),
	       (BYTES_TO_BLOCKS (heap_size)),
	       MIN_HEAP_DELTA);
      fprintf (stdout, "  -constant %d\n", (BYTES_TO_BLOCKS (const_size)));
    }
  termination_suffix (TERM_NO_ERROR_HANDLER, 1, 1);
}

void
DEFUN_VOID (termination_gc_out_of_space)
{
  termination_prefix (TERM_GC_OUT_OF_SPACE);
  fputs ("You are out of space at the end of a Garbage Collection!\n",
	 stdout);
  fprintf (stdout, "Free = 0x%lx; MemTop = 0x%lx; Heap_Top = 0x%lx\n",
	   Free, MemTop, Heap_Top);
  fprintf (stdout, "Words required = %ld; Words available = %ld\n",
	   (MemTop - Free), GC_Space_Needed);
  termination_suffix_trace (TERM_GC_OUT_OF_SPACE);
}

void
DEFUN_VOID (termination_eof)
{
  Microcode_Termination (TERM_EOF);
}

void
DEFUN (termination_signal, (signal_name), CONST char * signal_name)
{
  if (signal_name != 0)
    {
      termination_prefix (TERM_SIGNAL);
      fprintf (stdout, "Killed by %s.\n", signal_name);
    }
  else
    attempt_termination_backout (TERM_SIGNAL);
  termination_suffix_trace (TERM_SIGNAL);
}
