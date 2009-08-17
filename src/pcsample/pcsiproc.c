/* -*-C-*-

$Id: b4b7aa3acfdafc19582f24c3c4a7161e63488f04 $

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

/* PCSIPROC.C -- defines PC Sample subroutines for profiling interp-procs *\
\*              (a.k.a. interpreted procedures) within pcsample.c         */

/*****************************************************************************/
#ifdef REALLY_INCLUDE_PROFILE_CODE /* scan_defines concession */

#include <microcode/lookup.h>		/* For AUX_LIST_TYPE    */

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*\
 * TODO:
 *
 *  - Maybe flatten number of primitives?
 *
\*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

/*===========================================================================*\
 * Interp-Proc Profile Buffer is for buffering sightings of interpreted procs *
 * (a.k.a. compounds) until they can be spilled into the Interp-Proc Profile  *
 * Table.								      *
 *									      *
 * This hairy mess is to reduce the overhead of passing interpreted procs up  *
 * to Scheme (where they can be entered into a hash table)... only once the   *
 * buffer is nearly filled does an interrupt get generated to spill the buffer*
 * contents into the profile hashtable.                                       *
\*===========================================================================*/

/*****************************************************************************
 * Interp-Proc Profile Buffer consists of a vector of slots and a handfull of
 * state variables...
 */

static struct profile_buffer_state interp_proc_profile_buffer_state;

static void
DEFUN_VOID (init_IPPB_profile_buffer_state)
{
  init_profile_uni_buffer_state (&interp_proc_profile_buffer_state,
				 " IPPB",			/* name	     */
				 PC_Sample_Interp_Proc_Buffer,	/* ID	     */
				 8*128,				/* slack     */
				   128,				/* slack_inc */
				 INT_IPPB_Flush,		/* flush_INT */
				 INT_IPPB_Extend		/* extnd_INT */
				 );
}

/* convenient shorthand for use in primitives below... */

#define					  IPPB_name			\
	(interp_proc_profile_buffer_state    . name)
#define					  IPPB_ID			\
	(interp_proc_profile_buffer_state    . ID)
#define					  IPPB_enabled			\
	(interp_proc_profile_buffer_state    . enabled_flag)
#define					  IPPB_buffer			\
	(interp_proc_profile_buffer_state    . buffer)
#define					  IPPB_length			\
	(interp_proc_profile_buffer_state    . length)
#define					  IPPB_next_empty_slot_index	\
	(interp_proc_profile_buffer_state    . next_empty_slot_index)
#define					  IPPB_slack			\
	(interp_proc_profile_buffer_state    . slack)
#define					  IPPB_slack_increment		\
	(interp_proc_profile_buffer_state    . slack_increment)
#define					  IPPB_flush_INT		\
	(interp_proc_profile_buffer_state    . flush_INT)
#define					  IPPB_extend_INT		\
	(interp_proc_profile_buffer_state    . extend_INT)
#define					  IPPB_flush_noisy		\
	(interp_proc_profile_buffer_state    . flush_noisy_flag)
#define					  IPPB_extend_noisy		\
	(interp_proc_profile_buffer_state    . extend_noisy_flag)
#define					  IPPB_overflow_noisy		\
	(interp_proc_profile_buffer_state    . overflow_noisy_flag)
#define					  IPPB_flush_immediate		\
	(interp_proc_profile_buffer_state    . flush_immed_flag)
#define					  IPPB_debugging		\
	(interp_proc_profile_buffer_state    . debug_flag)
#define					  IPPB_monitoring		\
	(interp_proc_profile_buffer_state    . monitor_flag)
#define					  IPPB_flush_count		\
	(interp_proc_profile_buffer_state    . flush_count)
#define					  IPPB_extend_count		\
	(interp_proc_profile_buffer_state    . extend_count)
#define					  IPPB_overflow_count		\
	(interp_proc_profile_buffer_state    . overflow_count)
#define					  IPPB_extra_info		\
	(interp_proc_profile_buffer_state    . extra_buffer_state_info)

/*---------------------------------------------------------------------------*/
#define IPPB_disable() do						      \
{									      \
  Set_Fixed_Obj_Slot (PC_Sample_Interp_Proc_Buffer, SHARP_F ) ;		      \
  IPPB_buffer		     =			    SHARP_F   ;		      \
  IPPB_enabled		     =			    false     ;		      \
  IPPB_next_empty_slot_index =			    0	      ;		      \
  IPPB_length		     =			    0	      ; /* Paranoia */\
} while (FALSE)
/*...........................................................................*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/DISABLE",
		  Prim_IPPB_disable, 0, 0,
 "()\n\
 Disables the interpreted procedure profile buffer hence disabling profiling\n\
 of interpreted procedures (unless and until a new buffer is installed).\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_disable ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}
/*---------------------------------------------------------------------------*/
#define IPPB_install(buffer_arg) do					      \
{									      \
  Set_Fixed_Obj_Slot (PC_Sample_Interp_Proc_Buffer, buffer_arg ) ;	      \
  IPPB_buffer  =				    buffer_arg	 ;	      \
  IPPB_enabled =				    true	 ;	      \
  IPPB_length  =		    (VECTOR_LENGTH (buffer_arg)) ;	      \
  /* NB: Do NOT reset next_empty_slot_index since may be extending */	      \
} while (FALSE)
/*...........................................................................*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/INSTALL",
		  Prim_IPPB_install, 1, 1,
 "(vector)\n\
 Installs VECTOR as the interpreted procedure profile buffer.\
 ")
{
  PRIMITIVE_HEADER(1);
  CHECK_ARG(1, VECTOR_P);
  IPPB_install (ARG_REF (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
static void
DEFUN_VOID(resynch_IPPB_post_gc_hook)
{
  if IPPB_enabled 
     IPPB_install (Get_Fixed_Obj_Slot (PC_Sample_Interp_Proc_Buffer)) ;
}
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/SLACK", Prim_IPPB_slack, 0, 0,
 "()\n\
 Returns the `slack' by which the near-fullness of the interpreted procedure\n\
 profile buffer is determined and by which increment the buffer is extended\n\
 when full.\n\
 \n\
 Note that the slack will always be a positive fixnum.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (ulong_to_integer (IPPB_slack));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/SET-SLACK",
		  Prim_IPPB_set_slack, 1, 1,
 "(positive-fixnum)\n\
 Sets the `slack' by which the near-fullness of the interpreted procedure\n\
 profile buffer is determined and by which increment the buffer is extended\n\
 when full.\n\
 \n\
 Note that the slack must be a positive fixnum.\
 ")
{
  PRIMITIVE_HEADER(1);
  CHECK_ARG (1, FIXNUM_POSITIVE_P);
  IPPB_slack = (integer_to_ulong (ARG_REF (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/SLACK-INCREMENT",
		  Prim_IPPB_slack_increment, 0, 0,
 "()\n\
 Returns the amount by which the interpreted procedure profile buffer slack\n\
 is incremented when a buffer overflow occurs. In this sense it cuts the\n\
 slack some slack.\n\
 \n\
 Note that the slack increment will always be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (long_to_integer (IPPB_slack_increment));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/SET-SLACK-INCREMENT",
		  Prim_IPPB_set_slack_increment, 1, 1,
 "(fixnum)\n\
 Sets the amount by which the interpreted procedure profile buffer slack is\n\
 incremented when a buffer overflow occurs.\n\
 \n\
 Note that the slack increment must be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ")
{
  PRIMITIVE_HEADER(1);
  CHECK_ARG (1, INTEGER_P);
  IPPB_slack_increment = (integer_to_long (ARG_REF (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/EXTEND-NOISY?",
		  Prim_IPPB_extend_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of IPPB extensions is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_extend_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/FLUSH-NOISY?",
		  Prim_IPPB_flush_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of IPPB extensions is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_flush_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/OVERFLOW-NOISY?",
		  Prim_IPPB_overflow_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of IPPB overflows is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_overflow_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/EXTEND-NOISY?/TOGGLE!",
		  Prim_IPPB_extend_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of IPPB extensions.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_extend_noisy = (! (IPPB_extend_noisy)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_extend_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/FLUSH-NOISY?/TOGGLE!",
		  Prim_IPPB_flush_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of IPPB flushes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_flush_noisy = (! (IPPB_flush_noisy)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_flush_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/OVERFLOW-NOISY?/TOGGLE!",
		  Prim_IPPB_overflow_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of IPPB overflows.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_overflow_noisy = (! (IPPB_overflow_noisy)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_overflow_noisy)) ;
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/EMPTY?", Prim_IPPB_empty_p, 0, 0,
 "()\n\
 Returns a boolean indicating whether or not the IPPB is empty.\
 ")
{
 PRIMITIVE_HEADER(0);
 PRIMITIVE_RETURN(BOOLEAN_TO_OBJECT (IPPB_next_empty_slot_index == 0)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("INTERP-PROC-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX", 
		  Prim_IPPB_next_empty_slot_index, 0, 0,
 "()\n\
 Returns the index of the next `free' slot of the interp-proc profile buffer.\
 ")
{
 PRIMITIVE_HEADER(0);
 PRIMITIVE_RETURN(ulong_to_integer (IPPB_next_empty_slot_index));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%INTERP-PROC-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX/RESET",
		  Prim_IPPB_next_empty_slot_index_reset, 0, 0,
  "()\n\
  Resets the index of the next `free' slot of the interp-proc profile buffer.\
  \n\
  Only officially designated wizards should even think of using this\n\
  super secret primitive. FNORD!\
  ")
{
 PRIMITIVE_HEADER(0);
 IPPB_next_empty_slot_index = ((unsigned long) 0);
 PRIMITIVE_RETURN(UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-FLUSH-IMMEDIATE?",
		  Prim_pc_sample_IPPB_flush_immediate_p, 0, 0,
 "()\n\
 Specifies whether the IPPB is flushed upon each entry.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_flush_immediate)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-FLUSH-IMMEDIATE?/TOGGLE!",
		  Prim_pc_sample_IPPB_flush_immediate_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether the IPPBuffer is flushed upon each entry.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler debugging purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_flush_immediate = (! (IPPB_flush_immediate)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_flush_immediate)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-DEBUGGING?",
		  Prim_pc_sample_IPPB_debugging_p, 0, 0,
 "()\n\
 Specifies whether the IPPB is in debugging mode.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_debugging)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-DEBUGGING?/TOGGLE!",
		  Prim_pc_sample_IPPB_debugging_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether the IPPBuffer is in debugging mode.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler debugging purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_debugging = (! (IPPB_debugging)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_debugging)) ;
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-MONITORING?",
		  Prim_pc_sample_IPPB_monitoring_p, 0, 0,
 "()\n\
 Specifies whether the IPPB is in monitoring mode.\n\
 \n\
 This, for instance, is how a count of buffer overflows is accumulated.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_monitoring)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-MONITORING?/TOGGLE!",
		  Prim_pc_sample_IPPB_monitoring_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether the IPPB is in monitoring mode.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler monitoring purposes only.\n\
 For instance, toggling this monitor flag to true triggers accumulating\n\
 a count of buffer overflows.\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_monitoring = (! (IPPB_monitoring)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (IPPB_monitoring)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-FLUSH-COUNT",
		  Prim_pc_sample_IPPB_flush_count, 0, 0,
 "()\n\
 Returns the number of IPPB flush requests that have been issued since the\n\
 last PC-SAMPLE/IPPB-FLUSH-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (IPPB_flush_count));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-FLUSH-COUNT/RESET",
		  Prim_pc_sample_IPPB_flush_count_reset, 0, 0,
 "()\n\
 Resets the IPPB flush count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_flush_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-EXTEND-COUNT",
		  Prim_pc_sample_IPPB_extend_count, 0, 0,
 "()\n\
 Returns the number of IPPB extend requests that have been issued since the\n\
 last PC-SAMPLE/IPPB-EXTEND-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (IPPB_extend_count));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-EXTEND-COUNT/RESET",
		  Prim_pc_sample_IPPB_extend_count_reset, 0, 0,
 "()\n\
 Resets the IPPB extend count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_extend_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-OVERFLOW-COUNT",
		  Prim_pc_sample_IPPB_overflow_count, 0, 0,
 "()\n\
 Returns the number of IPPB overflows that have been issued since the\n\
 last PC-SAMPLE/IPPB-OVERFLOW-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\n\
 \n\
 Each overflow indicates a sample that was punted into the bit bucket.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (IPPB_overflow_count));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-OVERFLOW-COUNT/RESET",
		  Prim_pc_sample_IPPB_overflow_count_reset, 0, 0,
 "()\n\
 Resets the IPPB overflow count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_overflow_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/IPPB-EXTRA-INFO",
		  Prim_pc_sample_IPPB_extra_info, 0, 0,
 "()\n\
 Returns the extra info entry associated with the IPP Buffer.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (IPPB_extra_info) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/SET-IPPB-EXTRA-INFO!",
		  Prim_pc_sample_set_IPPB_extra_info_bang, 1, 1,
 "(object)\n\
 Stores OBJECT in the extra info entry of the IPPB.\n\
 \n\
 This is for mondo bizarro sampler frobnication purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(1);
  IPPB_extra_info = ARG_REF(1);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*****************************************************************************
 * kludgerous ``hidden arg'' passing mechanism
 */

static SCHEME_OBJECT pc_sample_current_env_frame = UNSPECIFIC ;

/*****************************************************************************/
static void
DEFUN (pc_sample_record_interp_proc, (trinfo), struct trap_recovery_info * trinfo)
{
  /* GJR suggested nabbing the current ENV to find the current PROC,
   * warning that the current ENV may be invalid, e.g. in the middle
   * of a LOAD.  Its validity will have been assured by the caller here.
   *
   * Since no real virtual PC is maintained in the interpreter, this ENV
   * frobbing is our only means of mapping a SIGCONTEXT into some unique ID
   * of the interp-proc being interpreted. Specifically, we recover the lambda
   * lurking within the body of the procedure whose arguments gave rise to the
   * current ENV frame.
   *
   * Oh, TRINFO arg is for cutesy diagnostics of Unidentifiable Function Objs.
   */

  SCHEME_OBJECT interp_proc_lambda ;
  SCHEME_OBJECT the_procedure = (MEMORY_REF (pc_sample_current_env_frame,
					     ENVIRONMENT_FUNCTION));

  /* Stutter step to make sure it really *is* a procedure object */

  if ((OBJECT_TYPE (the_procedure)) == AUX_LIST_TYPE)
    the_procedure     = (MEMORY_REF (the_procedure, ENV_EXTENSION_PROCEDURE));

  interp_proc_lambda  = (MEMORY_REF (the_procedure, PROCEDURE_LAMBDA_EXPR  ));

  /* Hurumph... since the lambda may never have been hashed (and trap
   * handlers are forbidden to do the CONSing necessary to generate new hash
   * numbers), and since there is no microcode/scheme interface for hashing
   * microcode objects (i.e., C data) anyway, we just pass the buck up to the
   * interrupt handler mechanism: interrupt handlers are called at delicately
   * perspicatious moments so they are permitted to CONS. This buck is passed
   * by buffering lambdas until we have enough of them that it is worth
   * issuing a request to spill the buffer into the lambda hashtable.
   * For more details, see pcsiproc.scm in the runtime directory.
   */

  pc_sample_record_buffer_entry( interp_proc_lambda,
				&interp_proc_profile_buffer_state);

#if (	defined(PCS_LOG)	/* Sample console logging */		      \
     || defined(PCS_LOG_INTERP_PROC)					      \
     )
  log_interp_proc_sample (trinfo) ;
#endif

}



/*****************************************************************************/
#endif /* REALLY_INCLUDE_PROFILE_CODE */
