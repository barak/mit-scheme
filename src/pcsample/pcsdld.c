/* -*-C-*-

$Id: pcsdld.c,v 1.7 2003/02/14 18:28:31 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

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

/* PCSDLD.C -- defines the PC Sample dynamic load interface to Scheme */

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*\
 * TODO:
 *	Get a real job. Find a wife, CONS up some progeny. Write a will. Croak.
 *
\*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

/*****************************************************************************
 * Uhm... don't forget to pay the piper... must define prims first so known.
 *****************************************************************************/

#ifndef REALLY_INCLUDE_PROFILE_CODE	/* scan_defines concession */
#define REALLY_INCLUDE_PROFILE_CODE	/* scan_defines concession */
#endif

#include "pcsample.c"		/* The PC sampler microcode */

/*****************************************************************************/
#include <microcode/usrdef.h>		/* For declare_primitive */

extern void EXFUN (initialize_pcsample_primitives, (void));
       void
DEFUN_VOID        (initialize_pcsample_primitives)
{
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PC-SAMPLE/TIMER-CLEAR",
		     Prim_pc_sample_timer_clear, 0, 0,
		     "()\n\
  Turn off the PC sample timer.\
  ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PC-SAMPLE/TIMER-SET",
		     Prim_pc_sample_timer_set, 2, 2,
		     "(first interval)\n\
  Set the PC sample timer.\n\
  First arg FIRST says how long to wait until the first interrupt;\n\
  second arg INTERVAL says how long to wait between interrupts after that.\n\
  Both arguments are in units of milliseconds.\
  ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/HALTED?",
		     Prim_pc_sample_halted_p, 0, 0,
		     "()\n\
 Specifies whether PC sampling has been brute forcably disabled.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/HALTED?/TOGGLE!",
		     Prim_pc_sample_halted_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether PC sampling is brute forcably disabled.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 -------\n\
 WARNING! If pc-sample/init has not been called (to initialize profiling\n\
 -------  tables) then you will lose big if you naively toggle halted-flag\n\
          to #F because that will start the profile timer.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/CACHE-GC-PRIMITIVE-INDEX",
		     Prim_pc_sample_cache_GC_primitive_index, 0, 0,
		     "()\n\
  Signals the microcode to go find the GARBAGE-COLLECT primitive and cache\n\
  away its index into the Primitive Table.\n\
  \n\
  This should be invoked each time the Primitive Table is altered in such a\n\
  way that existing primitives can shift about.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PC-SAMPLE/SPILL-GC-SAMPLES-INTO-PRIMITIVE-TABLE",
		     Prim_pc_sample_spill_GC_samples_into_primitive_table, 0, 0,
		     "()\n\
  Make sure all samples taken during GC are present and accounted for in the\n\
  Primitive Sample Table.\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/INSTALL-GC-SYNCH-GC-HOOKS",
		     Prim_pc_sample_install_gc_synch_gc_hooks, 0, 0,
		     "()\n\
  This must be called once when PC sampling is enabled.\n\
  \n\
  If it returns #F then PC sampling must be disabled.  You.lose\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/INSTALL-MICROCODE",
		     Prim_pc_sample_install_microcode, 0, 0,
		     "()\n\
  Installs the microcode support structures for PC sampling.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/DISABLE-MICROCODE",
		     Prim_pc_sample_disable_microcode, 0, 0,
		     "()\n\
  Disables the microcode support structures for PC sampling.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/DISABLE",
		     Prim_IPPB_disable, 0, 0,
		     "()\n\
 Disables the interpreted procedure profile buffer hence disabling profiling\n\
 of interpreted procedures (unless and until a new buffer is installed).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/INSTALL",
		     Prim_IPPB_install, 1, 1,
		     "(vector)\n\
 Installs VECTOR as the interpreted procedure profile buffer.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/SLACK",
		     Prim_IPPB_slack, 0, 0,
		     "()\n\
 Returns the `slack' by which the near-fullness of the interpreted procedure\n\
 profile buffer is determined and by which increment the buffer is extended\n\
 when full.\n\
 \n\
 Note that the slack will always be a positive fixnum.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/SET-SLACK",
		     Prim_IPPB_set_slack, 1, 1,
		     "(positive-fixnum)\n\
 Sets the `slack' by which the near-fullness of the interpreted procedure\n\
 profile buffer is determined and by which increment the buffer is extended\n\
 when full.\n\
 \n\
 Note that the slack must be a positive fixnum.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/SLACK-INCREMENT",
		     Prim_IPPB_slack_increment, 0, 0,
		     "()\n\
 Returns the amount by which the interpreted procedure profile buffer slack\n\
 is incremented when a buffer overflow occurs. In this sense it cuts the\n\
 slack some slack.\n\
 \n\
 Note that the slack increment will always be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/SET-SLACK-INCREMENT",
		     Prim_IPPB_set_slack_increment, 1, 1,
		     "(fixnum)\n\
 Sets the amount by which the interpreted procedure profile buffer slack is\n\
 incremented when a buffer overflow occurs.\n\
 \n\
 Note that the slack increment must be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/EXTEND-NOISY?",
		     Prim_IPPB_extend_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of IPPB extensions is enabled.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/FLUSH-NOISY?",
		     Prim_IPPB_flush_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of IPPB extensions is enabled.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/OVERFLOW-NOISY?",
		     Prim_IPPB_overflow_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of IPPB overflows is enabled.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/EXTEND-NOISY?/TOGGLE!",
		     Prim_IPPB_extend_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of IPPB extensions.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/FLUSH-NOISY?/TOGGLE!",
		     Prim_IPPB_flush_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of IPPB flushes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/OVERFLOW-NOISY?/TOGGLE!",
		     Prim_IPPB_overflow_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of IPPB overflows.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/EMPTY?",
		     Prim_IPPB_empty_p, 0, 0,
		     "()\n\
 Returns a boolean indicating whether or not the IPPB is empty.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("INTERP-PROC-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX", 
		     Prim_IPPB_next_empty_slot_index, 0, 0,
		     "()\n\
 Returns the index of the next `free' slot of the interp-proc profile buffer.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%INTERP-PROC-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX/RESET",
		     Prim_IPPB_next_empty_slot_index_reset, 0, 0,
		     "()\n\
  Resets the index of the next `free' slot of the interp-proc profile buffer.\
  \n\
  Only officially designated wizards should even think of using this\n\
  super secret primitive. FNORD!\
  ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-FLUSH-IMMEDIATE?",
		     Prim_pc_sample_IPPB_flush_immediate_p, 0, 0,
		     "()\n\
 Specifies whether the IPPB is flushed upon each entry.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-FLUSH-IMMEDIATE?/TOGGLE!",
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
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-DEBUGGING?",
		     Prim_pc_sample_IPPB_debugging_p, 0, 0,
		     "()\n\
 Specifies whether the IPPB is in debugging mode.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-DEBUGGING?/TOGGLE!",
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
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-MONITORING?",
		     Prim_pc_sample_IPPB_monitoring_p, 0, 0,
		     "()\n\
 Specifies whether the IPPB is in monitoring mode.\n\
 \n\
 This, for instance, is how a count of buffer overflows is accumulated.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-MONITORING?/TOGGLE!",
		     Prim_pc_sample_IPPB_monitoring_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether the IPPB is in monitoring mode.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler monitoring purposes only.\n\
 For instance, toggling this monitor flag to true triggers accumulating\n\
 a count of buffer overflows.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-FLUSH-COUNT",
		     Prim_pc_sample_IPPB_flush_count, 0, 0,
		     "()\n\
 Returns the number of IPPB flush requests that have been issued since the\n\
 last PC-SAMPLE/IPPB-FLUSH-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-FLUSH-COUNT/RESET",
		     Prim_pc_sample_IPPB_flush_count_reset, 0, 0,
		     "()\n\
 Resets the IPPB flush count (obviously... sheesh!).\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-EXTEND-COUNT",
		     Prim_pc_sample_IPPB_extend_count, 0, 0,
		     "()\n\
 Returns the number of IPPB extend requests that have been issued since the\n\
 last PC-SAMPLE/IPPB-EXTEND-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-EXTEND-COUNT/RESET",
		     Prim_pc_sample_IPPB_extend_count_reset, 0, 0,
		     "()\n\
 Resets the IPPB extend count (obviously... sheesh!).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-OVERFLOW-COUNT",
		     Prim_pc_sample_IPPB_overflow_count, 0, 0,
		     "()\n\
 Returns the number of IPPB overflows that have been issued since the\n\
 last PC-SAMPLE/IPPB-OVERFLOW-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\n\
 \n\
 Each overflow indicates a sample that was punted into the bit bucket.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-OVERFLOW-COUNT/RESET",
		     Prim_pc_sample_IPPB_overflow_count_reset, 0, 0,
		     "()\n\
 Resets the IPPB overflow count (obviously... sheesh!).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/IPPB-EXTRA-INFO",
		     Prim_pc_sample_IPPB_extra_info, 0, 0,
		     "()\n\
 Returns the extra info entry associated with the IPP Buffer.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/SET-IPPB-EXTRA-INFO!",
		     Prim_pc_sample_set_IPPB_extra_info_bang, 1, 1,
		     "(object)\n\
 Stores OBJECT in the extra info entry of the IPPB.\n\
 \n\
 This is for mondo bizarro sampler frobnication purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/

  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFERS/DISABLE",
		     Prim_PCBPB_disable, 0, 0,
		     "()\n\
 Disables the purified code block profile buffers hence disabling purified\n\
 code block profiling (unless and until new buffers are installed).\
 ");
  /*.........................................................................*/
  declare_primitive ( "HEATHEN-CODE-BLOCK-PROFILE-BUFFERS/DISABLE",
		     Prim_HCBPB_disable, 0, 0,
		     "()\n\
 Disables the  heathen code block profile buffers hence disabling  heathen\n\
 code block profiling (unless and until new buffers are installed).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFERS/INSTALL",
		     Prim_PCBPB_install, 2, 2,
		     "(block-vector offset-vector)\n\
 Installs BLOCK-VECTOR and OFFSET-VECTOR as the purified code block profile\n\
 buffers.\
 ");
  /*.........................................................................*/
  declare_primitive ( "HEATHEN-CODE-BLOCK-PROFILE-BUFFERS/INSTALL",
		     Prim_HCBPB_install, 2, 2,
		     "(block-vector offset-vector)\n\
 Installs BLOCK-VECTOR and OFFSET-VECTOR as the  heathen code block profile\n\
 buffers.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/SLACK",
		     Prim_PCBPB_slack, 0, 0,
		     "()\n\
 Returns the `slack' by which the near-fullness of the profile buffer for\n\
 purified code blocks is determined and by which increment the buffer is\n\
 extended when full.\
  ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/SLACK",
		     Prim_HCBPB_slack, 0, 0,
		     "()\n\
 Returns the `slack' by which the near-fullness of the profile buffer for\n\
 heathen (i.e., non-purified) code blocks is determined and by which\n\
 increment the buffer is extended when full.\
  ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/SET-SLACK",
		     Prim_PCBPB_set_slack, 1, 1,
		     "(positive-fixnum)\n\
 Sets the `slack' by which the near-fullness of the PCBPB is determined and\n\
 by which increment the buffer is extended when full.\n\
 \n\
 Note that the slack must be a positive fixnum.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/SET-SLACK",
		     Prim_HCBPB_set_slack, 1, 1,
		     "(positive-fixnum)\n\
 Sets the `slack' by which the near-fullness of the HCBPB is determined and\n\
 by which increment the buffer is extended when full.\n\
 \n\
 Note that the slack must be a positive fixnum.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/SLACK-INCREMENT",
		     Prim_PCBPB_slack_increment, 0, 0,
		     "()\n\
 Returns the amount by which the PCBPB slack is incremented when a buffer\n\
 overflow occurs. In this sense it cuts the slack more slack.\n\
 \n\
 Note that the slack increment will always be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/SLACK-INCREMENT",
		     Prim_HCBPB_slack_increment, 0, 0,
		     "()\n\
 Returns the amount by which the HCBPB slack is incremented when a buffer\n\
 overflow occurs. In this sense it cuts the slack more slack.\n\
 \n\
 Note that the slack increment will always be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/SET-SLACK-INCREMENT",
		     Prim_PCBPB_set_slack_increment, 1, 1,
		     "(fixnum)\n\
 Sets the amount by which the PCBPB slack is incremented when a buffer\n\
 overflow occurs.\n\
 \n\
 Note that the slack increment must be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/SET-SLACK-INCREMENT",
		     Prim_HCBPB_set_slack_increment, 1, 1,
		     "(fixnum)\n\
 Sets the amount by which the HCBPB slack is incremented when a buffer\n\
 overflow occurs.\n\
 \n\
 Note that the slack increment must be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/EXTEND-NOISY?",
		     Prim_PCBPB_extend_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of PCBPB buffer extensions is enabled.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/EXTEND-NOISY?",
		     Prim_HCBPB_extend_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of HCBPB buffer extensions is enabled.\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/FLUSH-NOISY?",
		     Prim_PCBPB_flush_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of PCBPB buffer extensions is enabled.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/FLUSH-NOISY?",
		     Prim_HCBPB_flush_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of HCBPB buffer extensions is enabled.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/OVERFLOW-NOISY?",
		     Prim_PCBPB_overflow_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of PCBPB buffer extensions is enabled.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/OVERFLOW-NOISY?",
		     Prim_HCBPB_overflow_noisy_p, 0, 0,
		     "()\n\
 Specifies whether notification of HCBPB buffer extensions is enabled.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/EXTEND-NOISY?/TOGGLE!",
		     Prim_PCBPB_extend_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of PCBPB buffer extensions.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/EXTEND-NOISY?/TOGGLE!",
		     Prim_HCBPB_extend_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of HCBPB buffer extensions.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/FLUSH-NOISY?/TOGGLE!",
		     Prim_PCBPB_flush_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of PCBPB buffer flushes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/FLUSH-NOISY?/TOGGLE!",
		     Prim_HCBPB_flush_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of HCBPB buffer flushes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/OVERFLOW-NOISY?/TOGGLE!",
		     Prim_PCBPB_overflow_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of PCBPB buffer overflowes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/OVERFLOW-NOISY?/TOGGLE!",
		     Prim_HCBPB_overflow_noisy_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether to notify of HCBPB buffer overflowes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/EMPTY?",
		     Prim_PCBPB_empty_p, 0, 0,
		     "()\n\
 Returns a boolean indicating whether or not the profile buffer for\n\
 purified code blocks is empty.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/EMPTY?",
		     Prim_HCBPB_empty_p, 0, 0,
		     "()\n\
 Returns a boolean indicating whether or not the profile buffer for\n\
 heathen (i.e., unpurified) code blocks is empty.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX", 
		     Prim_PCBPB_next_empty_slot_index, 0, 0,
		     "()\n\
 Returns the index of the next `free' slot of the profile buffer for\n\
 purified code blocks.\
 ");
  /*.........................................................................*/
  declare_primitive ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX", 
		     Prim_HCBPB_next_empty_slot_index, 0, 0,
		     "()\n\
 Returns the index of the next `free' slot of the profile buffer for\n\
 heathen (i.e., unpurified) code blocks.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PURIFIED-CODE-BLOCK-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX/RESET",
		     Prim_PCBPB_next_empty_slot_index_reset, 0, 0,
		     "()\n\
  Resets the index of the next `free' slot of the profile buffer for\n\
  purified code blocks.\
  \n\
  Only officially designated wizards should even think of using this\n\
  super secret primitive. FNORD!\
  ");
  /*.........................................................................*/
  declare_primitive ("%HEATHEN-CODE-BLOCK-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX/RESET",
		     Prim_HCBPB_next_empty_slot_index_reset, 0, 0,
		     "()\n\
  Resets the index of the next `free' slot of the profile buffer for\n\
  heathen (i.e., unpurified) code blocks.\
  \n\
  Only officially designated wizards should even think of using this\n\
  super secret primitive. FNORD!\
  ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-FLUSH-IMMEDIATE?",
		     Prim_pc_sample_PCBPB_flush_immediate_p, 0, 0,
		     "()\n\
 Specifies whether the Purified Code Block Profile Buffer is flushed upon\n\
 each entry.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-FLUSH-IMMEDIATE?",
		     Prim_pc_sample_HCBPB_flush_immediate_p, 0, 0,
		     "()\n\
 Specifies whether the  Heathen Code Block Profile Buffer is flushed upon\n\
 each entry.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-FLUSH-IMMEDIATE?/TOGGLE!",
		     Prim_pc_sample_PCBPB_flush_immediate_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether the Purified Code Block Profile Buffer\n\
 is flushed upon each entry.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler debugging purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-FLUSH-IMMEDIATE?/TOGGLE!",
		     Prim_pc_sample_HCBPB_flush_immediate_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether the  Heathen Code Block Profile Buffer\n\
 is flushed upon each entry.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler debugging purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-DEBUGGING?",
		     Prim_pc_sample_PCBPB_debugging_p, 0, 0,
		     "()\n\
 Specifies whether the Purified Code Block Profile Buffer is in debugging mode.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-DEBUGGING?",
		     Prim_pc_sample_HCBPB_debugging_p, 0, 0,
		     "()\n\
 Specifies whether the  Heathen Code Block Profile Buffer is in debugging mode.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-DEBUGGING?/TOGGLE!",
		     Prim_pc_sample_PCBPB_debugging_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether the Purified Code Block Profile Buffer\n\
 is in debugging mode.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler debugging purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-DEBUGGING?/TOGGLE!",
		     Prim_pc_sample_HCBPB_debugging_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether the  Heathen Code Block Profile Buffer\n\
 is in debugging mode.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler debugging purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-MONITORING?",
		     Prim_pc_sample_PCBPB_monitoring_p, 0, 0,
		     "()\n\
 Specifies whether the PCBPB is in monitoring mode.\n\
 \n\
 This, for instance, is how a count of buffer overflows is accumulated.\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-MONITORING?",
		     Prim_pc_sample_HCBPB_monitoring_p, 0, 0,
		     "()\n\
 Specifies whether the HCBPB is in monitoring mode.\n\
 \n\
 This, for instance, is how a count of buffer overflows is accumulated.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-MONITORING?/TOGGLE!",
		     Prim_pc_sample_PCBPB_monitoring_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether the Purified Code Block Profile Buffer\n\
 is in monitoring mode.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler monitoring purposes only.\n\
 For instance, toggling this monitor flag to true triggers accumulating\n\
 a count of buffer overflows.\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-MONITORING?/TOGGLE!",
		     Prim_pc_sample_HCBPB_monitoring_p_toggle_bang, 0, 0,
		     "()\n\
 Toggles the Boolean sense of whether the  Heathen Code Block Profile Buffer\n\
 is in monitoring mode.\n\
 \n\
 It returns the newly installed sense of the flag.\n\
 \n\
 This is for mondo bizarro sampler monitoring purposes only.\n\
 For instance, toggling this monitor flag to true triggers accumulating\n\
 a count of buffer overflows.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-FLUSH-COUNT",
		     Prim_pc_sample_PCBPB_flush_count, 0, 0,
		     "()\n\
 Returns the number of PCBPB flush requests that have been issued since the\n\
 last PC-SAMPLE/PCBPB-FLUSH-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-FLUSH-COUNT",
		     Prim_pc_sample_HCBPB_flush_count, 0, 0,
		     "()\n\
 Returns the number of HCBPB flush requests that have been issued since the\n\
 last PC-SAMPLE/HCBPB-FLUSH-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-FLUSH-COUNT/RESET",
		     Prim_pc_sample_PCBPB_flush_count_reset, 0, 0,
		     "()\n\
 Resets the PCBPB flush count (obviously... sheesh!).\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-FLUSH-COUNT/RESET",
		     Prim_pc_sample_HCBPB_flush_count_reset, 0, 0,
		     "()\n\
 Resets the HCBPB flush count (obviously... sheesh!).\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-EXTEND-COUNT",
		     Prim_pc_sample_PCBPB_extend_count, 0, 0,
		     "()\n\
 Returns the number of PCBPB extend requests that have been issued since the\n\
 last PC-SAMPLE/PCBPB-EXTEND-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-EXTEND-COUNT",
		     Prim_pc_sample_HCBPB_extend_count, 0, 0,
		     "()\n\
 Returns the number of HCBPB extend requests that have been issued since the\n\
 last PC-SAMPLE/HCBPB-EXTEND-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-EXTEND-COUNT/RESET",
		     Prim_pc_sample_PCBPB_extend_count_reset, 0, 0,
		     "()\n\
 Resets the PCBPB extend count (obviously... sheesh!).\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-EXTEND-COUNT/RESET",
		     Prim_pc_sample_HCBPB_extend_count_reset, 0, 0,
		     "()\n\
 Resets the HCBPB extend count (obviously... sheesh!).\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-OVERFLOW-COUNT",
		     Prim_pc_sample_PCBPB_overflow_count, 0, 0,
		     "()\n\
 Returns the number of PCBPB overflows that have been issued since the last\n\
 PC-SAMPLE/PCBPB-OVERFLOW-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\n\
 \n\
 Each overflow indicates a sample that was punted into the bit bucket.\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-OVERFLOW-COUNT",
		     Prim_pc_sample_HCBPB_overflow_count, 0, 0,
		     "()\n\
 Returns the number of HCBPB overflows that have been issued since the last\n\
 PC-SAMPLE/HCBPB-OVERFLOW-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\n\
 \n\
 Each overflow indicates a sample that was punted into the bit bucket.\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-OVERFLOW-COUNT/RESET",
		     Prim_pc_sample_PCBPB_overflow_count_reset, 0, 0,
		     "()\n\
 Resets the PCBPB overflow count (obviously... sheesh!).\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-OVERFLOW-COUNT/RESET",
		     Prim_pc_sample_HCBPB_overflow_count_reset, 0, 0,
		     "()\n\
 Resets the HCBPB overflow count (obviously... sheesh!).\
 ");

  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/PCBPB-EXTRA-INFO",
		     Prim_pc_sample_PCBPB_extra_info, 0, 0,
		     "()\n\
 Returns the extra info entry associated with the Purified Code Block\n\
 Profile Buffer.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/HCBPB-EXTRA-INFO",
		     Prim_pc_sample_HCBPB_extra_info, 0, 0,
		     "()\n\
 Returns the extra info entry associated with the  Heathen Code Block\n\
 Profile Buffer.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/
  declare_primitive ("%PC-SAMPLE/SET-PCBPB-EXTRA-INFO!",
		     Prim_pc_sample_set_PCBPB_extra_info, 1, 1,
		     "(object)\n\
 Stores OBJECT in the extra info entry of the Purified Code Block\n\
 Profile Buffer.\n\
 \n\
 This is for mondo bizarro sampler frobnication purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ");
  /*.........................................................................*/
  declare_primitive ("%PC-SAMPLE/SET-HCBPB-EXTRA-INFO!",
		     Prim_pc_sample_set_HCBPB_extra_info, 1, 1,
		     "(object)\n\
 Stores OBJECT in the extra info entry of the  Heathen Code Block\n\
 Profile Buffer.\n\
 \n\
 This is for mondo bizarro sampler frobnication purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ");
  /*-------------------------------------------------------------------------*/



  declare_primitive ("%PC-SAMPLE/SET-ZONE!",
		     Prim_pc_sample_set_current_zone, 1, 1,
		     "(index)\n\
Set current pc-sampling zone to INDEX (a small exact integer), returning \
the previous value if different, else #F if same.");

  declare_primitive ("%PC-SAMPLE/MAX-ZONE",
		     Prim_pc_sample_get_max_zone, 0, 0, 0);

  declare_primitive ("%PC-SAMPLE/CLEAR-ZONES!",
		     Prim_pc_sample_clear_zones, 0, 0,
		     "()\nZero zone counts.");

  declare_primitive ("%PC-SAMPLE/READ-ZONES!", Prim_pc_sample_read_zones, 1, 1,
		     "(flonum-vector)\n\
Copy zone counts into FLONUM-VECTOR.  Returns the number copied, which \
is limited by either the number of zones to the capacity of FLONUM-VECTOR.");

}
