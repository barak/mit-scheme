/* -*-C-*-

$Id: pcscobl.c,v 1.4 2003/02/14 18:28:31 cph Exp $

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

/* PCSCOBL.C -- PC Sample subroutines for profiling code blocks *\
\*               (a.k.a. compiled procs) within pcsample.c       */

/**					**\
|***  BASED VERY HEAVILY ON PCSIPROC.C  ***|
\**					**/

/*****************************************************************************/
#ifdef REALLY_INCLUDE_PROFILE_CODE /* scan_defines concession */

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*\
 * TODO:
 *
 *  - Maybe flatten number of primitives?
 *
\*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

/*****************************************************************************\
 * Code Block Profile Buffers are used for code blocks to serve the same end *
 *  that the Interp-Proc Profile Buffer serves for interpreted procedures.   *
 *  See pcsiproc.[ch].							     *
\*****************************************************************************/

/*===========================================================================*\
 *
 * Code Block Profile Buffers consist of vectors of slots and a handfull of
 *  state variables...
 *
 * There are two distinct Code Block Profile Buffers:
 * 
 *  PCBPB - ``Purified'' Code Block Profile Buffer: for code blocks in constant
 *                                                  space, hence non-relocating
 *  HCBPB - ``Heathen''  Code Block Profile Buffer: for nomadic code blocks
 * 
 * Each conceptual buffer actually corresponds to two distinguishable buffers:
 * the first being a buffer of (Scheme) pointers to code block objects and the
 * second being a buffer of corresponding offsets. This is done because we want
 * to record not just the code block we are in but also the offset into it in
 * case a code block contains multiple procedure bodies. We cannot record a
 * CONS pair of code block/offset since the low level signal system must not
 * allocate heap storage. So, we maintain a synch'd pair of vectors, one for
 * what would be the CARs (blocks) and the other for the CDRs (offsets).
 *
 * << C'est la guerre. >>
 *
\*===========================================================================*/

/* block and offset buffers are synch'd wrt nxt-mt, slack & slack incr */

static struct profile_buffer_state purified_cobl_profile_buffer_state;
static struct profile_buffer_state  heathen_cobl_profile_buffer_state;

static void
DEFUN_VOID (init_CBPB_profile_buffer_states)
{
  init_profile_bi_buffer_state (&purified_cobl_profile_buffer_state,
				"PCBPB",			/* name      */
				PC_Sample_PCB_Block_Buffer,	/* ID        */
				PC_Sample_PCB_Offset_Buffer,	/* ID_aux    */
				8*128,				/* slack     */
				  128,				/* slack_inc */
				INT_PCBPB_Flush,		/* flush_INT */
				INT_PCBPB_Extend		/* extnd_INT */
				);

  init_profile_bi_buffer_state (& heathen_cobl_profile_buffer_state,
				"HCBPB",			/* name      */
				PC_Sample_HCB_Block_Buffer,	/* ID	     */
				PC_Sample_HCB_Offset_Buffer,	/* ID_aux    */
				8*128,				/* slack     */
				  128,				/* slack_inc */
				INT_HCBPB_Flush,		/* flush_INT */
				INT_HCBPB_Extend		/* extnd_INT */
				);
}



/* convenient shorthand for use in primitives below... */

#define					   PCBPB_name			 \
	(purified_cobl_profile_buffer_state    . name)
#define					   HCBPB_name			 \
	( heathen_cobl_profile_buffer_state    . name)
#define					   PCBPB_ID			 \
	(purified_cobl_profile_buffer_state    . ID)
#define					   HCBPB_ID			 \
	( heathen_cobl_profile_buffer_state    . ID)
#define					   PCBPB_enabled		 \
	(purified_cobl_profile_buffer_state    . enabled_flag)
#define					   HCBPB_enabled		 \
	( heathen_cobl_profile_buffer_state    . enabled_flag)

    /* ... continued on next page ... */

    /* ... convenient shorthand: continued from previous page ... */


#define					   PCBPB_buffer			 \
	(purified_cobl_profile_buffer_state    . buffer)
#define					   HCBPB_buffer			 \
	( heathen_cobl_profile_buffer_state    . buffer)
#define					   PCBPB_buffer_aux		 \
	(purified_cobl_profile_buffer_state    . buffer_aux)
#define					   HCBPB_buffer_aux		 \
	( heathen_cobl_profile_buffer_state    . buffer_aux)
#define					   PCBPB_length			 \
	(purified_cobl_profile_buffer_state    . length)
#define					   HCBPB_length			 \
	( heathen_cobl_profile_buffer_state    . length)
#define					   PCBPB_next_empty_slot_index	 \
	(purified_cobl_profile_buffer_state    . next_empty_slot_index)
#define					   HCBPB_next_empty_slot_index	 \
	( heathen_cobl_profile_buffer_state    . next_empty_slot_index)
#define					   PCBPB_slack			 \
	(purified_cobl_profile_buffer_state    . slack)
#define					   HCBPB_slack			 \
	( heathen_cobl_profile_buffer_state    . slack)
#define					   PCBPB_slack_increment	 \
	(purified_cobl_profile_buffer_state    . slack_increment)
#define					   HCBPB_slack_increment	 \
	( heathen_cobl_profile_buffer_state    . slack_increment)
#define					   PCBPB_flush_INT		 \
	(purified_cobl_profile_buffer_state    . flush_INT)
#define					   HCBPB_flush_INT		 \
	( heathen_cobl_profile_buffer_state    . flush_INT)
#define					   PCBPB_extend_INT		 \
	(purified_cobl_profile_buffer_state    . extend_INT)
#define					   HCBPB_extend_INT		 \
	( heathen_cobl_profile_buffer_state    . extend_INT)
#define					   PCBPB_flush_noisy		 \
	(purified_cobl_profile_buffer_state    . flush_noisy_flag)
#define					   HCBPB_flush_noisy		 \
	( heathen_cobl_profile_buffer_state    . flush_noisy_flag)
#define					   PCBPB_extend_noisy		 \
	(purified_cobl_profile_buffer_state    . extend_noisy_flag)
#define					   HCBPB_extend_noisy		 \
	( heathen_cobl_profile_buffer_state    . extend_noisy_flag)
#define					   PCBPB_overflow_noisy		 \
	(purified_cobl_profile_buffer_state    . overflow_noisy_flag)
#define					   HCBPB_overflow_noisy		 \
	( heathen_cobl_profile_buffer_state    . overflow_noisy_flag)
#define					   PCBPB_flush_immediate	 \
	(purified_cobl_profile_buffer_state    . flush_immed_flag)
#define					   HCBPB_flush_immediate	 \
	( heathen_cobl_profile_buffer_state    . flush_immed_flag)
#define					   PCBPB_debugging		 \
	(purified_cobl_profile_buffer_state    . debug_flag)
#define					   HCBPB_debugging		 \
	(purified_cobl_profile_buffer_state    . debug_flag)
#define					   PCBPB_monitoring		 \
	(purified_cobl_profile_buffer_state    . monitor_flag)
#define					   HCBPB_monitoring		 \
	(purified_cobl_profile_buffer_state    . monitor_flag)
#define					   PCBPB_flush_count		 \
	(purified_cobl_profile_buffer_state    . flush_count)
#define					   HCBPB_flush_count		 \
	(purified_cobl_profile_buffer_state    . flush_count)
#define					   PCBPB_extend_count		 \
	(purified_cobl_profile_buffer_state    . extend_count)
#define					   HCBPB_extend_count		 \
	(purified_cobl_profile_buffer_state    . extend_count)
#define					   PCBPB_overflow_count		 \
	(purified_cobl_profile_buffer_state    . overflow_count)
#define					   HCBPB_overflow_count		 \
	(purified_cobl_profile_buffer_state    . overflow_count)
#define					   PCBPB_extra_info 		 \
	(purified_cobl_profile_buffer_state    . extra_buffer_state_info)
#define					   HCBPB_extra_info 		 \
	( heathen_cobl_profile_buffer_state    . extra_buffer_state_info)

/*---------------------------------------------------------------------------*/
#define PCBPB_disable()  do						      \
{									      \
 Set_Fixed_Obj_Slot (PC_Sample_PCB_Block_Buffer,  SHARP_F);		      \
 Set_Fixed_Obj_Slot (PC_Sample_PCB_Offset_Buffer, SHARP_F);		      \
 PCBPB_buffer		     =			  SHARP_F ;		      \
 PCBPB_buffer_aux	     =			  SHARP_F ;		      \
 PCBPB_enabled		     =			  false	  ;		      \
 PCBPB_next_empty_slot_index =			  0	  ;		      \
 PCBPB_length		     =			  0	  ; /* Paranoia */    \
} while (FALSE)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFERS/DISABLE",
		  Prim_PCBPB_disable, 0, 0,
 "()\n\
 Disables the purified code block profile buffers hence disabling purified\n\
 code block profiling (unless and until new buffers are installed).\
 ")
{
 PRIMITIVE_HEADER(0);
 PCBPB_disable ();
 PRIMITIVE_RETURN (UNSPECIFIC);
}
/*...........................................................................*/
#define HCBPB_disable()	 do						      \
{									      \
 Set_Fixed_Obj_Slot (PC_Sample_HCB_Block_Buffer,  SHARP_F);		      \
 Set_Fixed_Obj_Slot (PC_Sample_HCB_Offset_Buffer, SHARP_F);		      \
 HCBPB_buffer		     =			  SHARP_F ;		      \
 HCBPB_buffer_aux	     =			  SHARP_F ;		      \
 HCBPB_enabled		     =			  false	  ;		      \
 HCBPB_next_empty_slot_index =			  0	  ;		      \
 HCBPB_length		     =			  0	  ; /* Paranoia */    \
} while (FALSE)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
DEFINE_PRIMITIVE ( "HEATHEN-CODE-BLOCK-PROFILE-BUFFERS/DISABLE",
		  Prim_HCBPB_disable, 0, 0,
 "()\n\
 Disables the  heathen code block profile buffers hence disabling  heathen\n\
 code block profiling (unless and until new buffers are installed).\
 ")
{
 PRIMITIVE_HEADER(0);
 HCBPB_disable ();
 PRIMITIVE_RETURN (UNSPECIFIC);
}

/*****************************************************************************/
static void
DEFUN_VOID (CBPBs_disable)
{
  PCBPB_disable ();
  HCBPB_disable ();
}

/*---------------------------------------------------------------------------*/
#define CHECK_VECTORS_SAME_LENGTH_P(v1, v2) do				      \
{									      \
  if ((VECTOR_LENGTH (v1)) != (VECTOR_LENGTH (v2)))			      \
  {									      \
    outf_error ("Vector arguments must be of the same length (%d != %d).\n",  \
		(VECTOR_LENGTH (v1)), (VECTOR_LENGTH (v2))) ;		      \
    outf_flush_error () ;						      \
    error_external_return () ;						      \
  }									      \
} while (FALSE)
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
#define PCBPB_install(buffer_arg_1, buffer_arg_2) do			      \
{									      \
  Set_Fixed_Obj_Slot (PC_Sample_PCB_Block_Buffer,  buffer_arg_1)  ;	      \
  Set_Fixed_Obj_Slot (PC_Sample_PCB_Offset_Buffer, buffer_arg_2)  ;	      \
  PCBPB_buffer	   =				   buffer_arg_1	  ;	      \
  PCBPB_buffer_aux =				   buffer_arg_2	  ;	      \
  PCBPB_enabled	   =				   true		  ;	      \
  PCBPB_length	   =		   (VECTOR_LENGTH (buffer_arg_1)) ;	      \
} while (FALSE)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFERS/INSTALL",
		  Prim_PCBPB_install, 2, 2,
 "(block-vector offset-vector)\n\
 Installs BLOCK-VECTOR and OFFSET-VECTOR as the purified code block profile\n\
 buffers.\
 ")
{
  SCHEME_OBJECT buffer_arg_1 ;
  SCHEME_OBJECT buffer_arg_2 ;

  PRIMITIVE_HEADER(2);
  CHECK_ARG(1, VECTOR_P);
  CHECK_ARG(2, VECTOR_P);
  buffer_arg_1 = (ARG_REF (1)) ;
  buffer_arg_2 = (ARG_REF (2)) ;
  CHECK_VECTORS_SAME_LENGTH_P(buffer_arg_1, buffer_arg_2) ;
  PCBPB_install(buffer_arg_1, buffer_arg_2) ;
  /* NB: Do NOT reset next_empty_slot_index since may be extending */
  PRIMITIVE_RETURN (UNSPECIFIC);
}
/*...........................................................................*/
#define HCBPB_install(buffer_arg_1, buffer_arg_2) do			      \
{									      \
  Set_Fixed_Obj_Slot (PC_Sample_HCB_Block_Buffer,  buffer_arg_1)  ;	      \
  Set_Fixed_Obj_Slot (PC_Sample_HCB_Offset_Buffer, buffer_arg_2)  ;	      \
  HCBPB_buffer	   =				   buffer_arg_1	  ;	      \
  HCBPB_buffer_aux =				   buffer_arg_2	  ;	      \
  HCBPB_enabled	   =				   true		  ;	      \
  HCBPB_length	   =		   (VECTOR_LENGTH (buffer_arg_1)) ;	      \
} while (FALSE)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
DEFINE_PRIMITIVE ( "HEATHEN-CODE-BLOCK-PROFILE-BUFFERS/INSTALL",
		  Prim_HCBPB_install, 2, 2,
 "(block-vector offset-vector)\n\
 Installs BLOCK-VECTOR and OFFSET-VECTOR as the  heathen code block profile\n\
 buffers.\
 ")
{
  SCHEME_OBJECT buffer_arg_1 ;
  SCHEME_OBJECT buffer_arg_2 ;

  PRIMITIVE_HEADER(2);
  CHECK_ARG(1, VECTOR_P);
  CHECK_ARG(2, VECTOR_P);
  buffer_arg_1 = (ARG_REF (1)) ;
  buffer_arg_2 = (ARG_REF (2)) ;
  CHECK_VECTORS_SAME_LENGTH_P(buffer_arg_1, buffer_arg_2) ;
  HCBPB_install(buffer_arg_1, buffer_arg_2);
  /* NB: Do NOT reset next_empty_slot_index since may be extending */
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
static void
DEFUN_VOID(resynch_CBPBs_post_gc_hook)
{
  if PCBPB_enabled
     PCBPB_install ((Get_Fixed_Obj_Slot (PC_Sample_PCB_Block_Buffer)),
		    (Get_Fixed_Obj_Slot (PC_Sample_PCB_Offset_Buffer))) ;
  if HCBPB_enabled
     HCBPB_install ((Get_Fixed_Obj_Slot (PC_Sample_HCB_Block_Buffer)),
		    (Get_Fixed_Obj_Slot (PC_Sample_HCB_Offset_Buffer))) ;
}
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/SLACK", Prim_PCBPB_slack,
		                                              0, 0,
  "()\n\
 Returns the `slack' by which the near-fullness of the profile buffer for\n\
 purified code blocks is determined and by which increment the buffer is\n\
 extended when full.\
  ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (ulong_to_integer(PCBPB_slack));
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/SLACK", Prim_HCBPB_slack,
		                                             0, 0,
  "()\n\
 Returns the `slack' by which the near-fullness of the profile buffer for\n\
 heathen (i.e., non-purified) code blocks is determined and by which\n\
 increment the buffer is extended when full.\
  ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (ulong_to_integer(HCBPB_slack));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/SET-SLACK",
		  Prim_PCBPB_set_slack, 1, 1,
 "(positive-fixnum)\n\
 Sets the `slack' by which the near-fullness of the PCBPB is determined and\n\
 by which increment the buffer is extended when full.\n\
 \n\
 Note that the slack must be a positive fixnum.\
 ")
{
  PRIMITIVE_HEADER(1);
  CHECK_ARG (1, FIXNUM_POSITIVE_P);
  PCBPB_slack = (integer_to_ulong (ARG_REF (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/SET-SLACK",
		  Prim_HCBPB_set_slack, 1, 1,
 "(positive-fixnum)\n\
 Sets the `slack' by which the near-fullness of the HCBPB is determined and\n\
 by which increment the buffer is extended when full.\n\
 \n\
 Note that the slack must be a positive fixnum.\
 ")
{
  PRIMITIVE_HEADER(1);
  CHECK_ARG (1, FIXNUM_POSITIVE_P);
  HCBPB_slack = (integer_to_ulong (ARG_REF (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/SLACK-INCREMENT",
		  Prim_PCBPB_slack_increment, 0, 0,
 "()\n\
 Returns the amount by which the PCBPB slack is incremented when a buffer\n\
 overflow occurs. In this sense it cuts the slack more slack.\n\
 \n\
 Note that the slack increment will always be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (long_to_integer(PCBPB_slack_increment));
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/SLACK-INCREMENT",
		  Prim_HCBPB_slack_increment, 0, 0,
 "()\n\
 Returns the amount by which the HCBPB slack is incremented when a buffer\n\
 overflow occurs. In this sense it cuts the slack more slack.\n\
 \n\
 Note that the slack increment will always be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (long_to_integer(HCBPB_slack_increment));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/SET-SLACK-INCREMENT",
		  Prim_PCBPB_set_slack_increment, 1, 1,
 "(fixnum)\n\
 Sets the amount by which the PCBPB slack is incremented when a buffer\n\
 overflow occurs.\n\
 \n\
 Note that the slack increment must be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ")
{
  PRIMITIVE_HEADER(1);
  CHECK_ARG (1, INTEGER_P);
  PCBPB_slack_increment = (integer_to_long (ARG_REF (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/SET-SLACK-INCREMENT",
		  Prim_HCBPB_set_slack_increment, 1, 1,
 "(fixnum)\n\
 Sets the amount by which the HCBPB slack is incremented when a buffer\n\
 overflow occurs.\n\
 \n\
 Note that the slack increment must be a fixnum, but it can be negative\n\
 (in which case it functions as a slack decrement).\
 ")
{
  PRIMITIVE_HEADER(1);
  CHECK_ARG (1, INTEGER_P);
  HCBPB_slack_increment = (integer_to_long (ARG_REF (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/EXTEND-NOISY?",
		  Prim_PCBPB_extend_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of PCBPB buffer extensions is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_extend_noisy)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/EXTEND-NOISY?",
		  Prim_HCBPB_extend_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of HCBPB buffer extensions is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_extend_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/FLUSH-NOISY?",
		  Prim_PCBPB_flush_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of PCBPB buffer extensions is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_flush_noisy)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/FLUSH-NOISY?",
		  Prim_HCBPB_flush_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of HCBPB buffer extensions is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_flush_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/OVERFLOW-NOISY?",
		  Prim_PCBPB_overflow_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of PCBPB buffer extensions is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_overflow_noisy)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/OVERFLOW-NOISY?",
		  Prim_HCBPB_overflow_noisy_p, 0, 0,
 "()\n\
 Specifies whether notification of HCBPB buffer extensions is enabled.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_overflow_noisy)) ;
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/EXTEND-NOISY?/TOGGLE!",
		  Prim_PCBPB_extend_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of PCBPB buffer extensions.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  PCBPB_extend_noisy = (! PCBPB_extend_noisy) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_flush_noisy)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/EXTEND-NOISY?/TOGGLE!",
		  Prim_HCBPB_extend_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of HCBPB buffer extensions.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  HCBPB_extend_noisy = (! HCBPB_extend_noisy) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_flush_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/FLUSH-NOISY?/TOGGLE!",
		  Prim_PCBPB_flush_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of PCBPB buffer flushes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  PCBPB_flush_noisy = (! PCBPB_flush_noisy) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_flush_noisy)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/FLUSH-NOISY?/TOGGLE!",
		  Prim_HCBPB_flush_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of HCBPB buffer flushes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  HCBPB_flush_noisy = (! HCBPB_flush_noisy) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_flush_noisy)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/OVERFLOW-NOISY?/TOGGLE!",
		  Prim_PCBPB_overflow_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of PCBPB buffer overflowes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  PCBPB_overflow_noisy = (! PCBPB_overflow_noisy) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_overflow_noisy)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/OVERFLOW-NOISY?/TOGGLE!",
		  Prim_HCBPB_overflow_noisy_p_toggle_bang, 0, 0,
 "()\n\
 Toggles the Boolean sense of whether to notify of HCBPB buffer overflowes.\n\
 \n\
 It returns the newly installed sense of the flag.\
 ")
{
  PRIMITIVE_HEADER(0);
  HCBPB_overflow_noisy = (! HCBPB_overflow_noisy) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_overflow_noisy)) ;
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/EMPTY?",
		  Prim_PCBPB_empty_p, 0, 0,
 "()\n\
 Returns a boolean indicating whether or not the profile buffer for\n\
 purified code blocks is empty.\
 ")
{
 PRIMITIVE_HEADER(0);
 PRIMITIVE_RETURN(BOOLEAN_TO_OBJECT (PCBPB_next_empty_slot_index == 0));
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/EMPTY?",
		  Prim_HCBPB_empty_p, 0, 0,
 "()\n\
 Returns a boolean indicating whether or not the profile buffer for\n\
 heathen (i.e., unpurified) code blocks is empty.\
 ")
{
 PRIMITIVE_HEADER(0);
 PRIMITIVE_RETURN(BOOLEAN_TO_OBJECT (HCBPB_next_empty_slot_index == 0));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("PURIFIED-CODE-BLOCK-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX", 
		  Prim_PCBPB_next_empty_slot_index, 0, 0,
  "()\n\
 Returns the index of the next `free' slot of the profile buffer for\n\
 purified code blocks.\
 ")
{
 PRIMITIVE_HEADER(0);
 PRIMITIVE_RETURN(ulong_to_integer(PCBPB_next_empty_slot_index));
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("HEATHEN-CODE-BLOCK-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX", 
		  Prim_HCBPB_next_empty_slot_index, 0, 0,
  "()\n\
 Returns the index of the next `free' slot of the profile buffer for\n\
 heathen (i.e., unpurified) code blocks.\
 ")
{
 PRIMITIVE_HEADER(0);
 PRIMITIVE_RETURN(ulong_to_integer(HCBPB_next_empty_slot_index));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PURIFIED-CODE-BLOCK-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX/RESET",
		  Prim_PCBPB_next_empty_slot_index_reset, 0, 0,
  "()\n\
  Resets the index of the next `free' slot of the profile buffer for\n\
  purified code blocks.\
  \n\
  Only officially designated wizards should even think of using this\n\
  super secret primitive. FNORD!\
  ")
{
 PRIMITIVE_HEADER(0);
 PCBPB_next_empty_slot_index = ((unsigned long) 0);
 PRIMITIVE_RETURN(UNSPECIFIC);
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%HEATHEN-CODE-BLOCK-PROFILE-BUFFER/NEXT-EMPTY-SLOT-INDEX/RESET",
		  Prim_HCBPB_next_empty_slot_index_reset, 0, 0,
 "()\n\
  Resets the index of the next `free' slot of the profile buffer for\n\
  heathen (i.e., unpurified) code blocks.\
  \n\
  Only officially designated wizards should even think of using this\n\
  super secret primitive. FNORD!\
  ")
{
 PRIMITIVE_HEADER(0);
 HCBPB_next_empty_slot_index = ((unsigned long) 0);
 PRIMITIVE_RETURN(UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-FLUSH-IMMEDIATE?",
		  Prim_pc_sample_PCBPB_flush_immediate_p, 0, 0,
 "()\n\
 Specifies whether the Purified Code Block Profile Buffer is flushed upon\n\
 each entry.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_flush_immediate)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-FLUSH-IMMEDIATE?",
		  Prim_pc_sample_HCBPB_flush_immediate_p, 0, 0,
 "()\n\
 Specifies whether the  Heathen Code Block Profile Buffer is flushed upon\n\
 each entry.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_flush_immediate)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-FLUSH-IMMEDIATE?/TOGGLE!",
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
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_flush_immediate = (! (PCBPB_flush_immediate)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_flush_immediate)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-FLUSH-IMMEDIATE?/TOGGLE!",
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
 ")
{
  PRIMITIVE_HEADER(0);
  IPPB_flush_immediate = (! (HCBPB_flush_immediate)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_flush_immediate)) ;
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-DEBUGGING?",
		  Prim_pc_sample_PCBPB_debugging_p, 0, 0,
 "()\n\
 Specifies whether the Purified Code Block Profile Buffer is in debugging mode.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_debugging)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-DEBUGGING?",
		  Prim_pc_sample_HCBPB_debugging_p, 0, 0,
 "()\n\
 Specifies whether the  Heathen Code Block Profile Buffer is in debugging mode.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_debugging)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-DEBUGGING?/TOGGLE!",
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
 ")
{
  PRIMITIVE_HEADER(0);
  PCBPB_debugging = (! (PCBPB_debugging)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_debugging)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-DEBUGGING?/TOGGLE!",
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
 ")
{
  PRIMITIVE_HEADER(0);
  HCBPB_debugging = (! (HCBPB_debugging)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_debugging)) ;
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-MONITORING?",
		  Prim_pc_sample_PCBPB_monitoring_p, 0, 0,
 "()\n\
 Specifies whether the PCBPB is in monitoring mode.\n\
 \n\
 This, for instance, is how a count of buffer overflows is accumulated.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_monitoring)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-MONITORING?",
		  Prim_pc_sample_HCBPB_monitoring_p, 0, 0,
 "()\n\
 Specifies whether the HCBPB is in monitoring mode.\n\
 \n\
 This, for instance, is how a count of buffer overflows is accumulated.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_monitoring)) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-MONITORING?/TOGGLE!",
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
 ")
{
  PRIMITIVE_HEADER(0);
  PCBPB_monitoring = (! (PCBPB_monitoring)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (PCBPB_monitoring)) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-MONITORING?/TOGGLE!",
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
 ")
{
  PRIMITIVE_HEADER(0);
  HCBPB_monitoring = (! (HCBPB_monitoring)) ;
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (HCBPB_monitoring)) ;
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-FLUSH-COUNT",
		  Prim_pc_sample_PCBPB_flush_count, 0, 0,
 "()\n\
 Returns the number of PCBPB flush requests that have been issued since the\n\
 last PC-SAMPLE/PCBPB-FLUSH-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (PCBPB_flush_count));
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-FLUSH-COUNT",
		  Prim_pc_sample_HCBPB_flush_count, 0, 0,
 "()\n\
 Returns the number of HCBPB flush requests that have been issued since the\n\
 last PC-SAMPLE/HCBPB-FLUSH-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (HCBPB_flush_count));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-FLUSH-COUNT/RESET",
		  Prim_pc_sample_PCBPB_flush_count_reset, 0, 0,
 "()\n\
 Resets the PCBPB flush count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  PCBPB_flush_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-FLUSH-COUNT/RESET",
		  Prim_pc_sample_HCBPB_flush_count_reset, 0, 0,
 "()\n\
 Resets the HCBPB flush count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  HCBPB_flush_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-EXTEND-COUNT",
		  Prim_pc_sample_PCBPB_extend_count, 0, 0,
 "()\n\
 Returns the number of PCBPB extend requests that have been issued since the\n\
 last PC-SAMPLE/PCBPB-EXTEND-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (PCBPB_extend_count));
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-EXTEND-COUNT",
		  Prim_pc_sample_HCBPB_extend_count, 0, 0,
 "()\n\
 Returns the number of HCBPB extend requests that have been issued since the\n\
 last PC-SAMPLE/HCBPB-EXTEND-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (HCBPB_extend_count));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-EXTEND-COUNT/RESET",
		  Prim_pc_sample_PCBPB_extend_count_reset, 0, 0,
 "()\n\
 Resets the PCBPB extend count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  PCBPB_extend_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-EXTEND-COUNT/RESET",
		  Prim_pc_sample_HCBPB_extend_count_reset, 0, 0,
 "()\n\
 Resets the HCBPB extend count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  HCBPB_extend_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-OVERFLOW-COUNT",
		  Prim_pc_sample_PCBPB_overflow_count, 0, 0,
 "()\n\
 Returns the number of PCBPB overflows that have been issued since the last\n\
 PC-SAMPLE/PCBPB-OVERFLOW-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\n\
 \n\
 Each overflow indicates a sample that was punted into the bit bucket.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (PCBPB_overflow_count));
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-OVERFLOW-COUNT",
		  Prim_pc_sample_HCBPB_overflow_count, 0, 0,
 "()\n\
 Returns the number of HCBPB overflows that have been issued since the last\n\
 PC-SAMPLE/HCBPB-OVERFLOW-COUNT/RESET was issued (or since booting if no\n\
 resets issued).\n\
 \n\
 Each overflow indicates a sample that was punted into the bit bucket.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN(ulong_to_integer (HCBPB_overflow_count));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-OVERFLOW-COUNT/RESET",
		  Prim_pc_sample_PCBPB_overflow_count_reset, 0, 0,
 "()\n\
 Resets the PCBPB overflow count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  PCBPB_overflow_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-OVERFLOW-COUNT/RESET",
		  Prim_pc_sample_HCBPB_overflow_count_reset, 0, 0,
 "()\n\
 Resets the HCBPB overflow count (obviously... sheesh!).\
 ")
{
  PRIMITIVE_HEADER(0);
  HCBPB_overflow_count = ((unsigned long) 0);
  PRIMITIVE_RETURN(UNSPECIFIC);
}

/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/PCBPB-EXTRA-INFO",
		  Prim_pc_sample_PCBPB_extra_info, 0, 0,
 "()\n\
 Returns the extra info entry associated with the Purified Code Block\n\
 Profile Buffer.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (PCBPB_extra_info) ;
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/HCBPB-EXTRA-INFO",
		  Prim_pc_sample_HCBPB_extra_info, 0, 0,
 "()\n\
 Returns the extra info entry associated with the  Heathen Code Block\n\
 Profile Buffer.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (HCBPB_extra_info) ;
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/SET-PCBPB-EXTRA-INFO!",
		  Prim_pc_sample_set_PCBPB_extra_info, 1, 1,
 "(object)\n\
 Stores OBJECT in the extra info entry of the Purified Code Block\n\
 Profile Buffer.\n\
 \n\
 This is for mondo bizarro sampler frobnication purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(1);
  PCBPB_extra_info = ARG_REF(1);
  PRIMITIVE_RETURN (UNSPECIFIC);
}
/*...........................................................................*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/SET-HCBPB-EXTRA-INFO!",
		  Prim_pc_sample_set_HCBPB_extra_info, 1, 1,
 "(object)\n\
 Stores OBJECT in the extra info entry of the  Heathen Code Block\n\
 Profile Buffer.\n\
 \n\
 This is for mondo bizarro sampler frobnication purposes only.\n\
 \n\
 Only officially designated moby wizards should even think of thinking of\n\
 using this most ultra super duper secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(1);
  HCBPB_extra_info = ARG_REF(1);
  PRIMITIVE_RETURN (UNSPECIFIC);
}
/*---------------------------------------------------------------------------*/

/*****************************************************************************/
#define pc_sample_record_cobl(trinfo, buffer_state) do			      \
{									      \
  /* pc_info_1 = code block						      \
   * pc_info_2 = offset into block					      \
   */									      \
									      \
  SCHEME_OBJECT	 block = (trinfo -> pc_info_1) ;			      \
  SCHEME_OBJECT offset = (trinfo -> pc_info_2) ;			      \
									      \
  /* Hurumph... since the lambda may never have been hashed (and trap	      \
   * handlers are forbidden to do the CONSing necessary to generate new hash  \
   * numbers), and since there is no microcode/scheme interface for hashing   \
   * microcode objects (i.e., C data) anyway, we just pass the buck up to the \
   * interrupt handler mechanism: interrupt handlers are called at delicately \
   * perspicatious moments so they are permitted to CONS. This buck is passed \
   * by buffering lambdas until we have enough of them that it is worth issu- \
   * ing a request to spill the buffer into the lambda hashtable. For more    \
   * details, see pcsiproc.scm in the runtime directory.		      \
   */									      \
									      \
  pc_sample_record_bi_buffer_entry (block, offset, buffer_state) ;	      \
									      \
} while (FALSE)

/*---------------------------------------------------------------------------*/
static void
DEFUN (pc_sample_record_purified_cobl, (trinfo), struct trap_recovery_info * trinfo)
{
  pc_sample_record_cobl (trinfo, &purified_cobl_profile_buffer_state) ;

#if (	defined(PCS_LOG)	/* Sample console logging */		      \
     || defined(PCS_LOG_COBL)						      \
     || defined(PCS_LOG_PURE_COBL)					      \
     )
  log_cobl_sample (trinfo) ;
#endif

}
/*---------------------------------------------------------------------------*/
static void
DEFUN (pc_sample_record_heathen_cobl, (trinfo), struct trap_recovery_info * trinfo)
{
  pc_sample_record_cobl (trinfo, & heathen_cobl_profile_buffer_state) ;

#if (	defined(PCS_LOG)	/* Sample console logging */		      \
     || defined(PCS_LOG_COBL)						      \
     || defined(PCS_LOG_HEATHEN_COBL)					      \
     )
  log_cobl_sample (trinfo) ;
#endif

}




/*****************************************************************************/
#endif /* REALLY_INCLUDE_PROFILE_CODE */
