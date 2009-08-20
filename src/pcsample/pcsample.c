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

/* PCSAMPLE.C -- defines the PC Sample subroutines for UNIX implementations */

/*****************************************************************************/
#ifdef REALLY_INCLUDE_PROFILE_CODE /* scan_defines concession */

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*\
 * TODO:
 *
 *  - The mumble_index func ptrs can be avoided via macro passing?!
 *  - Maybe macro-ize/in-line code:
 *	PC_SAMPLE
 *	PC_SAMPLE_RECORD
 *	PC_SAMPLE_UPDATE_BI_BUFFER (after merging out paranoia & verbosity)
 *	PC_SAMPLE_RECORD_TABLE_ENTRY and some others?
 *      PC_SAMPLE_SPILL_GC_SAMPLES_INTO_PRIMITIVE_TABLE
 *
\*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/


#include <microcode/ux.h>		/* UNIX bullocks               */
#include <microcode/osenv.h>		/* For profile_timer_set/clear */
#include <microcode/config.h>		/* For TRUE/FALSE & true/false */
#include <microcode/scheme.h>
#include <microcode/uxtrap.h>		/* UNIX trap handlers         */
#include <microcode/uxsig.h>		/* For DEFUN_STD_HANDLER */
#include <microcode/prims.h>		/* For DEFINE_PRIMITIVE */
#include <microcode/cmpintmd.h> 	/* Compiled code interface macros */

#ifdef HAVE_ITIMER		   /* No interrupt timer ==> no PC sampling */

/*****************************************************************************
 * Very crude, brute force enable/disable key switch ... KERCHUNK! Debuggery */

static volatile Boolean pc_sample_halted = true ;
static volatile clock_t profile_interval =    0 ; /* one-shot interval */

/*---------------------------------------------------------------------------*/
static void
DEFUN (OS_pc_sample_timer_set, (first, interval),
       clock_t first AND
       clock_t interval)
{
  /* The profile trap handler will issue another one-shot triggering
   * of the prof timer once it has handled the pending profile request.
   * This assures that the profile interval cannot be so small as
   * to cause PROF triggers to deluge the system.
   */

  Tsignal_handler_result sighnd_profile() ; /* See uxtrap.c section */

  {
    OS_profile_timer_clear ();    /* ``Cease fire!'' while reset */
    pc_sample_halted = false;     /* clear internal state flag */
    profile_interval = interval;  /* trap handler re-arms @ interval */
    activate_handler (SIGPROF, ((Tsignal_handler) sighnd_profile));
                                  /* in case deactivated */
    OS_profile_timer_set (first, ((clock_t) 0)); /* Open fire! (one shot) */
  }

#if (   defined(PCS_LOG_TIMER_DELTA) /* Profile gestalt debuggery */	      \
     || defined(PCS_LOG_TIMER_SET)					      \
     )
  outf_console ("0x%x  ", profile_interval) ;
  outf_flush_console () ;
#endif
}

static void
DEFUN_VOID (OS_pc_sample_timer_clear)
{
  long old_mask = sigblock (sigmask (SIGPROF));	/* atomic wrt sigprof */
  { 
    OS_profile_timer_clear ()       ; /* ``Cease fire!'' */
    deactivate_handler (SIGPROF)    ; /* disable handler */
    pc_sample_halted = true         ; /* set internal state flag */
    profile_interval = ((clock_t) 0); /* disable re-triggers too */
  } 
  (void) sigblock (old_mask) ;                 	/* end atomic wrt sigprof */

#if (   defined(PCS_LOG_TIMER_DELTA) /* Profile gestalt debuggery */	      \
     || defined(PCS_LOG_TIMER_CLEAR)					      \
     )
  outf_console ("-\n") ;
  outf_flush_console () ;
#endif  

}


/*****************************************************************************/
#if !defined(HAVE_SIGCONTEXT) || !defined(HAS_COMPILER_SUPPORT) || defined(USE_STACKLETS)
/*---------------------------------------------------------------------------*/

static void
DEFUN (profile_trap_handler, (scp), struct FULL_SIGCONTEXT * scp)
{
  /* Cannot recover PC w/o sigcontext (?) so nothing to sample */

#ifndef PCS_TACIT_NO_TRAP
  outf_error ("\nProfile trap handler called but is non-existent.\n") ;
  outf_flush_error () ;
#endif

  return;
}

#else  /* HAVE_SIGCONTEXT and HAS_COMPILER_SUPPORT and not USE_STACKLETS     */

/* Timezones
 * These timezones are different to the ones in the microcode.  The basic support here allows them to be
 */

#define INITIAL_ZONE_LIMIT 10
static int current_zone = 0;
static int max_zone = INITIAL_ZONE_LIMIT;

static double initial_zone_buffer[INITIAL_ZONE_LIMIT] = {0.0};
static double *zones = initial_zone_buffer;

/* Invariant: 0 <= current_zone < max_zone */
/* Invariant: zones -> allocation of max_zone doubles */


#define essential_profile_trap_handler(scp)  do				      \
{									      \
  extern void EXFUN (pc_sample, (struct FULL_SIGCONTEXT *));		      \
  extern void EXFUN (zone_sample, ());				      \
									      \
  pc_sample (scp) ;		/* For now, profiler just PC samples */	      \
  zones[current_zone] += 1.0;   /* and zone sampling */                       \
  OS_pc_sample_timer_set(profile_interval,   /* launch another 1-shot */      \
			 profile_interval) ; /* at the same interval  */      \
} while (FALSE)


#ifndef PCS_TRAP_LOG		/* Sample debuggery */
#define real_profile_trap_handler(scp) essential_profile_trap_handler(scp)
#else
#define real_profile_trap_handler(scp)	do				      \
{									      \
  essential_profile_trap_handler(scp);					      \
  outf_console ("\n; Profile trap handler called while interval = %d.\n",     \
		profile_interval) ;					      \
  outf_flush_console () ;						      \
} while (FALSE)
#endif

static void
DEFUN (profile_trap_handler, (scp), struct FULL_SIGCONTEXT * scp)
{

#ifndef  PCS_TRAP_HANDLER_PARANOIA

  real_profile_trap_handler (scp) ;
  return;

#else /* PCS_TRAP_HANDLER_PARANOIA */

  if (   (! (pc_sample_halted))
      && (profile_interval != ((clock_t) 0)))
    real_profile_trap_handler (scp) ;

#ifndef PCS_TACIT_PUNT_BELATED	/* Sample debuggery */
  else if (profile_interval == ((clock_t) 0))
  {
    /* This shouldn't arise since now de-activate trap handler @ timer clear */
    outf_console ("\n\
                   \n;----------------------------------------------\
                   \n; Profile trap handler punted a belated sample.\
                   \n;----------------------------------------------\
                   \n\
                   \n") ;
    outf_flush_console () ;
  }
#endif

#ifndef PCS_TACIT_WIZARD_HALT	/* Sample gestalt debuggery */
  else if (pc_sample_halted)
  {
    /* Only official wizards should ever witness this. FNORD! */

    outf_console ("!") ;
    outf_flush_console ();
  }
#endif

#ifndef PCS_TACIT_MUSIC_MAN	/* Sample debuggery */
  else
  { 
    outf_error ("\n ; There's trouble, right here in Sample City.\n") ;
    outf_flush_error () ;
  }
#endif

#endif  /* PCS_TRAP_HANDLER_PARANOIA */
}

#endif /* HAVE_SIGCONTEXT and HAS_COMPILER_SUPPORT and not USE_STACKLETS */


DEFUN_STD_HANDLER (sighnd_profile,
  {
    profile_trap_handler (scp);
  })		   

DEFINE_PRIMITIVE ("PC-SAMPLE/TIMER-CLEAR", Prim_pc_sample_timer_clear, 0, 0,
  "()\n\
  Turn off the PC sample timer.\
  ")
{
  PRIMITIVE_HEADER (0);
  OS_pc_sample_timer_clear ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("PC-SAMPLE/TIMER-SET", Prim_pc_sample_timer_set, 2, 2,
  "(first interval)\n\
  Set the PC sample timer.\n\
  First arg FIRST says how long to wait until the first interrupt;\n\
  second arg INTERVAL says how long to wait between interrupts after that.\n\
  Both arguments are in units of milliseconds.\
  ")
{
  PRIMITIVE_HEADER (2);
  OS_pc_sample_timer_set ((arg_nonnegative_integer (1)),
			  (arg_nonnegative_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%PC-SAMPLE/HALTED?", Prim_pc_sample_halted_p, 0, 0,
 "()\n\
 Specifies whether PC sampling has been brute forcably disabled.\n\
 \n\
 Only officially designated wizards should even think of using this\n\
 super secret primitive. FNORD!\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (pc_sample_halted)) ;
}

DEFINE_PRIMITIVE ("%PC-SAMPLE/HALTED?/TOGGLE!",
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
 ")
{
  PRIMITIVE_HEADER(0);
  pc_sample_halted = (! (pc_sample_halted)) ;
  if (   (! (pc_sample_halted))
      && (profile_interval != ((clock_t) 0)))
    OS_pc_sample_timer_set(1, profile_interval) ; /* Throw the switch, Igor! */
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (pc_sample_halted)) ;
}

/*****************************************************************************
 * Mondo hack to keep track of where the primitive GARBAGE-COLLECT is so we
 *  can still sample GC calls during GC despite the PC_Sample_Primitive_Table
 *  can shift about
 *****************************************************************************/

long Garbage_Collect_Primitive_Index = -1;	/* installed later */

static void
DEFUN_VOID (pc_sample_cache_GC_primitive_index)
{
  SCHEME_OBJECT primitive = make_primitive("GARBAGE-COLLECT");
  Garbage_Collect_Primitive_Index = ((primitive != SHARP_F)
				     ? PRIMITIVE_NUMBER(primitive) : -1) ;
#ifdef PCS_LOG_GCI_CACHE
  outf_console ("\n  GC Index %d (0x%x)\n",
		Garbage_Collect_Primitive_Index,
		Garbage_Collect_Primitive_Index) ;
  outf_flush_console () ;
#endif

}

DEFINE_PRIMITIVE ("%PC-SAMPLE/CACHE-GC-PRIMITIVE-INDEX",
		  Prim_pc_sample_cache_GC_primitive_index, 0, 0,
 "()\n\
  Signals the microcode to go find the GARBAGE-COLLECT primitive and cache\n\
  away its index into the Primitive Table.\n\
  \n\
  This should be invoked each time the Primitive Table is altered in such a\n\
  way that existing primitives can shift about.\
 ")
{
  PRIMITIVE_HEADER(0);
  pc_sample_cache_GC_primitive_index();
  PRIMITIVE_RETURN(UNSPECIFIC);
}


static volatile Boolean pc_sample_within_GC_flag    = false;
static volatile double                   GC_samples = 0    ;

static void
DEFUN_VOID (pc_sample_spill_GC_samples_into_primitive_table)
{
  if (   (                     GC_samples !=  0) /* Something to tally       */
      && (Garbage_Collect_Primitive_Index != -1) /* Safe to tally GC samples */
     )
  {
    /* flush GC_samples into GARBAGE-COLLECT entry w/in Primitive Table */
    double * fpp
      = ((double *)
	 (MEMORY_LOC
	  ((VECTOR_REF((Get_Fixed_Obj_Slot(PC_Sample_Primitive_Table)),
		       Garbage_Collect_Primitive_Index)),
	   1))) ;
    (* fpp) = ((* fpp) + ((double) GC_samples)) ;
  }
  GC_samples = 0 ;		/* reset counter */
}

DEFINE_PRIMITIVE ("PC-SAMPLE/SPILL-GC-SAMPLES-INTO-PRIMITIVE-TABLE",
		  Prim_pc_sample_spill_GC_samples_into_primitive_table, 0, 0,
 "()\n\
  Make sure all samples taken during GC are present and accounted for in the\n\
  Primitive Sample Table.\
 ")
{
  PRIMITIVE_HEADER(0);
  pc_sample_spill_GC_samples_into_primitive_table();
  PRIMITIVE_RETURN(UNSPECIFIC);
}


static void
DEFUN_VOID (pc_sample__pre_gc_gc_synch_hook)
{
  pc_sample_within_GC_flag = true; 	/* will count samples during GC */
}

static void
DEFUN_VOID (pc_sample_post_gc_gc_synch_hook)
{
  if ((Get_Fixed_Obj_Slot(PC_Sample_Primitive_Table)) != SHARP_F) /* enabled */
    pc_sample_spill_GC_samples_into_primitive_table() ;
  pc_sample_within_GC_flag = false;
  /***************************************************************************
   * Moby hack: may still get a few samples after this hook runs but they will
   * not be lost since we reset the counter *after* GC appears to be over, not
   * at the beginning of the next GC. Thus, eventually these GCs will be coun-
   * ted, just not necessarily right away. To be sure, however, that they get
   * appropriately charged to the current sample run, we will manually call
   * this hook whenever we try to access the primitive table in runtime code.
   ***************************************************************************/
}

/****************************************************************************
 *  Following debuggery was used to isolate bug with unwarranted samples.   *
 ****************************************************************************/
static Boolean
DEFUN (repugnant_sample_block_addr_p, (block_addr), SCHEME_OBJECT * block_addr)
{
  /*    If you uncomment the next lines, add 0x10+ to each constant below  */
  /* outf_error ("Block addr = %lx\n", ((unsigned long) block_addr));
     outf_flush_error () ;
   */
  return (   (((unsigned long) block_addr) == 0x411F60FC) /* IPPB/flush      */
	  || (((unsigned long) block_addr) == 0x411EEBD0) /* IPPB/need2flush?*/
	  || (((unsigned long) block_addr) == 0x410C6A94) /* name->package   */
	  || (((unsigned long) block_addr) == 0x410EB880) /* package/child   */
	  || (((unsigned long) block_addr) == 0x410AEB24) /* ->environment   */
	  ); /*                               block-off+0x40000000           */
}

static void			/* debuggery hook */
DEFUN (flame_block, (block_addr), SCHEME_OBJECT * block_addr)
{
  if (pc_sample_halted)
    outf_console ("\n\nAAAHH!! 0x%x\n\n",((unsigned long) block_addr));
  else
    outf_console ("MADRE!! Bad ass = %lx ; P(h) = %d ; P(i) = %d\n",
		  ((unsigned long) block_addr),
		  pc_sample_halted,
		  profile_interval) ;

  outf_flush_console () ;
}

static struct trap_recovery_info *
DEFUN (find_sigcontext_ptr_pc, (scp, trinfo),
       struct FULL_SIGCONTEXT    * scp    AND
       struct trap_recovery_info * trinfo
       )
{
  /* Recover the PC from the signal context ptr.     */
  /* (Extracted from continue_from_trap in uxtrap.c) */

  long the_pc = ((FULL_SIGCONTEXT_PC (scp)) & PC_VALUE_MASK);

  int builtin_index;
  int utility_index;

  int pc_in_builtin;
  int pc_in_utility;
  int pc_in_C;
  int pc_in_heap;
  int pc_in_constant_space;
  int pc_in_scheme;
  int pc_in_hyper_space;

  if ((the_pc & PC_ALIGNMENT_MASK) != 0)
  {
    pc_in_builtin 	 = false;
    pc_in_utility 	 = false;
    pc_in_C       	 = false;
    pc_in_heap           = false;
    pc_in_constant_space = false;
    pc_in_scheme 	 = false;
    pc_in_hyper_space 	 =  true;
  }
  else
  {
    extern int EXFUN (pc_to_builtin_index, (unsigned long));
    extern int EXFUN (pc_to_utility_index, (unsigned long));

    builtin_index = (pc_to_builtin_index (the_pc));
    utility_index = (pc_to_utility_index (the_pc));

    pc_in_builtin        = (builtin_index != -1);
    pc_in_utility        = (utility_index != -1);    
    pc_in_heap           = (   (the_pc <  ((long) Heap_Top   ))
			    && (the_pc >= ((long) Heap_Bottom)));
    pc_in_constant_space = (   (the_pc <  ((long) Free_Constant ))
			    && (the_pc >= ((long) Constant_Space)));
    pc_in_scheme         = (   pc_in_heap
			    || pc_in_constant_space
			    || pc_in_builtin);
    /* This doesnt work for dynamically loaded libraries, e.g. libc.sl:
    pc_in_C              = (   (the_pc <= ((long) (get_etext ())))
			    && (!pc_in_builtin));
    */
    pc_in_C              = (   (!pc_in_scheme)
			    && (!pc_in_builtin));
    pc_in_hyper_space    = (   (! pc_in_C     )
			    && (! pc_in_scheme));
  }

  if (    pc_in_hyper_space
      || (pc_in_scheme && ALLOW_ONLY_C)) /* In hyper space. */
  {
    (trinfo -> state)           = STATE_UNKNOWN;
    (trinfo -> pc_info_1)       = 0; /* UFO[0]: Doesnt look like a primitive */
    (trinfo -> pc_info_2)       = the_pc;
    (trinfo -> extra_trap_info) = pc_in_hyper_space;
  }
  else if (pc_in_scheme)	        /* In compiled code. */
  {
    SCHEME_OBJECT * block_addr = (pc_in_builtin
				  ? ((SCHEME_OBJECT *) NULL)
				  : (find_block_address (((PTR) the_pc),
							 (pc_in_heap
							  ? Heap_Bottom
							  : Constant_Space))));
    if (block_addr != ((SCHEME_OBJECT *) NULL))
    {
      (trinfo -> state)           = STATE_COMPILED_CODE;
      (trinfo -> pc_info_1)       = /* code block */
	(MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block_addr));
      (trinfo -> pc_info_2)       = /* offset w/in block */
	(LONG_TO_UNSIGNED_FIXNUM (the_pc - ((long) block_addr)));
      (trinfo -> extra_trap_info) = pc_in_constant_space;
#ifdef PCS_LOG_REPUGNANCE
      if (repugnant_sample_block_addr_p (block_addr))
	flame_block (block_addr);
#endif
    }
    else if (pc_in_builtin)		/* In builtin */
    {
      (trinfo -> state)           = STATE_BUILTIN;
      (trinfo -> pc_info_1)       = builtin_index;
      (trinfo -> pc_info_2)       = SHARP_T;
      (trinfo -> extra_trap_info) = true;
    }
    else				/* In Probably Compiled frobby */
    {
      int prob_comp_index = (pc_in_constant_space ? 0 : 1) ;

      (trinfo -> state)           = STATE_PROBABLY_COMPILED;
      (trinfo -> pc_info_1)       = prob_comp_index;
      (trinfo -> pc_info_2)       = the_pc;
      (trinfo -> extra_trap_info) = pc_in_constant_space;
    }
  }
  else					/* pc_in_C */
  {
    /* In the interpreter, a primitive, or a compiled code utility. */

    SCHEME_OBJECT primitive = (Regs[REGBLOCK_PRIMITIVE]);

    if (pc_in_utility)			/* In Utility */
    {
      (trinfo -> state)           = STATE_UTILITY;
      (trinfo -> pc_info_1)       = utility_index;
      (trinfo -> pc_info_2)       = SHARP_F;
      (trinfo -> extra_trap_info) = false;
    }
    else if ((OBJECT_TYPE (primitive)) == TC_PRIMITIVE)	/* In Primitive */
    {
      (trinfo -> state)           = STATE_PRIMITIVE;
      (trinfo -> pc_info_1)       = (PRIMITIVE_NUMBER (primitive));
      (trinfo -> pc_info_2)       = primitive;
      (trinfo -> extra_trap_info) = true;
    }
    else				/* In Interpreted or In UFO ?!?!?!?! */
    {
      (trinfo -> state)           = STATE_UNKNOWN;
      (trinfo -> pc_info_1)       = 1; /* UFO[1]: Looked like a primitive */
      (trinfo -> pc_info_2)       = the_pc;
      (trinfo -> extra_trap_info) = primitive;
    }
  }
  return (trinfo) ;
}

/*****************************************************************************/
static SCHEME_OBJECT
DEFUN (pc_sample_flame_bad_table, (table_no, table), unsigned int  table_no AND
                                                     SCHEME_OBJECT table)
{
  outf_error ("\nPC sample table (0x%x) find fault: ", table_no);

  if (table_no >= NFixed_Objects)
    outf_error ("bad ucode band--- table out of range.") ;
  else if (! (VECTOR_P(table)))
    outf_error ("table was not a Scheme VECTOR.") ;
  else
    outf_error("Bloody mess, that!") ;

  outf_error ("\n") ;
  outf_flush_error () ;

  return (UNSPECIFIC) ;		/* Fault: signal UNSPECIFIC */
}

#ifndef PCS_TABLE_PARANOIA
#define pc_sample_find_table(table_no) Get_Fixed_Obj_Slot (table_no)
#else
#define pc_sample_find_table(table_no) do				      \
{									      \
  SCHEME_OBJECT table;							      \
									      \
  if (	 (table_no < NFixed_Objects)				/* in band? */\
      && ((table = (Get_Fixed_Obj_Slot (table_no))) != SHARP_F) /* enabled? */\
      && (VECTOR_P(table))					/*   valid? */\
     )				/* Success: return vector */		      \
    return (table) ;							      \
  else if (table == SHARP_F)	/* Disabled: percolate #F */		      \
    return (SHARP_F) ;							      \
  else				/* fault: lay blame */			      \
    pc_sample_flame_bad_table (table_no, table);			      \
} while (FALSE)
#endif	/* PCS_TABLE_PARANOIA */


static unsigned long
DEFUN (pc_sample_cc_block_index, (trinfo), struct trap_recovery_info * trinfo)
{
  /*  SCHEME_OBJECT block  = (trinfo -> pc_info_1);
   *  unsigned int  offset = (trinfo -> pc_info_2);
   */
  /* SOME DAY....
   * Compute unique ID for the entry in the code block as:
   * code_block_ID + index_of_current_cc_block_entry
   */
  /* MUCH LATER             CC_BLOCK_ID    (block_addr) +
   *       INDEX_OF_CURRENT_CC_BLOCK_ENTRY (block_addr, offset)) ;
   *
   * .... BUT UNTIL THAT DAY ARRIVES, just store a count
   */

  return((unsigned long) 0) ;
}

/*****************************************************************************/
static unsigned long
DEFUN (pc_sample_counter_index, (trinfo), struct trap_recovery_info * trinfo)
{
  /* For now, we just increment a single counter. Later a more exotic structure
   * may be maintained.. like discriminated counters and a real-time histogram.
   */

  return ((unsigned long) 0) ;
}

/*****************************************************************************/
static unsigned long
DEFUN (pc_sample_indexed_table_index, (trinfo), struct trap_recovery_info * trinfo)
{
  /* pc_info_1 = index into Mumble_Procedure_Table */

  return ((unsigned long) (trinfo -> pc_info_1)) ;
}

/*****************************************************************************/
static void
DEFUN (pc_sample_record_table_entry, (table, index), unsigned int  table  AND
                                                     unsigned long index)
{

#ifdef PCS_LOG_PUNTS		/* Punt warnings */
  if (pc_sample_halted)
  {
    outf_console
      ("\n; PC sample punted in the nick of time from table 0x%x[%d].\n",
       table, index) ;
    outf_flush_console () ;
  }
  else
#endif

  {
    /* For now, we just increment a counter. Later a more exotic structure
     * may be maintained here.. like a counter and a real-time histogram...
     */
    double * fpp = ((double *) (MEMORY_LOC ((VECTOR_REF (table, index)), 1)));
    
    (*fpp) += 1.0;
  }
}









/*****************************************************************************
 * Sample verbosity (console logging)...
 *****************************************************************************/

/*---------------------------------------------------------------------------*/
static void
DEFUN (log_cobl_sample, (trinfo), struct trap_recovery_info * trinfo)
{ 
  /* pc_info_1 = code block
   * pc_info_2 = offset into block
   * xtra_info = pc_in_constant_space
   */
  outf_console
    ("; PC Sampler encountered a Compiled FNORD! 0x%x (off = %d, P(c) = %d%%)\n",
     ((unsigned           long)(trinfo -> pc_info_1)       ),
     ( UNSIGNED_FIXNUM_TO_LONG((trinfo -> pc_info_2))      ),
     ((                    int)(trinfo -> extra_trap_info) )) ;
  outf_flush_console () ;
}
/*---------------------------------------------------------------------------*/
static void
DEFUN (log_prob_comp_sample, (trinfo), struct trap_recovery_info * trinfo)
{
  /* pc_info_2 = the_pc (long)
   * xtra_info = pc_in_constant_space
   */
  outf_console
    ("; PC Sampler stumbled into a Prob Comp FNORD! at addr 0x%x (P(c) = %d%%)\n",
     (trinfo -> pc_info_2), ((Boolean)(trinfo -> extra_trap_info))) ;
  outf_flush_console () ;
}

/*****************************************************************************
 * More Sample verbosity (console logging)...
 *****************************************************************************/

/*---------------------------------------------------------------------------*/
static void
DEFUN (log_UNKNOWN_STATE_sample, (trinfo), struct trap_recovery_info * trinfo)
{
  /* ``UNKNOWN_STATE'' samples are either interpreted procs or UFOs.
   *     Any way you look at it, you lose. What's that you say...?
   */
  outf_console
    ((((trinfo -> pc_info_1) == SHARP_T) /* pc_apparent_prim? */
      ? "; PC Sampler taught it taw a pwimitive...\
       \n; But it didn't.  It didn't taw a pwimitive."
      : (((trinfo -> extra_trap_info) == SHARP_T) /* dreaded hyper space */
	 /*------------------------------------------------------------------*/
	 ? "; **** WARNING! WARNING! DANGER, WILL ROBINSON! DANGER!       ****\
          \n; **** LOST IN HYPER SPACE! WE'RE DOOMED! DOOMED, I TELL YOU! ****\
          \n; **** ALL DOOMED!! OH, THE PAIN!! THE PAIN!!!                ****"
	 /*------------------------------------------------------------------*/
	 : "; PC Sampler had a close encounter with an Unidentifiable Functional Object\
          \n;  -- i.e., This is a UFO sighting!  Run for your life!!\
	  \n; ``You will be assimilated. Resistance is futile.''"))) ;
	 /*------------------------------------------------------------------*/
  outf_console ("\n") ;
}
/*---------------------------------------------------------------------------*/
static void
DEFUN (log_interp_proc_sample, (trinfo), struct trap_recovery_info * trinfo)
{ 
  /* pc_info_1 = pc_an_apparent_primitive
   * pc_info_2 = the_pc
   * extra_trap_info = /prim                if pc_info_1 = #T
   *                   \pc_in_hyper_space   otherwise
   */
  outf_console
    ("\n\
      \n;---------------------------------------------------------------------\
      \n; PC Sampler slogged down inside an interpreted bog\
      \n;    in Loch 0x%x at Glen 0x%x.",
     (trinfo -> pc_info_2),
     (trinfo -> extra_trap_info)) ;
  outf_console ("\n; The context was as follows:\n") ;
  log_UNKNOWN_STATE_sample (trinfo) ;
  outf_flush_console () ;
}
/*---------------------------------------------------------------------------*/
static void
DEFUN (log_UFO_sample, (trinfo), struct trap_recovery_info * trinfo)
{
  /* pc_info_1 = pc_an_apparent_primitive_flag
   * pc_info_2 = the_pc
   * xtra_info = /prim                if pc_info_1 = #T
   *             \pc_in_hyper_space   otherwise
   */
  outf_console
    ("\n\
      \n;---------------------------------------------------------------------\
      \n; BEGIN TRANSMISSION	\n;                   		\
      \n;          ^		\n;         ` `			\
      \n;      _ `   ` _    	\n;    \"  `     `  \"  	\
      \n;  \"   `       `   \"	\n;  \"   ` .` `. `   \"	\
      \n;    \" `` _ _ `` \"	\n;      `       `		\n;\
      \n; CAPTAINS'S LOG: ``UFO'' sighting at sector [0x%x] at warp [%d])\n",
     ((unsigned long)(trinfo -> pc_info_2)),
     (((trinfo -> pc_info_1) == SHARP_T)
      ? ((unsigned long) (trinfo -> extra_trap_info))  /* pwimitive   */
      : (      (Boolean) (trinfo -> extra_trap_info))) /* hyperspace? */
     ) ;
  log_UNKNOWN_STATE_sample (trinfo) ;
  outf_console
    ("\n\
      \n; END TRANSMISSION\
      \n;---------------------------------------------------------------------\
      \n") ;
  outf_flush_console () ;
}

/*****************************************************************************/
static void
DEFUN (pc_sample_update_table, (PC_Sample_Table, trinfo, index_func_ptr),
                                unsigned int PC_Sample_Table        AND
                                struct trap_recovery_info * trinfo  AND
                                unsigned long (* index_func_ptr)())
{
  SCHEME_OBJECT table = UNSPECIFIC;
  unsigned long index;

#if (   defined(PCS_LOG)		/* Sample logging */		      \
     || defined(PCS_LOG_PROB_COMP)					      \
     )
  if (PC_Sample_Table == PC_Sample_Prob_Comp_Table)
    log_prob_comp_sample (trinfo) ;
#endif

#if (   defined(PCS_LOG)		/* Sample logging */		      \
     || defined(PCS_LOG_UFO)						      \
     )
  if (PC_Sample_Table == PC_Sample_UFO_Table)
    log_UFO_sample (trinfo) ;
#endif

  if ((table = pc_sample_find_table (PC_Sample_Table)) == SHARP_F)
  {
    /* Samples of this type are disabled, so drop the sample on the floor */
    /* for now... later count drops */
    return;
  }
  else
  {
    index = ((* index_func_ptr)(trinfo)) ;

#ifdef PCS_TABLE_PARANOIA
    if (   (VECTOR_P     (table)        )
	&& (VECTOR_LENGTH(table) > index)
	)
    {
#endif	/* ------------------------------ PARANOIA OVERDRIVE --------------. */
    									/* | */
      if (   (PC_Sample_Table == PC_Sample_Primitive_Table)		/* | */
	  && (	        index == Garbage_Collect_Primitive_Index)	/* | */
	  )								/* | */
	/* Yow! The primitives sample table will be moved by the GC   *//* | */
	/*      so storing into it can lose by storing into the old   *//* | */
	/*      (broken heart) address.				      *//* | */
	/*							      *//* | */
	/*      To avoid this, we keep a count of GC samples until    *//* | */
	/*      the GC is over then add the GC_samples to the GC      *//* | */
	/*      primitive's sample entry.			      *//* | */
	/*							      *//* | */
	/*      GJR installed gc_hooks for just this purpose.	      *//* | */
	/*      Damned sporting of him, I must say.		      *//* | */
	/*							      *//* | */
	GC_samples += 1;						/* | */
      else								/* | */
	(pc_sample_record_table_entry (table, index)) ;			/* | */
    									/* | */
#ifdef PCS_TABLE_PARANOIA /* <----------- PARANOIA OVERDRIVE --------------' */
    }
    else if (VECTOR_P(table))	  /* index was out of range */
    {
      outf_error
	("\nPC sample table (0x%x) update fault: index out of range-- %d >= %d.\n",
	 PC_Sample_Table, index, (VECTOR_LENGTH(table))) ;
      outf_flush_error () ;
    }
    else if (table == UNSPECIFIC) /* fault */
      return; /* Let it slide: already flamed about it in finder. */
    else 			  /* something's broken */
    {
      outf_error ("\nPC sample find table do a poo-poo, do a poo-poo.\n") ;
      outf_flush_error () ;
    }
#endif  /*  PCS_TABLE_PARANOIA */
  }
}

/*****************************************************************************/
struct profile_buffer_state
{
	 char * name;			/* name string */

  unsigned  int ID;		        /* indices into the Fixed Obj Vector */
  unsigned  int ID_aux;		        /*    ... for the buffer(s)          */

	Boolean enabled_flag;	        /* the buffer qua buffer, as it were */
  SCHEME_OBJECT buffer;
  SCHEME_OBJECT buffer_aux;
  unsigned long length;
  unsigned long next_empty_slot_index;

  unsigned long slack;			/* flush/extend nearness thresholds */
	   long slack_increment;

  unsigned  int	 flush_INT;		/* Interrupt request bits */
  unsigned  int extend_INT;

	Boolean	   flush_noisy_flag; 	/* verbosity flags for monitoring    */
	Boolean	  extend_noisy_flag;    /*  ... buffer parameter performance */
        Boolean overflow_noisy_flag;

	Boolean	 flush_immed_flag;	/* debuggery hook */

	Boolean        debug_flag; 	/* random hook */
	Boolean      monitor_flag; 	/* random hook */

  unsigned long    flush_count;		/* Counts for performance monitoring */
  unsigned long   extend_count;
  unsigned long overflow_count;

  SCHEME_OBJECT extra_buffer_state_info; /* etc hook for future extensions */
};

/*****************************************************************************/
static void
DEFUN (init_profile_buffer_state, (pbs_ptr,
				   name, ID, ID_aux, slack, slack_increment,
				   flush_INT, extend_INT),
       struct profile_buffer_state * pbs_ptr	      AND
			      char * name	      AND
		       unsigned int  ID		      AND
		       unsigned int  ID_aux	      AND
		       unsigned long slack	      AND
				long slack_increment  AND
		       unsigned int   flush_INT	      AND
		       unsigned int  extend_INT)
{
  (pbs_ptr -> name)			= name;			/* arg */
  (pbs_ptr -> ID)			= ID;			/* arg */
  (pbs_ptr -> ID_aux)			= ID_aux;		/* arg */
  (pbs_ptr -> enabled_flag)		= false;
  (pbs_ptr -> buffer)			= UNSPECIFIC;
  (pbs_ptr -> buffer_aux)		= UNSPECIFIC;
  (pbs_ptr -> length)			= ((unsigned long) 0);
  (pbs_ptr -> next_empty_slot_index)	= ((unsigned long) 0);
  (pbs_ptr -> slack)			= slack;		/* arg */
  (pbs_ptr -> slack_increment)		= slack_increment;	/* arg */
  (pbs_ptr ->  flush_INT)		=  flush_INT;		/* arg */
  (pbs_ptr -> extend_INT)		= extend_INT;		/* arg */
  (pbs_ptr ->    flush_noisy_flag)	= false;
  (pbs_ptr ->   extend_noisy_flag)	= false;
  (pbs_ptr -> overflow_noisy_flag)	=  true;
  (pbs_ptr ->    flush_immed_flag)	= false;
  (pbs_ptr ->	       debug_flag)	= false; /* i.e. no count flush/xtnd */
  (pbs_ptr ->	     monitor_flag)	=  true; /* i.e. count buf overflows */
  (pbs_ptr ->    flush_count)		= ((unsigned long) 0);
  (pbs_ptr ->   extend_count)		= ((unsigned long) 0);
  (pbs_ptr -> overflow_count)		= ((unsigned long) 0);
  (pbs_ptr -> extra_buffer_state_info)	= SHARP_F;
}
/*---------------------------------------------------------------------------*/
#define init_profile_bi_buffer_state(pbs_ptr,				      \
				     name, ID, ID_aux, slack, slack_increment,\
				     flush_INT, extend_INT)		      \
	   init_profile_buffer_state(pbs_ptr,				      \
				     name, ID, ID_aux, slack, slack_increment,\
				     flush_INT, extend_INT)

#define init_profile_uni_buffer_state(pbs_ptr,				      \
				     name, ID,	       slack, slack_increment,\
				     flush_INT, extend_INT)		      \
	   init_profile_buffer_state(pbs_ptr,				      \
				     name, ID, false,  slack, slack_increment,\
				     flush_INT, extend_INT)
/*...........................................................................*\
|*. For example...							     *|
\*...........................................................................*/

static struct profile_buffer_state dummy_profile_buffer_state;

static void
DEFUN_VOID (init_dummy_profile_buffer_state)
{
  init_profile_buffer_state(&dummy_profile_buffer_state,
			    "PBS Fnord!",		/* name	     */
			    false,			/* ID	     */
			    false,			/* ID_aux    */
			    ((unsigned long) 0),	/* slack     */
			    ((         long) 0),	/* slack_inc */
			    ((unsigned  int) 0),	/* flush_INT */
			    ((unsigned  int) 0)		/* extnd_INT */
			    );
}
/*---------------------------------------------------------------------------*/

/*****************************************************************************/
static void
DEFUN (pc_sample_record_bi_buffer_entry, (entry, entry_aux, PBS),
       SCHEME_OBJECT entry                AND
       SCHEME_OBJECT entry_aux		  AND
       struct profile_buffer_state * PBS)
{
  /* Cache some useful state values */

  unsigned long buffer_length         = (PBS -> length               ) ;
  unsigned long next_empty_slot_index = (PBS -> next_empty_slot_index) ;

  if (next_empty_slot_index >= buffer_length)
  {
    (PBS -> next_empty_slot_index) = buffer_length - 1 ;
    if (PBS -> overflow_noisy_flag)
    {
      outf_error ("\n\nBloody Hell! The bloody %s bloody overflowed.\n",
		  (PBS -> name)) ;
      outf_flush_error () ;
    }
    if (PBS -> monitor_flag)
      (PBS -> overflow_count) += 1;
  }

#ifdef PCS_LOG_PUNTS		/* Punt warnings */
  else if (pc_sample_halted)
  {
    outf_console ("\n; PC sample %s entry punted in the nick of time.\n",
		  (PBS -> name)) ;
    outf_flush_console () ;

    return;
  }
#endif

  else
  {
    unsigned long next_index_plus_slack ;

    /* Cache some more useful state values */

    Boolean uni_buffer_flag = (! (PBS -> ID_aux)) ;

    SCHEME_OBJECT  buffer     = (PBS -> buffer    ) ;
    SCHEME_OBJECT  buffer_aux = (PBS -> buffer_aux) ;
    unsigned long slack       = (PBS -> slack     ) ;
    unsigned  int  flush_INT  = (PBS ->  flush_INT) ;
    unsigned  int extend_INT  = (PBS -> extend_INT) ;

    (  VECTOR_SET(buffer    , next_empty_slot_index, entry    )) ;
    if (! uni_buffer_flag)
      (VECTOR_SET(buffer_aux, next_empty_slot_index, entry_aux)) ;

    next_empty_slot_index += 1 ; 			     /* incr  cache */
    (PBS -> next_empty_slot_index) = next_empty_slot_index ; /* synch cache */

    next_index_plus_slack = next_empty_slot_index + slack ;

#ifdef PCS_FLUSH_DEBUGGERY	/* Flush debuggering */
    outf_console (";============================================\n") ;
    outf_console ("; name == %s\n", (PBS -> name)                  ) ;
    outf_console ("; ni+s == %d\n", next_index_plus_slack          ) ;
    outf_console ("; blen == %d\n", buffer_length                  ) ;
    outf_console ("; nmti == %d\n", next_empty_slot_index          ) ;
    outf_console ("; slak == %d\n", slack                          ) ;
    outf_console ("; BFQP == %d\n", INTERRUPT_QUEUED_P ( flush_INT)) ;
    outf_console ("; BFXP == %d\n", INTERRUPT_QUEUED_P (extend_INT)) ;
    outf_flush_console () ;
#endif


    /* ... continued on next page ... */

    /* ... pc_sample_record_bi_buffer_entry: continued from previous page... */

    /* Buffer Nearly Full (or unsigned overflow) ? */

    if (   (next_index_plus_slack > buffer_length) 	   /* nearfull */
	|| (next_index_plus_slack < next_empty_slot_index) /* overflow */
	|| (next_index_plus_slack < slack                ) /* overflow */
	|| (PBS -> flush_immed_flag) 			/* Flush debuggering */
	)
    { 
      if (! (INTERRUPT_QUEUED_P(flush_INT)))
      {
	REQUEST_INTERRUPT(flush_INT) ;
	if    (PBS -> flush_noisy_flag)
	{ outf_console ("\n;>>>>>>>>>  %s Flush Request issued.",
			(PBS -> name)) ;  outf_flush_console () ;
	}
	if   ((PBS -> debug_flag) && (PBS -> monitor_flag)) /* can monitor */
	  (PBS -> flush_count) += 1; 			    /*  in runtime */
      }
      else if (PBS -> flush_noisy_flag)
      { outf_console ("\n;>>  >>  >  %s Flush Request still queued.",
		      (PBS -> name)) ;  outf_flush_console () ;
      }
    }

    /* Buffer Full? */

    if (   (! (INTERRUPT_QUEUED_P (extend_INT)))
	&& (next_empty_slot_index >= buffer_length) 	/* > is PARANOIA */
	)
    { 
      int slack_inc_neg_p     ; /* Gonna cut the slack a little slack */
      unsigned long new_slack ; /*  to increase our margin of safety. */

      /* Cache one last useful state value */

      long slack_increment = (PBS -> slack_increment) ;

      /* Back up the next slot pointer so we don't go out of range */

      (PBS -> next_empty_slot_index) = buffer_length - 1 ;

      /* Increase slack to attempt to avoid additional overflows */

      slack_inc_neg_p = (slack_increment < 0) ;
      new_slack = (slack_inc_neg_p
		   ? slack - ((unsigned long) (- slack_increment))
		   : slack + ((unsigned long)    slack_increment )) ;

      if      (   slack_inc_neg_p  && (new_slack > slack)) 
	new_slack = 1     ;	/* unsigned underflow: min to 1 */
      else if ((! slack_inc_neg_p) && (new_slack < slack))
	new_slack = slack ;	/* unsigned  overflow: max to old value */

      (PBS -> slack) = new_slack ;

      /* Issue extend request */

      REQUEST_INTERRUPT (extend_INT) ;
      if     (PBS -> extend_noisy_flag)
      { outf_console ("\n;>>>>>>>>>  %s Extend Request issued.", 
		      (PBS -> name)) ;  outf_flush_console () ;
      }
      if    ((PBS -> debug_flag) && (PBS -> monitor_flag)) /* can monitor */
	(PBS -> extend_count) += 1; 			   /*  in runtime */
    }
    else if ((PBS -> extend_noisy_flag) && (INTERRUPT_QUEUED_P (extend_INT)))
    { outf_console ("\n;>>  >>  >  %s Extend Request still queued.",
		    (PBS -> name)) ;  outf_flush_console () ;
    }
  }
}
/*...........................................................................*/
#define FNORD UNSPECIFIC

#define pc_sample_record_buffer_entry(entry,        PBS) /* uni_buffer is a */\
     pc_sample_record_bi_buffer_entry(entry, FNORD, PBS) /* ...special case */

/*****************************************************************************/
static void
DEFUN (pc_sample_update_bi_buffer, (buffer_state, trinfo, record_func_ptr),
       struct profile_buffer_state * buffer_state  AND
       struct   trap_recovery_info * trinfo        AND
       void (* record_func_ptr)())
{
  /* Like interp-procs, wanna maintain a hashtable of instances encountered,
   * so we maintain a buffer and defer to an interrupt handler to flush and
   * extend the buffer as needed. Both the code block and the offset into the
   * code block are informative (since code blocks can contain multiple
   * definitions) so both are stored in synchronized buffers [i.e., slot N of
   * each of two buffers stores the Nth sampled code block and its associated
   * code block offset].
   *
   * Moreover, purified (non-relocateable) code blocks are distinguished from
   * non-purified (``heathen''?) code blocks since the GC can move the latter
   * around but not the former...meaning that purified ones can be hashed off
   * their addr/offset alone whereas heathens must be obj hashed (christened?).
   *
   * FOR PURIFIED CODE BLOCKS...
   * Win. Location is fixed so needn't sweat GC re-location
   * For now, buffer addr/offset pairs for later hashing.
   *
   * FOR HEATHEN CODE BLOCKS...
   * Sigh. GC can re-locate, so buffer SCHEME_OBJ ptr for hashing.
   * For now, buffer away the re-locatable addr & offset for later hashing.
   *
   * Once we arrange for the linker/loader to embed a hash code, we can just
   * use that instead of buffered add/offset pairs.
   */

#ifndef PCS_FOV_SNARK_HUNT

  if (buffer_state -> enabled_flag)
    ((* record_func_ptr)(trinfo)) ;
  else
  {
    /* Samples of this type are disabled, so drop the sample on the floor */
    /* for now... later count drops */
    return;
  }

  return;


	/* ... continued on next page ... */

  	/* ... pc_sample_update_bi_buffer: continued from previous page ... */



#else  /* PCS_FOV_SNARK_HUNT */

  Boolean uni_buffer_flag = (! (buffer_state -> ID_aux)) ;

  SCHEME_OBJECT buffer_1 =    (pc_sample_find_table (buffer_state -> ID    )) ;
  SCHEME_OBJECT buffer_2 = (uni_buffer_flag
			    ? SHARP_F 		/* treat as if disabled */
			    : (pc_sample_find_table (buffer_state -> ID_aux)));

  if (   (VECTOR_P (buffer_1))	        /* massive paranoia...           */
      && (uni_buffer_flag || (VECTOR_P (buffer_2)))
      && (buffer_state -> enabled_flag)	/* ... flag alone should suffice */
      )
    ((* record_func_ptr)(trinfo)) ;

  /* very paranoid debuggery... should just return now, no questions asked */

  else if (   (buffer_1 == SHARP_F   ) 	       /* buffer_1 disabled?       */
	   || (buffer_1 == UNSPECIFIC) 	       /* buffer_1 un-initialized  */
	   || (   (!  uni_buffer_flag) 	       /* regardez buffer_2?       */
	       && (   (buffer_2 == SHARP_F   ) /* buffer_2 disabled?       */
		   || (buffer_2 == UNSPECIFIC) /* buffer_2 un-initialized? */
		   )
	      )
	  )
  {

#ifdef PCS_PBS_ENABLE_PARANOIA			/* Paranoia */
    if (buffer_state -> enabled_flag)
    {
      outf_error ("\nSigh. %s looked enabled but is disabled.\n",
		  (buffer_state -> name)) ;
      outf_flush_error () ;
    }
#endif

    return;  /* Let it slide: find_table will have flamed if appropriate. */
  }
  else
  {
    outf_error ("\nThere's something rotten in the state of update_buffer\n") ;
    outf_flush_error () ;
  }

#endif  /* PCS_FOV_SNARK_HUNT */

}
/*...........................................................................*/

#define pc_sample_update_buffer(buffer_state, trinfo, record_func_ptr)        \
     pc_sample_update_bi_buffer(buffer_state, trinfo, record_func_ptr)/* aka */

/*****************************************************************************/
#include "pcsiproc.c"		/* (Interpreted) Interp-Proc sampling */
#include "pcscobl.c"		/*    (Compiled)  Code Block sampling */ 

#define VALID_PC_SAMPLE_ENV_P(env) ((OBJECT_TYPE (env) == TC_ENVIRONMENT))
/*****************************************************************************/
static void
DEFUN (pc_sample_record, (trinfo), struct trap_recovery_info * trinfo)
{

#ifdef PCS_LOG_PUNTS		/* Punt warnings */
  if (pc_sample_halted)
  {
    outf_console
      ("\n; PC sample punted at the last moment: HALTED flag set.\n");
    outf_flush_console ();
  }
  else
#endif

  {
    switch (trinfo -> state)
    {
      case STATE_BUILTIN:
           pc_sample_update_table (PC_Sample_Builtin_Table,     trinfo,
				   pc_sample_indexed_table_index);
	   break;
      case STATE_UTILITY:
	   pc_sample_update_table (PC_Sample_Utility_Table,     trinfo,
				   pc_sample_indexed_table_index);
	   break;
      case STATE_PRIMITIVE:
	   pc_sample_update_table (PC_Sample_Primitive_Table,   trinfo,
				   pc_sample_indexed_table_index);
	   break;
      case STATE_PROBABLY_COMPILED:
	   pc_sample_update_table (PC_Sample_Prob_Comp_Table,   trinfo,
				   pc_sample_indexed_table_index);
	   break;
      case STATE_COMPILED_CODE:
	   pc_sample_update_table (PC_Sample_Code_Block_Table,  trinfo,
				   pc_sample_cc_block_index);

	   /* Above line is a back door for future expansion...real code is: */

	   (((Boolean)(trinfo -> extra_trap_info)) /* pc_in_constant_space */
	    ? (pc_sample_update_bi_buffer (&purified_cobl_profile_buffer_state,
					   trinfo,
					   pc_sample_record_purified_cobl))
	    : (pc_sample_update_bi_buffer (& heathen_cobl_profile_buffer_state,
					   trinfo,
					   pc_sample_record_heathen_cobl))) ;
	   break;
      case STATE_UNKNOWN:  /* i.e., in interpreted code or in hyper space */
	   /* Hope we're in interpreted code and attempt to deduce the current
	    * interp-proc from the current active environment frame anyway.
	    * GJR suggested nabbing the current ENV to find the current PROC,
	    * warning that the current ENV may be invalid, e.g. in the middle
	    * of a LOAD.  In that case we are S.O.L., so record a UFO.  Sigh.
	    */
	   ((VALID_PC_SAMPLE_ENV_P (pc_sample_current_env_frame = Fetch_Env()))
	    ? pc_sample_update_buffer (&interp_proc_profile_buffer_state,
				       trinfo,
				       pc_sample_record_interp_proc)
	    : pc_sample_update_table  (PC_Sample_UFO_Table,
				       trinfo,
				       pc_sample_indexed_table_index)) ;
	   break;
    }
  }
}

/*****************************************************************************/
void
DEFUN (pc_sample, (scp), struct FULL_SIGCONTEXT * scp)
{

#ifdef PCS_LOG_PUNTS		/* Punt warnings */
  if (pc_sample_halted)
  {
    outf_console ("\n; PC sample called but punted due to halt flag.\n") ;
    outf_flush_console () ;
  }
  else
#endif

    if (pc_sample_within_GC_flag)
      GC_samples += 1;
    else
    {
      struct trap_recovery_info                        trinfo ;

      (pc_sample_record (find_sigcontext_ptr_pc (scp, &trinfo)));

#ifdef PCS_LOG			/* Sample logging */
      outf_console ("; PC sample called.\n") ;
      outf_flush_console () ;
#endif

    }
}

/*****************************************************************************/
static int
DEFUN_VOID (pc_sample_install_gc_synch_gc_hooks)
{
  static int stat = -1;		/* some clown may call this more than once */

  if (stat != 0)
  {
    if      ((stat =  add_pre_gc_hook(pc_sample__pre_gc_gc_synch_hook)) != 0)
      outf_error (";Could not add pre_gc GC synch hook. You.lose\n");

    else if ((stat = add_post_gc_hook(pc_sample_post_gc_gc_synch_hook)) != 0)
      outf_error (";Could not add post_gc GC synch hook. You.lose\n");

    else if ((stat = add_post_gc_hook(resynch_IPPB_post_gc_hook)) != 0)
      outf_error (";Could not add post GC IPPB re-synch hook. You.lose\n");

    else if ((stat = add_post_gc_hook(resynch_CBPBs_post_gc_hook)) != 0)
      outf_error (";Could not add post GC CBPB re-synch hook. You.lose\n");

    outf_flush_error () ;
  }
  return (stat);
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/INSTALL-GC-SYNCH-GC-HOOKS",
		  Prim_pc_sample_install_gc_synch_gc_hooks, 0, 0,
 "()\n\
  This must be called once when PC sampling is enabled.\n\
  \n\
  If it returns #F then PC sampling must be disabled.  You.lose\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((pc_sample_install_gc_synch_gc_hooks() == 0)));
}

/*****************************************************************************/
static void
DEFUN_VOID (pc_sample_disable_microcode)
{
    IPPB_disable ();		/* From pcsiproc.c */
   CBPBs_disable ();		/* From pcscobl.c  */
}
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
static void
DEFUN_VOID (pc_sample_init_profile_buffer_states)
{
  init_dummy_profile_buffer_state ();
   init_IPPB_profile_buffer_state ();
   init_CBPB_profile_buffer_states();
}
/*---------------------------------------------------------------------------*/
static int
DEFUN_VOID (pc_sample_install_microcode)
{
  static int stat = -1;		/* Some clown may call this more than once */

  if (stat != 0)
  {
    if (! (Valid_Fixed_Obj_Vector ())) /* Profile tables are in the FOV */
    {
      outf_error
	("\npc_sample_install_microcode encountered an invalid Fixed Obj Vector.\n") ;
      outf_flush_error () ;
    }
    else			/* safe to init */
    {
      pc_sample_cache_GC_primitive_index();

      pc_sample_init_profile_buffer_states();

      if ((stat = pc_sample_install_gc_synch_gc_hooks()) != 0) /* Once only! */
      {
	outf_error
	  ("; PC Sample GC synch GC hooks installation failed (0x%x)\n");
	outf_flush_error () ;
      }
      /* ... maybe more stuff here later ... */

      if (stat != 0)
      {
	outf_error ("; PC Sample installation failed.  You.lose\n");
	outf_flush_error () ;
      }
    }
  }
  return (stat);
}
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/INSTALL-MICROCODE",
		  Prim_pc_sample_install_microcode, 0, 0,
 "()\n\
  Installs the microcode support structures for PC sampling.\
 ")
{
  PRIMITIVE_HEADER(0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((pc_sample_install_microcode() == 0)));
}
/*---------------------------------------------------------------------------*/
DEFINE_PRIMITIVE ("%PC-SAMPLE/DISABLE-MICROCODE",
		  Prim_pc_sample_disable_microcode, 0, 0,
  "()\n\
  Disables the microcode support structures for PC sampling.\
 ")
{
  PRIMITIVE_HEADER(0);
  pc_sample_disable_microcode ();
  PRIMITIVE_RETURN (UNSPECIFIC) ;
}
/*****************************************************************************/

/* Zone operations

   These are not locked agains PC-Sampling activity but they are safe
   in the sense that they will at worst gain or lose a sample
*/

DEFINE_PRIMITIVE ("%PC-SAMPLE/SET-ZONE!",
		  Prim_pc_sample_set_current_zone, 1, 1,
  "(index)\n\
Set current pc-sampling zone to INDEX (a small exact integer), returning \
the previous value if different, else #F if same.")
{
    PRIMITIVE_HEADER(1);
    {
	int  old_zone = current_zone;
	int  new_zone = arg_index_integer (1, INITIAL_ZONE_LIMIT);
	if (old_zone == new_zone) {
	    PRIMITIVE_RETURN (SHARP_F);
	} else {
	    current_zone = new_zone;
	    PRIMITIVE_RETURN (LONG_TO_FIXNUM(old_zone));
	}
    }
}

DEFINE_PRIMITIVE ("%PC-SAMPLE/MAX-ZONE",
		  Prim_pc_sample_get_max_zone, 0, 0, 0)
{
    PRIMITIVE_HEADER(0);
    PRIMITIVE_RETURN(LONG_TO_FIXNUM(max_zone));
}

DEFINE_PRIMITIVE ("%PC-SAMPLE/CLEAR-ZONES!", Prim_pc_sample_clear_zones, 0, 0,
  "()\n\
Zero zone counts.")
{
    PRIMITIVE_HEADER (0);
    {
	int  i;
	for (i = 0; i < max_zone; i++) zones[i] = 0.0;
    }
    PRIMITIVE_RETURN(UNSPECIFIC);
}

DEFINE_PRIMITIVE ("%PC-SAMPLE/READ-ZONES!", Prim_pc_sample_read_zones, 1, 1,
  "(flonum-vector)\n\
Copy zone counts into FLONUM-VECTOR.  Returns the number copied, which \
is limited by either the number of zones to the capacity of FLONUM-VECTOR.")
{
    PRIMITIVE_HEADER (1);
    {
	SCHEME_OBJECT vector = (FLOATING_VECTOR_ARG (1));
	int length = FLOATING_VECTOR_LENGTH (vector);
	int limit = (length<max_zone) ? length : max_zone;
	int i;
	for (i = 0; i < limit; i++)
	  FLOATING_VECTOR_SET (vector, i, zones[i]);
	PRIMITIVE_RETURN (LONG_TO_FIXNUM(limit));	
    }
}

#endif /* HAVE_ITIMER */
#endif /* REALLY_INCLUDE_PROFILE_CODE */
