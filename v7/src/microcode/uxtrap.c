/* -*-C-*-

$Id: uxtrap.c,v 1.37 2003/02/14 18:28:24 cph Exp $

Copyright (c) 1990-2002 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "ux.h"
#include "uxtrap.h"
#include "uxutil.h"
#include "option.h"
#include "ostop.h"

extern CONST char * EXFUN (find_signal_name, (int signo));
extern void EXFUN (UX_dump_core, (void));
extern PTR initial_C_stack_pointer;

static enum trap_state trap_state;
static enum trap_state user_trap_state;

static enum trap_state saved_trap_state;
static int saved_signo;
static SIGINFO_T saved_info;
static struct FULL_SIGCONTEXT * saved_scp;

static void EXFUN (initialize_ux_signal_codes, (void));
static void EXFUN
  (continue_from_trap,
   (int signo, SIGINFO_T info, struct FULL_SIGCONTEXT * scp));

void
DEFUN_VOID (UX_initialize_trap_recovery)
{
  trap_state = trap_state_recover;
  user_trap_state = trap_state_recover;
  initialize_ux_signal_codes ();
}

enum trap_state
DEFUN (OS_set_trap_state, (state), enum trap_state state)
{
  enum trap_state old_trap_state = user_trap_state;
  user_trap_state = state;
  trap_state = state;
  return (old_trap_state);
}

static void
DEFUN_VOID (trap_normal_termination)
{
  trap_state = trap_state_exitting_soft;
  termination_trap ();
}

static void
DEFUN_VOID (trap_immediate_termination)
{
  trap_state = trap_state_exitting_hard;
  OS_restore_external_state ();
  exit (1);
}

static void
DEFUN_VOID (trap_dump_core)
{
  if (! (option_disable_core_dump))
    UX_dump_core ();
  else
    {
      fputs (">> Core dumps are disabled - Terminating normally.\n", stdout);
      fflush (stdout);
      termination_trap ();
    }
}

static void
DEFUN_VOID (trap_recover)
{
  if (WITHIN_CRITICAL_SECTION_P ())
    {
      CLEAR_CRITICAL_SECTION_HOOK ();
      EXIT_CRITICAL_SECTION ({});
    }
  reset_interruptable_extent ();
  continue_from_trap (saved_signo, saved_info, saved_scp);
}

void
DEFUN (trap_handler, (message, signo, info, scp),
       CONST char * message AND
       int signo AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp)
{
  int code = ((SIGINFO_VALID_P (info)) ? (SIGINFO_CODE (info)) : 0);
  Boolean stack_overflowed_p = (STACK_OVERFLOWED_P ());
  enum trap_state old_trap_state = trap_state;

  if (old_trap_state == trap_state_exitting_hard)
    _exit (1);
  else if (old_trap_state == trap_state_exitting_soft)
    trap_immediate_termination ();
  trap_state = trap_state_trapped;
  if (WITHIN_CRITICAL_SECTION_P ())
  {
    fprintf (stdout,
	     "\n>> A %s has occurred within critical section \"%s\".\n",
	     message, (CRITICAL_SECTION_NAME ()));
    fprintf (stdout, ">> [signal %d (%s), code %d]\n",
	     signo, (find_signal_name (signo)), code);
  }
  else if (stack_overflowed_p || (old_trap_state != trap_state_recover))
  {
    fprintf (stdout, "\n>> A %s has occurred.\n", message);
    fprintf (stdout, ">> [signal %d (%s), code %d]\n",
	     signo, (find_signal_name (signo)), code);
  }
  if (stack_overflowed_p)
  {
    fputs (">> The stack has overflowed overwriting adjacent memory.\n",
	   stdout);
    fputs (">> This was probably caused by a runaway recursion.\n", stdout);
  }
  fflush (stdout);

  switch (old_trap_state)
  {
  case trap_state_trapped:
    if ((saved_trap_state == trap_state_recover) ||
	(saved_trap_state == trap_state_query))
    {
      fputs (">> The trap occurred while processing an earlier trap.\n",
	     stdout);
      fprintf (stdout,
	       ">> [The earlier trap raised signal %d (%s), code %d.]\n",
	       saved_signo,
	       (find_signal_name (saved_signo)),
	       ((SIGINFO_VALID_P (saved_info))
		? (SIGINFO_CODE (saved_info))
		: 0));
      fputs (((WITHIN_CRITICAL_SECTION_P ())
	      ? ">> Successful recovery is extremely unlikely.\n"
	      : ">> Successful recovery is unlikely.\n"),
	     stdout);
      break;
    }
    else
      trap_immediate_termination ();
  case trap_state_recover:
    if ((WITHIN_CRITICAL_SECTION_P ()) || stack_overflowed_p)
    {
      fputs (">> Successful recovery is unlikely.\n", stdout);
      break;
    }
    else
    {
      saved_trap_state = old_trap_state;
      saved_signo = signo;
      saved_info = info;
      saved_scp = scp;
      trap_recover ();
    }
  case trap_state_exit:
    termination_trap ();

  default:
    break;
  }

  fflush (stdout);
  saved_trap_state = old_trap_state;
  saved_signo = signo;
  saved_info = info;
  saved_scp = scp;

  while (1)
  {
    static CONST char * trap_query_choices[] =
    {
      "D = dump core",
      "I = terminate immediately",
      "N = terminate normally",
      "R = attempt recovery",
      "Q = terminate normally",
      0
      };
    switch (userio_choose_option
	    ("Choose one of the following actions:",
	     "Action -> ",
	     trap_query_choices))
    {
    case 'I':
      trap_immediate_termination ();
    case 'D':
      trap_dump_core ();
    case '\0':
      /* Error in IO. Assume everything scrod. */
    case 'N':
    case 'Q':
      trap_normal_termination ();
    case 'R':
      trap_recover ();
    }
  }
}

struct ux_sig_code_desc
{
  int signo;
  unsigned long code_mask;
  unsigned long code_value;
  char *name;
};

static struct ux_sig_code_desc ux_signal_codes [64];

#define DECLARE_UX_SIGNAL_CODE(s, m, v, n)				\
{									\
  ((ux_signal_codes [i]) . signo) = (s);				\
  ((ux_signal_codes [i]) . code_mask) = (m);				\
  ((ux_signal_codes [i]) . code_value) = (v);				\
  ((ux_signal_codes [i]) . name) = (n);					\
  i += 1;								\
}

static void
DEFUN_VOID (initialize_ux_signal_codes)
{
  unsigned int i = 0;
  INITIALIZE_UX_SIGNAL_CODES ();
  DECLARE_UX_SIGNAL_CODE (0, 0, 0, ((char *) 0));
}

static SCHEME_OBJECT
DEFUN (find_signal_code_name, (signo, info, scp),
       int signo AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp)
{
  unsigned long code = 0;
  char * name = 0;
  if (SIGINFO_VALID_P (info))
    {
      code = (SIGINFO_CODE (info));
#ifdef SPECIAL_SIGNAL_CODE_NAMES
      SPECIAL_SIGNAL_CODE_NAMES ();
      if (name == 0)
#endif
	{
	  struct ux_sig_code_desc * entry = (& (ux_signal_codes [0]));
	  while ((entry -> signo) != 0)
	    if (((entry -> signo) == signo)
		&& (((entry -> code_mask) & code) == (entry -> code_value)))
	      {
		name = (entry -> name);
		break;
	      }
	    else
	      entry += 1;
	}
    }
  return (cons ((long_to_integer ((long) code)),
		((name == 0) ? SHARP_F
		 : (char_pointer_to_string ((unsigned char *) name)))));
}

static void
DEFUN (setup_trap_frame, (signo, info, scp, trinfo, new_stack_pointer),
       int signo AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp AND
       struct trap_recovery_info * trinfo AND
       SCHEME_OBJECT * new_stack_pointer)
{
  SCHEME_OBJECT handler = SHARP_F;
  SCHEME_OBJECT signal_name, signal_code;
  int stack_recovered_p = (new_stack_pointer != 0);
  long saved_mask = (FETCH_INTERRUPT_MASK ());
  SET_INTERRUPT_MASK (0);	/* To prevent GC for now. */
  if ((! (Valid_Fixed_Obj_Vector ())) ||
      ((handler = (Get_Fixed_Obj_Slot (Trap_Handler))) == SHARP_F))
    {
      fprintf (stderr, "There is no trap handler for recovery!\n");
      fflush (stderr);
      termination_trap ();
    }
  if (Free > MemTop)
  {
      Request_GC (0);
  }
  signal_name =
    ((signo == 0)
     ? SHARP_F
     : (char_pointer_to_string
	((unsigned char *) (find_signal_name (signo)))));
  signal_code = (find_signal_code_name (signo, info, scp));
  if (!stack_recovered_p)
    {
      INITIALIZE_STACK ();
     Will_Push (CONTINUATION_SIZE);
      Store_Return (RC_END_OF_COMPUTATION);
      exp_register = SHARP_F;
      Save_Cont ();
     Pushed ();
    }
  else
    sp_register = new_stack_pointer;
 Will_Push (7 + CONTINUATION_SIZE);
  STACK_PUSH (trinfo -> extra_trap_info);
  STACK_PUSH (trinfo -> pc_info_2);
  STACK_PUSH (trinfo -> pc_info_1);
  STACK_PUSH (trinfo -> state);
  STACK_PUSH (BOOLEAN_TO_OBJECT (stack_recovered_p));
  STACK_PUSH (signal_code);
  STACK_PUSH (signal_name);
  Store_Return (RC_HARDWARE_TRAP);
  exp_register = (long_to_integer (signo));
  Save_Cont ();
 Pushed ();
  if (stack_recovered_p
      /* This may want to do it in other cases, but this may be enough. */
      && (trinfo->state == STATE_COMPILED_CODE))
  {
    Stop_History ();
  }
  history_register = (Make_Dummy_History ());
 Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
  STACK_PUSH (signal_name);
  STACK_PUSH (handler);
  STACK_PUSH (STACK_FRAME_HEADER + 1);
 Pushed ();
  SET_INTERRUPT_MASK (saved_mask);
  abort_to_interpreter (PRIM_APPLY);
}

/* 0 is an invalid signal, it means a user requested reset. */

void
DEFUN (hard_reset, (scp), struct FULL_SIGCONTEXT * scp)
{
  continue_from_trap (0, 0, scp);
}

/* Called synchronously. */

void
DEFUN_VOID (soft_reset)
{
  struct trap_recovery_info trinfo;
  SCHEME_OBJECT * new_stack_pointer =
    (((sp_register <= Stack_Top) && (sp_register > Stack_Guard))
     ? sp_register
     : 0);
  if ((Registers[REGBLOCK_PRIMITIVE]) != SHARP_F)
    {
      (trinfo . state) = STATE_PRIMITIVE;
      (trinfo . pc_info_1) = (Registers[REGBLOCK_PRIMITIVE]);
      (trinfo . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (Registers[REGBLOCK_LEXPR_ACTUALS]));
      (trinfo . extra_trap_info) = SHARP_F;
    }
  else
    {
      (trinfo . state) = STATE_UNKNOWN;
      (trinfo . pc_info_1) = SHARP_F;
      (trinfo . pc_info_2) = SHARP_F;
      (trinfo . extra_trap_info) = SHARP_F;
    }
  if ((Free >= Heap_Top) || (Free < Heap_Bottom))
    /* Let's hope this works. */
    Free = MemTop;
  setup_trap_frame (0, 0, 0, (&trinfo), new_stack_pointer);
}

#if !defined(HAVE_STRUCT_SIGCONTEXT) || !defined(HAS_COMPILER_SUPPORT) || defined(USE_STACKLETS)

static struct trap_recovery_info dummy_recovery_info =
{
  STATE_UNKNOWN,
  SHARP_F,
  SHARP_F,
  SHARP_F
};

static void
DEFUN (continue_from_trap, (signo, info, scp),
       int signo AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp)
{
  if (Free < MemTop)
  {
    Free = MemTop;
  }
  setup_trap_frame (signo, info, scp, (&dummy_recovery_info), 0);
}

#else /* HAS_COMPILER_SUPPORT and not USE_STACKLETS */

/* Heuristic recovery from Unix signals (traps).

   continue_from_trap attempts to:

   1) validate the trap information (pc and sp);
   2) determine whether compiled code was executing, a primitive was
      executing, or execution was in the interpreter;
   3) guess what C global state is still valid; and
   4) set up a recovery frame for the interpreter so that debuggers can
      display more information. */

#include "gccode.h"

#define SCHEME_ALIGNMENT_MASK		((sizeof (long)) - 1)
#define STACK_ALIGNMENT_MASK		SCHEME_ALIGNMENT_MASK
#define FREE_PARANOIA_MARGIN		0x100

#define C_STACK_SIZE			0x01000000

static void
DEFUN (continue_from_trap, (signo, info, scp),
       int signo AND
       SIGINFO_T info AND
       struct FULL_SIGCONTEXT * scp)
{
  int pc_in_builtin;
  int builtin_index;
  int pc_in_C;
  int pc_in_heap;
  int pc_in_constant_space;
  int pc_in_scheme;
  int pc_in_hyper_space;
  int pc_in_utility;
  int utility_index;
  int scheme_sp_valid;
  long C_sp = (FULL_SIGCONTEXT_SP (scp));
  long scheme_sp = (FULL_SIGCONTEXT_SCHSP (scp));
  long the_pc = ((FULL_SIGCONTEXT_PC (scp)) & PC_VALUE_MASK);
  SCHEME_OBJECT * new_stack_pointer;
  SCHEME_OBJECT * xtra_info;
  struct trap_recovery_info trinfo;
  extern int EXFUN (pc_to_utility_index, (unsigned long));
  extern int EXFUN (pc_to_builtin_index, (unsigned long));

  if ((the_pc & PC_ALIGNMENT_MASK) != 0)
  {
    pc_in_builtin = 0;
    pc_in_utility = 0;
    pc_in_C = 0;
    pc_in_heap = 0;
    pc_in_constant_space = 0;
    pc_in_scheme = 0;
    pc_in_hyper_space = 1;
  }
  else
  {
    builtin_index = (pc_to_builtin_index (the_pc));
    pc_in_builtin = (builtin_index != -1);
    utility_index = (pc_to_utility_index (the_pc));
    pc_in_utility = (utility_index != -1);    
    pc_in_C = ((the_pc <= ((long) (get_etext ()))) && (!pc_in_builtin));
    pc_in_heap = ADDRESS_HEAP_P ((SCHEME_OBJECT*) the_pc);
    pc_in_constant_space = ADDRESS_CONSTANT_P ((SCHEME_OBJECT*) the_pc);
    pc_in_scheme = (pc_in_heap || pc_in_constant_space || pc_in_builtin);
    pc_in_hyper_space = ((!pc_in_C) && (!pc_in_scheme));
  }

  scheme_sp_valid =
    (pc_in_scheme
     && ((scheme_sp < ((long) Stack_Top)) &&
	 (scheme_sp >= ((long) Stack_Bottom)) &&
	 ((scheme_sp & STACK_ALIGNMENT_MASK) == 0)));

  new_stack_pointer =
    (scheme_sp_valid
     ? ((SCHEME_OBJECT *) scheme_sp)
     : (pc_in_C && (sp_register < Stack_Top)
	&& (sp_register > Stack_Bottom))
     ? sp_register
     : ((SCHEME_OBJECT *) 0));

  if (pc_in_hyper_space || (pc_in_scheme && ALLOW_ONLY_C))
  {
    /* In hyper space. */
    (trinfo . state) = STATE_UNKNOWN;
    (trinfo . pc_info_1) = SHARP_F;
    (trinfo . pc_info_2) = SHARP_F;
    new_stack_pointer = 0;
    if ((Free < MemTop) ||
	(Free >= Heap_Top) ||
	((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
      Free = MemTop;
  }
  else if (pc_in_scheme)
  {
    /* In compiled code. */
    SCHEME_OBJECT * block_addr;
#ifdef HAVE_FULL_SIGCONTEXT
    SCHEME_OBJECT * maybe_free;
#endif
    block_addr =
      (pc_in_builtin
       ? ((SCHEME_OBJECT *) NULL)
       : (find_block_address (((PTR) the_pc),
			      (pc_in_heap ? Heap_Bottom : Constant_Space))));
    if (block_addr != ((SCHEME_OBJECT *) NULL))
    {
      (trinfo . state) = STATE_COMPILED_CODE;
      (trinfo . pc_info_1) =
	(MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block_addr));
      (trinfo . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (the_pc - ((long) block_addr)));
    }
    else if (pc_in_builtin)
    {
      (trinfo . state) = STATE_BUILTIN;
      (trinfo . pc_info_1) = (LONG_TO_UNSIGNED_FIXNUM (builtin_index));
      (trinfo . pc_info_2) = SHARP_T;
    }
    else 
    {
      (trinfo . state) = STATE_PROBABLY_COMPILED;
      (trinfo . pc_info_1) = (LONG_TO_UNSIGNED_FIXNUM (the_pc));
      (trinfo . pc_info_2) = SHARP_F;
    }

    if ((block_addr == ((SCHEME_OBJECT *) NULL)) && (! pc_in_builtin))
    {
      if ((Free < MemTop) ||
	  (Free >= Heap_Top) ||
	  ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
	Free = MemTop;
    }
    else
    {
#ifdef HAVE_FULL_SIGCONTEXT
      maybe_free = ((SCHEME_OBJECT *) (FULL_SIGCONTEXT_RFREE (scp)));
      if (((((unsigned long) maybe_free) & SCHEME_ALIGNMENT_MASK) == 0)
	  && (maybe_free >= Heap_Bottom) && (maybe_free < Heap_Top))
	Free = (maybe_free + FREE_PARANOIA_MARGIN);
      else
#endif
	if ((Free < MemTop) || (Free >= Heap_Top)
	    || ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
	  Free = MemTop;
    }
  }

  else /* pc_in_C */
  {
    /* In the interpreter, a primitive, or a compiled code utility. */

    SCHEME_OBJECT primitive = (Registers[REGBLOCK_PRIMITIVE]);

    if (pc_in_utility)
    {
      (trinfo . state) = STATE_UTILITY;
      (trinfo . pc_info_1) = (LONG_TO_UNSIGNED_FIXNUM (utility_index));
      (trinfo . pc_info_2) = UNSPECIFIC;
    }
    else if ((OBJECT_TYPE (primitive)) != TC_PRIMITIVE)
    {
      (trinfo . state) = STATE_UNKNOWN;
      (trinfo . pc_info_1) = SHARP_F;
      (trinfo . pc_info_2) = SHARP_F;
      new_stack_pointer = 0;
    }
    else
    {
      (trinfo . state) = STATE_PRIMITIVE;
      (trinfo . pc_info_1) = primitive;
      (trinfo . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (Registers[REGBLOCK_LEXPR_ACTUALS]));
    }
    if ((new_stack_pointer == 0)
	|| ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0)
	|| ((Free < Heap_Bottom) || (Free >= Heap_Top))
	|| ((Free < MemTop) && ((Free + FREE_PARANOIA_MARGIN) >= MemTop)))
      Free = MemTop;
    else if ((Free + FREE_PARANOIA_MARGIN) < MemTop)
      Free +=  FREE_PARANOIA_MARGIN;
  }
  xtra_info = Free;
  Free += (1 + 2 + PROCESSOR_NREGS);
  (trinfo . extra_trap_info) =
    (MAKE_POINTER_OBJECT (TC_NON_MARKED_VECTOR, xtra_info));
  (*xtra_info++) =
    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (2 + PROCESSOR_NREGS)));
  (*xtra_info++) = ((SCHEME_OBJECT) the_pc);
  (*xtra_info++) = ((SCHEME_OBJECT) C_sp);
  {
    int counter = FULL_SIGCONTEXT_NREGS;
    long * regs = ((long *) (FULL_SIGCONTEXT_FIRST_REG (scp)));
    while ((counter--) > 0)
      (*xtra_info++) = ((SCHEME_OBJECT) (*regs++));
  }
  /* We assume that regs,sp,pc is the order in the processor.
     Scheme can always fix this. */
  if ((PROCESSOR_NREGS - FULL_SIGCONTEXT_NREGS) > 0)
    (*xtra_info++) = ((SCHEME_OBJECT) C_sp);
  if ((PROCESSOR_NREGS - FULL_SIGCONTEXT_NREGS) > 1)
    (*xtra_info++) = ((SCHEME_OBJECT) the_pc);
  setup_trap_frame (signo, info, scp, (&trinfo), new_stack_pointer);
}

/* Find the compiled code block in area which contains `pc_value'.
   This attempts to be more efficient than `find_block_address_in_area'.
   If the pointer is in the heap, it can actually do twice as
   much work, but it is expected to pay off on the average. */

static SCHEME_OBJECT * EXFUN
  (find_block_address_in_area, (char * pc_value, SCHEME_OBJECT * area_start));

#define MINIMUM_SCAN_RANGE		2048

SCHEME_OBJECT *
DEFUN (find_block_address, (pc_value, area_start),
       char * pc_value AND
       SCHEME_OBJECT * area_start)
{
  if (area_start == Constant_Space)
    {
      extern SCHEME_OBJECT * EXFUN
	(find_constant_space_block, (SCHEME_OBJECT *));
      SCHEME_OBJECT * constant_block =
	(find_constant_space_block
	 ((SCHEME_OBJECT *)
	  (((unsigned long) pc_value) &~ SCHEME_ALIGNMENT_MASK)));
      return
	((constant_block == 0)
	 ? 0
	 : (find_block_address_in_area (pc_value, constant_block)));
    }
  {
    SCHEME_OBJECT * nearest_word =
      ((SCHEME_OBJECT *)
       (((unsigned long) pc_value) &~ SCHEME_ALIGNMENT_MASK));
    long maximum_distance = (nearest_word - area_start);
    long distance = maximum_distance;
    while ((distance / 2) > MINIMUM_SCAN_RANGE)
      distance = (distance / 2);
    while ((distance * 2) < maximum_distance)
      {
	SCHEME_OBJECT * block =
	  (find_block_address_in_area (pc_value, (nearest_word - distance)));
	if (block != 0)
	  return (block);
	distance *= 2;
      }
  }
  return (find_block_address_in_area (pc_value, area_start));
}

/*
  Find the compiled code block in area which contains `pc_value',
  by scanning sequentially the complete area.
  For the time being, skip over manifest closures and linkage sections. */

static SCHEME_OBJECT *
DEFUN (find_block_address_in_area, (pc_value, area_start),
       char * pc_value AND
       SCHEME_OBJECT * area_start)
{
  SCHEME_OBJECT * first_valid = area_start;
  SCHEME_OBJECT * area = area_start;
  while (((char *) area) < pc_value)
    {
      SCHEME_OBJECT object = (*area);
      switch (OBJECT_TYPE (object))
	{
	case TC_LINKAGE_SECTION:
	  {
	    switch (READ_LINKAGE_KIND (object))
	    {
	      case GLOBAL_OPERATOR_LINKAGE_KIND:
	      case OPERATOR_LINKAGE_KIND:
	      {
		long count = (READ_OPERATOR_LINKAGE_COUNT (object));
		area = ((END_OPERATOR_LINKAGE_AREA (area, count)) + 1);
		break;
	      }

	      default:
#if FALSE
	      {
		gc_death (TERM_EXIT,
			  "find_block_address: Unknown compiler linkage kind.",
			  area, NULL);
		/*NOTREACHED*/
	      }
#else
	      /* Fall through, no reason to crash here. */
#endif
	      case ASSIGNMENT_LINKAGE_KIND:
	      case CLOSURE_PATTERN_LINKAGE_KIND:
	      case REFERENCE_LINKAGE_KIND:
	        area += ((READ_CACHE_LINKAGE_COUNT (object)) + 1);
		break;

	    }
	    break;
	  }
	case TC_MANIFEST_CLOSURE:
	  {
	    area += 1;
	    {
	      long count = (MANIFEST_CLOSURE_COUNT (area));
	      area = (MANIFEST_CLOSURE_END (area, count));
	    }
	    break;
	  }
	case TC_MANIFEST_NM_VECTOR:
	  {
	    long count = (OBJECT_DATUM (object));
	    if (((char *) (area + (count + 1))) < pc_value)
	      {
		area += (count + 1);
		first_valid = area;
		break;
	      }
	    {
	      SCHEME_OBJECT * block = (area - 1);
	      return
		(((area == first_valid) ||
		  (((OBJECT_TYPE (*block)) != TC_MANIFEST_VECTOR)
		   && ((OBJECT_TYPE (*block)) !=
#ifdef TC_POSITIVE_FIXNUM
		       TC_POSITIVE_FIXNUM
#else
		       TC_FIXNUM
#endif
		       ))
		   ||
		  ((OBJECT_DATUM (*block)) < (count + 1)) ||
		  (! (PLAUSIBLE_CC_BLOCK_P (block))))
		 ? 0
		 : block);
	    }
	  }
	default:
	  {
	    area += 1;
	    break;
	  }
	}
    }
  return (0);
}

#endif /* HAS_COMPILER_SUPPORT and not USE_STACKLETS */



SCHEME_OBJECT
DEFUN (find_ccblock, (the_pc),
       long the_pc)
{
  int pc_in_builtin;
  int builtin_index;
  int pc_in_C;
  int pc_in_heap;
  int pc_in_constant_space;
  int pc_in_scheme;
  int pc_in_hyper_space;
  int pc_in_utility;
  int utility_index;
  extern int EXFUN (pc_to_utility_index, (unsigned long));
  extern int EXFUN (pc_to_builtin_index, (unsigned long));

  if ((the_pc & PC_ALIGNMENT_MASK) != 0)
  {
    pc_in_builtin = 0;
    pc_in_utility = 0;
    pc_in_C = 0;
    pc_in_heap = 0;
    pc_in_constant_space = 0;
    pc_in_scheme = 0;
    pc_in_hyper_space = 1;
  }
  else
  {
    builtin_index = (pc_to_builtin_index (the_pc));
    pc_in_builtin = (builtin_index != -1);
    utility_index = (pc_to_utility_index (the_pc));
    pc_in_utility = (utility_index != -1);    
    pc_in_C = ((the_pc <= ((long) (get_etext ()))) && (!pc_in_builtin));
    pc_in_heap = ADDRESS_HEAP_P ((SCHEME_OBJECT*) the_pc);
    pc_in_constant_space = ADDRESS_CONSTANT_P ((SCHEME_OBJECT*) the_pc);
    pc_in_scheme = (pc_in_heap || pc_in_constant_space || pc_in_builtin);
    pc_in_hyper_space = ((!pc_in_C) && (!pc_in_scheme));
  }

  if (pc_in_hyper_space || (pc_in_scheme && ALLOW_ONLY_C))
  {
      return  SHARP_F;
  }
  else if (pc_in_scheme)
  {
    /* In compiled code. */
    SCHEME_OBJECT * block_addr;
    block_addr =
      (pc_in_builtin
       ? ((SCHEME_OBJECT *) NULL)
       : (find_block_address (((PTR) the_pc),
			      (pc_in_heap ? Heap_Bottom : Constant_Space))));
    if (block_addr != ((SCHEME_OBJECT *) NULL))
    {
	return  MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block_addr);
    }
    else if (pc_in_builtin)
    {
	return  SHARP_F;
    }
    else 
    {
	return  SHARP_F;
    }
  }
  else /* pc_in_C */
  {
    /* In the interpreter, a primitive, or a compiled code utility. */

    SCHEME_OBJECT primitive = (Registers[REGBLOCK_PRIMITIVE]);

    if (pc_in_utility)
    {
	return  SHARP_F;
    }
    else if ((OBJECT_TYPE (primitive)) != TC_PRIMITIVE)
    {
	return  SHARP_F;
    }
    else
    {
	return  SHARP_F;
    }
  }
}
