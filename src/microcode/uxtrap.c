/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
#include "history.h"
#include "ux.h"
#include "uxtrap.h"
#include "uxutil.h"
#include "option.h"
#include "ostop.h"
#include "gccode.h"

#ifdef HAVE_SIGCONTEXT
#  define ENABLE_TRAP_RECOVERY 1
#endif

/* FIXME: Support these architectures.  */
#ifdef __ppc__
#  undef ENABLE_TRAP_RECOVERY
#endif
#ifdef __ppc64__
#  undef ENABLE_TRAP_RECOVERY
#endif
#ifdef __ia64__
#  undef ENABLE_TRAP_RECOVERY
#endif

extern const char * find_signal_name (int);
extern void UX_dump_core (void);
extern void * initial_C_stack_pointer;

struct ux_sig_code_desc
{
  int signo;
  unsigned long code_mask;
  unsigned long code_value;
  const char * name;
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

#define DECLARE_GENERIC_SIGNAL_CODE(v, n)				\
  DECLARE_UX_SIGNAL_CODE ((-1), (~ 0L), v, n)

enum pc_location
{
  pcl_heap,
  pcl_constant,
  pcl_builtin,
  pcl_utility,
  pcl_primitive,
  pcl_unknown
};

#ifdef TC_POSITIVE_FIXNUM
#  define FIXNUM_MARKER TC_POSITIVE_FIXNUM
#else
#  define FIXNUM_MARKER TC_FIXNUM
#endif

#ifndef SPECIAL_SIGNAL_CODE_NAMES
#  define SPECIAL_SIGNAL_CODE_NAMES()
#endif

static enum trap_state trap_state;
static enum trap_state user_trap_state;
static enum trap_state saved_trap_state;
static int saved_signo;
static SIGINFO_T saved_info;
static SIGCONTEXT_T * saved_scp;

static void continue_from_trap
  (int, SIGINFO_T, SIGCONTEXT_T *);

#ifdef CC_SUPPORT_P
   static SCHEME_OBJECT * find_heap_address (unsigned long);
   static SCHEME_OBJECT * find_constant_address (unsigned long);
#  ifdef ENABLE_TRAP_RECOVERY
     static SCHEME_OBJECT * find_block_address (unsigned long, SCHEME_OBJECT *);
     static SCHEME_OBJECT * find_block_address_in_area
       (SCHEME_OBJECT *, SCHEME_OBJECT *);
#  endif
#endif

static void setup_trap_frame
  (int,
   SIGINFO_T,
   SIGCONTEXT_T *,
   struct trap_recovery_info *,
   SCHEME_OBJECT *);

static void initialize_ux_signal_codes (void);
static SCHEME_OBJECT find_signal_code_name (int, SIGINFO_T, SIGCONTEXT_T *);

static enum pc_location classify_pc
  (unsigned long, SCHEME_OBJECT **, unsigned int *);

static void trap_normal_termination (void);
static void trap_immediate_termination (void);
static void trap_dump_core (void);
static void trap_recover (void);

void
UX_initialize_trap_recovery (void)
{
  trap_state = trap_state_recover;
  user_trap_state = trap_state_recover;
  initialize_ux_signal_codes ();
}

enum trap_state
OS_set_trap_state (enum trap_state state)
{
  enum trap_state old_trap_state = user_trap_state;
  user_trap_state = state;
  trap_state = state;
  return (old_trap_state);
}

void
hard_reset (SIGCONTEXT_T * scp)
{
  /* 0 is an invalid signal, it means a user requested reset. */
  continue_from_trap (0, 0, scp);
}

void
soft_reset (void)
{
  /* Called synchronously. */
  struct trap_recovery_info trinfo;
  SCHEME_OBJECT * new_stack_pointer
    = ((SP_OK_P (stack_pointer)) ? stack_pointer : 0);
  if (GET_PRIMITIVE != SHARP_F)
    {
      (trinfo . state) = STATE_PRIMITIVE;
      (trinfo . pc_info_1) = GET_PRIMITIVE;
      (trinfo . pc_info_2) = (ULONG_TO_FIXNUM (GET_LEXPR_ACTUALS));
      (trinfo . extra_trap_info) = SHARP_F;
    }
  else
    {
      (trinfo . state) = STATE_UNKNOWN;
      (trinfo . pc_info_1) = SHARP_F;
      (trinfo . pc_info_2) = SHARP_F;
      (trinfo . extra_trap_info) = SHARP_F;
    }
  if (!ADDRESS_IN_HEAP_P (Free))
    Free = heap_alloc_limit;	/* Let's hope this works. */
  setup_trap_frame (0, 0, 0, (&trinfo), new_stack_pointer);
}

#ifdef CC_SUPPORT_P
SCHEME_OBJECT
find_ccblock (unsigned long pc)
{
  SCHEME_OBJECT * block_addr;
  unsigned int index;

  block_addr = 0;
  classify_pc (pc, (&block_addr), (&index));
  return ((block_addr != 0) ? (MAKE_CC_BLOCK (block_addr)) : SHARP_F);
}
#endif

void
trap_handler (const char * message,
	      int signo,
	      SIGINFO_T info,
	      SIGCONTEXT_T * scp)
{
  int code = ((SIGINFO_VALID_P (info)) ? (SIGINFO_CODE (info)) : 0);
  bool stack_overflowed_p = (STACK_OVERFLOWED_P ());
  enum trap_state old_trap_state = trap_state;

  if (old_trap_state == trap_state_exitting_hard)
    _exit (1);
  if (old_trap_state == trap_state_exitting_soft)
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
      if ((saved_trap_state == trap_state_recover)
	  || (saved_trap_state == trap_state_query))
	{
	  fprintf (stdout,
		   ">> The trap occurred while processing an earlier trap.\n");
	  fprintf (stdout,
		   ">> [The earlier trap raised signal %d (%s), code %d.]\n",
		   saved_signo,
		   (find_signal_name (saved_signo)),
		   ((SIGINFO_VALID_P (saved_info))
		    ? (SIGINFO_CODE (saved_info))
		    : 0));
	  fprintf (stdout, ">> Successful recovery is %sunlikely.\n",
		   ((WITHIN_CRITICAL_SECTION_P ()) ? "extremely " : ""));
	}
      else
	trap_immediate_termination ();
      break;

    case trap_state_recover:
      if ((WITHIN_CRITICAL_SECTION_P ()) || stack_overflowed_p)
	fprintf (stdout, ">> Successful recovery is unlikely.\n");
      else
	{
	  saved_trap_state = old_trap_state;
	  saved_signo = signo;
	  saved_info = info;
	  saved_scp = scp;
	  trap_recover ();
	}
      break;

    case trap_state_exit:
      termination_trap ();
      break;

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
      static const char * trap_query_choices[] =
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
	  break;
	case 'D':
	  trap_dump_core ();
	  break;
	case '\0':
	  /* Error in IO. Assume everything scrod. */
	case 'N':
	case 'Q':
	  trap_normal_termination ();
	  break;
	case 'R':
	  trap_recover ();
	  break;
	}
    }
}

#ifdef ENABLE_TRAP_RECOVERY

/* Heuristic recovery from Unix signals (traps).

   continue_from_trap attempts to:

   1) validate the trap information (pc and sp);
   2) determine whether compiled code was executing, a primitive was
      executing, or execution was in the interpreter;
   3) guess what C global state is still valid; and
   4) set up a recovery frame for the interpreter so that debuggers can
      display more information. */

#define SCHEME_ALIGNMENT_MASK ((sizeof (SCHEME_OBJECT)) - 1)
#define FREE_PARANOIA_MARGIN 0x100

#define ALIGNED_P(addr)							\
  ((((unsigned long) (addr)) & SCHEME_ALIGNMENT_MASK) == 0)

#define SET_RECOVERY_INFO(s, arg1, arg2) do				\
{									\
  (recovery_info . state) = s;						\
  (recovery_info . pc_info_1) = arg1;					\
  (recovery_info . pc_info_2) = arg2;					\
} while (0)

static void
continue_from_trap (int signo, SIGINFO_T info, SIGCONTEXT_T * scp)
{
  unsigned long pc = (SIGCONTEXT_PC (scp));
  SCHEME_OBJECT primitive = GET_PRIMITIVE;
  SCHEME_OBJECT * block_addr;
  unsigned int index;
  SCHEME_OBJECT * new_sp = 0;
  struct trap_recovery_info recovery_info;

#ifdef PC_VALUE_MASK
  pc &= PC_VALUE_MASK;
#endif

  /* Choose new SP and encode location data.  */
  switch (classify_pc (pc, (&block_addr), (&index)))
    {
    case pcl_primitive:
      new_sp = stack_pointer;
      SET_RECOVERY_INFO
	(STATE_PRIMITIVE, primitive, (ULONG_TO_FIXNUM (GET_LEXPR_ACTUALS)));
      break;

    case pcl_heap:
    case pcl_constant:
#ifdef CC_SUPPORT_P
      new_sp = ((SCHEME_OBJECT *) (SIGCONTEXT_SCHSP (scp)));
      Free = ((SCHEME_OBJECT *) (SIGCONTEXT_RFREE (scp)));
      SET_RECOVERY_INFO
	(STATE_COMPILED_CODE,
	 (MAKE_CC_BLOCK (block_addr)),
	 (LONG_TO_UNSIGNED_FIXNUM (pc - ((unsigned long) block_addr))));
      break;
#endif

    case pcl_utility:
#ifdef CC_SUPPORT_P
      new_sp = stack_pointer;
      SET_RECOVERY_INFO (STATE_UTILITY, (ULONG_TO_FIXNUM (index)), UNSPECIFIC);
      break;
#endif

    case pcl_builtin:
#ifdef CC_SUPPORT_P
      new_sp = ((SCHEME_OBJECT *) (SIGCONTEXT_SCHSP (scp)));
      Free = ((SCHEME_OBJECT *) (SIGCONTEXT_RFREE (scp)));
      SET_RECOVERY_INFO (STATE_BUILTIN, (ULONG_TO_FIXNUM (index)), UNSPECIFIC);
      break;
#endif

    case pcl_unknown:
      new_sp = 0;
      SET_RECOVERY_INFO
	(STATE_UNKNOWN,
	 (LONG_TO_UNSIGNED_FIXNUM (pc)),
	 UNSPECIFIC);
      break;
    }

  /* Sanity-check the new SP.  */
  if (! ((ADDRESS_IN_STACK_P (new_sp)) && (ALIGNED_P (new_sp))))
    new_sp = 0;

  /* Sanity-check Free.  */
  if ((new_sp != 0)
      && (ADDRESS_IN_HEAP_P (Free))
      && (ALIGNED_P (Free)))
    {
      if (FREE_OK_P (Free))
	{
	  Free += FREE_PARANOIA_MARGIN;
	  if (!FREE_OK_P (Free))
	    Free = heap_alloc_limit;
	}
    }
  else
    Free = heap_alloc_limit;

  /* Encode the registers.  */
  (recovery_info . extra_trap_info) =
    (MAKE_POINTER_OBJECT (TC_NON_MARKED_VECTOR, Free));
  (*Free++) =
    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (2 + SIGCONTEXT_NREGS)));
  (*Free++) = ((SCHEME_OBJECT) pc);
  (*Free++) = ((SCHEME_OBJECT) (SIGCONTEXT_SP (scp)));
  {
    unsigned long * scan
      = ((unsigned long *) (SIGCONTEXT_FIRST_REG (scp)));
    unsigned long * end = (scan + SIGCONTEXT_NREGS);
    while (scan < end)
      (*Free++) = ((SCHEME_OBJECT) (*scan++));
  }

  setup_trap_frame (signo, info, scp, (&recovery_info), new_sp);
}

/* Find the compiled code block in area that contains `pc'.
   This attempts to be more efficient than `find_block_address_in_area'.
   If the pointer is in the heap, it can actually do twice as
   much work, but it is expected to pay off on the average. */

#ifdef CC_SUPPORT_P

#define MINIMUM_SCAN_RANGE 2048

static SCHEME_OBJECT *
find_heap_address (unsigned long pc)
{
  return (find_block_address (pc, heap_start));
}

static SCHEME_OBJECT *
find_constant_address (unsigned long pc)
{
  return (find_block_address (pc, constant_start));
}

static SCHEME_OBJECT *
find_block_address (unsigned long pc, SCHEME_OBJECT * area_start)
{
  SCHEME_OBJECT * pcp = ((SCHEME_OBJECT *) (pc &~ SCHEME_ALIGNMENT_MASK));
  unsigned long maximum_distance = (pcp - area_start);
  unsigned long distance = maximum_distance;

  while ((distance / 2) > MINIMUM_SCAN_RANGE)
    distance = (distance / 2);
  while (1)
    {
      SCHEME_OBJECT * block
	= (find_block_address_in_area (pcp, (pcp - distance)));
      distance *= 2;
      if ((block != 0) || (distance >= maximum_distance))
	return (block);
    }
}

/* Find the compiled code block in area that contains `pc_value',
   by scanning sequentially the complete area.
   For the time being, skip over manifest closures and linkage sections.  */

static SCHEME_OBJECT *
find_block_address_in_area (SCHEME_OBJECT * pcp, SCHEME_OBJECT * area_start)
{
  SCHEME_OBJECT * first_valid = area_start;
  SCHEME_OBJECT * area = area_start;

  while (area < pcp)
    {
      SCHEME_OBJECT object = (*area);
      switch (OBJECT_TYPE (object))
	{
	case TC_LINKAGE_SECTION:
	  {
	    unsigned long count = (linkage_section_count (object));
	    area += 1;
	    switch (linkage_section_type (object))
	      {
	      case LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR:
	      case LINKAGE_SECTION_TYPE_OPERATOR:
		area += (count * UUO_LINK_SIZE);
		break;

	      default:
	        area += count;
		break;
	      }
	  }
	  break;

	case TC_MANIFEST_CLOSURE:
	  area = (compiled_closure_objects (area + 1));
	  break;

	case TC_MANIFEST_NM_VECTOR:
	  {
	    unsigned long count = (OBJECT_DATUM (object));
	    if ((area + (count + 1)) < pcp)
	      {
		area += (count + 1);
		first_valid = area;
	      }
	    else
	      {
		SCHEME_OBJECT * block = (area - 1);
		return
		  (((area != first_valid)
		    && (((OBJECT_TYPE (*block)) == TC_MANIFEST_VECTOR)
			|| ((OBJECT_TYPE (*block)) == FIXNUM_MARKER))
		    && ((OBJECT_DATUM (*block)) >= (count + 1))
		    && (plausible_cc_block_p (block)))
		   ? block
		   : 0);
	      }
	  }
	  break;

	default:
	  area += 1;
	  break;
	}
    }
  return (0);
}
#endif /* CC_SUPPORT_P */

#else /* not ENABLE_TRAP_RECOVERY */

static struct trap_recovery_info dummy_recovery_info =
{
  STATE_UNKNOWN,
  SHARP_F,
  SHARP_F,
  SHARP_F
};

static void
continue_from_trap (int signo, SIGINFO_T info, SIGCONTEXT_T * scp)
{
  if (Free < heap_alloc_limit)
    Free = heap_alloc_limit;
  setup_trap_frame (signo, info, scp, (&dummy_recovery_info), 0);
}

#ifdef CC_SUPPORT_P

static SCHEME_OBJECT *
find_heap_address (unsigned long pc)
{
  return (0);
}

static SCHEME_OBJECT *
find_constant_address (unsigned long pc)
{
  return (0);
}

#endif /* CC_SUPPORT_P */
#endif /* not ENABLE_TRAP_RECOVERY */

static void
setup_trap_frame (int signo,
		  SIGINFO_T info,
		  SIGCONTEXT_T * scp,
		  struct trap_recovery_info * trinfo,
		  SCHEME_OBJECT * new_stack_pointer)
{
  unsigned long saved_mask = GET_INT_MASK;
  SCHEME_OBJECT handler;
  SCHEME_OBJECT signal_name;

  SET_INTERRUPT_MASK (0);	/* To prevent GC for now. */

  handler
    = ((VECTOR_P (fixed_objects))
       ? (VECTOR_REF (fixed_objects, TRAP_HANDLER))
       : SHARP_F);
  if (!INTERPRETER_APPLICABLE_P (handler))
    {
      fprintf (stderr, "There is no trap handler for recovery!\n");
      fflush (stderr);
      termination_trap ();
    }

  signal_name =
    ((signo != 0)
     ? (char_pointer_to_string (find_signal_name (signo)))
     : SHARP_F);

  if (!FREE_OK_P (Free))
    REQUEST_GC (0);

  if (new_stack_pointer != 0)
    stack_pointer = new_stack_pointer;
  else
    {
      INITIALIZE_STACK ();
     Will_Push (CONTINUATION_SIZE);
      SET_RC (RC_END_OF_COMPUTATION);
      SET_EXP (SHARP_F);
      SAVE_CONT ();
     Pushed ();
    }

 Will_Push (7 + CONTINUATION_SIZE);
  STACK_PUSH (trinfo -> extra_trap_info);
  STACK_PUSH (trinfo -> pc_info_2);
  STACK_PUSH (trinfo -> pc_info_1);
  STACK_PUSH (trinfo -> state);
  STACK_PUSH (BOOLEAN_TO_OBJECT (new_stack_pointer != 0));
  STACK_PUSH (find_signal_code_name (signo, info, scp));
  STACK_PUSH (signal_name);
  SET_RC (RC_HARDWARE_TRAP);
  SET_EXP (long_to_integer (signo));
  SAVE_CONT ();
 Pushed ();

  if ((new_stack_pointer != 0)
      /* This may want to do it in other cases, but this may be enough. */
      && ((trinfo -> state) == STATE_COMPILED_CODE))
    stop_history ();
  history_register = (make_dummy_history ());

 Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
  STACK_PUSH (signal_name);
  STACK_PUSH (handler);
  PUSH_APPLY_FRAME_HEADER (1);
 Pushed ();

  SET_INTERRUPT_MASK (saved_mask);
  abort_to_interpreter (PRIM_APPLY);
}

static void
initialize_ux_signal_codes (void)
{
  unsigned int i = 0;
  INITIALIZE_UX_SIGNAL_CODES ();

#ifdef _POSIX_REALTIME_SIGNALS
  DECLARE_GENERIC_SIGNAL_CODE
    (SI_USER, "signal sent by kill");
  DECLARE_GENERIC_SIGNAL_CODE
    (SI_QUEUE, "signal sent by sigqueue");
  DECLARE_GENERIC_SIGNAL_CODE
    (SI_TIMER, "signal generated by timer expiration");
  DECLARE_GENERIC_SIGNAL_CODE
    (SI_ASYNCIO, "signal generated by asynchronous I/O completion");
  DECLARE_GENERIC_SIGNAL_CODE
    (SI_MESGQ, "signal generated by message queue arrival");
#endif /* _POSIX_REALTIME_SIGNALS */

  DECLARE_UX_SIGNAL_CODE (0, 0, 0, 0);
}

static SCHEME_OBJECT
find_signal_code_name (int signo, SIGINFO_T info, SIGCONTEXT_T * scp)
{
  unsigned long code = 0;
  const char * name = 0;
  if (SIGINFO_VALID_P (info))
    {
      code = (SIGINFO_CODE (info));
      SPECIAL_SIGNAL_CODE_NAMES ();
      if (name == 0)
	{
	  struct ux_sig_code_desc * entry = (& (ux_signal_codes[0]));
	  while ((entry -> signo) != 0)
	    if ((((entry -> signo) < 0) || ((entry -> signo) == signo))
		&& (((entry -> code_mask) & code) == (entry -> code_value)))
	      {
		name = (entry -> name);
		break;
	      }
	    else
	      entry += 1;
	}
    }
  return
    (cons ((ulong_to_integer (code)),
	   ((name == 0)
	    ? SHARP_F
	    : (char_pointer_to_string (name)))));
}

static enum pc_location
classify_pc (unsigned long pc,
	     SCHEME_OBJECT ** r_block_addr,
	     unsigned int * r_index)
{
#ifdef CC_SUPPORT_P
  if (PC_ALIGNED_P (pc))
    {
      if (HEAP_ADDRESS_P ((SCHEME_OBJECT *) pc))
	{
	  SCHEME_OBJECT * block_addr = (find_heap_address (pc));
	  if (block_addr == 0)
	    return (pcl_unknown);
	  if (r_block_addr != 0)
	    (*r_block_addr) = block_addr;
	  return (pcl_heap);
	}
      if (ADDRESS_IN_CONSTANT_P ((SCHEME_OBJECT *) pc))
	{
	  SCHEME_OBJECT * block_addr = (find_constant_address (pc));
	  if (block_addr == 0)
	    return (pcl_unknown);
	  if (r_block_addr != 0)
	    (*r_block_addr) = block_addr;
	  return (pcl_constant);
	}
      if (ADDRESS_UCODE_P (pc))
	{
	  int index = (pc_to_builtin_index (pc));
	  if (index >= 0)
	    {
	      if (r_index != 0)
		(*r_index) = index;
	      return (pcl_builtin);
	    }
	  index = (pc_to_utility_index (pc));
	  if (index >= 0)
	    {
	      if (r_index != 0)
		(*r_index) = index;
	      return (pcl_utility);
	    }
	  if ((OBJECT_TYPE (GET_PRIMITIVE)) == TC_PRIMITIVE)
	    return (pcl_primitive);
	}
    }
#else
  if ((ADDRESS_UCODE_P (pc))
      && ((OBJECT_TYPE (GET_PRIMITIVE)) == TC_PRIMITIVE))
    return (pcl_primitive);
#endif
  return (pcl_unknown);
}

static void
trap_normal_termination (void)
{
  trap_state = trap_state_exitting_soft;
  termination_trap ();
}

static void
trap_immediate_termination (void)
{
  trap_state = trap_state_exitting_hard;
  OS_restore_external_state ();
  exit (1);
}

static void
trap_dump_core (void)
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
trap_recover (void)
{
  if (WITHIN_CRITICAL_SECTION_P ())
    {
      CLEAR_CRITICAL_SECTION_HOOK ();
      EXIT_CRITICAL_SECTION ({});
    }
  reset_interruptable_extent ();
  continue_from_trap (saved_signo, saved_info, saved_scp);
}
