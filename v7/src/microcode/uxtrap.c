/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxtrap.c,v 1.5 1990/11/13 08:45:19 cph Exp $

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
#include "ux.h"
#include "uxtrap.h"
#include "uxutil.h"
#include "option.h"

extern CONST char * EXFUN (find_signal_name, (int signo));
extern void EXFUN (UX_dump_core, (void));
extern PTR initial_C_stack_pointer;

static enum trap_state trap_state;
static enum trap_state user_trap_state;

static enum trap_state saved_trap_state;
static int saved_signo;
static int saved_code;
static struct FULL_SIGCONTEXT * saved_scp;

static void EXFUN
  (continue_from_trap, (int signo, int code, struct FULL_SIGCONTEXT * scp));

void
DEFUN_VOID (UX_initialize_trap_recovery)
{
  trap_state = trap_state_recover;
  user_trap_state = trap_state_recover;
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
DEFUN_VOID (trap_immediate_termination)
{
  OS_restore_external_state ();
  exit (1);
}

static void
DEFUN_VOID (trap_dump_core)
{
  if (option_disable_core_dump)
    {
      fputs (">> Core dumps are disabled - Terminating normally.\n", stdout);
      fflush (stdout);
      termination_trap ();
    }
  else
    UX_dump_core ();
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
  continue_from_trap (saved_signo, saved_code, saved_scp);
}

void
DEFUN (trap_handler, (message, signo, code, scp),
       CONST char * message AND
       int signo AND
       int code AND
       struct FULL_SIGCONTEXT * scp)
{
  enum trap_state old_trap_state = trap_state;
  trap_state = trap_state_trapped;
  if (WITHIN_CRITICAL_SECTION_P ())
    {
      fprintf (stdout,
	       "\n>> A %s has occurred within critical section \"%s\".\n",
	       message, (CRITICAL_SECTION_NAME ()));
      fprintf (stdout, ">> [signal %d (%s), code %d]\n",
	       signo, (find_signal_name (signo)), code);
    }
  else if (old_trap_state != trap_state_recover)
    {
      fprintf (stdout, "\n>> A %s has occurred.\n", message);
      fprintf (stdout, ">> [signal %d (%s), code %d]\n",
	      signo, (find_signal_name (signo)), code);
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
		   saved_code);
	  fputs (((WITHIN_CRITICAL_SECTION_P ())
		  ? ">> Successful recovery is extremely unlikely.\n"
		  : ">> Successful recovery is unlikely.\n"),
		 stdout);
	  break;
	}
      else
	trap_immediate_termination ();
    case trap_state_recover:
      if (WITHIN_CRITICAL_SECTION_P ())
      {
	fputs (">> Successful recovery is unlikely.\n", stdout);
	break;
      }
      else
      {
	saved_trap_state = old_trap_state;
	saved_signo = signo;
	saved_code = code;
	saved_scp = scp;
	trap_recover ();
      }
    case trap_state_exit:
      termination_trap ();
    }
  fflush (stdout);
  saved_trap_state = old_trap_state;
  saved_signo = signo;
  saved_code = code;
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
	case 'N':
	case 'Q':
	  termination_trap ();
	case 'R':
	  trap_recover ();
	}
    }
}

#define STATE_UNKNOWN		(LONG_TO_UNSIGNED_FIXNUM (0))
#define STATE_PRIMITIVE		(LONG_TO_UNSIGNED_FIXNUM (1))
#define STATE_COMPILED_CODE	(LONG_TO_UNSIGNED_FIXNUM (2))
#define STATE_PROBABLY_COMPILED	(LONG_TO_UNSIGNED_FIXNUM (3))

struct trap_recovery_info
{
  SCHEME_OBJECT state;
  SCHEME_OBJECT pc_info_1;
  SCHEME_OBJECT pc_info_2;
  SCHEME_OBJECT extra_trap_info;
};

static struct trap_recovery_info dummy_recovery_info =
{
  STATE_UNKNOWN,
  SHARP_F,
  SHARP_F,
  SHARP_F
};

struct ux_sig_code_desc
{
  int signo;
  unsigned long code_mask;
  unsigned long code_value;
  char *name;
};

DECLARE_UX_SIGNAL_CODES;

static SCHEME_OBJECT
DEFUN (find_signal_code_name, (signo, code),
       int signo AND
       int code)
{
  SCHEME_OBJECT codenam, codenum, result;
  struct ux_sig_code_desc *entry;

  for (entry = &ux_signal_codes[0];
       entry->signo != 0;
       entry += 1)
  {
    if ((entry->signo == signo) &&
	(((entry->code_mask) & ((unsigned long) code)) ==
	 (entry->code_value)))
    {
      break;
    }
  }

  codenam = ((entry->signo == 0)
	     ? SHARP_F
	     : (char_pointer_to_string (entry->name)));
  codenum = (long_to_integer ((long) code));

  result = (MAKE_POINTER_OBJECT (TC_LIST, Free));
  *Free++ = codenum;
  *Free++ = codenam;

  return (result);  
}

static void
DEFUN (setup_trap_frame, (signo, code, info, new_stack_pointer),
       int signo AND
       int code AND
       struct trap_recovery_info * info AND
       SCHEME_OBJECT * new_stack_pointer)
{
  SCHEME_OBJECT handler;
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
     : (char_pointer_to_string (find_signal_name (signo))));
  signal_code = (find_signal_code_name (signo, code));
  History = (Make_Dummy_History ());
  if (!stack_recovered_p)
    {
      Initialize_Stack ();
     Will_Push (CONTINUATION_SIZE);
      Store_Return (RC_END_OF_COMPUTATION);
      Store_Expression (SHARP_F);
      Save_Cont ();
     Pushed ();
    }
  else
    Stack_Pointer = new_stack_pointer;
 Will_Push ((6 + CONTINUATION_SIZE) + (STACK_ENV_EXTRA_SLOTS + 2));
  STACK_PUSH (info -> extra_trap_info);
  STACK_PUSH (info -> pc_info_2);
  STACK_PUSH (info -> pc_info_1);
  STACK_PUSH (info -> state);
  STACK_PUSH (BOOLEAN_TO_OBJECT (stack_recovered_p));
  STACK_PUSH (signal_code);
  STACK_PUSH (signal_name);
  Store_Return (RC_HARDWARE_TRAP);
  Store_Expression (long_to_integer (signo));
  Save_Cont ();
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
  struct trap_recovery_info info;
  SCHEME_OBJECT * new_stack_pointer =
    (((Stack_Pointer <= Stack_Top) && (Stack_Pointer > Stack_Guard))
     ? Stack_Pointer
     : 0);
  if ((Regs[REGBLOCK_PRIMITIVE]) != SHARP_F)
    {
      (info . state) = STATE_PRIMITIVE;
      (info . pc_info_1) = (Regs[REGBLOCK_PRIMITIVE]);
      (info . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (Regs[REGBLOCK_LEXPR_ACTUALS]));
      (info . extra_trap_info) = SHARP_F;
    }
  else
    {
      (info . state) = STATE_UNKNOWN;
      (info . pc_info_1) = SHARP_F;
      (info . pc_info_2) = SHARP_F;
      (info . extra_trap_info) = SHARP_F;
    }
  if ((Free >= Heap_Top) || (Free < Heap_Bottom))
    /* Let's hope this works. */
    Free = MemTop;
  setup_trap_frame (0, 0, (&info), new_stack_pointer);
}

#if !defined(HAVE_SIGCONTEXT) || !defined(HAS_COMPILER_SUPPORT) || defined(USE_STACKLETS)

static void
DEFUN (continue_from_trap, (signo, code, scp),
       int signo AND
       int code AND
       struct FULL_SIGCONTEXT * scp)
{
  if (Free < MemTop)
  {
    Free = MemTop;
  }
  setup_trap_frame (signo, code, (&dummy_recovery_info), 0);
}

#else /* HAVE_SIGCONTEXT and HAS_COMPILER_SUPPORT and not USE_STACKLETS */

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

/* PCs must be aligned according to this. */

#define PC_ALIGNMENT_MASK		((1 << PC_ZERO_BITS) - 1)

/* But they may have bits that can be masked by this. */

#ifndef PC_VALUE_MASK
#define PC_VALUE_MASK			(~0)
#endif

#define C_STACK_SIZE			0x01000000

#ifdef HAS_COMPILER_SUPPORT
#define ALLOW_ONLY_C 0
#else
#define ALLOW_ONLY_C 1
#define PLAUSIBLE_CC_BLOCK_P(block) 0
#endif

static SCHEME_OBJECT * EXFUN
  (find_block_address, (char * pc_value, SCHEME_OBJECT * area_start));

static void
DEFUN (continue_from_trap, (signo, code, scp),
       int signo AND
       int code AND
       struct FULL_SIGCONTEXT * scp)
{
  int pc_in_C;
  int pc_in_heap;
  int pc_in_constant_space;
  int pc_in_scheme;
  int pc_in_hyper_space;
  int scheme_sp_valid;
  long C_sp = (FULL_SIGCONTEXT_SP (scp));
  long scheme_sp = (FULL_SIGCONTEXT_SCHSP (scp));
  long the_pc = ((FULL_SIGCONTEXT_PC (scp)) & PC_VALUE_MASK);
  SCHEME_OBJECT * new_stack_pointer;
  SCHEME_OBJECT * xtra_info;
  struct trap_recovery_info info;
  extern long etext;

#if 0
  fprintf (stderr, "\ncontinue_from_trap:");
  fprintf (stderr, "\tpc = 0x%08lx\n", the_pc);
  fprintf (stderr, "\tCsp = 0x%08lx\n", C_sp);
  fprintf (stderr, "\tssp = 0x%08lx\n", scheme_sp);
  fprintf (stderr, "\tesp = 0x%08lx\n", Ext_Stack_Pointer);
#endif

  if ((the_pc & PC_ALIGNMENT_MASK) != 0)
  {
    pc_in_C = 0;
    pc_in_heap = 0;
    pc_in_constant_space = 0;
    pc_in_scheme = 0;
    pc_in_hyper_space = 1;
  }
  else
  {
    pc_in_C = (the_pc <= ((long) (&etext)));
    pc_in_heap =
      ((the_pc < ((long) Heap_Top)) && (the_pc >= ((long) Heap_Bottom)));
    pc_in_constant_space =
      ((the_pc < ((long) Constant_Top)) &&
       (the_pc >= ((long) Constant_Space)));
    pc_in_scheme = (pc_in_heap || pc_in_constant_space);
    pc_in_hyper_space = ((!pc_in_C) && (!pc_in_scheme));
  }

  scheme_sp_valid =
    (pc_in_scheme
     && ((scheme_sp < ((long) Stack_Top)) &&
	 (scheme_sp >= ((long) Absolute_Stack_Base)) &&
	 ((scheme_sp & STACK_ALIGNMENT_MASK) == 0)));

  new_stack_pointer =
    (scheme_sp_valid
     ? ((SCHEME_OBJECT *) scheme_sp)
     : (pc_in_C && (Stack_Pointer < Stack_Top)
	&& (Stack_Pointer > Absolute_Stack_Base))
     ? Stack_Pointer
     : ((SCHEME_OBJECT *) 0));

  if (pc_in_hyper_space || (pc_in_scheme && ALLOW_ONLY_C))
  {
    /* In hyper space. */
    (info . state) = STATE_UNKNOWN;
    (info . pc_info_1) = SHARP_F;
    (info . pc_info_2) = SHARP_F;
    new_stack_pointer = 0;
    if ((Free < MemTop) ||
	(Free >= Heap_Top) ||
	((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
    {
      Free = MemTop;
    }
  }
  else if (pc_in_scheme)
  {
    /* In compiled code. */
    SCHEME_OBJECT * block_addr;
    SCHEME_OBJECT * maybe_free;
    block_addr =
      (find_block_address (((PTR) the_pc),
			   (pc_in_heap ? Heap_Bottom : Constant_Space)));
    if (block_addr == 0)
    {
      (info . state) = STATE_PROBABLY_COMPILED;
      (info . pc_info_1) = (LONG_TO_UNSIGNED_FIXNUM (the_pc));
      (info . pc_info_2) = SHARP_F;
      if ((Free < MemTop) ||
	  (Free >= Heap_Top) ||
	  ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
	Free = MemTop;
    }
    else
    {
      (info . state) = STATE_COMPILED_CODE;
      (info . pc_info_1) =
	(MAKE_POINTER_OBJECT (TC_COMPILED_CODE_BLOCK, block_addr));
      (info . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (the_pc - ((long) block_addr)));
#ifdef HAVE_FULL_SIGCONTEXT
      maybe_free = ((SCHEME_OBJECT *) (FULL_SIGCONTEXT_RFREE (scp)));
      if (((((unsigned long) maybe_free) & SCHEME_ALIGNMENT_MASK) == 0)
	  && (maybe_free >= Heap_Bottom) && (maybe_free < Heap_Top))
      {
	Free = (maybe_free + FREE_PARANOIA_MARGIN);
      }
      else
#endif
      {
	if ((Free < MemTop) || (Free >= Heap_Top)
	    || ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0))
	{
	  Free = MemTop;
	}
      }
    }
  }
  else
  {
    /* In the interpreter, a primitive, or a compiled code utility. */

    SCHEME_OBJECT primitive = (Regs[REGBLOCK_PRIMITIVE]);

    if ((OBJECT_TYPE (primitive)) != TC_PRIMITIVE)
    {
      (info . state) = STATE_UNKNOWN;
      (info . pc_info_1) = SHARP_F;
      (info . pc_info_2) = SHARP_F;
      new_stack_pointer = 0;
    }
    else
    {
      long primitive_address =
	((long) (Primitive_Procedure_Table[OBJECT_DATUM (primitive)]));
      (info . state) = STATE_PRIMITIVE;
      (info . pc_info_1) = primitive;
      (info . pc_info_2) =
	(LONG_TO_UNSIGNED_FIXNUM (Regs[REGBLOCK_LEXPR_ACTUALS]));
    }
    if ((new_stack_pointer == 0)
	|| ((((unsigned long) Free) & SCHEME_ALIGNMENT_MASK) != 0)
	|| ((Free < Heap_Bottom) || (Free >= Heap_Top))
	|| ((Free < MemTop) && ((Free + FREE_PARANOIA_MARGIN) >= MemTop)))
    {
      Free = MemTop;
    }
    else if ((Free + FREE_PARANOIA_MARGIN) < MemTop)
    {
      Free +=  FREE_PARANOIA_MARGIN;
    }
  }
  xtra_info = Free;
  Free += (1 + 2 + PROCESSOR_NREGS);
  (info . extra_trap_info) =
    (MAKE_POINTER_OBJECT (TC_NON_MARKED_VECTOR, xtra_info));
  (*xtra_info++) =
    (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (2 + PROCESSOR_NREGS)));
  (*xtra_info++) = ((SCHEME_OBJECT) the_pc);
  (*xtra_info++) = ((SCHEME_OBJECT) C_sp);
  {
    int counter = FULL_SIGCONTEXT_NREGS;
    int * regs = (FULL_SIGCONTEXT_FIRST_REG (scp));
    while ((counter--) > 0)
    {
      (*xtra_info++) = ((SCHEME_OBJECT) (*regs++));
    }
  }
  /* We assume that regs,sp,pc is the order in the processor.
     Scheme can always fix this. */
  if ((PROCESSOR_NREGS - FULL_SIGCONTEXT_NREGS) > 0)
  {
    (*xtra_info++) = ((SCHEME_OBJECT) C_sp);
  }
  if ((PROCESSOR_NREGS - FULL_SIGCONTEXT_NREGS) > 1)
  {
    (*xtra_info++) = ((SCHEME_OBJECT) the_pc);
  }
  setup_trap_frame (signo, code, (&info), new_stack_pointer);
}

/* Find the compiled code block in area which contains `pc_value'.
   This attempts to be more efficient than `find_block_address_in_area'.
   If the pointer is in the heap, it can actually do twice as
   much work, but it is expected to pay off on the average. */

static SCHEME_OBJECT * EXFUN
  (find_block_address_in_area, (char * pc_value, SCHEME_OBJECT * area_start));

#define MINIMUM_SCAN_RANGE		2048

static SCHEME_OBJECT *
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
	    if ((READ_LINKAGE_KIND (object)) != OPERATOR_LINKAGE_KIND)
	      area += ((READ_CACHE_LINKAGE_COUNT (object)) + 1);
	    else
	      {
		long count = (READ_OPERATOR_LINKAGE_COUNT (object));
		area = ((END_OPERATOR_LINKAGE_AREA (area, count)) + 1);
	      }
	    break;
	  }
	case TC_MANIFEST_CLOSURE:
	  {
	    area += 1;
	    {
	      long count = (MANIFEST_CLOSURE_COUNT (area));
	      area = ((MANIFEST_CLOSURE_END (area, count)) + 1);
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
		  ((OBJECT_TYPE (*block)) != TC_MANIFEST_VECTOR) ||
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

#endif /* HAVE_SIGCONTEXT and HAS_COMPILER_SUPPORT and not USE_STACKLETS */
