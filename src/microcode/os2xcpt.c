/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
#include "gccode.h"
#include "os2.h"

extern int OS2_disable_stack_guard (void *);
extern int OS2_essential_thread_p (TID);
extern void OS2_message_box (const char *, const char *, int);

extern ULONG C_Stack_Pointer;
extern ULONG C_Frame_Pointer;

typedef enum
{
  trap_state_trapped,
  trap_state_exit,
  trap_state_suspend,
  trap_state_recover,
  trap_state_exitting_soft,
  trap_state_exitting_hard
} trap_state_t;

#define STATE_UNKNOWN		(LONG_TO_UNSIGNED_FIXNUM (0))
#define STATE_PRIMITIVE		(LONG_TO_UNSIGNED_FIXNUM (1))
#define STATE_COMPILED_CODE	(LONG_TO_UNSIGNED_FIXNUM (2))
#define STATE_PROBABLY_COMPILED	(LONG_TO_UNSIGNED_FIXNUM (3))

typedef struct
{
  SCHEME_OBJECT state;
  SCHEME_OBJECT pc_info_1;
  SCHEME_OBJECT pc_info_2;
  SCHEME_OBJECT extra_trap_info;
} trap_recovery_info_t;

typedef struct
{
  ULONG number;
  const char * name;
  const char * description;
} exception_entry_t;

#define SCHEME_ALIGNMENT_MASK ((sizeof (long)) - 1)
#define FREE_PARANOIA_MARGIN 0x100

static ULONG find_program_end_address (void);
extern ULONG APIENTRY OS2_exception_handler
  (PEXCEPTIONREPORTRECORD, PEXCEPTIONREGISTRATIONRECORD, PCONTEXTRECORD,
   PVOID);
static void trap_immediate_termination (void);
static void trap_normal_termination (void);
static void trap_recover (PEXCEPTIONREPORTRECORD, PCONTEXTRECORD);
static void continue_from_trap (PEXCEPTIONREPORTRECORD, PCONTEXTRECORD);
static void do_abort_to_interpreter (void);
static SCHEME_OBJECT * compiled_code_free (PCONTEXTRECORD);
static SCHEME_OBJECT * interpreter_free (int force_gc);
static SCHEME_OBJECT * find_block_address (char *, SCHEME_OBJECT *);
static SCHEME_OBJECT * find_block_address_in_area (char *, SCHEME_OBJECT *);
static void setup_trap_frame
  (PEXCEPTIONREPORTRECORD, PCONTEXTRECORD, trap_recovery_info_t *,
   SCHEME_OBJECT *);
static exception_entry_t * find_exception_entry (ULONG);
static const char * find_exception_name (ULONG);
static void describe_exception (ULONG, int);
static int isvowel (char);
static void noise_start (void);
static void noise (const char *, ...);
static USHORT noise_end (const char *, ULONG);

static trap_state_t trap_state;
static trap_state_t user_trap_state;
static trap_state_t saved_trap_state;
static ULONG saved_exception_number;
static ULONG program_end_address;

void
OS2_initialize_exception_handling (void)
{
  trap_state = trap_state_recover;
  user_trap_state = trap_state_recover;
  program_end_address = (find_program_end_address ());
}

static ULONG
find_program_end_address (void)
{
  /* The normal configuration for a C program is for the program text
     to start at 0x10000 and go up contiguously from there.  */
  ULONG start = 0x10000;	/* First 16 pages reserved for OS.  */
  ULONG step = 0x1000;		/* 4k page size.  */
  ULONG end = 0x20000000;	/* 512M maximum process address space.  */
  ULONG flag_mask
    = (PAG_FREE | PAG_READ | PAG_WRITE | PAG_EXECUTE | PAG_GUARD
       | PAG_DEFAULT | PAG_SHARED | PAG_COMMIT);
  ULONG program_flags		/* Permissions for program text pages.  */
    = (PAG_READ | PAG_EXECUTE | PAG_COMMIT);
  ULONG length = (end - start);
  ULONG flags;
  APIRET rc;

  rc = (DosQueryMem (((PVOID) start), (& length), (& flags)));
  if (! ((rc == NO_ERROR) && ((flags & flag_mask) == program_flags)))
    OS2_logic_error ("Error reading program text start address.");
  while (1)
    {
      start += length;
      length = (end - start);
      rc = (DosQueryMem (((PVOID) start), (& length), (& flags)));
      if (rc == NO_ERROR)
	{
	  if ((flags & flag_mask) != program_flags)
	    return (start);
	}
      else if (rc == ERROR_INVALID_ADDRESS)
	return (start);
      else
	OS2_logic_error ("Error from DosQueryMem.");
    }
}

void
OS2_enter_interpreter (void (* enter_interpreter) (void))
{
  /* This registration record is required to be allocated on the C
     stack, so we have to use this unusual mechanism to install the
     trap-handling code.  */
  EXCEPTIONREGISTRATIONRECORD registration;
  (registration . ExceptionHandler) = OS2_exception_handler;
  DosSetExceptionHandler (& registration);
  (* enter_interpreter) ();
  outf_fatal ("Exception!\n");
  termination_trap ();
}

trap_state_t
OS_set_trap_state (trap_state_t state)
{
  trap_state_t old_trap_state = user_trap_state;
  user_trap_state = state;
  trap_state = state;
  return (old_trap_state);
}

ULONG APIENTRY
OS2_exception_handler (PEXCEPTIONREPORTRECORD report,
		       PEXCEPTIONREGISTRATIONRECORD registration,
		       PCONTEXTRECORD context,
		       PVOID dispatcher_context)
{
  trap_state_t old_trap_state;
  int stack_overflowed_p;
  ULONG exception_number;
  int recovery_unlikely_p = 0;

  /* We must ignore EH_NONCONTINUABLE exceptions because in order to
     do the throw, the registers must be correctly configured for C,
     and we accomplish this by bashing the context and returning with
     XCPT_CONTINUE_EXECUTION from here.  */
  if ((((report -> fHandlerFlags)
	& (EH_UNWINDING | EH_EXIT_UNWIND | EH_STACK_INVALID | EH_NESTED_CALL
	   | EH_NONCONTINUABLE))
       != 0)
      || (! (((report -> ExceptionNum) == XCPT_ACCESS_VIOLATION)
	     || ((report -> ExceptionNum) == XCPT_ARRAY_BOUNDS_EXCEEDED)
	     || ((report -> ExceptionNum) == XCPT_DATATYPE_MISALIGNMENT)
	     || ((report -> ExceptionNum) == XCPT_FLOAT_DENORMAL_OPERAND)
	     || ((report -> ExceptionNum) == XCPT_FLOAT_DIVIDE_BY_ZERO)
	     || ((report -> ExceptionNum) == XCPT_FLOAT_INEXACT_RESULT)
	     || ((report -> ExceptionNum) == XCPT_FLOAT_INVALID_OPERATION)
	     || ((report -> ExceptionNum) == XCPT_FLOAT_OVERFLOW)
	     || ((report -> ExceptionNum) == XCPT_FLOAT_STACK_CHECK)
	     || ((report -> ExceptionNum) == XCPT_FLOAT_UNDERFLOW)
	     || ((report -> ExceptionNum) == XCPT_GUARD_PAGE_VIOLATION)
	     || ((report -> ExceptionNum) == XCPT_ILLEGAL_INSTRUCTION)
	     || ((report -> ExceptionNum) == XCPT_INTEGER_DIVIDE_BY_ZERO)
	     || ((report -> ExceptionNum) == XCPT_INTEGER_OVERFLOW)
	     || ((report -> ExceptionNum) == XCPT_INVALID_LOCK_SEQUENCE)
	     || ((report -> ExceptionNum) == XCPT_PRIVILEGED_INSTRUCTION))))
    return (XCPT_CONTINUE_SEARCH);
  exception_number = (report -> ExceptionNum);
  stack_overflowed_p = (STACK_OVERFLOWED_P ());

  /* If this is a guard page violation, we're only interested if it
     occurred in one of the Scheme stack guard pages.  Test this by
     examining the second parameter, which is the address of the
     access within the guard page.  `OS2_disable_stack_guard' will
     perform this test, additionally disabling the guard page if it is
     one of ours.  */
  if (exception_number == XCPT_GUARD_PAGE_VIOLATION)
    {
      if (!OS2_disable_stack_guard ((void *) ((report -> ExceptionInfo) [1])))
	return (XCPT_CONTINUE_SEARCH);
      /* OK, we've determined that this is one of our guard pages, and
	 it has been disabled.  If `stack_overflowed_p' is true, we
	 can't recover cleanly and must terminate Scheme.  Otherwise,
	 we still have some maneuvering room -- so signal a Scheme
	 stack-overflow interrupt and continue.  When Scheme takes the
	 interrupt, it will do a throw, and the throw will re-enable
	 the stack guard.  */
      if (!stack_overflowed_p)
	{
	  REQUEST_INTERRUPT (INT_Stack_Overflow);
	  return (XCPT_CONTINUE_EXECUTION);
	}
    }

  old_trap_state = trap_state;
  if (old_trap_state == trap_state_exitting_hard)
    _exit (1);
  if (old_trap_state == trap_state_exitting_soft)
    trap_immediate_termination ();
  trap_state = trap_state_trapped;

  noise_start ();
  if (WITHIN_CRITICAL_SECTION_P ())
    {
      noise ("Scheme has detected ");
      describe_exception (exception_number, 0);
      noise (" within critical section \"%s\".  ", (CRITICAL_SECTION_NAME ()));
    }
  else if (stack_overflowed_p || (old_trap_state != trap_state_recover))
    {
      noise ("Scheme has detected ");
      describe_exception (exception_number, 0);
      noise (".  ");
    }
  if (stack_overflowed_p)
    {
      noise ("The stack has overflowed overwriting adjacent memory.  ");
      noise ("This was probably caused by a runaway recursion.  ");
    }

  switch (old_trap_state)
    {
    case trap_state_recover:
      if ((WITHIN_CRITICAL_SECTION_P ()) || stack_overflowed_p)
	{
	  noise ("Successful recovery is unlikely.  ");
	  recovery_unlikely_p = 1;
	  break;
	}
      saved_trap_state = old_trap_state;
      saved_exception_number = exception_number;
      (void) noise_end ("Exception Info", (MB_OK | MB_ERROR));
      trap_recover (report, context);
      return (XCPT_CONTINUE_EXECUTION);

    case trap_state_trapped:
      if (saved_trap_state == trap_state_recover)
	{
	  noise ("This occurred while attempting to recover from ");
	  describe_exception (saved_exception_number, 1);
	  noise (".  Successful recovery is ");
	  if (WITHIN_CRITICAL_SECTION_P ())
	    noise ("extremely ");
	  noise ("unlikely.  ");
	  recovery_unlikely_p = 1;
	  break;
	}
      (void) noise_end ("Exception Info", (MB_OK | MB_ERROR));
      trap_immediate_termination ();
      break;

    case trap_state_exit:
      (void) noise_end ("Exception Info", (MB_OK | MB_ERROR));
      termination_trap ();
      break;
    }

  noise ("\n\n");
  saved_trap_state = old_trap_state;
  saved_exception_number = exception_number;
  {
    int first_query = 1;
    while (1)
      {
	noise ("Attempt recovery?");
	if ((noise_end
	     ("Recovery Choice",
	      (MB_YESNO
	       | (first_query ? MB_ERROR : 0)
	       | (recovery_unlikely_p ? MB_DEFBUTTON2 : MB_DEFBUTTON1))))
	    == MBID_YES)
	  {
	    trap_recover (report, context);
	    return (XCPT_CONTINUE_EXECUTION);
	  }
	else
	  {
	    first_query = 0;
	    noise ("Terminate Scheme normally?  ");
	    noise ("Selecting \"No\" terminates Scheme immediately ");
	    noise ("(without cleanup).  Selecting \"Cancel\" returns to ");
	    noise ("Recovery Choice dialog.");
	    switch (noise_end ("Termination Choices", (MB_YESNOCANCEL)))
	      {
	      case MBID_YES:
		trap_normal_termination ();
		break;
	      case MBID_NO:
		trap_immediate_termination ();
		_exit (1);
		break;
	      }
	  }
      }
  }
  return (XCPT_CONTINUE_SEARCH);
}

static void
trap_immediate_termination (void)
{
  extern void OS_restore_external_state (void);
  trap_state = trap_state_exitting_hard;
  OS_restore_external_state ();
  exit (1);
}

static void
trap_normal_termination (void)
{
  trap_state = trap_state_exitting_soft;
  termination_trap ();
}

static void
trap_recover (PEXCEPTIONREPORTRECORD report, PCONTEXTRECORD context)
{
  if (WITHIN_CRITICAL_SECTION_P ())
    {
      CLEAR_CRITICAL_SECTION_HOOK ();
      EXIT_CRITICAL_SECTION ({});
    }
  continue_from_trap (report, context);
}

/* Heuristic recovery from processor traps/exceptions.

   continue_from_trap attempts to:

   1) validate the trap information (pc and sp);
   2) determine whether compiled code was executing, a primitive was
      executing, or execution was in the interpreter;
   3) guess what C global state is still valid; and
   4) set up a recovery frame for the interpreter so that debuggers
      can display more information.  */

static void
continue_from_trap (PEXCEPTIONREPORTRECORD report, PCONTEXTRECORD context)
{
  ULONG pc;
  enum
    {
      pc_in_hyperspace,
      pc_in_c,
      pc_in_primitive,
      pc_in_utility,
      pc_in_builtin,
      pc_in_heap
    } pc_location;

  SCHEME_OBJECT * block_address;
  trap_recovery_info_t trinfo;
  SCHEME_OBJECT * new_sp;

  /* Punt if the context doesn't contain the registers we need to see.  */
  if (((context -> ContextFlags) & CONTEXT_CONTROL) == 0)
    {
      (trinfo . state) = STATE_UNKNOWN;
      (trinfo . pc_info_1) = SHARP_F;
      (trinfo . pc_info_2) = SHARP_F;
      (trinfo . extra_trap_info) = SHARP_F;
      Free = (interpreter_free (1));
      new_sp = 0;
      goto done;
    }

  /* Classify the PC location.  */
  pc = (context -> ctx_RegEip);
  if (!PC_ALIGNED_P (pc))
    pc_location = pc_in_hyperspace;
  else if (pc <= program_end_address)
    {
      if ((pc_to_builtin_index (pc)) != (-1))
	pc_location = pc_in_builtin;
      else if ((pc_to_utility_index (pc)) != (-1))
	pc_location = pc_in_utility;
      else if (PRIMITIVE_P (GET_PRIMITIVE))
	pc_location = pc_in_primitive;
      else
	pc_location = pc_in_c;
    }
  else if ((((ULONG) heap_start) <= pc) && (pc < ((ULONG) heap_end)))
    {
      pc_location = pc_in_heap;
      block_address = (find_block_address (((void *) pc), heap_start));
    }
  else if ((((ULONG) constant_start) <= pc) && (pc < ((ULONG) constant_end)))
    {
      pc_location = pc_in_heap;
      block_address = (find_block_address (((void *) pc), constant_start));
    }
  else
    pc_location = pc_in_hyperspace;

  /* Find Scheme's stack pointer.  */
  switch (pc_location)
    {
    case pc_in_builtin:
    case pc_in_heap:
      new_sp = ((SCHEME_OBJECT *) (context -> ctx_RegEsp));
      break;
    case pc_in_utility:
    case pc_in_primitive:
    case pc_in_c:
      new_sp = stack_pointer;
      break;
    default:
      new_sp = 0;
      break;
    }
  if (! ((ADDRESS_IN_STACK_P (new_sp))
	 && ((((ULONG) new_sp) & SCHEME_ALIGNMENT_MASK) == 0)))
    new_sp = 0;

  /* Build the trinfo structure.  */
  switch (pc_location)
    {
    case pc_in_heap:
      if (block_address != 0)
	{
	  (trinfo . state) = STATE_COMPILED_CODE;
	  (trinfo . pc_info_1) = (MAKE_CC_BLOCK (block_address));
	  (trinfo . pc_info_2)
	    = (LONG_TO_UNSIGNED_FIXNUM (pc - ((ULONG) block_address)));
	}
      else
	{
	  (trinfo . state) = STATE_PROBABLY_COMPILED;
	  (trinfo . pc_info_1) = (LONG_TO_UNSIGNED_FIXNUM (pc));
	  (trinfo . pc_info_2) = SHARP_F;
	}
      Free = (compiled_code_free (context));
      break;
    case pc_in_builtin:
      (trinfo . state) = STATE_PROBABLY_COMPILED;
      (trinfo . pc_info_1)
	= (LONG_TO_UNSIGNED_FIXNUM (pc_to_builtin_index (pc)));
      (trinfo . pc_info_2) = SHARP_T;
      Free = (compiled_code_free (context));
      break;
    case pc_in_utility:
      (trinfo . state) = STATE_PROBABLY_COMPILED;
      (trinfo . pc_info_1)
	= (LONG_TO_UNSIGNED_FIXNUM (pc_to_utility_index (pc)));
      (trinfo . pc_info_2) = UNSPECIFIC;
      Free = ((new_sp == 0) ? heap_alloc_limit : (interpreter_free (0)));
      break;
    case pc_in_primitive:
      (trinfo . state) = STATE_PRIMITIVE;
      (trinfo . pc_info_1) = GET_PRIMITIVE;
      (trinfo . pc_info_2) = (ULONG_TO_FIXNUM (GET_LEXPR_ACTUALS));
      Free = ((new_sp == 0) ? heap_alloc_limit : (interpreter_free (0)));
      break;
    default:
      (trinfo . state) = STATE_UNKNOWN;
      (trinfo . pc_info_1) = SHARP_F;
      (trinfo . pc_info_2) = SHARP_F;
      Free = (interpreter_free (1));
      break;
    }
  {
    SCHEME_OBJECT v
      = (allocate_non_marked_vector
	 (TC_NON_MARKED_VECTOR,
	  ((((context -> ContextFlags) & CONTEXT_INTEGER) == 0) ? 4 : 10),
	  0));
    /* First two elements of vector must be PC and SP, in that order.  */
    VECTOR_SET (v, 0, ((SCHEME_OBJECT) (context -> ctx_RegEip)));
    VECTOR_SET (v, 1, ((SCHEME_OBJECT) (context -> ctx_RegEsp)));
    VECTOR_SET (v, 2, ((SCHEME_OBJECT) (context -> ctx_RegEbp)));
    VECTOR_SET (v, 3, ((SCHEME_OBJECT) (context -> ctx_EFlags)));
    if (((context -> ContextFlags) & CONTEXT_INTEGER) != 0)
      {
	VECTOR_SET (v, 4, ((SCHEME_OBJECT) (context -> ctx_RegEdi)));
	VECTOR_SET (v, 5, ((SCHEME_OBJECT) (context -> ctx_RegEsi)));
	VECTOR_SET (v, 6, ((SCHEME_OBJECT) (context -> ctx_RegEax)));
	VECTOR_SET (v, 7, ((SCHEME_OBJECT) (context -> ctx_RegEbx)));
	VECTOR_SET (v, 8, ((SCHEME_OBJECT) (context -> ctx_RegEcx)));
	VECTOR_SET (v, 9, ((SCHEME_OBJECT) (context -> ctx_RegEdx)));
      }
    (trinfo . extra_trap_info) = v;
  }
 done:
  setup_trap_frame (report, context, (& trinfo), new_sp);

  /* If this was a hardware-generated floating-point exception, clear
     the corresponding bit in the processor status word.  Otherwise
     the exception will be resignalled when we restart.  */
  if (((context -> ContextFlags) & CONTEXT_FLOATING_POINT) != 0)
    switch (report -> ExceptionNum)
      {
      case XCPT_FLOAT_DENORMAL_OPERAND:
	((context -> ctx_env) [1]) &=~ 0x02;
	break;
      case XCPT_FLOAT_DIVIDE_BY_ZERO:
	((context -> ctx_env) [1]) &=~ 0x04;
	break;
      case XCPT_FLOAT_INEXACT_RESULT:
	((context -> ctx_env) [1]) &=~ 0x20;
	break;
      case XCPT_FLOAT_INVALID_OPERATION:
	((context -> ctx_env) [1]) &=~ 0x01;
	break;
      case XCPT_FLOAT_OVERFLOW:
	((context -> ctx_env) [1]) &=~ 0x08;
	break;
      case XCPT_FLOAT_UNDERFLOW:
	((context -> ctx_env) [1]) &=~ 0x10;
	break;
      }
  /* Now attempt to continue.  This requires some trickery if the
     registers are configured for Scheme compiled code, because
     longjmp will fail unless the stack and frame pointers are set up
     for C.  This is because of error checking that is built in to the
     OS/2 exception handling mechanism: it checks the stack pointer to
     make sure that the exception-handler registration records are on
     the stack.  */
  if (! ((pc_location == pc_in_builtin) || (pc_location == pc_in_heap)))
    abort_to_interpreter (PRIM_APPLY);
  (context -> ctx_RegEsp) = C_Stack_Pointer;
  (context -> ctx_RegEbp) = C_Frame_Pointer;
  (context -> ctx_RegEip) = ((ULONG) do_abort_to_interpreter);
}

static void
do_abort_to_interpreter (void)
{
  abort_to_interpreter (PRIM_APPLY);
}

static SCHEME_OBJECT *
compiled_code_free (PCONTEXTRECORD context)
{
  if (((context -> ContextFlags) & CONTEXT_INTEGER) != 0)
    {
      ULONG edi = (context -> ctx_RegEdi);
      if (((edi & SCHEME_ALIGNMENT_MASK) == 0)
	  && (((ULONG) heap_start) <= edi)
	  && (edi < ((ULONG) heap_end)))
	return (((SCHEME_OBJECT *) edi) + FREE_PARANOIA_MARGIN);
    }
  return (interpreter_free (1));
}

static SCHEME_OBJECT *
interpreter_free (int force_gc)
{
  return
    ((((force_gc ? heap_alloc_limit : heap_start) <= Free)
      && (Free < heap_end)
      && ((((ULONG) Free) & SCHEME_ALIGNMENT_MASK) == 0))
     ? (((Free + FREE_PARANOIA_MARGIN) < heap_alloc_limit)
	? (Free + FREE_PARANOIA_MARGIN)
	: (Free < heap_alloc_limit)
	? heap_alloc_limit
	: Free)
     : heap_alloc_limit);
}

/* Find the compiled code block in area which contains `pc_value'.
   This attempts to be more efficient than `find_block_address_in_area'.
   If the pointer is in the heap, it can actually do twice as
   much work, but it is expected to pay off on the average. */

#define MINIMUM_SCAN_RANGE		2048

static SCHEME_OBJECT *
find_block_address (char * pc_value, SCHEME_OBJECT * area_start)
{
  SCHEME_OBJECT * nearest_word
    = ((SCHEME_OBJECT *)
       (((unsigned long) pc_value) &~ SCHEME_ALIGNMENT_MASK));
  long maximum_distance = (nearest_word - area_start);
  long distance = maximum_distance;
  while ((distance / 2) > MINIMUM_SCAN_RANGE)
    distance = (distance / 2);
  while ((distance * 2) < maximum_distance)
    {
      SCHEME_OBJECT * block
	= (find_block_address_in_area (pc_value, (nearest_word - distance)));
      if (block != 0)
	return (block);
      distance *= 2;
    }
  return (find_block_address_in_area (pc_value, area_start));
}

/* Find the compiled code block in area which contains `pc_value', by
   scanning sequentially the complete area.  For the time being, skip
   over manifest closures and linkage sections. */

static SCHEME_OBJECT *
find_block_address_in_area (char * pc_value, SCHEME_OBJECT * area_start)
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
	    unsigned long count = (linkage_section_count (object));
	    area += 1;
	    switch (linkage_section_type (object))
	      {
	      case LINKAGE_SECTION_TYPE_OPERATOR:
	      case LINKAGE_SECTION_TYPE_GLOBAL_OPERATOR:
		area += (count * UUO_LINK_SIZE);
		break;

	      case LINKAGE_SECTION_TYPE_REFERENCE:
	      case LINKAGE_SECTION_TYPE_ASSIGNMENT:
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
	    if (((char *) (area + (count + 1))) < pc_value)
	      {
		area += (count + 1);
		first_valid = area;
		break;
	      }
	    {
	      SCHEME_OBJECT * block = (area - 1);
	      return
		(((area > first_valid)
		  && ((OBJECT_TYPE (*block)) == TC_MANIFEST_VECTOR)
		  && ((OBJECT_DATUM (*block)) >= (count + 1))
		  && (plausible_cc_block_p (block)))
		 ? block
		 : 0);
	    }
	  }

	default:
	  area += 1;
	  break;
	}
    }
  return (0);
}

static void
setup_trap_frame (PEXCEPTIONREPORTRECORD report,
		  PCONTEXTRECORD context,
		  trap_recovery_info_t * trinfo,
		  SCHEME_OBJECT * new_sp)
{
  long saved_mask;
  SCHEME_OBJECT handler;
  SCHEME_OBJECT trap_name;

  /* Disable interrupts while building stack frame.  */
  saved_mask = GET_INT_MASK;
  SET_INTERRUPT_MASK (0);

  /* Get the trap handler -- lose if there isn't one.  */
  handler
    = ((VECTOR_P (fixed_objects))
       ? (VECTOR_REF (fixed_objects, TRAP_HANDLER))
       : SHARP_F);
  if (!INTERPRETER_APPLICABLE_P (handler))
    {
      noise_start ();
      noise ("There is no trap handler for recovery!\n");
      noise ("This occurred during ");
      describe_exception ((report -> ExceptionNum), 0);
      noise (".\n");
      noise ("pc = %#08x; sp = %#08x.\n",
	     (context -> ctx_RegEip), (context -> ctx_RegEsp));
      (void) noise_end ("Exception Info", (MB_OK | MB_ERROR));
      termination_trap ();
    }

  /* Set the GC interrupt bit if necessary.  */
  if (!FREE_OK_P (Free))
    REQUEST_GC (0);

  /* Make sure the stack is correctly initialized.  */
  if (new_sp != 0)
    stack_pointer = new_sp;
  else
    {
      INITIALIZE_STACK ();
     Will_Push (CONTINUATION_SIZE);
      SET_RC (RC_END_OF_COMPUTATION);
      SET_EXP (SHARP_F);
      SAVE_CONT ();
     Pushed ();
    }
  {
    const char * name = (find_exception_name (report -> ExceptionNum));
    trap_name = ((name == 0) ? SHARP_F : (char_pointer_to_string (name)));
  }
  /* Push the hardware-trap stack frame.  The continuation parser will
     find this and use it to present meaningful debugging information
     to the user.  */
 Will_Push (7 + CONTINUATION_SIZE);
  STACK_PUSH (trinfo -> extra_trap_info);
  STACK_PUSH (trinfo -> pc_info_2);
  STACK_PUSH (trinfo -> pc_info_1);
  STACK_PUSH (trinfo -> state);
  STACK_PUSH (BOOLEAN_TO_OBJECT (new_sp != 0));
  STACK_PUSH (long_to_integer (report -> ExceptionNum));
  STACK_PUSH (trap_name);
  SET_RC (RC_HARDWARE_TRAP);
  SET_EXP (UNSPECIFIC);
  SAVE_CONT ();
 Pushed ();

  /* Make sure the history register is properly initialized.  */
  if ((new_sp != 0) && ((trinfo -> state) == STATE_COMPILED_CODE))
    stop_history ();
  history_register = (make_dummy_history ());

  /* Push the call frame for the trap handler.  */
 Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
  STACK_PUSH (trap_name);
  STACK_PUSH (handler);
  PUSH_APPLY_FRAME_HEADER (1);
 Pushed ();

  /* Restore the interrupt mask and call the handler.  */
  SET_INTERRUPT_MASK (saved_mask);
}

#define EXCEPTION_ENTRY(name, description) { name, #name, description }
static exception_entry_t exception_names [] =
{
  EXCEPTION_ENTRY (XCPT_ACCESS_VIOLATION, "access violation"),
  EXCEPTION_ENTRY (XCPT_ARRAY_BOUNDS_EXCEEDED, "array bounds exceeded"),
  EXCEPTION_ENTRY (XCPT_ASYNC_PROCESS_TERMINATE, "async process terminate"),
#ifdef XCPT_B1NPX_ERRATA_02
  EXCEPTION_ENTRY (XCPT_B1NPX_ERRATA_02, "B1NPX errata"),
#endif
  EXCEPTION_ENTRY (XCPT_BAD_STACK, "bad stack"),
  EXCEPTION_ENTRY (XCPT_BREAKPOINT, "breakpoint"),
  EXCEPTION_ENTRY (XCPT_DATATYPE_MISALIGNMENT, "data type misalignment"),
  EXCEPTION_ENTRY (XCPT_FLOAT_DENORMAL_OPERAND,
		   "floating point denormal operand"),
  EXCEPTION_ENTRY (XCPT_FLOAT_DIVIDE_BY_ZERO, "floating point divide by zero"),
  EXCEPTION_ENTRY (XCPT_FLOAT_INEXACT_RESULT, "floating point inexact result"),
  EXCEPTION_ENTRY (XCPT_FLOAT_INVALID_OPERATION,
		   "floating point invalid operation"),
  EXCEPTION_ENTRY (XCPT_FLOAT_OVERFLOW, "floating point overflow"),
  EXCEPTION_ENTRY (XCPT_FLOAT_STACK_CHECK, "floating point stack check"),
  EXCEPTION_ENTRY (XCPT_FLOAT_UNDERFLOW, "floating point underflow"),
  EXCEPTION_ENTRY (XCPT_GUARD_PAGE_VIOLATION, "guard page violation"),
  EXCEPTION_ENTRY (XCPT_ILLEGAL_INSTRUCTION, "illegal instruction"),
  EXCEPTION_ENTRY (XCPT_INTEGER_DIVIDE_BY_ZERO, "integer divide by zero"),
  EXCEPTION_ENTRY (XCPT_INTEGER_OVERFLOW, "integer overflow"),
  EXCEPTION_ENTRY (XCPT_INVALID_DISPOSITION, "invalid disposition"),
  EXCEPTION_ENTRY (XCPT_INVALID_LOCK_SEQUENCE, "invalid lock sequence"),
  EXCEPTION_ENTRY (XCPT_INVALID_UNWIND_TARGET, "invalid unwind target"),
  EXCEPTION_ENTRY (XCPT_IN_PAGE_ERROR, "in-page error"),
  EXCEPTION_ENTRY (XCPT_NONCONTINUABLE_EXCEPTION, "noncontinuable exception"),
  EXCEPTION_ENTRY (XCPT_PRIVILEGED_INSTRUCTION, "privileged instruction"),
  EXCEPTION_ENTRY (XCPT_PROCESS_TERMINATE, "process terminate"),
  EXCEPTION_ENTRY (XCPT_SIGNAL, "signal"),
  EXCEPTION_ENTRY (XCPT_SINGLE_STEP, "single step"),
  EXCEPTION_ENTRY (XCPT_UNABLE_TO_GROW_STACK, "unable to grow stack"),
  EXCEPTION_ENTRY (XCPT_UNWIND, "unwind")
};

static exception_entry_t *
find_exception_entry (ULONG exception_number)
{
  unsigned int i = 0;
  unsigned int end
    = ((sizeof (exception_names)) / (sizeof (exception_entry_t)));
  while (i < end)
    {
      if (exception_number == ((exception_names [i]) . number))
	return (& (exception_names [i]));
      i += 1;
    }
  return (0);
}

static const char *
find_exception_name (ULONG exception_number)
{
  exception_entry_t * entry = (find_exception_entry (exception_number));
  return ((entry == 0) ? 0 : (entry -> name));
}

static void
describe_exception (ULONG exception_number, int earlierp)
{
  exception_entry_t * entry = (find_exception_entry (exception_number));
  const char * prefix = (earlierp ? "earlier " : "");
  if (entry == 0)
    noise ("an %sunknown exception [code = %d]", prefix, exception_number);
  else
    noise ("a%s %s%s exception",
	   ((earlierp || (isvowel ((entry -> description) [0]))) ? "n" : ""),
	   prefix,
	   (entry -> description));
}

static int
isvowel (char c)
{
  return
    ((c == 'a') || (c == 'e') || (c == 'i') || (c == 'o') || (c == 'u')
     || (c == 'A') || (c == 'E') || (c == 'I') || (c == 'O') || (c == 'U'));
}

static char * noise_accumulator;
static char * noise_accumulator_position;

static void
noise_start (void)
{
  noise_accumulator = 0;
  noise_accumulator_position = 0;
}

static void
noise (const char * format, ...)
{
  unsigned int index = (noise_accumulator_position - noise_accumulator);
  noise_accumulator
    = ((noise_accumulator == 0)
       ? (OS_malloc (256))
       : (OS_realloc (noise_accumulator, (index + 256))));
  noise_accumulator_position = (noise_accumulator + index);
  {
    va_list arg_pointer;
    va_start (arg_pointer, format);
    noise_accumulator_position
      += (vsprintf (noise_accumulator_position, format, arg_pointer));
    va_end (arg_pointer);
  }
}

static USHORT
noise_end (const char * title, ULONG style)
{
  if (noise_accumulator == 0)
    return (MBID_YES);
  {
    USHORT rc
      = (WinMessageBox (HWND_DESKTOP,
			NULLHANDLE, /* client window handle */
			noise_accumulator,
			((char *) title),
			0,
			style));
    OS_free (noise_accumulator);
    noise_accumulator = 0;
    noise_accumulator_position = 0;
    return (rc);
  }
}

ULONG APIENTRY
OS2_subthread_exception_handler (PEXCEPTIONREPORTRECORD report,
				 PEXCEPTIONREGISTRATIONRECORD registration,
				 PCONTEXTRECORD context,
				 PVOID dispatcher_context)
{
  ULONG exception_number;
  PTIB ptib;
  PPIB ppib;
  TID tid;
  char * format
    = "Scheme has detected exception number %#08x within thread %d.%s%s\
  This indicates a bug in the Scheme implementation.\
  Please report this information to a Scheme wizard.\n\n";
  char backtrace [1024];

  if (((report -> fHandlerFlags)
       & (EH_UNWINDING | EH_EXIT_UNWIND | EH_STACK_INVALID | EH_NESTED_CALL))
      != 0)
    return (XCPT_CONTINUE_SEARCH);
  exception_number = (report -> ExceptionNum);
  if (! ((exception_number == XCPT_ACCESS_VIOLATION)
	 || (exception_number == XCPT_ARRAY_BOUNDS_EXCEEDED)
	 || (exception_number == XCPT_DATATYPE_MISALIGNMENT)
	 || (exception_number == XCPT_FLOAT_DENORMAL_OPERAND)
	 || (exception_number == XCPT_FLOAT_DIVIDE_BY_ZERO)
	 || (exception_number == XCPT_FLOAT_INEXACT_RESULT)
	 || (exception_number == XCPT_FLOAT_INVALID_OPERATION)
	 || (exception_number == XCPT_FLOAT_OVERFLOW)
	 || (exception_number == XCPT_FLOAT_STACK_CHECK)
	 || (exception_number == XCPT_FLOAT_UNDERFLOW)
	 || (exception_number == XCPT_ILLEGAL_INSTRUCTION)
	 || (exception_number == XCPT_INTEGER_DIVIDE_BY_ZERO)
	 || (exception_number == XCPT_INTEGER_OVERFLOW)
	 || (exception_number == XCPT_INVALID_LOCK_SEQUENCE)
	 || (exception_number == XCPT_PRIVILEGED_INSTRUCTION)))
    return (XCPT_CONTINUE_SEARCH);
  (void) dos_get_info_blocks ((&ptib), (&ppib));
  if (((context -> ContextFlags) & CONTEXT_CONTROL) == 0)
    (backtrace[0]) = '\0';
  else
    {
      ULONG * ebp = ((ULONG *) (context -> ctx_RegEbp));
      unsigned int count = 0;
      sprintf (backtrace, "  (Backtrace:");
      sprintf ((backtrace + (strlen (backtrace))), " %#08x",
	       (context -> ctx_RegEip));
      while ((((char *) ebp) > ((char *) (ptib -> tib_pstack)))
	     && (((char *) ebp) < ((char *) (ptib -> tib_pstacklimit)))
	     && (count < 10))
	{
	  sprintf ((backtrace + (strlen (backtrace))), " %#08x", (ebp[1]));
	  ebp = ((ULONG *) (ebp[0]));
	}
      sprintf ((backtrace + (strlen (backtrace))), ")");
    }
  tid = (ptib -> tib_ptib2 -> tib2_ultid);
  if (OS2_essential_thread_p (tid))
    {
      outf_fatal (format, exception_number, tid, backtrace, "");
      termination_init_error ();
    }
  else
    {
      char buffer [1024];
      sprintf (buffer, format, exception_number, tid, backtrace,
	       "  The thread will be killed.");
      OS2_message_box ("Scheme Error", buffer, 0);
      OS2_endthread ();
    }
}
