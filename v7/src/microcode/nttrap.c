/* -*-C-*-

$Id: nttrap.c,v 1.28 2007/01/05 15:33:07 cph Exp $

Copyright 1993,1997,1998,2000,2001,2002 Massachusetts Institute of Technology
Copyright 2006 Massachusetts Institute of Technology

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

#include <stdarg.h>
#include "scheme.h"
#include "os.h"
#include "nt.h"
#include "nttrap.h"
#include "gccode.h"
#include "ntscmlib.h"
#include <windows.h>

#ifdef __OPEN_WATCOM_14__
#  include <excpt.h>
#endif

#ifdef W32_TRAP_DEBUG
extern char * AskUser (char *, int);
extern int EXFUN (TellUser, (char *, ...));
extern int EXFUN (TellUserEx, (int, char *, ...));
#endif /* W32_TRAP_DEBUG */

extern void EXFUN (callWinntExceptionTransferHook, (void));
extern void EXFUN (NT_initialize_traps, (void));
extern void EXFUN (NT_restore_traps, (void));

extern DWORD
  C_Stack_Pointer,
  C_Frame_Pointer;

#ifdef W32_TRAP_DEBUG

static BOOL trap_verbose_p = FALSE;

#define IFVERBOSE(command) do						\
{									\
  if (trap_verbose_p)							\
  {									\
    int result = command;						\
    if (result == IDCANCEL)						\
      trap_verbose_p = FALSE;						\
  }									\
} while (0)

#else /* not W32_TRAP_DEBUG */

#define IFVERBOSE(command)		do { } while (0)

#endif /* W32_TRAP_DEBUG */

static char * trap_output = ((char *) NULL);
static char * trap_output_pointer = ((char *) NULL);

static void
DEFUN_VOID (trap_noise_start)
{
  trap_output = ((char *) NULL);
  trap_output_pointer = ((char *) NULL);
  return;
}

static void
DEFUN (trap_noise, (format), char * format DOTS)
{
  va_list arg_ptr;
  unsigned long size;
  char * temp;
  
  size = (trap_output_pointer - trap_output);
  temp = ((trap_output == ((char *) NULL))
	  ? ((char *) (malloc (256)))
	  : ((char *) (realloc (trap_output, (256 + size)))));
  if (temp == ((char *) NULL))
    return;

  trap_output = temp;
  trap_output_pointer = (temp + size);
  va_start (arg_ptr, format);
  size = (wvsprintf (trap_output_pointer, format, arg_ptr));
  trap_output_pointer += size;
  va_end (arg_ptr);
  return;
}

static int
DEFUN (trap_noise_end, (style), UINT style)
{
  int value;

  if (trap_output == ((char *) NULL))
    return (IDYES);

  value = (MessageBox (NULL,
		       trap_output,
		       "MIT/GNU Scheme Exception Information",
		       style));
  free (trap_output);
  trap_output = ((char *) NULL);
  trap_output_pointer = ((char *) NULL);
  return (value);
}

static BOOL
DEFUN (isvowel, (c), char c)
{
  switch (c)
  {
    case 'a':
    case 'e':
    case 'i':
    case 'o':
    case 'u':
    case 'A':
    case 'E':
    case 'I':
    case 'O':
    case 'U':
      return (TRUE);

    default:
      return (FALSE);
  }
}

struct exception_name_s
{
  DWORD code;
  char * name;
};

static struct exception_name_s exception_names[] =
{
 {
   EXCEPTION_ACCESS_VIOLATION,
   "ACCESS_VIOLATION",
 },
 {
   EXCEPTION_DATATYPE_MISALIGNMENT,
   "DATATYPE_MISALIGNMENT",
 },
 {
   EXCEPTION_BREAKPOINT,
   "BREAKPOINT",
 },
 {
   EXCEPTION_SINGLE_STEP,
   "SINGLE_STEP",
 },
 {
   EXCEPTION_ARRAY_BOUNDS_EXCEEDED,
   "ARRAY_BOUNDS_EXCEEDED",
 },
 {
   EXCEPTION_FLT_DENORMAL_OPERAND,
   "FLT_DENORMAL_OPERAND",
 },
 {
   EXCEPTION_FLT_DIVIDE_BY_ZERO,
   "FLT_DIVIDE_BY_ZERO",
 },
 {
   EXCEPTION_FLT_INEXACT_RESULT,
   "FLT_INEXACT_RESULT",
 },
 {
   EXCEPTION_FLT_INVALID_OPERATION,
   "FLT_INVALID_OPERATION",
 },
 {
   EXCEPTION_FLT_OVERFLOW,
   "FLT_OVERFLOW",
 },
 {
   EXCEPTION_FLT_STACK_CHECK,
   "FLT_STACK_CHECK",
 },
 {
   EXCEPTION_FLT_UNDERFLOW,
   "FLT_UNDERFLOW",
 },
 {
   EXCEPTION_INT_DIVIDE_BY_ZERO,
   "INT_DIVIDE_BY_ZERO",
 },
 {
   EXCEPTION_INT_OVERFLOW,
   "INT_OVERFLOW",
 },

 {
   EXCEPTION_PRIV_INSTRUCTION,
   "PRIV_INSTRUCTION",
 },
 {
   EXCEPTION_IN_PAGE_ERROR,
   "IN_PAGE_ERROR",
 },
 {
   EXCEPTION_ILLEGAL_INSTRUCTION,
   "ILLEGAL_INSTRUCTION",
 },
 {
   EXCEPTION_NONCONTINUABLE_EXCEPTION,
   "NONCONTINUABLE_EXCEPTION",
 },
 {
   EXCEPTION_STACK_OVERFLOW,
   "STACK_OVERFLOW",
 },
 {
   EXCEPTION_INVALID_DISPOSITION,
   "INVALID_DISPOSITION",
 },
};

const int excp_name_limit = ((sizeof (exception_names))
			     / (sizeof (struct exception_name_s)));

static char *
find_exception_name (DWORD code)
{
  int i;

  for (i = 0; i < excp_name_limit; i++)
    if (exception_names[i].code == code)
      return (exception_names[i].name);
  return ((char *) NULL);
}

static void
DEFUN (describe_trap, (noise, code),
       char * noise AND DWORD code)
{
  char * name;

  name = (find_exception_name (code));
  if (name == ((char *) NULL))
    trap_noise (">> The %s an unknown trap [code = %d].\n",
		noise, code);
  else
    trap_noise (">> The %s a%s %s trap.\n",
		noise,
		((isvowel (name[0])) ? "n" : ""),
		name);
  return;
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

struct nt_trap_code_desc
{
  int trapno;
  unsigned long code_mask;
  unsigned long code_value;
  char *name;
};

static enum trap_state trap_state;
static enum trap_state user_trap_state;

static enum trap_state saved_trap_state;
static DWORD saved_trap_code;

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
  extern void EXFUN (OS_restore_external_state, (void));

  trap_state = trap_state_exitting_hard;
  OS_restore_external_state ();
  exit (1);
}

void
DEFUN_VOID (NT_initialize_traps)
{
  trap_state = trap_state_recover;
  user_trap_state = trap_state_recover;
  (void) SetErrorMode (SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX);
}

void
DEFUN_VOID (NT_restore_traps)
{
  return;
}

static int
DEFUN (display_exception_information, (info, context, flags),
       PEXCEPTION_RECORD info AND PCONTEXT context AND int flags)
{
  int value;
  char msgbuf[4096];
  char * flag, * name, * bufptr;

  bufptr = &msgbuf[0];
  name = (find_exception_name (info->ExceptionCode));
  flag = ((info->ExceptionFlags == 0) ? "Continuable" : "Non-continuable");
  if (name == ((char *) NULL))
    bufptr += (sprintf (bufptr, "%s Unknown Exception %d Raised at address 0x%lx",
			flag, info->ExceptionCode, info->ExceptionAddress));
  else
    bufptr += (sprintf (bufptr, "%s %s Exception Raised at address 0x%lx",
			flag, name, info->ExceptionAddress));

#ifdef W32_TRAP_DEBUG
  if (context == ((PCONTEXT) NULL))
    bufptr += (sprintf (bufptr, "\nContext is NULL."));
  else
  {
    if ((context->ContextFlags & CONTEXT_CONTROL) != 0)
      bufptr += (sprintf (bufptr,
			  "\nContext contains CONTROL information."));
    if ((context->ContextFlags & CONTEXT_INTEGER) != 0)
      bufptr += (sprintf (bufptr,
			  "\nContext contains INTEGER registers."));
    if ((context->ContextFlags & CONTEXT_SEGMENTS) != 0)
      bufptr += (sprintf (bufptr,
			  "\nContext contains SEGMENT registers."));
    if ((context->ContextFlags & CONTEXT_FLOATING_POINT) != 0)
      bufptr += (sprintf (bufptr,
			  "\nContext contains floating-point registers."));
    bufptr += (sprintf (bufptr, "\ncontext->Eip        = 0x%lx.", context->Eip));
    bufptr += (sprintf (bufptr, "\ncontext->Esp        = 0x%lx.", context->Esp));
    bufptr += (sprintf (bufptr, "\nsp_register         = 0x%lx.", sp_register));
    bufptr += (sprintf (bufptr, "\nadj (sp_register) = 0x%lx.",
			(ADDR_TO_SCHEME_ADDR (sp_register))));
  }
#endif /* W32_TRAP_DEBUG */

  info = info->ExceptionRecord;
  if (info != ((PEXCEPTION_RECORD) NULL))
    bufptr += (sprintf (bufptr,
			"\nTrap occurred within an earlier trap."));

#ifdef W32_TRAP_DEBUG
  if (flags == MB_YESNO)
    bufptr += (sprintf (bufptr, "\n\nDisplay More Information?"));
#else /* not W32_TRAP_DEBUG */
  flags = MB_OK;
  bufptr +=
    (sprintf (bufptr,
	      "\n\nScheme cannot find the state necessary to continue."));
#endif /* W32_TRAP_DEBUG */

  value = (MessageBox (NULL, &msgbuf[0],
		       "MIT/GNU Scheme Exception Info",
		       (flags | MB_ICONSTOP)));
  return (value);
}

#define TEMP_STACK_LEN 2048	/* objects */

static BOOL
  return_by_aborting,
  clear_real_stack;

static SCHEME_OBJECT
  temp_stack_buffer[TEMP_STACK_LEN],
  * temp_stack = &temp_stack_buffer[0],
  * temp_stack_end = &temp_stack_buffer[TEMP_STACK_LEN],
  * temp_stack_limit,
  * real_stack_guard,
  * real_stack_pointer;

int
WinntExceptionTransferHook (void)
{
  /* These must be static because the memcpy below may
     be overwriting this procedure's locals!
   */

  static int size;
  static SCHEME_OBJECT * temp_stack_ptr, * new_sp;

  temp_stack_ptr = sp_register;
  size = (temp_stack_limit - temp_stack_ptr);
  IFVERBOSE (TellUserEx (MB_OKCANCEL, "WinntExceptionTransferHook."));

  if (clear_real_stack)
    INITIALIZE_STACK ();
  else
  {
    sp_register = real_stack_pointer;
    Stack_Guard = real_stack_guard;
  }
    
  new_sp = (real_stack_pointer - size);
  if (new_sp != temp_stack_ptr)
    memcpy (new_sp, temp_stack_ptr, (size * (sizeof (SCHEME_OBJECT))));
  sp_register = new_sp;
  SET_INTERRUPT_MASK ((FETCH_INTERRUPT_MASK ()));
  if (return_by_aborting)
    abort_to_interpreter (PRIM_APPLY);
  return (PRIM_APPLY);
}

extern unsigned short __cdecl EXFUN (getCS, (void));
extern unsigned short __cdecl EXFUN (getDS, (void));

/* Needed because Stack_Check checks for <= instead of < when pushing */

#define MAGIC_BUFFER_SIZE	1

static void
DEFUN (setup_trap_frame, (code, context, trinfo, new_stack_pointer),
       DWORD code
       AND PCONTEXT context
       AND struct trap_recovery_info * trinfo
       AND SCHEME_OBJECT * new_stack_pointer)
{
  SCHEME_OBJECT trap_name, trap_code;
  SCHEME_OBJECT handler;
  int stack_recovered_p = (new_stack_pointer != 0);
  long saved_mask = (FETCH_INTERRUPT_MASK ());
  SET_INTERRUPT_MASK (0);	/* To prevent GC for now. */

  IFVERBOSE (TellUserEx (MB_OKCANCEL,
			 "setup_trap_frame (%s, 0x%lx, %s, 0x%lx, 0x%lx).",
			 (find_exception_name (code)),
			 context,
			 trinfo,
			 new_stack_pointer));

  if ((! (Valid_Fixed_Obj_Vector ()))
      || ((handler = (Get_Fixed_Obj_Slot (Trap_Handler))) == SHARP_F))
    {
      trap_noise_start ();
      trap_noise ("There is no trap handler for recovery!\n");
      describe_trap ("trap is", code);
      (void) trap_noise_end (MB_OK | MB_ICONSTOP);
      termination_trap ();
    }
  if (Free > MemTop)
    Request_GC (0);

  trap_name = ((context == ((PCONTEXT) NULL))
	       ? SHARP_F
	       : (char_pointer_to_string (find_exception_name (code))));
  trap_code = (long_to_integer (0));

  if (win32_under_win32s_p ())
  {
    if (! stack_recovered_p)
      INITIALIZE_STACK ();
    clear_real_stack = FALSE;
    real_stack_pointer = sp_register;
    real_stack_guard = Stack_Guard;
    temp_stack_limit = sp_register;
  }
  else
  {
    clear_real_stack = (!stack_recovered_p);
    real_stack_pointer = new_stack_pointer;
    real_stack_guard = Stack_Guard;
    temp_stack_limit = temp_stack_end;
    sp_register = temp_stack_end;
    Stack_Guard = temp_stack;
  }

 Will_Push (7 + CONTINUATION_SIZE);
  STACK_PUSH (trinfo -> extra_trap_info);
  STACK_PUSH (trinfo -> pc_info_2);
  STACK_PUSH (trinfo -> pc_info_1);
  STACK_PUSH (trinfo -> state);
  STACK_PUSH (BOOLEAN_TO_OBJECT (stack_recovered_p));
  STACK_PUSH (trap_code);
  STACK_PUSH (trap_name);
  Store_Return (RC_HARDWARE_TRAP);
  exp_register = (long_to_integer (code));
  Save_Cont ();
 Pushed ();
  if (stack_recovered_p
      /* This may want to be done in other cases, but this may be enough. */
      && (trinfo->state == STATE_COMPILED_CODE))
    Stop_History ();

  history_register = (Make_Dummy_History ());
 Will_Push (STACK_ENV_EXTRA_SLOTS + 2);
  STACK_PUSH (trap_name);
  STACK_PUSH (handler);
  STACK_PUSH (STACK_FRAME_HEADER + 1);
 Pushed ();
  SET_INTERRUPT_MASK (saved_mask);

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "setup_trap_frame done."));
  return;
}

/* Heuristic recovery from processor traps/exceptions.

   continue_from_trap attempts to:

   1) validate the trap information (pc and sp);
   2) determine whether compiled code was executing,
      a primitive was executing,
      or execution was in the interpreter;
   3) guess what C global state is still valid; and
   4) set up a recovery frame for the interpreter so that debuggers can
      display more information. 
*/

#define SCHEME_ALIGNMENT_MASK		((sizeof (long)) - 1)
#define STACK_ALIGNMENT_MASK		SCHEME_ALIGNMENT_MASK
#define FREE_PARANOIA_MARGIN		0x100

/* PCs must be aligned according to this. */

#define PC_ALIGNMENT_MASK		((1 << PC_ZERO_BITS) - 1)

/* But they may have bits that can be masked by this. */

#ifndef PC_VALUE_MASK
# define PC_VALUE_MASK			(~0)
#endif

#define C_STACK_SIZE			0x01000000

#ifdef HAS_COMPILER_SUPPORT
# define ALLOW_ONLY_C 0
#else
# define ALLOW_ONLY_C 1
# define PLAUSIBLE_CC_BLOCK_P(block)	0
#endif

static SCHEME_OBJECT * EXFUN
  (find_block_address, (char * pc_value, SCHEME_OBJECT * area_start));

#define IA32_NREGS 12

/* For now */
#define GET_ETEXT() (Heap_Bottom)

static void
DEFUN (continue_from_trap, (code, context),
       DWORD code AND PCONTEXT context)
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
  long scheme_sp;
  long the_pc;
  SCHEME_OBJECT * new_stack_pointer;
  SCHEME_OBJECT * xtra_info;
  struct trap_recovery_info trinfo;
  extern int EXFUN (pc_to_utility_index, (unsigned long));
  extern int EXFUN (pc_to_builtin_index, (unsigned long));

  IFVERBOSE (TellUserEx (MB_OKCANCEL,
			 "continue_from_trap (%s, 0x%lx).",
			 (find_exception_name (code)), context));

  if (context == ((PCONTEXT) NULL))
  {
    if (Free < MemTop)
      Free = MemTop;
    setup_trap_frame (code, context, (&dummy_recovery_info), 0);
    /*NOTREACHED*/
  }

  if (context->SegSs == (getDS ()))
  {
    IFVERBOSE
      (TellUserEx
       (MB_OKCANCEL,
	"continue_from_trap: SS = C DS; sp_register = 0x%lx; Esp = 0x%lx.",
	sp_register, context->Esp));
    scheme_sp = (context->Esp);
  }
  else
  {
    IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap: SS unknown!"));
    scheme_sp = 0;
  }

  if (context->SegCs == (getCS ()))
  {
    IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap: CS = C CS."));
    the_pc = (context->Eip & PC_VALUE_MASK);
  }
  else
  {
    IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap: CS unknown"));
    goto pc_in_hyperspace;
  }

  if ((the_pc & PC_ALIGNMENT_MASK) != 0)
  {
pc_in_hyperspace:
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
    pc_in_C = ((the_pc <= ((long) (GET_ETEXT ()))) && (! pc_in_builtin));
    pc_in_heap =
      ((the_pc < ((long) Heap_Top)) && (the_pc >= ((long) Heap_Bottom)));
    pc_in_constant_space =
      ((the_pc < ((long) Constant_Top)) &&
       (the_pc >= ((long) Constant_Space)));
    pc_in_scheme = (pc_in_heap || pc_in_constant_space || pc_in_builtin);
    pc_in_hyper_space = ((!pc_in_C) && (!pc_in_scheme));
  }

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap 1"));

  scheme_sp_valid =
    (pc_in_scheme
     && ((scheme_sp < ((long) Stack_Top)) &&
	 (scheme_sp >= ((long) Stack_Bottom)) &&
	 ((scheme_sp & STACK_ALIGNMENT_MASK) == 0)));

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap 2"));

  new_stack_pointer =
    (scheme_sp_valid
     ? ((SCHEME_OBJECT *) scheme_sp)
     : ((pc_in_C
	&& (sp_register < Stack_Top)
	&& (sp_register > Stack_Bottom))
        ? sp_register
        : ((SCHEME_OBJECT *) 0)));

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap 3"));

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
    SCHEME_OBJECT * maybe_free;
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
      (trinfo . state) = STATE_PROBABLY_COMPILED;
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
      maybe_free = ((SCHEME_OBJECT *) context->Edi);
      if (((((unsigned long) maybe_free) & SCHEME_ALIGNMENT_MASK) == 0)
	  && (maybe_free >= Heap_Bottom) && (maybe_free < Heap_Top))
	Free = (maybe_free + FREE_PARANOIA_MARGIN);
      else
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
      (trinfo . state) = STATE_PROBABLY_COMPILED;
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

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap 4"));

  if (win32_under_win32s_p ())
    (trinfo . extra_trap_info) = SHARP_F;
  else
  {
    xtra_info = Free;
    Free += (1 + (IA32_NREGS + 2));
    (trinfo . extra_trap_info) =
      (MAKE_POINTER_OBJECT (TC_NON_MARKED_VECTOR, xtra_info));
    (*xtra_info++) = (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, (IA32_NREGS + 2)));
    (*xtra_info++) = ((SCHEME_OBJECT) the_pc);
    (*xtra_info++) = ((SCHEME_OBJECT) scheme_sp);
    {
      int counter = IA32_NREGS;
      int * regs = ((int *) context->Edi);
      while ((counter--) > 0)
	(*xtra_info++) = ((SCHEME_OBJECT) (*regs++));
    }
  }

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap 5"));

  /* Handshake with try+except. */

  context->Eip = ((DWORD) callWinntExceptionTransferHook);
  context->SegCs = (getCS ());
  return_by_aborting = TRUE;

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap 6"));

  if (pc_in_scheme && (! (win32_under_win32s_p ())))
  {
    context->Esp = C_Stack_Pointer;
    context->Ebp = C_Frame_Pointer;
    if (pc_in_scheme)
      return_by_aborting = FALSE;
  }

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap 7"));

  setup_trap_frame (code, context, (&trinfo), new_stack_pointer);

  IFVERBOSE (TellUserEx (MB_OKCANCEL, "continue_from_trap 8"));
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
		(((area == first_valid)
		  || ((OBJECT_TYPE (* block)) != TC_MANIFEST_VECTOR)
		  || ((OBJECT_DATUM (* block)) < ((unsigned long) (count + 1)))
		  || (! (PLAUSIBLE_CC_BLOCK_P (block))))
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

static void
DEFUN (trap_recover, (code, context),
       DWORD code AND PCONTEXT context)
{
  IFVERBOSE (TellUserEx (MB_OKCANCEL,
			 "trap_recover (%s, 0x%lx).",
			 (find_exception_name (code)), context));

  if (WITHIN_CRITICAL_SECTION_P ())
    {
      CLEAR_CRITICAL_SECTION_HOOK ();
      EXIT_CRITICAL_SECTION ({});
    }
  reset_interruptable_extent ();
  continue_from_trap (code, context);
}

static void
DEFUN (nt_trap_handler, (code, context),
       DWORD code AND PCONTEXT context)
{
  Boolean stack_overflowed_p = (STACK_OVERFLOWED_P ());
  enum trap_state old_trap_state = trap_state;
  int flags;

  IFVERBOSE (TellUserEx (MB_OKCANCEL,
			 "nt_trap_handler (%s, 0x%lx).",
			 (find_exception_name (code)), context));

  if (old_trap_state == trap_state_exitting_hard)
    _exit (1);
  else if (old_trap_state == trap_state_exitting_soft)
    trap_immediate_termination ();

  trap_state = trap_state_trapped;

  trap_noise_start ();
  if (WITHIN_CRITICAL_SECTION_P ())
  {
    trap_noise (">> The system has trapped within critical section \"%s\".\n",
		(CRITICAL_SECTION_NAME ()));
    describe_trap ("trap is", code);
  }
  else if (stack_overflowed_p || (old_trap_state != trap_state_recover))
  {
    trap_noise (">> The system has trapped.\n");
    describe_trap ("trap is", code);
  }
  if (stack_overflowed_p)
  {
    trap_noise (">> The stack has overflowed overwriting adjacent memory.\n");
    trap_noise (">> This was probably caused by a runaway recursion.\n");
  }

  switch (old_trap_state)
  {
  case trap_state_trapped:
    if ((saved_trap_state == trap_state_recover)
	|| (saved_trap_state == trap_state_query))
    {
      trap_noise (">> The trap occurred while processing an earlier trap.\n");
      describe_trap ("earlier trap was", saved_trap_code);
      trap_noise ((WITHIN_CRITICAL_SECTION_P ())
		  ? ">> Successful recovery is extremely unlikely.\n"
		  : ">> Successful recovery is unlikely.\n");
      break;
    }
    else
    { 
      (void) trap_noise_end (MB_OK | MB_ICONSTOP);
      trap_immediate_termination ();
    }

  case trap_state_recover:
    if ((WITHIN_CRITICAL_SECTION_P ()) || stack_overflowed_p)
    {
      trap_noise (">> Successful recovery is unlikely.\n");
      break;
    }
    else
    {
      saved_trap_state = old_trap_state;
      saved_trap_code = code;
      (void) trap_noise_end (MB_OK | MB_ICONSTOP);
      trap_recover (code, context);
      return;
    }
  case trap_state_exit:
    (void) trap_noise_end (MB_OK | MB_ICONSTOP);
    termination_trap ();
  }

  trap_noise ("\n");
  saved_trap_state = old_trap_state;
  saved_trap_code = code;
  flags = MB_ICONSTOP;

  while (1)
  {
    trap_noise ("Attempt recovery?");
    if ((trap_noise_end (MB_YESNO | flags)) == IDYES)
    {
      trap_recover (code, context);
      return;
    }
    flags = 0;

    trap_noise ("Terminate Scheme normally?");
    switch (trap_noise_end (MB_YESNOCANCEL))
    {
      case IDYES:
        trap_normal_termination ();

      case IDNO:
        trap_immediate_termination ();
	_exit (1);

      default:
	break;
    }
  }
}

#ifdef W32_TRAP_DEBUG

static void
DEFUN (parse_response, (buf, addr, len),
       char * buf AND unsigned long * addr AND int * len)
{
  const char * separators = " ,\t;";
  char * token;

  token = (strtok (buf, separators));
  if (token == ((char *) NULL))
    return;
  * addr = (strtoul (token, ((char **) NULL), 0));
  token = (strtok (((char *) NULL), separators));
  if (token == ((char *) NULL))
    return;
  * len = ((int) (strtoul (token, ((char **) NULL), 0)));
  return;
}

static void
DEFUN (tinyexcpdebug, (code, info),
       DWORD code AND LPEXCEPTION_POINTERS info)
{
  int count, len;
  char * message;
  unsigned long * addr;
  char responsebuf[256], * response;
 
  if ((MessageBox
       (NULL, "Debug?", "MIT/GNU Scheme Exception Debugger", MB_YESNO))
      != IDYES)
    return;

  message = "&info =";
  addr = ((unsigned long *) (& info));
  len = 1;

  while (1)
  {
    trap_noise_start ();
    trap_noise ("%s 0x%lx.\n", message, ((unsigned long) addr));
    for (count = 0; count < len; count++)
      trap_noise ("\n*0x%08x\t= 0x%08x\t= %d.",
		  (addr + count),
		  addr[count],
		  addr[count]);
    trap_noise ("\n\nMore?");
    if ((trap_noise_end (MB_YESNO)) != IDYES)
      break;
    response = (AskUser (&responsebuf[0], (sizeof (responsebuf))));
    if (response == ((char *) NULL))
      continue;
    message = "Contents of";
    parse_response (&responsebuf[0], &addr, &len);
  }
  return;
}
#endif /* W32_TRAP_DEBUG */

#ifndef PAGE_SIZE
# define PAGE_SIZE 0x1000
#endif

static Boolean stack_protected = FALSE;
unsigned long protected_stack_base;
unsigned long protected_stack_end;

void
DEFUN_VOID (win32_unprotect_stack)
{
  DWORD old_protection;

  if ((stack_protected)
      && (VirtualProtect (((LPVOID) protected_stack_base),
			  PAGE_SIZE,
			  PAGE_READWRITE,
			  &old_protection)))
    stack_protected = FALSE;
  return;
}

void
DEFUN_VOID (win32_protect_stack)
{
  DWORD old_protection;

  if ((! stack_protected)
      && (VirtualProtect (((LPVOID) protected_stack_base),
			  PAGE_SIZE,
			  (PAGE_GUARD | PAGE_READWRITE),
			  &old_protection)))
    stack_protected = TRUE;
 return; 
}

void
DEFUN_VOID (win32_stack_reset)
{
  unsigned long boundary;

  /* This presumes that the distance between Stack_Bottom and
     Stack_Guard is at least a page.
   */

  boundary = ((((unsigned long) Stack_Guard)
	       & (~ ((unsigned long) (PAGE_SIZE - 1))))
	      - (2 * PAGE_SIZE));
  if (stack_protected && (protected_stack_base == boundary))
    return;
  win32_unprotect_stack ();
  protected_stack_base = boundary;
  protected_stack_end  = (boundary + PAGE_SIZE);
  win32_protect_stack ();
  return;
}

#define EXCEPTION_CODE_GUARDED_PAGE_ACCESS	0x80000001L

static LONG
DEFUN (WinntException, (code, info),
       DWORD code AND LPEXCEPTION_POINTERS info)
{
  PCONTEXT context;

  context = info->ContextRecord;
  if ((info->ExceptionRecord->ExceptionFlags != 0)
      || (context == ((PCONTEXT) NULL))
      || ((context->ContextFlags & CONTEXT_CONTROL) == 0)
      || ((context->ContextFlags & CONTEXT_INTEGER) == 0)
      || ((context->ContextFlags & CONTEXT_SEGMENTS) == 0))
  {
    (void)
      display_exception_information (info->ExceptionRecord,
				     info->ContextRecord,
				     MB_OK);
    trap_immediate_termination ();
    /*NOTREACHED*/
    return (0);
  }
  else if (code == EXCEPTION_CODE_GUARDED_PAGE_ACCESS)
  {
    if (stack_protected
	&& (context->Esp >= protected_stack_base)
	&& (context->Esp <= protected_stack_end))
      REQUEST_INTERRUPT (INT_Stack_Overflow);
    /* Just in case */
    stack_protected = FALSE;
    return (EXCEPTION_CONTINUE_EXECUTION);
  }
  else
  {
#ifdef W32_TRAP_DEBUG
    trap_verbose_p = ((display_exception_information
		       (info->ExceptionRecord,
			info->ContextRecord,
			MB_YESNO))
		      == IDYES);
    tinyexcpdebug (code, info);
#endif /* W32_TRAP_DEBUG */
    nt_trap_handler (code, context);
    return (EXCEPTION_CONTINUE_EXECUTION);
  }
}

#if (defined(__WATCOMC__) && (__WATCOMC__ < 1100))
/* Watcom 10 has broken __try/__except support,
   which has been fixed in version 11.  */
#define USE_SET_UNHANDLED_EXCEPTION_FILTER
#endif

#ifdef USE_SET_UNHANDLED_EXCEPTION_FILTER
static LONG WINAPI
scheme_unhandled_exception_filter (LPEXCEPTION_POINTERS info)
{
  return (WinntException (((info -> ExceptionRecord) -> ExceptionCode), info));
}
#endif /* USE_SET_UNHANDLED_EXCEPTION_FILTER */

void
win32_enter_interpreter (void (*enter_interpreter) (void))
{
#ifdef USE_SET_UNHANDLED_EXCEPTION_FILTER
  (void) SetUnhandledExceptionFilter (scheme_unhandled_exception_filter);
  (* enter_interpreter) ();
  outf_fatal ("Exception!\n");
  termination_trap ();
#else /* not USE_SET_UNHANDLED_EXCEPTION_FILTER */
  do
  {
    __try
    {
      (* enter_interpreter) ();
    }
    __except (WinntException ((GetExceptionCode ()),
			      (GetExceptionInformation ())))
    {
      outf_fatal ("Exception!\n");
      termination_trap ();
    }
  } while (1);
#endif /* not USE_SET_UNHANDLED_EXCEPTION_FILTER */
}
