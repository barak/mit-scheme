/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosexcp.c,v 1.1 1992/05/05 06:55:13 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dos.h>
#include "dossys.h"
#include "dosinsn.h"
#include "dosexcp.h"

/* It would be nice to be able to use something akin to Zortech's int_intercept
   to get control of trap handlers, but Zortech does not provide that ability.
   In fact, it shadows the exception numbers with DOS interrupts (for compatibility),
   but does not map the traps to an accessible region.
   In the meantime, exceptions are only caught under DPMI.
*/

#if 0

#include <int.h>

static unsigned long *
DEFUN (store_trap_data, (trap_stack_ptr, intno, code, pd), 
       unsigned long ** trap_stack_ptr 
       AND unsigned char intno AND unsigned char code
       AND struct INT_DATA * pd)
{
  unsigned long 
    * trap_stack,
    * trapped_stack;
  
  union
  { 
    unsigned long long_value;
    struct
    { 
      unsigned char code;
      unsigned char ss_is_ds;
      unsigned char intno;
      unsigned char padding;
    } byte_values;
  } code_word;

  trap_stack = (* trap_stack_ptr);
  trapped_stack = ((unsigned long) pd->regs.oldstack_off);

  code_word.byte_values.code = code;
  code_word.byte_values.intno = intno;
  code_word.byte_values.ss_is_ds = (pd->oldstack_seg == pd->sregs.ds);
  code_word.byte_values.padding = 0;

  *--trap_stack = code_word.long_value;
  *--trap_stack = pd->regs.e.eax;
  *--trap_stack = pd->regs.e.ecx;
  *--trap_stack = pd->regs.e.edx;
  *--trap_stack = pd->regs.e.ebx;

  /* The following checks whether there was a ring change when the
     interrupt was taken.  If there was, the old SP is pushed on the
     exception trap frame which lives on the stack of the new
     privilege level, otherwise the trap frame was pushed on the 
     interrupted stack, which is shared by the low-level handler.
     Compare the PL of the two PCs.
   */
  *--trap_stack = (((pd->regs.sregs.cs & 0x3) == (trapped_stack[3] & 0x3))
                   ? (trapped_stack + 5)
		   : (trapped_stack[5]));	/* esp */
  *--trap_stack = trapped_stack[0];		/* ebp */
  *--trap_stack = pd->regs.e.esi;
  *--trap_stack = pd->regs.e.edi;
  *--trap_stack = trapped_stack[2];		/* eip */
  *--trap_stack = pd->regs.e.flags;
  (* trap_stack_ptr) = trap_stack;
  return (trapped_stack + 2);
}

static int
DEFUN (dosx_trap_handler, (intno, pd), 
       unsigned char intno AND struct INT_DATA * pd)
{
  extern void asm_trap_handler ();
  extern unsigned long
    * asm_trap_stack,
    * asm_trap_stack_limit,
    * asm_trap_stack_base;
  unsigned long * pc_loc;
  int code;

  code = 0;
  if (asm_trap_stack <= asm_trap_stack_limit)
  {
    /* Lose badly.  Too many nested traps. */
    asm_trap_stack = asm_trap_stack_base;
    code = 1;
  }
  pc_loc = store_trap_data (&asm_trap_stack, intno, code, pd);
  (* pc_loc) = ((unsigned long) asm_trap_handler);
  return (0);
}

#define DEFINE_TRAP_HANDLER(name,intno)		\
extern int EXFUN (name, (struct INT_DATA *));	\
int						\
DEFUN (name, (pd), struct INT_DATA * pd)	\
{						\
  return (dosx_trap_handler (intno, pd));	\
}

DEFINE_TRAP_HANDLER (handle_integer_divide_by_0, DOS_INTVECT_DIVIDE_BY_0)
DEFINE_TRAP_HANDLER (handle_overflow, DOS_INTVECT_OVERFLOW)
DEFINE_TRAP_HANDLER (handle_bounds_check, DOS_INTVECT_PRINT_SCREEN)
DEFINE_TRAP_HANDLER (handle_invalid_opcode, DOS_INVALID_OPCODE)
/* And many more friends. */

#endif /* 0 */

int
DPMI_get_exception_vector (unsigned exception,
			   unsigned short * cs_selector,
			   unsigned * code_offset)
{
  union REGS regs1, regs2;

  if (exception > 0x1f)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  regs1.e.eax = 0x202;
  regs1.e.ebx = exception;
  int86 (0x31, &regs1, &regs2);
  if ((regs2.e.flags & 1) != 0)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  * cs_selector = regs2.x.cx;
  * code_offset = regs2.e.edx;
  return (DOS_SUCCESS);
}

int
DPMI_set_exception_vector (unsigned exception,
			   unsigned short cs_selector,
			   unsigned code_offset)
{
  union REGS regs;
  struct SREGS sregs;
  
  if (exception > 0x1f)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  segread (& sregs);
  regs.e.eax = 0x203;
  regs.e.ebx = exception;
  regs.e.ecx = cs_selector;
  regs.e.edx = code_offset;
#ifdef _DOSEXCP_DEBUG
  if (exception == DOS_EXCP_General_protection)
  {
    printf ("About to do int86x for excp %d.\n", DOS_EXCP_General_protection);
    printf ("sregs.ds = 0x%04x; sregs.es = 0x%04x; sregs.fs = 0x%04x\n",
            sregs.ds, sregs.es, sregs.fs);
    printf ("sregs.gs = 0x%04x; sregs.ss = 0x%04x; sregs.cs = 0x%04x\n",
            sregs.gs, sregs.ss, sregs.cs);
    printf ("regs.e.eax = 0x%08x; regs.e.ebx = 0x%08x; regs.e.ecx = 0x%08x\n",
            regs.e.eax, regs.e.ebx, regs.e.ecx);
    printf ("regs.e.edx = 0x%08x\n", regs.e.edx);
    fflush (stdout);
    sleep (1);
  }
#endif
  int86x (0x31, &regs, &regs, &sregs);
  if ((regs.e.flags & 1) != 0)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  return (DOS_SUCCESS);
}

static void *
make_DPMI_exception_trampoline (unsigned exception,
				void ((*funcptr)
				      (unsigned,
				       unsigned,
				       struct sigcontext *)),
				void * stack)
{
  void DPMI_exception_method (void);
  void DPMI_GP_exception_method (void);
  void * trampoline;
  int size;
  INSN_DECLS();

  size = ((exception == DOS_EXCP_General_protection) ? 8 : 6);
  trampoline = (malloc (TRAMP_SIZE (size)));
  if (trampoline == ((void *) NULL))
  {
    errno = ENOMEM;
    return ((void *) NULL);
  }
  
  INIT_INSNS (trampoline);

  PUSH_INSN (exception);
  PUSH_INSN (getDS ());
#if 0
  PUSH_INSN (getCS ());
#else
  PUSH_INSN (0);		/* Use same CS and near calls and returns */
#endif
  PUSH_INSN (funcptr);
  PUSH_INSN (getDS ());		/* Assumed to be on Heap if not null! */
  PUSH_INSN (stack);
  if (exception == DOS_EXCP_General_protection)
  {
    unsigned short previous_cs;
    unsigned previous_eip;
    
    if ((DPMI_get_exception_vector (exception, & previous_cs, & previous_eip))
	!= DOS_SUCCESS)
    {
      free (trampoline);
      errno = EACCES;
      return ((void *) NULL);
    }
#ifdef _DOSEXCP_DEBUG
    printf ("Previous CS = 0x%04x; Previous EIP = 0x%08x\n",
	    previous_cs, previous_eip);
    printf ("Current CS = 0x%04x; Current SS = 0x%04x; Current DS = 0x%04x\n",
	    (getCS ()), (getSS ()), (getDS ()));
    fflush (stdout);
#endif
    PUSH_INSN (previous_cs);
    PUSH_INSN (previous_eip);
    JMP_INSN (DPMI_GP_exception_method);
  }
  else
    JMP_INSN (DPMI_exception_method);

  HLT_INSNS (size);

  return (trampoline);
}

int
DPMI_set_exception_handler (unsigned exception,
			    void ((*funcptr)
				  (unsigned,
				   unsigned,
				   struct sigcontext *)),
			    void * stack)
{
  void * handler;
  
  if (exception > 0x1f)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  handler = (make_DPMI_exception_trampoline (exception, funcptr, stack));
  if ((handler == ((void *) NULL))
      || ((DPMI_set_exception_vector (exception,
				      (getCS ()),
				      ((unsigned) handler)))
	  != DOS_SUCCESS))
  {
    int saved_errno = errno;

    if (handler != ((void *) NULL))
      free (handler);
    errno = saved_errno;
    return (DOS_FAILURE);
  }
  return (DOS_SUCCESS);
}

/* This assumes that it is undoing the effects of DPMI_set_exception_handler */

int
DPMI_restore_exception_handler (unsigned exception,
				unsigned short cs_selector,
				unsigned code_offset)
{
  unsigned short current_cs;
  unsigned current_eip;
  
  if (((DPMI_get_exception_vector (exception, & current_cs, & current_eip))
       != DOS_SUCCESS)
      || ((DPMI_set_exception_vector (exception, cs_selector, code_offset))
	  != DOS_SUCCESS))
    return (DOS_FAILURE);
  free ((void *) current_eip);
  return (DOS_SUCCESS);
}
