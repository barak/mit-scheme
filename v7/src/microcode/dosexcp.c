/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosexcp.c,v 1.2 1992/07/28 14:34:05 jinx Exp $

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
   to get control of trap handlers, but Zortech's DOSX does not
   provide that ability.  In fact, it shadows the exception numbers
   with DOS interrupts (for compatibility), but does not map the traps
   to an accessible region.

   In the meantime, exceptions are only caught under DPMI if running DOSX,
   or everywhere if running X32.
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
  
  if (exception > 0x1f)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  regs.e.eax = 0x203;
  regs.e.ebx = exception;
  regs.e.ecx = cs_selector;
  regs.e.edx = code_offset;
  int86 (0x31, &regs, &regs);
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

int
X32_get_exception_vector (unsigned exception,
			  unsigned short * cs_selector,
			  unsigned * code_offset)
{
  struct SREGS sregs;
  union REGS regs;

  if (exception > 15)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  segread (&sregs);
  regs.e.eax = 0x2532;
  regs.h.cl = exception;
  int86x (0x21, &regs, &regs, &sregs);
  if ((regs.e.flags & 1) != 0)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  * cs_selector = sregs.es;
  * code_offset = regs.e.ebx;
  return (DOS_SUCCESS);
}

static int
X32_set_exception_vector (unsigned exception,
			  unsigned short cs_selector,
			  unsigned code_offset)
{
  union REGS regs;
  struct SREGS sregs;
  
  if (exception > 15)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  segread (& sregs);
  regs.e.eax = 0x2533;
  regs.h.cl = exception;
  sregs.ds = cs_selector;
  regs.e.edx = code_offset;

  int86x (0x21, &regs, &regs, &sregs);
  if ((regs.e.flags & 1) != 0)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }
  return (DOS_SUCCESS);
}

struct X32_excp_handler
{
  unsigned esp;
  unsigned ss;
  unsigned eip;
  unsigned cs;
  unsigned ds;
};

extern struct X32_excp_handler X32_excp_handlers[];

int
X32_set_exception_handler (unsigned exception,
			   void ((*funcptr)
				 (unsigned,
				  unsigned,
				  struct sigcontext *)),
			   void * stack)
{
  unsigned short cs, ds;
  struct X32_excp_handler * handler, old_handler;
  extern void X32_exception_method (void);

  if (exception > 15)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }

  cs = (getCS ());
  ds = (getDS ());

  handler = &X32_excp_handlers[exception];

  old_handler.esp = handler->esp;
  old_handler.ss = handler->ss;
  old_handler.eip = handler->eip;
  old_handler.cs = handler->cs;
  old_handler.ds = handler->ds;

  handler->esp = ((unsigned) stack);
  handler->ss  = ds;
  handler->eip = ((unsigned) funcptr);
  handler->cs  = 0;	/* 0 means handler expects near call */
  handler->ds = ds;
  
  if ((X32_set_exception_vector (exception,
				 cs,
				 ((unsigned) X32_exception_method)))
      != DOS_SUCCESS)
  {
    handler->esp = old_handler.esp;
    handler->ss = old_handler.ss;
    handler->eip = old_handler.eip;
    handler->cs = old_handler.cs;
    handler->ds = old_handler.ds;

    return (DOS_FAILURE);
  }
  return (DOS_SUCCESS);
}

/* This assumes that it is undoing the effects of X32_set_exception_handler */

int
X32_restore_exception_handler (unsigned exception,
			       unsigned short cs_selector,
			       unsigned code_offset)
{
  struct X32_excp_handler * handler, old_handler;

  if (exception > 15)
  {
    errno = EINVAL;
    return (DOS_FAILURE);
  }

  handler = &X32_excp_handlers[exception];

  old_handler.esp = handler->esp;
  old_handler.ss = handler->ss;
  old_handler.eip = handler->eip;
  old_handler.cs = handler->cs;
  old_handler.ds = handler->ds;

  handler->esp = 0;
  handler->ss  = 0;
  handler->eip = 0;
  handler->cs  = 0;
  handler->ds = 0;
  
  if ((X32_set_exception_vector (exception, cs_selector, code_offset))
      != DOS_SUCCESS)
  {
    handler->esp = old_handler.esp;
    handler->ss = old_handler.ss;
    handler->eip = old_handler.eip;
    handler->cs = old_handler.cs;
    handler->ds = old_handler.ds;

    return (DOS_FAILURE);
  }
  return (DOS_SUCCESS);
}
