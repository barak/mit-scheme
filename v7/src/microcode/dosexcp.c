/* -*-C-*-

$Id: dosexcp.c,v 1.4 1992/09/19 19:05:17 jinx Exp $

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
  PUSH_INSN (0);		/* Use same CS and near calls and returns */
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

extern int DPMI_free_scheme_stack (unsigned short);
extern int DPMI_alloc_scheme_stack (unsigned short *,
				    unsigned short *,
				    unsigned long);

int
DPMI_free_scheme_stack (unsigned short ss)
{
  union REGS regs;

  regs.x.ax = 0x1;
  regs.x.bx = ss;
  int86 (0x31, &regs, &regs);
  return ((regs.e.cflag != 0) ? DOS_FAILURE : DOS_SUCCESS);
}

#define I386_PAGE_BITS	12
#define I386_PAGE_SIZE	(1 << I386_PAGE_BITS)
#define I386_PAGE_MASK	(I386_PAGE_SIZE - 1) 

int
DPMI_alloc_scheme_stack (unsigned short * ds,
			 unsigned short * ss,
			 unsigned long limit)
{
  unsigned short ds_sel, css_sel, ss_sel;
  unsigned long descriptor[2];
  struct SREGS sregs;
  union REGS regs;

  segread (&sregs);
  css_sel = sregs.ss;
  ds_sel = sregs.ds;

  regs.x.ax = 0x0;		/* Allocate LDT Descriptor */
  regs.x.cx = 1;
  int86 (0x31, &regs, &regs);
  if (regs.e.cflag != 0)
    return (DOS_FAILURE);
  ss_sel = regs.x.ax;

  sregs.es = ds_sel;
  regs.x.ax = 0xb;		/* Get Descriptor */
  regs.x.bx = css_sel;
  regs.e.edi = ((unsigned long) &descriptor[0]);
  int86x (0x31, &regs, &regs, &sregs);
  if (regs.e.cflag != 0)
  {
fail:
    DPMI_free_scheme_stack (ss_sel);
    fprintf (stderr, "DPMI_alloc_scheme_stack: failed.\n");
    return (DOS_FAILURE);    
  }
  
  /* Set the granularity bit and the limit */
  descriptor[1] = (descriptor[1] | (1UL << 23));
  descriptor[1] &= (~ (0xfUL << 16));
  descriptor[1] |= ((limit >> I386_PAGE_BITS) & (0xfUL << 16));

  descriptor[0] &= 0xffff0000UL;
  descriptor[0] |= ((limit >> I386_PAGE_BITS) & 0xffff);

  regs.x.ax = 0xc;		/* Set Descriptor */
  regs.x.bx = ss_sel;
  regs.e.edi = ((unsigned long) &descriptor[0]);
  int86x (0x31, &regs, &regs, &sregs);
  if (regs.e.cflag != 0)
    goto fail;

  *ds = ds_sel;
  *ss = ss_sel;
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
