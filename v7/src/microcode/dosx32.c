/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/dosx32.c,v 1.1 1992/07/28 17:55:42 jinx Exp $

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

#include <int.h>
#include <stdio.h>
#include "ansidecl.h"

/* Exports */

extern int EXFUN (X32_lock_scheme_microcode, (void));

extern int EXFUN (under_X32_p, (void));

#ifdef USE_LOCKED_INT_INTERCEPT

extern int EXFUN (locked_int_intercept,
		  (unsigned,
		   int (_cdecl * funcptr) (struct INT_DATA *),
		   unsigned));

extern void EXFUN (locked_int_restore, (unsigned));

/* Imports */

extern void EXFUN (int_service, (void));

#else /* not USE_LOCKED_INT_INTERCEPT */

extern int EXFUN (X32_interrupt_restore, (unsigned));

extern int EXFUN (X32_int_intercept, (unsigned, void (*) (void), PTR));

#endif /* USE_LOCKED_INT_INTERCEPT */

extern unsigned short EXFUN (getCS, (void));

extern unsigned short EXFUN (getDS, (void));

#define I486_PAGE_SIZE 4096

#define OPERATION_LOCK		0
#define OPERATION_UNLOCK	1

static unsigned short x32_lock_methods[2] = { 0x501, 0x601 };

static int
DEFUN (lock_unlock, (operation, segment, offset, size),
       unsigned short operation
       AND unsigned short segment
       AND PTR offset
       AND unsigned long size)
{
  union REGS regs;
  struct SREGS sregs;
  unsigned long base;
  unsigned long bound;
  unsigned long page_aligned_size;
  
  base = ((unsigned long) offset);
  page_aligned_size = size;

  bound = (base + size);
  base = (base & (~ (I486_PAGE_SIZE - 1)));
  bound = ((bound + (I486_PAGE_SIZE - 1)) & (~ (I486_PAGE_SIZE - 1)));
  page_aligned_size = (bound - base);
  
  regs.x.ax = 0x252b;
  regs.x.bx = x32_lock_methods[operation];
  regs.e.edx = page_aligned_size;
  regs.e.ecx = base;
  segread (&sregs);
  sregs.es = segment;
  int86x (0x21, &regs, &regs, &sregs);
  return (((regs.e.flags & 1) != 0) ? -1 : 0);
}

int
DEFUN (lock_region, (segment, offset, size),
       unsigned short segment
       AND PTR offset
       AND unsigned long size)
{
  return (lock_unlock (OPERATION_LOCK, segment, offset, size));
}

int
DEFUN (unlock_region, (segment, offset, size),
       unsigned short segment
       AND PTR offset
       AND unsigned long size)
{
  return (lock_unlock (OPERATION_UNLOCK, segment, offset, size));
}

#ifdef USE_LOCKED_INT_INTERCEPT

extern PTR EXFUN (malloc, (unsigned long));
extern int EXFUN (free, (PTR));

PTR
DEFUN (locked_malloc, (size), unsigned long size)
{
  PTR block;

  block = (malloc (size));
  if (block == ((PTR) NULL))
    return (block);
  if ((lock_region ((getDS ()), block, size)) != 0)
  {
    free (block);
    return ((PTR) NULL);
  }
  return (block);
}

void
DEFUN (locked_free, (block, size),
       PTR block AND unsigned long size)
{
  if ((unlock_region ((getDS ()), block, size)) != 0)
    return;
  free (block);
  return;
}

#pragma ZTC align 1			/* no alignment for DOS structs	*/

struct trampoline
{
  unsigned char callf;		/* CALLF opcode */
  PTR off;			/* CALLF offset */
  unsigned short seg;		/* CALLF segment */
  struct INT_DATA closure;
  unsigned short datasel;
};

#pragma ZTC align


#define MIN_STACK_SIZE 128


void
DEFUN (locked_int_restore, (vectornumber),
       unsigned vectornumber)
{
  union REGS regs;
  struct SREGS sregs;
  struct trampoline * tramp;
  struct INT_DATA * clos;
  
  /* Get protected mode interrupt vector. */

  {
    regs.x.ax = 0x2502;
    regs.h.cl = vectornumber;
    segread (&sregs);
    int86x (0x21, &regs, &regs, &sregs);
    if ((regs.e.flags & 1) != 0)
      return;
    tramp = ((struct trampoline *) regs.e.ebx);
  }

  clos = &tramp->closure;

  /* Restore previous vectors */
  {
    regs.x.ax = 0x2507;
    regs.h.cl = vectornumber;
    segread (&sregs);
    sregs.ds = clos->prevvec_seg;
    regs.e.edx = clos->prevvec_off;
    regs.e.ebx = (* ((unsigned long *) &clos->prevvecr_off));
    int86x (0x21, &regs, &regs, &sregs);
    if ((regs.e.flags & 1) != 0)
      return;
  }

  if (clos->stacksize != 0)
    locked_free (((PTR) (((char *) clos->newstack_off) - clos->stacksize)),
		 clos->stacksize);

  locked_free (((PTR) tramp), (sizeof (struct trampoline)));
  return;
}

int
DEFUN (locked_int_intercept, (vectornumber, handler, stacksize),
       unsigned vectornumber
       AND int EXFUN ((_cdecl * handler), (struct INT_DATA *))
       AND unsigned stacksize)
{
  extern void EXFUN (int_setvector, (unsigned, unsigned, unsigned));
  union REGS regs;
  struct SREGS sregs;
  struct trampoline * tramp;
  struct INT_DATA * clos;
  
  tramp = ((struct trampoline *)
	   (locked_malloc (sizeof (struct trampoline))));
  if (tramp == ((struct trampoline *) NULL))
    return (-1);
  
  tramp->callf = 0x9a;
  tramp->off = ((PTR) int_service);
  tramp->seg = (getCS ());
  
  clos = &tramp->closure;

  if (stacksize == 0)
  {
    clos->stacksize = 0;
    clos->newstack_off = 0;
    clos->newstack_seg = 0;
  }
  else
  {
    PTR stack;

    if (stacksize < MIN_STACK_SIZE)
      stacksize = MIN_STACK_SIZE;
    
    stack = (locked_malloc (stacksize));
    if (stack == ((PTR) NULL))
    {
      locked_free (((PTR) tramp), (sizeof (struct trampoline)));
      return (-1);
    }

    clos->stacksize = stacksize;
    clos->newstack_off = ((unsigned) (((char *) stack) + stacksize));
    clos->newstack_seg = (getDS ());
  }
  clos->funcptr = handler;
  
  /* Preserve previous real mode interrupt handler */

  {
    regs.x.ax = 0x2503;
    regs.h.cl = vectornumber;
    int86 (0x21, &regs, &regs);
    if ((regs.e.flags & 1) != 0)
    {
error_getting_old_handlers:
      if (clos->stacksize != 0)
	locked_free (((PTR) (((char *) clos->newstack_off) - clos->stacksize)),
		     clos->stacksize);
      locked_free (((PTR) tramp), (sizeof (struct trampoline)));
      return (-1);
    }
    * ((unsigned *) &clos->prevvecr_off) = regs.e.ebx;
  }

  /* Preserve previous protected mode interrupt handler. */
  
  {
    regs.x.ax = 0x2502;
    regs.h.cl = vectornumber;
    segread (&sregs);
    int86x (0x21, &regs, &regs, &sregs);
    if ((regs.e.flags & 1) != 0)
      goto error_getting_old_handlers;

    clos->prevvec_off = regs.e.ebx;
    clos->prevvec_seg = sregs.es;
  }
  tramp->datasel = (getDS ());

  int_setvector (vectornumber, ((unsigned) tramp), (getCS ()));
  return (0);
}
#endif /* USE_LOCKED_INT_INTERCEPT */

int
DEFUN_VOID (X32_lock_scheme_microcode)
{
  extern unsigned X32_locked_data_start;
  extern unsigned X32_locked_data_end;
#ifdef USE_LOCKED_INT_INTERCEPT
  extern int EXFUN (bios_timer_handler, (struct INT_DATA *));
  extern void EXFUN (bios_timer_handler_end, (void));
#else
  extern void EXFUN (X32_locked_code_start, (void));
  extern void EXFUN (X32_locked_code_end, (void));
#endif
  unsigned short cs, ds;

  ds = (getDS ());
  cs = (getCS ());

  if ((lock_region (ds, ((PTR) &X32_locked_data_start),
		    (((unsigned long) &X32_locked_data_end)
		     - ((unsigned long) &X32_locked_data_start))))
      != 0)
    return (-1);

#ifdef USE_LOCKED_INT_INTERCEPT
  if ((lock_region (cs, ((PTR) bios_timer_handler),
		    (((unsigned long) bios_timer_handler_end)
		     - ((unsigned long) bios_timer_handler))))
      != 0)
  {
    unlock_region (ds, ((PTR) &X32_locked_data_start),
		   (((unsigned long) &X32_locked_data_end)
		    - ((unsigned long) &X32_locked_data_start)));
    return (-1);
  }
  if ((lock_region (cs, ((PTR) int_service),
		    (((unsigned long) int_intercept)
		     - ((unsigned long) int_service))))
      != 0)
  {
    unlock_region (ds, ((PTR) &X32_locked_data_start),
		   (((unsigned long) &X32_locked_data_end)
		    - ((unsigned long) &X32_locked_data_start)));
    unlock_region (cs, ((PTR) bios_timer_handler),
		   (((unsigned long) bios_timer_handler_end)
		    - ((unsigned long) bios_timer_handler)));
    return (-1);
  }
#else /* not USE_LOCKED_INT_INTERCEPT */

  if ((lock_region (cs, ((PTR) X32_locked_code_start),
		    (((unsigned long) X32_locked_code_end)
		     - ((unsigned long) X32_locked_code_start))))
      != 0)
  {
    unlock_region (ds, ((PTR) &X32_locked_data_start),
		   (((unsigned long) &X32_locked_data_end)
		    - ((unsigned long) &X32_locked_data_start)));
    return (-1);
  }

#endif /* USE_LOCKED_INT_INTERCEPT */
  return (0);
}

int
DEFUN_VOID (under_X32_p)
{
  union REGS regs;
  
  regs.x.bx = (getDS ());
  regs.x.ax = 0x3504;
  int86 (0x21, &regs, &regs);
  return ((regs.e.flags & 1) == 0);
}

#ifndef USE_LOCKED_INT_INTERCEPT

struct save_area
{
  unsigned protected_offset;
  unsigned protected_segment;
  unsigned real_handler;
};

struct save_record
{
  unsigned iv;
  struct save_area * area;
  struct save_record * next;
};

static struct save_record
  * X32_save_areas = ((struct save_record *) NULL);

static int
DEFUN (X32_do_restore, (iv, area),
       unsigned iv
       AND struct save_area * area)
{
  struct SREGS sregs;
  union REGS regs;
  
  segread (&sregs);
  regs.x.ax = 0x2507;
  regs.h.cl = iv;
  regs.e.edx = area->protected_offset;
  regs.e.ebx = area->real_handler;
  sregs.ds = area->protected_segment;
  int86x (0x21, &regs, &regs, &sregs);
  return (((regs.e.flags & 0x1) == 0) ? 0 : -1);
}

int 
DEFUN (X32_interrupt_restore, (iv), unsigned iv)
{
  struct save_record ** loc, * this;
  
  loc = &X32_save_areas;
  this = (*loc);
  while (this != ((struct save_record *) NULL))
  {
    if (this->iv == iv)
    {
      if ((X32_do_restore (iv, this->area)) != 0)
	return (-1);
      *loc = (this->next);
      free (this);
      return (0);
    }
    loc = &this->next;
    this = (*loc);
  }
  return (-1);
}

int
DEFUN (X32_remember_interrupt, (iv, area),
       unsigned iv
       AND struct save_area * area)
{
  struct save_record * this;

  this = ((struct save_record *) (malloc (sizeof (struct save_record))));
  if (this == ((struct save_record *) NULL))
    return (-1);
  this->iv = iv;
  this->area = area;
  this->next = X32_save_areas;
  X32_save_areas = this;
  return (0);
}

int 
DEFUN (X32_int_intercept, (iv, handler, ptr),
       unsigned iv
       AND void EXFUN ((* handler), (void))
       AND PTR ptr)
{
  struct SREGS sregs;
  union REGS regs;
  struct save_area * area = ((struct save_area *) ptr);

  /* Preserve previous real mode interrupt handler */

  {
    regs.x.ax = 0x2503;
    regs.h.cl = iv;
    int86 (0x21, &regs, &regs);
    if ((regs.e.flags & 1) != 0)
      return (-1);
    area->real_handler = regs.e.ebx;
  }

  /* Preserve previous protected mode interrupt handler. */
  
  {
    regs.x.ax = 0x2502;
    regs.h.cl = iv;
    segread (&sregs);
    int86x (0x21, &regs, &regs, &sregs);
    if ((regs.e.flags & 1) != 0)
      return (-1);

    area->protected_segment = sregs.es;
    area->protected_offset = regs.e.ebx;
  }
  
  /* Set real and protected mode handler. */

  {
    segread (&sregs);
    regs.x.ax = 0x2506;
    regs.h.cl = iv;
    regs.e.edx = ((unsigned) handler);
    sregs.ds = (getCS ());
    int86x (0x21, &regs, &regs, &sregs);
    if ((regs.e.flags & 1) != 0)
      return (-1);
  }
  if ((X32_remember_interrupt (iv, area)) != 0)
  {
    (void) X32_do_restore (iv, area);
    return (-1);
  }
  return (0);
}

#endif /* USE_LOCKED_INT_INTERCEPT */
