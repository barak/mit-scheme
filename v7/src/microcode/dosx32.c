/* -*-C-*-

$Id: dosx32.c,v 1.2 1992/09/03 07:30:13 jinx Exp $

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

extern int EXFUN (X32_interrupt_restore, (unsigned));

extern int EXFUN (X32_int_intercept, (unsigned, void (*) (void), PTR));

extern unsigned short EXFUN (getCS, (void));

extern unsigned short EXFUN (getDS, (void));

int
DEFUN_VOID (under_X32_p)
{
  union REGS regs;
  
  regs.x.bx = (getDS ());
  regs.x.ax = 0x3504;
  int86 (0x21, &regs, &regs);
  return ((regs.e.flags & 1) == 0);
}

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

static int
DEFUN (lock_region, (segment, offset, size),
       unsigned short segment
       AND PTR offset
       AND unsigned long size)
{
  return (lock_unlock (OPERATION_LOCK, segment, offset, size));
}

static int
DEFUN (unlock_region, (segment, offset, size),
       unsigned short segment
       AND PTR offset
       AND unsigned long size)
{
  return (lock_unlock (OPERATION_UNLOCK, segment, offset, size));
}

#define ES	0
#define CS	1
#define SS	2
#define DS	3

struct wired_area_s
{
  int seg;
  PTR start;
  PTR end;
};
  
extern unsigned X32_locked_data_start;
extern unsigned X32_locked_data_end;
extern void EXFUN (X32_locked_code_start, (void));
extern void EXFUN (X32_locked_code_end, (void));
extern unsigned char scan_code_tables_start[];
extern unsigned char scan_code_tables_end[];

static struct wired_area_s wired_areas[] =
{
  {
    DS,
    ((PTR) &X32_locked_data_start),
    ((PTR) &X32_locked_data_end)
  },
  {
    CS,
    ((PTR) X32_locked_code_start),
    ((PTR) X32_locked_code_end)
  },
  {
    DS,
    ((PTR) &scan_code_tables_start[0]),
    ((PTR) &scan_code_tables_end[0])
  }
};

#define N_WIRED_AREAS ((sizeof (wired_areas)) / (sizeof (struct wired_area_s)))

int
DEFUN_VOID (X32_lock_scheme_microcode)
{
  int i, j;
  unsigned short cs, ds, sel;

  ds = (getDS ());
  cs = (getCS ());

  for (i = 0; i < N_WIRED_AREAS; i++)
  {
    sel = ((wired_areas[i].seg == CS) ? cs : ds);
    if ((lock_region (sel,
		      wired_areas[i].start,
		      (((unsigned long) wired_areas[i].end)
		       - ((unsigned long) wired_areas[i].start))))
	!= 0)
    {
      while (--i >= 0)
      {
	sel = ((wired_areas[i].seg == CS) ? cs : ds);
	(void) unlock_region (sel,
			      wired_areas[i].start,
			      (((unsigned long) wired_areas[i].end)
			       - ((unsigned long) wired_areas[i].start)));
      }
      return (-1);
    }
  }
  return (0);
}

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
