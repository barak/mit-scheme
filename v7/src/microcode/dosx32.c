/* -*-C-*-

$Id: dosx32.c,v 1.5 1992/10/12 20:00:46 jinx Exp $

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
#include "scheme.h"
#include "msdos.h"
#include "dosio.h"
#include "dossys.h"

#ifdef getDS
#undef getDS
#endif

#ifdef getCS
#undef getCS
#endif

extern unsigned short EXFUN (getCS, (void));
extern unsigned short EXFUN (getDS, (void));

/* Exports */

extern int EXFUN (under_X32_p, (void));
extern int EXFUN (X32_lock_scheme_microcode, (void));
extern int EXFUN (X32_interrupt_restore, (unsigned));
extern int EXFUN (X32_int_intercept, (unsigned, void (*) (void), PTR));

int
DEFUN_VOID (under_X32_p)
{
  union REGS regs;
  
  regs.x.bx = (getDS ());
  regs.x.ax = 0x3504;
  intdos (&regs, &regs);
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
  intdosx (&regs, &regs, &sregs);
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
  void EXFUN ((* handler), (void));
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
  intdosx (&regs, &regs, &sregs);
  return (((regs.e.flags & 0x1) == 0) ? 0 : -1);
}

static int
DEFUN (X32_do_install, (iv, handler),
       unsigned iv
       AND void EXFUN ((* handler), (void)))
{
  struct SREGS sregs;
  union REGS regs;

  /* Set real and protected mode handler. */

  segread (&sregs);
  regs.x.ax = 0x2506;
  regs.h.cl = iv;
  regs.e.edx = ((unsigned) handler);
  sregs.ds = (getCS ());
  intdosx (&regs, &regs, &sregs);
  if ((regs.e.flags & 1) != 0)
    return (-1);
  return (0);
}

static struct save_record **
DEFUN (X32_find_interrupt, (iv), unsigned iv)
{
  struct save_record ** loc, * this;
  
  loc = &X32_save_areas;
  this = (* loc);
  while (this != ((struct save_record *) NULL))
  {
    if (this->iv == iv)
      break;
    loc = &this->next;
    this = (*loc);
  }
  return (loc);
}

static int
DEFUN (X32_remember_interrupt, (iv, area, handler),
       unsigned iv
       AND struct save_area * area
       AND void EXFUN ((* handler), (void)))
{
  struct save_record * this;

  this = ((struct save_record *) (malloc (sizeof (struct save_record))));
  if (this == ((struct save_record *) NULL))
    return (-1);
  this->iv = iv;
  this->area = area;
  this->next = X32_save_areas;
  this->handler = handler;
  X32_save_areas = this;
  return (0);
}

int
DEFUN (X32_interrupt_restore, (iv), unsigned iv)
{
  struct save_record ** loc, * this;
  
  loc = (X32_find_interrupt (iv));
  this = (* loc);
  if ((this == ((struct save_record *) NULL))
      || ((X32_do_restore (iv, this->area)) != 0))
    return (-1);
    
  * loc = (this->next);
  free (this);
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
    intdos (&regs, &regs);
    if ((regs.e.flags & 1) != 0)
      return (-1);
    area->real_handler = regs.e.ebx;
  }

  /* Preserve previous protected mode interrupt handler. */
  
  {
    regs.x.ax = 0x2502;
    regs.h.cl = iv;
    segread (&sregs);
    intdosx (&regs, &regs, &sregs);
    if ((regs.e.flags & 1) != 0)
      return (-1);

    area->protected_segment = sregs.es;
    area->protected_offset = regs.e.ebx;
  }
  
  if ((X32_do_install (iv, handler)) != 0)
    return (-1);

  if ((X32_remember_interrupt (iv, area, handler)) != 0)
  {
    (void) X32_do_restore (iv, area);
    return (-1);
  }
  return (0);
}

extern int EXFUN (X32_subprocess, (const char *, int, int, int));
extern int EXFUN (X32_system, (const char *));
extern int EXFUN (system, (const char *));

static int
DEFUN (dummy_system, (command), const char * command)
{
  return (-1);
}

static int
DEFUN (X32_DPMI_system, (command), const char * command)
{
  /* Plain system does not work in X32 under DPMI
     in the presence of our timer interrupt handler.
     Disable the timer interrupt around the call to system.
   */
  static struct save_record ** ptimer_record = ((struct save_record **) NULL);
  struct save_record * timer_record;
  int result;
  
  if (ptimer_record == ((struct save_record **) NULL))
  {
    ptimer_record = (X32_find_interrupt (DOS_INTVECT_USER_TIMER_TICK));
    if (ptimer_record == ((struct save_record **) NULL))
      return (-1);
  }
  
  timer_record = * ptimer_record;
  if ((X32_do_restore (DOS_INTVECT_USER_TIMER_TICK, timer_record->area))
      != 0)
    return (-1);

  result = (system (command));
  
  if ((X32_do_install (DOS_INTVECT_USER_TIMER_TICK, timer_record->handler))
      != 0)
  {
    /* We are massively scrod. */
    * ptimer_record = timer_record->next;
    return (-2);
  }
  /* Request a low-level timer interrupt, in case we missed it because
     we disabled it.
   */
  REQUEST_INTERRUPT (INT_Global_GC);
  return (result);
}

int EXFUN (which_system, (const char *));

static int EXFUN ((* fsystem), (const char *)) = which_system;

static int
DEFUN (which_system, (command), const char * command)
{
  if (! (under_X32_p ()))
    fsystem = dummy_system;
  else if (! (under_DPMI_p ()))
    fsystem = system;
  else
    fsystem = X32_DPMI_system;
  return ((* fsystem) (command));
}

int
DEFUN (X32_system, (command), const char * command)
{
  return (((* fsystem) (command)));
}

/* values for io specs:
   -1   => default (console)
   >= 0 => channel number.
 */

static int
DEFUN (swap_io_handle, (handle, spec),
       int handle AND int spec)
{
  if ((spec == -1) || ((CHANNEL_DESCRIPTOR (spec)) == handle))
    return (-1);
  else
  {
    int saved = (dup (handle));
    if (saved == -1)
      return (-3);
    if ((dup2 ((CHANNEL_DESCRIPTOR (spec)), handle)) != 0)
    {
      close (saved);
      return (-4);
    }
    return (saved);
  }
}

static int
DEFUN (restore_io_handle, (handle, saved_handle),
       int handle AND int saved_handle)
{
  if (saved_handle < 0)
    return (0);
  else if ((dup2 (saved_handle, handle)) != 0)
    return (-1);
  close (saved_handle);
  return (0);
}

#define SWAPPING_HANDLE(h, spec, code) do				\
{									\
  int saved_handle = (swap_io_handle ((h), (spec)));			\
  if (saved_handle < -1)						\
    result = saved_handle;						\
  else									\
  {									\
    code;								\
    restore_io_handle ((h), saved_handle);				\
  }									\
} while (0)

int
DEFUN (X32_subprocess, (command, in_spec, out_spec, err_spec),
       const char * command
       AND int in_spec
       AND int out_spec
       AND int err_spec)
{
  int result;
  
  SWAPPING_HANDLE (0, in_spec,
		   SWAPPING_HANDLE (1, out_spec,
				    SWAPPING_HANDLE (2, err_spec,
						     (result = ((* fsystem)
								(command))))));
  return (result);
}
