/* -*-C-*-

$Id: scheme16.c,v 1.14 2003/02/14 18:48:12 cph Exp $

Copyright 1993-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* MIT/GNU Scheme under Windows system utiltities DLL source.
   Win16 side of the Win32s version.
 */

#define _WINDLL
#define W32SUT_16
#include "ntscmlib.h"
#include <dos.h>

#ifdef DEBUG
#include <windows.h>
int
TellUser (char FAR * format, ...)
{
  va_list arg_ptr;
  char buffer[1024];
  
  va_start (arg_ptr, format);
  wvsprintf (&buffer[0], format, arg_ptr);
  va_end (arg_ptr);
  return (MessageBox (NULL,
		      ((LPCSTR) &buffer[0]),
		      ((LPCSTR) "MIT/GNU Scheme Win16 Notification"),
		      (MB_TASKMODAL | MB_ICONINFORMATION | MB_OK)));
}

#define DEBUGGING(what) what
#else
#define DEBUGGING(what) do { } while (0)
#endif /* DEBUG */

struct seg_desc_s
{
  unsigned long low;
  unsigned long high;
};

static BOOL
DPMI_get_descriptor (UINT selector, struct seg_desc_s far * desc)
{
  UINT saved_es;

  _asm
  {
	_emit	066h
	push	di
	_emit	066h
	push	bx
	_emit	066h
	xor	di,di
	mov	ax,es
	mov	word ptr [bp-2],ax
	les	di, dword ptr 6[bp]
	mov	bx, word ptr 4[bp]
	mov	ax, 000bh
	int	31h
	jc	fail
	mov	ax, word ptr [bp-2]
	mov	es,ax
	_emit	066h
	pop	bx
	_emit	066h
	pop	di
	mov	ax,0
	leave
	ret
  fail:
	mov	ax, word ptr [bp-2]
	mov	es,ax
	_emit	066h
	pop	bx
	_emit	066h
	pop	di
	mov	ax,1
	leave
	ret
  }
}

static BOOL
DPMI_set_descriptor (UINT selector, struct seg_desc_s far * desc)
{
  UINT saved_es;

  _asm
  {
	_emit	066h
	push	di
	_emit	066h
	push	bx
	_emit	066h
	xor	di,di
	mov	ax,es
	mov	word ptr [bp-2],ax
	les	di, dword ptr 6[bp]
	mov	bx, word ptr 4[bp]
	mov	ax, 000ch
	int	31h
	jc	fail
	mov	ax, word ptr [bp-2]
	mov	es,ax
	_emit	066h
	pop	bx
	_emit	066h
	pop	di
	mov	ax,0
	leave
	ret
  fail:
	mov	ax, word ptr [bp-2]
	mov	es,ax
	_emit	066h
	pop	bx
	_emit	066h
	pop	di
	mov	ax,1
	leave
	ret
  }
}

static DWORD
win16_alloc_scheme_selectors (struct ntw32lib_selalloc_s FAR * buf)
{
  UINT cs_sel, ds_sel;
  struct seg_desc_s desc;
  unsigned long nbase, nlimit;
  
  ds_sel = (AllocSelector (0));
  if (ds_sel == 0)
    return (0L);
  nbase = (GetSelectorBase (buf->ds32));

  nbase = (nbase + buf->base);
  (void) DPMI_get_descriptor (buf->ds32, & desc);

  desc.low &= 0xffffUL;
  desc.low |= (nbase << 16);
  desc.high &= 0x00ffff00UL;
  desc.high |= (nbase & 0xff000000UL);
  desc.high |= ((nbase >> 16) & 0xff);
  (void) DPMI_set_descriptor (ds_sel, & desc);

  cs_sel = (AllocDStoCSAlias (ds_sel));
  if (cs_sel == 0)
  {
#if 0
    FreeSelector (ds_sel);
#endif
    return (0L);
  }
  buf->cs = cs_sel;
  buf->ds = ds_sel;
  buf->ss = ds_sel;

  nbase = (GetSelectorBase (cs_sel));
  nlimit = (GetSelectorLimit (cs_sel));

  if ((nbase != 0) && (nlimit != 0))
    return (1L);
  else
  {
#if 0
    FreeSelector (cs_sel);
    FreeSelector (ds_sel);      
#endif
    return (0L);
  }
}

static DWORD
win16_release_scheme_selectors (struct ntw32lib_selfree_s FAR * buf)
{
#if 0
  if ((buf->ds != 0) && (buf->ds != buf->ds32))
    FreeSelector (buf->ds);
  if ((buf->cs != 0) && (buf->cs != buf->cs32))
    FreeSelector (buf->cs);
#endif
  return (1L);
}

static BOOL
DPMI_lock_unlock (UINT fun, unsigned long lin, unsigned long nbytes)
{
  _asm
  {
    	push	si
	push	di
	push	bx

	mov	ax, 4[bp]
	mov	cx, 6[bp]
	mov	bx, 8[bp]
	mov	di, 10[bp]
	mov	si, 12[bp]

	int	31h
	jc	fail
	mov	ax,1
	jmp	join

	fail:
	xor	ax,ax
	join:
	pop	bx
	pop	di
	pop	si
	leave
	ret
  }
}

static BOOL
pagelockunlock (unsigned int dpmi_fun, void FAR * low, unsigned long nbytes)
{
  unsigned int seg, off;
  unsigned long base, lin;

  seg = (FP_SEG (low));
  off = (FP_OFF (low));
  base = (GetSelectorBase (seg));
  lin = (base + ((unsigned long) off));

  return (DPMI_lock_unlock (dpmi_fun, lin, nbytes));
}

static BOOL
pagelock (void FAR * low, unsigned long nbytes)
{
  return (pagelockunlock (0x0600, low, nbytes));
}

static BOOL
pageunlock (void FAR * low, unsigned long nbytes)
{
  return (pagelockunlock (0x0601, low, nbytes));
}

static DWORD
win16_lock_area (struct ntw32lib_vlock_s FAR * buf)
{
  return ((DWORD) (pagelock (buf->area, buf->size)));
}

static DWORD
win16_unlock_area (struct ntw32lib_vulock_s FAR * buf)
{
  return ((DWORD) (pageunlock (buf->area, buf->size)));  
}

#ifndef MK_FP
static void FAR * 
MK_FP (unsigned short seg, unsigned short off)
{
  union
  {
    struct
    {
      unsigned short off;
      unsigned short seg;
    } split;
    void FAR * result;
  } views;

  views.split.seg = seg;
  views.split.off = off;
  return (views.result);
}
#endif /* MK_FP */

static WORD htimer = 0;
static unsigned long timer_index = 0;
static WORD (FAR PASCAL * KillSystemTimer) (WORD htimer);

static struct ntw16lib_itimer_s
{
  struct ntw16lib_itimer_s FAR * next;
  unsigned long index;
  unsigned long FAR * base;
  long memtop_off;
  long int_code_off;
  long int_mask_off;
  unsigned long bit_mask;
  long ctr_off;
  UINT catatonia_message;
  UINT interrupt_message;
  HWND window;
  UINT selector;
  HGLOBAL ghan;
} FAR * async_timers = ((struct ntw16lib_itimer_s FAR *) NULL);

#define INTERRUPT_CODE(scm_timer)					\
  ((scm_timer -> base) [scm_timer -> int_code_off])

#define INTERRUPT_MASK(scm_timer)					\
  ((scm_timer -> base) [scm_timer -> int_mask_off])

#define MEMTOP(scm_timer)						\
  ((scm_timer -> base) [scm_timer -> memtop_off])

#define CATATONIA_COUNTER(scm_timer)					\
  ((scm_timer -> base) [scm_timer -> ctr_off])

#define CATATONIA_LIMIT(scm_timer)					\
  ((scm_timer -> base) [(scm_timer -> ctr_off) + 1])

#define CATATONIA_FLAG(scm_timer)					\
  ((scm_timer -> base) [(scm_timer -> ctr_off) + 2])

void FAR _export 
scheme_asynctimer (void)
{
  struct ntw16lib_itimer_s FAR * scm_timer;

  for (scm_timer = async_timers;
       scm_timer != ((struct ntw16lib_itimer_s FAR *) NULL);
       scm_timer = scm_timer->next)
    {
      (INTERRUPT_CODE (scm_timer)) |= (scm_timer -> bit_mask);
      if (((INTERRUPT_CODE (scm_timer)) & (INTERRUPT_MASK (scm_timer)))
	  != 0L)
	{
	  (MEMTOP (scm_timer)) = ((unsigned long) -1L);
	  PostMessage ((scm_timer -> window),
		       (scm_timer -> interrupt_message),
		       ((WPARAM) 0),
		       ((LPARAM) 0));
	}
      (CATATONIA_COUNTER (scm_timer)) += 1L;
      if (((CATATONIA_COUNTER (scm_timer)) > (CATATONIA_LIMIT (scm_timer)))
	  && ((CATATONIA_LIMIT (scm_timer)) != 0L))
	{
	  if ((CATATONIA_FLAG (scm_timer)) == 0L)
	    {
	      (CATATONIA_FLAG (scm_timer)) = 1L;
	      PostMessage ((scm_timer -> window),
			   (scm_timer -> catatonia_message),
			   ((WPARAM) 0),
			   ((LPARAM) 0));
	    }
	  (CATATONIA_COUNTER (scm_timer)) = 0L;
	}
    }
}

static void
scheme_asynctimer_end (void)
{
}

static void
possibly_uninstall_async_handler (void)
{
  if (async_timers != ((struct ntw16lib_itimer_s FAR *) NULL))
    return;
  DEBUGGING (TellUser ("Un-Installing asynctimer."));
  if (htimer != 0)
  {
    KillSystemTimer (htimer);
    htimer = 0;
  }
  pageunlock (&async_timers,
	      (sizeof (struct ntw16lib_itimer_s FAR *)));
  pageunlock (((void FAR *) scheme_asynctimer),
	      ((unsigned long) scheme_asynctimer_end)
	      - ((unsigned long) scheme_asynctimer));
  return;
}

static DWORD
win16_flush_timer (struct ntw32lib_ftimer_s FAR * buf)
{
  unsigned long index = buf->handle;
  struct ntw16lib_itimer_s FAR * FAR * ptr = & async_timers;

  while ((* ptr) != ((struct ntw16lib_itimer_s FAR *) NULL))
  {
    if (((* ptr) -> index) == index)
    {
      struct ntw16lib_itimer_s FAR * current = (* ptr);

      (* ptr) = current->next;
      if (index == (timer_index - 1))
	timer_index = index;
      FreeSelector (current->selector);
      GlobalPageUnlock (current->ghan);
      GlobalUnlock (current->ghan);
      GlobalFree (current->ghan);
      possibly_uninstall_async_handler ();
      return (1L);
    }
    ptr = & ((* ptr) -> next);
  }
  return (0L);
}

static DWORD
do_install_async_handler (void)
{
  WORD (FAR PASCAL * CreateSystemTimer) (WORD rate, FARPROC callback);
  HINSTANCE hsystem;

  DEBUGGING (TellUser ("Installing asynctimer."));
  if (! (pagelock (((void FAR *) scheme_asynctimer),
		   ((unsigned long) scheme_asynctimer_end)
		   - ((unsigned long) scheme_asynctimer))))
    return (WIN32_ASYNC_TIMER_NOLOCK);
  else if (! (pagelock (&async_timers,
			(sizeof (struct ntw16lib_itimer_s FAR *)))))
  {
    pageunlock (((void FAR *) scheme_asynctimer),
		((unsigned long) scheme_asynctimer_end)
		- ((unsigned long) scheme_asynctimer));
    return (WIN32_ASYNC_TIMER_NOLOCK);
  }

  hsystem = (GetModuleHandle ("SYSTEM"));
  CreateSystemTimer = (GetProcAddress (hsystem, "CREATESYSTEMTIMER"));
  KillSystemTimer = (GetProcAddress (hsystem, "KILLSYSTEMTIMER"));

  if ((CreateSystemTimer == ((WORD (FAR PASCAL *) (WORD, FARPROC)) NULL))
      || (KillSystemTimer == ((WORD (FAR PASCAL *) (WORD)) NULL)))
  {
    possibly_uninstall_async_handler ();
    return (WIN32_ASYNC_TIMER_NONE);
  }

  htimer = (CreateSystemTimer (55, ((FARPROC) scheme_asynctimer)));
  if (htimer == 0)
  {
    possibly_uninstall_async_handler ();
    return (WIN32_ASYNC_TIMER_EXHAUSTED);
  }
  return (WIN32_ASYNC_TIMER_OK);
}

static DWORD
win16_install_timer (struct ntw32lib_itimer_s FAR * buf)
{
  struct ntw16lib_itimer_s FAR * scm_timer;
  DWORD result;
  HGLOBAL ghan;

  if (htimer == 0)
  {
    result = (do_install_async_handler ());
    if (result != WIN32_ASYNC_TIMER_OK)
      return (result);
  }

  ghan = (GlobalAlloc (GMEM_FIXED, (sizeof (struct ntw16lib_itimer_s))));
  if (ghan == ((HGLOBAL) NULL))
  {
    possibly_uninstall_async_handler ();
    return (WIN32_ASYNC_TIMER_NOMEM);
  }
  scm_timer = ((struct ntw16lib_itimer_s FAR *) (GlobalLock (ghan)));
  if (scm_timer == ((struct ntw16lib_itimer_s FAR *) NULL))
  {
    GlobalFree (ghan);
    possibly_uninstall_async_handler ();
    return (WIN32_ASYNC_TIMER_NOLOCK);
  }
  if ((GlobalPageLock (ghan)) == 0)
  {
    GlobalUnlock (ghan);
    GlobalFree (ghan);
    possibly_uninstall_async_handler ();
    return (WIN32_ASYNC_TIMER_NOLOCK);
  }

  scm_timer->selector = (AllocSelector (FP_SEG (buf->base)));
  if (scm_timer->selector == 0)
  {
    GlobalPageUnlock (ghan);
    GlobalUnlock (ghan);
    GlobalFree (ghan);
    possibly_uninstall_async_handler ();
    return (WIN32_ASYNC_TIMER_NOLDT);
  }

  scm_timer->next = async_timers;
  scm_timer->index = timer_index++;
  scm_timer->base = (MK_FP (scm_timer->selector, (FP_OFF (buf->base))));
  scm_timer->memtop_off = buf->memtop_off;
  scm_timer->int_code_off = buf->int_code_off;
  scm_timer->int_mask_off = buf->int_mask_off;
  scm_timer->bit_mask = buf->bit_mask;
  scm_timer->ctr_off = buf->ctr_off;
  scm_timer->catatonia_message = ((UINT) buf->catatonia_message);
  scm_timer->interrupt_message = ((UINT) buf->interrupt_message);
  scm_timer->window = ((HWND) buf->window);
  scm_timer->ghan = ghan;

  buf->handle = scm_timer->index;
  async_timers = scm_timer;

  return (WIN32_ASYNC_TIMER_OK);
}

/* The 32-bit call-back thunk is not really needed right now, but ... */

static UT16CBPROC call_32_bit_code = NULL;

DWORD FAR PASCAL
ntw16lib_init (UT16CBPROC call_back, LPVOID buff)
{
  call_32_bit_code = call_back;
  return (1L);
}

DWORD FAR PASCAL
ntw16lib_handler (LPVOID buf, DWORD func)
{
  switch (func)
  {
    case NTW32LIB_VIRTUAL_LOCK:
      return (win16_lock_area (buf));

    case NTW32LIB_VIRTUAL_UNLOCK:
      return (win16_unlock_area (buf));

    case NTW32LIB_INSTALL_TIMER:
      return (win16_install_timer (buf));

    case NTW32LIB_FLUSH_TIMER:
      return (win16_flush_timer (buf));

    case NTW32LIB_ALLOC_SELECTORS:
      return (win16_alloc_scheme_selectors (buf));

    case NTW32LIB_FREE_SELECTORS:
      return (win16_release_scheme_selectors (buf));

    default:
      return (0L);
  }
}
