/* -*-C-*-

$Id: scheme16.c,v 1.1 1993/07/27 20:53:27 gjr Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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

/* MIT Scheme under Windows system utiltities DLL source.
   Win16 side of the Win32s version.
 */

#define W32SUT_16
#include <stdarg.h>
#include <windows.h>
#include <w32sut.h>
#include "ntw32lib.h"

#ifndef STD_MSGBOX_STYLE
#  define STD_MSGBOX_STYLE MB_OK
#endif

static void
TellUser (char * format, unsigned long value)
{
  char buffer[128];

  wsprintf (&buffer[0],
	    ((LPCSTR) format),
	    value);
	    
  MessageBox (NULL,
	      ((LPCSTR) &buffer[0]),
	      ((LPCSTR) "MIT Scheme Win16 Notification"),
	      STD_MSGBOX_STYLE);
  return;
}

static DWORD
win16_allocate_heap (struct ntw32lib_malloc_s FAR * buf)
{
  DWORD linear_address = 0L;
  DWORD handle = 0L;
  UINT lose  = 0;
  UINT code = 0;
    
#if 0
  union _REGS regs;

  regs.x.ax = 0x0501;
  regs.x.bx = (HIWORD (buf->size));
  regs.x.cx = (LOWORD (buf->size));
  (void) _int86 (0x31, &regs, &regs);

  if (regs.x.cflag)
  {
    TellUser ("DPMI failed.", 0L);
    return (0L);
  }
  linear_address = (MAKELONG (regs.x.cx, regs.x.bx));

#elif 0
  TellUser ("Trying to allocate %ld bytes.", buf->size);
  
  _asm	les	bx,DWORD PTR [bp+4]
  _asm	mov	bx,WORD PTR es:[bx+2]
  _asm	mov	cx,WORD PTR es:[bx]
  _asm	mov	ax,0501H
  _asm	int	031h

  _asm	jnc	dpmi_wins
  _asm	mov	WORD PTR [bp-10],1
  _asm	jmp	dpmi_merge

  _asm  dpmi_wins:
  _asm	mov	WORD PTR [bp-4],cx
  _asm	mov	WORD PTR [bp-2],bx
  _asm	dpmi_merge:

#else

  TellUser ("Trying to allocate %ld bytes.", buf->size);
  
  _asm	les	bx,DWORD PTR [bp+4]
  _asm	mov	ecx,DWORD PTR es:[bx]
  _asm	mov	ebx,00200000H
  _asm	mov	edx,1    
  _asm	mov	ax,0504H
  _asm	int	031H

  _asm	jnc	dpmi_wins
  _asm	mov	WORD PTR [bp-10],1
  _asm	mov	WORD PTR [bp-12],ax
  _asm	jmp	dpmi_merge

  _asm  dpmi_wins:
  _asm	mov	DWORD PTR [bp-4],ebx
  _asm	mov	DWORD PTR [bp-8],esi
  _asm	dpmi_merge:

#endif

  if (lose)
  {
    TellUser ("DPMI call failed 0x%x", ((unsigned long) code));
    return (0L);
  }
      
  TellUser ("Linear address = 0x%lx.", linear_address);
  TellUser ("Handle = 0x%lx.", handle);
  buf->area = linear_address;
  buf->handle = handle;
  return (linear_address);
}

static DWORD
win16_release_heap (struct ntw32lib_malloc_s FAR * buf)
{
  TellUser ("Freeing arena with handle 0x%lx", buf->handle);

  _asm	les	bx,DWORD PTR [bp+4]
  _asm	mov	si,WORD PTR es:[bx+6]
  _asm	mov	di,WORD PTR es:[bx+4]
  _asm	mov	ax,0502H
  _asm	int	031H

  return (0L);
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
    case NTW32LIB_MALLOC:
      return (win16_allocate_heap (buf));

    case NTW32LIB_FREE:
      return (win16_release_heap (buf));

    case NTW32LIB_VIRTUAL_LOCK:
      return (1L);

    case NTW32LIB_VIRTUAL_UNLOCK:
      return (1L);

    case NTW32LIB_INSTALL_TIMER:
      return (0L);

    case NTW32LIB_FLUSH_TIMER:
      return (0L);

    default:
      return (0L);
  }
}
