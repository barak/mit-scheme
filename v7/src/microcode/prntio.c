/* -*-C-*-

$Id: prntio.c,v 1.6 1997/05/17 07:00:23 cph Exp $

Copyright (c) 1993-97 Massachusetts Institute of Technology

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

/* Primitives to do the NT equivalent of Unix select. */

#include <windows.h>
#include "scheme.h"
#include "prims.h"
#include "ntio.h"
#include "nt.h"
#include "ntscreen.h"
#include "ntgui.h"
#include "syscall.h"

extern HANDLE master_tty_window;

static HANDLE * to_win_hand_vec (int nhand, SCHEME_OBJECT *);
static long wait_for_multiple_objects (DWORD, HANDLE *, DWORD, BOOL);

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer ((long) (CHANNEL_HANDLE (arg_channel (1)))));
}

DEFINE_PRIMITIVE ("NT:MSGWAITFORMULTIPLEOBJECTS", Prim_nt_msgwaitformultipleobjects, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  {
    SCHEME_OBJECT schhands = (VECTOR_ARG (1));
    BOOL wait_for_all = (BOOLEAN_ARG (2));
    DWORD timeout = (arg_ulong_integer (3));
    DWORD mask = (arg_ulong_integer (4));
    DWORD nhand = (VECTOR_LENGTH (schhands));
    HANDLE * handles;
    DWORD result;

    if (wait_for_all != FALSE)
      error_bad_range_arg (2);
    if (mask != QS_ALLINPUT)
      error_bad_range_arg (4);
    if (Screen_PeekEvent (master_tty_window, 0))
      PRIMITIVE_RETURN (long_to_integer (nhand + 1));
    handles = (to_win_hand_vec (nhand, (VECTOR_LOC (schhands, 0))));
    result = (wait_for_multiple_objects (nhand, handles, timeout, TRUE));
    if (handles != 0)
      free (handles);
    PRIMITIVE_RETURN (long_to_integer (result));
  }
}

DEFINE_PRIMITIVE ("NT:WAITFORMULTIPLEOBJECTS", Prim_nt_waitformultipleobjects, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT schhands = (VECTOR_ARG (1));
    BOOL wait_for_all = (BOOLEAN_ARG (2));
    DWORD timeout = (arg_ulong_integer (3));
    DWORD nhand = (VECTOR_LENGTH (schhands));
    HANDLE * handles;
    DWORD result;

    if (wait_for_all != FALSE)
      error_bad_range_arg (2);
    handles = (to_win_hand_vec (nhand, (VECTOR_LOC (schhands, 0))));
    do
      result = (wait_for_multiple_objects (nhand, handles, timeout, FALSE));
    while ((result == (nhand + 1))
	   && ((timeout == 0) || (timeout == INFINITE)));
    if (handles != 0)
      free (handles);
    PRIMITIVE_RETURN (long_to_integer (result));
  }
}

static HANDLE *
to_win_hand_vec (int nhand, SCHEME_OBJECT * schhands)
{
  int ctr;
  HANDLE * winhands;

  if (nhand == 0)
    return (0);
  winhands = (OS_malloc (nhand * (sizeof (HANDLE))));
  for (ctr = 0; ctr < nhand; ctr++)
    winhands[ctr] = ((HANDLE) (integer_to_long (schhands[ctr])));
  return (winhands);
}

static long
wait_for_multiple_objects (DWORD nhand, HANDLE * handles, DWORD timeout,
			   BOOL msgp)
{
  DWORD result;
  MSG m;
#ifdef TRACE_SCREEN_MSGS
  fprintf (trace_file, "MsgWaitForMultipleObjects: timeout=0x%x\n", timeout);
  fflush (trace_file);
#endif
  if (msgp)
    {
      if (Screen_pending_events_p ())
	return (nhand + 1);
      /* This is a kludge.  MsgWaitForMultipleObjects has a race
	 condition -- it ignores messages that are already queued.  So
	 check the queue as late as possible before the call, in order
	 to minimize the window in which we can get stuck waiting for
	 a message that has already arrived.  */
      if (PeekMessage ((&m), 0, 0, 0, PM_NOREMOVE))
	return (((m.message) == WM_SCHEME_INTERRUPT)
		? (nhand + 2)
		: (nhand + 1));
    }
  result =
    (MsgWaitForMultipleObjects (nhand, handles, FALSE, timeout, QS_ALLINPUT));
#ifdef TRACE_SCREEN_MSGS
  fprintf (trace_file, "MsgWaitForMultipleObjects: result=0x%x\n", result);
  fflush (trace_file);
#endif
  return
    ((result == WAIT_TIMEOUT)
     ? 0
     : (result == (WAIT_OBJECT_0 + nhand))
     ? (((!PeekMessage ((&m), 0, 0, 0, PM_NOREMOVE))
	 || ((m.message) == WM_SCHEME_INTERRUPT))
	? (nhand + 2)
	: (nhand + 1))
     : ((WAIT_OBJECT_0 <= result) && (result < (WAIT_OBJECT_0 + nhand)))
     ? ((result - WAIT_OBJECT_0) + 1)
     : ((WAIT_ABANDONED_0 <= result) && (result < (WAIT_ABANDONED_0 + nhand)))
     ? (- ((long) ((result - WAIT_ABANDONED_0) + 1)))
     : ((NT_error_api_call ((GetLastError ()),
			    apicall_MsgWaitForMultipleObjects)),
	0));
}
