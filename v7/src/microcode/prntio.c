/* -*-C-*-

$Id: prntio.c,v 1.3 1996/10/02 18:58:40 cph Exp $

Copyright (c) 1993-96 Massachusetts Institute of Technology

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
#include "syscall.h"

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (long_to_integer ((long) (CHANNEL_HANDLE (arg_channel (1)))));
}

static HANDLE *
DEFUN (to_win_hand_vec, (nhand, schhands),
       int nhand AND SCHEME_OBJECT * schhands)
{
  int ctr;
  HANDLE * winhands;

  if (nhand == 0)
    return ((HANDLE *) NULL);
  winhands = ((HANDLE *) (malloc (nhand * (sizeof (HANDLE)))));
  if (winhands == ((HANDLE *) NULL))
    error_system_call ((GetLastError ()), syscall_malloc);
  for (ctr = 0; ctr < nhand; ctr++)
    winhands[ctr] = ((HANDLE) (integer_to_long (schhands[ctr])));
  return (winhands);
}

static SCHEME_OBJECT
DEFUN (wait_result, (result, limit_object, limit_abandoned),
       DWORD result AND int limit_object AND int limit_abandoned)
{
  if (result == WAIT_TIMEOUT)
    return (FIXNUM_ZERO);
  else if ((result >= WAIT_OBJECT_0)
	   && (result <= (WAIT_OBJECT_0 + limit_object)))
    return (long_to_integer ((result + 1) - WAIT_OBJECT_0));
  else if ((result >= WAIT_ABANDONED_0)
	   && (result <= (WAIT_ABANDONED_0 + limit_abandoned)))
    return (long_to_integer (- ((long) ((result + 1) - WAIT_ABANDONED_0))));
  else
    error_system_call ((GetLastError ()), syscall_select);
}

DEFINE_PRIMITIVE ("NT:MSGWAITFORMULTIPLEOBJECTS", Prim_nt_msgwaitformultipleobjects, 4, 4, 0)
{
  extern HANDLE master_tty_window;
  PRIMITIVE_HEADER (4);
  {
    SCHEME_OBJECT schhands = (VECTOR_ARG (1));
    BOOL wait_for_all = (BOOLEAN_ARG (2));
    int timeout = (arg_nonnegative_integer (3));
    int mask = (arg_nonnegative_integer (4));
    int nhand = (VECTOR_LENGTH (schhands));
    HANDLE * handles;
    DWORD result;

    if (Screen_PeekEvent (master_tty_window, ((SCREEN_EVENT *) NULL)))
      return (long_to_integer (1 + nhand));

    handles = (to_win_hand_vec (nhand, (VECTOR_LOC (schhands, 0))));
    result = (MsgWaitForMultipleObjects (nhand, handles, wait_for_all,
					 timeout, mask));

    if (handles != ((HANDLE *) NULL))
      free (handles);
    PRIMITIVE_RETURN (wait_result (result, nhand, (nhand - 1)));
  }
}

DEFINE_PRIMITIVE ("NT:WAITFORMULTIPLEOBJECTS", Prim_nt_waitformultipleobjects, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    SCHEME_OBJECT schhands = (VECTOR_ARG (1));
    BOOL wait_for_all = (BOOLEAN_ARG (2));
    int timeout = (arg_nonnegative_integer (3));
    int nhand = (VECTOR_LENGTH (schhands));
    HANDLE * handles = (to_win_hand_vec (nhand, (VECTOR_LOC (schhands, 0))));
    DWORD result
      = (WaitForMultipleObjects (nhand, handles, wait_for_all, timeout));
    if (handles != ((HANDLE *) NULL))
      free (handles);
    PRIMITIVE_RETURN (wait_result (result, (nhand - 1), (nhand - 1)));
  }
}
