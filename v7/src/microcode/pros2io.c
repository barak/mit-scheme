/* -*-C-*-

$Id: pros2io.c,v 1.4 1995/04/28 06:59:29 cph Exp $

Copyright (c) 1994-95 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "prims.h"
#include "os2.h"

extern int OS2_process_any_status_change (void);
extern qid_t OS2_channel_thread_descriptor (Tchannel);

DEFINE_PRIMITIVE ("OS2-SELECT-REGISTRY-LUB", Prim_OS2_select_registry_lub, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (QID_MAX + 1));
}

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    Tchannel channel = (arg_channel (1));
    if (! ((CHANNEL_ABSTRACT_P (channel)) && (CHANNEL_INPUTP (channel))))
      error_bad_range_arg (1);
    PRIMITIVE_RETURN
      (LONG_TO_UNSIGNED_FIXNUM (OS2_channel_thread_descriptor (channel)));
  }
}

static qid_t
arg_qid (int arg_number)
{
  unsigned int qid = (arg_index_integer (arg_number, (QID_MAX + 1)));
  if (!OS2_qid_openp (qid))
    error_bad_range_arg (arg_number);
  return (qid);
}

DEFINE_PRIMITIVE ("OS2-SELECT-DESCRIPTOR", Prim_OS2_select_descriptor, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  switch (OS2_message_availablep ((arg_qid (1)), (BOOLEAN_ARG (2))))
    {
    case mat_available:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
    case mat_not_available:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
    case mat_interrupt:
      if (OS2_process_any_status_change ())
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (3));
      else
	PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (2));
    default:
      error_external_return ();
      PRIMITIVE_RETURN (UNSPECIFIC);
    }
}

DEFINE_PRIMITIVE ("OS2-SELECT-REGISTRY-TEST", Prim_OS2_select_registry_test, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (1, STRING_P);
  if ((STRING_LENGTH (ARG_REF (1))) != (QID_MAX + 1))
    error_bad_range_arg (1);
  CHECK_ARG (2, STRING_P);
  if ((STRING_LENGTH (ARG_REF (1))) != (QID_MAX + 1))
    error_bad_range_arg (2);
  {
    char * registry = (STRING_LOC ((ARG_REF (1)), 0));
    char * results = (STRING_LOC ((ARG_REF (2)), 0));
    int blockp = (BOOLEAN_ARG (3));
    int inputp = 0;
    int interruptp = 0;
    qid_t qid;
    int n;

    /* This first phase checks the qid subqueues and OS2_scheme_tqueue
       for any previously-queued input.  */
    for (qid = 0; (qid <= QID_MAX); qid += 1)
      {
	(results [qid]) = 0;
	if ((registry [qid]) != 0)
	  switch (OS2_message_availablep (qid, 0))
	    {
	    case mat_available:
	      inputp = 1;
	      (results [qid]) = 1;
	      break;
	    case mat_interrupt:
	      interruptp = 1;
	      break;
	    }
      }
    /* This second phase waits for input if necessary.  It does not
       check the subqueues for previously-stored data, so it's
       important that we already did this.  Otherwise we could end up
       waiting for input when there was valid input ready.  */
    if (blockp)
      while (! (inputp || interruptp))
	{
	  for (qid = 0; (qid <= QID_MAX); qid += 1)
	    (OS2_scheme_tqueue_avail_map [qid]) = 0;
	  n = (OS2_tqueue_select (OS2_scheme_tqueue, blockp));
	  if (n == (-1))
	    break;
	  else if (n < 0)
	    interruptp = 1;
	  else
	    for (qid = 0; (qid <= QID_MAX); qid += 1)
	      if (((registry [qid]) != 0)
		  && (OS2_scheme_tqueue_avail_map [qid]))
		{
		  inputp = 1;
		  (results [qid]) = 1;
		}
	}
    if (inputp)
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
    else if (!interruptp)
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
    else if (!OS2_process_any_status_change ())
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (2));
    else
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (3));
  }
}
