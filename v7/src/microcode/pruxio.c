/* -*-C-*-

$Id: pruxio.c,v 1.2 1993/04/06 21:34:02 cph Exp $

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

/* Primitives to perform I/O to and from files. */

#include "scheme.h"
#include "prims.h"
#include "osio.h"
#include "uxselect.h"

#ifndef __hp9000s700
/* Blows up HP 9000/700 compiler (HP-UX 8.05)!  */
extern Tchannel EXFUN (arg_channel, (int arg_number));
extern int EXFUN (UX_channel_descriptor, (Tchannel channel));
#endif

DEFINE_PRIMITIVE ("HAVE-SELECT?", Prim_have_select_p, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (UX_have_select_p));
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-SIZE", Prim_selreg_size, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_select_registry_size ()));
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-LUB", Prim_selreg_lub, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (UX_select_registry_lub ()));
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-CLEAR-ALL", Prim_selreg_clear_all,
		  1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  UX_select_registry_clear_all (STRING_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-SET", Prim_selreg_set,
		  2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  UX_select_registry_set ((STRING_ARG (1)), (arg_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-CLEAR", Prim_selreg_clear,
		  2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  UX_select_registry_clear ((STRING_ARG (1)), (arg_integer (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-IS-SET?", Prim_selreg_is_set_p,
		  2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    ((BOOLEAN_TO_OBJECT
      (UX_select_registry_is_set ((STRING_ARG (1)), (arg_integer (2))))));
}

DEFINE_PRIMITIVE ("SELECT-REGISTRY-TEST", Prim_selreg_test,
		  3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  switch (UX_select_registry_test ((STRING_ARG (1)), (STRING_ARG (2)),
				   (BOOLEAN_ARG (3))))
    {
    case select_input_none:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (0));
    case select_input_argument:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (1));
    case select_input_process_status:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (2));
    case select_input_interrupt:
      PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (3));
    default:
      error_external_return ();
    }
}

DEFINE_PRIMITIVE ("CHANNEL-DESCRIPTOR", Prim_channel_descriptor, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (UX_channel_descriptor (arg_channel (1))));
}
