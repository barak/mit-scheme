/* -*-C-*-

$Id: pruxdld.c,v 1.1 1993/08/28 05:41:26 gjr Exp $

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

/* This file contains the interface to the HP-UX (SunOS-style)
   dynamic loader.
   It has only been tried under HP-UX.
 */

#include <dl.h>
#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "syscall.h"

#ifndef DYNAMIC_PATH
# define DYNAMIC_PATH 0
#endif

static short shl_findsym_types [] =
{
  TYPE_PROCEDURE,
  TYPE_DATA,
  TYPE_UNDEFINED
};

DEFINE_PRIMITIVE ("LOAD-OBJECT-FILE", Prim_load_object_file, 1, 1,
		  "(load-object-file lib-file)")
{
  extern int errno;
  shl_t prim_lib_handle;
  PRIMITIVE_HEADER (1);

  prim_lib_handle = (shl_load ((STRING_ARG (1)),
			       (BIND_IMMEDIATE | BIND_NONFATAL | DYNAMIC_PATH),
			       0));
  if (prim_lib_handle == NULL)
    error_system_call (errno, syscall_dld);
  PRIMITIVE_RETURN (long_to_integer ((long) prim_lib_handle));
}

DEFINE_PRIMITIVE ("OBJECT-LOOKUP-SYMBOL", Prim_object_lookup_symbol, 3, 3,
		  "(object-lookup-symbol handle sym type)")
{
  char * sym;
  short type;
  unsigned long result;
  shl_t prim_lib_handle, * arg_handle;
  PRIMITIVE_HEADER (3);

  switch (ARG_REF (1))
  {
    case SHARP_F:
      prim_lib_handle = PROG_HANDLE;
      arg_handle = & prim_lib_handle;
      break;

    case SHARP_T:
      arg_handle = ((shl_t *) NULL);
      break;

    default:
      prim_lib_handle = ((shl_t) (arg_integer (1)));
      arg_handle = & prim_lib_handle;
      break;
  }

  sym = (STRING_ARG (2));
  type = shl_findsym_types [arg_index_integer (3, ((sizeof (shl_findsym_types))
						   / (sizeof (short))))];

  if ((shl_findsym (arg_handle, sym, type, ((void *) & result)))
      == -1)
    PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (long_to_integer (result));
}

DEFINE_PRIMITIVE ("INVOKE-C-THUNK", Prim_invoke_C_thunk, 1, 1,
		  "(invoke-C-thunk address)")
{
  long address;
  void EXFUN ((* thunk), (void));
  PRIMITIVE_HEADER (1);
  
  address = (arg_integer (1));
  thunk = ((void (*) ()) address);
  (* thunk) ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}
