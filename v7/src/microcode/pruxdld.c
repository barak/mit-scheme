/* -*-C-*-

$Id: pruxdld.c,v 1.2 1993/10/27 22:12:16 gjr Exp $

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

/* This file contains the interface to a unix dynamic loader.
   It has been tried under HP-UX, SunOS (4.1.?), and Alpha OSF 1.
 */

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "syscall.h"
#include "os.h"

#if defined(_HPUX)

#include <dl.h>

#ifndef DYNAMIC_PATH
# define DYNAMIC_PATH 0
#endif

typedef shl_t dyn_load_handle_t;

static dyn_load_handle_t
DEFUN (dyn_load, (path), char * path)
{
  return (shl_load (path,
		    (BIND_IMMEDIATE | BIND_NONFATAL | DYNAMIC_PATH),
		    0));
}

static int 
DEFUN (dyn_lookup, (handle, symbol, type, result),
       dyn_load_handle_t * handle
       AND char * symbol
       AND int type
       AND PTR * result)
{
  return (shl_findsym (handle, symbol, type, result));
}

#else /* not _HPUX */

#include <dlfcn.h>

#define TYPE_PROCEDURE	0
#define TYPE_DATA	1
#define TYPE_UNDEFINED	2

typedef void * dyn_load_handle_t;

#define PROG_HANDLE ((dyn_load_handle_t) NULL)

static dyn_load_handle_t
DEFUN (dyn_load, (path), char * path)
{
  dyn_load_handle_t result = (dlopen (path, RTLD_LAZY));

#if 0
  if (result == ((dyn_load_handle_t) NULL))
    fprintf (stderr, "\ndlopen: %s.\n", (dlerror ()));
#endif

  return (result);
}

static int
DEFUN (dyn_lookup, (handle, symbol, type, result),
       dyn_load_handle_t * handle
       AND char * symbol
       AND int type
       AND PTR * result)
{
  * result = (dlsym ((* handle), symbol));
  return (((* result) == ((PTR) NULL))
	  ? -1
	  : 0);
}

#endif /* _HPUX */

DEFINE_PRIMITIVE ("LOAD-OBJECT-FILE", Prim_load_object_file, 1, 1,
		  "(load-object-file lib-file)")
{
  extern int errno;
  dyn_load_handle_t prim_lib_handle;
  PRIMITIVE_HEADER (1);

  prim_lib_handle = (dyn_load (STRING_ARG (1)));
  if (prim_lib_handle == ((dyn_load_handle_t) NULL))
    error_system_call (errno, syscall_dld);
  PRIMITIVE_RETURN (long_to_integer ((long) prim_lib_handle));
}

static short dyn_load_types [] =
{
  TYPE_PROCEDURE,
  TYPE_DATA,
  TYPE_UNDEFINED
};

DEFINE_PRIMITIVE ("OBJECT-LOOKUP-SYMBOL", Prim_object_lookup_symbol, 3, 3,
		  "(object-lookup-symbol handle sym type)")
{
  char * sym;
  short type;
  PTR result;
  dyn_load_handle_t prim_lib_handle, arg_handle;
  PRIMITIVE_HEADER (3);

  switch (ARG_REF (1))
  {
    case SHARP_F:
      prim_lib_handle = PROG_HANDLE;
      arg_handle = & prim_lib_handle;
      break;

    case SHARP_T:
      arg_handle = ((dyn_load_handle_t *) NULL);
      break;

    default:
      prim_lib_handle = ((dyn_load_handle_t) (arg_integer (1)));
      arg_handle = & prim_lib_handle;
      break;
  }

  sym = (STRING_ARG (2));
  type = dyn_load_types [arg_index_integer (3, ((sizeof (dyn_load_types))
						/ (sizeof (short))))];

  if ((dyn_lookup (arg_handle, sym, type, & result)) == -1)
    PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (long_to_integer ((long) result));
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
