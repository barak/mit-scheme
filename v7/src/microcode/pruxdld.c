/* -*-C-*-

$Id: pruxdld.c,v 1.13 2000/12/05 21:23:47 cph Exp $

Copyright (c) 1993-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* This file contains the interface to a unix dynamic loader.
   It has been tried under HP-UX, SunOS (4.1.?), and Alpha OSF 1.
 */

/* This #if covers the entire file. */
#ifndef DISABLE_DLD_SUPPORT

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "syscall.h"
#include "os.h"

#if defined(_AIX)

#include <sys/ldr.h>

typedef int * dyn_load_handle_t;

#define TYPE_PROCEDURE	0
#define TYPE_DATA	1
#define TYPE_UNDEFINED	2

#define PROG_HANDLE ((dyn_load_handle_t) NULL)

static dyn_load_handle_t
DEFUN (dyn_load, (path), char * path)
{
  extern int EXFUN (main, (int, char *, char **));
  dyn_load_handle_t result = (load (path, L_NOAUTODEFER, ((char *) NULL)));
  if (result != ((dyn_load_handle_t) NULL))
    loadbind (0, main, result);
  return (result);
}

static int
DEFUN (dyn_lookup, (handle, symbol, type, result),
       dyn_load_handle_t * handle
       AND char * symbol
       AND int type
       AND PTR * result)
{
  /* This is bogus */
  * result = ((PTR) (* handle));
  return (0);
}

#else /* not _AIX */
#if defined(__HPUX__)

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
#if !(defined(hp9000s300) || defined(__hp9000s300))
  return (shl_findsym (handle, symbol, type, result));
#else
  /* External symbols on the 300s often have underscores.
     Look both ways.
   */
  char * temp;
  int retval = (shl_findsym (handle, symbol, type, result));

  if (retval != -1)
    return (retval);
  temp = ((char *) (malloc (2 + (strlen (symbol)))));
  if (temp == ((char *) NULL))
    return (-1);
  *temp = '_';
  strcpy (temp + 1, symbol);
  retval = (shl_findsym (handle, temp, type, result));
  free (temp);
  return (retval);
#endif
}

#else /* not __HPUX__ */

#include <dlfcn.h>

#define TYPE_PROCEDURE	0
#define TYPE_DATA	1
#define TYPE_UNDEFINED	2

typedef void * dyn_load_handle_t;

#define PROG_HANDLE ((dyn_load_handle_t) NULL)

static dyn_load_handle_t
DEFUN (dyn_load, (path), char * path)
{
  dyn_load_handle_t result = (dlopen (path, RTLD_LAZY | RTLD_GLOBAL));

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

#endif /* __HPUX__ */
#endif /* _AIX */

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
  dyn_load_handle_t prim_lib_handle, * arg_handle;
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
		  "(address)\n\
Treat the integer argument as the address of a C procedure of no\n\
arguments that returns a long.  Invoke it, and return\n\
the corresponding Scheme integer.")
{
  long address;
  long result;
  long EXFUN ((* thunk), (void));
  PRIMITIVE_HEADER (1);
  
  address = ((long) (arg_integer (1)));
  thunk = ((long EXFUN ((*), (void))) address);
  result = ((* thunk) ());
  PRIMITIVE_RETURN (long_to_integer (result));
}

DEFINE_PRIMITIVE ("ADDRESS-TO-STRING", Prim_address_to_string, 1, 1,
		  "(address)\n\
Treat the integer argument as a C (char *) pointer.\n\
Construct the corresponding Scheme string.")
{
  long address;
  PRIMITIVE_HEADER (1);

  address = ((long) (arg_integer (1)));
  PRIMITIVE_RETURN (char_pointer_to_string ((unsigned char *) address));
}

#endif /* not DISABLE_DLD_SUPPORT */
