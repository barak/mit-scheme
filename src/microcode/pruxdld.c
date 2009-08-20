/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* This file contains the interface to the unix dynamic loader.  */

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "syscall.h"
#include "os.h"
#include <dlfcn.h>


static bool cleanup_registered_p = false;
static unsigned int loaded_handles_size = 0;
static unsigned int n_loaded_handles = 0;
static void ** loaded_handles = 0;

static void * dld_load (const char *);
static void dld_unload (void *);
static void dld_unload_all (void);
static void * dld_lookup (void *, const char *);

#define ARG_HANDLE(n) ((void *) (arg_ulong_integer (n)))

DEFINE_PRIMITIVE ("DLD-LOAD-FILE", Prim_dld_load_file, 2, 2,
		  "(FILENAME WEAK-PAIR)\n\
Load the shared library FILENAME and store its handle\n\
in the cdr of WEAK-PAIR.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (2, WEAK_PAIR_P);
  SET_PAIR_CDR ((ARG_REF (2)),
		(ulong_to_integer
		 ((unsigned long)
		  (dld_load (((ARG_REF (1)) == SHARP_F)
			     ? 0
			     : (STRING_ARG (1)))))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DLD-LOOKUP-SYMBOL", Prim_dld_lookup_symbol, 2, 2,
		  "(HANDLE STRING)\n\
Look up the symbol named STRING in the shared library specified by HANDLE.\n\
Return the symbol's address, or #F if no such symbol.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (ulong_to_integer
     ((unsigned long) (dld_lookup ((ARG_HANDLE (1)), (STRING_ARG (2))))));
}

DEFINE_PRIMITIVE ("DLD-UNLOAD-FILE", Prim_dld_unload_file, 1, 1,
		  "(HANDLE)\n\
Unload the shared library specified by HANDLE.\n\
The file is unmapped from memory, and its symbols become unbound.")
{
  PRIMITIVE_HEADER (1);
  dld_unload (ARG_HANDLE (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("INVOKE-C-THUNK", Prim_invoke_C_thunk, 1, 1,
		  "(ADDRESS)\n\
Treat ADDRESS, a Scheme integer corresponding to a C unsigned long, as\n\
the address of a C procedure of no arguments that returns an unsigned\n\
long.  Invoke it, and return the corresponding Scheme integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer
     ((* ((unsigned long (*) (void)) (arg_ulong_integer (1))))
      ()));
}

DEFINE_PRIMITIVE ("ADDRESS-TO-STRING", Prim_address_to_string, 1, 1,
		  "(ADDRESS)\n\
Treat ADDRESS, a Scheme integer corresponding to a C unsigned long, as\n\
a C char * pointer.  Allocate and return a Scheme string with the same\n\
contents.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (char_pointer_to_string ((char *) (arg_ulong_integer (1))));
}

static void *
dld_load (const char * path)
{
  void * handle;

  if (!cleanup_registered_p)
    {
      add_reload_cleanup (dld_unload_all);
      cleanup_registered_p = true;
    }

  handle = (dlopen (path, (RTLD_LAZY | RTLD_GLOBAL)));
  if (handle == 0)
    {
      SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (v, 0, (LONG_TO_UNSIGNED_FIXNUM (ERR_IN_SYSTEM_CALL)));
      VECTOR_SET (v, 1, (char_pointer_to_string ("dlopen")));
      VECTOR_SET (v, 2, (char_pointer_to_string (dlerror ())));
      error_with_argument (v);
    }
  if (n_loaded_handles == loaded_handles_size)
    {
      if (loaded_handles_size == 0)
	{
	  loaded_handles_size = 16;
	  loaded_handles
	    = (OS_malloc (loaded_handles_size * (sizeof (void *))));
	}
      else
	{
	  loaded_handles_size *= 2;
	  loaded_handles
	    = (OS_realloc (loaded_handles,
			   (loaded_handles_size * (sizeof (void *)))));
	}
    }
  (loaded_handles[n_loaded_handles++]) = handle;
  return (handle);
}

static void
dld_unload (void * handle)
{
  if ((dlclose (handle)) != 0)
    {
      SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (v, 0, (LONG_TO_UNSIGNED_FIXNUM (ERR_IN_SYSTEM_CALL)));
      VECTOR_SET (v, 1, (char_pointer_to_string ("dlclose")));
      VECTOR_SET (v, 2, (char_pointer_to_string (dlerror ())));
      error_with_argument (v);
    }
  {
    void ** scan = loaded_handles;
    void ** end = (scan + n_loaded_handles);
    for (; (scan < end); scan += 1)
      if ((*scan) == handle)
	{
	  (*scan) = (* (end - 1));
	  n_loaded_handles -= 1;
	  break;
	}
  }
}

static void
dld_unload_all (void)
{
  if (loaded_handles_size > 0)
    {
      void ** scan = loaded_handles;
      void ** end = (scan + n_loaded_handles);
      while (scan < end)
	dlclose (*scan++);

      OS_free (loaded_handles);
      loaded_handles_size = 0;
      n_loaded_handles = 0;
      loaded_handles = 0;
    }
}

static void *
dld_lookup (void * handle, const char * symbol)
{
  void * address;
  const char * error_string;

  dlerror ();			/* discard any outstanding errors */
  address = (dlsym (handle, symbol));
  error_string = (dlerror ());
  if (error_string != 0)
    {
      SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (v, 0, (LONG_TO_UNSIGNED_FIXNUM (ERR_IN_SYSTEM_CALL)));
      VECTOR_SET (v, 1, (char_pointer_to_string ("dlopen")));
      VECTOR_SET (v, 2, (char_pointer_to_string (error_string)));
      error_with_argument (v);
    }
  return (address);
}
