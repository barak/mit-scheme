/* -*-C-*-

$Id: pruxdld.c,v 1.16 2003/01/05 23:30:21 cph Exp $

Copyright (c) 1993-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

/* This file contains the interface to the unix dynamic loader.  */

#include "scheme.h"
#include "prims.h"
#include "usrdef.h"
#include "syscall.h"
#include "os.h"

#ifdef __linux__

#include <dlfcn.h>

static unsigned long
DEFUN (dld_load, (path), CONST char * path)
{
  void * handle = (dlopen (path, (RTLD_LAZY | RTLD_GLOBAL)));
  if (handle == 0)
    {
      SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (v, 0, (LONG_TO_UNSIGNED_FIXNUM (ERR_IN_SYSTEM_CALL)));
      VECTOR_SET (v, 1, (char_pointer_to_string ("dlopen")));
      VECTOR_SET (v, 2, (char_pointer_to_string (dlerror ())));
      error_with_argument (v);
    }
  return ((unsigned long) handle);
}

static unsigned long
DEFUN (dld_lookup, (handle, symbol), unsigned long handle AND char * symbol)
{
  CONST char * old_error = (dlerror ());
  void * address = (dlsym (((void *) handle), symbol));
  CONST char * new_error = (dlerror ());
  if ((address == 0) && (new_error != old_error))
    {
      SCHEME_OBJECT v = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (v, 0, (LONG_TO_UNSIGNED_FIXNUM (ERR_IN_SYSTEM_CALL)));
      VECTOR_SET (v, 1, (char_pointer_to_string ("dlsym")));
      VECTOR_SET (v, 2, (char_pointer_to_string (new_error)));
      error_with_argument (v);
    }
  return ((unsigned long) address);
}

#endif /* __linux__ */

DEFINE_PRIMITIVE ("LOAD-OBJECT-FILE", Prim_load_object_file, 1, 1,
		  "(FILENAME)\n\
Load the shared library FILENAME and return a handle for it.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (ulong_to_integer (dld_load (STRING_ARG (1))));
}

DEFINE_PRIMITIVE ("OBJECT-LOOKUP-SYMBOL", Prim_object_lookup_symbol, 3, 3,
		  "(HANDLE SYMBOL TYPE)\n\
Look up SYMBOL, a Scheme string, in the dynamically-loaded file\n\
referenced by HANDLE.  TYPE is obsolete and must be specified as zero.\n\
Returns the symbol's address, or signals an error if no such symbol.")
{
  PRIMITIVE_HEADER (3);
  if ((ARG_REF (3)) != FIXNUM_ZERO)
    error_wrong_type_arg (3);
  PRIMITIVE_RETURN
    (ulong_to_integer
     (dld_lookup ((arg_ulong_integer (1)), (STRING_ARG (2)))));
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
     ((* ((unsigned long EXFUN ((*), (void))) (arg_ulong_integer (1))))
      ()));
}

DEFINE_PRIMITIVE ("ADDRESS-TO-STRING", Prim_address_to_string, 1, 1,
		  "(ADDRESS)\n\
Treat ADDRESS, a Scheme integer corresponding to a C unsigned long, as\n\
a C char * pointer.  Allocate and return a Scheme string with the same\n\
contents.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (char_pointer_to_string ((unsigned char *) (arg_ulong_integer (1))));
}
