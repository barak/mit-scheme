/* -*-C-*-

$Id: foreign.c,v 1.9 2007/01/12 03:45:55 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* This file contains the primitive support for the foreign function */
/* interface. */

#include <stdio.h>
#include <dl.h>
#include "scheme.h"
#include "prims.h"
#include "ux.h"
#include "osfs.h"
#include "foreign.h"

static int initialization_done = 0;

#define INITIALIZE_ONCE()						\
{									\
  if (!initialization_done)						\
    initialize_once ();							\
}

static void EXFUN (initialize_once, (void));

/* Allocation table stuff stolen from x11base.c */

PTR
DEFUN (foreign_malloc, (size), unsigned int size)
{
  PTR result = (UX_malloc (size));
  if (result == 0)
    error_external_return ();
  return (result);
}

PTR
DEFUN (foreign_realloc, (ptr, size), PTR ptr AND unsigned int size)
{
  PTR result = (UX_realloc (ptr, size));
  if (result == 0)
    error_external_return ();
  return (result);
}

struct allocation_table
{
  PTR * items;
  int length;
};

static struct allocation_table foreign_object_table;
static struct allocation_table foreign_function_table;

static void
DEFUN (allocation_table_initialize, (table), struct allocation_table * table)
{
  (table -> length) = 0;
}

static unsigned int
DEFUN (allocate_table_index, (table, item),
       struct allocation_table * table AND
       PTR item)
{
  unsigned int length = (table -> length);
  unsigned int new_length;
  PTR * items = (table -> items);
  PTR * new_items;
  PTR * scan;
  PTR * end;
  if (length == 0)
    {
      new_length = 4;
      new_items = (foreign_malloc ((sizeof (PTR)) * new_length));
    }
  else
    {
      scan = items;
      end = (scan + length);
      while (scan < end)
	if ((*scan++) == 0)
	  {
	    (*--scan) = item;
	    return (scan - items);
	  }
      new_length = (length * 2);
      new_items = (foreign_realloc (items, ((sizeof (PTR)) * new_length)));
    }
  scan = (new_items + length);
  end = (new_items + new_length);
  (*scan++) = item;
  while (scan < end)
    (*scan++) = 0;
  (table -> items) = new_items;
  (table -> length) = new_length;
  return (length);
}

static PTR
DEFUN (allocation_item_arg, (arg, table),
       unsigned int arg AND
       struct allocation_table * table)
{
  unsigned int index = (arg_index_integer (arg, (table -> length)));
  PTR item = ((table -> items) [index]);
  if (item == 0)
    error_bad_range_arg (arg);
  return (item);
}

/* Helper functions */
HANDLE
DEFUN (arg_handle, (arg_number), unsigned int arg_number)
{
  SCHEME_OBJECT arg;

  return (index_to_handle (arg_index_integer (arg_number,
					      foreign_object_table . length)));
}

HANDLE
DEFUN (foreign_pointer_to_handle, (ptr), PTR ptr)
{
  unsigned int index;
  HANDLE handle;
  FOREIGN_OBJECT *ptr_object;

  INITIALIZE_ONCE ();
  ptr_object = (FOREIGN_OBJECT *) foreign_malloc (sizeof (FOREIGN_OBJECT));
  ptr_object -> ptr = ptr;
  ptr_object -> handle = handle;
  index = allocate_table_index (&foreign_object_table, (PTR) ptr_object);
  handle = index_to_handle (index);
  ((FOREIGN_OBJECT *) ((foreign_object_table . items) [index])) -> handle =
      handle;
  return (handle_to_integer (handle));
}

PTR
DEFUN (handle_to_foreign_pointer, (handle), HANDLE handle)
{
  unsigned int index;

  index = handle_to_index (handle);
  if (index >= foreign_object_table . length) {
    error_external_return ();
  }
  return
    (((FOREIGN_OBJECT *) ((foreign_object_table . items) [index])) -> ptr);
}

int 
DEFUN (find_foreign_function, (func_name), char *func_name)
{
  int i;
  FOREIGN_FUNCTION *func_item;
  
  for (i=0; i < foreign_function_table . length; i++) {
    func_item = (foreign_function_table . items) [i];
    if (func_item == 0) continue;
    if (! strcmp (func_item -> name, func_name)) {
      return (i);
    }
  }
  return (-1);
}

unsigned int
DEFUN (register_foreign_function, (name, applicable_function),
                                  char * name AND
                                  PTR applicable_function)
{
  FOREIGN_FUNCTION *func_item;
  char * name_copy;

  INITIALIZE_ONCE ();
  func_item = (FOREIGN_FUNCTION *) foreign_malloc (sizeof (FOREIGN_FUNCTION));
  name_copy = (char *) foreign_malloc (1 + strlen (name));
  strcpy (name_copy, name);
  func_item -> name = name_copy;
  func_item -> applicable_function = applicable_function;
  return (allocate_table_index (&foreign_function_table, (PTR) func_item));
}

unsigned int
DEFUN (list_length, (list), SCHEME_OBJECT list)
{
  unsigned int i;

  i = 0;
  TOUCH_IN_PRIMITIVE (list, list);
  while (PAIR_P (list)) {
    i += 1;
    TOUCH_IN_PRIMITIVE ((PAIR_CDR (list)), list);
  }
  return (i);
}

PTR
DEFUN (apply_foreign_function, (func, arg_list),
                               PTR (*func)() AND
                               SCHEME_OBJECT arg_list)
{
  unsigned int arg_list_length;
  PTR * arg_vec;
  PTR result;
  unsigned int i;

  arg_list_length = list_length (arg_list);
  arg_vec = (PTR *) foreign_malloc (arg_list_length);
  for (i = 0; i < arg_list_length; i++, arg_list = PAIR_CDR (arg_list)) {
    arg_vec [i] = handle_to_foreign_pointer (PAIR_CAR (arg_list));
  }
  result = (*func) (arg_vec);
  free (arg_vec);
  return (result);
}

SCHEME_OBJECT
DEFUN (foreign_pointer_to_scheme_object, (ptr, type_translator),
                                         PTR ptr AND
                                         SCHEME_OBJECT (*type_translator) ())
{
  return (type_translator (ptr));
}

/* old version of foreign_pointer_to_scheme_object */
#if 0 
/* Note that foreign_pointer_to_scheme_object takes a pointer to pointer
   (i.e. a call by reference to a pointer) so that it can increment the
   pointer according to its type. This is used by the code which builds
   the composite objects. */

SCHEME_OBJECT
DEFUN (foreign_pointer_to_scheme_object, (ptr_to_ptr, type),
                                         PTR ptr_to_ptr AND
                                         SCHEME_OBJECT type)
{
  long type_enum;
  
  if (foreign_primtive_type_p (type)) {
    long long_val;
    double double_val;
    PTR temp_ptr;
    type_enum = integer_to_long (type);
    switch (type_enum) {
      case FOREIGN_INT:
	temp_ptr = ALIGN_FOREIGN_POINTER (*ptr_to_ptr, FOREIGN_INT);
        *ptr_to_ptr = (((int *) temp_ptr) + 1);
        long_val = (long) ((int) *temp_ptr);
      case FOREIGN_SHORT:
	temp_ptr = ALIGN_FOREIGN_POINTER (*ptr_to_ptr, FOREIGN_SHORT);
        *ptr_to_ptr = (((short *) temp_ptr) + 1);
        long_val = (long) ((short) *temp_ptr);
      case FOREIGN_LONG:
	temp_ptr = ALIGN_FOREIGN_POINTER (*ptr_to_ptr, FOREIGN_INT);
        *ptr_to_ptr = (((long *) temp_ptr) + 1);
        long_val = (long) *temp_ptr;
        return (long_to_integer (long_val));
      case FOREIGN_CHAR:
	temp_ptr = ALIGN_FOREIGN_POINTER (*ptr_to_ptr, FOREIGN_CHAR);
        *ptr_to_ptr = (((char *) temp_ptr) + 1);
	return (ASCII_TO_CHAR ((char) *temp_ptr));
      case FOREIGN_FLOAT:
	temp_ptr = ALIGN_FOREIGN_POINTER (*ptr_to_ptr, FOREIGN_FLOAT);
        *ptr_to_ptr = (((float *) temp_ptr) + 1);
        double_val = (double) ((float) *temp_ptr);
      case FOREIGN_DOUBLE:
	temp_ptr = ALIGN_FOREIGN_POINTER (*ptr_to_ptr, FOREIGN_DOUBLE);
        *ptr_to_ptr = (((double *) temp_ptr) + 1);
        double_val = (double) *temp_ptr;
	return (double_to_flonum (double_val));
      case FOREIGN_STRING:
	temp_ptr = ALIGN_FOREIGN_POINTER (*ptr_to_ptr, FOREIGN_STRING);
        *ptr_to_ptr = (((unsigned char *) temp_ptr) + 1);
	return (char_pointer_to_string (temp_ptr));
      case FOREIGN_PTR:
	temp_ptr = ALIGN_FOREIGN_POINTER (*ptr_to_ptr, FOREIGN_PTR);
        *ptr_to_ptr = (((PTR) temp_ptr) + 1);
	return (long_to_integer ((long) *temp_ptr));
      default:
	error_external_return ();
    }
  } else if (foreign_composite_type_p (type)) {
    /* We should probably tag the result vector. */
    type_enum = integer_to_long (which_composite_type (type));
    switch (type_enum) {
      case FOREIGN_STRUCT:
      case FOREIGN_UNION:
      {
	int num_fields;
	SCHEME_OBJECT field_types;
	SCHEME_OBJECT result_vector;
	unsigned int i;

	field_types = composite_type_field_types (type);
	num_fields = list_length (field_types);
	result_vector = allocate_marked_vector (TC_VECTOR, num_fields, true);
	for (i = 0; i < num_fields; ++i) {
	  if (!(PAIR_P (field_types))) {
	    error_external_return ();
	  }
	  FAST_VECTOR_SET (result_vector,
			   i,
			   foreign_pointer_to_scheme_object (
			    ptr_to_ptr, PAIR_CAR (field_types)));
	  TOUCH_IN_PRIMITIVE ((PAIR_CDR (field_types)), field_types);
	}
	return (result_vector);
      }
      default:
        error_external_return ();
    }
  } else {
    error_external_return ();
  }
}
#endif  /* if 0 */

static void
DEFUN_VOID (initialize_once)
{
  allocation_table_initialize (&foreign_object_table);
  allocation_table_initialize (&foreign_function_table);
  
  initialization_done = 1;
}

/* Functions to go in osxx.c */

#include <dl.h>

char *
DEFUN_VOID (OS_create_temporary_file_name)
{
  char * name_string;

  name_string = (char *) foreign_malloc (1 + TEMP_FILE_NAME_MAX_LEN);
  (void) UX_tmpnam (name_string);
  return (name_string);
}

#ifdef HAVE_DYNAMIC_LOADING
#ifdef __HPUX__
#include <dl.h>

LOAD_INFO *
DEFUN (OS_load_object_file, (load_file_name), char * load_file_name)
{
  shl_t shl_handle;
  int result;
  struct shl_descriptor *shl_desc;
  LOAD_INFO *info;

  shl_handle = shl_load (load_file_name, BIND_DEFERRED, 0L);
  
  if (shl_handle == NULL) {
    error_external_return ();
  }

  result = shl_gethandle (shl_handle, &shl_desc);

  if (result == -1) {
    error_external_return ();
  }

  info = foreign_malloc (sizeof (LOAD_INFO));
  info -> load_module_descriptor = shl_handle;
  info -> program_start = shl_desc -> tstart;
  info -> program_end = shl_desc -> tend;
  info -> data_start = shl_desc -> dstart;
  info -> data_end = shl_desc -> dend;
  return (info);
}

PTR
DEFUN (OS_find_function, (load_info, func_name),
                       LOAD_INFO * load_info AND
                       char * func_name)
{
  int return_code;
  PTR (* test_proc)();
  LOAD_DESCRIPTOR desc;

  desc = (load_info -> load_module_descriptor);
  return_code = shl_findsym (&desc ,
			     func_name,
			     TYPE_PROCEDURE,
			     (long *) &test_proc);

  return ((return_code == 0) ?
	  test_proc :
	  NULL);
}

#endif /* __HPUX__ */
#endif /* HAVE_DYNAMIC_LOADING */

/* Definitions of primitives */

DEFINE_PRIMITIVE ("CALL-FOREIGN-FUNCTION",
		  Prim_call_foreign_function, 2, 2,
"Calls the foreign function referenced by HANDLE with the ARG-LIST \n\
arguments. \n\
Returns a handle to the return value; \n\
The foreign function should have been created by  \n\
CREATE_PRIMITIVE_FOREIGN_FUNCTION. \n\
The elements of the ARG-LIST must be handles to foreign objects. \n\
Type and arity checking on the arguments should already have been done.")
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT arg_list;
    PTR result;

    CHECK_ARG (2, APPARENT_LIST_P);
    arg_list = ARG_REF (2);
    result = apply_foreign_function (handle_to_foreign_pointer
				     (arg_handle (1)), arg_list);
    PRIMITIVE_RETURN (foreign_pointer_to_handle (result));
  }
}

DEFINE_PRIMITIVE ("&CALL-FOREIGN-FUNCTION-RETURNING-SCHEME-OBJECT",
		  Prim_call_foreign_function_returning_scheme_object, 2, 2,
"Calls the foreign function referenced by HANDLE with the ARG-LIST \n\
arguments. \n\
Returns the result of the foreign function (which better be a scheme \n\
object. \n\
The foreign function should have been created by  \n\
CREATE_PRIMITIVE_FOREIGN_FUNCTION. \n\
The elements of the ARG-LIST must be handles to foreign objects. \n\
Type and arity checking on the arguments should already have been done.")
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT arg_list;
    PTR result;

    CHECK_ARG (2, APPARENT_LIST_P);
    arg_list = ARG_REF (2);
    result = apply_foreign_function (handle_to_foreign_pointer
				     (arg_handle (1)), arg_list);
    PRIMITIVE_RETURN (result);
  }
}

DEFINE_PRIMITIVE ("FOREIGN-HANDLE-TO-SCHEME-OBJECT",
		  Prim_foreign_handle_to_scheme_object, 2, 2,
"Returns the Scheme object corresponding to the foreign HANDLE \n\
interpreted as the foreign type TYPE.   \n\
A type is either an integer which enumerates the various foreign types \n\
(i.e.  FOREIGN_INT, FOREIGN_CHAR, FOREIGN_SHORT, FOREIGN_LONG, \n\
(FOREIGN_PTR, FOREIGN_DOUBLE, FOREIGN_STRING) or a list whose car is \n\
an integer representing FOREIGN_STRUCT or FOREIGN_UNION and whose cdr \n\
is a list of types.")
{
  PRIMITIVE_HEADER (2);
  {
    SCHEME_OBJECT arg2;
    PTR arg1_ptr;

    arg1_ptr = handle_to_foreign_pointer (arg_handle (1));
    arg2 = ARG_REF (2);
    if (! (INTEGER_P (arg2) || PAIR_P (arg2))) {
      error_wrong_type_arg (2);
    }
    PRIMITIVE_RETURN (foreign_pointer_to_scheme_object (&arg1_ptr, arg2));
  }
}

DEFINE_PRIMITIVE (LOAD-FOREIGN-FILE, Prim_load_foreign_file, 1, 1,
"Load the foreign object file FILENAME. \n\
Returns a handle to a LOAD_INFO data structure.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (foreign_pointer_to_handle
		     (OS_load_object_file (STRING_ARG (1))));
}

DEFINE_PRIMITIVE (CREATE-TEMPORARY-FILE-NAME, Prim_get_temporary_file_name,
		  0, 0,
"Return a temporary file name.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string (OS_create_temporary_file_name ()));
}

DEFINE_PRIMITIVE (FIND-FOREIGN-FUNCTION, Prim_find_foreign_function, 2, 2,
"Returns a handle to a foreign function. \n\
Takes the FUNCTION_NAME as a string and LOAD_INFO \n\
which is a handle to a load_info structure returned by LOAD-FOREIGN-FILE. \n\
If LOAD_INFO is not #F then we search for FUNCTION_NAME in the code which \n\
was loaded to yield LOAD_INFO. \n\
If LOAD_INFO is #F then we search over all the dynamically loaded files.")
{
  PRIMITIVE_HEADER (2);
  {
    PTR func_ptr;
    LOAD_INFO * load_info;

    load_info = ((EMPTY_LIST_P (ARG_REF (2))) ?
		 ((LOAD_INFO *) NULL) :
		 ((LOAD_INFO *) handle_to_foreign_pointer (arg_handle (2))));
		  
    func_ptr = OS_find_function (load_info, STRING_ARG (1));

    PRIMITIVE_RETURN ((func_ptr == NULL) ?
		      SHARP_F :
		      foreign_pointer_to_handle (func_ptr));
  }
}
