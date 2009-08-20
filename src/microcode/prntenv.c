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

/* Unix-specific process-environment primitives. */
/* Win32 imitation */

#include "scheme.h"
#include "prims.h"
#include "nt.h"
#include "ntio.h"

DEFINE_PRIMITIVE ("FILE-TIME->STRING", Prim_file_time_to_string, 1, 1,
  "Convert a file system time stamp into a date/time string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, INTEGER_P);
  {
    time_t clock = (arg_integer (1));
    char * time_string = (ctime (&clock));
    if (time_string == 0)
      PRIMITIVE_RETURN (SHARP_F);
    (time_string[24]) = '\0';
    PRIMITIVE_RETURN (char_pointer_to_string (time_string));
  }
}

DEFINE_PRIMITIVE ("GET-ENVIRONMENT-VARIABLE", Prim_get_environment_variable, 1, 1,
  "Look up the value of a variable in the user's shell environment.\n\
The argument, a variable name, must be a string.\n\
The result is either a string (the variable's value),\n\
 or #F indicating that the variable does not exist.")
{
  PRIMITIVE_HEADER (1);
  {
    const char * variable_value = (getenv (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((variable_value == 0)
       ? SHARP_F
       : (char_pointer_to_string (variable_value)));
  }
}

#define VQRESULT(index, value)						\
  VECTOR_SET (result, index, (ulong_to_integer (value)))


DEFINE_PRIMITIVE ("WIN32-VIRTUAL-QUERY", Prim_win32_virtual_query, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    MEMORY_BASIC_INFORMATION info;
    SCHEME_OBJECT result;
    (void) VirtualQuery
      (((LPCVOID) (arg_ulong_integer (1))), (&info), (sizeof (info)));
    result = (allocate_marked_vector (TC_VECTOR, 7, 1));
    VQRESULT (0, ((unsigned long) (info.BaseAddress)));
    VQRESULT (1, ((unsigned long) (info.AllocationBase)));
    VQRESULT (2, (info.AllocationProtect));
    VQRESULT (3, (info.RegionSize));
    VQRESULT (4, (info.State));
    VQRESULT (5, (info.Protect));
    VQRESULT (6, (info.Type));
    PRIMITIVE_RETURN (result);
  }
}

/* Registry Access */

#define REGISTRY_API_CALL(proc, args)					\
{									\
  LONG API_code = (proc args);						\
  if (API_code != ERROR_SUCCESS)					\
    NT_error_api_call (API_code, apicall_ ## proc);			\
}

#define HKEY_ARG(n) ((HKEY) (arg_ulong_integer (n)))
#define SUBKEY_ARG(n) ((LPCTSTR) (STRING_ARG (n)))
#define HKEY_TO_OBJECT(hkey) (ulong_to_integer ((unsigned long) (hkey)))

#define GUARANTEE_RESULT_SPACE()					\
{									\
  /* Do GC now if not enough storage to cons result. */			\
  /* 1024 is arbitrary but big enough for these primitives.  */		\
  Primitive_GC_If_Needed (1024);					\
}

#define ACCUM_PRK(name)							\
{									\
  v = (cons ((cons ((char_pointer_to_string (#name)),			\
		    (HKEY_TO_OBJECT (name)))),				\
	     v));							\
}

DEFINE_PRIMITIVE ("win32-predefined-registry-keys", Prim_win32_predefined_registry_keys, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    SCHEME_OBJECT v = EMPTY_LIST;
#ifdef HKEY_CLASSES_ROOT
    ACCUM_PRK (HKEY_CLASSES_ROOT);
#endif
#ifdef HKEY_CURRENT_USER
    ACCUM_PRK (HKEY_CURRENT_USER);
#endif
#ifdef HKEY_LOCAL_MACHINE
    ACCUM_PRK (HKEY_LOCAL_MACHINE);
#endif
#ifdef HKEY_USERS
    ACCUM_PRK (HKEY_USERS);
#endif
#ifdef HKEY_PERFORMANCE_DATA
    ACCUM_PRK (HKEY_PERFORMANCE_DATA);
#endif
#ifdef HKEY_CURRENT_CONFIG
    ACCUM_PRK (HKEY_CURRENT_CONFIG);
#endif
#ifdef HKEY_DYN_DATA
    ACCUM_PRK (HKEY_DYN_DATA);
#endif
    PRIMITIVE_RETURN (v);
  }
}

DEFINE_PRIMITIVE ("win32-open-registry-key", Prim_win32_open_registry_key, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, WEAK_PAIR_P);
  GUARANTEE_RESULT_SPACE ();
  {
    HKEY result;
    REGSAM mask = KEY_ALL_ACCESS;
    while (1)
      {
	LONG code
	  = (RegOpenKeyEx ((HKEY_ARG (1)), (SUBKEY_ARG (2)), 0,
			   mask, (&result)));
	if (code == ERROR_SUCCESS)
	  {
	    SET_PAIR_CDR ((ARG_REF (3)), (HKEY_TO_OBJECT (result)));
	    break;
	  }
	if (code == ERROR_FILE_NOT_FOUND)
	  {
	    SET_PAIR_CDR ((ARG_REF (3)), SHARP_F);
	    break;
	  }
	if (code == ERROR_ACCESS_DENIED)
	  switch (mask)
	    {
	    case KEY_ALL_ACCESS:
	      mask = KEY_READ;
	      continue;
	    case KEY_READ:
	      mask = KEY_ENUMERATE_SUB_KEYS;
	      continue;
	    case KEY_ENUMERATE_SUB_KEYS:
	      break;
	    }
	NT_error_api_call (code, apicall_RegOpenKeyEx);
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("win32-create-registry-key", Prim_win32_create_registry_key, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  CHECK_ARG (3, WEAK_PAIR_P);
  GUARANTEE_RESULT_SPACE ();
  {
    HKEY result;
    DWORD disposition;
    REGSAM mask = KEY_ALL_ACCESS;
    while (1)
      {
	LONG code
	  = (RegCreateKeyEx ((HKEY_ARG (1)), (SUBKEY_ARG (2)), 0,
			     "", REG_OPTION_NON_VOLATILE,
			     mask, 0, (&result), (&disposition)));
	if (code == ERROR_SUCCESS)
	  break;
	if (code == ERROR_ACCESS_DENIED)
	  switch (mask)
	    {
	    case KEY_ALL_ACCESS:
	      mask = KEY_READ;
	      continue;
	    case KEY_READ:
	      mask = KEY_ENUMERATE_SUB_KEYS;
	      continue;
	    case KEY_ENUMERATE_SUB_KEYS:
	      break;
	    }
	NT_error_api_call (code, apicall_RegCreateKeyEx);
      }
    SET_PAIR_CDR ((ARG_REF (3)), (HKEY_TO_OBJECT (result)));
    PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (disposition == REG_CREATED_NEW_KEY));
  }
}

DEFINE_PRIMITIVE ("win32-close-registry-key", Prim_win32_close_registry_key, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  REGISTRY_API_CALL (RegCloseKey, (HKEY_ARG (1)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("win32-set-registry-value", Prim_win32_set_registry_value, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  {
    DWORD data_type = (arg_ulong_integer (3));
    DWORD data_length;
    BYTE * data;
    union
      {
	DWORD dword;
	BYTE bytes [4];
      } dword_data;
    switch (data_type)
      {
      case REG_DWORD_LITTLE_ENDIAN:
	{
	  DWORD arg = (arg_ulong_integer (4));
	  ((dword_data . bytes) [0]) = (arg & 0xFF);
	  ((dword_data . bytes) [1]) = ((arg >> 8) & 0xFF);
	  ((dword_data . bytes) [2]) = ((arg >> 16) & 0xFF);
	  ((dword_data . bytes) [3]) = ((arg >> 24) & 0xFF);
	}
	data_length = (sizeof (dword_data . bytes));
	data = (dword_data . bytes);
	break;
      case REG_DWORD_BIG_ENDIAN:
	{
	  DWORD arg = (arg_ulong_integer (4));
	  ((dword_data . bytes) [3]) = (arg & 0xFF);
	  ((dword_data . bytes) [2]) = ((arg >> 8) & 0xFF);
	  ((dword_data . bytes) [1]) = ((arg >> 16) & 0xFF);
	  ((dword_data . bytes) [0]) = ((arg >> 24) & 0xFF);
	}
	data_length = (sizeof (dword_data . bytes));
	data = (dword_data . bytes);
	break;
      case REG_SZ:
      case REG_EXPAND_SZ:
      case REG_MULTI_SZ:
	CHECK_ARG (4, STRING_P);
	data_length = ((STRING_LENGTH (ARG_REF (4))) + 1);
	data = ((BYTE *) (STRING_BYTE_PTR (ARG_REF (4))));
	break;
      default:
	CHECK_ARG (4, STRING_P);
	data_length = (STRING_LENGTH (ARG_REF (4)));
	data = ((BYTE *) (STRING_BYTE_PTR (ARG_REF (4))));
	break;
	break;
      }
    REGISTRY_API_CALL
      (RegSetValueEx, ((HKEY_ARG (1)), (SUBKEY_ARG (2)), 0,
		       data_type, data, data_length));
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("win32-delete-registry-value", Prim_win32_delete_registry_value, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  REGISTRY_API_CALL (RegDeleteValue, ((HKEY_ARG (1)), (SUBKEY_ARG (2))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("win32-delete-registry-key", Prim_win32_delete_registry_key, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  REGISTRY_API_CALL (RegDeleteKey, ((HKEY_ARG (1)), (SUBKEY_ARG (2))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("win32-enumerate-registry-key", Prim_win32_enumerate_registry_key, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  GUARANTEE_RESULT_SPACE ();
  CHECK_ARG (3, STRING_P);
  {
    DWORD buffer_size = ((STRING_LENGTH (ARG_REF (3))) + 1);
    FILETIME last_write_time;
    LONG code
      = (RegEnumKeyEx ((HKEY_ARG (1)),
		       ((DWORD) (arg_ulong_integer (2))),
		       (STRING_POINTER (ARG_REF (3))),
		       (&buffer_size),
		       0, 0, 0, (&last_write_time)));
    if (code == ERROR_NO_MORE_ITEMS)
      PRIMITIVE_RETURN (SHARP_F);
    if (code != ERROR_SUCCESS)
      NT_error_api_call (code, apicall_RegEnumKeyEx);
    PRIMITIVE_RETURN (ulong_to_integer (buffer_size));
  }
}

DEFINE_PRIMITIVE ("win32-query-info-registry-key", Prim_win32_query_info_registry_key, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  GUARANTEE_RESULT_SPACE ();
  {
    DWORD n_sub_keys;
    DWORD max_sub_key_length;
    DWORD n_values;
    DWORD max_value_name_length;
    DWORD max_value_length;
    REGISTRY_API_CALL
      (RegQueryInfoKey, ((HKEY_ARG (1)),
			 0, 0, 0,
			 (&n_sub_keys),
			 (&max_sub_key_length),
			 0,
			 (&n_values),
			 (&max_value_name_length),
			 (&max_value_length),
			 0, 0));
    /* Gratuitous incompatibility alert!  NT doesn't include the
       terminating zero in the length field; 95/98 does.  */
    if (NT_windows_type == wintype_95)
      max_sub_key_length -= 1;
    {
      SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 5, 1));
      VECTOR_SET (result, 0, (ulong_to_integer (n_sub_keys)));
      VECTOR_SET (result, 1, (ulong_to_integer (max_sub_key_length)));
      VECTOR_SET (result, 2, (ulong_to_integer (n_values)));
      VECTOR_SET (result, 3, (ulong_to_integer (max_value_name_length)));
      VECTOR_SET (result, 4, (ulong_to_integer (max_value_length)));
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("win32-enumerate-registry-value", Prim_win32_enumerate_registry_value, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  GUARANTEE_RESULT_SPACE ();
  CHECK_ARG (3, STRING_P);
  if ((ARG_REF (4)) != SHARP_F)
    CHECK_ARG (4, STRING_P);
  {
    DWORD name_size = ((STRING_LENGTH (ARG_REF (3))) + 1);
    DWORD data_type;
    DWORD data_size
      = (((ARG_REF (4)) == SHARP_F)
	 ? 0
	 : (STRING_LENGTH (ARG_REF (4))));
    LONG code
      = (RegEnumValue ((HKEY_ARG (1)),
		       ((DWORD) (arg_ulong_integer (2))),
		       ((LPTSTR) (STRING_POINTER (ARG_REF (3)))),
		       (&name_size),
		       0,
		       (&data_type),
		       (((ARG_REF (4)) == SHARP_F)
			? 0
			: ((LPBYTE) (STRING_POINTER (ARG_REF (4))))),
		       (&data_size)));
    if (code == ERROR_NO_MORE_ITEMS)
      PRIMITIVE_RETURN (SHARP_F);
    if (code != ERROR_SUCCESS)
      NT_error_api_call (code, apicall_RegEnumValue);
    {
      SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 3, 1));
      VECTOR_SET (result, 0, (ulong_to_integer (name_size)));
      VECTOR_SET (result, 1, (ulong_to_integer (data_type)));
      VECTOR_SET (result, 2, (ulong_to_integer (data_size)));
      PRIMITIVE_RETURN (result);
    }
  }
}

DEFINE_PRIMITIVE ("win32-query-info-registry-value", Prim_win32_query_info_registry_value, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  GUARANTEE_RESULT_SPACE ();
  {
    DWORD data_type;
    DWORD data_size;
    LONG code
      = (RegQueryValueEx ((HKEY_ARG (1)), (SUBKEY_ARG (2)), 0,
			  (&data_type), 0, (&data_size)));
    if (code == ERROR_FILE_NOT_FOUND)
      PRIMITIVE_RETURN (SHARP_F);
    if (code != ERROR_SUCCESS)
      NT_error_api_call (code, apicall_RegQueryValueEx);
    PRIMITIVE_RETURN
      (cons ((ulong_to_integer (data_type)),
	     (ulong_to_integer (data_size))));
  }
}

DEFINE_PRIMITIVE ("win32-query-registry-value", Prim_win32_query_registry_value, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  GUARANTEE_RESULT_SPACE ();
  {
    DWORD data_type;
    DWORD data_size;
    union
      {
	DWORD dword;
	BYTE bytes [4];
      } dword_converter;
    SCHEME_OBJECT result;
    BYTE * data;

    {
      LONG code
	= (RegQueryValueEx ((HKEY_ARG (1)), (SUBKEY_ARG (2)), 0,
			    (&data_type), 0, (&data_size)));
      if (code == ERROR_FILE_NOT_FOUND)
	PRIMITIVE_RETURN (SHARP_F);
      if (code != ERROR_SUCCESS)
	NT_error_api_call (code, apicall_RegQueryValueEx);
    }
    switch (data_type)
      {
      case REG_DWORD_LITTLE_ENDIAN:
      case REG_DWORD_BIG_ENDIAN:
	data = (& (dword_converter . bytes));
	break;

      case REG_SZ:
      case REG_EXPAND_SZ:
      case REG_MULTI_SZ:
	result = (allocate_string (data_size - 1));
	data = ((BYTE *) (STRING_BYTE_PTR (result)));
	break;

      default:
	result = (allocate_string (data_size));
	data = ((BYTE *) (STRING_BYTE_PTR (result)));
	break;
      }
    REGISTRY_API_CALL
      (RegQueryValueEx, ((HKEY_ARG (1)), (SUBKEY_ARG (2)), 0,
			 0, data, (&data_size)));
    switch (data_type)
      {
      case REG_DWORD_LITTLE_ENDIAN:
	result
	  = (ulong_to_integer
	     (((DWORD) ((dword_converter . bytes) [0]))
	      || (((DWORD) ((dword_converter . bytes) [1])) << 8)
	      || (((DWORD) ((dword_converter . bytes) [2])) << 16)
	      || (((DWORD) ((dword_converter . bytes) [3])) << 24)));
	break;
      case REG_DWORD_BIG_ENDIAN:
	result
	  = (ulong_to_integer
	     (((DWORD) ((dword_converter . bytes) [3]))
	      || (((DWORD) ((dword_converter . bytes) [2])) << 8)
	      || (((DWORD) ((dword_converter . bytes) [1])) << 16)
	      || (((DWORD) ((dword_converter . bytes) [0])) << 24)));
	break;
      }
    PRIMITIVE_RETURN (cons ((ulong_to_integer (data_type)), result));
  }
}

DEFINE_PRIMITIVE ("win32-expand-environment-strings", Prim_win32_expand_environment_strings, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  CHECK_ARG (2, STRING_P);
  {
    DWORD n_chars
      = (ExpandEnvironmentStrings
	 (((LPCTSTR) (STRING_POINTER (ARG_REF (1)))),
	  ((LPTSTR) (STRING_POINTER (ARG_REF (2)))),
	  ((STRING_LENGTH (ARG_REF (2))) + 1)));
    if (n_chars == 0)
      NT_error_api_call ((GetLastError ()), apicall_ExpandEnvironmentStrings);
    PRIMITIVE_RETURN (ulong_to_integer (n_chars - 1));
  }
}
