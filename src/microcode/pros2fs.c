/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

#include "scheme.h"
#include "prims.h"
#include "os2.h"
#include "osfs.h"

extern FILESTATUS3 * OS2_read_file_status (const char *);
extern void OS2_write_file_status (const char *, FILESTATUS3 *);
extern char * OS2_drive_type (char);
extern long OS2_timezone (void);
extern long OS2_daylight_savings_p (void);
extern void OS_file_copy (const char *, const char *);

static SCHEME_OBJECT time_to_integer (FDATE *, FTIME *);
static void integer_to_time (SCHEME_OBJECT, FDATE *, FTIME *);

DEFINE_PRIMITIVE ("FILE-ATTRIBUTES", Prim_file_attributes, 1, 1,
  "Return attributes of FILE, as an integer.")
{
  PRIMITIVE_HEADER (1);
  {
    FILESTATUS3 * info = (OS2_read_file_status (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((info == 0)
       ? SHARP_F
       : (LONG_TO_UNSIGNED_FIXNUM (info -> attrFile)));
  }
}

DEFINE_PRIMITIVE ("SET-FILE-ATTRIBUTES!", Prim_set_file_attributes, 2, 2,
  "Set the attributes of FILE to ATTRIBUTES.")
{
  PRIMITIVE_HEADER (2);
  {
    FILESTATUS3 * info = (OS2_read_file_status (STRING_ARG (1)));
    if (info == 0)
      error_bad_range_arg (1);
    (info -> attrFile) = (arg_index_integer (2, 0x10000));
    OS2_write_file_status ((STRING_ARG (1)), info);
    PRIMITIVE_RETURN (UNSPECIFIC);
  }
}

DEFINE_PRIMITIVE ("FILE-LENGTH", Prim_file_length, 1, 1,
  "Return attributes of FILE, as an integer.")
{
  PRIMITIVE_HEADER (1);
  {
    FILESTATUS3 * info = (OS2_read_file_status (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((info == 0)
       ? SHARP_F
       : (ulong_to_integer (info -> cbFile)));
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
    PSZ result;
    XTD_API_CALL
      (dos_scan_env, ((STRING_ARG (1)), (& result)),
       {
	 if (rc == ERROR_ENVVAR_NOT_FOUND)
	   PRIMITIVE_RETURN (SHARP_F);
       });
    PRIMITIVE_RETURN (char_pointer_to_string (result));
  }
}

DEFINE_PRIMITIVE ("FILE-EQ?", Prim_file_eq_p, 2, 2,
  "True iff the two file arguments are the same file.")
{
  PRIMITIVE_HEADER (2);
  CHECK_ARG (1, STRING_P);
  CHECK_ARG (2, STRING_P);
  {
    unsigned long length = (STRING_LENGTH (ARG_REF (1)));
    const char * s1 = (STRING_POINTER (ARG_REF (1)));
    const char * s2 = (STRING_POINTER (ARG_REF (2)));
    const char * e1 = (s1 + length);
    if ((STRING_LENGTH (ARG_REF (2))) != length)
      PRIMITIVE_RETURN (SHARP_F);
    while (s1 < e1)
      if ((char_upcase (*s1++)) != (char_upcase (*s2++)))
	PRIMITIVE_RETURN (SHARP_F);
    PRIMITIVE_RETURN (SHARP_T);
  }
}

DEFINE_PRIMITIVE ("FILE-MOD-TIME", Prim_file_mod_time, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    FILESTATUS3 * info = (OS2_read_file_status (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((info == 0)
       ? SHARP_F
       : (time_to_integer ((& (info -> fdateLastWrite)),
			   (& (info -> ftimeLastWrite)))));
  }
}

DEFINE_PRIMITIVE ("FILE-ACCESS-TIME", Prim_file_acc_time, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    FILESTATUS3 * info = (OS2_read_file_status (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((info == 0)
       ? SHARP_F
       : (time_to_integer ((& (info -> fdateLastAccess)),
			   (& (info -> ftimeLastAccess)))));
  }
}

static SCHEME_OBJECT
time_to_integer (FDATE * date, FTIME * time)
{
  unsigned long accum;
  accum = (date -> year);
  accum = ((accum << 4) | (date -> month));
  accum = ((accum << 5) | (date -> day));
  accum = ((accum << 5) | (time -> hours));
  accum = ((accum << 6) | (time -> minutes));
  accum = ((accum << 5) | (time -> twosecs));
  return (ulong_to_integer (accum));
}

DEFINE_PRIMITIVE ("SET-FILE-TIMES!", Prim_set_file_times, 3, 3,
  "Change the access and modification times of FILE.\n\
The second and third arguments are the respective times.\n\
The file must exist and you must be the owner (or superuser).")
{
  PRIMITIVE_HEADER (3);
  {
    FILESTATUS3 * info = (OS2_read_file_status (STRING_ARG (1)));
    SCHEME_OBJECT atime = (ARG_REF (2));
    SCHEME_OBJECT mtime = (ARG_REF (3));
    if (info == 0)
      error_bad_range_arg (1);
    if (atime != SHARP_F)
      {
	if (!INTEGER_P (atime))
	  error_wrong_type_arg (2);
	if (integer_negative_p (atime))
	  error_bad_range_arg (2);
	integer_to_time (atime,
			 (& (info -> fdateLastAccess)),
			 (& (info -> ftimeLastAccess)));
      }
    if (mtime != SHARP_F)
      {
	if (!INTEGER_P (mtime))
	  error_wrong_type_arg (3);
	if (integer_negative_p (mtime))
	  error_bad_range_arg (3);
	integer_to_time (mtime,
			 (& (info -> fdateLastWrite)),
			 (& (info -> ftimeLastWrite)));
      }
    OS2_write_file_status ((STRING_ARG (1)), info);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
integer_to_time (SCHEME_OBJECT encoding, FDATE * date, FTIME * time)
{
  unsigned long accum = (integer_to_ulong (encoding));
  (time -> twosecs) = (accum & 0x1f);
  accum >>= 5;
  (time -> minutes) = (accum & 0x3f);
  accum >>= 6;
  (time -> hours) = (accum & 0x1f);
  accum >>= 5;
  (date -> day) = (accum & 0x1f);
  accum >>= 5;
  (date -> month) = (accum & 0x0f);
  accum >>= 4;
  (date -> year) = accum;
}

DEFINE_PRIMITIVE ("FILE-INFO", Prim_file_info, 1, 1,
  "Given a file name, return information about the file.\n\
If the file exists and its information is accessible,\n\
 the result is a vector of 6 items.\n\
Otherwise the result is #F.")
{
  FILESTATUS3 * info;
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (1);

  info = (OS2_read_file_status (STRING_ARG (1)));
  if (info == 0)
    PRIMITIVE_RETURN (SHARP_F);
  result = (allocate_marked_vector (TC_VECTOR, 8, true));
  VECTOR_SET (result, 0,
	      ((((info -> attrFile) & FILE_DIRECTORY) != 0)
	       ? SHARP_T
	       : SHARP_F));
  VECTOR_SET (result, 1,
	      (time_to_integer ((& (info -> fdateLastAccess)),
				(& (info -> ftimeLastAccess)))));
  VECTOR_SET (result, 2,
	      (time_to_integer ((& (info -> fdateLastWrite)),
				(& (info -> ftimeLastWrite)))));
  VECTOR_SET (result, 3,
	      (time_to_integer ((& (info -> fdateCreation)),
				(& (info -> ftimeCreation)))));
  VECTOR_SET (result, 4, (ulong_to_integer (info -> cbFile)));
  {
    unsigned int attr = (info -> attrFile);
    SCHEME_OBJECT modes = (allocate_string (5));
    char * s = (STRING_POINTER (modes));
    (s[0]) = (((attr & FILE_DIRECTORY) != 0) ? 'd' : '-');
    (s[1]) = (((attr & FILE_READONLY)  != 0) ? 'r' : '-');
    (s[2]) = (((attr & FILE_HIDDEN)    != 0) ? 'h' : '-');
    (s[3]) = (((attr & FILE_SYSTEM)    != 0) ? 's' : '-');
    (s[4]) = (((attr & FILE_ARCHIVED)  != 0) ? 'a' : '-');
    VECTOR_SET (result, 5, modes);
    VECTOR_SET (result, 6, (ulong_to_integer (attr)));
  }
  VECTOR_SET (result, 7, (ulong_to_integer (info -> cbFileAlloc)));
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("DRIVE-TYPE", Prim_drive_type, 1, 1, 0)
{
  SCHEME_OBJECT arg;
  char * type;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  arg = (ARG_REF (1));
  if (! (((STRING_LENGTH (arg)) == 1) && (isalpha (STRING_REF (arg, 0)))))
    error_bad_range_arg (1);
  type = (OS2_drive_type (STRING_REF (arg, 0)));
  PRIMITIVE_RETURN (char_pointer_to_string ((type == 0) ? "unknown" : type));
}

DEFINE_PRIMITIVE ("CURRENT-PID", Prim_current_pid, 0, 0,
  "Return Scheme's PID.")
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (ulong_to_integer (OS2_scheme_pid));
}

DEFINE_PRIMITIVE ("DOS-QUERY-MEMORY", Prim_dos_query_memory, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  {
    ULONG start = (arg_ulong_integer (1));
    ULONG length = (arg_ulong_integer (2));
    ULONG flags;
    XTD_API_CALL
      (dos_query_mem, (((PVOID) start), (&length), (&flags)),
       {
	 if (rc == ERROR_INVALID_ADDRESS)
	   PRIMITIVE_RETURN (SHARP_F);
       });
    PRIMITIVE_RETURN (cons ((ulong_to_integer (length)),
			    (ulong_to_integer (flags))));
  }
}

DEFINE_PRIMITIVE ("OS2-TIME-ZONE", Prim_OS2_timezone, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (long_to_integer (OS2_timezone ()));
}

DEFINE_PRIMITIVE ("OS2-DAYLIGHT-SAVINGS-TIME?", Prim_OS2_dst_p, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (OS2_daylight_savings_p ()));
}

DEFINE_PRIMITIVE ("OS2-COPY-FILE", Prim_OS2_copy_file, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS_file_copy ((STRING_ARG (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("OS2-SET-REL-MAX-FH", Prim_OS2_set_rel_max_fh, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    LONG req_max_fh = (arg_integer (1));
    ULONG current_max_fh;
    STD_API_CALL (dos_set_rel_max_fh, ((&req_max_fh), (&current_max_fh)));
    PRIMITIVE_RETURN (ulong_to_integer (current_max_fh));
  }
}
