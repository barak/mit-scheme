/* -*-C-*-

$Id: pros2fs.c,v 1.3 1995/01/31 22:11:35 cph Exp $

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
#include "osfs.h"

extern FILESTATUS3 * OS2_read_file_status (const char *);
extern void OS2_write_file_status (const char *, FILESTATUS3 *);
extern char * OS2_drive_type (char);

#ifndef FILE_TOUCH_OPEN_TRIES
#define FILE_TOUCH_OPEN_TRIES 5
#endif

static SCHEME_OBJECT time_to_integer (FDATE *, FTIME *);
static void integer_to_time (SCHEME_OBJECT, FDATE *, FTIME *);
static SCHEME_OBJECT file_touch (const char *);
static void protect_handle (LHANDLE);

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
       : (LONG_TO_UNSIGNED_FIXNUM (info -> cbFile)));
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
    const char * s1 = (STRING_LOC ((ARG_REF (1)), 0));
    const char * s2 = (STRING_LOC ((ARG_REF (2)), 0));
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

DEFINE_PRIMITIVE ("FILE-TOUCH", Prim_file_touch, 1, 1,
  "Given a file name, change the times of the file to the current time.\n\
If the file does not exist, create it.\n\
Both the access time and modification time are changed.\n\
Return #F if the file existed and its time was modified.\n\
Otherwise the file did not exist and it was created.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (file_touch ((CONST char *) (STRING_ARG (1))));
}

static SCHEME_OBJECT
time_to_integer (FDATE * date, FTIME * time)
{
  unsigned long id;
  unsigned long it;
  id = (date -> year);
  id = ((id << 4) | (date -> month));
  id = ((id << 5) | (date -> day));
  it = (time -> hours);
  it = ((it << 6) | (time -> minutes));
  it = ((it << 5) | (time -> twosecs));
  return
    (integer_add ((integer_multiply ((LONG_TO_UNSIGNED_FIXNUM (id)),
				     (LONG_TO_UNSIGNED_FIXNUM (0x10000)))),
		  (LONG_TO_UNSIGNED_FIXNUM (it))));
}

static void
integer_to_time (SCHEME_OBJECT encoding, FDATE * date, FTIME * time)
{
  unsigned long id;
  unsigned long it;
  {
    SCHEME_OBJECT q;
    SCHEME_OBJECT r;
    (void) integer_divide
      (encoding, (LONG_TO_UNSIGNED_FIXNUM (0x10000)), (&q), (&r));
    it = (UNSIGNED_FIXNUM_TO_LONG (r));
    /* If encoding is larger than 32 bits, ignore MS bits.  */
    (void) integer_divide
      (q, (LONG_TO_UNSIGNED_FIXNUM (0x10000)), (&q), (&r));
    id = (UNSIGNED_FIXNUM_TO_LONG (r));
  }
  (date -> day) = (id & 0x1f);
  id >>= 5;
  (date -> month) = (id & 0x0f);
  id >>= 4;
  (date -> year) = id;
  (time -> twosecs) = (it & 0x1f);
  it >>= 5;
  (time -> minutes) = (it & 0x3f);
  it >>= 6;
  (time -> hours) = it;
}

static SCHEME_OBJECT
file_touch (const char * filename)
{
  HFILE handle;
  ULONG action;
  APIRET rc;
  unsigned int count = 0;

  transaction_begin ();
  while (1)
    {
      APIRET rc
	= (dos_open (((char *) filename),
		     (&handle),
		     (&action),
		     0,
		     FILE_NORMAL,
		     (OPEN_ACTION_OPEN_IF_EXISTS | OPEN_ACTION_CREATE_IF_NEW),
		     (OPEN_ACCESS_READWRITE | OPEN_SHARE_DENYREADWRITE),
		     0));
      if (rc == NO_ERROR)
	break;
      if ((rc != NO_ERROR)
	  && (rc != ERROR_FILE_NOT_FOUND)
	  && (rc != ERROR_PATH_NOT_FOUND)
	  && ((++ count) >= FILE_TOUCH_OPEN_TRIES))
	OS2_error_system_call (rc, syscall_dos_open);
    }
  protect_handle (handle);
  if (action == FILE_CREATED)
    {
      transaction_commit ();
      return (SHARP_T);
    }
  /* Existing file -- we'll write something to it to make sure that it
     has its times updated properly upon close.  This was needed for
     unix implementation, but it is not known whether it is needed in
     OS/2.  In any case, it does no harm to do this.  */
  {
    FILESTATUS3 info;
    char buffer [1];
    ULONG n;
    STD_API_CALL (dos_query_file_info,
		  (handle, FIL_STANDARD, (& info), (sizeof (info))));
    if ((info . cbFile) == 0)
      {
	/* Zero-length file: write a byte, then reset the length.  */
	(buffer[0]) = '\0';
	STD_API_CALL (dos_write, (handle, buffer, 1, (& n)));
	STD_API_CALL (dos_set_file_size, (handle, 0));
      }
    else
      {
	/* Read the first byte, then write it back in place.  */
	STD_API_CALL (dos_read, (handle, buffer, 1, (&n)));
	STD_API_CALL (dos_set_file_ptr, (handle, 0, FILE_BEGIN, (& n)));
	STD_API_CALL (dos_write, (handle, buffer, 1, (& n)));
      }
  }
  transaction_commit ();
  return (SHARP_F);
}

static void
protect_handle_1 (void * hp)
{
  (void) dos_close (* ((LHANDLE *) hp));
}

static void
protect_handle (LHANDLE h)
{
  LHANDLE * hp = (dstack_alloc (sizeof (LHANDLE)));
  (*hp) = h;
  transaction_record_action (tat_always, protect_handle_1, hp);
}

DEFINE_PRIMITIVE ("FILE-INFO", Prim_file_info, 1, 1,
  "Given a file name, return information about the file.\n\
If the file exists and its information is accessible,\n\
 the result is a vector of 6 items.\n\
Otherwise the result is #F.")
{
  FILESTATUS3 * info;
  SCHEME_OBJECT result;
  SCHEME_OBJECT modes;
  PRIMITIVE_HEADER (1);

  info = (OS2_read_file_status (STRING_ARG (1)));
  if (info == 0)
    PRIMITIVE_RETURN (SHARP_F);
  result = (allocate_marked_vector (TC_VECTOR, 6, true));
  modes = (allocate_string (5));
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
  VECTOR_SET (result, 4, (long_to_integer (info -> cbFile)));
  {
    unsigned int attr = (info -> attrFile);
    char * s = ((char *) (STRING_LOC (modes, 0)));
    (s[0]) = (((attr & FILE_DIRECTORY) != 0) ? 'd' : '-');
    (s[1]) = (((attr & FILE_READONLY)  != 0) ? 'r' : '-');
    (s[2]) = (((attr & FILE_HIDDEN)    != 0) ? 'h' : '-');
    (s[3]) = (((attr & FILE_SYSTEM)    != 0) ? 's' : '-');
    (s[4]) = (((attr & FILE_ARCHIVED)  != 0) ? 'a' : '-');
  }
  VECTOR_SET (result, 5, modes);
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
  PRIMITIVE_RETURN (long_to_integer (OS2_scheme_pid));
}
