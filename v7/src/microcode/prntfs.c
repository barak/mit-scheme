/* -*-C-*-

$Id: prntfs.c,v 1.7 1996/10/07 17:54:58 cph Exp $

Copyright (c) 1993-96 Massachusetts Institute of Technology

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

/* Unix-specific file-system primitives. */
/* DOS Imitation */

#include "scheme.h"
#include "prims.h"
#include "nt.h"
#include "osfs.h"

#include <sys/utime.h>
#include <memory.h>
#include <math.h>

extern void EXFUN (OS_file_copy, (CONST char *, CONST char *));

static SCHEME_OBJECT file_attributes_internal
  (DWORD, FILETIME *, FILETIME *, FILETIME *, DWORD, DWORD);
static SCHEME_OBJECT EXFUN (file_touch, (CONST char * filename));
static void EXFUN (protect_fd, (int fd));

#ifndef FILE_TOUCH_OPEN_TRIES
#define FILE_TOUCH_OPEN_TRIES 5
#endif

struct file_info
{
  DWORD attributes;
  FILETIME atime;
  FILETIME mtime;
  FILETIME ctime;
  DWORD llength;
  DWORD hlength;
};

static HANDLE create_file_for_info (const char *);
static void close_file_handle (HANDLE);

static int
get_file_info (const char * namestring, struct file_info * info)
{
  HANDLE hfile = INVALID_HANDLE_VALUE;
  memset (info, 0, (sizeof (*info)));
  (info -> attributes) = (GetFileAttributes (namestring));
  if ((info -> attributes) == 0xFFFFFFFF)
    goto error_return;
  hfile = (create_file_for_info (namestring));
  if (hfile == INVALID_HANDLE_VALUE)
    return (0);
  if (!GetFileTime (hfile,
		    (& (info -> ctime)),
		    (& (info -> atime)),
		    (& (info -> mtime))))
    goto error_return;
  (info -> llength) = (GetFileSize (hfile, (& (info -> hlength))));
  if ((info -> llength) == 0xFFFFFFFF)
    goto error_return;
  close_file_handle (hfile);
  return (1);
 error_return:
  {
    DWORD code = (GetLastError ());
    if (hfile != INVALID_HANDLE_VALUE)
      (void) CloseHandle (hfile);
    if (! ((code == ERROR_FILE_NOT_FOUND) || (code == ERROR_PATH_NOT_FOUND)))
      error_system_call (code, syscall_lstat);
    return (0);
  }
}

static HANDLE
create_file_for_info (const char * namestring)
{
  HANDLE hfile
    = (CreateFile (namestring,
		   0,
		   (FILE_SHARE_READ | FILE_SHARE_WRITE),
		   0,
		   OPEN_EXISTING,
		   FILE_FLAG_BACKUP_SEMANTICS,
		   NULL));
  if (hfile == INVALID_HANDLE_VALUE)
    {
      DWORD code = (GetLastError ());
      if (! ((code == ERROR_FILE_NOT_FOUND) || (code == ERROR_PATH_NOT_FOUND)))
	error_system_call (code, syscall_open);
    }
  return (hfile);
}

static void
close_file_handle (HANDLE hfile)
{
  if (!CloseHandle (hfile))
    error_system_call ((GetLastError ()), syscall_close);
}

static double ut_zero = 0.0;

static void
initialize_ut_zero (void)
{
  if (ut_zero == 0.0)
    {
      SYSTEMTIME st;
      FILETIME ft;
      (st . wYear) = 1970;
      (st . wMonth) = 1;
      (st . wDay) = 1;
      (st . wHour) = 0;
      (st . wMinute) = 0;
      (st . wSecond) = 0;
      (st . wMilliseconds) = 0;
      (void) SystemTimeToFileTime ((&st), (&ft));
      ut_zero
	= ((((double) (ft . dwHighDateTime)) * 4294967296.)
	   + ((double) (ft . dwLowDateTime)));
    }
}

unsigned long
file_time_to_unix_time (FILETIME * ft)
{
  double fd
    = ((((double) (ft -> dwHighDateTime)) * 4294967296.)
       + ((double) (ft -> dwLowDateTime)));
  initialize_ut_zero ();
  if (fd <= ut_zero)
    return (0);
  return ((unsigned long) (floor (((fd - ut_zero) + 5000000.) / 10000000.)));
}

void
unix_time_to_file_time (unsigned long ut, FILETIME * ft)
{
  double ud = (((double) ut) * 10000000.);
  double udh = (floor (ud / 4294967296.));
  (ft -> dwHighDateTime) = ((DWORD) udh);
  (ft -> dwLowDateTime) = ((DWORD) (ud -(udh * 4294967296.)));
}

DEFINE_PRIMITIVE ("FILE-MODES", Prim_file_modes, 1, 1,
  "Return mode bits of FILE, as an integer.")
{
  struct file_info info;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    ((get_file_info ((STRING_ARG (1)), (&info)))
     ? (LONG_TO_UNSIGNED_FIXNUM (info . attributes))
     : SHARP_F);
}

DEFINE_PRIMITIVE ("SET-FILE-MODES!", Prim_set_file_modes, 2, 2,
  "Set the mode bits of FILE to MODE.")
{
  PRIMITIVE_HEADER (2);
  if (!SetFileAttributes ((STRING_ARG (1)), (arg_ulong_integer (2))))
    error_system_call ((GetLastError ()), syscall_chmod);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FILE-MOD-TIME", Prim_file_mod_time, 1, 1, 0)
{
  struct file_info info;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    ((get_file_info ((STRING_ARG (1)), (&info)))
     ? (ulong_to_integer (file_time_to_unix_time (& (info . mtime))))
     : SHARP_F);
}

DEFINE_PRIMITIVE ("SET-FILE-TIMES!", Prim_set_file_times, 3, 3,
  "Change the access and modification times of FILE.\n\
The second and third arguments are the respective times.\n\
The file must exist and you must be the owner.")
{
  HANDLE hfile;
  FILETIME atime;
  FILETIME mtime;
  PRIMITIVE_HEADER (3);

  hfile
    = (CreateFile ((STRING_ARG (1)),
		   GENERIC_WRITE,
		   FILE_SHARE_READ,
		   0,
		   OPEN_EXISTING,
		   FILE_ATTRIBUTE_NORMAL,
		   NULL));
  if (hfile == INVALID_HANDLE_VALUE)
    error_system_call ((GetLastError ()), syscall_open);
  unix_time_to_file_time ((arg_ulong_integer (2)), (&atime));
  unix_time_to_file_time ((arg_ulong_integer (3)), (&mtime));
  if (!SetFileTime (hfile, 0, (&atime), (&mtime)))
    {
      DWORD code = (GetLastError ());
      (void) CloseHandle (hfile);
      error_system_call (code, syscall_utime);
    }
  close_file_handle (hfile);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Returns a vector of 10 items:

   0 = #T iff the file is a directory,
       string (name linked to) for symbolic link,
       #F for all other files.
   1 = number of links to the file
   2 = user id, as an unsigned integer
   3 = group id, as an unsigned integer
   4 = last access time of the file
   5 = last modification time of the file
   6 = last change time of the file
   7 = size of the file in bytes
   8 = mode string for the file
   9 = inode number of the file

   The file_mode_string stuff was gobbled from GNU Emacs. */

DEFINE_PRIMITIVE ("FILE-ATTRIBUTES", Prim_file_attributes, 1, 1,
  "Given a file name, return attribute information about the file.\n\
If the file exists and its status information is accessible, the result\n\
is a vector of 10 items (see the reference manual for details).  Otherwise\n\
the result is #F.")
{
  struct file_info info;
  SCHEME_OBJECT result;
  SCHEME_OBJECT modes;
  PRIMITIVE_HEADER (1);

  if (!get_file_info ((STRING_ARG (1)), (&info)))
    PRIMITIVE_RETURN (SHARP_F);
  result = (allocate_marked_vector (TC_VECTOR, 10, true));
  modes = (allocate_string (6));
  VECTOR_SET (result, 0,
	      (BOOLEAN_TO_OBJECT
	       ((info . attributes) & FILE_ATTRIBUTE_DIRECTORY)));
  VECTOR_SET (result, 1, (ulong_to_integer (1)));
  VECTOR_SET (result, 2, (ulong_to_integer (0)));
  VECTOR_SET (result, 3, (ulong_to_integer (0)));
#define STORE_FILE_TIME(index, name)					\
  VECTOR_SET (result, (index),						\
	      (ulong_to_integer						\
	       (file_time_to_unix_time (& (info . name)))))
  STORE_FILE_TIME(4, atime);
  STORE_FILE_TIME(5, mtime);
  STORE_FILE_TIME(6, ctime);
#undef STORE_FILE_TIME
  {
    SCHEME_OBJECT l = (ulong_to_integer (info . llength));
    if ((info . hlength) != 0)
      l = (integer_add
	   ((integer_multiply
	     ((ulong_to_integer (info . hlength)),
	      (integer_add_1 (ulong_to_integer (0xFFFFFFFF))))),
	    l));
    VECTOR_SET (result, 7, l);
  }
#define ATTRIBUTE_LETTER(index, mask, letter)				\
  STRING_SET (modes,							\
	      (index),							\
	      (((info . attributes) & (mask)) ? (letter) : '-'))
  ATTRIBUTE_LETTER (0, FILE_ATTRIBUTE_DIRECTORY, 'd');
  ATTRIBUTE_LETTER (1, FILE_ATTRIBUTE_READONLY, 'r');
  ATTRIBUTE_LETTER (2, FILE_ATTRIBUTE_HIDDEN, 'h');
  ATTRIBUTE_LETTER (3, FILE_ATTRIBUTE_SYSTEM, 's');
  ATTRIBUTE_LETTER (4, FILE_ATTRIBUTE_ARCHIVE, 'a');
  ATTRIBUTE_LETTER (5, FILE_ATTRIBUTE_COMPRESSED, 'c');
#undef ATTRIBUTE_LETTER
  VECTOR_SET (result, 8, modes);
  VECTOR_SET (result, 9, (ulong_to_integer (0)));
  PRIMITIVE_RETURN (result);
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
DEFUN (file_touch, (filename), CONST char * filename)
{
  int fd;
  transaction_begin ();
  {
    unsigned int count = 0;
    while (1)
      {
	count += 1;
	/* Use O_EXCL to prevent overwriting existing file. */
	fd = (NT_open (filename, (O_RDWR | O_CREAT | O_EXCL), MODE_REG));
	if (fd >= 0)
	  {
	    protect_fd (fd);
	    transaction_commit ();
	    return (SHARP_T);
	  }
	if (errno == EEXIST)
	  {
	    fd = (NT_open (filename, O_RDWR, MODE_REG));
	    if (fd >= 0)
	      {
		protect_fd (fd);
		break;
	      }
	    else if (errno == ENOENT)
	      continue;
	  }
	if (count >= FILE_TOUCH_OPEN_TRIES)
	  error_system_call (errno, syscall_open);
      }
  }
  {
    struct stat file_status;
    STD_VOID_SYSTEM_CALL (syscall_fstat, (NT_fstat (fd, (&file_status))));
    if (((file_status . st_mode) & S_IFMT) != S_IFREG)
      error_bad_range_arg (1);
    /* CASE 3: file length of 0 needs special treatment. */
    if ((file_status . st_size) == 0)
     {
	char buf [1];
	(buf[0]) = '\0';
	STD_VOID_SYSTEM_CALL (syscall_write, (NT_write (fd, buf, 1)));
#ifdef HAVE_TRUNCATE
	STD_VOID_SYSTEM_CALL (syscall_ftruncate, (NT_ftruncate (fd, 0)));
	transaction_commit ();
#else /* not HAVE_TRUNCATE */
	transaction_commit ();
	fd = (NT_open (filename, (O_WRONLY | O_TRUNC), MODE_REG));
	if (fd >= 0)
	  STD_VOID_SYSTEM_CALL (syscall_close, (NT_close (fd)));
#endif /* HAVE_TRUNCATE */
	return (SHARP_F);
      }
  }
  /* CASE 4: read, then write back the first byte in the file. */
  {
    char buf [1];
    int scr;
    STD_UINT_SYSTEM_CALL (syscall_read, scr, (NT_read (fd, buf, 1)));
    if (scr > 0)
      {
	STD_VOID_SYSTEM_CALL (syscall_lseek, (NT_lseek (fd, 0, SEEK_SET)));
	STD_VOID_SYSTEM_CALL (syscall_write, (NT_write (fd, buf, 1)));
      }
  }
  transaction_commit ();
  return (SHARP_F);
}

static void
DEFUN (protect_fd_close, (ap), PTR ap)
{
  NT_close (* ((int *) ap));
  return;
}

static void
DEFUN (protect_fd, (fd), int fd)
{
  int * p = (dstack_alloc (sizeof (int)));
  (*p) = fd;
  transaction_record_action (tat_always, protect_fd_close, p);
}

DEFINE_PRIMITIVE ("FILE-EQ?", Prim_file_eq_p, 2, 2,
  "True iff the two file arguments are the same file.")
{
  static char buf1[128], buf2[128];
  char *filepart;
  PRIMITIVE_HEADER (2);

  if (GetFullPathName(STRING_ARG (1), 128, buf1, &filepart) == 0  ||
      GetFullPathName(STRING_ARG (2), 128, buf2, &filepart) == 0)
    error_external_return ();
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT ((strcmp (&buf1[0], &buf2[0])) == 0));
}

DEFINE_PRIMITIVE ("NT-GET-VOLUME-INFORMATION", Prim_NT_get_vol_info, 1, 1, 0)
{
  char name [256];
  DWORD serial_number;
  DWORD max_component_length;
  DWORD file_system_flags;
  char file_system_name [256];
  SCHEME_OBJECT result;
  PRIMITIVE_HEADER (1);

  if (! (GetVolumeInformation ((STRING_ARG (1)),
			       name,
			       (sizeof (name)),
			       (&serial_number),
			       (&max_component_length),
			       (&file_system_flags),
			       file_system_name,
			       (sizeof (file_system_name)))))
    PRIMITIVE_RETURN (SHARP_F);
  result = (allocate_marked_vector (TC_VECTOR, 5, 1));
  VECTOR_SET (result, 0, (char_pointer_to_string (name)));
  VECTOR_SET (result, 1, (ulong_to_integer (serial_number)));
  VECTOR_SET (result, 2, (ulong_to_integer (max_component_length)));
  VECTOR_SET (result, 3, (ulong_to_integer (file_system_flags)));
  VECTOR_SET (result, 4, (char_pointer_to_string (file_system_name)));
  PRIMITIVE_RETURN (result);
}

DEFINE_PRIMITIVE ("NT-COPY-FILE", Prim_NT_copy_file, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  OS_file_copy ((STRING_ARG (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("NT-GET-FILE-ATTRIBUTES", Prim_NT_get_file_attributes, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    CONST char * filename = (STRING_ARG (1));
    DWORD attributes = (GetFileAttributes (filename));
    if (attributes == 0xFFFFFFFF)
      {
	DWORD error_code = (GetLastError ());
	if (! ((error_code == ERROR_FILE_NOT_FOUND)
	       || (error_code == ERROR_PATH_NOT_FOUND)))
	  error_system_call (error_code, syscall_stat);
	PRIMITIVE_RETURN (SHARP_F);
      }
    PRIMITIVE_RETURN (ulong_to_integer (attributes));
  }
}

DEFINE_PRIMITIVE ("NT-SET-FILE-ATTRIBUTES", Prim_NT_set_file_attributes, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  STD_BOOL_SYSTEM_CALL
    (syscall_chmod,
     (SetFileAttributes ((STRING_ARG (1)), (arg_ulong_integer (2)))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
