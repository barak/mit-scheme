/* -*-C-*-

$Id: ntfs.c,v 1.9 1995/10/28 01:03:40 cph Exp $

Copyright (c) 1992-95 Massachusetts Institute of Technology

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

#include "nt.h"
#include "osfs.h"
#include <string.h>
#include "outf.h"

int
DEFUN (NT_read_file_status, (name, s),
       CONST char * name AND
       struct stat * s)
{ char filename[128];

  nt_pathname_as_filename (name, filename);

  while ((stat (filename, s)) < 0)
    {
      if (errno == EINTR)
	continue;
      if ((errno == ENOENT) || (errno == ENOTDIR))
	return (0);
      error_system_call (errno, syscall_lstat);
    }
  return (1);
}

enum file_existence
DEFUN (OS_file_existence_test, (name), char * name)
{
  struct stat s;
  char filename[128];

  nt_pathname_as_filename(name, filename);

  return
    (((NT_stat (filename, (&s))) < 0)
     ? file_doesnt_exist : file_does_exist);
}

int
DEFUN (OS_file_access, (name, mode), CONST char * name AND unsigned int mode)
{
  char filename[128];

  nt_pathname_as_filename (name, filename);
  return ((NT_access (filename, mode)) == 0);
}

int
DEFUN (OS_file_directory_p, (name), char * name)
{
  struct stat s;
  char filename[128];

  nt_pathname_as_filename(name, filename);
  return (((NT_stat (filename, (&s))) == 0) &&
	  (((s . st_mode) & S_IFMT) == S_IFDIR));
}

CONST char *
DEFUN (OS_file_soft_link_p, (name), CONST char * name)
{
  return (0);
}

static void
DEFUN (guarantee_writable, (name, errorp),
       CONST char * name AND
       int errorp)
{
  DWORD attributes = (GetFileAttributes (name));
  if (attributes == 0xFFFFFFFF)
    {
      DWORD error_code = (GetLastError ());
      if ((error_code != ERROR_FILE_NOT_FOUND) && errorp)
	error_system_call (error_code, syscall_stat);
    }
  else if ((attributes & FILE_ATTRIBUTE_READONLY) != 0)
    {
      if ((! (SetFileAttributes (name,
				 (attributes &~ FILE_ATTRIBUTE_READONLY))))
	  && errorp)
	error_system_call ((GetLastError ()), syscall_chmod);
    }
}

void
DEFUN (OS_file_remove, (name), CONST char * name)
{
  guarantee_writable (name, 1);
  STD_VOID_SYSTEM_CALL (syscall_unlink, (NT_unlink (name)));
}

void
DEFUN (OS_file_remove_link, (name), CONST char * name)
{
  struct stat s;
  if ((NT_stat (name, (&s)) == 0)
      && (((s . st_mode) & S_IFMT) == S_IFREG))
    {
      guarantee_writable (name, 0);
      NT_unlink (name);
    }
}

void
DEFUN (OS_file_rename, (from, to),
       CONST char * from AND
       CONST char * to)
{
  guarantee_writable (name, 1);
  STD_BOOL_SYSTEM_CALL (syscall_rename, (MoveFile (from, to)));
}

void
DEFUN (OS_file_copy, (from, to),
       CONST char * from AND
       CONST char * to)
{
  guarantee_writable (name, 1);
  /* This system-call name is wrong, but there's no corresponding unix
     operation, and I don't feel like customizing this for NT now.  */
  STD_BOOL_SYSTEM_CALL (syscall_rename, (CopyFile (from, to, FALSE)));
}

void
DEFUN (OS_file_link_hard, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  error_unimplemented_primitive ();
}

void
DEFUN (OS_file_link_soft, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  error_unimplemented_primitive ();
}

void
DEFUN (OS_directory_make, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_mkdir, (NT_mkdir (name)));
}

void
DEFUN (OS_directory_delete, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_rmdir, (NT_rmdir (name)));
}

typedef struct nt_dir_struct
{
  WIN32_FIND_DATA entry;
  HANDLE handle;         /* may be DIR_UNALLOCATED */
  BOOL more;
  char pathname[256];
} nt_dir;

#define GET_DIRECTORY_ENTRY_NAME(entry, pathname)		\
  (strcpy(pathname, (entry).cFileName), strlwr(pathname))

static nt_dir ** directory_pointers;
static unsigned int n_directory_pointers;

void
DEFUN_VOID (NT_initialize_directory_reader)
{
  directory_pointers = 0;
  n_directory_pointers = 0;
}

static unsigned int
DEFUN (allocate_directory_pointer, (pointer), nt_dir * pointer)
{
  if (n_directory_pointers == 0)
    {
      nt_dir ** pointers = ((nt_dir **) (NT_malloc ((sizeof (nt_dir *)) * 4)));
      if (pointers == 0)
	error_system_call (ENOMEM, syscall_malloc);
      directory_pointers = pointers;
      n_directory_pointers = 4;
      {
	nt_dir ** scan = directory_pointers;
	nt_dir ** end = (scan + n_directory_pointers);
	(*scan++) = pointer;
	while (scan < end)
	  (*scan++) = 0;
      }
      return (0);
    }
  {
    nt_dir ** scan = directory_pointers;
    nt_dir ** end = (scan + n_directory_pointers);
    while (scan < end)
      if ((*scan++) == 0)
	{
	  (*--scan) = pointer;
	  return (scan - directory_pointers);
	}
  }
  {
    unsigned int result = n_directory_pointers;
    unsigned int n_pointers = (2 * n_directory_pointers);
    nt_dir ** pointers =
      ((nt_dir **)
       (NT_realloc (((PTR) directory_pointers),
		    ((sizeof (nt_dir *)) * n_pointers))));
    if (pointers == 0)
      error_system_call (ENOMEM, syscall_realloc);
    {
      nt_dir ** scan = (pointers + result);
      nt_dir ** end = (pointers + n_pointers);
      (*scan++) = pointer;
      while (scan < end)
	(*scan++) = 0;
    }
    directory_pointers = pointers;
    n_directory_pointers = n_pointers;
    return (result);
  }
}

#define REFERENCE_DIRECTORY(index) (directory_pointers[(index)])
#define DEALLOCATE_DIRECTORY(index) ((directory_pointers[(index)]) = 0)

int
DEFUN (OS_directory_valid_p, (index), long index)
{
  return
    ((0 <= index)
     && (index < (long) n_directory_pointers)
     && ((REFERENCE_DIRECTORY (index)) != 0));
}

unsigned int
DEFUN (OS_directory_open, (name), CONST char * name)
{
  char filename[128], searchname[128];
  nt_dir * dir = NT_malloc(sizeof(nt_dir));

  if (dir == 0)
    error_system_call (ENOMEM, syscall_malloc);

  if (nt_pathname_as_filename (name, filename))
    sprintf (searchname, "%s*.*", filename);
  else
    sprintf (searchname, "%s\\*.*", filename);

  dir->handle = FindFirstFile(searchname, &(dir->entry));
  if (dir->handle == INVALID_HANDLE_VALUE)
    error_system_call (errno, syscall_opendir);

  dir->more = TRUE;
  return (allocate_directory_pointer (dir));
}

CONST char *
DEFUN (OS_directory_read, (index), unsigned int index)
{
  nt_dir * dir = REFERENCE_DIRECTORY (index);

  if (dir == 0 || !dir->more)
    return 0;

  GET_DIRECTORY_ENTRY_NAME(dir->entry, dir->pathname);
  dir->more = FindNextFile(dir->handle, &(dir->entry));
  return (dir -> pathname);
}

CONST char *
DEFUN (OS_directory_read_matching, (index, prefix),
       unsigned int index AND
       CONST char * prefix)
{
  error_unimplemented_primitive ();
  return (0);
}

void
DEFUN (OS_directory_close, (index), unsigned int index)
{ 
  nt_dir * dir = REFERENCE_DIRECTORY (index);

  if (dir)
  {
    FindClose(dir->handle);
    NT_free(dir);
  }
  DEALLOCATE_DIRECTORY (index);
}
