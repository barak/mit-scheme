/* -*-C-*-

$Id: ntfs.c,v 1.6 1993/08/21 03:21:47 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

void
DEFUN (OS_file_remove, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_unlink, (NT_unlink (name)));
}

void
DEFUN (OS_file_remove_link, (name), CONST char * name)
{
  struct stat s;
  if ( (NT_stat (name, (&s)) == 0) &&
       (((s . st_mode) & S_IFMT) == S_IFREG) )
   NT_unlink (name);
  return;
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
DEFUN (OS_file_rename, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  if ((NT_rename (from_name, to_name)) != 0)
    error_system_call (errno, syscall_rename);
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

#define DIR_UNALLOCATED (-1L)

/* This is used to cache the result of _findfirst because directory open
   does not return the first file.
 */

typedef struct DIR_struct
{
  struct _finddata_t entry;
  long handle;         /* may be DIR_UNALLOCATED */
  char pathname[256];
} DIR;

#define Get_Directory_Entry_Name(entry, pathname)		\
  (strcpy(pathname, (entry).name), strlwr(pathname))

int OS_directory_index;

static DIR ** directory_pointers;
static unsigned int n_directory_pointers;

void
DEFUN_VOID (NT_initialize_directory_reader)
{
  directory_pointers = 0;
  n_directory_pointers = 0;
  OS_directory_index = (-1);
}

static unsigned int
DEFUN (allocate_directory_pointer, (pointer), DIR * pointer)
{
  if (n_directory_pointers == 0)
    {
      DIR ** pointers = ((DIR **) (NT_malloc ((sizeof (DIR *)) * 4)));
      if (pointers == 0)
	error_system_call (ENOMEM, syscall_malloc);
      directory_pointers = pointers;
      n_directory_pointers = 4;
      {
	DIR ** scan = directory_pointers;
	DIR ** end = (scan + n_directory_pointers);
	(*scan++) = pointer;
	while (scan < end)
	  (*scan++) = 0;
      }
      return (0);
    }
  {
    DIR ** scan = directory_pointers;
    DIR ** end = (scan + n_directory_pointers);
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
    DIR ** pointers =
      ((DIR **)
       (NT_realloc (((PTR) directory_pointers),
		    ((sizeof (DIR *)) * n_pointers))));
    if (pointers == 0)
      error_system_call (ENOMEM, syscall_realloc);
    {
      DIR ** scan = (pointers + result);
      DIR ** end = (pointers + n_pointers);
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
     && (index < n_directory_pointers)
     && ((REFERENCE_DIRECTORY (index)) != 0));
}

unsigned int
DEFUN (OS_directory_open, (name), CONST char * name)
{
  char filename[128], searchname[128];
  DIR * dir = malloc(sizeof(DIR));

  if (dir == 0)
    error_system_call (ENOMEM, syscall_malloc);

  if (nt_pathname_as_filename (name, filename))
    sprintf (searchname, "%s*.*", filename);
  else
    sprintf (searchname, "%s\\*.*", filename);

  dir->handle = _findfirst (searchname, &(dir->entry));
  if ((dir->handle == DIR_UNALLOCATED)
#if 0
      || ((dir->entry.attrib & _A_SUBDIR) == 0)
#endif
      )
    error_system_call (errno, syscall_opendir);

  return (allocate_directory_pointer (dir));
}

CONST char *
DEFUN (OS_directory_read, (index), unsigned int index)
{ DIR * dir = REFERENCE_DIRECTORY (index);

  if (dir == 0 || dir->handle==DIR_UNALLOCATED)
    return 0;

  Get_Directory_Entry_Name(dir->entry, dir->pathname);
  if (_findnext (dir->handle, &(dir->entry)))
    dir->handle = DIR_UNALLOCATED;
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
{ DIR * dir = REFERENCE_DIRECTORY (index);

  if (dir)
  {
    if (dir->handle != DIR_UNALLOCATED)
      _findclose (dir->handle);
    free(dir);
  }
  DEALLOCATE_DIRECTORY (index);
}
