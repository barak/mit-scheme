/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxfs.c,v 1.8 1992/07/06 23:42:07 jinx Exp $

Copyright (c) 1990-1992 Massachusetts Institute of Technology

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

#include "ux.h"
#include "osfs.h"

int
DEFUN (UX_read_file_status, (filename, s),
       CONST char * filename AND
       struct stat * s)
{
  while ((UX_lstat (filename, s)) < 0)
    {
      if (errno == EINTR)
	continue;
      if ((errno == ENOENT) || (errno == ENOTDIR))
	return (0);
      error_system_call (errno, syscall_lstat);
    }
  return (1);
}

int
DEFUN (UX_read_file_status_indirect, (filename, s),
       CONST char * filename AND
       struct stat * s)
{
  while ((UX_stat (filename, s)) < 0)
    {
      if (errno == EINTR)
	continue;
      if ((errno == ENOENT) || (errno == ENOTDIR))
	return (0);
      error_system_call (errno, syscall_stat);
    }
  return (1);
}

enum file_existence
DEFUN (OS_file_existence_test, (name), CONST char * name)
{
  struct stat s;
  return
    ((UX_read_file_status_indirect (name, (&s)))
     ? file_does_exist
     : (UX_read_file_status (name, (&s)))
     ? file_is_link
     : file_doesnt_exist);
}

int
DEFUN (OS_file_directory_p, (name), CONST char * name)
{
  struct stat s;
  return
    ((UX_read_file_status_indirect (name, (&s)))
     && (((s . st_mode) & S_IFMT) == S_IFDIR));
}

CONST char *
DEFUN (OS_file_soft_link_p, (name), CONST char * name)
{
#ifdef HAVE_SYMBOLIC_LINKS
  struct stat s;
  if (! ((UX_read_file_status (name, (&s)))
	 && (((s . st_mode) & S_IFMT) == S_IFLNK)))
    return (0);
  {
    int scr;
    int buffer_length = 100;
    char * buffer = (UX_malloc (buffer_length));
    if (buffer == 0)
      error_system_call (ENOMEM, syscall_malloc);
    while (1)
      {
	STD_UINT_SYSTEM_CALL
	  (syscall_readlink, scr, (UX_readlink (name, buffer, buffer_length)));
	if (scr < buffer_length)
	  break;
	buffer_length *= 2;
	buffer = (UX_realloc (buffer, buffer_length));
	if (buffer == 0)
	  error_system_call (ENOMEM, syscall_realloc);
      }
    (buffer[scr]) = '\0';
    return ((CONST char *) buffer);
  }
#else
  return (0);
#endif
}

int
DEFUN (OS_file_access, (name, mode), CONST char * name AND unsigned int mode)
{
  return ((UX_access (name, mode)) == 0);
}

void
DEFUN (OS_file_remove, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_unlink, (UX_unlink (name)));
}

void
DEFUN (OS_file_remove_link, (name), CONST char * name)
{
  struct stat s;
  if ((UX_read_file_status (name, (&s)))
      && ((((s . st_mode) & S_IFMT) == S_IFREG)
#ifdef HAVE_SYMBOLIC_LINKS
	  || (((s . st_mode) & S_IFMT) == S_IFLNK)
#endif
	  ))
    UX_unlink (name);
}

void
DEFUN (OS_file_link_hard, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  STD_VOID_SYSTEM_CALL (syscall_link, (UX_link (from_name, to_name)));
}

void
DEFUN (OS_file_link_soft, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
#ifdef HAVE_SYMBOLIC_LINKS
  STD_VOID_SYSTEM_CALL (syscall_symlink, (UX_symlink (from_name, to_name)));
#else
  error_unimplemented_primitive ();
#endif
}

void
DEFUN (OS_file_rename, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  STD_VOID_SYSTEM_CALL (syscall_rename, (UX_rename (from_name, to_name)));
}

void
DEFUN (OS_directory_make, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_mkdir, (UX_mkdir (name, MODE_DIR)));
}

void
DEFUN (OS_directory_delete, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_rmdir, (UX_rmdir (name)));
}

int OS_directory_index;

#if defined(HAVE_DIRENT) || defined(HAVE_DIR)

static DIR ** directory_pointers;
static unsigned int n_directory_pointers;

void
DEFUN_VOID (UX_initialize_directory_reader)
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
      DIR ** pointers = ((DIR **) (UX_malloc ((sizeof (DIR *)) * 4)));
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
       (UX_realloc (((PTR) directory_pointers),
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
  /* Cast `name' to non-const because hp-ux 7.0 declaration incorrect. */
  DIR * pointer = (opendir ((char *) name));
  if (pointer == 0)
    error_system_call (errno, syscall_opendir);
  return (allocate_directory_pointer (pointer));
}

#ifndef HAVE_DIRENT
#define dirent direct
#endif

CONST char *
DEFUN (OS_directory_read, (index), unsigned int index)
{
  struct dirent * entry = (readdir (REFERENCE_DIRECTORY (index)));
  return ((entry == 0) ? 0 : (entry -> d_name));
}

CONST char *
DEFUN (OS_directory_read_matching, (index, prefix), 
       unsigned int index AND
       CONST char * prefix)
{
  DIR * pointer = (REFERENCE_DIRECTORY (index));
  unsigned int n = (strlen (prefix));
  while (1)
    {
      struct dirent * entry = (readdir (pointer));
      if (entry == 0)
	return (0);
      if ((strncmp (prefix, (entry -> d_name), n)) == 0)
	return (entry -> d_name);
    }
}

void
DEFUN (OS_directory_close, (index), unsigned int index)
{
  closedir (REFERENCE_DIRECTORY (index));
  DEALLOCATE_DIRECTORY (index);
}

#else /* not HAVE_DIRENT nor HAVE_DIR */

void
DEFUN_VOID (UX_initialize_directory_reader)
{
  OS_directory_index = (-1);
}

int
DEFUN (OS_directory_valid_p, (index), long index)
{
  return (0);
}

unsigned int
DEFUN (OS_directory_open, (name), CONST char * name)
{
  error_unimplemented_primitive ();
  return (0);
}

#ifndef HAVE_DIRENT
#define dirent direct
#endif

CONST char *
DEFUN (OS_directory_read, (index), unsigned int index)
{
  error_unimplemented_primitive ();
  return (0);
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
  error_unimplemented_primitive ();
}

#endif /* HAVE_DIRENT */
