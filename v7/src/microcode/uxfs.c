/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/uxfs.c,v 1.1 1990/06/20 19:37:11 cph Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

enum file_existence
DEFUN (OS_file_existence_test, (name), CONST char * name)
{
  struct stat s;
  return
    (((UX_stat (name, (&s))) < 0)
     ? (((errno == ENOENT) || (errno == ENOTDIR))
	? file_doesnt_exist
	: file_may_exist)
     : file_does_exist);
}

int
DEFUN (OS_file_access, (name, mode), CONST char * name AND unsigned int mode)
{
  return ((UX_access (name, mode)) == 0);
}

int
DEFUN (OS_file_directory_p, (name), CONST char * name)
{
  struct stat s;
  return (((UX_stat (name, (&s))) == 0) &&
	  (((s . st_mode) & S_IFMT) == S_IFDIR));
}

CONST char *
DEFUN (OS_file_soft_link_p, (name), CONST char * name)
{
#ifdef HAVE_SYMBOLIC_LINKS
  struct stat s;
  if (((UX_lstat (name, (&s))) < 0)
      || (((s . st_mode) & S_IFMT) != S_IFLNK))
    return (0);
  {
    int scr;
    int buffer_length = 100;
    char * buffer = (UX_malloc (buffer_length));
    if (buffer == 0)
      error_system_call (ENOMEM, "malloc");
    while (1)
      {
	STD_UINT_SYSTEM_CALL
	  ("readlink", scr, (UX_readlink (name, buffer, buffer_length)));
	if (scr < buffer_length)
	  break;
	buffer_length *= 2;
	buffer = (UX_realloc (buffer, buffer_length));
	if (buffer == 0)
	  error_system_call (ENOMEM, "realloc");
      }
    (buffer[scr]) = '\0';
    return ((CONST char *) buffer);
  }
#else
  return (0);
#endif
}

void
DEFUN (OS_file_remove, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL ("unlink", (UX_unlink (name)));
}

void
DEFUN (OS_file_remove_link, (name), CONST char * name)
{
  struct stat s;
  if (((UX_lstat (name, (&s))) == 0) &&
      (((((s . st_mode) & S_IFMT) == S_IFREG) && ((s . st_nlink) > 1))
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
  STD_VOID_SYSTEM_CALL ("link", (UX_link (from_name, to_name)));
}

void
DEFUN (OS_file_link_soft, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
#ifdef HAVE_SYMBOLIC_LINKS
  STD_VOID_SYSTEM_CALL ("symlink", (UX_symlink (from_name, to_name)));
#else
  error_unimplemented_primitive ();
#endif
}

void
DEFUN (OS_file_rename, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  STD_VOID_SYSTEM_CALL ("rename", (UX_rename (from_name, to_name)));
}

void
DEFUN (OS_directory_make, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL ("mkdir", (UX_mkdir (name, MODE_DIR)));
}

#if defined(HAVE_DIRENT) || defined(HAVE_DIR)

static DIR * directory_pointer = 0;
#ifdef HAVE_DIRENT
static struct dirent * directory_entry;
#else
static struct direct * directory_entry;
#endif

#define READ_DIRECTORY_ENTRY()						\
{									\
  directory_entry = (readdir (directory_pointer));			\
  if (directory_entry == 0)						\
    {									\
      closedir (directory_pointer);					\
      directory_pointer = 0;						\
      return (0);							\
    }									\
  return (directory_entry -> d_name);					\
}

CONST char *
DEFUN (OS_directory_open, (name), CONST char * name)
{
  if (directory_pointer != 0)
    error_external_return ();
  /* Cast `name' to non-const because hp-ux 7.0 declaration incorrect. */
  directory_pointer = (opendir ((char *) name));
  if (directory_pointer == 0)
#ifdef HAVE_DIRENT
    error_system_call (errno, "opendir");
#else
    error_external_return ();
#endif
  READ_DIRECTORY_ENTRY ();
}

CONST char *
DEFUN_VOID (OS_directory_read)
{
  if (directory_pointer == 0)
    error_external_return ();
  READ_DIRECTORY_ENTRY ();
}

void
DEFUN_VOID (OS_directory_close)
{
  if (directory_pointer != 0)
    {
      closedir (directory_pointer);
      directory_pointer = 0;
    }
}

void
DEFUN_VOID (UX_initialize_directory_reader)
{
  directory_pointer = 0;
}

#else /* not HAVE_DIRENT nor HAVE_DIR */

CONST char *
DEFUN (OS_directory_open, (name), CONST char * name)
{
  error_unimplemented_primitive ();
  return (0);
}

CONST char *
DEFUN_VOID (OS_directory_read)
{
  error_unimplemented_primitive ();
  return (0);
}

void
DEFUN_VOID (OS_directory_close)
{
  error_unimplemented_primitive ();
}

void
DEFUN_VOID (UX_initialize_directory_reader)
{
}

#endif /* HAVE_DIRENT */
