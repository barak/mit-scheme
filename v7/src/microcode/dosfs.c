/* -*-C-*-

$Id: dosfs.c,v 1.6 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "msdos.h"
#include "osfs.h"

enum file_existence
DEFUN (OS_file_existence_test, (name), char * name)
{
  struct stat s;
  char filename[128];
 
  dos_pathname_as_filename (name, filename);
  
  return
    (((DOS_stat (filename, (&s))) < 0)
     ? file_doesnt_exist : file_does_exist);
}

int
DEFUN (OS_file_access, (name, mode), CONST char * name AND unsigned int mode)
{
  char filename[128];
 
  dos_pathname_as_filename (name, filename);
  return ((DOS_access (filename, mode)) == 0);
}

int
DEFUN (OS_file_directory_p, (name), char * name)
{	
  struct stat s;
  char filename[128];
 
  dos_pathname_as_filename (name, filename);
  return (((DOS_stat (filename, (&s))) == 0) &&
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
  STD_VOID_SYSTEM_CALL (syscall_unlink, (DOS_unlink (name)));
}

void
DEFUN (OS_file_remove_link, (name), CONST char * name)
{
  struct stat s;
  if ( (DOS_stat (name, (&s)) == 0) &&
       (((s . st_mode) & S_IFMT) == S_IFREG) )
   DOS_unlink (name);
  return;
}

void
DEFUN (OS_file_link_hard, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  error_unimplemented_primitive ();
  /*NOTREACHED*/
}

void
DEFUN (OS_file_link_soft, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  error_unimplemented_primitive ();
  /*NOTREACHED*/
}

void
DEFUN (OS_file_rename, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  if ((rename (from_name, to_name)) != 0)
    error_system_call (errno, syscall_rename);
}

void
DEFUN (OS_file_copy, (from_name, to_name),
       CONST char * from_name AND
       CONST char * to_name)
{
  int result;
  Tchannel source_channel = (OS_open_input_file (from_name));
  Tchannel destination_channel = (OS_open_output_file (to_name));
  off_t source_length = (OS_file_length (source_channel));

  result = (OS_channel_copy (source_length,
			     source_channel,
			     destination_channel));
  
  OS_channel_close (source_channel);
  OS_channel_close (destination_channel);

  if (result < 0)
  {
    signal_error_from_primitive (ERR_IO_ERROR);
  }
  return;
}

void
DEFUN (OS_directory_make, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_mkdir, (DOS_mkdir (name)));
}

void
DEFUN (OS_directory_delete, (name), CONST char * name)
{
  STD_VOID_SYSTEM_CALL (syscall_rmdir, (DOS_rmdir (name)));
}

/* This is such that directory open does not return the first file */

typedef struct DIR_struct
{
  struct FIND *entry;
  char pathname[13];
} DIR;

#define GET_DIRECTORY_ENTRY_NAME(entry, pathname)		\
  (strcpy (pathname, ((entry)->name)), strlwr (pathname))

static DIR ** directory_pointers;
static unsigned int n_directory_pointers;

void
DEFUN_VOID (DOS_initialize_directory_reader)
{
  directory_pointers = 0;
  n_directory_pointers = 0;
  return;
}

static unsigned int
DEFUN (allocate_directory_pointer, (pointer), DIR * pointer)
{
  if (n_directory_pointers == 0)
    {
      DIR ** pointers = ((DIR **) (DOS_malloc ((sizeof (DIR *)) * 4)));
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
       (DOS_realloc (((PTR) directory_pointers),
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
  struct FIND *entry;
  DIR * pointer = malloc (sizeof(DIR));
  
  if (pointer == 0)
    error_system_call (ENOMEM, syscall_malloc);

  if (dos_pathname_as_filename (name, filename))
    sprintf (searchname, "%s*.*", filename);
  else
    sprintf (searchname, "%s\\*.*", filename);

  if ((entry = findfirst(searchname, FA_DIREC)) == 0)
    error_system_call (errno, syscall_opendir);
  
  pointer->entry = entry;
  return (allocate_directory_pointer (pointer));
}

CONST char *
DEFUN (OS_directory_read, (index), unsigned int index)
{
  DIR * pointer = REFERENCE_DIRECTORY (index);
  if (pointer->entry == 0)
    return 0;

  GET_DIRECTORY_ENTRY_NAME (pointer->entry, pointer->pathname);
  pointer->entry = findnext ();
  return (pointer -> pathname);
}

CONST char *
DEFUN (OS_directory_read_matching, (index, prefix),
       unsigned int index AND
       CONST char * prefix)
{
  error_unimplemented_primitive ();
  /*NOTREACHED*/
}

void
DEFUN (OS_directory_close, (index), unsigned int index)
{ DIR * pointer = REFERENCE_DIRECTORY (index);

  free (pointer);
  DEALLOCATE_DIRECTORY (index);
}

int
DEFUN (DOS_read_file_status, (name, s),
       CONST char * name AND
       struct stat * s)
{
  char filename[128];

  dos_pathname_as_filename (name, filename);
  
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
