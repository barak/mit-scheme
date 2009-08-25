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

#include "nt.h"
#include "ntfs.h"
#include <string.h>
#include "outf.h"

#ifndef FILE_TOUCH_OPEN_TRIES
#  define FILE_TOUCH_OPEN_TRIES 5
#endif

static enum get_file_info_result get_file_info_from_dir
  (const char *, BY_HANDLE_FILE_INFORMATION *, int);
static int valid_drive_p (const char *);
static HANDLE create_file_for_info (const char *);

enum get_file_info_result
NT_get_file_info (const char * namestring, BY_HANDLE_FILE_INFORMATION * info,
		  int inaccessible_ok)
{
  char nscopy [MAX_PATH];
  HANDLE hfile;

  strcpy (nscopy, namestring);
  {
    unsigned int len = (strlen (nscopy));
    if ((len > 3) && ((nscopy [len - 1]) == '\\'))
      (nscopy [len - 1]) = '\0';
  }
  hfile = (create_file_for_info (nscopy));
  if (hfile == INVALID_HANDLE_VALUE)
    {
      DWORD code = (GetLastError ());
      if (STAT_NOT_FOUND_P (code))
	return (gfi_not_found);
      if (STAT_NOT_ACCESSIBLE_P (code))
	return (get_file_info_from_dir (nscopy, info, inaccessible_ok));
      NT_error_api_call (code, apicall_CreateFile);
    }
  if (!GetFileInformationByHandle (hfile, info))
    {
      DWORD code = (GetLastError ());
      (void) CloseHandle (hfile);
      if (STAT_NOT_FOUND_P (code))
	return (gfi_not_found);
      if (inaccessible_ok && (STAT_NOT_ACCESSIBLE_P (code)))
	return (gfi_not_accessible);
      NT_error_api_call (code, apicall_GetFileInformationByHandle);
    }
  STD_BOOL_API_CALL (CloseHandle, (hfile));
  return (gfi_ok);
}

/* Incredible kludge.  Some files (e.g. \pagefile.sys) cannot be
   accessed by the usual technique, but much of the same information
   is available by reading the directory.  More M$ bullshit.  */
static enum get_file_info_result
get_file_info_from_dir (const char * namestring,
			BY_HANDLE_FILE_INFORMATION * info,
			int inaccessible_ok)
{
  WIN32_FIND_DATA fi;
  HANDLE handle = (FindFirstFile (namestring, (&fi)));
  if (handle == INVALID_HANDLE_VALUE)
    {
      DWORD code = (GetLastError ());
      if (STAT_NOT_FOUND_P (code))
	{
	  /* On Windows 95, the root directory of a drive cannot be
	     interrogated using either method.  Test to see if it is a
	     valid drive name, and if so, dummy it.  */
	  if (((strlen (namestring)) == 3)
	      && ((namestring[1]) == ':')
	      && ((namestring[2]) == '\\')
	      && (valid_drive_p (namestring)))
	    {
	      (info -> dwFileAttributes) = FILE_ATTRIBUTE_DIRECTORY;
	      ((info -> ftCreationTime) . dwHighDateTime) = 0;
	      ((info -> ftCreationTime) . dwLowDateTime) = 0;
	      ((info -> ftLastAccessTime) . dwHighDateTime) = 0;
	      ((info -> ftLastAccessTime) . dwLowDateTime) = 0;
	      ((info -> ftLastWriteTime) . dwHighDateTime) = 0;
	      ((info -> ftLastWriteTime) . dwLowDateTime) = 0;
	      (info -> dwVolumeSerialNumber) = 0;
	      (info -> nFileSizeHigh) = 0;
	      (info -> nFileSizeLow) = 0;
	      (info -> nNumberOfLinks) = 1;
	      (info -> nFileIndexHigh) = 0;
	      (info -> nFileIndexLow) = 0;
	      return (gfi_ok);
	    }
	  else
	    return (gfi_not_found);
	}
      if (inaccessible_ok && (STAT_NOT_ACCESSIBLE_P (code)))
	return (gfi_not_accessible);
      NT_error_api_call (code, apicall_FindFirstFile);
    }
  FindClose (handle);
  (info -> dwFileAttributes) = (fi . dwFileAttributes);
  (info -> ftCreationTime) = (fi . ftCreationTime);
  (info -> ftLastAccessTime) = (fi . ftLastAccessTime);
  (info -> ftLastWriteTime) = (fi . ftLastWriteTime);
  (info -> dwVolumeSerialNumber) = 0;
  (info -> nFileSizeHigh) = (fi . nFileSizeHigh);
  (info -> nFileSizeLow) = (fi . nFileSizeLow);
  (info -> nNumberOfLinks) = 1;
  (info -> nFileIndexHigh) = 0;
  (info -> nFileIndexLow) = 0;
  return (gfi_ok);
}

static int
valid_drive_p (const char * namestring)
{
  DWORD sectors_per_cluster;
  DWORD bytes_per_sector;
  DWORD number_of_free_clusters;
  DWORD total_number_of_clusters;
  return
    (GetDiskFreeSpace (namestring,
		       (&sectors_per_cluster),
		       (&bytes_per_sector),
		       (&number_of_free_clusters),
		       (&total_number_of_clusters)));
}

static HANDLE
create_file_for_info (const char * namestring)
{
  return
    (CreateFile (namestring,
		 0,
		 (FILE_SHARE_READ | FILE_SHARE_WRITE),
		 0,
		 OPEN_EXISTING,
		 FILE_FLAG_BACKUP_SEMANTICS,
		 NULL));
}

enum file_existence
OS_file_existence_test (const char * name)
{
  BY_HANDLE_FILE_INFORMATION info;
  return
    (((NT_get_file_info (name, (&info), 1)) == gfi_ok)
     ? file_does_exist
     : file_doesnt_exist);
}

enum file_existence
OS_file_existence_test_direct (const char * name)
{
  return (OS_file_existence_test (name));
}

enum file_type
OS_file_type_direct (const char * name)
{
  BY_HANDLE_FILE_INFORMATION info;
  return
    (((NT_get_file_info (name, (&info), 0)) == gfi_not_found)
     ? file_type_nonexistent
     : (((info . dwFileAttributes) & FILE_ATTRIBUTE_DIRECTORY) == 0)
     ? file_type_regular
     : file_type_directory);
}

enum file_type
OS_file_type_indirect (const char * name)
{
  return (OS_file_type_direct (name));
}

#define R_OK 4
#define W_OK 2
#define X_OK 1

int
OS_file_access (const char * name, unsigned int mode)
{
  BY_HANDLE_FILE_INFORMATION info;
  if ((NT_get_file_info (name, (&info), 1)) != gfi_ok)
    return (0);
  if (((mode & W_OK) != 0)
      && (((info . dwFileAttributes) & FILE_ATTRIBUTE_READONLY) != 0))
    return (0);
  if (((mode & X_OK) != 0)
      && (((info . dwFileAttributes) & FILE_ATTRIBUTE_DIRECTORY) == 0))
    {
      const char * extension = (strrchr (name, '.'));
      if (! (((stricmp (extension, ".exe")) == 0)
	     || ((stricmp (extension, ".com")) == 0)
	     || ((stricmp (extension, ".bat")) == 0)))
	return (0);
    }
  return (1);
}

int
OS_file_directory_p (const char * name)
{
  BY_HANDLE_FILE_INFORMATION info;
  return
    (((NT_get_file_info (name, (&info), 0)) == gfi_ok)
     && (((info . dwFileAttributes) & FILE_ATTRIBUTE_DIRECTORY) != 0));
}

const char *
OS_file_soft_link_p (const char * name)
{
  return (0);
}

static void
guarantee_writable (const char * name,
       int errorp)
{
  DWORD attributes = (GetFileAttributes (name));
  if (attributes == 0xFFFFFFFF)
    {
      DWORD error_code = (GetLastError ());
      if ((! ((error_code == ERROR_FILE_NOT_FOUND)
	      || (error_code == ERROR_PATH_NOT_FOUND)))
	  && errorp)
	NT_error_api_call (error_code, apicall_GetFileAttributes);
    }
  else if ((attributes & FILE_ATTRIBUTE_READONLY) != 0)
    {
      if ((! (SetFileAttributes (name,
				 (attributes &~ FILE_ATTRIBUTE_READONLY))))
	  && errorp)
	NT_error_api_call ((GetLastError ()), apicall_SetFileAttributes);
    }
}

void
OS_file_remove (const char * name)
{
  guarantee_writable (name, 1);
  STD_BOOL_API_CALL (DeleteFile, (name));
}

void
OS_file_remove_link (const char * name)
{
  struct stat s;
  if ((stat (name, (&s)) == 0)
      && (((s . st_mode) & S_IFMT) == S_IFREG))
    {
      guarantee_writable (name, 0);
      unlink (name);
    }
}

void
OS_file_rename (const char * from, const char * to)
{
  guarantee_writable (to, 1);
  STD_BOOL_API_CALL (MoveFile, (from, to));
}

void
OS_file_copy (const char * from, const char * to)
{
  guarantee_writable (to, 1);
  STD_BOOL_API_CALL (CopyFile, (from, to, FALSE));
}

void
OS_file_link_hard (const char * from_name, const char * to_name)
{
  error_unimplemented_primitive ();
}

void
OS_file_link_soft (const char * from_name, const char * to_name)
{
  error_unimplemented_primitive ();
}

void
OS_directory_make (const char * name)
{
  STD_BOOL_API_CALL (CreateDirectory, (name, 0));
}

void
OS_directory_delete (const char * name)
{
  STD_BOOL_API_CALL (RemoveDirectory, (name));
}

static void protect_fd (int fd);

int
OS_file_touch (const char * filename)
{
  int fd;
  transaction_begin ();
  {
    unsigned int count = 0;
    while (1)
      {
	count += 1;
	/* Use O_EXCL to prevent overwriting existing file. */
	fd = (open (filename, (O_RDWR | O_CREAT | O_EXCL), MODE_REG));
	if (fd >= 0)
	  {
	    protect_fd (fd);
	    transaction_commit ();
	    return (1);
	  }
	if (errno == EEXIST)
	  {
	    fd = (open (filename, O_RDWR, MODE_REG));
	    if (fd >= 0)
	      {
		protect_fd (fd);
		break;
	      }
	    else if (errno == ENOENT)
	      continue;
	  }
	if (count >= FILE_TOUCH_OPEN_TRIES)
	  NT_error_unix_call (errno, syscall_open);
      }
  }
  {
    struct stat file_status;
    STD_VOID_UNIX_CALL (fstat, (fd, (&file_status)));
    if (((file_status . st_mode) & S_IFMT) != S_IFREG)
      return (-1);
    /* CASE 3: file length of 0 needs special treatment. */
    if ((file_status . st_size) == 0)
     {
	char buf [1];
	(buf[0]) = '\0';
	STD_VOID_UNIX_CALL (write, (fd, buf, 1));
	transaction_commit ();
	fd = (open (filename, (O_WRONLY | O_TRUNC), MODE_REG));
	if (fd >= 0)
	  STD_VOID_UNIX_CALL (close, (fd));
	return (0);
      }
  }
  /* CASE 4: read, then write back the first byte in the file. */
  {
    char buf [1];
    int scr;
    STD_UINT_UNIX_CALL (scr, read, (fd, buf, 1));
    if (scr > 0)
      {
	STD_VOID_UNIX_CALL (lseek, (fd, 0, SEEK_SET));
	STD_VOID_UNIX_CALL (write, (fd, buf, 1));
      }
  }
  transaction_commit ();
  return (0);
}

static void
protect_fd_close (void * ap)
{
  close (* ((int *) ap));
}

static void
protect_fd (int fd)
{
  int * p = (dstack_alloc (sizeof (int)));
  (*p) = fd;
  transaction_record_action (tat_always, protect_fd_close, p);
}

typedef struct nt_dir_struct
{
  WIN32_FIND_DATA entry;
  HANDLE handle;         /* may be DIR_UNALLOCATED */
  BOOL more;
} nt_dir;

static nt_dir ** directory_pointers;
static unsigned int n_directory_pointers;

void
NT_initialize_directory_reader (void)
{
  directory_pointers = 0;
  n_directory_pointers = 0;
}

static unsigned int
allocate_directory_pointer (nt_dir * pointer)
{
  if (n_directory_pointers == 0)
    {
      nt_dir ** pointers = (OS_malloc ((sizeof (nt_dir *)) * 4));
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
    nt_dir ** pointers
      = (OS_realloc (((void *) directory_pointers),
		     ((sizeof (nt_dir *)) * n_pointers)));
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
OS_directory_valid_p (unsigned int index)
{
  return
    ((index < n_directory_pointers)
     && ((REFERENCE_DIRECTORY (index)) != 0));
}

unsigned int
OS_directory_open (const char * search_pattern)
{
  char pattern [MAX_PATH];
  nt_dir * dir = (OS_malloc (sizeof (nt_dir)));
  strcpy (pattern, search_pattern);
  {
    unsigned int len = (strlen (pattern));
    if ((len > 0) && ((pattern [len - 1]) == '\\'))
      strcat (pattern, "*.*");
  }
  (dir -> handle) = (FindFirstFile (pattern, (& (dir -> entry))));
  if ((dir -> handle) == INVALID_HANDLE_VALUE)
    {
      DWORD code = (GetLastError ());
      if (code != ERROR_FILE_NOT_FOUND)
	{
	  free (dir);
	  NT_error_api_call (code, apicall_FindFirstFile);
	}
      (dir -> more) = FALSE;
    }
  else
    (dir -> more) = TRUE;
  return (allocate_directory_pointer (dir));
}

int
win32_directory_read (unsigned int index, WIN32_FIND_DATA * info)
{
  nt_dir * dir = (REFERENCE_DIRECTORY (index));
  if ((dir == 0) || (! (dir -> more)))
    return (0);
  (*info) = (dir -> entry);
  if ((dir -> handle) == INVALID_HANDLE_VALUE)
    (dir -> more) = FALSE;
  else
    (dir -> more) = (FindNextFile ((dir -> handle), (& (dir -> entry))));
  return (1);
}

const char *
OS_directory_read (unsigned int index)
{
  static WIN32_FIND_DATA info;
  return
    ((win32_directory_read (index, (&info)))
     ? (info . cFileName)
     : 0);
}

const char *
OS_directory_read_matching (unsigned int index, const char * prefix)
{
  unsigned int n = (strlen (prefix));
  while (1)
    {
      const char * pathname = (OS_directory_read (index));
      if (pathname == 0)
	return (0);
      if ((strnicmp (pathname, prefix, n)) == 0)
	return (pathname);
    }
}

void
OS_directory_close (unsigned int index)
{
  nt_dir * dir = (REFERENCE_DIRECTORY (index));
  if (dir)
    {
      if ((dir -> handle) != INVALID_HANDLE_VALUE)
	FindClose (dir -> handle);
      free (dir);
    }
  DEALLOCATE_DIRECTORY (index);
}
