/* -*-C-*-

$Id: ntfs.c,v 1.18 1997/10/26 09:32:53 cph Exp $

Copyright (c) 1992-97 Massachusetts Institute of Technology

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
#include "ntfs.h"
#include <string.h>
#include "outf.h"

static HANDLE create_file_for_info (const char *);

static enum get_file_info_result get_file_info_from_dir
  (const char *, BY_HANDLE_FILE_INFORMATION *);

enum get_file_info_result
NT_get_file_info (const char * namestring, BY_HANDLE_FILE_INFORMATION * info)
{
  HANDLE hfile = (create_file_for_info (namestring));
  if (hfile == INVALID_HANDLE_VALUE)
    {
      DWORD code = (GetLastError ());
      if (STAT_NOT_FOUND_P (code))
	return (gfi_not_found);
      if (STAT_NOT_ACCESSIBLE_P (code))
	return (get_file_info_from_dir (namestring, info));
      NT_error_api_call (code, apicall_CreateFile);
    }
  if (!GetFileInformationByHandle (hfile, info))
    {
      DWORD code = (GetLastError ());
      (void) CloseHandle (hfile);
      if (STAT_NOT_FOUND_P (code))
	return (gfi_not_found);
      if (STAT_NOT_ACCESSIBLE_P (code))
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
			BY_HANDLE_FILE_INFORMATION * info)
{
  char nscopy [MAX_PATH];
  WIN32_FIND_DATA fi;
  HANDLE handle;

  strcpy (nscopy, namestring);
  {
    unsigned int len = (strlen (nscopy));
    if ((len > 0) && ((nscopy [len - 1]) == '\\'))
      (nscopy [len - 1]) = '\0';
  }
  handle = (FindFirstFile (nscopy, (&fi)));
  if (handle == INVALID_HANDLE_VALUE)
    {
      DWORD code = (GetLastError ());
      if (STAT_NOT_FOUND_P (code))
	return (gfi_not_found);
      if (STAT_NOT_ACCESSIBLE_P (code))
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
DEFUN (OS_file_existence_test, (name), CONST char * name)
{
  BY_HANDLE_FILE_INFORMATION info;
  return
    (((NT_get_file_info (name, (&info))) == gfi_ok)
     ? file_does_exist
     : file_doesnt_exist);
}

#define R_OK 4
#define W_OK 2
#define X_OK 1

int
DEFUN (OS_file_access, (name, mode), CONST char * name AND unsigned int mode)
{
  BY_HANDLE_FILE_INFORMATION info;
  if ((NT_get_file_info (name, (&info))) != gfi_ok)
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
DEFUN (OS_file_directory_p, (name), CONST char * name)
{
  BY_HANDLE_FILE_INFORMATION info;
  return
    (((NT_get_file_info (name, (&info))) == gfi_ok)
     && (((info . dwFileAttributes) & FILE_ATTRIBUTE_DIRECTORY) != 0));
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
DEFUN (OS_file_remove, (name), CONST char * name)
{
  guarantee_writable (name, 1);
  STD_BOOL_API_CALL (DeleteFile, (name));
}

void
DEFUN (OS_file_remove_link, (name), CONST char * name)
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
DEFUN (OS_file_rename, (from, to),
       CONST char * from AND
       CONST char * to)
{
  guarantee_writable (to, 1);
  STD_BOOL_API_CALL (MoveFile, (from, to));
}

void
DEFUN (OS_file_copy, (from, to),
       CONST char * from AND
       CONST char * to)
{
  guarantee_writable (to, 1);
  STD_BOOL_API_CALL (CopyFile, (from, to, FALSE));
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
  STD_BOOL_API_CALL (CreateDirectory, (name, 0));
}

void
DEFUN (OS_directory_delete, (name), CONST char * name)
{
  STD_BOOL_API_CALL (RemoveDirectory, (name));
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
      = (OS_realloc (((PTR) directory_pointers),
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
DEFUN (OS_directory_valid_p, (index), long index)
{
  return
    ((0 <= index)
     && (index < (long) n_directory_pointers)
     && ((REFERENCE_DIRECTORY (index)) != 0));
}

unsigned int
DEFUN (OS_directory_open, (name), CONST char * search_pattern)
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
      free (dir);
      NT_error_api_call ((GetLastError ()), apicall_FindFirstFile);
    }
  (dir -> more) = TRUE;
  return (allocate_directory_pointer (dir));
}

CONST char *
DEFUN (OS_directory_read, (index), unsigned int index)
{
  nt_dir * dir = (REFERENCE_DIRECTORY (index));
  if ((dir == 0) || (! (dir -> more)))
    return (0);
  GET_DIRECTORY_ENTRY_NAME ((dir -> entry), (dir -> pathname));
  (dir -> more) = (FindNextFile ((dir -> handle), (& (dir -> entry))));
  return (dir -> pathname);
}

CONST char *
DEFUN (OS_directory_read_matching, (index, prefix),
       unsigned int index AND
       CONST char * prefix)
{
  unsigned int n = (strlen (prefix));
  while (1)
    {
      CONST char * pathname = (OS_directory_read (index));
      if (pathname == 0)
	return (0);
      if ((strnicmp (pathname, prefix, n)) == 0)
	return (pathname);
    }
}

int
win32_directory_read (unsigned int index, WIN32_FIND_DATA * info)
{
  nt_dir * dir = (REFERENCE_DIRECTORY (index));
  if ((dir == 0) || (! (dir -> more)))
    return (0);
  (*info) = (dir -> entry);
  (dir -> more) = (FindNextFile ((dir -> handle), (& (dir -> entry))));
  return (1);
}

void
DEFUN (OS_directory_close, (index), unsigned int index)
{
  nt_dir * dir = (REFERENCE_DIRECTORY (index));
  if (dir)
    {
      FindClose (dir -> handle);
      free (dir);
    }
  DEALLOCATE_DIRECTORY (index);
}
