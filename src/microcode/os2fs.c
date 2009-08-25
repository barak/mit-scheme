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

#include "os2.h"
#include "osfs.h"

#ifdef __GCC2__
#  define stricmp strcasecmp
#  define strnicmp strncasecmp
#endif

#ifndef FILE_TOUCH_OPEN_TRIES
#  define FILE_TOUCH_OPEN_TRIES 5
#endif

static const char * make_pathname (const char *, const char *);
static const char * filename_extension (const char *);
extern char * OS2_drive_type (char);
extern char * OS2_remove_trailing_backslash (const char *);

FILESTATUS3 *
OS2_read_file_status (const char * filename)
{
  char name [CCHMAXPATH];
  static FILESTATUS3 info;
  unsigned int flen = (strlen (filename));
  FASTCOPY (filename, name, flen);
  /* Strip trailing backslash.  */
  if ((flen > 0) && ((name [flen - 1]) == '\\'))
    flen -= 1;
  (name [flen]) = '\0';
  /* Canonicalize various forms of reference to root directory.  */
  if ((flen == 5)
      && (isalpha (name [0]))
      && ((name [1]) == ':')
      && ((name [2]) == '\\')
      && ((name [3]) == '.')
      && ((name [4]) == '.'))
    (name [4]) = '\0';
  else if ((flen == 2)
	   && (isalpha (name [0]))
	   && ((name [1]) == ':'))
    {
      (name [2]) = '\\';
      (name [3]) = '.';
      (name [4]) = '\0';
    }
  else if (flen == 0)
    {
      (name [0]) = '\\';
      (name [1]) = '.';
      (name [2]) = '\0';
    }
  {
    APIRET rc
      = (dos_query_path_info (name, FIL_STANDARD, (&info), (sizeof (info))));
    /* So many different things can go wrong here that it is a bad
       idea to attempt to enumerate them.  Several times I have
       thought I had all the possible conditions and later discovered
       that I was wrong.  */
    if (rc != NO_ERROR)
      return (0);
  }
  return (&info);
}

void
OS2_write_file_status (const char * filename, FILESTATUS3 * info)
{
  STD_API_CALL
    (dos_set_path_info,
     ((OS2_remove_trailing_backslash (filename)),
      FIL_STANDARD, info, (sizeof (FILESTATUS3)), 0));
}

enum file_existence
OS_file_existence_test (const char * filename)
{
  return
    ((OS2_read_file_status (filename))
     ? file_does_exist
     : file_doesnt_exist);
}

enum file_existence
OS_file_existence_test_direct (const char * filename)
{
  return (OS_file_existence_test (filename));
}

enum file_type
OS_file_type_direct (const char * filename)
{
  FILESTATUS3 * info = (OS2_read_file_status (filename));
  return
    ((info == 0)
     ? file_type_nonexistent
     : (((info -> attrFile) & FILE_DIRECTORY) == 0)
     ? file_type_regular
     : file_type_directory);
}

enum file_type
OS_file_type_indirect (const char * filename)
{
  return (OS_file_type_direct (filename));
}

#define R_OK 4
#define W_OK 2
#define X_OK 1

int
OS_file_access (const char * filename, unsigned int mode)
{
  FILESTATUS3 * info = (OS2_read_file_status (filename));
  if (!info)
    return (0);
  if (((mode & W_OK) != 0) && (((info -> attrFile) & FILE_READONLY) != 0))
    return (0);
  if (((mode & X_OK) != 0) && (((info -> attrFile) & FILE_DIRECTORY) == 0))
    {
      const char * extension = (filename_extension (filename));
      if (! (((stricmp (extension, ".exe")) == 0)
	     || ((stricmp (extension, ".com")) == 0)
	     || ((stricmp (extension, ".cmd")) == 0)
	     || ((stricmp (extension, ".bat")) == 0)))
	return (0);
    }
  return (1);
}

int
OS_file_directory_p (const char * filename)
{
  if (((strlen (filename)) == 3)
      && (isalpha (filename [0]))
      && ((filename [1]) == ':')
      && ((filename [2]) == '\\'))
    return ((OS2_drive_type (filename [0])) != 0);
  else
    {
      FILESTATUS3 * info = (OS2_read_file_status (filename));
      return ((info == 0) ? 0 : (((info -> attrFile) & FILE_DIRECTORY) != 0));
    }
}

char *
OS2_drive_type (char drive_letter)
{
  char name [3];
  static char cbuf [(sizeof (FSQBUFFER2)) + (3 * CCHMAXPATH)];
  FSQBUFFER2 * buffer = ((FSQBUFFER2 *) cbuf);
  ULONG size = (sizeof (cbuf));
  (name [0]) = drive_letter;
  (name [1]) = ':';
  (name [2]) = '\0';
  STD_API_CALL
    (dos_query_fs_attach, (name, 0, FSAIL_QUERYNAME, buffer, (& size)));
  if (((buffer -> iType) == FSAT_LOCALDRV)
      || ((buffer -> iType) == FSAT_REMOTEDRV))
    {
      char * fsdname = ((buffer -> szName) + (buffer -> cbName) + 1);
      if ((buffer -> iType) == FSAT_REMOTEDRV)
	/* This bit of magic causes the "attach data" to be appended
	   to the driver name, with a colon separator.  In the case of
	   an NFS drive, the "attach data" is the mount information,
	   e.g. "martigny:/zu".  This information is valuable, because
	   it can be used to make crude inferences about the file
	   system on the remote machine.  */
	(fsdname [buffer -> cbFSDName]) = ':';
      return (fsdname);
    }
  else
    return (0);
}

const char *
OS_file_soft_link_p (const char * filename)
{
  return (0);
}

void
OS_file_remove (const char * filename)
{
  {
    FILESTATUS3 * info = (OS2_read_file_status (filename));
    if (info == 0)
      return;
    if (((info -> attrFile) & FILE_READONLY) != 0)
      {
	(info -> attrFile) &=~ FILE_READONLY;
	STD_API_CALL
	  (dos_set_path_info,
	   (((char *) filename), FIL_STANDARD, info, (sizeof (*info)), 0));
      }
  }
  STD_API_CALL (dos_delete, ((char *) filename));
}

void
OS_file_remove_link (const char * filename)
{
  OS_file_remove (filename);
}

void
OS_file_rename (const char * from_name, const char * to_name)
{
  STD_API_CALL (dos_move, (((char *) from_name), ((char *) to_name)));
}

void
OS_file_link_hard (const char * from_name, const char * to_name)
{
  OS2_error_unimplemented_primitive ();
}

void
OS_file_link_soft (const char * from_name, const char * to_name)
{
  OS2_error_unimplemented_primitive ();
}

void
OS_file_copy (const char * from, const char * to)
{
  FILESTATUS3 * info = (OS2_read_file_status (to));
  if ((info != 0) && (((info -> attrFile) & FILE_READONLY) != 0))
    {
      (info -> attrFile) &=~ FILE_READONLY;
      OS2_write_file_status (to, info);
    }
  STD_API_CALL (dos_copy, (((PSZ) from), ((PSZ) to), DCPY_EXISTING));
}

void
OS_directory_make (const char * directory_name)
{
  STD_API_CALL
    (dos_create_dir, ((OS2_remove_trailing_backslash (directory_name)), 0));
}

void
OS_directory_delete (const char * directory_name)
{
  STD_API_CALL
    (dos_delete_dir, (OS2_remove_trailing_backslash (directory_name)));
}

static void protect_handle (LHANDLE);

int
OS_file_touch (const char * filename)
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
      return (1);
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
  return (0);
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

typedef struct
{
  char allocatedp;
  HDIR handle;
  FILEFINDBUF3 info;
  ULONG count;
} dir_search_state;

static dir_search_state * dir_search_states;
static unsigned int n_dir_search_states;

void
OS2_initialize_directory_reader (void)
{
  dir_search_states = 0;
  n_dir_search_states = 0;
}

static unsigned int
allocate_dir_search_state (void)
{
  if (n_dir_search_states == 0)
    {
      dir_search_state * states =
	((dir_search_state *) (OS_malloc ((sizeof (dir_search_state)) * 4)));
      dir_search_states = states;
      n_dir_search_states = 4;
      {
	dir_search_state * scan = dir_search_states;
	dir_search_state * end = (scan + n_dir_search_states);
	((scan++) -> allocatedp) = 1;
	while (scan < end)
	  ((scan++) -> allocatedp) = 0;
      }
      return (0);
    }
  {
    dir_search_state * scan = dir_search_states;
    dir_search_state * end = (scan + n_dir_search_states);
    while (scan < end)
      if (! ((scan++) -> allocatedp))
	{
	  ((--scan) -> allocatedp) = 1;
	  return (scan - dir_search_states);
	}
  }
  {
    unsigned int result = n_dir_search_states;
    unsigned int n_states = (2 * n_dir_search_states);
    dir_search_state * states =
      ((dir_search_state *)
       (OS_realloc (((void *) dir_search_states),
		    ((sizeof (dir_search_state)) * n_states))));
    {
      dir_search_state * scan = (states + result);
      dir_search_state * end = (states + n_states);
      ((scan++) -> allocatedp) = 1;
      while (scan < end)
	((scan++) -> allocatedp) = 0;
    }
    dir_search_states = states;
    n_dir_search_states = n_states;
    return (result);
  }
}

#define REFERENCE_DIR_SEARCH_STATE(index) (& (dir_search_states[(index)]))
#define DEALLOCATE_DIR_SEARCH_STATE(state) ((state) -> allocatedp) = 0

int
OS_directory_valid_p (unsigned int index)
{
  return
    ((index < n_dir_search_states)
     && ((REFERENCE_DIR_SEARCH_STATE (index)) -> allocatedp));
}

static void
dir_open_deallocate (void * arg)
{
  DEALLOCATE_DIR_SEARCH_STATE ((dir_search_state *) arg);
}

unsigned int
OS_directory_open (const char * search_pattern)
{
  static char pattern [CCHMAXPATH];
  unsigned int index = (allocate_dir_search_state ());
  dir_search_state * s = (REFERENCE_DIR_SEARCH_STATE (index));
  transaction_begin ();
  transaction_record_action (tat_abort, dir_open_deallocate, s);
  strcpy (pattern, search_pattern);
  {
    unsigned int len = (strlen (pattern));
    if ((len > 0) && ((pattern [len - 1]) == '\\'))
      strcat (pattern, "*");
  }
  (s -> handle) = HDIR_CREATE;
  (s -> count) = 1;
  while (1)
    {
      APIRET rc
	= (dos_find_first
	   (pattern, (& (s -> handle)), FILE_ANY, (& (s -> info)),
	    (sizeof (s -> info)), (& (s -> count)), FIL_STANDARD));
      if (rc == NO_ERROR)
	break;
      if (rc == ERROR_NO_MORE_FILES)
	{
	  (s -> handle) = HDIR_CREATE;
	  (s -> count) = 0;
	  break;
	}
      if (rc != ERROR_INTERRUPT)
	OS2_error_system_call (rc, syscall_dos_find_first);
    }
  transaction_commit ();
  return (index);
}

static void
dir_find_next (dir_search_state * s)
{
  (s -> count) = 1;
  XTD_API_CALL
    (dos_find_next,
     ((s -> handle), (& (s -> info)), (sizeof (s -> info)), (& (s -> count))),
     {
       if (rc == ERROR_NO_MORE_FILES)
	 {
	   (s -> count) = 0;
	   return;
	 }
     });
}

static const char *
dir_current_name (dir_search_state * s)
{
  static char result [CCHMAXPATH];
  strcpy (result, ((s -> info) . achName));
  dir_find_next (s);
  return (result);
}

const char *
OS_directory_read (unsigned int index)
{
  dir_search_state * s = (REFERENCE_DIR_SEARCH_STATE (index));
  return (((s -> count) == 0) ? 0 : (dir_current_name (s)));
}

const char *
OS_directory_read_matching (unsigned int index, const char * prefix)
{
  dir_search_state * s = (REFERENCE_DIR_SEARCH_STATE (index));
  unsigned int n = (strlen (prefix));
  while (1)
    {
      if ((s -> count) == 0)
	return (0);
      if ((strnicmp (((s -> info) . achName), prefix, n)) == 0)
	return (dir_current_name (s));
      dir_find_next (s);
    }
}

void
OS_directory_close (unsigned int index)
{
  dir_search_state * s = (REFERENCE_DIR_SEARCH_STATE (index));
  if ((s -> handle) != HDIR_CREATE)
    STD_API_CALL (dos_find_close, (s -> handle));
  DEALLOCATE_DIR_SEARCH_STATE (s);
}

static const char *
filename_extension (const char * filename)
{
  const char * start;
  const char * period;
  start = (strrchr (filename, '\\'));
  start = ((start == 0) ? filename : (start + 1));
  period = (strrchr (start, '.'));
  return ((period == 0) ? (filename + (strlen (filename))) : period);
}

static const char *
make_pathname (const char * directory, const char * name)
{
  unsigned int dirlen = (strlen (directory));
  unsigned int namlen = (strlen (name));
  char * result = (OS_malloc (dirlen + namlen + 2));
  strcpy (result, directory);
  if ((dirlen > 0) && ((result [dirlen - 1]) != '\\'))
    strcat (result, "\\");
  strcat (result, name);
  return (result);
}

char *
OS2_remove_trailing_backslash (const char * filename)
{
  static char result [CCHMAXPATH];
  unsigned int len = (strlen (filename));
  if ((len == 0) || ((filename [len - 1]) != '\\'))
    return ((char *) filename);
  FASTCOPY (filename, result, (len - 1));
  (result [len - 1]) = '\0';
  return (result);
}
