/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* Primitives to perform file-system operations. */

#include "scheme.h"
#include "prims.h"
#include "osfile.h"
#include "osfs.h"
#include "osio.h"

#define STRING_RESULT(expression) do					\
{									\
  const char * result = (expression);					\
  PRIMITIVE_RETURN							\
    ((result == 0)							\
     ? SHARP_F								\
     : (char_pointer_to_string (result)));				\
} while (0)

DEFINE_PRIMITIVE ("FILE-EXISTS?", Prim_file_exists_p, 1, 1,
  "Return #T iff FILENAME refers to an existing file.\n\
Return #F if the file doesn't exist.\n\
Return zero if it's a symbolic link that points to a nonexisting file.\n\
Signal an error if the file's existence is indeterminate.")
{
  PRIMITIVE_HEADER (1);
  {
    enum file_existence result = (OS_file_existence_test (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((result == file_doesnt_exist)
       ? SHARP_F
       : (result == file_does_exist)
       ? SHARP_T
       : FIXNUM_ZERO);
  }
}

DEFINE_PRIMITIVE ("FILE-EXISTS-DIRECT?", Prim_file_exists_direct_p, 1, 1,
  "Return #T iff FILENAME refers to an existing file.\n\
Return #F if the file doesn't exist.\n\
Return zero if it's a symbolic link.\n\
Signal an error if the file's existence is indeterminate.")
{
  PRIMITIVE_HEADER (1);
  {
    enum file_existence result
      = (OS_file_existence_test_direct (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((result == file_doesnt_exist)
       ? SHARP_F
       : (result == file_does_exist)
       ? SHARP_T
       : FIXNUM_ZERO);
  }
}

DEFINE_PRIMITIVE ("FILE-TYPE-DIRECT", Prim_file_type_direct, 1, 1,
  "Return type of FILE, as an exact non-negative integer.\n\
Don't indirect through symbolic links.")
{
  PRIMITIVE_HEADER (1);
  {
    enum file_type t = (OS_file_type_direct (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((t == file_type_nonexistent)
       ? SHARP_F
       : (ulong_to_integer ((unsigned long) t)));
  }
}

DEFINE_PRIMITIVE ("FILE-TYPE-INDIRECT", Prim_file_type_indirect, 1, 1,
  "Return type of FILE, as an exact non-negative integer.\n\
Indirect through symbolic links.")
{
  PRIMITIVE_HEADER (1);
  {
    enum file_type t = (OS_file_type_indirect (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((t == file_type_nonexistent)
       ? SHARP_F
       : (ulong_to_integer ((unsigned long) t)));
  }
}

DEFINE_PRIMITIVE ("FILE-ACCESS", Prim_file_access, 2, 2,
  "Return #T iff FILENAME exists and is accessible according to MODE.\n\
MODE is an integer between 0 and 7 inclusive, bitwise encoded:\n\
  4 ==> file is readable;\n\
  2 ==> file is writable;\n\
  1 ==> file is executable.")
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT
     (OS_file_access ((STRING_ARG (1)), (arg_index_integer (2, 8)))));
}

DEFINE_PRIMITIVE ("FILE-DIRECTORY?", Prim_file_directory_p, 1, 1,
  "Return #T iff FILENAME refers to an existing directory.\n\
Otherwise #F is returned, meaning either that FILENAME doesn't exist\n\
 or that it isn't a directory.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (OS_file_directory_p (STRING_ARG (1))));
}

DEFINE_PRIMITIVE ("FILE-SOFT-LINK?", Prim_file_soft_link_p, 1, 1,
  "Iff FILENAME refers to an existing soft link, return the link contents.\n\
Otherwise #F is returned, meaning either that FILENAME doesn't exist\n\
 or that it isn't a soft link.")
{
  PRIMITIVE_HEADER (1);
  STRING_RESULT (OS_file_soft_link_p (STRING_ARG (1)));
}

DEFINE_PRIMITIVE ("FILE-REMOVE", Prim_file_remove, 1, 1,
  "Delete file FILENAME.\n\
If FILENAME is a soft link, the link is deleted.")
{
  PRIMITIVE_HEADER (1);
  OS_file_remove (STRING_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FILE-REMOVE-LINK", Prim_file_remove_link, 1, 1,
  "If file FILENAME is a link to another file (hard or soft), remove it.")
{
  PRIMITIVE_HEADER (1);
  OS_file_remove_link (STRING_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FILE-RENAME", Prim_file_rename, 2, 2,
  "Rename file FROM-NAME to TO-NAME.")
{
  PRIMITIVE_HEADER (2);
  OS_file_rename ((STRING_ARG (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FILE-LINK-HARD", Prim_file_link_hard, 2, 2,
  "Create a hard link from file FROM-NAME to file TO-NAME.\n\
TO-NAME becomes another name for the file FROM-NAME.")
{
  PRIMITIVE_HEADER (2);
  OS_file_link_hard ((STRING_ARG (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("FILE-LINK-SOFT", Prim_file_link_soft, 2, 2,
  "Create a soft link from file FROM-NAME to file TO-NAME.\n\
TO-NAME becomes a soft link containing the string FROM-NAME.")
{
  PRIMITIVE_HEADER (2);
  OS_file_link_soft ((STRING_ARG (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("LINK-FILE", Prim_link_file, 3, 3,
  "This is an obsolete primitive.  Use `file-link-hard' or `file-link-soft'.\n\
Create a new name for file FROM-NAME, called TO-NAME.\n\
If third arg HARD? is #F, a soft link is created;\n\
 otherwise a hard link is created.")
{
  PRIMITIVE_HEADER (3);
  {
    const char * from_name = (STRING_ARG (1));
    const char * to_name = (STRING_ARG (2));
    if ((ARG_REF (3)) != SHARP_F)
      OS_file_link_hard (from_name, to_name);
    else
      OS_file_link_soft (from_name, to_name);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#ifndef FILE_COPY_BUFFER_LENGTH
#  define FILE_COPY_BUFFER_LENGTH 8192
#endif

int
OS_channel_copy (off_t source_length,
       Tchannel source_channel,
       Tchannel destination_channel)
{
  char buffer [FILE_COPY_BUFFER_LENGTH];
  off_t transfer_length =
    ((source_length > (sizeof (buffer))) ? (sizeof (buffer)) : source_length);

  while (source_length > 0)
  {
    long nread =
      (OS_channel_read (source_channel, buffer, transfer_length));
    if (nread <= 0)
    {
      return (-1);
    }
    if ((OS_channel_write (destination_channel, buffer, nread)) <
	nread)
    {
      return (-1);
    }
    source_length -= nread;
    if (source_length < (sizeof (buffer)))
      transfer_length = source_length;
  }
  return (0);
}

DEFINE_PRIMITIVE ("FILE-COPY", Prim_file_copy, 2, 2,
  "Make a new copy of the file FROM-NAME, called TO-NAME.")
{
  PRIMITIVE_HEADER (2);
  OS_file_copy ((STRING_ARG (1)), (STRING_ARG (2)));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DIRECTORY-MAKE", Prim_directory_make, 1, 1,
  "Create a new directory, called NAME.")
{
  PRIMITIVE_HEADER (1);
  OS_directory_make (STRING_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DIRECTORY-DELETE", Prim_directory_delete, 1, 1,
  "Delete directory called NAME.")
{
  PRIMITIVE_HEADER (1);
  OS_directory_delete (STRING_ARG (1));
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
  {
    int rc = (OS_file_touch ((const char *) (STRING_ARG (1))));
    if (rc < 0)
      error_bad_range_arg (1);
    PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (rc));
  }
}

DEFINE_PRIMITIVE ("NEW-DIRECTORY-OPEN", Prim_new_directory_open, 1, 1,
  "Open the directory NAME for reading, returning a directory number.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (OS_directory_open (STRING_ARG (1))));
}

static unsigned int
arg_directory_index (unsigned int argument)
{
  unsigned int index = (arg_ulong_integer (argument));
  if (!OS_directory_valid_p (index))
    error_bad_range_arg (argument);
  return (index);
}

DEFINE_PRIMITIVE ("NEW-DIRECTORY-CLOSE", Prim_new_directory_close, 1, 1,
  "Close DIRECTORY.")
{
  PRIMITIVE_HEADER (1);
  OS_directory_close (arg_directory_index (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("NEW-DIRECTORY-READ", Prim_new_directory_read, 1, 1,
  "Read and return a filename from DIRECTORY, or #F if no more files.")
{
  PRIMITIVE_HEADER (1);
  STRING_RESULT (OS_directory_read (arg_directory_index (1)));
}

DEFINE_PRIMITIVE ("NEW-DIRECTORY-READ-MATCHING", Prim_new_directory_read_match, 2, 2,
  "Read and return a filename from DIRECTORY.\n\
The filename must begin with the STRING.\n\
Return #F if there are no more matching files in the directory.")
{
  PRIMITIVE_HEADER (2);
  STRING_RESULT
    (OS_directory_read_matching ((arg_directory_index (1)), (STRING_ARG (2))));
}
