/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/prosfs.c,v 1.6 1992/01/20 17:29:30 jinx Exp $

Copyright (c) 1987-1992 Massachusetts Institute of Technology

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

/* Primitives to perform file-system operations. */

#include "scheme.h"
#include "prims.h"
#include "osfile.h"
#include "osfs.h"
#include "osio.h"

extern int EXFUN (OS_channel_copy,
		  (off_t source_length,
		   Tchannel source_channel,
		   Tchannel destination_channel));

#define STRING_RESULT(expression)					\
{									\
  CONST char * result = (expression);					\
  PRIMITIVE_RETURN							\
    ((result == 0)							\
     ? SHARP_F								\
     : (char_pointer_to_string ((unsigned char *) result)));		\
}

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
    CONST char * from_name = (STRING_ARG (1));
    CONST char * to_name = (STRING_ARG (2));
    if ((ARG_REF (3)) != SHARP_F)
      OS_file_link_hard (from_name, to_name);
    else
      OS_file_link_soft (from_name, to_name);
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#ifndef FILE_COPY_BUFFER_LENGTH
#define FILE_COPY_BUFFER_LENGTH 8192
#endif

int
DEFUN (OS_channel_copy, (source_length, source_channel, destination_channel),
       off_t source_length AND
       Tchannel source_channel AND
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

DEFINE_PRIMITIVE ("DIRECTORY-OPEN", Prim_directory_open, 1, 1,
  "Open the directory NAME for reading.\n\
If successful, return the first filename in the directory as a string.\n\
If there is no such file, #F is returned.")
{
  PRIMITIVE_HEADER (1);
  OS_directory_open (STRING_ARG (1));
  STRING_RESULT (OS_directory_read ());
}

DEFINE_PRIMITIVE ("DIRECTORY-OPEN-NOREAD", Prim_directory_open_noread, 1, 1,
  "Open the directory NAME for reading.")
{
  PRIMITIVE_HEADER (1);
  OS_directory_open (STRING_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DIRECTORY-READ", Prim_directory_read, 0, 0,
  "Read and return a filename from the directory opened by `directory-open'.\n\
Return #F if there are no more files in the directory.")
{
  PRIMITIVE_HEADER (0);
  STRING_RESULT (OS_directory_read ());
}

DEFINE_PRIMITIVE ("DIRECTORY-READ-MATCHING", Prim_directory_read_matching, 1, 1,
  "Read and return a filename from the directory opened by `directory-open'.\n\
The filename must begin with the argument string.\n\
Return #F if there are no more matching files in the directory.")
{
  PRIMITIVE_HEADER (1);
  STRING_RESULT (OS_directory_read_matching (STRING_ARG (1)));
}

DEFINE_PRIMITIVE ("DIRECTORY-CLOSE", Prim_directory_close, 0, 0,
  "Close the directory opened by `directory-open'.")
{
  PRIMITIVE_HEADER (0);
  OS_directory_close ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}
