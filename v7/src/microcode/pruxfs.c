/* -*-C-*-

$Id: pruxfs.c,v 9.56 2000/12/05 21:23:47 cph Exp $

Copyright (c) 1987-2000 Massachusetts Institute of Technology

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

/* Unix-specific file-system primitives. */

#include "scheme.h"
#include "prims.h"
#include "ux.h"
#include "osfs.h"

extern int EXFUN
  (UX_read_file_status, (CONST char * filename, struct stat * s));
extern int EXFUN
  (UX_read_file_status_indirect, (CONST char * filename, struct stat * s));
extern CONST char * EXFUN (UX_file_system_type, (CONST char * name));

static SCHEME_OBJECT EXFUN (file_attributes_internal, (struct stat * s));
static void EXFUN (file_mode_string, (struct stat * s, char * a));
static char EXFUN (file_type_letter, (struct stat * s));
static void EXFUN (rwx, (unsigned short bits, char * chars));

DEFINE_PRIMITIVE ("FILE-MODES", Prim_file_modes, 1, 1,
  "Return mode bits of FILE, as an integer.")
{
  struct stat stat_result;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    ((UX_read_file_status_indirect ((STRING_ARG (1)), (&stat_result)))
     ? (LONG_TO_UNSIGNED_FIXNUM ((stat_result . st_mode) & 07777))
     : SHARP_F);
}

DEFINE_PRIMITIVE ("SET-FILE-MODES!", Prim_set_file_modes, 2, 2,
  "Set the mode bits of FILE to MODE.")
{
  PRIMITIVE_HEADER (2);
  if ((UX_chmod ((STRING_ARG (1)), (arg_index_integer (2, 010000)))) < 0)
    error_system_call (errno, syscall_chmod);
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("FILE-MOD-TIME", Prim_file_mod_time, 1, 1, 0)
{
  struct stat s;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    ((UX_read_file_status ((STRING_ARG (1)), (&s)))
     ? (long_to_integer (s . st_mtime))
     : SHARP_F);
}

DEFINE_PRIMITIVE ("FILE-MOD-TIME-INDIRECT", Prim_file_mod_time_indirect, 1, 1, 0)
{
  struct stat s;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    ((UX_read_file_status_indirect ((STRING_ARG (1)), (&s)))
     ? (long_to_integer (s . st_mtime))
     : SHARP_F);
}

DEFINE_PRIMITIVE ("FILE-ACCESS-TIME", Prim_file_acc_time, 1, 1, 0)
{
  struct stat s;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    ((UX_read_file_status ((STRING_ARG (1)), (&s)))
     ? (long_to_integer (s . st_atime))
     : SHARP_F);
}

DEFINE_PRIMITIVE ("FILE-ACCESS-TIME-INDIRECT", Prim_file_acc_time_indirect, 1, 1, 0)
{
  struct stat s;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    ((UX_read_file_status_indirect ((STRING_ARG (1)), (&s)))
     ? (long_to_integer (s . st_atime))
     : SHARP_F);
}

DEFINE_PRIMITIVE ("SET-FILE-TIMES!", Prim_set_file_times, 3, 3,
  "Change the access and modification times of FILE.\n\
The second and third arguments are the respective times.\n\
The file must exist and you must be the owner (or superuser).")
{
  struct utimbuf times;
  PRIMITIVE_HEADER (3);
  times.actime = (arg_nonnegative_integer (2));
  times.modtime = (arg_nonnegative_integer (3));
  STD_VOID_SYSTEM_CALL
    (syscall_utime, (UX_utime ((STRING_ARG (1)), (&times))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Returns a vector of 10 items:

   0 = #T iff the file is a directory,
       string (name linked to) for symbolic link,
       #F for all other files.
   1 = number of links to the file
   2 = user id, as an unsigned integer
   3 = group id, as an unsigned integer
   4 = last access time of the file
   5 = last modification time of the file
   6 = last change time of the file
   7 = size of the file in bytes
   8 = mode string for the file
   9 = inode number of the file

   The file_mode_string stuff was gobbled from GNU Emacs. */

#define FILE_ATTRIBUTES_PRIMITIVE(stat_syscall)				\
{									\
  struct stat s;							\
  PRIMITIVE_HEADER (1);							\
  PRIMITIVE_RETURN							\
    ((stat_syscall ((STRING_ARG (1)), (&s)))				\
     ? (file_attributes_internal (&s))					\
     : SHARP_F);							\
}

DEFINE_PRIMITIVE ("FILE-ATTRIBUTES", Prim_file_attributes, 1, 1,
  "Given a file name, return attribute information about the file.\n\
If the file exists and its status information is accessible, the result\n\
is a vector of 10 items (see the reference manual for details).  Otherwise\n\
the result is #F.")
     FILE_ATTRIBUTES_PRIMITIVE (UX_read_file_status)

DEFINE_PRIMITIVE ("FILE-ATTRIBUTES-INDIRECT", Prim_file_attributes_indirect, 1, 1,
  "Like FILE-ATTRIBUTES but indirect through symbolic links.")
     FILE_ATTRIBUTES_PRIMITIVE (UX_read_file_status_indirect)

static SCHEME_OBJECT
DEFUN (file_attributes_internal, (s), struct stat * s)
{
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 10, true));
  SCHEME_OBJECT modes = (allocate_string (10));
  switch ((s -> st_mode) & S_IFMT)
    {
    case S_IFDIR:
      VECTOR_SET (result, 0, SHARP_T);
      break;
#ifdef S_IFLNK
    case S_IFLNK:
      VECTOR_SET (result, 0,
		  (char_pointer_to_string
		   ((unsigned char *)
		    (OS_file_soft_link_p
		     ((CONST char *) (STRING_LOC ((ARG_REF (1)), 0)))))));
      break;
#endif
    default:
      VECTOR_SET (result, 0, SHARP_F);
      break;
    }
  VECTOR_SET (result, 1, (long_to_integer (s -> st_nlink)));
  VECTOR_SET (result, 2, (long_to_integer (s -> st_uid)));
  VECTOR_SET (result, 3, (long_to_integer (s -> st_gid)));
  VECTOR_SET (result, 4, (long_to_integer (s -> st_atime)));
  VECTOR_SET (result, 5, (long_to_integer (s -> st_mtime)));
  VECTOR_SET (result, 6, (long_to_integer (s -> st_ctime)));
  VECTOR_SET (result, 7, (long_to_integer (s -> st_size)));
  file_mode_string (s, ((char *) (STRING_LOC (modes, 0))));
  VECTOR_SET (result, 8, modes);
  VECTOR_SET (result, 9, (long_to_integer (s -> st_ino)));
  return (result);
}

/* file_mode_string - set file attribute data

   File_mode_string converts the data in the st_mode field of file
   status block `s' to a 10 character attribute string, which it
   stores in the block that `a' points to.

   This attribute string is modelled after the string produced by the
   Berkeley ls.

   As usual under Unix, the elements of the string are numbered from
   0.  Their meanings are:

   0	File type.  'd' for directory, 'c' for character special, 'b'
	for block special, 'm' for multiplex, 'l' for symbolic link,
	's' for socket, 'p' for fifo, '-' for any other file type
   1	'r' if the owner may read, '-' otherwise.
   2	'w' if the owner may write, '-' otherwise.
   3	'x' if the owner may execute, 's' if the file is set-user-id,
	'-' otherwise.  'S' if the file is set-user-id, but the
	execute bit isn't set.  (sys V `feature' which helps to catch
	screw case.)
   4	'r' if group members may read, '-' otherwise.
   5	'w' if group members may write, '-' otherwise.
   6	'x' if group members may execute, 's' if the file is
	set-group-id, '-' otherwise.  'S' if it is set-group-id but
	not executable.
   7	'r' if any user may read, '-' otherwise.
   8	'w' if any user may write, '-' otherwise.
   9	'x' if any user may execute, 't' if the file is "sticky" (will
	be retained in swap space after execution), '-' otherwise. */

static void
DEFUN (file_mode_string, (s, a), struct stat * s AND char * a)
{
  (a[0]) = (file_type_letter (s));
  rwx ((((s -> st_mode) & 0700) << 0), (& (a [1])));
  rwx ((((s -> st_mode) & 0070) << 3), (& (a [4])));
  rwx ((((s -> st_mode) & 0007) << 6), (& (a [7])));
#ifdef S_ISUID
  if (((s -> st_mode) & S_ISUID) != 0)
    (a[3]) = (((a[3]) == 'x') ? 's' : 'S');
#endif
#ifdef S_ISGID
  if (((s -> st_mode) & S_ISGID) != 0)
    (a[6]) = (((a [6]) == 'x') ? 's' : 'S');
#endif
#ifdef S_ISVTX
  if (((s -> st_mode) & S_ISVTX) != 0)
    (a[9]) = (((a [9]) == 'x') ? 't' : 'T');
#endif
}

static char
DEFUN (file_type_letter, (s), struct stat * s)
{
  switch ((s -> st_mode) & S_IFMT)
    {
    case S_IFDIR:
      return ('d');
    case S_IFCHR:
      return ('c');
    case S_IFBLK:
      return ('b');
#ifdef S_IFLNK
    case S_IFLNK:
      return ('l');
#endif
#ifdef S_IFMPC
/* These do not seem to exist */
    case S_IFMPC:
    case S_IFMPB:
      return ('m');
#endif
#ifdef S_IFSOCK
    case S_IFSOCK:
      return ('s');
#endif
#ifdef S_IFIFO
    case S_IFIFO:
      return ('p');
#endif
#ifdef S_IFNWK /* hp-ux hack */
    case S_IFNWK:
      return ('n');
#endif
    default:
      return ('-');
    }
}

static void
DEFUN (rwx, (bits, chars), unsigned short bits AND char * chars)
{
  (chars[0]) = (((bits & S_IRUSR) != 0) ? 'r' : '-');
  (chars[1]) = (((bits & S_IWUSR) != 0) ? 'w' : '-');
  (chars[2]) = (((bits & S_IXUSR) != 0) ? 'x' : '-');
}

DEFINE_PRIMITIVE ("FILE-EQ?", Prim_file_eq_p, 2, 2,
  "True iff the two file arguments are the same file.")
{
  PRIMITIVE_HEADER (2);
  {
    struct stat s1;
    struct stat s2;
    PRIMITIVE_RETURN
      (BOOLEAN_TO_OBJECT
       ((UX_read_file_status ((STRING_ARG (1)), (&s1)))
	&& (UX_read_file_status ((STRING_ARG (2)), (&s2)))
	&& ((s1 . st_dev) == (s2 . st_dev))
	&& ((s1 . st_ino) == (s2 . st_ino))));
  }
}

DEFINE_PRIMITIVE ("FILE-SYSTEM-TYPE", Prim_file_system_type, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    CONST char * result = (UX_file_system_type (STRING_ARG (1)));
    PRIMITIVE_RETURN
      (char_pointer_to_string
       ((unsigned char *) ((result == 0) ? "unknown" : result)));
  }
}
