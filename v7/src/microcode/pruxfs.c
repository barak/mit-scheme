/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/pruxfs.c,v 9.44 1991/01/24 11:25:09 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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

/* Unix-specific file-system primitives. */

#include "scheme.h"
#include "prims.h"
#include "ux.h"
#include "osfs.h"

static SCHEME_OBJECT EXFUN (file_attributes_internal, (struct stat * s));
static void EXFUN (file_mode_string, (struct stat * s, char * a));
static char EXFUN (file_type_letter, (struct stat * s));
static void EXFUN (rwx, (unsigned short bits, char * chars));
static SCHEME_OBJECT EXFUN (file_touch, (CONST char * filename));
static void EXFUN (protect_fd, (int fd));

#ifndef FILE_TOUCH_OPEN_TRIES
#define FILE_TOUCH_OPEN_TRIES 5
#endif

DEFINE_PRIMITIVE ("FILE-MODES", Prim_file_modes, 1, 1,
  "Return mode bits of FILE, as an integer.")
{
  struct stat stat_result;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (((UX_stat ((STRING_ARG (1)), (&stat_result))) < 0)
     ? SHARP_F
     : (LONG_TO_UNSIGNED_FIXNUM ((stat_result . st_mode) & 07777)));
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
    (((UX_lstat ((STRING_ARG (1)), (&s))) < 0)
     ? SHARP_F
     : (long_to_integer (s . st_mtime)));
}

DEFINE_PRIMITIVE ("FILE-MOD-TIME-INDIRECT", Prim_file_mod_time_indirect, 1, 1, 0)
{
  struct stat s;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (((UX_stat ((STRING_ARG (1)), (&s))) < 0)
     ? SHARP_F
     : (long_to_integer (s . st_mtime)));
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
    (((stat_syscall ((STRING_ARG (1)), (&s))) < 0)			\
     ? SHARP_F								\
     : (file_attributes_internal (&s)));				\
}

DEFINE_PRIMITIVE ("FILE-ATTRIBUTES", Prim_file_attributes, 1, 1,
  "Given a file name, return attribute information about the file.\n\
If the file exists and its status information is accessible, the result\n\
is a vector of 10 items (see the reference manual for details).  Otherwise\n\
the result is #F.")
     FILE_ATTRIBUTES_PRIMITIVE (UX_lstat)

DEFINE_PRIMITIVE ("FILE-ATTRIBUTES-INDIRECT", Prim_file_attributes_indirect, 1, 1,
  "Like FILE-ATTRIBUTES but indirect through symbolic links.")
     FILE_ATTRIBUTES_PRIMITIVE (UX_stat)

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
		   (OS_file_soft_link_p
		    ((CONST char *) (STRING_LOC ((ARG_REF (1)), 0))))));
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
  (chars[0]) = (((bits & S_IREAD) != 0)  ? 'r' : '-');
  (chars[1]) = (((bits & S_IWRITE) != 0) ? 'w' : '-');
  (chars[2]) = (((bits & S_IEXEC) != 0)  ? 'x' : '-');
}

DEFINE_PRIMITIVE ("FILE-TOUCH", Prim_file_touch, 1, 1,
  "Given a file name, change the times of the file to the current time.\n\
If the file does not exist, create it.\n\
Both the access time and modification time are changed.\n\
Return #F if the file existed and its time was modified.\n\
Otherwise the file did not exist and it was created.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (file_touch ((CONST char *) (STRING_ARG (1))));
}

static SCHEME_OBJECT
DEFUN (file_touch, (filename), CONST char * filename)
{
  int fd;
  transaction_begin ();
  {
    unsigned int count = 0;
    while (1)
      {
	count += 1;
	/* Use O_EXCL to prevent overwriting existing file. */
	fd = (UX_open (filename, (O_RDWR | O_CREAT | O_EXCL), MODE_REG));
	if (fd >= 0)
	  {
	    protect_fd (fd);
	    transaction_commit ();
	    return (SHARP_T);
	  }
	if (errno == EEXIST)
	  {
	    fd = (UX_open (filename, O_RDWR, MODE_REG));
	    if (fd >= 0)
	      {
		protect_fd (fd);
		break;
	      }
	    else if ((errno == ENOENT) || (errno == ESTALE))
	      continue;
	  }
	if (count >= FILE_TOUCH_OPEN_TRIES)
	  error_system_call (errno, syscall_open);
      }
  }
  {
    struct stat file_status;
    STD_VOID_SYSTEM_CALL (syscall_fstat, (UX_fstat (fd, (&file_status))));
    if (((file_status . st_mode) & S_IFMT) != S_IFREG)
      error_bad_range_arg (1);
    /* CASE 3: file length of 0 needs special treatment. */
    if ((file_status . st_size) == 0)
      {
	char buf [1];
	(buf[0]) = '\0';
	STD_VOID_SYSTEM_CALL (syscall_write, (UX_write (fd, buf, 1)));
#ifdef HAVE_TRUNCATE
	STD_VOID_SYSTEM_CALL (syscall_ftruncate, (UX_ftruncate (fd, 0)));
	transaction_commit ();
#else /* not HAVE_TRUNCATE */
	transaction_commit ();
	fd = (UX_open (filename, (O_WRONLY | O_TRUNC), MODE_REG));
	if (fd >= 0)
	  STD_VOID_SYSTEM_CALL (syscall_close, (UX_close (fd)));
#endif /* HAVE_TRUNCATE */
	return (SHARP_F);
      }
  }
  /* CASE 4: read, then write back the first byte in the file. */
  {
    char buf [1];
    int scr;
    STD_UINT_SYSTEM_CALL (syscall_read, scr, (UX_read (fd, buf, 1)));
    if (scr > 0)
      {
	STD_VOID_SYSTEM_CALL (syscall_lseek, (UX_lseek (fd, 0, SEEK_SET)));
	STD_VOID_SYSTEM_CALL (syscall_write, (UX_write (fd, buf, 1)));
      }
  }
  transaction_commit ();
  return (SHARP_F);
}

static void
DEFUN (protect_fd_close, (ap), PTR ap)
{
  UX_close (* ((int *) ap));
}

static void
DEFUN (protect_fd, (fd), int fd)
{
  int * p = (dstack_alloc (sizeof (int)));
  (*p) = fd;
  transaction_record_action (tat_always, protect_fd_close, p);
}
