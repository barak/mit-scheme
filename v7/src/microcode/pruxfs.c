/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/pruxfs.c,v 9.39 1990/04/12 22:51:15 jinx Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

/* Simple unix primitives. */

#include "scheme.h"
#include "prims.h"
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef bsd
#include <sys/time.h>
#include <sys/file.h>
#else
#include <time.h>
#include <fcntl.h>
#endif
#ifdef hpux
#include <unistd.h>
#endif
#include <errno.h>
extern int errno;

/* This returns the string that `perror' would have printed, except
   that it is not terminated by a newline.  */

SCHEME_OBJECT
system_error_message (s)
     char * s;
{
  extern char * sys_errlist [];
  extern int sys_nerr;
  char * error_message;
  char unknown_error [64];
  extern char * malloc ();
  SCHEME_OBJECT result;

  if ((errno >= 0) && (errno <= sys_nerr))
    error_message = (sys_errlist [errno]);
  else
    {
      sprintf (unknown_error, "Unknown error %d", errno);
      error_message = unknown_error;
    }
  if (s == NULL)
    {
      result = (allocate_string (strlen (error_message)));
      strcpy ((STRING_LOC (result, 0)), error_message);
    }
  else
    {
      result = (allocate_string ((strlen (s)) + (strlen (error_message)) + 2));
      sprintf ((STRING_LOC (result, 0)), "%s: %s", s, error_message);
    }
  return (result);
}

DEFINE_PRIMITIVE ("GET-ENVIRONMENT-VARIABLE", Prim_get_environment_variable, 1, 1,
  "Looks up the value of a variable in the user's shell environment.\n\
The argument, a variable name, must be a string.\n\
The result is either a string (the variable's value),\n\
or #F indicating that the variable does not exist.")
{
  char * variable_value;
  extern char * getenv ();
  PRIMITIVE_HEADER (1);
  variable_value = (getenv (STRING_ARG (1)));
  PRIMITIVE_RETURN
    ((variable_value == ((char *) 0))
     ? SHARP_F
     : (char_pointer_to_string (variable_value)));
}

DEFINE_PRIMITIVE ("CURRENT-USER-NAME", Prim_get_user_name, 0, 0,
  "Returns (as a string) the user name of the user running Scheme.")
{
  char * user_name;
  char * getlogin ();
  PRIMITIVE_HEADER (0);

  user_name = (getlogin ());
  if (user_name == NULL)
    {
      unsigned short getuid ();
      struct passwd *entry;
      struct passwd *getpwuid ();

      entry = (getpwuid (getuid ()));
      if (entry == NULL)
	error_external_return ();
      user_name = (entry -> pw_name);
    }
  PRIMITIVE_RETURN (char_pointer_to_string (user_name));
}

DEFINE_PRIMITIVE ("GET-USER-HOME-DIRECTORY", Prim_get_user_home_directory, 1, 1,
  "Returns the file name of a given user's home directory.\n\
The user name argument must be a string.\n\
The result is either the file name as a string,\n\
or #F indicating that no such user is known.")
{
  struct passwd * entry;
  struct passwd * getpwnam ();
  PRIMITIVE_HEADER (1);
  entry = (getpwnam (STRING_ARG (1)));
  PRIMITIVE_RETURN
    ((entry == ((struct passwd *) 0))
     ? SHARP_F
     : (char_pointer_to_string (entry -> pw_dir)));
}

DEFINE_PRIMITIVE ("CURRENT-FILE-TIME", Prim_current_file_time, 0, 0,
  "Returns the current file system time stamp.\n\
This is an integer whose units are in seconds.")
{
  extern long time ();
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (long_to_integer (time ((long *) 0)));
}

DEFINE_PRIMITIVE ("FILE-TIME->STRING", Prim_file_time_to_string, 1, 1,
  "Converts a file system time stamp into a date/time string.")
{
  long clock;
  char * time_string;
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, INTEGER_P);
  {
    fast SCHEME_OBJECT number = (ARG_REF (1));
    if (! (integer_to_long_p (number)))
      error_bad_range_arg (1);
    clock = (integer_to_long (number));
  }
  time_string = (ctime (& clock));
  if ((time_string [24]) == '\n')
    (time_string [24]) = '\0';
  PRIMITIVE_RETURN (char_pointer_to_string (time_string));
}

DEFINE_PRIMITIVE ("UID->STRING", Prim_uid_to_string, 1, 1,
  "Given a unix user ID number, returns the corresponding user name.\n\
If the argument is not a known user ID, returns #F.")
{
  struct passwd * getpwuid ();
  void endpwent ();
  struct passwd * entry;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, FIXNUM_P);
  entry = (getpwuid (arg_nonnegative_integer (1)));
  endpwent ();
  PRIMITIVE_RETURN
    ((entry == NULL)
     ? SHARP_F
     : (char_pointer_to_string (entry -> pw_name)));
}

DEFINE_PRIMITIVE ("GID->STRING", Prim_gid_to_string, 1, 1,
  "Given a unix group ID number, returns the corresponding group name.\n\
If the argument is not a known group ID, returns #F.")
{
  struct group * getgrgid ();
  void endgrent ();
  struct group * entry;
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, FIXNUM_P);
  entry = (getgrgid (arg_nonnegative_integer (1)));
  endgrent ();
  PRIMITIVE_RETURN
    ((entry == NULL)
     ? SHARP_F
     : (char_pointer_to_string (entry -> gr_name)));
}

DEFINE_PRIMITIVE ("FILE-DIRECTORY?", Prim_file_directory_p, 1, 1,
  "Returns #T if the argument file name is a directory;\n\
otherwise returns #F.")
{
  struct stat stat_result;
  PRIMITIVE_HEADER (1);

  if ((stat ((STRING_ARG (1)), (& stat_result))) < 0)
    PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN
    ((((stat_result . st_mode) & S_IFMT) == S_IFDIR) ? SHARP_T : SHARP_F);
}

DEFINE_PRIMITIVE ("FILE-MODES", Prim_file_modes, 1, 1,
  "Return mode bits of FILE, as an integer.")
{
  struct stat stat_result;
  PRIMITIVE_HEADER (1);

  if ((stat ((STRING_ARG (1)), (& stat_result))) < 0)
    PRIMITIVE_RETURN (SHARP_F);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM ((stat_result . st_mode) & 07777));
}

DEFINE_PRIMITIVE ("SET-FILE-MODES!", Prim_set_file_modes, 2, 2,
  "Return mode bits of FILE, as an integer.")
{
  PRIMITIVE_HEADER (2);
  if ((chmod ((STRING_ARG (1)), (arg_index_integer (2, 010000)))) < 0)
    error_external_return ();
  PRIMITIVE_RETURN (SHARP_F);
}

DEFINE_PRIMITIVE ("FILE-ACCESS", Prim_file_access, 2, 2, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (((access ((STRING_ARG (1)), (arg_index_integer (2, 8)))) >= 0)
     ? SHARP_T
     : SHARP_F);
}

DEFINE_PRIMITIVE ("CURRENT-UID", Prim_current_uid, 0, 0,
  "Return the effective uid of Scheme, as an integer.")
{
  unsigned short geteuid ();
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (geteuid ()));
}

DEFINE_PRIMITIVE ("CURRENT-GID", Prim_current_gid, 0, 0,
  "Return the effective gid of Scheme, as an integer.")
{
  unsigned short getegid ();
  PRIMITIVE_HEADER (0);

  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (getegid ()));
}

/* The following is originally from GNU Emacs. */

#ifdef S_IFLNK

static SCHEME_OBJECT
file_symlink_p (filename)
     SCHEME_OBJECT filename;
{
  char *buf;
  int bufsize;
  int valsize;
  SCHEME_OBJECT val;
  extern char *malloc ();
  extern void free ();

  bufsize = 100;
  while (1)
    {
      buf = (malloc (bufsize));
      if (buf == NULL)
	error_external_return ();
      valsize =
	(readlink ((STRING_LOC (filename, 0)), buf, bufsize));
      if (valsize < bufsize)
	break;
      /* Buffer was not long enough */
      free (buf);
      bufsize *= 2;
    }
  if (valsize < 0)
    {
      free (buf);
      return (SHARP_F);
    }
  (buf [valsize]) = '\0';
  val = (char_pointer_to_string (buf));
  free (buf);
  return (val);
}

#endif /* S_IFLNK */

DEFINE_PRIMITIVE ("FILE-SYMLINK?", Prim_file_symlink_p, 1, 1,
  "If FILENAME is a symbolic link, returns its contents;\n\
otherwise returns #F.")
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
#ifdef S_IFLNK
  PRIMITIVE_RETURN (file_symlink_p (ARG_REF (1)));
#else /* not S_IFLNK */
  PRIMITIVE_RETURN (SHARP_F);
#endif /* S_IFLNK */
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

   The filemodestring stuff was gobbled from GNU Emacs. */

static SCHEME_OBJECT file_attributes_internal ();
static void filemodestring ();
static void rwx ();
static void setst ();

/* If system does not have symbolic links, it does not have lstat.
   In that case, use ordinary stat instead.  */
#ifndef S_IFLNK
#define lstat stat
#endif

DEFINE_PRIMITIVE ("FILE-ATTRIBUTES", Prim_file_attributes, 1, 1,
  "Given a file name, returns attribute information about the file.\n\
If the file exists and its status information is accessible, the result\n\
is a vector of 10 items (see the reference manual for details).  Otherwise\n\
the result is #F.")
{
  struct stat stat_result;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (((lstat ((STRING_ARG (1)), (& stat_result))) < 0)
     ? SHARP_F
     : (file_attributes_internal (& stat_result)));
}

DEFINE_PRIMITIVE ("FILE-ATTRIBUTES-INDIRECT", Prim_file_attributes_indirect, 1, 1,
  "Like FILE-ATTRIBUTES except that it indirects through symbolic links.")
{
  struct stat stat_result;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (((stat ((STRING_ARG (1)), (& stat_result))) < 0)
     ? SHARP_F
     : (file_attributes_internal (& stat_result)));
}

static SCHEME_OBJECT
file_attributes_internal (stat_result)
     struct stat * stat_result;
{
  extern SCHEME_OBJECT allocate_marked_vector ();
  extern SCHEME_OBJECT allocate_string ();
  SCHEME_OBJECT result = (allocate_marked_vector (TC_VECTOR, 10, true));
  SCHEME_OBJECT modes = (allocate_string (10));
  switch ((stat_result -> st_mode) & S_IFMT)
    {
    case S_IFDIR:
      VECTOR_SET (result, 0, SHARP_T);
      break;
#ifdef S_IFLNK
    case S_IFLNK:
      VECTOR_SET (result, 0, (file_symlink_p (ARG_REF (1))));
      break;
#endif
    default:
      VECTOR_SET (result, 0, SHARP_F);
      break;
    }
  VECTOR_SET (result, 1, (long_to_integer (stat_result -> st_nlink)));
  VECTOR_SET (result, 2, (long_to_integer (stat_result -> st_uid)));
  VECTOR_SET (result, 3, (long_to_integer (stat_result -> st_gid)));
  VECTOR_SET (result, 4, (long_to_integer (stat_result -> st_atime)));
  VECTOR_SET (result, 5, (long_to_integer (stat_result -> st_mtime)));
  VECTOR_SET (result, 6, (long_to_integer (stat_result -> st_ctime)));
  VECTOR_SET (result, 7, (long_to_integer (stat_result -> st_size)));
  filemodestring (stat_result, (STRING_LOC (modes, 0)));
  VECTOR_SET (result, 8, modes);
  VECTOR_SET (result, 9, (long_to_integer (stat_result -> st_ino)));
  return (result);
}

/* filemodestring - set file attribute data

   Filemodestring converts the data in the st_mode field of file
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
	be retained in swap space after execution), '-' otherwise.
   */

static void
filemodestring (s, a)
   struct stat * s;
   char * a;
{
  extern char file_type_letter ();

  (a [0]) = (file_type_letter (s));
  /* Aren't there symbolic names for these byte-fields? */
  rwx ((((s -> st_mode) & 0700) << 0), (& (a [1])));
  rwx ((((s -> st_mode) & 0070) << 3), (& (a [4])));
  rwx ((((s -> st_mode) & 0007) << 6), (& (a [7])));
  setst ((s -> st_mode), a);
  return;
}

/* rwx - look at read, write, and execute bits and set character
   flags accordingly. */

static void
rwx (bits, chars)
   unsigned short bits;
   char chars[];
{
  (chars [0]) = ((bits & S_IREAD)  ? 'r' : '-');
  (chars [1]) = ((bits & S_IWRITE) ? 'w' : '-');
  (chars [2]) = ((bits & S_IEXEC)  ? 'x' : '-');
  return;
}

/* setst - set s & t flags in a file attributes string */

static void
setst (bits, chars)
   unsigned short bits;
   char chars[];
{
#ifdef S_ISUID
   if (bits & S_ISUID)
     (chars [3]) = (((chars [3]) == 'x') ? 's' : 'S');
#endif
#ifdef S_ISGID
   if (bits & S_ISGID)
     (chars [6]) = (((chars [6]) == 'x') ? 's' : 'S');
#endif
#ifdef S_ISVTX
   if (bits & S_ISVTX)
     (chars [9]) = (((chars [9]) == 'x') ? 't' : 'T');
#endif
   return;
}

DEFINE_PRIMITIVE ("SYSTEM", Prim_system, 1, 1,
  "Invokes sh (the Bourne shell) on the string argument.\n\
Waits until the shell terminates, then returns its exit status as an integer.")
{
  extern int system ();
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (long_to_integer (system (STRING_ARG (1))));
}

static SCHEME_OBJECT file_touch ();

DEFINE_PRIMITIVE ("FILE-TOUCH", Prim_file_touch, 1, 1,
  "Given a file name, changes the times of the file to the current time.\n\
If the file does not exist, creates it.\n\
Both the access time and modification time are changed.\n\
Returns #T if the file did not exist and it was created.\n\
Returns #F if the file existed and its time was modified.\n\
Otherwise it returns a unix error string.")
{
  PRIMITIVE_HEADER (1);

  PRIMITIVE_RETURN (file_touch (STRING_ARG (1)));
}

#define N_RETRIES 5

static SCHEME_OBJECT
file_touch (filename)
     char * filename;
{
  int result;
  struct stat file_status;
  int fd;
  char buf [1];
  int count;
  SCHEME_OBJECT ret_val;

  extern int ftruncate ();
  extern int lseek ();
  extern int open ();
  extern int read ();
  extern int stat ();
  extern int write ();

#if 0
/*
  IMPORTANT: Don't turn this code on without examining the code below
  carefully. The code below has changed since this stuff was last enabled!
 */
#ifdef bsd
  extern long time ();
  long current_time;
  struct timeval tvp [2];
  extern int utimes ();
#else /* not bsd */
#ifdef hpux
  extern int utime ();
#endif /* hpux */
#endif /* bsd */
#endif /* 0 */

  ret_val = SHARP_F;

  for (count = 0; true; count += 1)
  {
    /* Use O_EXCL to prevent overwriting existing file.
     */
    fd = (open (filename, (O_RDWR | O_CREAT | O_EXCL), 0666));
    if (fd >= 0)
    {
      ret_val = SHARP_T;
      goto zero_length_file;
    }
    else if (errno == EEXIST)
    {
      fd = (open (filename, O_RDWR, 0666));
      if (fd >= 0)
      {
	break;
      }
      else if ((errno != ENOENT) || (count >= N_RETRIES))
      {
	return (system_error_message ("open"));
      }
      /* The file disappeared between both opens.
	 Go around the loop again.
	 */
    }
    else if (count >= N_RETRIES)
    {
      return (system_error_message ("open"));
    }
  }

  result = fstat(fd, &file_status);
  if (result != 0)
    return(system_error_message("fstat"));

#if 0
/*
  IMPORTANT: Don't turn this code on without examining the code below
  carefully. The code below has changed since this stuff was last enabled!
 */

  /* Disable this code -- this is subject to clock skew problems
     when the file is on an NFS server.  */

  /* CASE 2: try utime (utimes) if it's available. */
#ifdef bsd

  current_time = (time (0));
  ((tvp [0]) . tv_sec) = current_time;
  ((tvp [0]) . tv_usec) = 0;
  ((tvp [1]) . tv_sec) = current_time;
  ((tvp [1]) . tv_usec) = 0;
  result = (utimes (filename, tvp));
  if (result == 0)
    return (ret_val);

#else /* not bsd */
#ifdef hpux

  result = (utime (filename, 0));
  if (result == 0)
    return (ret_val);

#endif /* hpux */
#endif /* bsd */

  /* utime (utimes) has failed, or does not exist.  Instead, open the
     file, read one byte, and write it back in place.  */

#endif /* 0 */

  if (((file_status . st_mode) & S_IFMT) != S_IFREG)
    return (char_pointer_to_string ("can only touch regular files"));

  /* CASE 3: file length of 0 needs special treatment. */
  if ((file_status . st_size) == 0)
    {
    zero_length_file:
      (buf [0]) = '\0';
      while (1)
	{
	  result = (write (fd, buf, 1));
	  if (result > 0) break;
	  if ((result < 0) && (errno != EINTR))
	    {
	      (void) close (fd);
	      return (system_error_message ("write"));
	    }
	}
      if ((lseek (fd, 0, 0)) != 0)
	{
	  (void) ftruncate (fd, 0);
	  (void) close (fd);
	  return (system_error_message ("lseek"));
	}
      while (1)
	{
	  result = (read (fd, buf, 1));
	  if (result > 0) break;
	  if (result == 0)
	    {
	      (void) ftruncate (fd, 0);
	      (void) close (fd);
	      return (char_pointer_to_string ("read: eof encountered"));
	    }
	  if ((result < 0) && (errno != EINTR))
	    {
	      (void) ftruncate (fd, 0);
	      (void) close (fd);
	      return (system_error_message ("read"));
	    }
	}
      if ((ftruncate (fd, 0)) != 0)
      {
	(void) close(fd);
	return (system_error_message ("ftruncate"));
      }
      if ((close (fd)) != 0)
	return (system_error_message ("close"));
      return (ret_val);
    }

  /* CASE 4: read, then write back the first byte in the file. */
  while (1)
    {
      result = (read (fd, buf, 1));
      if (result > 0) break;
      if (result == 0)
	{
	  (void) close (fd);
	  return (char_pointer_to_string ("read: eof encountered"));
	}
      if ((result < 0) && (errno != EINTR))
	{
	  (void) close (fd);
	  return (system_error_message ("read"));
	}
    }
  if ((lseek (fd, 0, 0)) != 0)
    {
      (void) close (fd);
      return (system_error_message ("lseek"));
    }
  while (1)
    {
      result = (write (fd, buf, 1));
      if (result > 0) break;
      if ((result < 0) && (errno != EINTR))
	{
	  (void) close (fd);
	  return (system_error_message ("write"));
	}
    }
  if ((close (fd)) != 0)
    return (system_error_message ("close"));
  return (ret_val);
}
