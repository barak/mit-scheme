/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/pruxfs.c,v 9.22 1987/07/18 03:04:11 cph Rel $

Copyright (c) 1987 Massachusetts Institute of Technology

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
#include "primitive.h"
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef bsd
#include <sys/time.h>
#else
#include <time.h>
#endif

/* Looks up in the user's shell environment the value of the 
   variable specified as a string. */

Define_Primitive (Prim_get_environment_variable, 1, "GET-ENVIRONMENT-VARIABLE")
{
  char *variable_value;
  extern char *getenv ();
  Primitive_1_Arg ();

  CHECK_ARG (1, STRING_P);
  variable_value = (getenv (Scheme_String_To_C_String (Arg1)));
  return
    ((variable_value == NULL)
     ? NIL
     : (C_String_To_Scheme_String (variable_value)));
}

Define_Primitive (Prim_get_user_name, 0, "CURRENT-USER-NAME")
{
  char *user_name;
  char *getlogin ();
  Primitive_0_Args ();

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
  return (C_String_To_Scheme_String (user_name));
}

Define_Primitive (Prim_get_user_home_directory, 1, "GET-USER-HOME-DIRECTORY")
{
  struct passwd *entry;
  struct passwd *getpwnam ();
  Primitive_1_Arg ();

  CHECK_ARG (1, STRING_P);
  entry = (getpwnam (Scheme_String_To_C_String (Arg1)));
  return
    ((entry == NULL)
     ? NIL
     : (C_String_To_Scheme_String (entry -> pw_dir)));
}

Define_Primitive (Prim_current_file_time, 0, "CURRENT-FILE-TIME")
{
  Primitive_0_Args ();

  return (C_Integer_To_Scheme_Integer (time ((long *) 0)));
}

Define_Primitive (Prim_file_time_to_string, 1, "FILE-TIME->STRING")
{
  long clock;
  long temp;
  char *time_string;
  Primitive_1_Arg ();

  CHECK_ARG (1, INTEGER_P);
  temp = (Scheme_Integer_To_C_Integer (Arg1, (& clock)));
  if (temp != PRIM_DONE)
    return (temp);
  time_string = (ctime (& clock));
  if ((time_string [24]) == '\n')
    (time_string [24]) = '\0';
  return (C_String_To_Scheme_String (time_string));
}

Define_Primitive (Prim_uid_to_string, 1, "UID->STRING")
{
  struct passwd *getpwuid ();
  void endpwent ();
  struct passwd *entry;
  Primitive_1_Args ();

  CHECK_ARG (1, FIXNUM_P);
  entry = (getpwuid (UNSIGNED_FIXNUM_VALUE (Arg1)));
  endpwent ();
  if (entry == NULL)
    error_bad_range_arg (1);
  return (C_String_To_Scheme_String (entry -> pw_name));
}

Define_Primitive (Prim_gid_to_string, 1, "GID->STRING")
{
  struct group *getgrgid ();
  void endgrent ();
  struct group *entry;
  Primitive_1_Args ();

  CHECK_ARG (1, FIXNUM_P);
  entry = (getgrgid (UNSIGNED_FIXNUM_VALUE (Arg1)));
  endgrent ();
  if (entry == NULL)
    error_bad_range_arg (1);
  return (C_String_To_Scheme_String (entry -> gr_name));
}

/* Returns a vector of 9 items:

   0 = #T iff the file is a directory
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

Define_Primitive (Prim_file_attributes, 1, "FILE-ATTRIBUTES")
{
  struct stat stat_result;
  Pointer result;
  static void filemodestring ();
  char modes[11];
  Primitive_1_Arg ();

  CHECK_ARG (1, STRING_P);
  if ((stat ((Scheme_String_To_C_String (Arg1)), (& stat_result))) < 0)
    return (NIL);
  Primitive_GC_If_Needed (10);
  result = (Make_Pointer (TC_VECTOR, Free));
  (*Free++) = (Make_Non_Pointer (TC_MANIFEST_VECTOR, 9));
  Free += 9;
  User_Vector_Set
    (result, 0,
     ((((stat_result . st_mode) & S_IFMT) == S_IFDIR) ? TRUTH : NIL));
  User_Vector_Set (result, 1, (MAKE_UNSIGNED_FIXNUM (stat_result . st_nlink)));
  User_Vector_Set (result, 2, (MAKE_UNSIGNED_FIXNUM (stat_result . st_uid)));
  User_Vector_Set (result, 3, (MAKE_UNSIGNED_FIXNUM (stat_result . st_gid)));
  User_Vector_Set
    (result, 4, (C_Integer_To_Scheme_Integer (stat_result . st_atime)));
  User_Vector_Set
    (result, 5, (C_Integer_To_Scheme_Integer (stat_result . st_mtime)));
  User_Vector_Set
    (result, 6, (C_Integer_To_Scheme_Integer (stat_result . st_ctime)));
  User_Vector_Set
    (result, 7, (C_Integer_To_Scheme_Integer (stat_result . st_size)));
  filemodestring ((& stat_result), modes);
  (modes [10]) = '\0';
  User_Vector_Set (result, 8, (C_String_To_Scheme_String (modes)));
  User_Vector_Set (result, 9, (MAKE_UNSIGNED_FIXNUM (stat_result . st_ino)));
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
   struct stat *s;
   char *a;
{
  static char ftypelet ();
  static void rwx (), setst ();

  a[0] = ftypelet (s);
  /* Aren't there symbolic names for these byte-fields? */
  rwx ((s->st_mode & 0700) << 0, &(a[1]));
  rwx ((s->st_mode & 0070) << 3, &(a[4]));
  rwx ((s->st_mode & 0007) << 6, &(a[7]));
  setst (s->st_mode, a);
  return;
}

/* ftypelet - file type letter

   Ftypelet accepts a file status block and returns a character code
   describing the type of the file.  'd' is returned for directories,
   'b' for block special files, 'c' for character special files, 'm'
   for multiplexor files, 'l' for symbolic link, 's' for socket, 'p'
   for fifo, '-' for any other file type */

static char
ftypelet (s)
   struct stat *s;
{
  switch (s->st_mode & S_IFMT)
    {
    default:
      return '-';
    case S_IFDIR:
      return 'd';
#ifdef S_IFLNK
    case S_IFLNK:
      return 'l';
#endif
#ifdef S_IFCHR
    case S_IFCHR:
      return 'c';
#endif
#ifdef S_IFBLK
    case S_IFBLK:
      return 'b';
#endif
#ifdef S_IFMPC
/* These do not seem to exist */
    case S_IFMPC:
    case S_IFMPB:
      return 'm';
#endif
#ifdef S_IFSOCK
    case S_IFSOCK:
      return 's';
#endif
#ifdef S_IFIFO
    case S_IFIFO:
      return 'p';
#endif
#ifdef S_IFNWK /* hp-ux hack */
    case S_IFNWK:
      return 'n';
#endif
    }
}

/* rwx - look at read, write, and execute bits and set character
   flags accordingly. */

static void
rwx (bits, chars)
   unsigned short bits;
   char chars[];
{
  chars[0] = (bits & S_IREAD)  ? 'r' : '-';
  chars[1] = (bits & S_IWRITE) ? 'w' : '-';
  chars[2] = (bits & S_IEXEC)  ? 'x' : '-';
}

/* setst - set s & t flags in a file attributes string */

static void
setst (bits, chars)
   unsigned short bits;
   char chars[];
{
#ifdef S_ISUID
   if (bits & S_ISUID)
     {
       if (chars[3] != 'x')
	 /* Screw case: set-uid, but not executable. */
	 chars[3] = 'S';
       else
	 chars[3] = 's';
     }
#endif
#ifdef S_ISGID
   if (bits & S_ISGID)
     {
       if (chars[6] != 'x')
	 /* Screw case: set-gid, but not executable. */
	 chars[6] = 'S';
       else
	 chars[6] = 's';
     }
#endif
#ifdef S_ISVTX
   if (bits & S_ISVTX)
      chars[9] = 't';
#endif
}
