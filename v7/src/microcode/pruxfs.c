/* -*-C-*-

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/pruxfs.c,v 9.21 1987/01/22 14:34:49 jinx Exp $

   Simple unix primitives.

*/

#include <pwd.h>
#include "scheme.h"
#include "primitive.h"

/* Looks up in the user's shell environment the value of the 
   variable specified as a string. */

Define_Primitive( Prim_get_environment_variable, 1, "GET-ENVIRONMENT-VARIABLE")
{
  char *variable_value;
  extern char *getenv();
  Primitive_1_Arg();

  Arg_1_Type( TC_CHARACTER_STRING);
  variable_value = getenv( Scheme_String_To_C_String( Arg1));
  return ((variable_value == NULL)
	  ? NIL
	  : C_String_To_Scheme_String( variable_value));
}

Define_Primitive( Prim_get_user_name, 0, "CURRENT-USER-NAME")
{
  char *user_name;
  char *getlogin();
  Primitive_0_Args();

  user_name = getlogin();
  if (user_name == NULL)
    {
      unsigned short getuid();
      struct passwd *entry;
      struct passwd *getpwuid();
      
      entry = getpwuid( getuid());
      if (entry == NULL)
	Primitive_Error( ERR_EXTERNAL_RETURN);
      user_name = entry->pw_name;
    }
  return (C_String_To_Scheme_String( user_name));
}

Define_Primitive( Prim_get_user_home_directory, 1, "GET-USER-HOME-DIRECTORY")
{
  struct passwd *entry;
  struct passwd *getpwnam();
  Primitive_1_Arg();

  Arg_1_Type( TC_CHARACTER_STRING);
  entry = getpwnam( Scheme_String_To_C_String( Arg1));
  return ((entry == NULL)
	  ? NIL
	  : C_String_To_Scheme_String( entry->pw_dir));
}
