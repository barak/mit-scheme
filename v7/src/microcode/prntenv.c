/* -*-C-*-

$Id: prntenv.c,v 1.8 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1993-1999 Massachusetts Institute of Technology

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

/* Unix-specific process-environment primitives. */
/* DOS imitation */

#include "scheme.h"
#include "prims.h"
#include "nt.h"
#include "ntio.h"

DEFINE_PRIMITIVE ("FILE-TIME->STRING", Prim_file_time_to_string, 1, 1,
  "Convert a file system time stamp into a date/time string.")
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, INTEGER_P);
  {
    time_t clock = (arg_integer (1));
    char * time_string = (ctime (&clock));
    if (time_string == 0)
      PRIMITIVE_RETURN (SHARP_F);
    (time_string[24]) = '\0';
    PRIMITIVE_RETURN (char_pointer_to_string ((unsigned char *) time_string));
  }
}

DEFINE_PRIMITIVE ("GET-ENVIRONMENT-VARIABLE", Prim_get_environment_variable, 1, 1,
  "Look up the value of a variable in the user's shell environment.\n\
The argument, a variable name, must be a string.\n\
The result is either a string (the variable's value),\n\
 or #F indicating that the variable does not exist.")
{
  PRIMITIVE_HEADER (1);
  {
    CONST char * variable_value = (getenv (STRING_ARG (1)));
    PRIMITIVE_RETURN
      ((variable_value == 0)
       ? SHARP_F
       : (char_pointer_to_string ((unsigned char *) variable_value)));
  }
}

#define VQRESULT(index, value)						\
  VECTOR_SET (result, index, (ulong_to_integer (value)))


DEFINE_PRIMITIVE ("WIN32-VIRTUAL-QUERY", Prim_win32_virtual_query, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    MEMORY_BASIC_INFORMATION info;
    SCHEME_OBJECT result;
    (void) VirtualQuery
      (((LPCVOID) (arg_ulong_integer (1))), (&info), (sizeof (info)));
    result = (allocate_marked_vector (TC_VECTOR, 7, 1));
    VQRESULT (0, ((unsigned long) (info.BaseAddress)));
    VQRESULT (1, ((unsigned long) (info.AllocationBase)));
    VQRESULT (2, (info.AllocationProtect));
    VQRESULT (3, (info.RegionSize));
    VQRESULT (4, (info.State));
    VQRESULT (5, (info.Protect));
    VQRESULT (6, (info.Type));
    PRIMITIVE_RETURN (result);
  }
}
