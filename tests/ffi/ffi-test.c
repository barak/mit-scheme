/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

/* A test library; used to test the C/Unix FFI. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ffi-test.h"

static void *callback_data;
static TestDoubleCallback callback_func;

extern void
test_register_double (TestDoubleCallback callback, void *user_data)
{
  callback_func = callback;
  callback_data = user_data;
}

extern double
test_double (double d, TestStruct *s)
{
  if (!callback_data) return 0.0;
  return (d * callback_func (s->second, callback_data));
}

extern char *
test_string (char *stri, TestStruct *stru)
{
  int l1 = strlen (stri);
  int l2 = strlen (stru->fourth);
  char *s = malloc (3);
  snprintf (s, 3, "%d", l1 + l2);
  return (s);
}

extern TestStruct
test_struct (TestStruct s)
{
  s.second += strlen (s.fourth);
  return (s);
}

extern TestUnion
test_union (TestUnion u)
{
  u.d += 1.0;
  return (u);
}
