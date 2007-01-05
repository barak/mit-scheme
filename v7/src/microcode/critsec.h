/* -*-C-*-

$Id: critsec.h,v 1.8 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

/* Critical sections.
   There should be a stack of critical sections, each with a
   queue of hooks. */

extern char * critical_section_name;
extern int critical_section_hook_p;
extern void EXFUN ((*critical_section_hook), (char *));

#define DECLARE_CRITICAL_SECTION()					\
  char * critical_section_name = 0;					\
  int critical_section_hook_p;						\
  void (*critical_section_hook) ()

#define ENTER_CRITICAL_SECTION(name) critical_section_name = (name)
#define RENAME_CRITICAL_SECTION(name) critical_section_name = (name)

#define EXIT_CRITICAL_SECTION(code_if_hook)				\
{									\
  if (critical_section_hook_p)						\
    {									\
      code_if_hook;							\
      {									\
	char * name = critical_section_name;				\
	critical_section_hook_p = 0;					\
	critical_section_name = 0;					\
	(*critical_section_hook) (name);				\
      }									\
    }									\
  else									\
    critical_section_name = 0;						\
}

#define SET_CRITICAL_SECTION_HOOK(hook)					\
{									\
  critical_section_hook = (hook);					\
  critical_section_hook_p = 1;						\
}

#define CLEAR_CRITICAL_SECTION_HOOK() critical_section_hook_p = 0
#define WITHIN_CRITICAL_SECTION_P() (critical_section_name != 0)
#define CRITICAL_SECTION_NAME() (critical_section_name)
