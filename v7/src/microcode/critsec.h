/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/critsec.h,v 1.1 1990/06/20 19:35:41 cph Rel $

Copyright (c) 1990 Massachusetts Institute of Technology

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

/* Critical sections.
   There should be a stack of critical sections, each with a
   queue of hooks. */

extern char * critical_section_name;
extern int critical_section_hook_p;
extern void (*critical_section_hook) ();

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
