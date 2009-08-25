/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

/* This file contains primitives to debug memory management. */

#include "scheme.h"
#include "prims.h"

/* New debugging utilities */

#define FULL_EQ		0
#define ADDRESS_EQ	2
#define DATUM_EQ	3

static SCHEME_OBJECT *
Find_Occurrence (SCHEME_OBJECT * From,
		 SCHEME_OBJECT * To,
		 SCHEME_OBJECT What,
		 int Mode)
{
  SCHEME_OBJECT Obj;

  switch (Mode)
  { default:
    case FULL_EQ:
    {
      Obj = What;
      for (; From < To; From++)
      {
	if (OBJECT_TYPE (*From) == TC_MANIFEST_NM_VECTOR)
	{
	  From += OBJECT_DATUM (*From);
	}
	else if (*From == Obj)
	{
	  return From;
	}
      }
     return To;
    }

    case ADDRESS_EQ:
    {
      Obj = OBJECT_DATUM (What);
      for (; From < To; From++)
      {
	if (OBJECT_TYPE (*From) == TC_MANIFEST_NM_VECTOR)
	{
	  From += OBJECT_DATUM (*From);
	}
	else if ((OBJECT_DATUM (*From) == Obj)
		 && (!GC_TYPE_NON_POINTER (*From)))
	  return From;
      }
      return To;
    }
    case DATUM_EQ:
    {
      Obj = OBJECT_DATUM (What);
      for (; From < To; From++)
      {
	if (OBJECT_TYPE (*From) == TC_MANIFEST_NM_VECTOR)
	{
	  From += OBJECT_DATUM (*From);
	}
	else if (OBJECT_DATUM (*From) == Obj)
	{
	  return From;
	}
      }
      return To;
    }
  }
}

#define PRINT_P		1
#define STORE_P		2

static long
Find_In_Area (char * Name,
	      SCHEME_OBJECT * From, SCHEME_OBJECT * To, SCHEME_OBJECT Obj,
	      int Mode,
	      bool print_p, bool store_p)
{
  SCHEME_OBJECT *Where;
  long occurrences = 0;

  if (print_p)
  {
    outf_console("    Looking in %s:\n", Name);
  }
  Where = From-1;

  while ((Where = Find_Occurrence(Where+1, To, Obj, Mode)) < To)
  {
    occurrences += 1;
    if (print_p)
#if (SIZEOF_UNSIGNED_LONG == 4)
      outf_console("Location = 0x%08lx; Contents = 0x%08lx\n",
	     ((long) Where), ((long) (*Where)));
#else
      outf_console("Location = 0x%lx; Contents = 0x%lx\n",
	     ((long) Where), ((long) (*Where)));
#endif
    if (store_p)
      *Free++ = (LONG_TO_UNSIGNED_FIXNUM ((long) Where));
  }
  return occurrences;
}

SCHEME_OBJECT
Find_Who_Points (SCHEME_OBJECT Obj, int Find_Mode, int Collect_Mode)
{
  long n = 0;
  SCHEME_OBJECT *Saved_Free = Free;
  bool print_p = (Collect_Mode & PRINT_P);
  bool store_p = (Collect_Mode & STORE_P);

  /* No overflow check done. Hopefully referenced few times, or invoked before
     to find the count and insure that there is enough space. */
  if (store_p)
  {
    Free += 1;
  }
  if (print_p)
  {
    putchar('\n');
#if (SIZEOF_UNSIGNED_LONG == 4)
    outf_console("*** Looking for Obj = 0x%08lx; Find_Mode = %2ld ***\n",
	   ((long) Obj), ((long) Find_Mode));
#else
    outf_console("*** Looking for Obj = 0x%lx; Find_Mode = %2ld ***\n",
	   ((long) Obj), ((long) Find_Mode));
#endif
  }
  n += Find_In_Area("Constant Space",
		    constant_start, constant_alloc_next, Obj,
		    Find_Mode, print_p, store_p);
  n += Find_In_Area("the Heap",
		    heap_start, Saved_Free, Obj,
		    Find_Mode, print_p, store_p);
  n += Find_In_Area("the Stack",
		    stack_pointer, stack_end, Obj,
		    Find_Mode, print_p, store_p);
  if (print_p)
  {
    outf_console("Done.\n");
  }
  if (store_p)
  {
    *Saved_Free = (MAKE_OBJECT (TC_MANIFEST_VECTOR, n));
    return (MAKE_POINTER_OBJECT (TC_VECTOR, Saved_Free));
  }
  else
  {
    return (LONG_TO_FIXNUM (n));
  }
}

void
Print_Memory (SCHEME_OBJECT * Where, long How_Many)
{
  SCHEME_OBJECT *End   = &Where[How_Many];

#if (SIZEOF_UNSIGNED_LONG == 4)
  outf_console ("\n*** Memory from 0x%08lx to 0x%08lx (excluded) ***\n",
	  ((long) Where), ((long) End));
  while (Where < End)
  {
    outf_console ("0x%0l8x\n", ((long) (*Where++)));
  }
#else
  outf_console ("\n*** Memory from 0x%lx to 0x%lx (excluded) ***\n",
	  ((long) Where), ((long) End));
  while (Where < End)
  {
    outf_console ("0x%lx\n", ((long) (*Where++)));
  }
#endif
  outf_console ("Done.\n");
  return;
}

/* Primitives to give scheme a handle on utilities from DEBUG.C */

DEFINE_PRIMITIVE ("DEBUG-SHOW-ENV", Prim_debug_show_env, 1, 1, 0)
{
  SCHEME_OBJECT environment;
  PRIMITIVE_HEADER (1);

  environment = (ARG_REF (1));
  outf_console ("\n*** Environment = 0x%lx ***\n", ((long) environment));
  Show_Env (environment);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DEBUG-STACK-TRACE", Prim_debug_stack_trace, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  outf_console ("\n*** Back Trace: ***\n");
  Back_Trace (CONSOLE_OUTPUT);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DEBUG-FIND-SYMBOL", Prim_debug_find_symbol, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  {
    SCHEME_OBJECT string = (ARG_REF (1));
    SCHEME_OBJECT symbol = (find_symbol ((STRING_LENGTH (string)),
					 (STRING_POINTER (string))));
    if (symbol == SHARP_F)
      outf_console ("\nNot interned.\n");
    else
      {
	outf_console ("\nInterned Symbol: 0x%lx", ((long) symbol));
	Print_Expression (MEMORY_REF (symbol, SYMBOL_GLOBAL_VALUE), "Value");
	outf_console ("\n");
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Primitives to give scheme a handle on utilities in this file. */

DEFINE_PRIMITIVE ("DEBUG-EDIT-FLAGS", Prim_debug_edit_flags, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  debug_edit_flags ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DEBUG-FIND-WHO-POINTS", Prim_debug_find_who_points, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  PRIMITIVE_RETURN
    (Find_Who_Points
     ((ARG_REF (1)),
      (OBJECT_DATUM (ARG_REF (2))),
      (OBJECT_DATUM (ARG_REF (3)))));
}

DEFINE_PRIMITIVE ("DEBUG-PRINT-MEMORY", Prim_debug_print_memory, 2, 2, 0)
{
  SCHEME_OBJECT object;
  PRIMITIVE_HEADER (2);
  object = (ARG_REF (1));
  Print_Memory
    (((GC_TYPE_NON_POINTER (object))
      ? ((SCHEME_OBJECT *) (OBJECT_DATUM (object)))
      : (OBJECT_ADDRESS (object))),
     (OBJECT_DATUM (ARG_REF (2))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
