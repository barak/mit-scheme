/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/xdebug.c,v 9.28 1992/02/03 23:51:07 jinx Exp $

Copyright (c) 1987-1992 Massachusetts Institute of Technology

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

/* This file contains primitives to debug memory management. */

#include "scheme.h"
#include "prims.h"

/* New debugging utilities */

#define FULL_EQ		0
#define ADDRESS_EQ	2
#define DATUM_EQ	3

static SCHEME_OBJECT *
Find_Occurrence(From, To, What, Mode)
     fast SCHEME_OBJECT *From, *To;
     SCHEME_OBJECT What;
     int Mode;
{
  fast SCHEME_OBJECT Obj;

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
	else if ((OBJECT_DATUM (*From) == Obj) &&
		 (!(GC_Type_Non_Pointer(*From))))
	{
	  return From;
	}
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
Find_In_Area(Name, From, To, Obj, Mode, print_p, store_p)
     char *Name;
     SCHEME_OBJECT *From, *To, Obj;
     int Mode;
     Boolean print_p, store_p;
{
  fast SCHEME_OBJECT *Where;
  fast long occurrences = 0;

  if (print_p)
  {
    printf("    Looking in %s:\n", Name);
  }
  Where = From-1;

  while ((Where = Find_Occurrence(Where+1, To, Obj, Mode)) < To)
  {
    occurrences += 1;
    if (print_p)
#ifndef b32
      printf("Location = 0x%lx; Contents = 0x%lx\n",
	     ((long) Where), ((long) (*Where)));
#else
      printf("Location = 0x%08lx; Contents = 0x%08lx\n",
	     ((long) Where), ((long) (*Where)));
#endif
    if (store_p)
      *Free++ = (LONG_TO_UNSIGNED_FIXNUM (Where));
  }
  return occurrences;
}

SCHEME_OBJECT
Find_Who_Points(Obj, Find_Mode, Collect_Mode)
     SCHEME_OBJECT Obj;
     int Find_Mode, Collect_Mode;
{
  long n = 0;
  SCHEME_OBJECT *Saved_Free = Free;
  Boolean print_p = (Collect_Mode & PRINT_P);
  Boolean store_p = (Collect_Mode & STORE_P);

  /* No overflow check done. Hopefully referenced few times, or invoked before
     to find the count and insure that there is enough space. */
  if (store_p)
  {
    Free += 1;
  }
  if (print_p)
  {
    putchar('\n');
#ifndef b32
    printf("*** Looking for Obj = 0x%lx; Find_Mode = %2ld ***\n",
	   ((long) Obj), ((long) Find_Mode));
#else
    printf("*** Looking for Obj = 0x%08lx; Find_Mode = %2ld ***\n",
	   ((long) Obj), ((long) Find_Mode));
#endif
  }
  n += Find_In_Area("Constant Space",
		    Constant_Space, Free_Constant, Obj,
		    Find_Mode, print_p, store_p);
  n += Find_In_Area("the Heap",
		    Heap_Bottom, Saved_Free, Obj,
		    Find_Mode, print_p, store_p);
#ifndef USE_STACKLETS
  n += Find_In_Area("the Stack",
		    Stack_Pointer, Stack_Top, Obj,
		    Find_Mode, print_p, store_p);
#endif
  if (print_p)
  {
    printf("Done.\n");
  }
  if (store_p)
  {
    *Saved_Free = MAKE_OBJECT (TC_MANIFEST_VECTOR, n);
    return MAKE_POINTER_OBJECT (TC_VECTOR, Saved_Free);
  }
  else
  {
    return (LONG_TO_FIXNUM (n));
  }
}

Print_Memory(Where, How_Many)
     SCHEME_OBJECT *Where;
     long How_Many;
{
  fast SCHEME_OBJECT *End   = &Where[How_Many];

#ifndef b32
  printf ("\n*** Memory from 0x%lx to 0x%lx (excluded) ***\n",
	  ((long) Where), ((long) End));
  while (Where < End)
  {
    printf ("0x%lx\n", ((long) (*Where++)));
  }
#else
  printf ("\n*** Memory from 0x%08lx to 0x%08lx (excluded) ***\n",
	  ((long) Where), ((long) End));
  while (Where < End)
  {
    printf ("0x%0l8x\n", ((long) (*Where++)));
  }
#endif
  printf ("Done.\n");
  return;
}

/* Primitives to give scheme a handle on utilities from DEBUG.C */

DEFINE_PRIMITIVE ("DEBUG-SHOW-PURE", Prim_debug_show_pure, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  printf ("\n*** Constant & Pure Space: ***\n");
  Show_Pure ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DEBUG-SHOW-ENV", Prim_debug_show_env, 1, 1, 0)
{
  SCHEME_OBJECT environment;
  PRIMITIVE_HEADER (1);

  environment = (ARG_REF (1));
  printf ("\n*** Environment = 0x%lx ***\n", ((long) environment));
  Show_Env (environment);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DEBUG-STACK-TRACE", Prim_debug_stack_trace, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  printf ("\n*** Back Trace: ***\n");
  Back_Trace (stdout);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DEBUG-FIND-SYMBOL", Prim_debug_find_symbol, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);

  CHECK_ARG (1, STRING_P);
  {
    fast SCHEME_OBJECT symbol = (find_symbol (ARG_REF (1)));
    if (symbol == SHARP_F)
      printf ("\nNot interned.\n");
    else
      {
	printf ("\nInterned Symbol: 0x%lx", ((long) symbol));
	Print_Expression (MEMORY_REF (symbol, SYMBOL_GLOBAL_VALUE), "Value");
	printf ("\n");
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
    (((GC_Type_Non_Pointer (object))
      ? ((SCHEME_OBJECT *) (OBJECT_DATUM (object)))
      : (OBJECT_ADDRESS (object))),
     (OBJECT_DATUM (ARG_REF (2))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
