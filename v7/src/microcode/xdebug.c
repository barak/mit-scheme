/* -*-C-*-

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/xdebug.c,v 9.25 1989/06/16 09:40:14 cph Rel $
 *
 * This file contains primitives to debug the memory management in the
 * Scheme system.
 *
 */

#include "scheme.h"
#include "prims.h"

/* New debugging utilities */

#define FULL_EQ		0
#define ADDRESS_EQ	2
#define DATUM_EQ	3

static Pointer *
Find_Occurrence(From, To, What, Mode)
     fast Pointer *From, *To;
     Pointer What;
     int Mode;
{
  fast Pointer Obj;

  switch (Mode)
  { default:
    case FULL_EQ:
    {
      Obj = What;
      for (; From < To; From++)
      {
	if (OBJECT_TYPE(*From) == TC_MANIFEST_NM_VECTOR)
	{
	  From += OBJECT_DATUM(*From); 
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
      Obj = OBJECT_DATUM(What);
      for (; From < To; From++)
      {
	if (OBJECT_TYPE(*From) == TC_MANIFEST_NM_VECTOR)
	{
	  From += OBJECT_DATUM(*From); 
	}
	else if ((OBJECT_DATUM(*From) == Obj) &&
		 (!(GC_Type_Non_Pointer(*From))))
	{
	  return From;
	}
      }
      return To;
    }
    case DATUM_EQ:
    {
      Obj = OBJECT_DATUM(What);
      for (; From < To; From++)
      {
	if (OBJECT_TYPE(*From) == TC_MANIFEST_NM_VECTOR)
	{
	  From += OBJECT_DATUM(*From); 
	}
	else if (OBJECT_DATUM(*From) == Obj)
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
     Pointer *From, *To, Obj;
     int Mode;
     Boolean print_p, store_p;
{
  fast Pointer *Where;
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
      printf("Location = 0x%x; Contents = 0x%x\n",
	     ((long) Where), ((long) (*Where)));
#else
      printf("Location = 0x%08x; Contents = 0x%08x\n",
	     ((long) Where), ((long) (*Where)));
#endif
    if (store_p)
    {
      /* Note that Make_Pointer (vs. Make_Non_Pointer) is correct here!! */
      *Free++ = Make_Pointer(TC_ADDRESS, Where);
    }
  }
  return occurrences;
}

Pointer
Find_Who_Points(Obj, Find_Mode, Collect_Mode)
     Pointer Obj;
     int Find_Mode, Collect_Mode;
{
  long n = 0;
  Pointer *Saved_Free = Free;
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
    printf("*** Looking for Obj = 0x%x; Find_Mode = %2d ***\n",
	   Obj, Find_Mode);
#else
    printf("*** Looking for Obj = 0x%08x; Find_Mode = %2d ***\n",
	   Obj, Find_Mode);
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
    *Saved_Free = Make_Non_Pointer(TC_MANIFEST_VECTOR, n);
    return Make_Pointer(TC_VECTOR, Saved_Free);
  }
  else
  {
    return Make_Non_Pointer(TC_FIXNUM, n);
  }
}

Print_Memory(Where, How_Many)
     Pointer *Where;
     long How_Many;
{
  fast Pointer *End   = &Where[How_Many];

#ifndef b32
  printf("\n*** Memory from 0x%x to 0x%x (excluded) ***\n", Where, End);
  while (Where < End)
  {
    printf("0x%x\n", *Where++);
  }
#else
  printf("\n*** Memory from 0x%08x to 0x%08x (excluded) ***\n", Where, End);
  while (Where < End)
  {
    printf("0x%08x\n", *Where++);
  }
#endif
  printf("Done.\n");
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
  Pointer environment;
  PRIMITIVE_HEADER (1);

  environment = (ARG_REF (1));
  printf ("\n*** Environment = 0x%x ***\n", environment);
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
    fast Pointer symbol = (find_symbol (ARG_REF (1)));
    if (symbol == SHARP_F)
      printf ("\nNot interned.\n");
    else
      {
	printf ("\nInterned Symbol: 0x%x", symbol);
	Print_Expression (Vector_Ref (symbol, SYMBOL_GLOBAL_VALUE), "Value");
	printf ("\n");
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Primitives to give scheme a handle on utilities on this file. */

DEFINE_PRIMITIVE ("DEBUG-FLAGS", Prim_debug_flags, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);

  Handle_Debug_Flags ();
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
  Pointer object;
  PRIMITIVE_HEADER (2);

  object = (ARG_REF (1));
  Print_Memory
    (((GC_Type_Non_Pointer (object))
      ? ((Pointer *) (OBJECT_DATUM (object)))
      : (Get_Pointer (object))),
     (OBJECT_DATUM (ARG_REF (2))));
  PRIMITIVE_RETURN (UNSPECIFIC);
}
