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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bintopsb.c,v 9.24 1987/04/16 02:05:24 jinx Exp $
 *
 * This File contains the code to translate internal format binary
 * files to portable format.
 *
 */

/* Cheap renames */

#define Internal_File Input_File
#define Portable_File Output_File

#include "translate.h"
#include "trap.h"

static Boolean Shuffle_Bytes = false;
static Boolean upgrade_traps = false;

static Pointer *Mem_Base;
static long Heap_Relocation, Constant_Relocation;
static long Free, Scan, Free_Constant, Scan_Constant;
static long Objects, Constant_Objects;
static long NFlonums, NIntegers, NStrings;
static long NBits, NChars;
static Pointer *Free_Objects, *Free_Cobjects;

Load_Data(Count, To_Where)
long Count;
char *To_Where;
{ fread(To_Where, sizeof(Pointer), Count, Internal_File);
}

#define Reloc_or_Load_Debug false

#include "load.c"

/* Utility macros and procedures
   Pointer Objects handled specially in the portable format.
*/

#ifndef isalpha
/* Just in case the stdio library atypically contains the character
   macros, just like the C book claims. */
#include <ctype.h>
#endif

#ifndef ispunct
/* This is in some libraries but not others */
static char punctuation[] = "'\",<.>/?;:{}[]|`~=+-_()*&^%$#@!";

Boolean ispunct(c)
fast char c;
{ fast char *s = &punctuation[0];
  while (*s != '\0') if (*s++ == c) return true;
  return false;
}
#endif

#define OUT(s)			\
fprintf(Portable_File, s);	\
break

void
print_a_char(c, name)
     fast char c;
     char *name;
{
  switch(c)
  { case '\n': OUT("\\n");
    case '\t': OUT("\\t");
    case '\b': OUT("\\b");
    case '\r': OUT("\\r");
    case '\f': OUT("\\f");
    case '\\': OUT("\\\\");
    case '\0': OUT("\\0");
    case ' ' : OUT(" ");
    default:
    if ((isalpha(c)) || (isdigit(c)) || (ispunct(c)))
      putc(c, Portable_File);
    else
    { fprintf(stderr,
	      "%s: %s: File may not be portable: c = 0x%x\n",
	      Program_Name, name, ((int) c));
      /* This does not follow C conventions, but eliminates ambiguity */
      fprintf(Portable_File, "\X%x ", ((int) c));
    }
  }
}

#define Do_String(Code, Rel, Fre, Scn, Obj, FObj)			\
{									\
  Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
  if (Type_Code(Old_Contents) == TC_BROKEN_HEART)			\
    Mem_Base[(Scn)] =							\
      Make_New_Pointer((Code), Old_Contents);				\
  else									\
  {									\
    fast long i;							\
									\
    Mem_Base[(Scn)] = Make_Non_Pointer((Code), (Obj));			\
    *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Obj));		\
    (Obj) += 1;								\
    *(FObj)++ = Make_Non_Pointer(TC_STRING, 0);				\
    *(FObj)++ = Old_Contents;						\
    i = Get_Integer(Old_Contents);					\
    NStrings += 1;							\
    NChars += pointer_to_char(i-1);					\
    while(--i >= 0)							\
      *(FObj)++ = *Old_Address++;					\
  }									\
}

void
print_a_string(from)
     Pointer *from;
{ fast long len;
  fast char *string;
  long maxlen;

  maxlen = pointer_to_char((Get_Integer(*from++))-1);
  len = Get_Integer(*from++);
  fprintf(Portable_File, "%02x %ld %ld ",
	  TC_CHARACTER_STRING,
	  (Compact_P ? len : maxlen),
	  len);
  string = ((char *) from);
  if (Shuffle_Bytes)
  { while(len > 0)
    {
      print_a_char(string[3], "print_a_string");
      if (len > 1)
	print_a_char(string[2], "print_a_string");
      if (len > 2)
	print_a_char(string[1], "print_a_string");
      if (len > 3)
	print_a_char(string[0], "print_a_string");
      len -= 4;
      string += 4;
    }
  }
  else while(--len >= 0) print_a_char(*string++, "print_a_string");
  putc('\n', Portable_File);
  return;
}

void
print_a_fixnum(val)
     long val;
{
  fast long size_in_bits;
  fast unsigned long temp;

  temp = ((val < 0) ? -val : val);
  for (size_in_bits = 0; temp != 0; size_in_bits += 1)
    temp = temp >> 1;
  fprintf(Portable_File, "%02x %c ",
	  TC_FIXNUM,
	  (val < 0 ? '-' : '+'));
  if (val == 0)
    fprintf(Portable_File, "0\n");
  else
  {
    fprintf(Portable_File, "%ld ", size_in_bits);
    temp = ((val < 0) ? -val : val);
    while (temp != 0)
    { fprintf(Portable_File, "%01lx", (temp % 16));
      temp = temp >> 4;
    }
    fprintf(Portable_File, "\n");
  }
  return;
}

#define Do_Bignum(Code, Rel, Fre, Scn, Obj, FObj)			\
{ Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
  if (Type_Code(Old_Contents) == TC_BROKEN_HEART)			\
    Mem_Base[(Scn)] =							\
      Make_New_Pointer((Code), Old_Contents);				\
  else									\
  { fast long length;							\
    Mem_Base[(Scn)] = Make_Non_Pointer((Code), (Obj));			\
    NIntegers += 1;							\
    NBits += bignum_to_bits(LEN(BIGNUM(Old_Address)));			\
    *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Obj));		\
    (Obj) += 1;								\
    *(FObj)++ = Make_Non_Pointer(TC_BIG_FIXNUM, 0);			\
    *(FObj)++ = Old_Contents;						\
    for (length = Get_Integer(Old_Contents);				\
	 --length >= 0;	)						\
      *(FObj)++ = *Old_Address++;					\
  }									\
}

void
print_a_bignum(from)
     Pointer *from;
{
  fast bigdigit *the_number, *the_top;
  fast long size_in_bits;
  fast unsigned long temp;	/* Potential signed problems */

  the_number = BIGNUM(from);
  temp = LEN(the_number);
  if (temp == 0) 
    fprintf(Portable_File, "%02x + 0\n",
	    (Compact_P ? TC_FIXNUM : TC_BIG_FIXNUM));
  else
  { fast long tail;
    for (size_in_bits = ((temp - 1) * SHIFT),
	 temp = ((long) (*Bignum_Top(the_number)));
	 temp != 0;
	 size_in_bits += 1)
      temp = temp >> 1;

    fprintf(Portable_File, "%02x %c %ld ",
	    (Compact_P ? TC_FIXNUM : TC_BIG_FIXNUM),
	    (NEG_BIGNUM(the_number) ? '-' : '+'),
	    size_in_bits);
    tail = size_in_bits % SHIFT;
    if (tail == 0) tail = SHIFT;
    temp = 0;
    size_in_bits = 0;
    the_top = Bignum_Top(the_number);
    for(the_number = Bignum_Bottom(the_number);
	the_number <= the_top;
	the_number += 1)
    { temp |= (((unsigned long) (*the_number)) << size_in_bits);
      for (size_in_bits += ((the_number != the_top) ? SHIFT : tail);
	   size_in_bits > 3;
	   size_in_bits -= 4)
      { fprintf(Portable_File, "%01lx", temp % 16);
	temp = temp >> 4;
      }
    }
    if (size_in_bits > 0) fprintf(Portable_File, "%01lx\n", temp);
    else fprintf(Portable_File, "\n");
  }
  return;
}

#define Do_Flonum(Code, Rel, Fre, Scn, Obj, FObj)			\
{ Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
  if (Type_Code(Old_Contents) == TC_BROKEN_HEART)			\
    Mem_Base[(Scn)] =							\
      Make_New_Pointer((Code), Old_Contents);				\
  else									\
  { *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Obj));		\
    Mem_Base[(Scn)] = Make_Non_Pointer((Code), (Obj));			\
    (Obj) += 1;								\
    *(FObj)++ = Make_Non_Pointer(TC_BIG_FLONUM, 0);			\
    *((double *) (FObj)) = *((double *) Old_Address);			\
    (FObj) += float_to_pointer;						\
    NFlonums += 1;							\
  }									\
}

print_a_flonum(val)
double val;
{ fast long size_in_bits;
  fast double mant, temp;
  int expt;
  extern double frexp();

  fprintf(Portable_File, "%02x %c ",
	  TC_BIG_FLONUM,
	  ((val < 0.0) ? '-' : '+'));
  if (val == 0.0)
  { fprintf(Portable_File, "0\n");
    return;
  }
  mant = frexp(((val < 0.0) ? -val : val), &expt);
  size_in_bits = 1;
  for(temp = ((mant * 2.0) - 1.0);
      temp != 0;
      size_in_bits += 1)
  { temp *= 2.0;
    if (temp >= 1.0) temp -= 1.0;
  }
  fprintf(Portable_File, "%ld %ld ", expt, size_in_bits);
  for (size_in_bits = hex_digits(size_in_bits);
       size_in_bits > 0;
       size_in_bits -= 1)
  { fast unsigned int digit = 0;
    for (expt = 4; --expt >= 0;)
    { mant *= 2.0;
      digit = digit << 1;
      if (mant >= 1.0)
      { mant -= 1.0;
	digit += 1;
      }
    }
    fprintf(Portable_File, "%01x", digit);
  }
  fprintf(Portable_File, "\n");
  return;
}

/* Normal Objects */

#define Do_Cell(Code, Rel, Fre, Scn, Obj, FObj)			\
{ Old_Address += (Rel);						\
  Old_Contents = *Old_Address;					\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)		\
    Mem_Base[(Scn)] =						\
      Make_New_Pointer(Type_Code(This), Old_Contents);		\
  else								\
  { *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));	\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));	\
    Mem_Base[(Fre)++] = Old_Contents;				\
  }								\
}

#define Do_Pair(Code, Rel, Fre, Scn, Obj, FObj)			\
{ Old_Address += (Rel);						\
  Old_Contents = *Old_Address;					\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)		\
    Mem_Base[(Scn)] =						\
      Make_New_Pointer(Type_Code(This), Old_Contents);		\
  else								\
  { *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));	\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));	\
    Mem_Base[(Fre)++] = Old_Contents;				\
    Mem_Base[(Fre)++] = *Old_Address++;				\
  }								\
}

#define Do_Triple(Code, Rel, Fre, Scn, Obj, FObj)		\
{ Old_Address += (Rel);						\
  Old_Contents = *Old_Address;					\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)		\
    Mem_Base[(Scn)] =						\
      Make_New_Pointer(Type_Code(This), Old_Contents);		\
  else								\
  { *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));	\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));	\
    Mem_Base[(Fre)++] = Old_Contents;				\
    Mem_Base[(Fre)++] = *Old_Address++;				\
    Mem_Base[(Fre)++] = *Old_Address++;				\
  }								\
}

#define Do_Vector(Code, Rel, Fre, Scn, Obj, FObj)		\
{ Old_Address += (Rel);						\
  Old_Contents = *Old_Address;					\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)		\
    Mem_Base[(Scn)] =						\
      Make_New_Pointer(Type_Code(This), Old_Contents);		\
  else								\
  { fast long len = Get_Integer(Old_Contents);			\
    *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));	\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));	\
    Mem_Base[(Fre)++] = Old_Contents;				\
    while (len > 0)						\
    { Mem_Base[(Fre)++] = *Old_Address++;			\
      len -= 1;							\
    }								\
  }								\
}

/* Common Pointer Code */

#define Do_Pointer(Scn, Action)					\
Old_Address = Get_Pointer(This);				\
if (Datum(This) < Const_Base)					\
  Action(HEAP_CODE, Heap_Relocation, Free, 			\
	 Scn, Objects, Free_Objects)				\
else if (Datum(This) < Dumped_Constant_Top)			\
Action(CONSTANT_CODE, Constant_Relocation, Free_Constant,	\
       Scn, Constant_Objects, Free_Cobjects)			\
else								\
{ fprintf(stderr,						\
	  "%s: File is not portable: Pointer to stack.\n",	\
          Program_Name);					\
  exit(1);							\
}								\
(Scn) += 1;							\
break

/* Processing of a single area */

#define Do_Area(Code, Area, Bound, Obj, FObj)			\
  Process_Area(Code, &Area, &Bound, &Obj, &FObj)

Process_Area(Code, Area, Bound, Obj, FObj)
int Code;
fast long *Area, *Bound;
fast long *Obj;
fast Pointer **FObj;
{ fast Pointer This, *Old_Address, Old_Contents;
  while(*Area != *Bound)
  { This = Mem_Base[*Area];
    Switch_by_GC_Type(This)
    { case TC_MANIFEST_NM_VECTOR:
        if (Null_NMV)
	{ fast int i = Get_Integer(This);
	  *Area += 1;
	  for ( ; --i >= 0; *Area += 1)
	    Mem_Base[*Area] = NIL;
	  break;
	}
        /* else, Unknown object! */
        fprintf(stderr, "%s: File is not portable: NMH found\n",
		Program_Name);
	*Area += 1 + Get_Integer(This);
	break;

      case TC_BROKEN_HEART:
      /* [Broken Heart 0] is the cdr of fasdumped symbols. */
	if (Get_Integer(This) != 0)
	{ fprintf(stderr, "%s: Broken Heart found in scan.\n",
		  Program_Name);
	  exit(1);
	}
	*Area += 1;
	break;

      case_compiled_entry_point:
	fprintf(stderr,
		"%s: File is not portable: Compiled code.\n",
		Program_Name);
	exit(1);

      case TC_FIXNUM:
	NIntegers += 1;
	NBits += fixnum_to_bits;
	/* Fall Through */
      case TC_CHARACTER:
      Process_Character:
        Mem_Base[*Area] = Make_Non_Pointer(Code, *Obj);
        *Obj += 1;
        **FObj = This;
        *FObj += 1;
	/* Fall through */
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
      case TC_PRIMITIVE_EXTERNAL:
      case_simple_Non_Pointer:
	*Area += 1;
	break;

      case_Cell:
	Do_Pointer(*Area, Do_Cell);

      case TC_REFERENCE_TRAP:
      {
	long kind;

	kind = Datum(This);

	if (upgrade_traps)
	{
	  /* It is an old UNASSIGNED object. */
	  if (kind == 0)
	  {
	    Mem_Base[*Area] = UNASSIGNED_OBJECT;
	    *Area += 1;
	    break;
	  }
	  if (kind == 1)
	  {
	    Mem_Base[*Area] = UNBOUND_OBJECT;
	    *Area += 1;
	    break;
	  }
	  fprintf(stderr,
		  "%s: Bad old unassigned object. 0x%x.\n",
		  Program_Name, This);
	  exit(1);
	}
	if (kind <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */

	  *Area += 1;
	  break;
	}
      }
      /* Fall through */

      case TC_WEAK_CONS:
      case_Pair:
	Do_Pointer(*Area, Do_Pair);

      case TC_VARIABLE:
      case_Triple:
	Do_Pointer(*Area, Do_Triple);

      case TC_BIG_FLONUM:
	Do_Pointer(*Area, Do_Flonum);

      case TC_BIG_FIXNUM:
	Do_Pointer(*Area, Do_Bignum);

      case TC_CHARACTER_STRING:
	Do_Pointer(*Area, Do_String);

      case TC_ENVIRONMENT:
	if (upgrade_traps)
	{
	  fprintf(stderr,
		  "%s: Cannot upgrade environments.\n",
		  Program_Name);
	  exit(1);
	}
	/* Fall through */
      case TC_FUTURE:
      case_simple_Vector:
	Do_Pointer(*Area, Do_Vector);

      default:
      Bad_Type:
	fprintf(stderr, "%s: Unknown Type Code 0x%x found.\n",
		Program_Name, Type_Code(This));
	exit(1);
      }
  }
}

/* Output macros */

#define print_an_object(obj)					\
fprintf(Portable_File, "%02x %lx\n",				\
	Type_Code(obj), Get_Integer(obj))

#define print_external_object(from)				\
{ switch(Type_Code(*from))					\
  { case TC_FIXNUM:						\
    { long Value;						\
      Sign_Extend(*from++, Value);				\
      print_a_fixnum(Value);					\
      break;							\
    }								\
    case TC_BIG_FIXNUM:						\
      from += 1;						\
      print_a_bignum(from);					\
      from += 1 + Get_Integer(*from);				\
      break;							\
    case TC_CHARACTER_STRING:					\
      from += 1;						\
      print_a_string(from);					\
      from += 1 + Get_Integer(*from);				\
      break;							\
    case TC_BIG_FLONUM:						\
      print_a_flonum(*((double *) (from+1)));			\
      from += 1 + float_to_pointer;				\
      break;							\
    case TC_CHARACTER:						\
      fprintf(Portable_File, "%02x %03x\n",			\
	      TC_CHARACTER, (*from & MASK_EXTNDD_CHAR));	\
      from += 1;						\
      break;							\
    default:							\
      fprintf(stderr,						\
	      "%s: Bad Object to print externally %lx\n",	\
	      Program_Name, *from);				\
      exit(1);							\
  }								\
}

/* Debugging Aids and Consistency Checks */

#ifdef DEBUG

When(what, message)
Boolean what;
char *message;
{ if (what)
  { fprintf(stderr, "%s: Inconsistency: %s!\n",
	    Program_Name, (message));
    exit(1);
  }
  return;
}

#define print_header(name, obj, format)				\
fprintf(Portable_File, (format), (obj));			\
fprintf(stderr, "%s: ", (name));				\
fprintf(stderr, (format), (obj))

#else

#define When(what, message)

#define print_header(name, obj, format)				\
fprintf(Portable_File, (format), (obj))

#endif

/* The main program */

do_it()
{ Pointer *Heap;
  long Initial_Free;

  /* Load the Data */

  if (!Read_Header())
  { fprintf(stderr,
	    "%s: Input file does not appear to be in FASL format.\n",
	    Program_Name);
    exit(1);
  }

  if ((Version != FASL_FORMAT_VERSION) ||
      (Sub_Version > FASL_SUBVERSION) ||
      (Sub_Version < FASL_OLDEST_SUPPORTED) ||
      ((Machine_Type != FASL_INTERNAL_FORMAT) && (!Shuffle_Bytes)))
  { fprintf(stderr, "%s:\n", Program_Name);
    fprintf(stderr,
	    "FASL File Version %ld Subversion %ld Machine Type %ld\n",
	    Version, Sub_Version , Machine_Type);
    fprintf(stderr,
	    "Expected: Version %d Subversion %d Machine Type %d\n",
	    FASL_FORMAT_VERSION, FASL_SUBVERSION, FASL_INTERNAL_FORMAT);
    exit(1);
  }

  if (Machine_Type == FASL_INTERNAL_FORMAT)
    Shuffle_Bytes = false;
  upgrade_traps = (Sub_Version < FASL_REFERENCE_TRAP);

  /* Constant Space not currently supported */

  if (Const_Count != 0)
  { fprintf(stderr,
	    "%s: Input file has a constant space area.\n",
	    Program_Name);
    exit(1);
  }

  { long Size = ((3 * (Heap_Count + Const_Count)) + NROOTS + 1);
    Allocate_Heap_Space(Size + HEAP_BUFFER_SPACE);
    if (Heap == NULL)
    { fprintf(stderr,
	      "%s: Memory Allocation Failed.  Size = %ld Scheme Pointers\n",
	      Program_Name, Size);
      exit(1);
    }
  }
  Heap += HEAP_BUFFER_SPACE;
  Initial_Align_Float(Heap);
  Load_Data(Heap_Count, &Heap[0]);
  Load_Data(Const_Count, &Heap[Heap_Count]);
  Heap_Relocation = &Heap[0] - Get_Pointer(Heap_Base);
  Constant_Relocation = &Heap[Heap_Count] - Get_Pointer(Const_Base);

#ifdef DEBUG
  fprintf(stderr, "Dumped Heap Base = 0x%08x\n", Heap_Base);
  fprintf(stderr, "Dumped Constant Base = 0x%08x\n", Const_Base);
  fprintf(stderr, "Dumped Constant Top = 0x%08x\n", Dumped_Constant_Top);
  fprintf(stderr, "Heap Count = %6d\n", Heap_Count);
  fprintf(stderr, "Constant Count = %6d\n", Const_Count);
#endif

  /* Reformat the data */

  NFlonums = NIntegers = NStrings = NBits = NChars = 0;
  Mem_Base = &Heap[Heap_Count + Const_Count];
  if (Ext_Prim_Vector == NIL)
  { Mem_Base[0] = Make_Non_Pointer(TC_CELL, 2);
    Mem_Base[1] = Make_New_Pointer(TC_CELL, Dumped_Object);
    Mem_Base[2] = NIL;
    Initial_Free = NROOTS + 1;
    Scan = 1;
  }
  else
  { Mem_Base[0] = Ext_Prim_Vector;	/* Has CELL TYPE */
    Mem_Base[1] = Make_New_Pointer(TC_CELL, Dumped_Object);
    Initial_Free = NROOTS;
    Scan = 0;
  }
  Free = Initial_Free;
  Free_Objects = &Mem_Base[Heap_Count + Initial_Free];
  Objects = 0;

  Free_Constant = (2 * Heap_Count) + Initial_Free;
  Scan_Constant = Free_Constant;
  Free_Cobjects = &Mem_Base[Const_Count + Free_Constant];
  Constant_Objects = 0;

#if true
  Do_Area(HEAP_CODE, Scan, Free, Objects, Free_Objects);
#else
  /* When Constant Space finally becomes supported,
     something like this must be done. */
  while (true)
  { Do_Area(HEAP_CODE, Scan, Free, Objects, Free_Objects);
    Do_Area(CONSTANT_CODE, Scan_Constant,
	    Free_Constant, Constant_Objects, Free_Cobjects);
    Do_Area(PURE_CODE, Scan_Pure, Fre_Pure, Pure_Objects, Free_Pobjects);
    if (Scan == Free) break;
  }
#endif

  /* Consistency checks */

  When(((Free - Initial_Free) > Heap_Count), "Free overran Heap");
  When(((Free_Objects - &Mem_Base[Initial_Free + Heap_Count]) >
	Heap_Count),
       "Free_Objects overran Heap Object Space");
  When(((Free_Constant - (Initial_Free + (2 * Heap_Count))) > Const_Count),
       "Free_Constant overran Constant Space");
  When(((Free_Cobjects - &Mem_Base[Initial_Free + (2 * Heap_Count) + Const_Count]) >
	Const_Count),
       "Free_Cobjects overran Constant Object Space");

  /* Output the data */

  /* Header */

  print_header("Portable Version", PORTABLE_VERSION, "%ld\n");
  print_header("Flags", Make_Flags(), "%ld\n");
  print_header("Version", FASL_FORMAT_VERSION, "%ld\n");
  print_header("Sub Version", FASL_SUBVERSION, "%ld\n");
  print_header("Heap Count", (Free - NROOTS), "%ld\n");
  print_header("Heap Base", NROOTS, "%ld\n");
  print_header("Heap Objects", Objects, "%ld\n");

  /* Currently Constant and Pure not supported, but the header is ready */

  print_header("Pure Count", 0, "%ld\n");
  print_header("Pure Base", Free_Constant, "%ld\n");
  print_header("Pure Objects", 0, "%ld\n");
  print_header("Constant Count", 0, "%ld\n");
  print_header("Constant Base", Free_Constant, "%ld\n");
  print_header("Constant Objects", 0, "%ld\n");

  print_header("Number of flonums", NFlonums, "%ld\n");
  print_header("Number of integers", NIntegers, "%ld\n");
  print_header("Number of strings", NStrings, "%ld\n");
  print_header("Number of bits in integers", NBits, "%ld\n");
  print_header("Number of characters in strings", NChars, "%ld\n");
  print_header("& Dumped Object", (Get_Integer(Mem_Base[1])), "%ld\n");
  print_header("& Ext Prim Vector", (Get_Integer(Mem_Base[0])), "%ld\n");

  /* External Objects */
  
  /* Heap External Objects */

  Free_Objects = &Mem_Base[Initial_Free + Heap_Count];
  for (; Objects > 0; Objects -= 1)
    print_external_object(Free_Objects);
  
#if false
  /* Pure External Objects */

  Free_Cobjects = &Mem_Base[Pure_Objects_Start];
  for (; Pure_Objects > 0; Pure_Objects -= 1)
    print_external_object(Free_Cobjects);

  /* Constant External Objects */

  Free_Cobjects = &Mem_Base[Constant_Objects_Start];
  for (; Constant_Objects > 0; Constant_Objects -= 1)
    print_external_object(Free_Cobjects);

#endif

  /* Pointer Objects */

  /* Heap Objects */

  Free_Cobjects = &Mem_Base[Free];
  for (Free_Objects = &Mem_Base[NROOTS];
       Free_Objects < Free_Cobjects;
       Free_Objects += 1)
    print_an_object(*Free_Objects);

#if false
  /* Pure Objects */

  Free_Cobjects = &Mem_Base[Free_Pure];
  for (Free_Objects = &Mem_Base[Pure_Start];
       Free_Objects < Free_Cobjects;
       Free_Objects += 1)
    print_an_object(*Free_Objects);

  /* Constant Objects */

  Free_Cobjects = &Mem_Base[Free_Constant];
  for (Free_Objects = &Mem_Base[Constant_Start];
       Free_Objects < Free_Cobjects;
       Free_Objects += 1)
    print_an_object(*Free_Objects);
#endif

  return;
}

/* Top Level */

static int Noptions = 3;

static struct Option_Struct Options[] =
  {{"Do_Not_Compact", false, &Compact_P},
   {"Null_Out_NMVs", true, &Null_NMV},
   {"Swap_Bytes", true, &Shuffle_Bytes}};

main(argc, argv)
int argc;
char *argv[];
{ Setup_Program(argc, argv, Noptions, Options);
  return;
}
