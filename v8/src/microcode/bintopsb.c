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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/bintopsb.c,v 9.29 1987/11/17 08:02:39 jinx Exp $
 *
 * This File contains the code to translate internal format binary
 * files to portable format.
 *
 */

/* IO definitions */

#define Internal_File Input_File
#define Portable_File Output_File

#include "translate.h"
#include "trap.h"

long
Load_Data(Count, To_Where)
     long Count;
     char *To_Where;
{
  extern int fread();

  return (fread(To_Where, sizeof(Pointer), Count, Internal_File));
}

#define Reloc_or_Load_Debug false

#include "fasl.h"
#define INHIBIT_FASL_VERSION_CHECK
#include "load.c"
#include "bltdef.h"

/* Character macros and procedures */

extern int strlen();

#ifndef isalpha

/* Just in case the stdio library atypically contains the character
   macros, just like the C book claims. */

#include <ctype.h>

#endif /* isalpha */

#ifndef ispunct

/* This is in some libraries but not others */

static char punctuation[] = "'\",<.>/?;:{}[]|`~=+-_()*&^%$#@!";

Boolean
ispunct(c)
     fast char c;
{
  fast char *;

  s = &punctuation[0];
  while (*s != '\0')
  {
    if (*s++ == c)
    {
      return (true);
    }
  }
  return (false);
}

#endif /* ispunct */

/* Global data */

static Boolean Shuffle_Bytes = false;
static Boolean upgrade_traps = false;
static Boolean upgrade_primitives = false;

/* Needed to upgrade */
#define TC_PRIMITIVE_EXTERNAL	0x10

static Boolean upgrade_lengths = false;

#define STRING_LENGTH_TO_LONG(value)					\
((long) (upgrade_lengths ? Get_Integer(value) : (value)))

static Pointer *Mem_Base;
static long Heap_Relocation, Constant_Relocation;
static long Free, Scan, Free_Constant, Scan_Constant;
static long Objects, Constant_Objects;
static Pointer *Free_Objects, *Free_Cobjects;
static Pointer *primitive_table;

static long NFlonums;
static long NIntegers, NBits;
static long NBitstrs, NBBits;
static long NStrings, NChars;
static long NPChars;

#define OUT(s)								\
fprintf(Portable_File, s);						\
break

void
print_a_char(c, name)
     fast char c;
     char *name;
{
  switch(c)
  {
    case '\n': OUT("\\n");
    case '\t': OUT("\\t");
    case '\b': OUT("\\b");
    case '\r': OUT("\\r");
    case '\f': OUT("\\f");
    case '\\': OUT("\\\\");
    case '\0': OUT("\\0");
    case ' ' : OUT(" ");
    default:
    if ((isalpha(c)) || (isdigit(c)) || (ispunct(c)))
    {
      putc(c, Portable_File);
    }
    else
    {
      fprintf(stderr,
	      "%s: %s: File may not be portable: c = 0x%x\n",
	      Program_Name, name, ((int) c));
      /* This does not follow C conventions, but eliminates ambiguity */
      fprintf(Portable_File, "\X%x ", ((int) c));
    }
  }
  return;
}

#define Do_Compound(Code, Rel, Fre, Scn, Obj, FObj, kernel_code)	\
{									\
  Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
									\
  if (Type_Code(Old_Contents) == TC_BROKEN_HEART)			\
  {									\
    Mem_Base[(Scn)] = Make_New_Pointer((Code), Old_Contents);		\
  }									\
  else									\
  {									\
    kernel_code;							\
  }									\
}

#define standard_kernel(kernel_code, type, Code, Scn, Obj, FObj)	\
{									\
  fast long length;							\
									\
  Mem_Base[(Scn)] = Make_Non_Pointer((Code), (Obj));			\
  length = Get_Integer(Old_Contents);					\
  kernel_code;								\
  *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Obj));		\
  (Obj) += 1;								\
  *(FObj)++ = Make_Non_Pointer((type), 0);				\
  *(FObj)++ = Old_Contents;						\
  while(--length >= 0)							\
  {									\
    *(FObj)++ = *Old_Address++;						\
  }									\
}

#define do_string_kernel()						\
{									\
  NStrings += 1;							\
  NChars += pointer_to_char(length - 1);				\
}

#define do_bignum_kernel()						\
{									\
  NIntegers += 1;							\
  NBits += bignum_to_bits(LEN(BIGNUM(Old_Address)));			\
}

#define do_bit_string_kernel()						\
{									\
  NBitstrs += 1;							\
  NBBits += Old_Address[BIT_STRING_LENGTH_OFFSET];			\
}

#define do_flonum_kernel(Code, Scn, Obj, FObj)				\
{									\
  Mem_Base[(Scn)] = Make_Non_Pointer((Code), (Obj));			\
  NFlonums += 1;							\
  *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Obj));		\
  (Obj) += 1;								\
  *(FObj)++ = Make_Non_Pointer(TC_BIG_FLONUM, 0);			\
  *((double *) (FObj)) = *((double *) Old_Address);			\
  (FObj) += float_to_pointer;						\
}

#define Do_String(Code, Rel, Fre, Scn, Obj, FObj)			\
  Do_Compound(Code, Rel, Fre, Scn, Obj, FObj,				\
	      standard_kernel(do_string_kernel(), TC_CHARACTER_STRING,	\
			      Code, Scn, Obj, FObj))

#define Do_Bignum(Code, Rel, Fre, Scn, Obj, FObj)			\
  Do_Compound(Code, Rel, Fre, Scn, Obj, FObj,				\
	      standard_kernel(do_bignum_kernel(), TC_BIG_FIXNUM,	\
			      Code, Scn, Obj, FObj))

#define Do_Bit_String(Code, Rel, Fre, Scn, Obj, FObj)			\
  Do_Compound(Code, Rel, Fre, Scn, Obj, FObj,				\
	      standard_kernel(do_bit_string_kernel(), TC_BIT_STRING,	\
			      Code, Scn, Obj, FObj))

#define Do_Flonum(Code, Rel, Fre, Scn, Obj, FObj)			\
  Do_Compound(Code, Rel, Fre, Scn, Obj, FObj,				\
	      do_flonum_kernel(Code, Scn, Obj, FObj))

void
print_a_fixnum(val)
     long val;
{
  fast long size_in_bits;
  fast unsigned long temp;

  temp = ((val < 0) ? -val : val);
  for (size_in_bits = 0; temp != 0; size_in_bits += 1)
  {
    temp = temp >> 1;
  }
  fprintf(Portable_File, "%02x %c ",
	  TC_FIXNUM,
	  (val < 0 ? '-' : '+'));
  if (val == 0)
  {
    fprintf(Portable_File, "0\n");
  }
  else
  {
    fprintf(Portable_File, "%ld ", size_in_bits);
    temp = ((val < 0) ? -val : val);
    while (temp != 0)
    {
      fprintf(Portable_File, "%01lx", (temp & 0xf));
      temp = temp >> 4;
    }
    fprintf(Portable_File, "\n");
  }
  return;
}

void
print_a_string_internal(len, string)
     fast long len;
     fast char *string;
{
  fprintf(Portable_File, "%ld ", len);
  if (Shuffle_Bytes)
  {
    while(len > 0)
    {
      print_a_char(string[3], "print_a_string");
      if (len > 1)
      {
	print_a_char(string[2], "print_a_string");
      }
      if (len > 2)
      {
	print_a_char(string[1], "print_a_string");
      }
      if (len > 3)
      {
	print_a_char(string[0], "print_a_string");
      }
      len -= 4;
      string += 4;
    }
  }
  else
  {
    while(--len >= 0)
    {
      print_a_char(*string++, "print_a_string");
    }
  }
  putc('\n', Portable_File);
  return;
}

void
print_a_string(from)
     Pointer *from;
{
  long len;
  long maxlen;

  maxlen = pointer_to_char((Get_Integer(*from++)) - 1);
  len = STRING_LENGTH_TO_LONG(*from++);

  fprintf(Portable_File,
	  "%02x %ld ",
	  TC_CHARACTER_STRING,
	  (Compact_P ? len : maxlen));

  print_a_string_internal(len, ((char *) from));
  return;
}

void
print_a_primitive(arity, length, name)
     long arity, length;
     char *name;
{
  fprintf(Portable_File, "%ld ", arity);
  print_a_string_internal(length, name);
  return;
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
  {
    fprintf(Portable_File, "%02x + 0\n",
	    (Compact_P ? TC_FIXNUM : TC_BIG_FIXNUM));
  }
  else
  {
    fast long tail;

    for (size_in_bits = ((temp - 1) * SHIFT),
	 temp = ((long) (*Bignum_Top(the_number)));
	 temp != 0;
	 size_in_bits += 1)
    {
      temp = temp >> 1;
    }

    fprintf(Portable_File, "%02x %c %ld ",
	    (Compact_P ? TC_FIXNUM : TC_BIG_FIXNUM),
	    (NEG_BIGNUM(the_number) ? '-' : '+'),
	    size_in_bits);
    tail = size_in_bits % SHIFT;
    if (tail == 0)
    {
      tail = SHIFT;
    }
    temp = 0;
    size_in_bits = 0;
    the_top = Bignum_Top(the_number);
    for(the_number = Bignum_Bottom(the_number);
	the_number <= the_top;
	the_number += 1)
    {
      temp |= (((unsigned long) (*the_number)) << size_in_bits);
      for (size_in_bits += ((the_number != the_top) ? SHIFT : tail);
	   size_in_bits > 3;
	   size_in_bits -= 4)
      {
	fprintf(Portable_File, "%01lx", (temp & 0xf));
	temp = temp >> 4;
      }
    }
    if (size_in_bits > 0)
    {
      fprintf(Portable_File, "%01lx\n", (temp & 0xf));
    }
    else
    {
      fprintf(Portable_File, "\n");
    }
  }
  return;
}

/* The following procedure assumes that a C long is at least 4 bits. */

void
print_a_bit_string(from)
     Pointer *from;
{
  Pointer the_bit_string;
  fast long bits_remaining, leftover_bits;
  fast Pointer accumulator, next_word, *scan;

  the_bit_string = Make_Pointer(TC_BIT_STRING, from);
  bits_remaining = bit_string_length(the_bit_string);
  fprintf(Portable_File, "%02x %ld", TC_BIT_STRING, bits_remaining);

  if (bits_remaining != 0)
  {
    fprintf(Portable_File, " ");
    scan = bit_string_low_ptr(the_bit_string);
    for (leftover_bits = 0;
	 bits_remaining > 0;
	 bits_remaining -= POINTER_LENGTH)
    {
      next_word = *(inc_bit_string_ptr(scan));

      if (bits_remaining < POINTER_LENGTH)
	next_word &= low_mask(bits_remaining);

      if (leftover_bits != 0)
      {
	accumulator &= low_mask(leftover_bits);
	accumulator |=
	  ((next_word & low_mask(4 - leftover_bits)) << leftover_bits);
	next_word = (next_word >> (4 - leftover_bits));
	leftover_bits += ((bits_remaining > POINTER_LENGTH) ?
			  (POINTER_LENGTH - 4) :
			  (bits_remaining - 4));
	fprintf(Portable_File, "%01lx", (accumulator & 0xf));
      }
      else
      {
	leftover_bits = ((bits_remaining > POINTER_LENGTH) ?
			 POINTER_LENGTH :
			 bits_remaining);
      }

      for(accumulator = next_word; leftover_bits >= 4; leftover_bits -= 4)
      {
	fprintf(Portable_File, "%01lx", (accumulator & 0xf));
	accumulator = accumulator >> 4;
      }
    }
    if (leftover_bits != 0)
    {
      fprintf(Portable_File, "%01lx", (accumulator & 0xf));
    }
  }
  fprintf(Portable_File, "\n");
  return;
}

void
print_a_flonum(val)
     double val;
{
  fast long size_in_bits;
  fast double mant, temp;
  int expt;
  extern double frexp();

  fprintf(Portable_File, "%02x %c ",
	  TC_BIG_FLONUM,
	  ((val < 0.0) ? '-' : '+'));
  if (val == 0.0)
  {
    fprintf(Portable_File, "0\n");
    return;
  }
  mant = frexp(((val < 0.0) ? -val : val), &expt);
  size_in_bits = 1;

  for(temp = ((mant * 2.0) - 1.0);
      temp != 0;
      size_in_bits += 1)
  {
    temp *= 2.0;
    if (temp >= 1.0)
      temp -= 1.0;
  }
  fprintf(Portable_File, "%ld %ld ", expt, size_in_bits);

  for (size_in_bits = hex_digits(size_in_bits);
       size_in_bits > 0;
       size_in_bits -= 1)
  {
    fast unsigned int digit;

    digit = 0;
    for (expt = 4; --expt >= 0;)
    {
      mant *= 2.0;
      digit = digit << 1;
      if (mant >= 1.0)
      {
	mant -= 1.0;
	digit += 1;
      }
    }
    fprintf(Portable_File, "%01x", digit);
  }
  putc('\n', Portable_File);
  return;
}

/* Normal Objects */

#define Do_Cell(Code, Rel, Fre, Scn, Obj, FObj)				\
{									\
  Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
									\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)			\
  {									\
    Mem_Base[(Scn)] = Make_New_Pointer(Type_Code(This), Old_Contents);	\
  }									\
  else									\
  {									\
    *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));		\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));		\
    Mem_Base[(Fre)++] = Old_Contents;					\
  }									\
}

#define Do_Pair(Code, Rel, Fre, Scn, Obj, FObj)				\
{									\
  Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
									\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)			\
  {									\
    Mem_Base[(Scn)] = Make_New_Pointer(Type_Code(This), Old_Contents);	\
  }									\
  else									\
  {									\
    *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));		\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));		\
    Mem_Base[(Fre)++] = Old_Contents;					\
    Mem_Base[(Fre)++] = *Old_Address++;					\
  }									\
}

#define Do_Triple(Code, Rel, Fre, Scn, Obj, FObj)			\
{									\
  Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
									\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)			\
  {									\
    Mem_Base[(Scn)] = Make_New_Pointer(Type_Code(This), Old_Contents);	\
  }									\
  else									\
  {									\
    *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));		\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));		\
    Mem_Base[(Fre)++] = Old_Contents;					\
    Mem_Base[(Fre)++] = *Old_Address++;					\
    Mem_Base[(Fre)++] = *Old_Address++;					\
  }									\
}

#define Do_Quad(Code, Rel, Fre, Scn, Obj, FObj)				\
{									\
  Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
									\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)			\
  {									\
    Mem_Base[(Scn)] = Make_New_Pointer(Type_Code(This), Old_Contents);	\
  }									\
  else									\
  {									\
    *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));		\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));		\
    Mem_Base[(Fre)++] = Old_Contents;					\
    Mem_Base[(Fre)++] = *Old_Address++;					\
    Mem_Base[(Fre)++] = *Old_Address++;					\
    Mem_Base[(Fre)++] = *Old_Address++;					\
  }									\
}

#define Do_Vector(Code, Rel, Fre, Scn, Obj, FObj)			\
{									\
  Old_Address += (Rel);							\
  Old_Contents = *Old_Address;						\
									\
  if (Type_Code(Old_Contents)  == TC_BROKEN_HEART)			\
  {									\
    Mem_Base[(Scn)] = Make_New_Pointer(Type_Code(This), Old_Contents);	\
  }									\
  else									\
  {									\
    fast long len;							\
									\
    len = Get_Integer(Old_Contents);					\
    *Old_Address++ = Make_Non_Pointer(TC_BROKEN_HEART, (Fre));		\
    Mem_Base[(Scn)] = Make_Non_Pointer(Type_Code(This), (Fre));		\
    Mem_Base[(Fre)++] = Old_Contents;					\
    while (len > 0)							\
    {									\
      Mem_Base[(Fre)++] = *Old_Address++;				\
      len -= 1;								\
    }									\
  }									\
}

/* Common Pointer Code */

#define Do_Pointer(Scn, Action)						\
{									\
  Old_Address = Get_Pointer(This);					\
  if (Datum(This) < Const_Base)						\
  {									\
    Action(HEAP_CODE, Heap_Relocation, Free,				\
	   Scn, Objects, Free_Objects);					\
  }									\
  else if (Datum(This) < Dumped_Constant_Top)				\
  {									\
    Action(CONSTANT_CODE, Constant_Relocation, Free_Constant,		\
	   Scn, Constant_Objects, Free_Cobjects);			\
  }									\
  else									\
  {									\
    fprintf(stderr,							\
	    "%s: File is not portable: Pointer to stack.\n",		\
	    Program_Name);						\
    quit(1);								\
  }									\
  (Scn) += 1;								\
  break;								\
}

/* Primitive upgrading code. */

#define PRIMITIVE_UPGRADE_SPACE 2048
static Pointer *internal_renumber_table;
static Pointer *external_renumber_table;
static Pointer *external_prim_name_table;
static Boolean found_ext_prims = false;

Pointer *
relocate(object)
     Pointer object;
{
  Pointer *result;
  result = (Get_Pointer(object) + ((Datum(object) < Const_Base) ?
				   Heap_Relocation :
				   Constant_Relocation));
  return (result);
}

Pointer
upgrade_primitive(prim)
     Pointer prim;
{
  long datum, type, new_type, code;
  Pointer new;

  datum = OBJECT_DATUM(prim);
  type = OBJECT_TYPE(prim);
  if (type != TC_PRIMITIVE_EXTERNAL)
  {
    code = datum;
    new_type = type;
  }
  else
  {
    found_ext_prims = true;
    code = (datum + (MAX_BUILTIN_PRIMITIVE + 1));
    new_type = TC_PRIMITIVE;
  }

  new = internal_renumber_table[code];
  if (new == NIL)
  {
    /*
      This does not need to check for overflow because the worst case
      was checked in setup_primitive_upgrade;
     */

    new = Make_Non_Pointer(new_type, Primitive_Table_Length);
    internal_renumber_table[code] = new;
    external_renumber_table[Primitive_Table_Length] = prim;
    Primitive_Table_Length += 1;
    if (type == TC_PRIMITIVE_EXTERNAL)
    {
      NPChars +=
	STRING_LENGTH_TO_LONG((((Pointer *) (external_prim_name_table[datum]))
			       [STRING_LENGTH]));
    }
    else
    {
      NPChars += strlen(builtin_prim_name_table[datum]);
    }
    return (new);
  }
  else
  {
    return (Make_New_Pointer(new_type, new));
  }
}

Pointer *
setup_primitive_upgrade(Heap)
     Pointer *Heap;
{
  fast long count, length;
  Pointer *old_prims_vector;
  
  internal_renumber_table = &Heap[0];
  external_renumber_table =
    &internal_renumber_table[PRIMITIVE_UPGRADE_SPACE];
  external_prim_name_table =
    &external_renumber_table[PRIMITIVE_UPGRADE_SPACE];

  old_prims_vector = relocate(Ext_Prim_Vector);
  if (*old_prims_vector == NIL)
  {
    length = 0;
  }
  else
  {
    old_prims_vector = relocate(*old_prims_vector);
    length = Get_Integer(*old_prims_vector);
    old_prims_vector += VECTOR_DATA;
    for (count = 0; count < length; count += 1)
    {
      Pointer *temp;

      /* symbol */
      temp = relocate(old_prims_vector[count]);
      /* string */
      temp = relocate(temp[SYMBOL_NAME]);
      external_prim_name_table[count] = ((Pointer) temp);
    }
  }
  length += (MAX_BUILTIN_PRIMITIVE + 1);
  if (length > PRIMITIVE_UPGRADE_SPACE)
  {
    fprintf(stderr, "%s: Too many primitives.\n", Program_Name);
    fprintf(stderr,
	    "Increase PRIMITIVE_UPGRADE_SPACE and recompile %s.\n",
	    Program_Name);
    quit(1);
  }
  for (count = 0; count < length; count += 1)
  {
    internal_renumber_table[count] = NIL;
  }
  NPChars = 0;
  return (&external_prim_name_table[PRIMITIVE_UPGRADE_SPACE]);
}

/* Processing of a single area */

#define Do_Area(Code, Area, Bound, Obj, FObj)				\
  Process_Area(Code, &Area, &Bound, &Obj, &FObj)

Process_Area(Code, Area, Bound, Obj, FObj)
     int Code;
     fast long *Area, *Bound;
     fast long *Obj;
     fast Pointer **FObj;
{
  fast Pointer This, *Old_Address, Old_Contents;

  while(*Area != *Bound)
  {
    This = Mem_Base[*Area];

#ifdef PRIMITIVE_EXTERNAL_REUSED
    if (upgrade_primitives && (Type_Code(This) == TC_PRIMITIVE_EXTERNAL))
    {
      Mem_Base[*Area] = upgrade_primitive(This);
      *Area += 1;
      continue;
    }
#endif /* PRIMITIVE_EXTERNAL_REUSED */

    Switch_by_GC_Type(This)
    {
#ifndef PRIMITIVE_EXTERNAL_REUSED

      case TC_PRIMITIVE_EXTERNAL:

#endif /* PRIMITIVE_EXTERNAL_REUSED */

      case TC_PRIMITIVE:
      case TC_PCOMB0:
	if (upgrade_primitives)
	{
	  Mem_Base[*Area] = upgrade_primitive(This);
	}
	*Area += 1;
	break;

      case TC_MANIFEST_NM_VECTOR:
        if (Null_NMV)
	{
	  fast int i;

	  i = Get_Integer(This);
	  *Area += 1;
	  for ( ; --i >= 0; *Area += 1)
	  {
	    Mem_Base[*Area] = NIL;
	  }
	  break;
	}
        fprintf(stderr, "%s: File is not portable: NMH found\n",
		Program_Name);
	*Area += 1 + Get_Integer(This);
	break;

      case TC_BROKEN_HEART:
	/* [Broken Heart 0] is the cdr of fasdumped symbols. */
	if (OBJECT_DATUM(This) != 0)
	{
	  fprintf(stderr, "%s: Broken Heart found in scan.\n",
		  Program_Name);
	  quit(1);
	}
	*Area += 1;
	break;

      case TC_STACK_ENVIRONMENT:
      case_compiled_entry_point:
	fprintf(stderr,
		"%s: File is not portable: Compiled code.\n",
		Program_Name);
	quit(1);

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
	  quit(1);
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
	  quit(1);
	}
	/* Fall through */

      case TC_FUTURE:
      case_simple_Vector:
	if (Type_Code(This) == TC_BIT_STRING)
	{
	  Do_Pointer(*Area, Do_Bit_String);
	}
	else
	{
	  Do_Pointer(*Area, Do_Vector);
	}

      default:
      Bad_Type:
	fprintf(stderr, "%s: Unknown Type Code 0x%x found.\n",
		Program_Name, Type_Code(This));
	quit(1);
      }
  }
}

/* Output macros */

#define print_external_object(from)					\
{									\
  switch(Type_Code(*from))						\
  {									\
    case TC_FIXNUM:							\
    {									\
      long Value;							\
									\
      Sign_Extend(*from++, Value);					\
      print_a_fixnum(Value);						\
      break;								\
    }									\
									\
    case TC_BIT_STRING:							\
      print_a_bit_string(++from);					\
      from += (1 + Get_Integer(*from));					\
      break;								\
									\
    case TC_BIG_FIXNUM:							\
      print_a_bignum(++from);						\
      from += (1 + Get_Integer(*from));					\
      break;								\
									\
    case TC_CHARACTER_STRING:						\
      print_a_string(++from);						\
      from += (1 + Get_Integer(*from));					\
      break;								\
									\
    case TC_BIG_FLONUM:							\
      print_a_flonum( *((double *) (from + 1)));			\
      from += (1 + float_to_pointer);					\
      break;								\
									\
    case TC_CHARACTER:							\
      fprintf(Portable_File, "%02x %03x\n",				\
	      TC_CHARACTER, (*from & MASK_EXTNDD_CHAR));		\
      from += 1;							\
      break;								\
									\
    default:								\
      fprintf(stderr,							\
	      "%s: Bad Object to print externally %lx\n",		\
	      Program_Name, *from);					\
      quit(1);								\
  }									\
}

#define print_an_object(obj)						\
{									\
  fprintf(Portable_File, "%02x %lx\n",					\
	  Type_Code(obj), Get_Integer(obj));				\
}

/* Debugging Aids and Consistency Checks */

#ifdef DEBUG

#define DEBUGGING(action)		action

#define WHEN(condition, message)	when(condition, message)

void
when(what, message)
     Boolean what;
     char *message;
{
  if (what)
  {
    fprintf(stderr, "%s: Inconsistency: %s!\n",
	    Program_Name, (message));
    quit(1);
  }
  return;
}

#define PRINT_HEADER(name, obj, format)					\
{									\
  fprintf(Portable_File, (format), (obj));				\
  fprintf(stderr, "%s: ", (name));					\
  fprintf(stderr, (format), (obj));					\
}

#else /* not DEBUG */

#define DEBUGGING(action)

#define WHEN(what, message)

#define PRINT_HEADER(name, obj, format)					\
{									\
  fprintf(Portable_File, (format), (obj));				\
}

#endif /* DEBUG */

/* The main program */

void
do_it()
{
  Pointer *Heap;
  long Initial_Free;

  /* Load the Data */

  if (!Read_Header())
  {
    fprintf(stderr,
	    "%s: Input file does not appear to be in FASL format.\n",
	    Program_Name);
    quit(1);
  }

  if ((Version > FASL_READ_VERSION) ||
      (Version < FASL_OLDEST_VERSION) ||
      (Sub_Version > FASL_READ_SUBVERSION) ||
      (Sub_Version < FASL_OLDEST_SUBVERSION) ||
      ((Machine_Type != FASL_INTERNAL_FORMAT) &&
       (!Shuffle_Bytes)))
  {
    fprintf(stderr, "%s:\n", Program_Name);
    fprintf(stderr,
	    "FASL File Version %ld Subversion %ld Machine Type %ld\n",
	    Version, Sub_Version , Machine_Type);
    fprintf(stderr,
	    "Expected: Version %d Subversion %d Machine Type %d\n",
	    FASL_READ_VERSION, FASL_READ_SUBVERSION, FASL_INTERNAL_FORMAT);
    quit(1);
  }

  if (Machine_Type == FASL_INTERNAL_FORMAT)
  {
    Shuffle_Bytes = false;
  }

  upgrade_traps = (Sub_Version < FASL_REFERENCE_TRAP);
  upgrade_primitives = (Sub_Version < FASL_MERGED_PRIMITIVES);
  upgrade_lengths = upgrade_primitives;

  /* Constant Space not currently supported */

  if (Const_Count != 0)
  {
    fprintf(stderr,
	    "%s: Input file has a constant space area.\n",
	    Program_Name);
    quit(1);
  }

  {
    long Size;

    Size = ((3 * (Heap_Count + Const_Count)) +
	    (NROOTS + 1) +
	    (upgrade_primitives ?
	     (3 * PRIMITIVE_UPGRADE_SPACE) :
	     Primitive_Table_Size));
    Allocate_Heap_Space(Size + HEAP_BUFFER_SPACE);

    if (Heap == NULL)
    {
      fprintf(stderr,
	      "%s: Memory Allocation Failed.  Size = %ld Scheme Pointers\n",
	      Program_Name, Size);
      quit(1);
    }
  }

  Heap += HEAP_BUFFER_SPACE;
  Initial_Align_Float(Heap);
  Load_Data(Heap_Count, &Heap[0]);
  Load_Data(Const_Count, &Heap[Heap_Count]);
  Load_Data(Primitive_Table_Size, &Heap[Heap_Count + Const_Count]);
  Heap_Relocation = &Heap[0] - Get_Pointer(Heap_Base);
  Constant_Relocation = &Heap[Heap_Count] - Get_Pointer(Const_Base);

  DEBUGGING(fprintf(stderr,
		    "Dumped Heap Base = 0x%08x\n",
		    Heap_Base));

  DEBUGGING(fprintf(stderr,
		    "Dumped Constant Base = 0x%08x\n",
		    Const_Base));

  DEBUGGING(fprintf(stderr,
		    "Dumped Constant Top = 0x%08x\n",
		    Dumped_Constant_Top));

  DEBUGGING(fprintf(stderr,
		    "Heap Count = %6d\n",
		    Heap_Count));

  DEBUGGING(fprintf(stderr,
		    "Constant Count = %6d\n",
		    Const_Count));

  /* Determine primitive information. */

  primitive_table = &Heap[Heap_Count + Const_Count];
  if (upgrade_primitives)
  {
    Mem_Base = setup_primitive_upgrade(primitive_table);
  }
  else
  {
    fast Pointer *table;
    fast long count, char_count;

    for (char_count = 0,
	 count = Primitive_Table_Length,
	 table = primitive_table;
	 --count >= 0;)
    {
      char_count += STRING_LENGTH_TO_LONG(table[1 + STRING_LENGTH]);
      table += (2 + Get_Integer(table[1 + STRING_HEADER]));
    }
    NPChars = char_count;
    Mem_Base = &primitive_table[Primitive_Table_Size];
  }

  /* Reformat the data */

  NFlonums = NIntegers = NStrings = 0;
  NBits = NBBits = NChars = 0;

  Mem_Base[0] = Make_New_Pointer(TC_CELL, Dumped_Object);
  Initial_Free = NROOTS;
  Scan = 0;

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

  /*
    When Constant Space finally becomes supported,
    something like this must be done.
   */

  while (true)
  {
    Do_Area(HEAP_CODE, Scan, Free,
	    Objects, Free_Objects);
    Do_Area(CONSTANT_CODE, Scan_Constant, Free_Constant,
	    Constant_Objects, Free_Cobjects);
    Do_Area(PURE_CODE, Scan_Pure, Free_Pure,
	    Pure_Objects, Free_Pobjects);
    if (Scan == Free)
    {
      break;
    }
  }

#endif

  /* Consistency checks */

  WHEN(((Free - Initial_Free) > Heap_Count), "Free overran Heap");

  WHEN(((Free_Objects - &Mem_Base[Initial_Free + Heap_Count]) >
	Heap_Count),
       "Free_Objects overran Heap Object Space");

  WHEN(((Free_Constant - (Initial_Free + (2 * Heap_Count))) > Const_Count),
       "Free_Constant overran Constant Space");

  WHEN(((Free_Cobjects - &Mem_Base[Initial_Free +
				   (2 * Heap_Count) + Const_Count]) >
	Const_Count),
       "Free_Cobjects overran Constant Object Space");

  /* Output the data */

  if (found_ext_prims)
  {
    fprintf(stderr, "%s:\n", Program_Name);
    fprintf(stderr, "NOTE: The arity of some primitives is not known.\n");
    fprintf(stderr, "      The portable file has %ld as their arity.\n",
	    UNKNOWN_PRIMITIVE_ARITY);
    fprintf(stderr, "      You may want to fix this by hand.\n");
  }

  /* Header */

  PRINT_HEADER("Portable Version", PORTABLE_VERSION, "%ld\n");
  PRINT_HEADER("Flags", Make_Flags(), "%ld\n");
  PRINT_HEADER("Version", FASL_FORMAT_VERSION, "%ld\n");
  PRINT_HEADER("Sub Version", FASL_SUBVERSION, "%ld\n");

  PRINT_HEADER("Heap Count", (Free - NROOTS), "%ld\n");
  PRINT_HEADER("Heap Base", NROOTS, "%ld\n");
  PRINT_HEADER("Heap Objects", Objects, "%ld\n");

  /* Currently Constant and Pure not supported, but the header is ready */

  PRINT_HEADER("Pure Count", 0, "%ld\n");
  PRINT_HEADER("Pure Base", Free_Constant, "%ld\n");
  PRINT_HEADER("Pure Objects", 0, "%ld\n");

  PRINT_HEADER("Constant Count", 0, "%ld\n");
  PRINT_HEADER("Constant Base", Free_Constant, "%ld\n");
  PRINT_HEADER("Constant Objects", 0, "%ld\n");

  PRINT_HEADER("& Dumped Object", (Get_Integer(Mem_Base[0])), "%ld\n");

  PRINT_HEADER("Number of flonums", NFlonums, "%ld\n");
  PRINT_HEADER("Number of integers", NIntegers, "%ld\n");
  PRINT_HEADER("Number of bits in integers", NBits, "%ld\n");
  PRINT_HEADER("Number of bit strings", NBitstrs, "%ld\n");
  PRINT_HEADER("Number of bits in bit strings", NBBits, "%ld\n");
  PRINT_HEADER("Number of character strings", NStrings, "%ld\n");
  PRINT_HEADER("Number of characters in strings", NChars, "%ld\n");

  PRINT_HEADER("Number of primitives", Primitive_Table_Length, "%ld\n");
  PRINT_HEADER("Number of characters in primitives", NPChars, "%ld\n");

  /* External Objects */
  
  /* Heap External Objects */

  Free_Objects = &Mem_Base[Initial_Free + Heap_Count];
  for (; Objects > 0; Objects -= 1)
  {
    print_external_object(Free_Objects);
  }
  
#if false
  /* Pure External Objects */

  Free_Cobjects = &Mem_Base[Pure_Objects_Start];
  for (; Pure_Objects > 0; Pure_Objects -= 1)
  {
    print_external_object(Free_Cobjects);
  }

  /* Constant External Objects */

  Free_Cobjects = &Mem_Base[Constant_Objects_Start];
  for (; Constant_Objects > 0; Constant_Objects -= 1)
  {
    print_external_object(Free_Cobjects);
  }

#endif

  /* Pointer Objects */

  /* Heap Objects */

  Free_Cobjects = &Mem_Base[Free];
  for (Free_Objects = &Mem_Base[NROOTS];
       Free_Objects < Free_Cobjects;
       Free_Objects += 1)
  {
    print_an_object(*Free_Objects);
  }

#if false
  /* Pure Objects */

  Free_Cobjects = &Mem_Base[Free_Pure];
  for (Free_Objects = &Mem_Base[Pure_Start];
       Free_Objects < Free_Cobjects;
       Free_Objects += 1)
  {
    print_an_object(*Free_Objects);
  }

  /* Constant Objects */

  Free_Cobjects = &Mem_Base[Free_Constant];
  for (Free_Objects = &Mem_Base[Constant_Start];
       Free_Objects < Free_Cobjects;
       Free_Objects += 1)
  {
    print_an_object(*Free_Objects);
  }
#endif

  /* Primitives */

  if (upgrade_primitives)
  {
    Pointer obj;
    fast Pointer *table;
    fast long count, datum;

    for (count = Primitive_Table_Length,
	 table = external_renumber_table;
	 --count >= 0;)
    {
      obj = *table++;
      datum = OBJECT_DATUM(obj);
      if (OBJECT_TYPE(obj) == TC_PRIMITIVE_EXTERNAL)
      {
	Pointer *strobj;

	strobj = ((Pointer *) (external_prim_name_table[datum]));
	print_a_primitive(((long) UNKNOWN_PRIMITIVE_ARITY),
			  (STRING_LENGTH_TO_LONG(strobj[STRING_LENGTH])),
			  ((char *) &strobj[STRING_CHARS]));
      }
      else
      {
	char *string;

	string = builtin_prim_name_table[datum];
	print_a_primitive(((long) builtin_prim_arity_table[datum]),
			  ((long) strlen(string)),
			  string);
      }
    }
  }
  else
  {
    fast Pointer *table;
    fast long count;
    long arity;

    for (count = Primitive_Table_Length, table = primitive_table;
	 --count >= 0;)
    {
      Sign_Extend(*table, arity);
      table += 1;
      print_a_primitive(arity,
			(STRING_LENGTH_TO_LONG(table[STRING_LENGTH])),
			((char *) &table[STRING_CHARS]));
      table += (1 + Get_Integer(table[STRING_HEADER]));
    }
  }
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
{
  Setup_Program(argc, argv, Noptions, Options);
  do_it();
  quit(0);
}
