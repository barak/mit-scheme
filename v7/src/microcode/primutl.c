/* -*-C-*-

Copyright (c) 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/primutl.c,v 9.46 1988/08/15 20:53:13 cph Exp $
 *
 * This file contains the support routines for mapping primitive names
 * to numbers within the microcode.  Primitives are written in C
 * and available in Scheme, but not always present in all versions of
 * the interpreter.  Thus, these objects are always referenced
 * externally by name and converted to numeric references only for the
 * duration of a single Scheme session.
 */

#include "scheme.h"
#include "prims.h"

Pointer Undefined_Primitives = NIL;
Pointer Undefined_Primitives_Arity = NIL;

/* Common utilities. */

struct primitive_alias
  {
    char *alias;
    char *name;
  };

#include "prename.h"

static char *
primitive_alias_to_name (alias)
     char *alias;
{
  fast struct primitive_alias *alias_ptr;
  fast struct primitive_alias *alias_end;

  alias_ptr = aliases;
  alias_end = (alias_ptr + N_ALIASES);
  while (alias_ptr < alias_end)
    {
      if ((strcmp (alias, (alias_ptr -> alias))) == 0)
	return (alias_ptr -> name);
      alias_ptr += 1;
    }
  return (alias);
}

/*
  In primitive_name_to_code, size is really 1 less than size.
  It is really the index of the last valid entry.
 */

#if false

/* This version performs an expensive linear search. */

long
primitive_name_to_code(name, table, size)
     char *name;
     char *table[];
     int size;
{
  fast int i;

  name = (primitive_alias_to_name (name));
  for (i = size; i >= 0; i -= 1)
  {
    fast char *s1, *s2;

    s1 = name;
    s2 = table[i];

    while (*s1++ == *s2)
    {
      if (*s2++ == '\0')
      {
	return ((long) i);
      }
    }
  }
  return ((long) (-1));
}

#else /* not false */

/* This version performs a log (base 2) search.
   The table is assumed to be ordered alphabetically.
 */
   
long
primitive_name_to_code(name, table, size)
     char *name;
     fast char *table[];
     int size;
{
  extern int strcmp();
  fast int low, high, middle, result;

  name = (primitive_alias_to_name (name));
  low = 0;
  high = size;

  while(low < high)
  {
    middle = ((low + high) / 2);
    result = strcmp(name, table[middle]);
    if (result < 0)
    {
      high = (middle - 1);
    }
    else if (result > 0)
    {
      low = (middle + 1);
    }
    else
    {
      return ((long) middle);
    }
  }

  /* This takes care of the fact that division rounds down.
     If division were to round up, we would have to use high.
   */

  if (strcmp(name, table[low]) == 0)
  {
    return ((long) low);
  }
  return ((long) -1);
}

#endif /* false */

long
primitive_code_to_arity(number)
     long number;
{
  if (number <= MAX_PRIMITIVE)
  {
    return ((long) Primitive_Arity_Table[number]);
  }
  else
  {
    Pointer entry;
    long arity;

    entry = User_Vector_Ref(Undefined_Primitives_Arity,
			    (number - MAX_PRIMITIVE));
    if (entry == NIL)
    {
      return ((long) UNKNOWN_PRIMITIVE_ARITY);
    }
    else
    {
      Sign_Extend(entry, arity);
    }
    return (arity);
  }
}

char *
primitive_code_to_documentation (number)
     long number;
{
  return
    ((number > MAX_PRIMITIVE)
     ? ((char *) 0)
     : (Primitive_Documentation_Table [number]));
}

/* Externally visible utilities */

extern Pointer make_primitive();

Pointer
make_primitive(name)
     char *name;
{
  Pointer search_for_primitive();

  return (search_for_primitive(NIL, name, true, true,
			       UNKNOWN_PRIMITIVE_ARITY));
}

extern Pointer find_primitive();

Pointer
find_primitive(name, intern_p, allow_p, arity)
     Pointer name;
     Boolean intern_p, allow_p;
     int arity;
{
  Pointer search_for_primitive();

  return (search_for_primitive(name, Scheme_String_To_C_String(name),
			       intern_p, allow_p, arity));
}

extern long primitive_to_arity();

long
primitive_to_arity(primitive)
     Pointer primitive;
{
  return (primitive_code_to_arity(PRIMITIVE_NUMBER(primitive)));
}

extern char * primitive_to_documentation ();

char *
primitive_to_documentation (primitive)
     Pointer primitive;
{
  return (primitive_code_to_documentation (PRIMITIVE_NUMBER (primitive)));
}

extern long primitive_to_arguments();

/*
  This is only valid during the invocation of a primitive.
  It is used by various utilities to back out of code.
 */

long
primitive_to_arguments(primitive)
     Pointer primitive;
{
  long arity;

  arity = primitive_code_to_arity(PRIMITIVE_NUMBER(primitive));

  if (arity == ((long) LEXPR_PRIMITIVE_ARITY))
  {
    arity = ((long) Regs[REGBLOCK_LEXPR_ACTUALS]);
  }
  return (arity);
}

char *
primitive_code_to_name(code)
  int code;
{
  char *string;

  if (code <= MAX_PRIMITIVE)
  {
    string = Primitive_Name_Table[code];
  }
  else
  {
    /* NOTE:
       This is invoked by cons_primitive_table which is invoked by
       fasdump before the "fixups" are undone.  This means that the scheme
       string may actually have a broken heart as its first word, but
       this code will still work because the characters will still be there.
     */

    Pointer scheme_string;

    scheme_string = User_Vector_Ref(Undefined_Primitives,
				    (code - MAX_PRIMITIVE));
    string = Scheme_String_To_C_String(scheme_string);
  }
  return (string);
}

extern char *primitive_to_name();

char *
primitive_to_name(primitive)
     Pointer primitive;
{
  return (primitive_code_to_name(PRIMITIVE_NUMBER(primitive)));
}

/* this avoids some consing. */

Pointer
primitive_name(code)
     int code;
{
  Pointer scheme_string;
  extern Pointer string_to_symbol();

  if (code <= MAX_PRIMITIVE)
  {
    scheme_string = C_String_To_Scheme_String(Primitive_Name_Table[code]);
  }
  else
  {
    scheme_string = User_Vector_Ref(Undefined_Primitives,
				    (code - MAX_PRIMITIVE));
  }
  return (string_to_symbol(scheme_string));
}

/*
  scheme_name can be NIL, meaning cons up from c_name as needed.
  c_name must always be provided.
 */

Pointer
search_for_primitive(scheme_name, c_name, intern_p, allow_p, arity)
     Pointer scheme_name;
     char *c_name;
     Boolean intern_p, allow_p;
     int arity;
{
  extern int strcmp();
  long i, Max, old_arity;
  Pointer *Next;

  i = primitive_name_to_code(c_name,
			     &Primitive_Name_Table[0],
			     MAX_PRIMITIVE);
  if (i != -1)
  {
    old_arity = Primitive_Arity_Table[i];
    if ((arity == UNKNOWN_PRIMITIVE_ARITY) || (arity == old_arity))
    {
      return (MAKE_PRIMITIVE_OBJECT(0, i));
    }
    else
    {
      return (MAKE_SIGNED_FIXNUM(old_arity));
    }
  }
  /* Search the undefined primitives table if allowed. */

  if (!allow_p)
  {
    return (NIL);
  }

  /* The vector should be sorted for faster comparison. */

  Max = NUMBER_OF_UNDEFINED_PRIMITIVES();
  if (Max > 0)
  {
    Next = Nth_Vector_Loc(Undefined_Primitives, 2);

    for (i = 1; i <= Max; i++)
    {
      Pointer temp;

      temp = *Next++;
      if (strcmp(c_name, Scheme_String_To_C_String(temp)) == 0)
      {
	if (arity != UNKNOWN_PRIMITIVE_ARITY)
	{
	  temp = User_Vector_Ref(Undefined_Primitives_Arity, i);
	  if (temp == NIL)
	  {
	    User_Vector_Set(Undefined_Primitives_Arity,
			    i,
			    MAKE_SIGNED_FIXNUM(arity));
	  }
	  else
	  {
	    Sign_Extend(temp, old_arity);
	    if (arity != old_arity)
	    {
	      return (temp);
	    }
	  }
	}
	return (MAKE_PRIMITIVE_OBJECT((MAX_PRIMITIVE + i), (MAX_PRIMITIVE + 1)));
      }
    }
  }

  /*
    Intern the primitive name by adding it to the vector of
    undefined primitives, if interning is allowed.
   */

  if (!intern_p)
  {
    return (NIL);
  }

  if (scheme_name == NIL)
  {
    scheme_name = C_String_To_Scheme_String(c_name);
  }

  if ((Max % CHUNK_SIZE) == 0)
  {
    Primitive_GC_If_Needed(2 * (Max + CHUNK_SIZE + 2));
    if (Max > 0)
    {
      Next = Nth_Vector_Loc(Undefined_Primitives, 2);
    }
    Undefined_Primitives = Make_Pointer(TC_VECTOR, Free);
    *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, (Max + CHUNK_SIZE + 1));
    *Free++ = Make_Unsigned_Fixnum(Max + 1);
    for (i = 0; i < Max; i++)
    {
      *Free++ = Fetch(*Next++);
    }
    *Free++ = scheme_name;
    for (i = 1; i < CHUNK_SIZE; i++)
    {
      *Free++ = NIL;
    }
    if (Max > 0)
    {
      Next = Nth_Vector_Loc(Undefined_Primitives_Arity, 2);
    }
    Undefined_Primitives_Arity = Make_Pointer(TC_VECTOR, Free);
    *Free++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, (Max + CHUNK_SIZE + 1));
    *Free++ = NIL;
    for (i = 0; i < Max; i++)
    {
      *Free++ = Fetch(*Next++);
    }
    *Free++ = ((arity != UNKNOWN_PRIMITIVE_ARITY) ?
	       (MAKE_SIGNED_FIXNUM(arity)) :
	       NIL);
    for (i = 1; i < CHUNK_SIZE; i++)
    {
      *Free++ = NIL;
    }
    Max += 1;
  }
  else
  {
    Max += 1;
    User_Vector_Set(Undefined_Primitives, Max, scheme_name);
    if (arity != UNKNOWN_PRIMITIVE_ARITY)
    {
      User_Vector_Set(Undefined_Primitives_Arity,
		      Max,
		      MAKE_SIGNED_FIXNUM(arity));
    }
    User_Vector_Set(Undefined_Primitives, 0, (MAKE_UNSIGNED_FIXNUM(Max)));
  }
  return (MAKE_PRIMITIVE_OBJECT((MAX_PRIMITIVE + Max), (MAX_PRIMITIVE + 1)));
}

/* Dumping and loading primitive object references. */

extern Pointer
  *load_renumber_table,
  dump_renumber_primitive(),
  *initialize_primitive_table(),
  *cons_primitive_table(),
  *cons_whole_primitive_table();

extern void install_primitive_table();

Pointer *load_renumber_table;
static Pointer *internal_renumber_table;
static Pointer *external_renumber_table;
static long next_primitive_renumber;

Pointer *
initialize_primitive_table(where, end)
     fast Pointer *where;
     Pointer *end;
{
  Pointer *top;
  fast long number_of_primitives;

  number_of_primitives = NUMBER_OF_PRIMITIVES();
  top = &where[2 * number_of_primitives];
  if (top < end)
  {
    internal_renumber_table = where;
    external_renumber_table = &where[number_of_primitives];
    next_primitive_renumber = 0;

    while (--number_of_primitives >= 0)
    {
      *where++ = NIL;
    }
  }
  return (top);
}

Pointer
dump_renumber_primitive(primitive)
     fast Pointer primitive;
{
  fast long number;
  fast Pointer result;

  number = PRIMITIVE_NUMBER(primitive);
  result = internal_renumber_table[number];
  if (result == NIL)
  {
    result = Make_Non_Pointer(OBJECT_TYPE(primitive),
			      next_primitive_renumber);
    internal_renumber_table[number] = result;
    external_renumber_table[next_primitive_renumber] = primitive;
    next_primitive_renumber += 1;
    return (result);
  }
  else
  {
    return (Make_New_Pointer(OBJECT_TYPE(primitive), result));
  }
}

Pointer *
copy_primitive_information(code, start, end)
     long code;
     fast Pointer *start, *end;
{
  extern Pointer *copy_c_string_to_scheme_string();

  if (start < end)
  {
    *start++ = MAKE_SIGNED_FIXNUM(primitive_code_to_arity(((int) code)));
  }
  return
    copy_c_string_to_scheme_string(primitive_code_to_name(((int) code)),
				   start,
				   end);
}

Pointer *
cons_primitive_table(start, end, length)
     Pointer *start, *end;
     long *length;
{
  Pointer *saved;
  long count, code;

  saved = start;
  *length = next_primitive_renumber;

  for (count = 0;
       ((count < next_primitive_renumber) && (start < end));
       count += 1)
  {
    code = (PRIMITIVE_NUMBER(external_renumber_table[count]));
    start = copy_primitive_information(code, start, end);
  }
  return (start);
}

Pointer *
cons_whole_primitive_table(start, end, length)
     Pointer *start, *end;
     long *length;
{
  Pointer *saved;
  long count, number_of_primitives;

  number_of_primitives = NUMBER_OF_PRIMITIVES();
  *length = number_of_primitives;
  saved = start;

  for (count = 0;
       ((count < number_of_primitives) && (start < end));
       count += 1)
  {
    start = copy_primitive_information(count, start, end);
  }
  return (start);
}

void
install_primitive_table(table, length, flush_p)
     fast Pointer *table;
     fast long length;
     Boolean flush_p;
{
  fast Pointer *translation_table;
  Pointer result;
  long arity;

  if (flush_p)
  {
    Undefined_Primitives = NIL;
    Undefined_Primitives_Arity = NIL;
  }

  translation_table = load_renumber_table;
  while (--length >= 0)
  {
    Sign_Extend(*table, arity);
    table += 1;
    result =
      search_for_primitive(Make_Pointer(TC_CHARACTER_STRING, table),
			   ((char *) (&table[STRING_CHARS])),
			   true, true, arity);
    if (OBJECT_TYPE(result) != TC_PRIMITIVE)
    {
      Primitive_Error(ERR_WRONG_ARITY_PRIMITIVES);
    }
    *translation_table++ = result;
    table += (1 + OBJECT_DATUM(*table));
  }
  return;
}
