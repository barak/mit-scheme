/* -*-C-*-

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/primutl.c,v 9.51 1989/09/20 23:10:47 cph Exp $
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
#include <ctype.h>

SCHEME_OBJECT Undefined_Primitives = SHARP_F;
SCHEME_OBJECT Undefined_Primitives_Arity = SHARP_F;

/* Common utilities. */

static int
strcmp_ci (s1, s2)
     fast char * s1;
     fast char * s2;
{
  int length1 = (strlen (s1));
  int length2 = (strlen (s2));
  fast int length = ((length1 < length2) ? length1 : length2);

  while ((length--) > 0)
    {
      fast int c1 = (*s1++);
      fast int c2 = (*s2++);
      if (islower (c1)) c1 = (toupper (c1));
      if (islower (c2)) c2 = (toupper (c2));
      if (c1 < c2) return (-1);
      if (c1 > c2) return (1);
    }
  return (length1 - length2);
}

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
      if ((strcmp_ci (alias, (alias_ptr -> alias))) == 0)
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
  fast int low, high, middle, result;

  name = (primitive_alias_to_name (name));
  low = 0;
  high = size;

  while(low < high)
  {
    middle = ((low + high) / 2);
    result = strcmp_ci(name, table[middle]);
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

  if (strcmp_ci(name, table[low]) == 0)
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
    SCHEME_OBJECT entry;
    long arity;

    entry = VECTOR_REF (Undefined_Primitives_Arity, (number - MAX_PRIMITIVE));
    if (entry == SHARP_F)
    {
      return ((long) UNKNOWN_PRIMITIVE_ARITY);
    }
    else
    {
      arity = FIXNUM_TO_LONG (entry);
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

extern SCHEME_OBJECT make_primitive();

SCHEME_OBJECT
make_primitive(name)
     char *name;
{
  SCHEME_OBJECT search_for_primitive();

  return (search_for_primitive(SHARP_F, name, true, true,
			       UNKNOWN_PRIMITIVE_ARITY));
}

extern SCHEME_OBJECT find_primitive();

SCHEME_OBJECT
find_primitive(name, intern_p, allow_p, arity)
     SCHEME_OBJECT name;
     Boolean intern_p, allow_p;
     int arity;
{
  SCHEME_OBJECT search_for_primitive();

  return (search_for_primitive(name, (STRING_LOC (name, 0)),
			       intern_p, allow_p, arity));
}

extern long primitive_to_arity();

long
primitive_to_arity(primitive)
     SCHEME_OBJECT primitive;
{
  return (primitive_code_to_arity(PRIMITIVE_NUMBER(primitive)));
}

extern char * primitive_to_documentation ();

char *
primitive_to_documentation (primitive)
     SCHEME_OBJECT primitive;
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
     SCHEME_OBJECT primitive;
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

    SCHEME_OBJECT scheme_string;

    scheme_string = VECTOR_REF (Undefined_Primitives, (code - MAX_PRIMITIVE));
    string = ((char *) (STRING_LOC (scheme_string, 0)));
  }
  return (string);
}

extern char *primitive_to_name();

char *
primitive_to_name(primitive)
     SCHEME_OBJECT primitive;
{
  return (primitive_code_to_name(PRIMITIVE_NUMBER(primitive)));
}

/* this avoids some consing. */

SCHEME_OBJECT
primitive_name(code)
     int code;
{
  SCHEME_OBJECT scheme_string;

  if (code <= MAX_PRIMITIVE)
  {
    scheme_string = char_pointer_to_string(Primitive_Name_Table[code]);
  }
  else
  {
    scheme_string = VECTOR_REF (Undefined_Primitives, (code - MAX_PRIMITIVE));
  }
  return (scheme_string);
}

/*
  scheme_name can be #F, meaning cons up from c_name as needed.
  c_name must always be provided.
 */

SCHEME_OBJECT
search_for_primitive(scheme_name, c_name, intern_p, allow_p, arity)
     SCHEME_OBJECT scheme_name;
     char *c_name;
     Boolean intern_p, allow_p;
     int arity;
{
  long i, Max, old_arity;
  SCHEME_OBJECT *Next;

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
      return (LONG_TO_FIXNUM(old_arity));
    }
  }
  /* Search the undefined primitives table if allowed. */

  if (!allow_p)
  {
    return (SHARP_F);
  }

  /* The vector should be sorted for faster comparison. */

  Max = NUMBER_OF_UNDEFINED_PRIMITIVES();
  if (Max > 0)
  {
    Next = MEMORY_LOC (Undefined_Primitives, 2);

    for (i = 1; i <= Max; i++)
    {
      SCHEME_OBJECT temp;

      temp = *Next++;
      if (strcmp_ci(c_name, (STRING_LOC (temp, 0))) == 0)
      {
	if (arity != UNKNOWN_PRIMITIVE_ARITY)
	{
	  temp = VECTOR_REF (Undefined_Primitives_Arity, i);
	  if (temp == SHARP_F)
	    VECTOR_SET
	      (Undefined_Primitives_Arity, i, (LONG_TO_FIXNUM (arity)));
	  else
	  {
	    old_arity = FIXNUM_TO_LONG (temp);
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
    return (SHARP_F);
  }

  if (scheme_name == SHARP_F)
  {
    scheme_name = char_pointer_to_string(c_name);
  }

  if ((Max % CHUNK_SIZE) == 0)
    {
      if (Max > 0)
	Next = (MEMORY_LOC (Undefined_Primitives, 2));
      Undefined_Primitives =
	(allocate_marked_vector (TC_VECTOR, (Max + CHUNK_SIZE + 1), true));
      FAST_MEMORY_SET
	(Undefined_Primitives, 1, (LONG_TO_UNSIGNED_FIXNUM (Max + 1)));
      for (i = 0; (i < Max); i += 1)
	FAST_MEMORY_SET
	  (Undefined_Primitives, (i + 2), (MEMORY_FETCH (*Next++)));
      FAST_MEMORY_SET (Undefined_Primitives, (Max + 2), scheme_name);
      for (i = 1; (i < CHUNK_SIZE); i += 1)
	FAST_MEMORY_SET (Undefined_Primitives, (i + Max + 2), SHARP_F);

      if (Max > 0)
	Next = (MEMORY_LOC (Undefined_Primitives_Arity, 2));
      Undefined_Primitives_Arity =
	(allocate_marked_vector (TC_VECTOR, (Max + CHUNK_SIZE + 1), true));
      FAST_MEMORY_SET (Undefined_Primitives_Arity, 1, SHARP_F);
      for (i = 0; (i < Max); i += 1)
	FAST_MEMORY_SET
	  (Undefined_Primitives_Arity, (i + 2), (MEMORY_FETCH (*Next++)));
      FAST_MEMORY_SET
	(Undefined_Primitives_Arity,
	 (Max + 2),
	 ((arity != UNKNOWN_PRIMITIVE_ARITY)
	  ? (LONG_TO_FIXNUM (arity))
	  : SHARP_F));
      for (i = 1; (i < CHUNK_SIZE); i += 1)
	FAST_MEMORY_SET (Undefined_Primitives, (i + Max + 2), SHARP_F);

      Max += 1;
    }
  else
  {
    Max += 1;
    VECTOR_SET (Undefined_Primitives, Max, scheme_name);
    if (arity != UNKNOWN_PRIMITIVE_ARITY)
    {
      VECTOR_SET (Undefined_Primitives_Arity, Max, (LONG_TO_FIXNUM (arity)));
    }
    VECTOR_SET (Undefined_Primitives, 0, (LONG_TO_UNSIGNED_FIXNUM(Max)));
  }
  return (MAKE_PRIMITIVE_OBJECT((MAX_PRIMITIVE + Max), (MAX_PRIMITIVE + 1)));
}

/* Dumping and loading primitive object references. */

extern SCHEME_OBJECT
  *load_renumber_table,
  dump_renumber_primitive(),
  *initialize_primitive_table(),
  *cons_primitive_table(),
  *cons_whole_primitive_table();

extern void install_primitive_table();

SCHEME_OBJECT *load_renumber_table;
static SCHEME_OBJECT *internal_renumber_table;
static SCHEME_OBJECT *external_renumber_table;
static long next_primitive_renumber;

SCHEME_OBJECT *
initialize_primitive_table(where, end)
     fast SCHEME_OBJECT *where;
     SCHEME_OBJECT *end;
{
  SCHEME_OBJECT *top;
  fast long number_of_primitives;

  number_of_primitives = NUMBER_OF_PRIMITIVES();
  top = &where[2 * number_of_primitives];
  if (top < end)
  {
    internal_renumber_table = where;
    external_renumber_table = &where[number_of_primitives];
    next_primitive_renumber = 0;

    while (--number_of_primitives >= 0)
      (*where++) = SHARP_F;
  }
  return (top);
}

SCHEME_OBJECT
dump_renumber_primitive(primitive)
     fast SCHEME_OBJECT primitive;
{
  fast long number;
  fast SCHEME_OBJECT result;

  number = PRIMITIVE_NUMBER(primitive);
  result = internal_renumber_table[number];
  if (result == SHARP_F)
  {
    result = (OBJECT_NEW_DATUM (primitive, next_primitive_renumber));
    internal_renumber_table[number] = result;
    external_renumber_table[next_primitive_renumber] = primitive;
    next_primitive_renumber += 1;
    return (result);
  }
  else
  {
    return (MAKE_OBJECT_FROM_OBJECTS (primitive, result));
  }
}

/* Is supposed to have a null character. */
static char null_string [] = "";

SCHEME_OBJECT *
copy_primitive_information(code, start, end)
     long code;
     fast SCHEME_OBJECT * start;
     fast SCHEME_OBJECT * end;
{
  if (start < end)
    (*start++) = (LONG_TO_FIXNUM (primitive_code_to_arity ((int) code)));
  {
    fast char * source = (primitive_code_to_name ((int) code));
    SCHEME_OBJECT * saved = start;
    start += STRING_CHARS;
    {
      fast char * dest = ((char *) start);
      fast char * limit = ((char *) end);
      if (source == ((char *) 0))
	source = ((char *) (& (null_string [0])));
      while ((dest < limit) && (((*dest++) = (*source++)) != '\0'))
	;
      if (dest >= limit)
	while ((*source++) != '\0')
	  dest += 1;
      {
	long char_count = ((dest - 1) - ((char *) start));
	long word_count = (STRING_LENGTH_TO_GC_LENGTH (char_count));
	start = (saved + 1 + word_count);
	if (start < end)
	  {
	    (saved [STRING_HEADER]) =
	      (MAKE_OBJECT (TC_MANIFEST_NM_VECTOR, word_count));
	    (saved [STRING_LENGTH_INDEX]) = ((SCHEME_OBJECT) char_count);
	  }
	return (start);
      }
    }
  }
}

SCHEME_OBJECT *
cons_primitive_table(start, end, length)
     SCHEME_OBJECT *start, *end;
     long *length;
{
  SCHEME_OBJECT *saved;
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

SCHEME_OBJECT *
cons_whole_primitive_table(start, end, length)
     SCHEME_OBJECT *start, *end;
     long *length;
{
  SCHEME_OBJECT *saved;
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
     fast SCHEME_OBJECT *table;
     fast long length;
     Boolean flush_p;
{
  fast SCHEME_OBJECT *translation_table;
  SCHEME_OBJECT result;
  long arity;

  if (flush_p)
  {
    Undefined_Primitives = SHARP_F;
    Undefined_Primitives_Arity = SHARP_F;
  }

  translation_table = load_renumber_table;
  while (--length >= 0)
  {
    arity = FIXNUM_TO_LONG (*table);
    table += 1;
    result =
      search_for_primitive(MAKE_POINTER_OBJECT (TC_CHARACTER_STRING, table),
			   ((char *) (&table[STRING_CHARS])),
			   true, true, arity);
    if (OBJECT_TYPE (result) != TC_PRIMITIVE)
    {
      signal_error_from_primitive (ERR_WRONG_ARITY_PRIMITIVES);
    }
    *translation_table++ = result;
    table += (1 + OBJECT_DATUM (*table));
  }
  return;
}
