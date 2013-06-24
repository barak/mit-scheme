/* -*-C-*-

$Id: foreign.h,v 1.7 2007/01/05 21:19:25 cph Exp $

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

/* This file contains the primitive support for the foreign function */
/* interface. */

struct foreign_function {
  char * name;
  PTR    applicable_function;
};

typedef unsigned int HANDLE;

typedef struct foreign_function FOREIGN_FUNCTION;

struct foreign_object {
  PTR    ptr;
  HANDLE handle;
};

typedef struct foreign_object FOREIGN_OBJECT;

#ifdef __HPUX__
typedef shl_t LOAD_DESCRIPTOR;
typedef unsigned long LOAD_ADDRESS;
#endif

struct load_info {
  LOAD_DESCRIPTOR load_module_descriptor;
  LOAD_ADDRESS program_start;
  LOAD_ADDRESS program_end;
  LOAD_ADDRESS data_start;
  LOAD_ADDRESS data_end;
};

typedef struct load_info LOAD_INFO;  

#define index_to_handle(index) 	((HANDLE) index)
#define handle_to_index(handle)	((unsigned int) handle)
#define handle_to_integer(handle) (long_to_integer ((unsigned long) handle))
#define foreign_primtive_type_p(object) (FIXNUM_P (object))
#define foreign_composite_type_p(object) (PAIR_P (object))
#define which_composite_type(type) (PAIR_CAR (type))
#define composite_type_field_types(type) (PAIR_CDR (type))

/* the following define should be in some other .h file */
#define TEMP_FILE_NAME_MAX_LEN 	L_tmpnam  /* in stdio.h */

/* The following should be ifdef'ed up the wazoo for different machines */
#define DYNAMIC_COMPILE_SWITCHES  "+z"

/* These need to match the appropriate enumerations in foreign.scm */
#define FOREIGN_FIRST_TYPE	0
#define FOREIGN_INT 		0
#define FOREIGN_SHORT 		1
#define FOREIGN_LONG 		2
#define FOREIGN_CHAR 		3
#define FOREIGN_FLOAT 		4
#define FOREIGN_DOUBLE 		5
#define FOREIGN_STRING 		6
#define FOREIGN_PTR 		7
#define FOREIGN_STRUCT 		8
#define FOREIGN_UNION 		9
#define FOREIGN_LAST_TYPE	9

/* This is a bunch of stuff to figure out the alignment of fields in
   structures */

/* This defines a union which is guaranteed to have the most restrictive
   possible alignment constraint */
union greatest_alignment {
  int my_int;
  short my_short;
  long my_long;
  char my_char;
  float my_float;
  double my_double;
  char * my_string;
  void * my_ptr;
}

/* We hope that char's are the smallest sized objects */
typedef char smallest_size;

struct int_field {
  union greatest_alignment foo;
  smallest_size bar;
  int int_item;
}
#define INT_ALIGNMENT_MASK \
        ((long) ((sizeof (int_field) - sizeof (int)) - 1))
 
struct short_field {
  union greatest_alignment foo;
  smallest_size bar;
  short short_item;
}
#define SHORT_ALIGNMENT_MASK \
        ((long) ((sizeof (short_field) - sizeof (short)) - 1))
 
struct long_field {
  union greatest_alignment foo;
  smallest_size bar;
  long long_item;
}
#define LONG_ALIGNMENT_MASK \
        ((long) ((sizeof (long_field) - sizeof (long)) - 1))
 
struct char_field {
  union greatest_alignment foo;
  smallest_size bar;
  char char_item;
}
#define CHAR_ALIGNMENT_MASK \
        ((long) ((sizeof (char_field) - sizeof (char)) - 1))
 
struct float_field {
  union greatest_alignment foo;
  smallest_size bar;
  float float_item;
}
#define FLOAT_ALIGNMENT_MASK \
        ((long) ((sizeof (float_field) - sizeof (float)) - 1))
 
struct double_field {
  union greatest_alignment foo;
  smallest_size bar;
  double double_item;
}
#define DOUBLE_ALIGNMENT_MASK \
        ((long) ((sizeof (double_field) - sizeof (double)) - 1))
 
struct string_field {
  union greatest_alignment foo;
  smallest_size bar;
  char * string_item;
}
#define STRING_ALIGNMENT_MASK \
        ((long) ((sizeof (string_field) - sizeof (char *)) - 1))
 
struct ptr_field {
  union greatest_alignment foo;
  smallest_size bar;
  PTR ptr_item;
}
#define PTR_ALIGNMENT_MASK \
        ((long) ((sizeof (ptr_field) - sizeof (PTR)) - 1))

struct struct_field {
  union greatest_alignment foo;
  smallest_size bar;
  struct greatest_alignment2 struct_item;
}

#define ALIGN_FOREIGN_POINTER(ptr,type) \
#  if (type == FOREIGN_INT) \
     (((int *) (ptr & INT_ALIGNMENT_MASK)) + 1) \
#  else \
#  if (type == FOREIGN_SHORT) \
     (((short *) (ptr & SHORT_ALIGNMENT_MASK)) + 1) \
#  else \
#  if (type == FOREIGN_LONG) \
     (((long *) (ptr & LONG_ALIGNMENT_MASK)) + 1) \
#  else \
#  if (type == FOREIGN_CHAR) \
     (((char *) (ptr & CHAR_ALIGNMENT_MASK)) + 1) \
#  else \
#  if (type == FOREIGN_FLOAT) \
     (((float *) (ptr & FLOAT_ALIGNMENT_MASK)) + 1) \
#  else \
#  if (type == FOREIGN_DOUBLE) \
     (((double *) (ptr & DOUBLE_ALIGNMENT_MASK)) + 1) \
#  else \
#  if (type == FOREIGN_STRING) \
     (((unsigned char *) (ptr & STRING_ALIGNMENT_MASK)) + 1) \
#  else \
#  if (type == FOREIGN_PTR) \
     (((PTR) (ptr & PTR_ALIGNMENT_MASK)) + 1) \
#  endif \
#  endif \
#  endif \
#  endif \
#  endif \
#  endif \
#  endif \
#  endif


/* End of alignment junk */
