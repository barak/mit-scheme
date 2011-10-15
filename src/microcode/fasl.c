/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

/* I/O for fasdump and fasload */

#include "config.h"
#include "fasl.h"

static void encode_fasl_header (SCHEME_OBJECT *, fasl_header_t *);
static bool decode_fasl_header (SCHEME_OBJECT *, fasl_header_t *);

bool
open_fasl_output_file (const char * filename, fasl_file_handle_t * handle_r)
{
  FILE * s = (fopen (filename, "wb"));
  if (s == 0)
    return (false);
  (*handle_r) = s;
  return (true);
}

bool
close_fasl_output_file (fasl_file_handle_t handle)
{
  return ((fclose (handle)) == 0);
}

bool
write_fasl_header (fasl_header_t * h, fasl_file_handle_t handle)
{
  SCHEME_OBJECT raw [FASL_HEADER_LENGTH];

  encode_fasl_header (raw, h);
  return (write_to_fasl_file (raw, FASL_HEADER_LENGTH, handle));
}

bool
write_to_fasl_file (const void * start, size_t n_words,
		    fasl_file_handle_t handle)
{
  return ((fwrite (start, SIZEOF_SCHEME_OBJECT, n_words, handle)) == n_words);
}

bool
open_fasl_input_file (const char * filename, fasl_file_handle_t * handle_r)
{
  FILE * s = (fopen (filename, "rb"));
  if (s == 0)
    return (false);
  (*handle_r) = s;
  return (true);
}

bool
close_fasl_input_file (fasl_file_handle_t handle)
{
  return ((fclose (handle)) == 0);
}

bool
read_fasl_header (fasl_header_t * h, fasl_file_handle_t handle)
{
  SCHEME_OBJECT raw [FASL_HEADER_LENGTH];
  return
    ((read_from_fasl_file (raw, FASL_HEADER_LENGTH, handle))
     && (decode_fasl_header (raw, h)));
}

bool
read_from_fasl_file (void * start, size_t n_words, fasl_file_handle_t handle)
{
  return ((fread (start, SIZEOF_SCHEME_OBJECT, n_words, handle)) == n_words);
}

fasl_read_status_t
check_fasl_version (fasl_header_t * fh)
{
  return
    ((((FASLHDR_VERSION (fh)) >= OLDEST_INPUT_FASL_VERSION)
      && ((FASLHDR_VERSION (fh)) <= NEWEST_INPUT_FASL_VERSION))
     ? (((FASLHDR_ARCH (fh)) == CURRENT_FASL_ARCH)
	? FASL_FILE_FINE
	: FASL_FILE_BAD_MACHINE)
     : FASL_FILE_BAD_VERSION);
}

fasl_read_status_t
check_fasl_cc_version (fasl_header_t * fh,
		       unsigned long version, unsigned long type)
{
  return
    ((((FASLHDR_CC_VERSION (fh)) == 0)
      && ((FASLHDR_CC_ARCH (fh)) == COMPILER_NONE_TYPE))
     ? FASL_FILE_FINE
     : ((FASLHDR_CC_VERSION (fh)) == version)
     ? (((FASLHDR_CC_ARCH (fh)) == type)
	? FASL_FILE_FINE
	: FASL_FILE_BAD_PROCESSOR)
     : FASL_FILE_BAD_INTERFACE);
}

static void
encode_fasl_header (SCHEME_OBJECT * raw, fasl_header_t * h)
{
  {
    SCHEME_OBJECT * p = raw;
    SCHEME_OBJECT * e = (raw + FASL_HEADER_LENGTH);
    while (p < e)
      (*p++) = SHARP_F;
  }
#ifdef DEBUG
#ifdef HEAP_IN_LOW_MEMORY
  fprintf (stderr, "\nmemory_base = %#lx\n",
	   ((unsigned long) (FASLHDR_MEMORY_BASE (h))));
#endif
  fprintf (stderr, "\nheap start %#lx\n",
	   ((unsigned long) (FASLHDR_HEAP_START (h))));
  fprintf (stderr, "\nroot object %#lx\n",
	   ((unsigned long) (FASLHDR_ROOT_POINTER (h))));
#endif

  (raw[FASL_OFFSET_MARKER]) = FASL_FILE_MARKER;

  (raw[FASL_OFFSET_VERSION])
    = (MAKE_FASL_VERSION ((FASLHDR_VERSION (h)), (FASLHDR_ARCH (h))));
  (raw[FASL_OFFSET_CI_VERSION])
    = (MAKE_CI_VERSION ((FASLHDR_BAND_P (h)),
			(FASLHDR_CC_VERSION (h)),
			(FASLHDR_CC_ARCH (h))));

  (raw[FASL_OFFSET_MEM_BASE])
    = ((SCHEME_OBJECT) (FASLHDR_MEMORY_BASE (h)));

  (raw[FASL_OFFSET_DUMPED_OBJ])
    = (MAKE_BROKEN_HEART (FASLHDR_ROOT_POINTER (h)));

  (raw[FASL_OFFSET_HEAP_BASE])
    = (MAKE_BROKEN_HEART (FASLHDR_HEAP_START (h)));
  (raw[FASL_OFFSET_HEAP_SIZE])
    = (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_HEAP_SIZE (h))));

  if ((FASLHDR_VERSION (h)) >= FASL_VERSION_STACK_END)
    (raw[FASL_OFFSET_HEAP_RSVD])
      = (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_HEAP_RESERVED (h))));

  (raw[FASL_OFFSET_CONST_BASE])
    = (MAKE_BROKEN_HEART (FASLHDR_CONSTANT_START (h)));
  (raw[FASL_OFFSET_CONST_SIZE])
    = (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_CONSTANT_SIZE (h))));

  if ((FASLHDR_VERSION (h)) >= FASL_VERSION_STACK_END)
    {
      (raw[FASL_OFFSET_STACK_START])
	= (MAKE_BROKEN_HEART (FASLHDR_STACK_START (h)));
      (raw[FASL_OFFSET_STACK_SIZE])
	= (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_STACK_SIZE (h))));
    }
  else
    (raw[FASL_OFFSET_STACK_START])
      = (MAKE_BROKEN_HEART (FASLHDR_STACK_END (h)));

  (raw[FASL_OFFSET_PRIM_LENGTH])
    = (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_N_PRIMITIVES (h))));
  (raw[FASL_OFFSET_PRIM_SIZE])
    = (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_PRIMITIVE_TABLE_SIZE (h))));

  (raw[FASL_OFFSET_C_LENGTH])
    = (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_N_C_CODE_BLOCKS (h))));
  (raw[FASL_OFFSET_C_SIZE])
    = (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_C_CODE_TABLE_SIZE (h))));

  (raw[FASL_OFFSET_UT_BASE]) = (FASLHDR_UTILITIES_VECTOR (h));

  if ((FASLHDR_VERSION (h)) >= FASL_VERSION_EPHEMERONS)
    (raw[FASL_OFFSET_EPHEMERONS])
      = (MAKE_OBJECT (TC_BROKEN_HEART, (FASLHDR_EPHEMERON_COUNT (h))));
}

static bool
decode_fasl_header (SCHEME_OBJECT * raw, fasl_header_t * h)
{
  if ((raw[FASL_OFFSET_MARKER]) != FASL_FILE_MARKER)
    return (false);
  {
    SCHEME_OBJECT object = (raw[FASL_OFFSET_VERSION]);
    (FASLHDR_VERSION (h)) = (FASL_VERSION (object));
    (FASLHDR_ARCH (h)) = (FASL_ARCH (object));
  }
  {
    SCHEME_OBJECT object = (raw[FASL_OFFSET_CI_VERSION]);
    (FASLHDR_CC_VERSION (h)) = (CI_VERSION (object));
    (FASLHDR_CC_ARCH (h)) = (CI_PROCESSOR (object));
    (FASLHDR_BAND_P (h)) = (CI_BAND_P (object));
  }
  {
    SCHEME_OBJECT * fasl_memory_base
      = ((SCHEME_OBJECT *) (raw[FASL_OFFSET_MEM_BASE]));
    (FASLHDR_MEMORY_BASE (h)) = fasl_memory_base;

    (FASLHDR_ROOT_POINTER (h))
      = (fasl_object_address ((raw[FASL_OFFSET_DUMPED_OBJ]), h));

    (FASLHDR_HEAP_START (h))
      = (fasl_object_address ((raw[FASL_OFFSET_HEAP_BASE]), h));
    (FASLHDR_HEAP_END (h))
      = ((FASLHDR_HEAP_START (h))
	 + (OBJECT_DATUM (raw[FASL_OFFSET_HEAP_SIZE])));
    (FASLHDR_HEAP_RESERVED (h))
      = (((FASLHDR_VERSION (h)) >= FASL_VERSION_STACK_END)
	 ? (OBJECT_DATUM (raw[FASL_OFFSET_HEAP_RSVD]))
	 : 0);

    (FASLHDR_CONSTANT_START (h))
      = (fasl_object_address ((raw[FASL_OFFSET_CONST_BASE]), h));
    (FASLHDR_CONSTANT_END (h))
      = ((FASLHDR_CONSTANT_START (h))
	 + (OBJECT_DATUM (raw[FASL_OFFSET_CONST_SIZE])));

    if ((FASLHDR_VERSION (h)) >= FASL_VERSION_STACK_END)
      {
	(FASLHDR_STACK_START (h))
	  = (fasl_object_address ((raw[FASL_OFFSET_STACK_START]), h));
	(FASLHDR_STACK_END (h))
	  = ((FASLHDR_STACK_START (h))
	     + (OBJECT_DATUM (raw[FASL_OFFSET_STACK_SIZE])));
      }
    else
      /* In older versions, the "stack start" field held "stack
	 bottom" instead.  Since the stack grows downwards, this was
	 the maximum address.  */
      {
	(FASLHDR_STACK_END (h))
	  = (fasl_object_address ((raw[FASL_OFFSET_STACK_START]), h));
	/* If !HEAP_IN_LOW_MEMORY then fasl_memory_base is the right
	   value.  Otherwise, fasl_memory_base is zero and that is at
	   least guaranteed to encompass the whole stack.  */
	(FASLHDR_STACK_START (h)) = fasl_memory_base;
      }

    (FASLHDR_N_PRIMITIVES (h))
      = (OBJECT_DATUM (raw[FASL_OFFSET_PRIM_LENGTH]));
    (FASLHDR_PRIMITIVE_TABLE_SIZE (h))
      = (OBJECT_DATUM (raw[FASL_OFFSET_PRIM_SIZE]));

    (FASLHDR_N_C_CODE_BLOCKS (h))
      = (OBJECT_DATUM (raw[FASL_OFFSET_C_LENGTH]));
    (FASLHDR_C_CODE_TABLE_SIZE (h))
      = (OBJECT_DATUM (raw[FASL_OFFSET_C_SIZE]));

    {
      SCHEME_OBJECT ruv = (raw[FASL_OFFSET_UT_BASE]);
      if (ruv == SHARP_F)
	{
	  (FASLHDR_UTILITIES_VECTOR (h)) = SHARP_F;
	  (FASLHDR_UTILITIES_START (h)) = 0;
	}
      else
	{
	  SCHEME_OBJECT fuv
	    = (OBJECT_NEW_ADDRESS (ruv, (fasl_object_address (ruv, h))));
	  (FASLHDR_UTILITIES_VECTOR (h)) = fuv;
	  (FASLHDR_UTILITIES_START (h)) = (OBJECT_ADDRESS (fuv));
	}
    }
    (__FASLHDR_UTILITIES_END (h)) = 0;
  }
  if ((FASLHDR_VERSION (h)) >= FASL_VERSION_EPHEMERONS)
    (FASLHDR_EPHEMERON_COUNT (h))
      = (OBJECT_DATUM (raw[FASL_OFFSET_EPHEMERONS]));
  return (true);
}

SCHEME_OBJECT *
fasl_object_address (SCHEME_OBJECT o, fasl_header_t * h)
{
  if ((FASLHDR_MEMORY_BASE (h)) != 0)
    return ((FASLHDR_MEMORY_BASE (h)) + (OBJECT_DATUM (o)));
  if ((FASLHDR_ARCH (h)) == CURRENT_FASL_ARCH)
    return (OBJECT_ADDRESS (o));
  abort ();
  return (0);
}

insn_t *
fasl_cc_address (SCHEME_OBJECT o, fasl_header_t * h)
{
  if ((FASLHDR_MEMORY_BASE (h)) != 0)
    return (((insn_t *) (FASLHDR_MEMORY_BASE (h))) + (OBJECT_DATUM (o)));
  if ((FASLHDR_ARCH (h)) == CURRENT_FASL_ARCH)
    return (CC_ENTRY_ADDRESS (o));
  abort ();
  return (0);
}

SCHEME_OBJECT
fasl_raw_address_to_object (unsigned int type,
			    SCHEME_OBJECT * address,
			    fasl_header_t * h)
{
  if ((FASLHDR_MEMORY_BASE (h)) != 0)
    return (MAKE_OBJECT (type, (address - (FASLHDR_MEMORY_BASE (h)))));
  if ((FASLHDR_ARCH (h)) == CURRENT_FASL_ARCH)
    return (MAKE_POINTER_OBJECT (type, address));
  abort ();
  return (UNSPECIFIC);
}

SCHEME_OBJECT
fasl_raw_address_to_cc_entry (insn_t * address, fasl_header_t * h)
{
  if ((FASLHDR_MEMORY_BASE (h)) != 0)
    return (MAKE_OBJECT (TC_COMPILED_ENTRY,
			 (address - ((insn_t *) (FASLHDR_MEMORY_BASE (h))))));
  if ((FASLHDR_ARCH (h)) == CURRENT_FASL_ARCH)
    return (MAKE_CC_ENTRY (address));
  abort ();
  return (UNSPECIFIC);
}

SCHEME_OBJECT *
faslhdr_utilities_end (fasl_header_t * h)
{
  if (((__FASLHDR_UTILITIES_END (h)) == 0)
      && (VECTOR_P (FASLHDR_UTILITIES_VECTOR (h))))
    (__FASLHDR_UTILITIES_END (h))
      = (VECTOR_LOC ((FASLHDR_UTILITIES_VECTOR (h)),
		     (VECTOR_LENGTH (FASLHDR_UTILITIES_VECTOR (h)))));
  return (__FASLHDR_UTILITIES_END (h));
}
