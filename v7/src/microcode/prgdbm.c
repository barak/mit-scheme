/* -*-C-*-

$Id: prgdbm.c,v 1.5 2003/02/14 18:28:22 cph Exp $

Copyright (c) 1996-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

/* Interface to the gdbm database library */

#include "scheme.h"
#include "prims.h"
#include "os.h"
#include <gdbm.h>

/* Allocation Tables */

struct allocation_table
{
  PTR * items;
  int length;
};

static void
DEFUN (allocation_table_initialize, (table), struct allocation_table * table)
{
  (table -> length) = 0;
}

static unsigned int
DEFUN (allocate_table_index, (table, item),
       struct allocation_table * table AND
       PTR item)
{
  unsigned int length = (table -> length);
  unsigned int new_length;
  PTR * items = (table -> items);
  PTR * new_items;
  PTR * scan;
  PTR * end;
  if (length == 0)
    {
      new_length = 4;
      new_items = (OS_malloc ((sizeof (PTR)) * new_length));
    }
  else
    {
      scan = items;
      end = (scan + length);
      while (scan < end)
	if ((*scan++) == 0)
	  {
	    (*--scan) = item;
	    return (scan - items);
	  }
      new_length = (length * 2);
      new_items = (OS_realloc (items, ((sizeof (PTR)) * new_length)));
    }
  scan = (new_items + length);
  end = (new_items + new_length);
  (*scan++) = item;
  while (scan < end)
    (*scan++) = 0;
  (table -> items) = new_items;
  (table -> length) = new_length;
  return (length);
}

static PTR
DEFUN (allocation_item_arg, (arg, table),
       unsigned int arg AND
       struct allocation_table * table)
{
  unsigned int index = (arg_ulong_index_integer (arg, (table -> length)));
  PTR item = ((table -> items) [index]);
  if (item == 0)
    error_bad_range_arg (arg);
  return (item);
}

static struct allocation_table dbf_table;

#define DBF_VAL(dbf)							\
  (ulong_to_integer (allocate_table_index ((&dbf_table), ((PTR) (dbf)))))

#define DBF_ARG(arg)							\
  ((GDBM_FILE) (allocation_item_arg ((arg), (&dbf_table))))

#define GDBM_ERROR_VAL()						\
  (char_pointer_to_string ((unsigned char *) (gdbm_strerror (gdbm_errno))))

#define VOID_GDBM_CALL(expression)					\
  (((expression) == 0) ? SHARP_F : (GDBM_ERROR_VAL ()))

static datum
DEFUN (arg_datum, (arg), int arg)
{
  datum d;
  CHECK_ARG (arg, STRING_P);
  (d . dptr) = ((char *) (STRING_LOC ((ARG_REF (arg)), 0)));
  (d . dsize) = (STRING_LENGTH (ARG_REF (arg)));
  return (d);
}

static SCHEME_OBJECT
DEFUN (datum_to_object, (d), datum d)
{
  if (d . dptr)
    {
      SCHEME_OBJECT result = (allocate_string (d . dsize));
      CONST char * scan_d = (d . dptr);
      CONST char * end_d = (scan_d + (d . dsize));
      unsigned char * scan_result = (STRING_LOC (result, 0));
      while (scan_d < end_d)
	(*scan_result++) = ((unsigned char) (*scan_d++));
      free (d . dptr);
      return (result);
    }
  else
    return (SHARP_F);
}

static void
DEFUN (gdbm_fatal_error, (msg), char * msg)
{
  outf_error ("\ngdbm: %s\n", msg);
  outf_flush_error ();
  error_external_return ();
}

DEFINE_PRIMITIVE ("GDBM-OPEN", Prim_gdbm_open, 4, 4, 0)
{
  static int initialization_done = 0;
  PRIMITIVE_HEADER (4);
  if (!initialization_done)
    {
      allocation_table_initialize (&dbf_table);
      initialization_done = 1;
    }
  {
    GDBM_FILE dbf = (gdbm_open ((STRING_ARG (1)),
				(arg_integer (2)),
				(arg_integer (3)),
				(arg_integer (4)),
				gdbm_fatal_error));
    PRIMITIVE_RETURN ((dbf == 0) ? (GDBM_ERROR_VAL ()) : (DBF_VAL (dbf)));
  }
}

DEFINE_PRIMITIVE ("GDBM-CLOSE", Prim_gdbm_close, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  gdbm_close (DBF_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("GDBM-STORE", Prim_gdbm_store, 4, 4, 0)
{
  PRIMITIVE_HEADER (4);
  {
    int result = (gdbm_store ((DBF_ARG (1)),
			      (arg_datum (2)),
			      (arg_datum (3)),
			      (arg_integer (4))));
    PRIMITIVE_RETURN
      ((result < 0) ? (GDBM_ERROR_VAL ()) : (BOOLEAN_TO_OBJECT (!result)));
  }
}

DEFINE_PRIMITIVE ("GDBM-FETCH", Prim_gdbm_fetch, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (datum_to_object (gdbm_fetch ((DBF_ARG (1)), (arg_datum (2)))));
}

DEFINE_PRIMITIVE ("GDBM-EXISTS", Prim_gdbm_exists, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (gdbm_exists ((DBF_ARG (1)), (arg_datum (2)))));
}

DEFINE_PRIMITIVE ("GDBM-DELETE", Prim_gdbm_delete, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (((gdbm_delete ((DBF_ARG (1)), (arg_datum (2)))) == 0)
     ? SHARP_T
     : (gdbm_errno == GDBM_ITEM_NOT_FOUND)
     ? SHARP_F
     : (GDBM_ERROR_VAL ()));
}

DEFINE_PRIMITIVE ("GDBM-FIRSTKEY", Prim_gdbm_firstkey, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (datum_to_object (gdbm_firstkey (DBF_ARG (1))));
}

DEFINE_PRIMITIVE ("GDBM-NEXTKEY", Prim_gdbm_nextkey, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  PRIMITIVE_RETURN
    (datum_to_object (gdbm_nextkey ((DBF_ARG (1)), (arg_datum (2)))));
}

DEFINE_PRIMITIVE ("GDBM-REORGANIZE", Prim_gdbm_reorganize, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (VOID_GDBM_CALL (gdbm_reorganize (DBF_ARG (1))));
}

DEFINE_PRIMITIVE ("GDBM-SYNC", Prim_gdbm_sync, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  gdbm_sync (DBF_ARG (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("GDBM-VERSION", Prim_gdbm_version, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string ((unsigned char *) gdbm_version));
}

DEFINE_PRIMITIVE ("GDBM-SETOPT", Prim_gdbm_setopt, 3, 3, 0)
{
  PRIMITIVE_HEADER (3);
  {
    int value = (arg_integer (3));
    PRIMITIVE_RETURN
      (VOID_GDBM_CALL (gdbm_setopt ((DBF_ARG (1)),
				    (arg_integer (2)),
				    (&value),
				    (sizeof (int)))));
  }
}
