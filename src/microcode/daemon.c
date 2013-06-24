/* -*-C-*-

$Id: daemon.c,v 9.33 2003/02/14 18:28:18 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

/* This file contains code for the Garbage Collection daemons.
   There are currently two daemons, one for closing files which
   have disappeared due to GC, the other for supporting object
   hash tables where entries disappear when the corresponding
   object is released due to GC.

   Both of these daemons should be written in Scheme, but since the
   interpreter conses while executing Scheme programs, they are
   unsafe.  The Scheme versions actually exist, but are commented out
   of the appropriate runtime system sources. */

#include "scheme.h"
#include "prims.h"
#include "osio.h"

/* (CLOSE-LOST-OPEN-FILES file-list)
   file-list is an assq-like list where the associations are weak
   pairs rather than normal pairs.  This primitive destructively
   removes those weak pairs whose cars are #F, and closes the
   corresponding file descriptor contained in the cdrs. See io.scm in
   the runtime system for a longer description. */

DEFINE_PRIMITIVE ("CLOSE-LOST-OPEN-FILES", Prim_close_lost_open_files, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  {
    SCHEME_OBJECT file_list = (ARG_REF (1));
    SCHEME_OBJECT * smash = (PAIR_CDR_LOC (file_list));
    SCHEME_OBJECT cell = (*smash);
    while (cell != EMPTY_LIST)
      {
	SCHEME_OBJECT weak_cell = (FAST_PAIR_CAR (cell));
	if ((FAST_PAIR_CAR (weak_cell)) == SHARP_F)
	  {
	    OS_channel_close_noerror
	      (UNSIGNED_FIXNUM_TO_LONG (FAST_PAIR_CDR (weak_cell)));
	    cell = (FAST_PAIR_CDR (cell));
	    (*smash) = cell;
	  }
	else
	  {
	    smash = (PAIR_CDR_LOC (cell));
	    cell = (*smash);
	  }
      }
  }
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* Utilities for the rehash daemon below */

/* This runs with GC locked, being part of a GC daemon.
   It is also the case that the storage needed by this daemon is
   available, since it was all reclaimed by the immediately preceeding
   garbage collection, and at most that much is allocated now.
   Therefore, there is no gc check here. */

static void
DEFUN (rehash_pair, (pair, hash_table, table_size),
       SCHEME_OBJECT pair AND SCHEME_OBJECT hash_table
       AND long table_size)
{
  long object_datum, hash_address;
  SCHEME_OBJECT * new_pair;

  object_datum = (OBJECT_DATUM (FAST_PAIR_CAR (pair)));
  hash_address = (2 + (object_datum % table_size));
  new_pair = Free;
  *Free++ = (OBJECT_NEW_TYPE (TC_LIST, pair));
  *Free++ = (FAST_MEMORY_REF (hash_table, hash_address));
  FAST_MEMORY_SET (hash_table,
		   hash_address,
		   (MAKE_POINTER_OBJECT (TC_LIST, new_pair)));
  return;
}

static void
DEFUN (rehash_bucket, (bucket, hash_table, table_size),
       SCHEME_OBJECT * bucket AND SCHEME_OBJECT hash_table
       AND long table_size)
{
  fast SCHEME_OBJECT weak_pair;

  while (*bucket != EMPTY_LIST)
  {
    weak_pair = (FAST_PAIR_CAR (*bucket));
    if ((FAST_PAIR_CAR (weak_pair)) != SHARP_F)
    {
      rehash_pair (weak_pair, hash_table, table_size);
    }
    bucket = (PAIR_CDR_LOC (*bucket));
  }
  return;
}

static void
DEFUN (splice_and_rehash_bucket, (bucket, hash_table, table_size),
       SCHEME_OBJECT * bucket AND SCHEME_OBJECT hash_table
       AND long table_size)
{
  fast SCHEME_OBJECT weak_pair;

  while ((*bucket) != EMPTY_LIST)
  {
    weak_pair = (FAST_PAIR_CAR (*bucket));
    if ((FAST_PAIR_CAR (weak_pair)) != SHARP_F)
    {
      rehash_pair (weak_pair, hash_table, table_size);
      bucket = (PAIR_CDR_LOC (*bucket));
    }
    else
      *bucket = (FAST_PAIR_CDR (*bucket));
  }
  return;
}

/* (REHASH unhash-table hash-table)
   Cleans up and recomputes hash-table from the valid information in
   unhash-table after a garbage collection.
   See hash.scm in the runtime system for a description. */

DEFINE_PRIMITIVE ("REHASH", Prim_rehash, 2, 2, 0)
{
  long table_size, counter;
  SCHEME_OBJECT *bucket;
  PRIMITIVE_HEADER (2);
  table_size = (VECTOR_LENGTH (ARG_REF (1)));

  /* First cleanup the hash table */
  counter = table_size;
  bucket = (MEMORY_LOC ((ARG_REF (2)), 2));
  while ((counter--) > 0)
    (*bucket++) = EMPTY_LIST;

  /* Now rehash all the entries from the unhash table and maybe splice
     the buckets. */
  counter = table_size;
  bucket = (MEMORY_LOC ((ARG_REF (1)), 1));
  while ((counter--) > 0)
    {
      if ((FAST_PAIR_CAR (*bucket)) == SHARP_T)
	splice_and_rehash_bucket
	  ((PAIR_CDR_LOC (*bucket)), (ARG_REF (2)), table_size);
      else
	rehash_bucket ((PAIR_CDR_LOC (*bucket)), (ARG_REF (2)), table_size);
      bucket += 1;
    }
  PRIMITIVE_RETURN (UNSPECIFIC);
}
