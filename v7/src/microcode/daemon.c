/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/daemon.c,v 9.28 1990/06/20 17:39:39 cph Rel $

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
	    OS_channel_close
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

void
rehash_pair (pair, hash_table, table_size)
     SCHEME_OBJECT pair, hash_table;
     long table_size;
{ long object_datum, hash_address;
  SCHEME_OBJECT *new_pair;

  object_datum = OBJECT_DATUM (FAST_PAIR_CAR (pair));
  hash_address = 2+(object_datum % table_size);
  new_pair = Free;
  *Free++ = (OBJECT_NEW_TYPE (TC_LIST, pair));
  *Free++ = FAST_MEMORY_REF (hash_table, hash_address);
  FAST_MEMORY_SET (hash_table,
		   hash_address,
		   MAKE_POINTER_OBJECT (TC_LIST, new_pair));
  return;
}

void
rehash_bucket (bucket, hash_table, table_size)
     SCHEME_OBJECT *bucket, hash_table;
     long table_size;
{ fast SCHEME_OBJECT weak_pair;
  while (*bucket != EMPTY_LIST)
  { weak_pair = FAST_PAIR_CAR (*bucket);
    if (FAST_PAIR_CAR (weak_pair) != SHARP_F)
    { rehash_pair(weak_pair, hash_table, table_size);
    }
    bucket = PAIR_CDR_LOC (*bucket);
  }
  return;
}

void
splice_and_rehash_bucket(bucket, hash_table, table_size)
     SCHEME_OBJECT *bucket, hash_table;
     long table_size;
{ fast SCHEME_OBJECT weak_pair;
  while (*bucket != EMPTY_LIST)
  { weak_pair = FAST_PAIR_CAR (*bucket);
    if (FAST_PAIR_CAR (weak_pair) != SHARP_F)
    { rehash_pair(weak_pair, hash_table, table_size);
      bucket = PAIR_CDR_LOC (*bucket);
    }
    else
    { *bucket = FAST_PAIR_CDR (*bucket);
    }
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
  table_size = VECTOR_LENGTH (ARG_REF (1));

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
