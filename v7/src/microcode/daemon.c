/* -*-C-*-

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/daemon.c,v 9.26 1988/08/15 20:44:42 cph Rel $

   This file contains code for the Garbage Collection daemons.
   There are currently two daemons, one for closing files which
   have disappeared due to GC, the other for supporting object
   hash tables where entries disappear when the corresponding
   object is released due to GC.

   Both of these daemons should be written in Scheme, but since the
   interpreter conses while executing Scheme programs, they are
   unsafe.  The Scheme versions actually exist, but are commented out
   of the appropriate runtime system sources.
*/

#include "scheme.h"
#include "prims.h"

/* (CLOSE-LOST-OPEN-FILES file-list) 
   file-list is an assq-like list where the associations are weak
   pairs rather than normal pairs.  This primitive destructively
   removes those weak pairs whose cars are #F, and closes the
   corresponding file descriptor contained in the cdrs. See io.scm in
   the runtime system for a longer description.
*/

DEFINE_PRIMITIVE ("CLOSE-LOST-OPEN-FILES", Prim_close_lost_open_files, 1, 1, 0)
{
  extern Boolean OS_file_close();
  fast Pointer *Smash, Cell, Weak_Cell, Value;
  long channel_number;
  Primitive_1_Arg();

  Value = SHARP_T;

  for (Smash = Nth_Vector_Loc(Arg1, CONS_CDR), Cell = *Smash;
       Cell != NIL;
       Cell = *Smash)
  {
    Weak_Cell = Fast_Vector_Ref(Cell, CONS_CAR);
    if (Fast_Vector_Ref(Weak_Cell, CONS_CAR) == NIL)
    {
      channel_number = Get_Integer(Fast_Vector_Ref(Weak_Cell, CONS_CDR));
      if (!OS_file_close (Channels[channel_number]))
	Value = NIL;
      Channels[channel_number] = NULL;
      *Smash = Fast_Vector_Ref(Cell, CONS_CDR);
    }
    else
      Smash = Nth_Vector_Loc(Cell, CONS_CDR);
  }
  return Value;
}

/* Utilities for the rehash daemon below */

/* This runs with GC locked, being part of a GC daemon.
   It is also the case that the storage needed by this daemon is
   available, since it was all reclaimed by the immediately preceeding
   garbage collection, and at most that much is allocated now.
   Therefore, there is no gc check here.
*/

void
rehash_pair(pair, hash_table, table_size)
Pointer pair, hash_table;
long table_size;
{ long object_datum, hash_address;
  Pointer *new_pair;

  object_datum = Datum(Fast_Vector_Ref(pair, CONS_CAR));
  hash_address = 2+(object_datum % table_size);
  new_pair = Free;
  *Free++ = Make_New_Pointer(TC_LIST, pair);
  *Free++ = Fast_Vector_Ref(hash_table, hash_address);
  Fast_Vector_Set(hash_table,
		  hash_address,
		  Make_Pointer(TC_LIST, new_pair));
  return;
}

void
rehash_bucket(bucket, hash_table, table_size)
Pointer *bucket, hash_table;
long table_size;
{ fast Pointer weak_pair;
  while (*bucket != NIL)
  { weak_pair = Fast_Vector_Ref(*bucket, CONS_CAR);
    if (Fast_Vector_Ref(weak_pair, CONS_CAR) != NIL)
    { rehash_pair(weak_pair, hash_table, table_size);
    }
    bucket = Nth_Vector_Loc(*bucket, CONS_CDR);
  }
  return;
}

void
splice_and_rehash_bucket(bucket, hash_table, table_size)
Pointer *bucket, hash_table;
long table_size;
{ fast Pointer weak_pair;
  while (*bucket != NIL)
  { weak_pair = Fast_Vector_Ref(*bucket, CONS_CAR);
    if (Fast_Vector_Ref(weak_pair, CONS_CAR) != NIL)
    { rehash_pair(weak_pair, hash_table, table_size);
      bucket = Nth_Vector_Loc(*bucket, CONS_CDR);
    }
    else
    { *bucket = Fast_Vector_Ref(*bucket, CONS_CDR);
    }
  }
  return;
}

/* (REHASH unhash-table hash-table)
   Cleans up and recomputes hash-table from the valid information in
   unhash-table after a garbage collection.
   See hash.scm in the runtime system for a description.
*/

DEFINE_PRIMITIVE ("REHASH", Prim_rehash, 2, 2, 0)
{
  long table_size, counter;
  Pointer *bucket;
  Primitive_2_Args();

  table_size = Vector_Length(Arg1);

  /* First cleanup the hash table */
  for (counter = table_size, bucket = Nth_Vector_Loc(Arg2, 2);
       --counter >= 0;)
    *bucket++ = NIL;

  /* Now rehash all the entries from the unhash table and maybe splice
     the buckets. */

  for (counter = table_size, bucket = Nth_Vector_Loc(Arg1, 1);
       --counter >= 0;
       bucket += 1)
  { if (Fast_Vector_Ref(*bucket, CONS_CAR) == SHARP_T)
      splice_and_rehash_bucket(Nth_Vector_Loc(*bucket, CONS_CDR), Arg2, table_size);
    else
      rehash_bucket(Nth_Vector_Loc(*bucket, CONS_CDR), Arg2, table_size);
  }

  return SHARP_T;
}
