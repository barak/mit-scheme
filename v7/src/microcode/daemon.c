/* -*-C-*-

Copyright (c) 1986 Massachusetts Institute of Technology

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

/* File: daemon.c

   This file contains code for the Garbage Collection daemons.
   There are currently two daemons, one for closing files which
   have disappeared due to GC, the other for supporting object
   hash tables where entries disappear when the corresponding
   object is released due to GC.

*/

#include "scheme.h"
#include "primitive.h"
#include "gccode.h"

/* Hash Tables

   The hash table support here allows the Scheme runtime system to
   support "populations."  A population is conceptually a set of
   items, but with the special property that an item remains in the
   population only as long as the object would remain in the system
   were it not in the set.  That is, an item is removed from all
   populations it belongs to when a garbage collection removes the
   item from the system.

   The actual support provided is a pair of hash tables.  An object
   can be hashed to yield the current value of a constantly
   incrementing counter.  The hash table is constructed by hashing on
   the address of the object, and both the item and the unique number
   assigned to it are stored in the table.  The unhash table is
   constructed by hashing on the unique number and again storing both
   the item and its unique number.  Both the hash and unhash tables
   appear to the user to be vectors, but they have a NON_MARKED header
   so that the ordinary GC will not update pointers located within
   them.

   At every GC flip (i.e. after all objects have been moved from old
   space to new space, but before the Scheme code runs again), the
   Rehash Daemon is called.  It goes through the hash table (all of
   which points into old space) and reconstructs it.  Whenever it
   finds a non-pointer object or an object which points at a BROKEN
   HEART (i.e. one which the GC copied into new space) it rehashes the
   new address and adds it to the new table.

   Thus, the hash tables provide a mapping from objects to unique
   numbers, with the additional property that the table does not
   retain objects that the garbage collector would otherwise release
   from the system.

*/

#define Hash_It(P)			\
	(((Datum(P)>>16)&0xFF)+	\
         ((Datum(P)>>8)&0xFF)+	\
         (Datum(P) & 0xFF))

Pointer The_Hash_Table, The_Unhash_Table;
long HASH_TABLE_SIZE;

/* (INITIALIZE-OBJECT-HASH FIXNUM)
      [Primitive number 0x8A]
      Resets the unique ID generator used in the 2-dimensional hash
      tables which implement properties and populations.  The value of
      FIXNUM will be used for the next object put into the hash
      tables.
*/
Built_In_Primitive(Prim_Initialize_Object_Hash, 1, "INITIALIZE-OBJECT-HASH")
{ fast long i;
  long Length;
  Primitive_1_Arg();
  Arg_1_Type(TC_FIXNUM);
  HASH_TABLE_SIZE = Get_Integer(Arg1);
  Length = 8 + (2 * HASH_TABLE_SIZE);
  if (!Test_Pure_Space_Top(Free_Constant + Length))
  { Update_FObj_Slot(Hash_Table, NIL);
    Update_FObj_Slot(Unhash_Table, NIL);
    return NIL;
  }

/* Make a Constant/Pure block to hold the two vectors */

/* Constant part header */
  *Free_Constant++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, Length-3);
  *Free_Constant++ = Make_Non_Pointer(PURE_PART, Length-1);

/* Constant part contains hash and unhash tables */
  Update_FObj_Slot(Hash_Table, Make_Pointer(TC_VECTOR, Free_Constant));
  *Free_Constant++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, HASH_TABLE_SIZE);
  for (i=0; i < HASH_TABLE_SIZE; i++) *Free_Constant++ = NIL;
  Update_FObj_Slot(Unhash_Table, Make_Pointer(TC_VECTOR, Free_Constant));
  *Free_Constant++ = Make_Non_Pointer(TC_MANIFEST_VECTOR, HASH_TABLE_SIZE);
  for (i=0; i < HASH_TABLE_SIZE; i++) *Free_Constant++ = NIL;

/* Pure part header */
  *Free_Constant++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *Free_Constant++ = Make_Non_Pointer(CONSTANT_PART, Length-3);

/* Block trailer */
  *Free_Constant++ = Make_Non_Pointer(TC_MANIFEST_SPECIAL_NM_VECTOR, 1);
  *Free_Constant++ = Make_Non_Pointer(END_OF_BLOCK, Length-1);
  Update_FObj_Slot(Hash_Number, FIXNUM_0);
  Set_Pure_Top();
  return NIL;
}

Pointer Hash_One_Object(Object, New_Unique_ID, Update_UID_Count)
Pointer Object, New_Unique_ID;
Boolean Update_UID_Count;
{ Pointer Bucket;
  long UID_Hash, Obj_Hash;

  Obj_Hash = Hash_It(Object) % HASH_TABLE_SIZE + 1;
  Bucket = Vector_Ref(The_Hash_Table, Obj_Hash);
  while (Type_Code(Bucket) == TC_LIST)
  { Pointer This_Entry;
    This_Entry = Vector_Ref(Bucket, CONS_CAR);
    if (Vector_Ref(This_Entry, CONS_CAR) == Object)
      return Vector_Ref(This_Entry, CONS_CDR);
    Bucket = Vector_Ref(Bucket, CONS_CDR);
  }
  Primitive_GC_If_Needed(6);
  UID_Hash = Hash_It(New_Unique_ID) % HASH_TABLE_SIZE + 1;

  Free[CONS_CAR] = Make_Pointer(TC_LIST, Free+2);
  Free[CONS_CDR] = Vector_Ref(The_Hash_Table, Obj_Hash);
  Vector_Set(The_Hash_Table, Obj_Hash, Make_Pointer(TC_LIST, Free));
  Free += 2;

  Free[CONS_CAR] = Object;
  Free[CONS_CDR] = New_Unique_ID;
  Free += 2;

  Free[CONS_CAR] = Make_Pointer(TC_LIST, Free-2);
  Free[CONS_CDR] = Vector_Ref(The_Unhash_Table, UID_Hash);
  Vector_Set(The_Unhash_Table, UID_Hash, Make_Pointer(TC_LIST, Free));
  Free += 2;
  if (Update_UID_Count)
    Update_FObj_Slot(Hash_Number, FIXNUM_0+1+Get_Integer(New_Unique_ID));
  return New_Unique_ID;
}

/* (OBJECT-HASH OBJECT)
      [Primitive number 0x5A]
      Returns the unique hash number associated with OBJECT.  This is
      used in the implementation of property lists and populations.
*/
Built_In_Primitive(Prim_Object_Hash, 1, "OBJECT-HASH")
{ Primitive_1_Arg();

  The_Hash_Table = Get_Fixed_Obj_Slot(Hash_Table);
  The_Unhash_Table = Get_Fixed_Obj_Slot(Unhash_Table);
  if (The_Hash_Table==NIL) Primitive_Error(ERR_NO_HASH_TABLE);
  HASH_TABLE_SIZE = Vector_Length(The_Unhash_Table);
  return Hash_One_Object(Arg1, Get_Fixed_Obj_Slot(Hash_Number), true);
}

/* (OBJECT_UNHASH NUMBER)
      [Primitive number 0x5B]
      Returns the object associated with a hash number (ie the inverse
      operation of OBJECT_HASH).  Returns NIL if there is no
      associated object (which will occur if no object was ever hashed
      to this value, or if that object has been removed by a garbage
      collection, since these hash table are explicitly built in order
      NOT to retain objects which would otherwise disappear.)
*/
Built_In_Primitive(Prim_Object_Unhash, 1, "OBJECT-UNHASH")
{ long Obj_Hash;
  Pointer Bucket;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  The_Unhash_Table = Get_Fixed_Obj_Slot(Unhash_Table);
  if (The_Unhash_Table==NIL) Primitive_Error(ERR_NO_HASH_TABLE);
  HASH_TABLE_SIZE = Vector_Length(The_Unhash_Table);
  Obj_Hash = Hash_It(Arg1) % HASH_TABLE_SIZE + 1;
  Bucket = Vector_Ref(The_Unhash_Table, Obj_Hash);
  while (Type_Code(Bucket) == TC_LIST)
  { Pointer Entry;
    Entry = Vector_Ref(Bucket, CONS_CAR);
    if (Arg1 == Vector_Ref(Entry, CONS_CDR))
      return Vector_Ref(Entry, CONS_CAR);
    Bucket = Vector_Ref(Bucket, CONS_CDR);
  }
  return NIL;
}

/* (REHASH_GC_DAEMON)
      [Primitive number 0x5C]
      Used only immediately after a GC, this primitive creates a new
      pair of hash tables for use with the property list and
      population mechanisms.  It depends on the broken hearts left by
      the previous GC.
*/
Built_In_Primitive(Prim_Rehash_Gc_Daemon, 0, "REHASH-GC-DAEMON")
{ fast Pointer Chain;
  Primitive_0_Args();

  The_Hash_Table = Get_Fixed_Obj_Slot(Hash_Table);
  The_Unhash_Table = Get_Fixed_Obj_Slot(Unhash_Table);
  if (The_Hash_Table == NIL) return NIL;
  HASH_TABLE_SIZE = Vector_Length(The_Unhash_Table);
  Chain = NIL;

/* Create a single chain of all the entries from the hash table ...
   clear both the hash and unhash tables on the way */

  { fast Pointer Chain_End, Bucket;
    fast long i;
    Chain_End = NIL;

    for (i=1; i <= HASH_TABLE_SIZE; i++)
    { Fast_Vector_Set(The_Unhash_Table, i, NIL);
      Bucket = Fast_Vector_Ref(The_Hash_Table, i);
      if (Bucket != NIL)
      { if (Chain==NIL) Chain = Bucket;
	else Fast_Vector_Set(Chain_End, CONS_CDR, Bucket);
	while (Fast_Vector_Ref(Bucket, CONS_CDR) != NIL)
	  Bucket = Fast_Vector_Ref(Bucket, CONS_CDR);
	Chain_End = Bucket;
	Fast_Vector_Set(The_Hash_Table, i, NIL);
      }
    }
  }
  
/* Prim_Rehash_Gc_Daemon continues on the next page */

/* Prim_Rehash_Gc_Daemon, continued */

/* Walk the chain rehashing entries that have been relocated */

  { fast Pointer *Scan, Temp, *Old, *Low_Constant;
    Low_Constant = Constant_Space;
    while (Chain != NIL)
    { Scan = Get_Pointer(Fast_Vector_Ref(Chain, CONS_CAR));
      Chain = Fast_Vector_Ref(Chain, CONS_CDR);
      Temp = *Scan;
      switch(GC_Type(Temp))
      { case GC_Non_Pointer:
	  Hash_One_Object(Temp, Scan[1], false);
	  continue;

#define Rehash_An_Object(obj) Hash_One_Object(obj, Scan[1], false)

	case GC_Cell:
	case GC_Pair:
	case GC_Triple:
	case GC_Quadruple:
	case GC_Vector:
	  Old = Get_Pointer(Temp);
	  if (Old >= Low_Constant)
	  { Rehash_An_Object(Temp);
	    continue;
	  }
	  Normal_BH(false, Rehash_An_Object(*Scan));
	  continue;

	case GC_Compiled:
	  Old = Get_Pointer(Temp);
	  if (Old >= Low_Constant)
	  { Rehash_An_Object(Temp);
	    continue;
	  }
	  Compiled_BH(false, Rehash_An_Object(*Scan));
	  continue;

	case GC_Special:
	case GC_Undefined:
	default:
	  fprintf(stderr,
		  "\nRehash-GC-Daemon: Bad Object: Type = 0x%02x; Datum = %x\n",
		  Type_Code(Temp), Datum(Temp));
	  Microcode_Termination(TERM_INVALID_TYPE_CODE);
	}
    }
  }
  return TRUTH;
}

/* The format of the open files vector is:

        |----------------|--------|
        |MANIFEST_VECTOR |    n   |
        |----------------|--------|.
        |FIXNUM          |    m   | .    n = length of the vector
        |----------------|--------| |    m = count of used slots
   Lock |NULL or NM_VECT |   n-2  | |
       .|----------------|--------| |    HUNK3s are formatted:
      . |HUNK3           |   ----------> |--------------------|
      | |----------------|--------| |    |   Channel number   |
      | |HUNK3           |        | |    |--------------------|
      | |----------------|--------| |    |      File Name     |
   m <  |HUNK3           |        |  > n |--------------------|
      | |----------------|--------| |    |   Input or Output  |
      | |HUNK3           |        | |    |--------------------|
      | |----------------|--------| |
      . |   ...          |        | |    If the type code of Lock
       .|----------------|--------| |    is NULL, then the vector
        |  ---UNUSED---  |        | |    is in use by SCHEME and
        |----------------|--------| |    cannot be accessed here.
        |   ...          |        | .
        |----------------|--------|.
 */

#define OPEN_FILES_COUNT	1
#define OPEN_FILES_INTERLOCK	2
#define OPEN_FILES_FIRST_FILE	3

#define FILE_CHANNEL		0
#define FILE_NAME		1
#define FILE_IN_OR_OUT		2

/* (CLOSE_LOST_OPEN_FILES)
      [Primitive number 0xC7]
      This primitive can ONLY be called as one of the GC daemons.  It
      is responsible for closing and releasing any files which have
      "disappeared" due to a garbage collection.  It relies on the
      broken hearts left behind by the GC to do its work.

      Note that it depends on the fact that file blocks are hunk3s in
      the following way: The broken heart left around is in the first
      word of the old space copy of the file block.
*/
Built_In_Primitive(Prim_Close_Lost_Open_Files, 0, "CLOSE-LOST-OPEN-FILES")
{ Pointer Open_Files_Vector, *From_File, *To_File;
  long i, NFiles, Orig_Count;
  Primitive_0_Args();
  /* Close_Lost_Open_Files walks down the used entries of the
     Open Files Vector.  For each entry, it either relocates it (if
     the Garbage Collector provided a forwarding address) or it closes
     the file and removes the entry from the vector.
  */
  Open_Files_Vector = Get_Fixed_Obj_Slot(Open_Files);
  if ((Open_Files_Vector==NIL) ||
      (Type_Code(Vector_Ref(Open_Files_Vector,
                            OPEN_FILES_INTERLOCK)) ==
       TC_NULL)) return NIL;
  Orig_Count = Get_Integer(Vector_Ref(Open_Files_Vector,
                                      OPEN_FILES_COUNT));
  NFiles = Orig_Count;
  To_File = Nth_Vector_Loc(Open_Files_Vector, OPEN_FILES_FIRST_FILE);

/* Prim_Close_Lost_Open_Files continues on next page */

/* Prim_Close_Lost_Open_Files, continued */

  for (i=0, From_File=To_File; i < Orig_Count; i++, From_File++)
  { if (Type_Code(*Get_Pointer(*From_File))==TC_BROKEN_HEART)
    { /* The file block (hunk3) has been moved by the GC which just
         ended.  Relocate the pointer in the Open Files Vector. */
      Store_Address(*To_File, Datum(*Get_Pointer(*From_File)));
      To_File += 1;
    }
    else
    { if (Get_Pointer(*From_File) > Constant_Space)
      { Store_Address(*To_File, Datum(*From_File));
        To_File += 1;
      }
      else
      { /* The file is no longer accessible, since its file block
           was not relocated by the GC. Close the file and shrink the
           Open Files Vector */
        long File_Number;
        File_Number = Get_Integer(Vector_Ref(*From_File, FILE_CHANNEL));
        fclose(Channels[File_Number]);
        Channels[File_Number] = NULL;
        NFiles -= 1;
      }
    }
  }
  for (i=NFiles; i < Orig_Count; i++) *To_File++ = NIL;
  Vector_Set(Open_Files_Vector, OPEN_FILES_COUNT, FIXNUM_0+NFiles);
  return TRUTH;
}
