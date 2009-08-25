/* obstack.c - subroutines used implicitly by object stack macros
   Copyright (C) 1988 Free Software Foundation, Inc.

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 1, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

#include "obstack.h"


/* Determine default alignment.  */
struct fooalign {char x; double d;};
#define DEFAULT_ALIGNMENT	\
  ((long) ((char *)&((struct fooalign *) 0)->d - (char *)0))
/* If malloc were really smart, it would round addresses to DEFAULT_ALIGNMENT.
   But in fact it might be less smart and round addresses to as much as
   DEFAULT_ROUNDING.  So we prepare for it to do that.  */
union fooround {long x; double d;};
#define DEFAULT_ROUNDING (sizeof (union fooround))

/* When we copy a long block of data, this is the unit to do it with.
   On some machines, copying successive ints does not work;
   in such a case, redefine COPYING_UNIT to `long' (if that works)
   or `char' as a last resort.  */
#ifndef COPYING_UNIT
#  define COPYING_UNIT int
#endif

/* The non-GNU-C macros copy the obstack into this global variable
   to avoid multiple evaluation.  */

struct obstack *_obstack;

/* Initialize an obstack H for use.  Specify chunk size SIZE (0 means default).
   Objects start on multiples of ALIGNMENT (0 means use default).
   CHUNKFUN is the function to use to allocate chunks,
   and FREEFUN the function to free them.  */

void
_obstack_begin (struct obstack * h,
		int size,
		long alignment,
		void * (*chunkfun) (size_t),
		void (*freefun) (void *))
{
  struct _obstack_chunk* chunk; /* points to new chunk */

  if (alignment == 0)
    alignment = DEFAULT_ALIGNMENT;
  if (size == 0)
    /* Default size is what GNU malloc can fit in a 4096-byte block.  */
    {
      /* 12 is sizeof (mhead) and 4 is EXTRA from GNU malloc.
	 Use the values for range checking, because if range checking is off,
	 the extra bytes won't be missed terribly, but if range checking is on
	 and we used a larger request, a whole extra 4096 bytes would be
	 allocated.

	 These number are irrelevant to the new GNU malloc.  I suspect it is
	 less sensitive to the size of the request.  */
      int extra = ((((12 + DEFAULT_ROUNDING - 1) & ~(DEFAULT_ROUNDING - 1))
		    + 4 + DEFAULT_ROUNDING - 1)
		   & ~(DEFAULT_ROUNDING - 1));
      size = 4096 - extra;
    }

  h->chunkfun = (struct _obstack_chunk * (*)(long)) chunkfun;
  h->freefun = freefun;
  h->chunk_size = size;
  h->alignment_mask = alignment - 1;

  chunk	= h->chunk = (*h->chunkfun) (h->chunk_size);
  h->next_free = h->object_base = chunk->contents;
  h->chunk_limit = chunk->limit
   = (char *) chunk + h->chunk_size;
  chunk->prev = 0;
}

/* Allocate a new current chunk for the obstack *H
   on the assumption that LENGTH bytes need to be added
   to the current object, or a new object of length LENGTH allocated.
   Copies any partial object from the end of the old chunk
   to the beginning of the new one.  */

void
_obstack_newchunk (struct obstack * h, int length)
{
  struct _obstack_chunk*	old_chunk = h->chunk;
  struct _obstack_chunk*	new_chunk;
  long	new_size;
  int obj_size = h->next_free - h->object_base;
  int i;
  int already;

  /* Compute size for new chunk.  */
  new_size = (obj_size + length) + (obj_size >> 3) + 100;
  if (new_size < h->chunk_size)
    new_size = h->chunk_size;

  /* Allocate and initialize the new chunk.  */
  new_chunk = h->chunk = (*h->chunkfun) (new_size);
  new_chunk->prev = old_chunk;
  new_chunk->limit = h->chunk_limit = (char *) new_chunk + new_size;

  /* Move the existing object to the new chunk.
     Word at a time is fast and is safe if the object
     is sufficiently aligned.  */
  if (h->alignment_mask + 1 >= DEFAULT_ALIGNMENT)
    {
      for (i = obj_size / sizeof (COPYING_UNIT) - 1;
	   i >= 0; i--)
	((COPYING_UNIT *)new_chunk->contents)[i]
	  = ((COPYING_UNIT *)h->object_base)[i];
      /* We used to copy the odd few remaining bytes as one extra COPYING_UNIT,
	 but that can cross a page boundary on a machine
	 which does not do strict alignment for COPYING_UNITS.  */
      already = obj_size / sizeof (COPYING_UNIT) * sizeof (COPYING_UNIT);
    }
  else
    already = 0;
  /* Copy remaining bytes one by one.  */
  for (i = already; i < obj_size; i++)
    new_chunk->contents[i] = h->object_base[i];

  /* If the object just copied was the only data in OLD_CHUNK,
     free that chunk and remove it from the chain.  */
  if (h->object_base == old_chunk->contents)
    {
      new_chunk->prev = old_chunk->prev;
      (*h->freefun) (old_chunk);
    }

  h->object_base = new_chunk->contents;
  h->next_free = h->object_base + obj_size;
}

/* Return nonzero if object OBJ has been allocated from obstack H.
   This is here for debugging.
   If you use it in a program, you are probably losing.  */

int
_obstack_allocated_p (struct obstack * h, void * obj)
{
  struct _obstack_chunk*  lp;	/* below addr of any objects in this chunk */
  struct _obstack_chunk*  plp;	/* point to previous chunk if any */

  lp = (h)->chunk;
  while (lp != 0 && ((void *)lp > obj || (void *)(lp)->limit < obj))
    {
      plp = lp -> prev;
      lp = plp;
    }
  return lp != 0;
}

/* Free objects in obstack H, including OBJ and everything allocate
   more recently than OBJ.  If OBJ is zero, free everything in H.  */

#undef obstack_free

void
obstack_free (struct obstack *h, void * obj)
{
  struct _obstack_chunk*  lp;	/* below addr of any objects in this chunk */
  struct _obstack_chunk*  plp;	/* point to previous chunk if any */

  lp = (h)->chunk;
  /* We use >= because there cannot be an object at the beginning of a chunk.
     But there can be an empty object at that address
     at the end of another chunk.  */
  while (lp != 0 && ((void *)lp >= obj || (void *)(lp)->limit < obj))
    {
      plp = lp -> prev;
      (*h->freefun) (lp);
      lp = plp;
    }
  if (lp)
    {
      (h)->object_base = (h)->next_free = (char *)(obj);
      (h)->chunk_limit = lp->limit;
      (h)->chunk = lp;
    }
  else if (obj != 0)
    /* obj is not in any of the chunks! */
    abort ();
}

/* Let same .o link with output of gcc and other compilers.  */

void
_obstack_free (struct obstack * h, void * obj)
{
  obstack_free (h, obj);
}
