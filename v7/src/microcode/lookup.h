/* -*-C-*-

$Id: lookup.h,v 9.52 2000/12/05 21:23:45 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Macros and declarations for the variable lookup code. */

extern SCHEME_OBJECT
  * EXFUN (deep_lookup, (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT *)),
  * EXFUN (lookup_fluid, (SCHEME_OBJECT)),
  * EXFUN (force_definition, (SCHEME_OBJECT, SCHEME_OBJECT, long *));

extern long
  EXFUN (deep_lookup_end, (SCHEME_OBJECT *, SCHEME_OBJECT *)),
  EXFUN (deep_assignment_end,
	 (SCHEME_OBJECT *, SCHEME_OBJECT *, SCHEME_OBJECT, Boolean));

extern long EXFUN (recache_uuo_links, (SCHEME_OBJECT, SCHEME_OBJECT));

extern SCHEME_OBJECT
  unbound_trap_object[],
  uncompiled_trap_object[],
  illegal_trap_object[],
  fake_variable_object[];

#define GC_allocate_test(N)		GC_Check(N)

#define AUX_LIST_TYPE			TC_VECTOR

#define AUX_CHUNK_SIZE			20
#define AUX_LIST_COUNT			ENV_EXTENSION_COUNT
#define AUX_LIST_FIRST			ENV_EXTENSION_MIN_SIZE
#define AUX_LIST_INITIAL_SIZE		(AUX_LIST_FIRST + AUX_CHUNK_SIZE)

/* Variable compilation types. */

#define LOCAL_REF			TC_NULL
#define GLOBAL_REF			TC_UNINTERNED_SYMBOL
#define FORMAL_REF			TC_CHARACTER
#define AUX_REF				TC_FIXNUM
#define UNCOMPILED_REF			TC_CONSTANT

/* Common constants. */

#if (SIZEOF_UNSIGNED_LONG == 4)	/* 32 bit objects */
#  if (TYPE_CODE_LENGTH == 8)
#    define UNCOMPILED_VARIABLE		0x08000000
#  endif
#  if (TYPE_CODE_LENGTH == 6)
#    define UNCOMPILED_VARIABLE		0x20000000
#  endif
#  if (TC_CONSTANT != 0x08)
#    include "error:lookup.h and types.h are inconsistent"
#  endif
#endif

#ifndef UNCOMPILED_VARIABLE		/* Safe version */
#define UNCOMPILED_VARIABLE		MAKE_OBJECT (UNCOMPILED_REF, 0)
#endif

/* Macros for speedy variable reference. */

#if (LOCAL_REF == 0)

#define Lexical_Offset(Ind)		((long) (Ind))
#define Make_Local_Offset(Ind)		((SCHEME_OBJECT) (Ind))

#else

#define Lexical_Offset(Ind)		OBJECT_DATUM (Ind)
#define Make_Local_Offset(Ind)		MAKE_OBJECT (LOCAL_REF, Ind)

#endif

/* The code below depends on the following. */

/* Done as follows because of VMS. */

#define lookup_inconsistency_p						\
  ((VARIABLE_OFFSET == VARIABLE_COMPILED_TYPE) ||			\
   (VARIABLE_FRAME_NO != VARIABLE_COMPILED_TYPE))

#if (lookup_inconsistency_p)
#include "error: lookup.h inconsistency detected."
#endif

#define get_offset(hunk) Lexical_Offset(MEMORY_FETCH (hunk[VARIABLE_OFFSET]))

#ifdef PARALLEL_PROCESSOR

#define verify(type_code, variable, code, label)			\
{									\
  variable = code;							\
  if (OBJECT_TYPE (MEMORY_FETCH (hunk[VARIABLE_COMPILED_TYPE])) !=	\
      type_code)							\
    goto label;								\
}

#define verified_offset(variable, code)		variable

/* Unlike Lock_Cell, cell must be (SCHEME_OBJECT *).  This currently does
   not matter, but might on a machine with address mapping.
 */

#define DECLARE_LOCK(name) Lock_Handle name
#define setup_lock(handle, cell)		handle = Lock_Cell(cell)
#define remove_lock(handle)			Unlock_Cell(handle)

/* This should prevent a deadly embrace if whole contiguous
   regions are locked, rather than individual words.
 */

#define setup_locks(hand1, cel1, hand2, cel2)				\
{									\
  if (LOCK_FIRST(cel1, cel2))						\
  {									\
    setup_lock(hand1, cel1);						\
    setup_lock(hand2, cel2);						\
  }									\
  else									\
  {									\
    setup_lock(hand2, cel2);						\
    setup_lock(hand1, cel1);						\
  }									\
}

#define remove_locks(hand1, hand2)					\
{									\
  remove_lock(hand2);							\
  remove_lock(hand1);							\
}

#else /* not PARALLEL_PROCESSOR */

#define verify(type_code, variable, code, label)
#define verified_offset(variable, code)		code
/* #undef DECLARE_LOCK */
#define setup_lock(handle, cell)
#define remove_lock(ignore)
#define setup_locks(hand1, cel1, hand2, cel2)
#define remove_locks(ign1, ign2)

#endif /* PARALLEL_PROCESSOR */

/* This is provided as a separate macro so that it can be made
   atomic if necessary.
 */

#define update_lock(handle, cell)					\
{									\
  remove_lock(handle);							\
  setup_lock(handle, cell);						\
}

#ifndef Future_Variable_Splice
/* Parameter list (Vbl, Ofs, Value) displeased some compilers */
#define Future_Variable_Splice(a, b, c) 
#endif

/* SCHEME_OBJECT *cell, env, *hunk; */

#define lookup(cell, env, hunk, label)					\
{									\
  fast SCHEME_OBJECT frame;						\
									\
/* Deleted this label to eliminate compiler warnings: */		\
/* label: */								\
									\
  frame = (MEMORY_FETCH (hunk [VARIABLE_COMPILED_TYPE]));		\
									\
  switch (OBJECT_TYPE (frame))						\
  {									\
    case GLOBAL_REF:							\
      /* frame is a pointer to the same symbol. */			\
      cell = MEMORY_LOC (frame, SYMBOL_GLOBAL_VALUE);			\
      break;								\
									\
    case LOCAL_REF:							\
      cell = MEMORY_LOC (env, Lexical_Offset(frame));			\
      break;								\
									\
    case FORMAL_REF:							\
      lookup_formal(cell, env, hunk, label);				\
									\
    case AUX_REF:							\
      lookup_aux(cell, env, hunk, label);				\
									\
    default:								\
      /* Done here rather than in a separate case because of		\
	 peculiarities of the bobcat compiler.				\
       */								\
      cell = ((OBJECT_TYPE (frame) == UNCOMPILED_REF) ?			\
	      uncompiled_trap_object :					\
	      illegal_trap_object);					\
      break;								\
 }									\
}

#define lookup_formal(cell, env, hunk, label)				\
{									\
  fast long depth;							\
									\
  verify(FORMAL_REF, offset, get_offset(hunk), label);			\
  depth = (OBJECT_DATUM (frame));					\
  frame = env;								\
  while(--depth >= 0)							\
  {									\
    frame = FAST_MEMORY_REF (MEMORY_REF (frame, ENVIRONMENT_FUNCTION),	\
			    PROCEDURE_ENVIRONMENT);			\
  }									\
									\
  cell = MEMORY_LOC (frame,						\
			verified_offset(offset, get_offset(hunk)));	\
									\
  break;								\
}

#define lookup_aux(cell, env, hunk, label)				\
{									\
  fast long depth;							\
									\
  verify(AUX_REF, offset, get_offset(hunk), label);			\
  depth = (OBJECT_DATUM (frame));					\
  frame = env;								\
  while(--depth >= 0)							\
  {									\
    frame = FAST_MEMORY_REF (MEMORY_REF (frame, ENVIRONMENT_FUNCTION),	\
			    PROCEDURE_ENVIRONMENT);			\
  }									\
									\
  frame = MEMORY_REF (frame, ENVIRONMENT_FUNCTION);			\
  if (OBJECT_TYPE (frame) != AUX_LIST_TYPE)				\
  {									\
    cell = uncompiled_trap_object;					\
    break;								\
  }									\
  depth = verified_offset(offset, get_offset(hunk));			\
  if (depth > ((long) (VECTOR_LENGTH (frame))))				\
  {									\
    cell = uncompiled_trap_object;					\
    break;								\
  }									\
  frame = MEMORY_REF (frame, depth);					\
  if ((frame == SHARP_F) ||						\
      (FAST_PAIR_CAR (frame) != hunk[VARIABLE_SYMBOL]))			\
  {									\
    cell = uncompiled_trap_object;					\
    break;								\
  }									\
  cell = PAIR_CDR_LOC (frame);						\
  break;								\
}

/* Macros and exports for incremental definition and hooks. */

extern long
  EXFUN (extend_frame,
	 (SCHEME_OBJECT, SCHEME_OBJECT, SCHEME_OBJECT,
	  SCHEME_OBJECT, Boolean));

/* Definition recaches eagerly by default. */

#ifndef DEFINITION_RECACHES_LAZILY
# ifndef DEFINITION_RECACHES_EAGERLY
#  define DEFINITION_RECACHES_EAGERLY
# endif
#endif

#ifndef DEFINITION_RECACHES_EAGERLY

extern long
  EXFUN (compiler_uncache, (SCHEME_OBJECT *, SCHEME_OBJECT));

#define simple_uncache(cell, sym)		PRIM_DONE

#define shadowing_recache(cell, env, sym, value, shadowed_p)		\
  definition(cell, value, shadowed_p)

#define compiler_recache(old, new, env, sym, val, shadowed_p, link_p)	\
  PRIM_DONE

#else /* DEFINITION_RECACHES_EAGERLY */

extern long
  EXFUN (compiler_recache,
	 (SCHEME_OBJECT *, SCHEME_OBJECT *, SCHEME_OBJECT, SCHEME_OBJECT,
	  SCHEME_OBJECT, Boolean, Boolean));

extern SCHEME_OBJECT * shadowed_value_cell;

#define compiler_uncache(cell, sym)					\
  (shadowed_value_cell = cell, PRIM_DONE)

#define simple_uncache(cell, sym)					\
  compiler_uncache(cell, sym)

#define shadowing_recache(cell, env, sym, value, shadowed_p)		\
  compiler_recache(shadowed_value_cell, cell, env, sym, value,		\
	  	   shadowed_p, false)

#endif /* DEFINITION_RECACHES_EAGERLY */
