/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchgcl.c,v 9.28 1987/04/16 02:06:42 jinx Exp $ */

/* bchgcl, bchmmg, bchpur, and bchdmp can replace gcloop, memmag,
   purify, and fasdump, respectively, to provide garbage collection
   and related utilities to disk.
*/

#include "scheme.h"
#include "bchgcc.h"

/* Some utility macros */

#define copy_cell()							\
{ *To++ = *Old;								\
}

#define copy_pair()							\
{ *To++ = *Old++;							\
  *To++ = *Old;								\
}

#define copy_weak_pair()						\
{ long Car_Type;							\
									\
  Car_Type = Type_Code(*Old);						\
  *To++ = Make_New_Pointer(TC_NULL, *Old);				\
  Old += 1;								\
  *To++ = *Old;								\
  *Old = Make_New_Pointer(Car_Type, Weak_Chain);			\
  Weak_Chain = Temp;							\
}

#define copy_triple()							\
{ *To++ = *Old++;							\
  *To++ = *Old++;							\
  *To++ = *Old;								\
}

#define copy_quadruple()						\
{ *To++ = *Old++;							\
  *To++ = *Old++;							\
  *To++ = *Old++;							\
  *To++ = *Old;								\
}

/* Transporting vectors is done in 3 parts:
   - Finish filling the current free buffer, dump it, and get a new one.
   - Dump the middle of the vector directly by bufferfulls.
   - Copy the end of the vector to the new buffer.
   The last piece of code is the only one executed when the vector does
   not overflow the current buffer.
*/

#define copy_vector()							\
{ Pointer *Saved_Scan = Scan;						\
  unsigned long real_length = 1 + Get_Integer(*Old);			\
									\
  To_Address += real_length;						\
  Scan = To + real_length;						\
  if (Scan >= free_buffer_top)						\
  { unsigned long overflow;						\
									\
    overflow = Scan - free_buffer_top;					\
    while (To != free_buffer_top) *To++ = *Old++;			\
    To = dump_and_reset_free_buffer(0);					\
    real_length = (overflow / GC_DISK_BUFFER_SIZE);			\
    if (real_length > 0) dump_free_directly(Old, real_length);		\
    Old += (real_length * GC_DISK_BUFFER_SIZE);				\
    Scan = To + (overflow % GC_DISK_BUFFER_SIZE);			\
  }									\
  while (To != Scan) *To++ = *Old++;					\
  Scan = Saved_Scan;							\
}

#define relocate_normal_setup()						\
{									\
  Old = Get_Pointer(Temp);						\
  if (Old >= Low_Constant) continue;					\
  if (Type_Code(*Old) == TC_BROKEN_HEART)				\
  { *Scan = Make_New_Pointer(Type_Code(Temp), *Old);			\
    continue;								\
  }									\
  New_Address = Make_Broken_Heart(C_To_Scheme(To_Address));		\
}

#define relocate_normal_transport(copy_code, length)			\
{ copy_code;								\
  To_Address += (length);						\
  if (To >= free_buffer_top)						\
    To = dump_and_reset_free_buffer(To - free_buffer_top);		\
}

#define relocate_normal_end()						\
{ *Get_Pointer(Temp) = New_Address;					\
  *Scan = Make_New_Pointer(Type_Code(Temp), New_Address);		\
  continue;								\
}

#define relocate_normal_pointer(copy_code, length)			\
{ relocate_normal_setup();						\
  relocate_normal_transport(copy_code, length);				\
  relocate_normal_end();						\
}

Pointer
*GCLoop(Scan, To_ptr, To_Address_ptr)
fast Pointer *Scan;
Pointer **To_ptr, **To_Address_ptr;
{ fast Pointer *To, *Old, Temp, *Low_Constant, *To_Address, New_Address;

  To = *To_ptr;
  To_Address = *To_Address_ptr;
  Low_Constant = Constant_Space;

  for ( ; Scan != To; Scan++)
  { Temp = *Scan;
    Switch_by_GC_Type(Temp)
    { case TC_BROKEN_HEART:
        if (Scan != (Get_Pointer(Temp)))
	{ fprintf(stderr, "GC: Broken heart in scan.\n");
	  Microcode_Termination(TERM_BROKEN_HEART);
	}
	if (Scan != scan_buffer_top) goto end_gcloop;
	/* The -1 is here because of the Scan++ in the for header. */
	Scan = dump_and_reload_scan_buffer(0) - 1;
	continue;

      case TC_MANIFEST_NM_VECTOR:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
	/* Check whether this bumps over current buffer,
	   and if so we need a new bufferfull. */
	Scan += Get_Integer(Temp);
	if (Scan < scan_buffer_top)
	  break;
	else
	{ unsigned long overflow;
	  /* The + & -1 are here because of the Scan++ in the for header. */
	  overflow = (Scan - scan_buffer_top) + 1;
	  Scan = ((dump_and_reload_scan_buffer(overflow / GC_DISK_BUFFER_SIZE) +
		   (overflow % GC_DISK_BUFFER_SIZE)) - 1);
	  break;
	}

      case_Non_Pointer:
	break;

      case_compiled_entry_point:
	Old = Get_Pointer(Temp);
	if (Old >= Low_Constant) continue;
	Old = Get_Compiled_Block(Old);
	if (Type_Code(*Old) == TC_BROKEN_HEART) 
	{ *Scan = Relocate_Compiled(Temp, Get_Pointer(*Old), Old);
	  continue;
	}
	else
	{ Pointer *Saved_Old = Old;
	  New_Address = Make_Broken_Heart(C_To_Scheme(To_Address));
	  copy_vector();
	  *Saved_Old = New_Address;
	  *Scan = Relocate_Compiled(Temp, Get_Pointer(New_Address), Saved_Old);
	  continue;
	}

      case_Cell:
	relocate_normal_pointer(copy_cell(), 1);

      case TC_REFERENCE_TRAP:
	if (Datum(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* It is a pair, fall through. */
      case_Pair:
	relocate_normal_pointer(copy_pair(), 2);

      case TC_VARIABLE:
      case_Triple:
	relocate_normal_pointer(copy_triple(), 3);

      case_Quadruple:
	relocate_normal_pointer(copy_quadruple(), 4);

#ifdef FLOATING_ALIGNMENT
      case TC_BIG_FLONUM:
	/* This must be fixed. */
#include "error: bchgcl does not handle floating alignment."
#else
      case TC_BIG_FLONUM:
	/* Fall through */
#endif
      case_Vector:
	relocate_normal_setup();
      Move_Vector:
	copy_vector();
	relocate_normal_end();

      case TC_FUTURE:
	relocate_normal_setup();
	if (!(Future_Spliceable(Temp))) goto Move_Vector;
	*Scan = Future_Value(Temp);
	Scan -= 1;
	continue;

      case TC_WEAK_CONS:
	relocate_normal_pointer(copy_weak_pair(), 2);

      default:
	fprintf(stderr,
		"GCLoop: Bad type code = 0x%02x\n",
		Type_Code(Temp));
	Invalid_Type_Code();
      }
  }
end_gcloop:
  *To_ptr = To;
  *To_Address_ptr = To_Address;
  return Scan;
}
