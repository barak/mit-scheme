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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchdmp.c,v 9.30 1987/06/02 00:16:04 jinx Exp $ */

/* bchgcl, bchmmg, bchpur, and bchdmp can replace gcloop, memmag,
   purify, and fasdump, respectively, to provide garbage collection
   and related utilities to disk.
*/

#include "scheme.h"
#include "primitive.h"
#include "trap.h"
#include "lookup.h"		/* UNCOMPILED_VARIABLE */
#define In_Fasdump
#include "bchgcc.h"
#include "dump.c"

extern Pointer Make_Prim_Exts();
static char *dump_file_name;
static int real_gc_file, dump_file;
static Pointer *saved_free;
static Pointer fixup_buffer[GC_DISK_BUFFER_SIZE];
static Pointer *fixup_buffer_end = &fixup_buffer[GC_DISK_BUFFER_SIZE];
static Pointer *fixup;
static fixup_count = 0;

/* Utility macros. */

#define fasdump_normal_setup()						\
{									\
  Old = Get_Pointer(Temp);						\
  if (Type_Code(*Old) == TC_BROKEN_HEART)				\
  {									\
    *Scan = Make_New_Pointer(Type_Code(Temp), *Old);			\
    continue;								\
  }									\
  New_Address = Make_Broken_Heart(C_To_Scheme(To_Address));		\
  fasdump_remember_to_fix(Old, *Old);					\
}

#define fasdump_transport_end(length)					\
{									\
  To_Address += (length);						\
  if (To >= free_buffer_top)						\
    To = dump_and_reset_free_buffer(To - free_buffer_top);		\
}

#define fasdump_normal_transport(copy_code, length)			\
{									\
  copy_code;								\
  fasdump_transport_end(length);					\
}

#define fasdump_normal_end()						\
{									\
  *Get_Pointer(Temp) = New_Address;					\
  *Scan = Make_New_Pointer(Type_Code(Temp), New_Address);		\
  continue;								\
}

#define fasdump_normal_pointer(copy_code, length)			\
{									\
  fasdump_normal_setup();						\
  fasdump_normal_transport(copy_code, length);				\
  fasdump_normal_end();							\
}

#define fasdump_remember_to_fix(location, contents)			\
{									\
  if ((fixup == fixup_buffer) && (!reset_fixes()))			\
    return false;							\
  *--fixup = contents;							\
  *--fixup = ((Pointer) location);					\
}

void
fasdump_exit(length)
     long length;
{
  extern int ftruncate(), unlink();
  fast Pointer *fixes, *fix_address;

  Free = saved_free;
  gc_file = real_gc_file;
  ftruncate(dump_file, length);
  close(dump_file);
  if (length == 0)
    unlink(dump_file_name);
  dump_file_name = ((char *) NULL);
  
  fixes = fixup;

next_buffer:

  while (fixes != fixup_buffer_end)
  {
    fix_address = ((Pointer *) (*fixes++)); /* Where it goes. */
    *fix_address = *fixes++;		    /* Put it there. */
  }
  
  if (fixup_count >= 0)
  {
    lseek(real_gc_file, (fixup_count * GC_BUFFER_BYTES), 0);
    read(real_gc_file, fixup_buffer, GC_BUFFER_BYTES);
    fixup_count -= 1;
    fixes = fixup_buffer;
    goto next_buffer;
  }
  
  fixup = fixes;
  Fasdump_Exit_Hook();
  return;
}

Boolean
reset_fixes()
{
  fixup_count += 1;
  if ((lseek(real_gc_file, (fixup_count * GC_BUFFER_BYTES), 0) == -1) ||
      (write(real_gc_file, fixup_buffer, GC_BUFFER_BYTES) != GC_BUFFER_BYTES))
    return false;
  fixup = fixup_buffer_end;
  return true;
}

/* A copy of GCLoop, with minor modifications. */

Boolean
dumploop(Scan, To_ptr, To_Address_ptr)
     fast Pointer *Scan;
     Pointer **To_ptr, **To_Address_ptr;
{
  fast Pointer *To, *Old, Temp, *To_Address, New_Address;

  To = *To_ptr;
  To_Address = *To_Address_ptr;

  for ( ; Scan != To; Scan++)
  {
    Temp = *Scan;
    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
        if (Datum(Temp) == 0)
	  break;
        if (Scan != (Get_Pointer(Temp)))
	{
	  fprintf(stderr, "\ndumploop: Broken heart in scan.\n");
	  Microcode_Termination(TERM_BROKEN_HEART);
	}
	if (Scan != scan_buffer_top)
	  goto end_dumploop;
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
	{
	  unsigned long overflow;

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
	Compiled_BH(true, continue);
	{
	  Pointer *Saved_Old = Old;

	  fasdump_remember_to_fix(Old, *Old);
	  New_Address = Make_Broken_Heart(C_To_Scheme(To_Address));
	  copy_vector();
	  *Saved_Old = New_Address;
	  *Scan = Relocate_Compiled(Temp, Get_Pointer(New_Address), Saved_Old);
	  continue;
	}

      case_Cell:
	fasdump_normal_pointer(copy_cell(), 1);

      case TC_REFERENCE_TRAP:
	if (Datum(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  /* It is a non pointer. */
	  break;
	}
	/* It is a pair, fall through. */
      case TC_WEAK_CONS:
      case_Fasdump_Pair:
	fasdump_normal_pointer(copy_pair(), 2);

      case TC_INTERNED_SYMBOL:
      {
	fasdump_normal_setup();
	*To++ = *Old;
	*To++ = Make_Broken_Heart(0);
	fasdump_transport_end(2);
	fasdump_normal_end();
      }

      case TC_UNINTERNED_SYMBOL:
      {
	fasdump_normal_setup();
	*To++ = *Old;
	*To++ = UNBOUND_OBJECT;
	fasdump_transport_end(2);
	fasdump_normal_end();
      }

      case_Triple:
	fasdump_normal_pointer(copy_triple(), 3);

      case TC_VARIABLE:
      {
	fasdump_normal_setup();
	*To++ = *Old;
	*To++ = UNCOMPILED_VARIABLE;
	*To++ = NIL;
	fasdump_transport_end(3);
	fasdump_normal_end();
      }

      case_Quadruple:
	fasdump_normal_pointer(copy_quadruple(), 4);

#ifdef FLOATING_ALIGNMENT
      case TC_BIG_FLONUM:
	/* This must be fixed. */
#include "error: bchdmp does not handle floating alignment."
#else
      case TC_BIG_FLONUM:
	/* Fall through */
#endif
      case_Vector:
	fasdump_normal_setup();
      Move_Vector:
	copy_vector();
	fasdump_normal_end();

      case TC_FUTURE:
	fasdump_normal_setup();
	if (!(Future_Spliceable(Temp)))
	  goto Move_Vector;
	*Scan = Future_Value(Temp);
	Scan -= 1;
	continue;

      default:
	fprintf(stderr,
		"\ndumploop: Bad type code = 0x%02x\n",
		Type_Code(Temp));
	Invalid_Type_Code();
      }
  }
end_dumploop:
  *To_ptr = To;
  *To_Address_ptr = To_Address;
  return true;
}

/* (PRIMITIVE-FASDUMP object-to-dump file-name flag)
   Dump an object into a file so that it can be loaded using
   BINARY-FASLOAD.  A spare heap is required for this operation.  The
   first argument is the object to be dumped.  The second is the
   filename and the third a flag.  The flag, if #!TRUE, means that the
   object is to be dumped for reloading into constant space.  If the
   flag is NIL, it means that it will be reloaded into the heap.  This
   flag is currently ignored.  The primitive returns #!TRUE or NIL
   indicating whether it successfully dumped the object (it can fail
   on an object that is too large).
*/

Built_In_Primitive(Prim_Prim_Fasdump, 3, "PRIMITIVE-FASDUMP", 0x56)
{
  long length, hlength;
  Pointer Prim_Exts, *dumped_object, *exts, *free_buffer;
  Pointer header[FASL_HEADER_LENGTH];
  Primitive_3_Args();

  if (Type_Code(Arg2) != TC_CHARACTER_STRING)
    Primitive_Error(ERR_ARG_2_WRONG_TYPE);
  dump_file_name = Scheme_String_To_C_String(Arg2);
  dump_file = open(dump_file_name, GC_FILE_FLAGS, 0666);
  if (dump_file < 0)
    Primitive_Error(ERR_ARG_2_BAD_RANGE);

  Prim_Exts = Make_Prim_Exts();

  real_gc_file = gc_file;
  gc_file = dump_file;
  saved_free = Free;
  fixup = fixup_buffer_end;
  fixup_count = -1;

#if (GC_DISK_BUFFER_SIZE <= FASL_HEADER_LENGTH)
#include "error in bchdmp.c: FASL_HEADER_LENGTH too large"
#endif

  free_buffer = initialize_free_buffer();
  Free = ((Pointer *) NULL);
  free_buffer += FASL_HEADER_LENGTH;
  *free_buffer++ = Arg1;
  dumped_object = Free;
  Free += 1;
  *free_buffer++ = Prim_Exts;
  exts = Free;
  Free += 1;

  if (!dumploop((initialize_scan_buffer() + FASL_HEADER_LENGTH),
		&free_buffer, &Free))
  {
    fasdump_exit(0);
    PRIMITIVE_RETURN(NIL);
  }
  end_transport();

  length = (Free - dumped_object);
  prepare_dump_header(header, length, dumped_object, dumped_object,
		      0, Constant_Space, exts);
  hlength = (FASL_HEADER_LENGTH * sizeof(Pointer));
  if ((lseek(gc_file, 0, 0) == -1) ||
      (write(gc_file, ((char *) &header[0]), hlength) != hlength))
  {
    fasdump_exit(0);
    PRIMITIVE_RETURN(NIL);
  }
  fasdump_exit((sizeof(Pointer) * length) + hlength);
  PRIMITIVE_RETURN(TRUTH);
}

/* (DUMP-BAND PROCEDURE FILE-NAME)
   Saves all of the heap and pure space on FILE-NAME.  When the
   file is loaded back using BAND_LOAD, PROCEDURE is called with an
   argument of NIL.
*/
Built_In_Primitive(Prim_Band_Dump, 2, "DUMP-BAND", 0xB7)
{
  extern Pointer compiler_utilities;
  Pointer Combination, Ext_Prims;
  long Arg1Type;
  Primitive_2_Args();

  Band_Dump_Permitted();
  Arg1Type = Type_Code(Arg1);
  if ((Arg1Type != TC_CONTROL_POINT) &&
      (Arg1Type != TC_PRIMITIVE) &&
      (Arg1Type != TC_PRIMITIVE_EXTERNAL) &&
      (Arg1Type != TC_EXTENDED_PROCEDURE)) Arg_1_Type(TC_PROCEDURE);
  Arg_2_Type(TC_CHARACTER_STRING);
  if (!Open_Dump_File(Arg2, WRITE_FLAG))
    Primitive_Error(ERR_ARG_2_BAD_RANGE);
  /* Free cannot be saved around this code since Make_Prim_Exts will
     intern the undefined externals and potentially allocate space.
   */
  Ext_Prims = Make_Prim_Exts();
  Combination = Make_Pointer(TC_COMBINATION_1, Free);
  Free[COMB_1_FN] = Arg1;
  Free[COMB_1_ARG_1] = NIL;
  Free += 2;
  *Free++ = Combination;
  *Free++ = compiler_utilities;
  *Free = Make_Pointer(TC_LIST, Free-2);
  Free++;  /* Some compilers are TOO clever about this and increment Free
	      before calculating Free-2! */
  *Free++ = Ext_Prims;
  /* Aligning here confuses some of the counts computed.
     Align_Float(Free);
   */
  Write_File(((long) (Free-Heap_Bottom)), Heap_Bottom, Free-2,
             ((long) (Free_Constant-Constant_Space)),
	     Constant_Space, Free-1);
  fclose(File_Handle);
  PRIMITIVE_RETURN(TRUTH);
}
