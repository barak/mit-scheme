/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/fasload.c,v 9.48 1990/01/21 18:26:16 jinx Exp $

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

/* The "fast loader" which reads in and relocates binary files and then
   interns symbols.  It is called with one argument: the (character
   string) name of a file to load.  It is called as a primitive, and
   returns a single object read in. */

#include "scheme.h"
#include "prims.h"
#include "gccode.h"
#include "trap.h"
#include "load.c"

static long failed_heap_length = -1;

long
read_file_start(name)
     SCHEME_OBJECT name;
{
  long value, heap_length;
  Boolean file_opened;

  if (OBJECT_TYPE (name) != TC_CHARACTER_STRING)
  {
    return (ERR_ARG_1_WRONG_TYPE);
  }

  file_opened = Open_Dump_File(name, OPEN_FLAG);

  if (Per_File)
  {
    Handle_Debug_Flags();
  }

  if (!file_opened)
  {
    return (ERR_ARG_1_BAD_RANGE);
  }

  value = Read_Header();
  if (value != FASL_FILE_FINE)
  {
    Close_Dump_File();
    switch (value)
    {
      /* These may want to be separated further. */
      case FASL_FILE_TOO_SHORT:
      case FASL_FILE_NOT_FASL:
      case FASL_FILE_BAD_MACHINE:
      case FASL_FILE_BAD_VERSION:
      case FASL_FILE_BAD_SUBVERSION:
        return (ERR_FASL_FILE_BAD_DATA);

      case FASL_FILE_BAD_PROCESSOR:
      case FASL_FILE_BAD_INTERFACE:
	return (ERR_FASLOAD_COMPILED_MISMATCH);
    }
  }

  if (Or2(Reloc_Debug, File_Load_Debug))
  {
    print_fasl_information();
  }

  if (!Test_Pure_Space_Top(Free_Constant + Const_Count))
  {
    failed_heap_length = 0;
    Close_Dump_File();
    return (ERR_FASL_FILE_TOO_BIG);
  }

  heap_length = (Heap_Count + Primitive_Table_Size + Primitive_Table_Length);

  if (GC_Check(heap_length))
  {
    if (failed_heap_length == heap_length)
    {
      /* Heuristic check.  It may fail.
	 The GC should be modified to do this right.
       */
      failed_heap_length = -1;
      Close_Dump_File();
      return (ERR_FASL_FILE_TOO_BIG);
    }
    else
    {
      failed_heap_length = heap_length;
      Close_Dump_File();
      Request_GC(heap_length);
      return (PRIM_INTERRUPT);
    }
  }
  failed_heap_length = -1;
  return (PRIM_DONE);
}

SCHEME_OBJECT *
read_file_end()
{
  SCHEME_OBJECT *table;

  if ((Load_Data(Heap_Count, ((char *) Free))) != Heap_Count)
  {
    Close_Dump_File();
    signal_error_from_primitive (ERR_IO_ERROR);
  }
  NORMALIZE_REGION(((char *) Free), Heap_Count);
  Free += Heap_Count;

  if ((Load_Data(Const_Count, ((char *) Free_Constant))) != Const_Count)
  {
    Close_Dump_File();
    signal_error_from_primitive (ERR_IO_ERROR);
  }
  NORMALIZE_REGION(((char *) Free_Constant), Const_Count);
  Free_Constant += Const_Count;

  table = Free;
  if ((Load_Data(Primitive_Table_Size, ((char *) Free))) !=
      Primitive_Table_Size)
  {
    Close_Dump_File();
    signal_error_from_primitive (ERR_IO_ERROR);
  }
  NORMALIZE_REGION(((char *) table), Primitive_Table_Size);
  Free += Primitive_Table_Size;

  if (Close_Dump_File())
  {
    return (table);
  }
  else
  {
    signal_error_from_primitive (ERR_IO_ERROR);
  }
}

/* Statics used by Relocate, below */

relocation_type
  heap_relocation,
  const_relocation,
  stack_relocation;

/* Relocate a pointer as read in from the file.  If the pointer used
   to point into the heap, relocate it into the heap.  If it used to
   be constant area, relocate it to constant area.  Otherwise give an
   error.
*/

#ifdef ENABLE_DEBUGGING_TOOLS

static Boolean Warned = false;

SCHEME_OBJECT *
Relocate(P)
     long P;
{
  SCHEME_OBJECT *Result;

  if ((P >= Heap_Base) && (P < Dumped_Heap_Top))
  {
    Result = ((SCHEME_OBJECT *) (P + heap_relocation));
  }
  else if ((P >= Const_Base) && (P < Dumped_Constant_Top))
  {
    Result = ((SCHEME_OBJECT *) (P + const_relocation));
  }
  else if ((P >= Dumped_Constant_Top) && (P < Dumped_Stack_Top))
  {
    Result = ((SCHEME_OBJECT *) (P + stack_relocation));
  }
  else
  {
    printf("Pointer out of range: 0x%x\n", P, P);
    if (!Warned)
    {
      printf("Heap: %x-%x, Constant: %x-%x, Stack: ?-0x%x\n",
             Heap_Base, Dumped_Heap_Top,
             Const_Base, Dumped_Constant_Top, Dumped_Stack_Top);
      Warned = true;
    }
    Result = ((SCHEME_OBJECT *) 0);
  }
  if (Reloc_Debug)
  {
    printf("0x%06x -> 0x%06x\n", P, Result);
  }
  return (Result);
}

#define Relocate_Into(Loc, P) (Loc) = Relocate(P)

#else /* not ENABLE_DEBUGGING_TOOLS */

#define Relocate_Into(Loc, P)						\
{									\
  if ((P) < Dumped_Heap_Top)						\
  {									\
    (Loc) = ((SCHEME_OBJECT *) ((P) + heap_relocation));		\
  }									\
  else if ((P) < Dumped_Constant_Top)					\
  {									\
    (Loc) = ((SCHEME_OBJECT *) ((P) + const_relocation));		\
  }									\
  else									\
  {									\
    (Loc) = ((SCHEME_OBJECT *) ((P) + stack_relocation));		\
  }									\
}

#ifndef Conditional_Bug

#define Relocate(P)							\
((P < Const_Base) ?							\
 ((SCHEME_OBJECT *) (P + heap_relocation)) :				\
 ((P < Dumped_Constant_Top) ?						\
  ((SCHEME_OBJECT *) (P + const_relocation)) :				\
  ((SCHEME_OBJECT *) (P + stack_relocation))))

#else /* Conditional_Bug */

static SCHEME_OBJECT *Relocate_Temp;

#define Relocate(P)							\
  (Relocate_Into(Relocate_Temp, P), Relocate_Temp)

#endif /* Conditional_Bug */
#endif /* ENABLE_DEBUGGING_TOOLS */

/* Next_Pointer starts by pointing to the beginning of the block of
   memory to be handled.  This loop relocates all pointers in the
   block of memory.
*/

void
Relocate_Block(Scan, Stop_At)
     fast SCHEME_OBJECT *Scan, *Stop_At;
{
  extern SCHEME_OBJECT *load_renumber_table;
  fast SCHEME_OBJECT Temp;
  fast long address;

  if (Reloc_Debug)
  {
    fprintf(stderr,
	    "\nRelocate_Block: block = 0x%x, length = 0x%x, end = 0x%x.\n",
	    Scan, ((Stop_At - Scan) - 1), Stop_At);
  }

  while (Scan < Stop_At)
  {
    Temp = *Scan;
    Switch_by_GC_Type(Temp)
    {
      case TC_BROKEN_HEART:
      case TC_MANIFEST_SPECIAL_NM_VECTOR:
      case_Fasload_Non_Pointer:
        Scan += 1;
	break;

      case TC_PRIMITIVE:
	*Scan++ = (load_renumber_table [PRIMITIVE_NUMBER (Temp)]);
	break;

      case TC_PCOMB0:
	*Scan++ =
	  OBJECT_NEW_TYPE
	    (TC_PCOMB0, (load_renumber_table [PRIMITIVE_NUMBER (Temp)]));
        break;

      case TC_MANIFEST_NM_VECTOR:
        Scan += (OBJECT_DATUM (Temp) + 1);
        break;

      case TC_LINKAGE_SECTION:
      {
	/* Important: The relocation below will not work on machines
	   where Heap_In_Low_Memory is not true.  At this point we
	   don't have the original Memory_Base to find the original
	   address.  Perhaps it should be dumped.
	   This also applies to TC_MANIFEST_CLOSURE.
	   The lines affected are the ones where ADDRESS_TO_DATUM is used.
	 */
	if (READ_LINKAGE_KIND(Temp) != OPERATOR_LINKAGE_KIND)
	{
	  /* Assumes that all others are objects of type TC_QUAD without
	     their type codes.
	   */

	  fast long count;

	  Scan++;
	  for (count = READ_CACHE_LINKAGE_COUNT(Temp);
	       --count >= 0;
	       )
	  {
	    address = ((long) *Scan);
	    address = (ADDRESS_TO_DATUM (address));
	    *Scan++ = ((SCHEME_OBJECT) Relocate(address));
	  }
	  break;
	}
	else
	{
	  fast long count;
	  fast char *word_ptr;
	  SCHEME_OBJECT *end_scan;

	  count = (READ_OPERATOR_LINKAGE_COUNT (Temp));
	  word_ptr = (FIRST_OPERATOR_LINKAGE_ENTRY (Scan));
	  end_scan = (END_OPERATOR_LINKAGE_AREA (Scan, count));

	  while(--count >= 0)
	  {
	    Scan = ((SCHEME_OBJECT *) (word_ptr));
	    word_ptr = (NEXT_LINKAGE_OPERATOR_ENTRY (word_ptr));
	    EXTRACT_OPERATOR_LINKAGE_ADDRESS (address, Scan);
	    address = (ADDRESS_TO_DATUM (address));
	    address = ((long) (Relocate(address)));
	    STORE_OPERATOR_LINKAGE_ADDRESS (address, Scan);
	  }
	  Scan = &end_scan[1];
	  break;
	}
      }

      case TC_MANIFEST_CLOSURE:
      {
	/* See comment about relocation in TC_LINKAGE_SECTION above. */

	fast long count;
	fast char *word_ptr;
	SCHEME_OBJECT *area_end;

	Scan += 1;
	count = (MANIFEST_CLOSURE_COUNT (Scan));
	word_ptr = (FIRST_MANIFEST_CLOSURE_ENTRY (Scan));
	area_end = ((MANIFEST_CLOSURE_END (Scan, count)) + 1);

	while ((--count) >= 0)
	{
	  Scan = ((SCHEME_OBJECT *) (word_ptr));
	  word_ptr = (NEXT_MANIFEST_CLOSURE_ENTRY (word_ptr));
	  EXTRACT_CLOSURE_ENTRY_ADDRESS (address, Scan);
	  address = (ADDRESS_TO_DATUM (address));
	  address = ((long) (Relocate (address)));
	  STORE_CLOSURE_ENTRY_ADDRESS (address, Scan);
	}
	Scan = area_end;
	break;
      }

#ifdef BYTE_INVERSION
      case TC_CHARACTER_STRING:
	String_Inversion(Relocate(OBJECT_DATUM (Temp)));
	goto normal_pointer;
#endif

      case TC_REFERENCE_TRAP:
	if (OBJECT_DATUM (Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  Scan += 1;
	  break;
	}
	/* It is a pointer, fall through. */

      	/* Compiled entry points and stack environments work automagically. */
	/* This should be more strict. */

      default:
#ifdef BYTE_INVERSION
      normal_pointer:
#endif
	address = OBJECT_DATUM (Temp);
	*Scan++ = MAKE_POINTER_OBJECT (OBJECT_TYPE (Temp), Relocate(address));
	break;
      }
  }
  return;
}

Boolean
check_primitive_numbers(table, length)
     fast SCHEME_OBJECT *table;
     fast long length;
{
  fast long count, top;

  top = NUMBER_OF_DEFINED_PRIMITIVES();
  if (length < top)
    top = length;

  for (count = 0; count < top; count += 1)
  {
    if (table[count] != MAKE_PRIMITIVE_OBJECT(0, count))
      return (false);
  }
  /* Is this really correct?  Can't this screw up if there
     were more implemented primitives in the dumping microcode
     than in the loading microcode and they all fell after the
     last implemented primitive in the loading microcode?
   */
  if (length == top)
    return (true);
  for (count = top; count < length; count += 1)
  {
    if (table[count] != MAKE_PRIMITIVE_OBJECT(count, top))
      return (false);
  }
  return (true);
}

extern void get_band_parameters();

void
get_band_parameters(heap_size, const_size)
     long *heap_size, *const_size;
{
  /* This assumes we have just aborted out of a band load. */

  *heap_size = Heap_Count;
  *const_size = Const_Count;
  return;
}

void
Intern_Block(Next_Pointer, Stop_At)
     fast SCHEME_OBJECT *Next_Pointer, *Stop_At;
{
  if (Reloc_Debug)
  {
    printf("Interning a block.\n");
  }

  while (Next_Pointer < Stop_At)
  {
    switch (OBJECT_TYPE (*Next_Pointer))
    {
      case TC_MANIFEST_NM_VECTOR:
        Next_Pointer += (1 + OBJECT_DATUM (*Next_Pointer));
        break;

      case TC_INTERNED_SYMBOL:
	if (OBJECT_TYPE (MEMORY_REF (*Next_Pointer, SYMBOL_GLOBAL_VALUE)) ==
	    TC_BROKEN_HEART)
	{
	  SCHEME_OBJECT old_symbol = (*Next_Pointer);
	  MEMORY_SET (old_symbol, SYMBOL_GLOBAL_VALUE, UNBOUND_OBJECT);
	  {
	    extern SCHEME_OBJECT intern_symbol ();
	    SCHEME_OBJECT new_symbol = (intern_symbol (old_symbol));
	    if (new_symbol != old_symbol)
	      {
		(*Next_Pointer) = new_symbol;
		MEMORY_SET
		  (old_symbol,
		   SYMBOL_NAME,
		   (OBJECT_NEW_TYPE (TC_BROKEN_HEART, new_symbol)));
	      }
	  }
	}
	else if (OBJECT_TYPE (MEMORY_REF (*Next_Pointer, SYMBOL_NAME)) ==
		TC_BROKEN_HEART)
	{
	  *Next_Pointer =
	    (MAKE_OBJECT_FROM_OBJECTS
	     ((*Next_Pointer),
	      (FAST_MEMORY_REF ((*Next_Pointer), SYMBOL_NAME))));
	}
	Next_Pointer += 1;
	break;

      default:
	Next_Pointer += 1;
	break;
    }
  }
  if (Reloc_Debug)
  {
    printf("Done interning block.\n");
  }
  return;
}

/* This should be moved to config.h! */

#ifndef COMPUTE_RELOCATION
#define COMPUTE_RELOCATION(new, old) (((relocation_type) (new)) - (old))
#endif

SCHEME_OBJECT
load_file(from_band_load)
     Boolean from_band_load;
{
  SCHEME_OBJECT
    *Orig_Heap,
    *Constant_End, *Orig_Constant,
    *temp, *primitive_table;

  extern void install_primitive_table();
  extern SCHEME_OBJECT *load_renumber_table;

  /* Read File */

#ifdef ENABLE_DEBUGGING_TOOLS
  Warned = false;
#endif

  load_renumber_table = Free;
  Free += Primitive_Table_Length;
  ALIGN_FLOAT (Free);
  Orig_Heap = Free;
  Orig_Constant = Free_Constant;
  primitive_table = read_file_end();
  Constant_End = Free_Constant;
  heap_relocation = (COMPUTE_RELOCATION (Orig_Heap, Heap_Base));

  /*
    Magic!
    The relocation of compiled code entry points depends on the fact
    that fasdump never dumps the compiler utilities vector (which
    contains entry points used by compiled code to invoke microcode
    provided utilities, like return_to_interpreter).

    If the file is not a band, any pointers into constant space are
    pointers into the compiler utilities vector.  const_relocation is
    computed appropriately.

    Otherwise (the file is a band, and only bands can contain constant
    space segments) the utilities vector stuff is relocated
    automagically: the utilities vector is part of the band.
   */

  if ((!band_p) && (dumped_utilities != SHARP_F))
  {
    extern SCHEME_OBJECT compiler_utilities;

    if (compiler_utilities == SHARP_F)
    {
      signal_error_from_primitive (ERR_FASLOAD_COMPILED_MISMATCH);
    }

    const_relocation =
      (COMPUTE_RELOCATION ((OBJECT_ADDRESS (compiler_utilities)),
			   (OBJECT_DATUM (dumped_utilities))));
    Dumped_Constant_Top =
      (ADDRESS_TO_DATUM
       (MEMORY_LOC (dumped_utilities,
		    (1 + (VECTOR_LENGTH (compiler_utilities))))));
  }
  else
  {
    const_relocation = (COMPUTE_RELOCATION (Orig_Constant, Const_Base));
  }
  stack_relocation = (COMPUTE_RELOCATION (Stack_Top, Dumped_Stack_Top));

#ifdef BYTE_INVERSION
  Setup_For_String_Inversion();
#endif

  /* Setup the primitive table */

  install_primitive_table(primitive_table,
			  Primitive_Table_Length,
			  from_band_load);

  if ((!from_band_load)					||
      (heap_relocation != ((relocation_type) 0))	||
      (const_relocation != ((relocation_type) 0))	||
      (stack_relocation != ((relocation_type) 0))	||
      (!check_primitive_numbers(load_renumber_table,
				Primitive_Table_Length)))
  {
    /* We need to relocate.  Oh well. */
    if (Reloc_Debug)
    {
      printf("heap_relocation = %d = %x; const_relocation = %d = %x\n",
	     heap_relocation, heap_relocation,
	     const_relocation,  const_relocation);
    }

    /*
      Relocate the new data.

      There are no pointers in the primitive table, thus
      there is no need to relocate it.
      */

    Relocate_Block(Orig_Heap, primitive_table);
    Relocate_Block(Orig_Constant, Free_Constant);
  }

#ifdef BYTE_INVERSION
  Finish_String_Inversion();
#endif

  if (!from_band_load)
  {
    /* Again, there are no symbols in the primitive table. */

    Intern_Block(Orig_Heap, primitive_table);
    Intern_Block(Orig_Constant, Constant_End);
  }

  Set_Pure_Top();
  FASLOAD_RELOCATE_HOOK (Orig_Heap, primitive_table, Orig_Constant, Free_Constant);
  Relocate_Into(temp, Dumped_Object);
  return (*temp);
}

/* (BINARY-FASLOAD FILE-NAME)
   Load the contents of FILE-NAME into memory.  The file was
   presumably made by a call to PRIMITIVE-FASDUMP, and may contain
   data for the heap and/or the pure area.  The value returned is
   the object which was dumped.  Typically (but not always) this
   will be a piece of SCode which is then evaluated to perform
   definitions in some environment.
*/

DEFINE_PRIMITIVE ("BINARY-FASLOAD", Prim_binary_fasload, 1, 1, 0)
{
  long result;
  PRIMITIVE_HEADER (1);
  result = (read_file_start (ARG_REF (1)));
  if (band_p)
    signal_error_from_primitive (ERR_FASLOAD_BAND);
  if (result != PRIM_DONE)
    {
      if (result == PRIM_INTERRUPT)
	signal_interrupt_from_primitive ();
      else
	signal_error_from_primitive (result);
    }
  PRIMITIVE_RETURN (load_file (false));
}

/* Band loading. */

static char *reload_band_name = ((char *) NULL);


/* (RELOAD-BAND-NAME)
   Returns the filename (as a Scheme string) from which the runtime system
   was band loaded (load-band'ed ?), or #F if the system was fasl'ed.
*/

DEFINE_PRIMITIVE ("RELOAD-BAND-NAME", Prim_reload_band_name, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN
    ((reload_band_name == NULL)
     ? SHARP_F
     : (char_pointer_to_string (reload_band_name)));
}

/* Utility for load band below. */

extern void compiler_reset_error();

void
compiler_reset_error()
{
  fprintf(stderr,
	  "\ncompiler_restart_error: The band being restored and\n");
  fprintf(stderr,
	  "the compiled code interface in this microcode are inconsistent.\n");
  Microcode_Termination(TERM_COMPILER_DEATH);
}

/* (LOAD-BAND FILE-NAME)
   Restores the heap and pure space from the contents of FILE-NAME,
   which is typically a file created by DUMP-BAND.  The file can,
   however, be any file which can be loaded with BINARY-FASLOAD.
*/

#ifndef start_band_load
#define start_band_load()						\
{									\
  ENTER_CRITICAL_SECTION ("band load");					\
}
#endif

#ifndef end_band_load
#define end_band_load(success, dying)					\
{									\
  if (success || dying)							\
  {									\
    extern Boolean OS_file_close();					\
    int i;								\
									\
    for (i = 0; i < FILE_CHANNELS; i++)					\
    {									\
      if (Channels[i] != NULL)						\
      {									\
	OS_file_close(Channels[i]);					\
	Channels[i] = NULL;						\
      }									\
    }									\
  }									\
  EXIT_CRITICAL_SECTION ({});						\
}
#endif

DEFINE_PRIMITIVE ("LOAD-BAND", Prim_band_load, 1, 1, 0)
{
  extern char * malloc ();
  extern strcpy ();
  extern free ();
  extern void compiler_initialize ();
  extern void compiler_reset ();
  extern SCHEME_OBJECT compiler_utilities;
  static void terminate_band_load ();
  SCHEME_OBJECT
    argument,
    *saved_free,
    *saved_memtop,
    *saved_free_constant,
    *saved_stack_pointer;
  long temp, length;
  SCHEME_OBJECT result, cutl;
  char *band_name;
  Boolean load_file_failed;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();
  argument = (ARG_REF (1));
  saved_free = Free;
  Free = Heap_Bottom;
  saved_memtop = MemTop;
  SET_MEMTOP(Heap_Top);

  start_band_load();

  saved_free_constant = Free_Constant;
  Free_Constant = Constant_Space;
  saved_stack_pointer = Stack_Pointer;
  Stack_Pointer = Highest_Allocated_Address;

  temp = (read_file_start (argument));
  if (temp != PRIM_DONE)
  {
    Free = saved_free;
    SET_MEMTOP(saved_memtop);
    Free_Constant = saved_free_constant;
    Stack_Pointer = saved_stack_pointer;
    end_band_load(false, false);

    if (temp == PRIM_INTERRUPT)
    {
      signal_error_from_primitive (ERR_FASL_FILE_TOO_BIG);
    }
    else
    {
      signal_error_from_primitive (temp);
    }
  }

  /* Point of no return. */

  length = ((STRING_LENGTH (argument)) + 1); /* add 1 for \0 at end */
  band_name = malloc(length);
  if (band_name != ((char *) NULL))
    strcpy (band_name, ((char *) (STRING_LOC (argument, 0))));

  load_file_failed = true;

  UNWIND_PROTECT({
    		   result = load_file(true);
		   load_file_failed = false;
		 },
	         {
		   if (load_file_failed)
		   {
		     terminate_band_load(UNWIND_PROTECT_value,
					 band_name);
		     /*NOTREACHED*/
		   }
		 });

  if (reload_band_name != ((char *) NULL))
  {
    free(reload_band_name);
  }
  reload_band_name = band_name;

  /* Reset implementation state paramenters */

  INITIALIZE_INTERRUPTS();
  Initialize_Stack();
  Set_Pure_Top();
  cutl = MEMORY_REF (result, 1);
  if (cutl != SHARP_F)
  {
    compiler_utilities = cutl;
    compiler_reset(cutl);
  }
  else
  {
    compiler_initialize(true);
  }
  Restore_Fixed_Obj (SHARP_F);
  Fluid_Bindings = EMPTY_LIST;
  Current_State_Point = SHARP_F;

  /* Setup initial program */

  Store_Return (RC_END_OF_COMPUTATION);
  Store_Expression (SHARP_F);
  Save_Cont ();

  Store_Expression(MEMORY_REF (result, 0));
  Store_Env(MAKE_OBJECT (GLOBAL_ENV, GO_TO_GLOBAL));

  /* Clear various interpreter state parameters. */

  Trapping = false;
  Return_Hook_Address = NULL;
  History = Make_Dummy_History();
  Prev_Restore_History_Stacklet = SHARP_F;
  Prev_Restore_History_Offset = 0;

  end_band_load(true, false);
  Band_Load_Hook();

  /* Return in a non-standard way. */

  PRIMITIVE_ABORT(PRIM_DO_EXPRESSION);
  /*NOTREACHED*/
}

static void
terminate_band_load(abort_value, band_name)
     int abort_value;
     char *band_name;
{
  extern char
    * Error_Names[],
    * Abort_Names[];

  if (abort_value > 0)
  {
    fprintf(stderr,
	    "\nload-band: Error %d (%s) past the point of no return.\n",
	    abort_value, Error_Names[abort_value]);
  }
  else
  {
    fprintf(stderr,
	    "\nload-band: Abort %d (%s) past the point of no return.\n",
	    abort_value, Abort_Names[(-abort_value)-1]);
  }

  if (band_name != ((char *) NULL))
  {
    fprintf(stderr, "band-name = \"%s\".\n", band_name);
    free(band_name);
  }
  end_band_load(false, true);
  Microcode_Termination(TERM_DISK_RESTORE);
  /*NOTREACHED*/
}

#ifdef BYTE_INVERSION

#define MAGIC_OFFSET (TC_FIXNUM + 1)

SCHEME_OBJECT String_Chain, Last_String;

Setup_For_String_Inversion()
{
  String_Chain = SHARP_F;
  Last_String = SHARP_F;
  return;
}

Finish_String_Inversion()
{
  if (Byte_Invert_Fasl_Files)
  {
    while (String_Chain != SHARP_F)
    {
      long Count;
      SCHEME_OBJECT Next;

      Count = OBJECT_DATUM (FAST_MEMORY_REF (String_Chain, STRING_HEADER));
      Count = 4*(Count-2)+OBJECT_TYPE (String_Chain)-MAGIC_OFFSET;
      if (Reloc_Debug)
      {
	printf("String at 0x%x: restoring length of %d.\n",
	       OBJECT_ADDRESS (String_Chain), Count);
      }
      Next = (STRING_LENGTH (String_Chain));
      SET_STRING_LENGTH (String_Chain, Count);
      String_Chain = Next;
    }
  }
  return;
}

#define print_char(C) printf(((C < ' ') || (C > '|')) ?	\
			     "\\%03o" : "%c", (C && MAX_CHAR));

String_Inversion(Orig_Pointer)
     SCHEME_OBJECT *Orig_Pointer;
{
  SCHEME_OBJECT *Pointer_Address;
  char *To_Char;
  long Code;

  if (!Byte_Invert_Fasl_Files)
  {
    return;
  }

  Code = OBJECT_TYPE (Orig_Pointer[STRING_LENGTH_INDEX]);
  if (Code == 0)	/* Already reversed? */
  {
    long Count, old_size, new_size, i;

    old_size = OBJECT_DATUM (Orig_Pointer[STRING_HEADER]);
    new_size =
      2 + (((long) (Orig_Pointer[STRING_LENGTH_INDEX]))) / 4;

    if (Reloc_Debug)
    {
      printf("\nString at 0x%x with %d characters",
             Orig_Pointer,
             ((long) (Orig_Pointer[STRING_LENGTH_INDEX])));
    }

    if (old_size != new_size)
    {
      printf("\nWord count changed from %d to %d: ",
             old_size , new_size);
      printf("\nWhich, of course, is impossible!!\n");
      Microcode_Termination(TERM_EXIT);
    }

    Count = ((long) (Orig_Pointer[STRING_LENGTH_INDEX])) % 4;
    if (Count == 0)
    {
      Count = 4;
    }
    if (Last_String == SHARP_F)
    {
      String_Chain = MAKE_POINTER_OBJECT (Count + MAGIC_OFFSET, Orig_Pointer);
    }
    else
    {
      FAST_MEMORY_SET
	(Last_String, STRING_LENGTH_INDEX,
	 MAKE_POINTER_OBJECT (Count + MAGIC_OFFSET, Orig_Pointer));
    }

    Last_String = MAKE_POINTER_OBJECT (TC_NULL, Orig_Pointer);
    Orig_Pointer[STRING_LENGTH_INDEX] = SHARP_F;
    Count = OBJECT_DATUM (Orig_Pointer[STRING_HEADER]) - 1;
    if (Reloc_Debug)
    {
       printf("\nCell count=%d\n", Count);
     }
    Pointer_Address = &(Orig_Pointer[STRING_CHARS]);
    To_Char = (char *) Pointer_Address;
    for (i = 0; i < Count; i++, Pointer_Address++)
    {
      int C1, C2, C3, C4;

      C4 = OBJECT_TYPE (*Pointer_Address) & 0xFF;
      C3 = (((long) *Pointer_Address)>>16) & 0xFF;
      C2 = (((long) *Pointer_Address)>>8) & 0xFF;
      C1 = ((long) *Pointer_Address) & 0xFF;
      if (Reloc_Debug || (old_size != new_size))
      {
	print_char(C1);
        print_char(C2);
        print_char(C3);
        print_char(C4);
      }
      *To_Char++ = C1;
      *To_Char++ = C2;
      *To_Char++ = C3;
      *To_Char++ = C4;
    }
  }
  if (Reloc_Debug)
  {
    printf("\n");
  }
  return;
}
#endif /* BYTE_INVERSION */
