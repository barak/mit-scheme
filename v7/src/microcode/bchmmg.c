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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchmmg.c,v 9.29 1987/04/21 14:54:50 cph Exp $ */

/* Memory management top level.  Garbage collection to disk.

   The algorithm is basically the same as for the 2 space collector,
   except that new space is on the disk, and there are two windows to
   it (the scan and free buffers).  For information on the 2 space
   collector, read the comments in the replaced files.

   The memory management code is spread over 3 files:
   - bchmmg.c: initialization and top level.  Replaces memmag.c
   - bchgcl.c: main garbage collector loop.   Replaces gcloop.c
   - bchpur.c: constant/pure space hacking.   Replaces purify.c
   - bchdmp.c: object world image dumping.    Replaces fasdump.c

   Problems with this implementation right now:
   - It only works on Unix (or systems which support Unix i/o calls).
   - Purify is not implemented.
   - Fasdump is not implemented.
   - Floating alignment is not implemented.
   - Dumpworld will not work because the file is not closed at dump time.
   - Command line supplied gc files are not locked, so two processes can try
     to share them.
   - Compiled code handling in bchgcl is not generic, may only work for 68k
     family processors.
*/

#include "scheme.h"
#include "primitive.h"
#include "bchgcc.h"
#include <fcntl.h>

/* Exports */

extern void Clear_Memory(), Setup_Memory(), Reset_Memory();

/* 	Memory Allocation, sequential processor,
	garbage collection to disk version:

   ------------------------------------------
   |        GC Buffer Space                 |
   |                                        |
   ------------------------------------------
   |         Control Stack        ||        |
   |                              \/        |
   ------------------------------------------
   |     Constant + Pure Space    /\        |
   |                              ||        |
   ------------------------------------------
   |          Heap Space                    |
   |                                        |
   ------------------------------------------

   Each area has a pointer to its starting address and a pointer to
   the next free cell.  The GC buffer space contains two equal size
   buffers used during the garbage collection process.  Usually one is
   the scan buffer and the other is the free buffer, and they are
   dumped and loaded from disk as necessary.  Sometimes during the
   garbage collection (especially at the beginning and at the end)
   both buffers are identical, since transporting will occur into the
   area being scanned.
*/

/* Local declarations */

static long scan_position, free_position;
static Pointer *gc_disk_buffer_1, *gc_disk_buffer_2;
Pointer *scan_buffer_top, *scan_buffer_bottom, *scan_buffer;
Pointer *free_buffer_top, *free_buffer_bottom, *free_buffer;

/* Hacking the gc file */

extern char *mktemp();

static int gc_file;
static char *gc_file_name;
static char gc_default_file_name[FILE_NAME_LENGTH] = GC_DEFAULT_FILE_NAME;

void
open_gc_file()
{
  int position;
  int flags;

  (void) mktemp(gc_default_file_name);
  flags = (O_RDWR | O_CREAT | O_SYNCIO);

  position = Parse_Option("-gcfile", Saved_argc, Saved_argv, true);
  if ((position != NOT_THERE) &&
      (position != (Saved_argc - 1)))
  {
    gc_file_name = Saved_argv[position + 1];
  }
  else
  {
    gc_file_name = gc_default_file_name;
    flags |= O_EXCL;
  }

  while(true)
  {
    gc_file = open(gc_file_name, flags, GC_FILE_MASK);
    if (gc_file != -1)
      break;
    if (gc_file_name != gc_default_file_name)
    {
      fprintf(stderr,
	      "%s: GC file \"%s\" cannot be opened; ",
	      Saved_argv[0]), gc_file_name;
      gc_file_name = gc_default_file_name;
      fprintf(stderr,
	      "Using \"%s\" instead.\n",
	      gc_file_name);
      flags |= O_EXCL;
      continue;
    }
    fprintf(stderr,
	    "%s: GC file \"%s\" cannot be opened; ",
	    Saved_argv[0]), gc_file_name;
    fprintf(stderr, "Aborting.\n");
    exit(1);
  }
  return;
}

void
close_gc_file()
{
  if (close(gc_file) == -1)
    fprintf(stderr,
	    "%s: Problems closing GC file \"%s\".\n",
	    Saved_argv[0], gc_file_name);
  if (gc_file_name == gc_default_file_name)
    unlink(gc_file_name);
  return;
}

void 
Clear_Memory(Our_Heap_Size, Our_Stack_Size, Our_Constant_Size)
     int Our_Heap_Size, Our_Stack_Size, Our_Constant_Size;
{
  Heap_Top = Heap_Bottom + Our_Heap_Size;
  Set_Mem_Top(Heap_Top - GC_Reserve);
  Free = Heap_Bottom;
  Free_Constant = Constant_Space;
  Set_Pure_Top();
  Initialize_Stack();
  return;
}

void
Setup_Memory(Our_Heap_Size, Our_Stack_Size, Our_Constant_Size)
     int Our_Heap_Size, Our_Stack_Size, Our_Constant_Size;
{
  int Real_Stack_Size;

  Real_Stack_Size = Stack_Allocation_Size(Our_Stack_Size);

  /* Consistency check 1 */
  if (Our_Heap_Size == 0)
  {
    fprintf(stderr, "Configuration won't hold initial data.\n");
    exit(1);
  }

  /* Allocate.
     The two GC buffers are not included in the valid Scheme memory.
  */
  Highest_Allocated_Address = 
    Allocate_Heap_Space(Real_Stack_Size + Our_Heap_Size +
			Our_Constant_Size + (2 * GC_BUFFER_SPACE) +
			HEAP_BUFFER_SPACE);

  /* Consistency check 2 */
  if (Heap == NULL)
  {
    fprintf(stderr, "Not enough memory for this configuration.\n");
    exit(1);
  }

  /* Trim the system buffer space. */

  Highest_Allocated_Address -= (2 * GC_BUFFER_SPACE);
  Heap += HEAP_BUFFER_SPACE;
  Initial_Align_Float(Heap);

  Constant_Space = Heap + Our_Heap_Size;
  gc_disk_buffer_1 = Constant_Space + Our_Constant_Size + Real_Stack_Size;
  gc_disk_buffer_2 = (gc_disk_buffer_1 + GC_BUFFER_SPACE);

  /* Consistency check 3 */
  if (((C_To_Scheme(Highest_Allocated_Address)) & TYPE_CODE_MASK) != 0)
  {
    fprintf(stderr,
	    "Largest address does not fit in datum field of Pointer.\n");
    fprintf(stderr,
	    "Allocate less space or re-compile without Heap_In_Low_Memory.\n");
    exit(1);
  }

  Heap_Bottom = Heap;
  Clear_Memory(Our_Heap_Size, Our_Stack_Size, Our_Constant_Size);

  open_gc_file();
  return;
}

void
Reset_Memory()
{
  close_gc_file();
  return;
}

void
dump_buffer(from, position, nbuffers, name)
     Pointer *from;
     long *position, nbuffers;
     char *name;
{
  long bytes_written;

  if (lseek(gc_file, *position, 0) == -1)
  {
    fprintf(stderr,
	    "\nCould not position GC file to write the %s buffer.\n",
	    name);
    Microcode_Termination(TERM_EXIT);
    /*NOTREACHED*/
  }
  if ((bytes_written = write(gc_file, from, (nbuffers * GC_BUFFER_BYTES))) ==
      -1)
  {
    fprintf(stderr, "\nCould not write out the %s buffer.\n", name);
    Microcode_Termination(TERM_EXIT);
    /*NOTREACHED*/
  }

  *position += bytes_written;
  return;
}

void
load_buffer(position, to, nbytes, name)
     long position;
     Pointer *to;
     long nbytes;
     char *name;
{
  long bytes_read;

  if (lseek(gc_file, position, 0) == -1)
  {
    fprintf(stderr, "\nCould not position GC file to read %s.\n", name);
    Microcode_Termination(TERM_EXIT);
    /*NOTREACHED*/
  }
  if ((bytes_read = read(gc_file, to, nbytes)) != nbytes)
  {
    fprintf(stderr, "\nCould not read into %s.\n", name);
    Microcode_Termination(TERM_EXIT);
    /*NOTREACHED*/
  }
  return;
}

void
reload_scan_buffer()
{
  if (scan_position == free_position)
  {
    scan_buffer_bottom = free_buffer_bottom;
    scan_buffer_top = free_buffer_top;
    scan_buffer = scan_buffer_bottom;
    return;
  }
  scan_buffer_bottom = ((free_buffer_bottom == gc_disk_buffer_1) ?
			gc_disk_buffer_2 :
			gc_disk_buffer_1);
  load_buffer(scan_position, scan_buffer_bottom,
	      GC_BUFFER_BYTES, "the scan buffer");
  scan_buffer_top = scan_buffer_bottom + GC_DISK_BUFFER_SIZE;
  *scan_buffer_top = Make_Pointer(TC_BROKEN_HEART, scan_buffer_top);
  return;
}

void
initialize_scan_buffer()
{
  scan_position = 0;
  reload_scan_buffer();
  scan_buffer = scan_buffer_bottom;
  return;
}

/* This hacks the scan buffer also so that Scan is always below
   scan_buffer_top until the scan buffer is initialized.
*/
void
initialize_free_buffer()
{
  free_position = 0;
  free_buffer_bottom = gc_disk_buffer_1;
  free_buffer_top = free_buffer_bottom + GC_DISK_BUFFER_SIZE;
  free_buffer = free_buffer_bottom;
  scan_position = -1;
  scan_buffer_bottom = gc_disk_buffer_2;
  scan_buffer_top = scan_buffer_bottom + GC_DISK_BUFFER_SIZE;
  return;
}

Pointer *
dump_and_reload_scan_buffer(number_to_skip)
     long number_to_skip;
{
  dump_buffer(scan_buffer_bottom, &scan_position, 1, "scan");
  if (number_to_skip != 0)
    scan_position += (number_to_skip * GC_BUFFER_BYTES);
  reload_scan_buffer();
  return scan_buffer_bottom;
}

Pointer *
dump_and_reset_free_buffer(overflow)
     fast long overflow;
{
  fast Pointer *into, *from;

  from = free_buffer_top;
  if (free_buffer_bottom == scan_buffer_bottom)
  {
    /* No need to dump now, it will be dumped when scan is dumped.
       Does this work?
       We may need to dump the buffer anyway so we can dump the next one.
       It may not be possible to lseek past the end of file.
     */
    free_position += GC_BUFFER_BYTES;
    free_buffer_bottom = ((scan_buffer_bottom == gc_disk_buffer_1) ?
			  gc_disk_buffer_2 :
			  gc_disk_buffer_1);
    free_buffer_top = free_buffer_bottom + GC_DISK_BUFFER_SIZE;
  }
  else
    dump_buffer(free_buffer_bottom, &free_position, 1, "free");

  for (into = free_buffer_bottom; --overflow >= 0; )
    *into++ = *from++;

  /* This only needs to be done when they were the same buffer,
     but it does not hurt.
  */
  *scan_buffer_top = Make_Pointer(TC_BROKEN_HEART, scan_buffer_top);    

  return into;
}

void
dump_free_directly(from, nbuffers)
     Pointer *from;
     long nbuffers;
{
  dump_buffer(from, &free_position, nbuffers, "free");
  return;
}

static long current_buffer_position;

void
initialize_new_space_buffer()
{
  current_buffer_position = -1;
  return;
}

void
flush_new_space_buffer()
{
  if (current_buffer_position == -1)
    return;
  dump_buffer(gc_disk_buffer_1, &current_buffer_position,
	      1, "weak pair buffer");
  current_buffer_position = -1;
  return;
}

Pointer *
guarantee_in_memory(addr)
     Pointer *addr;
{
  long position, offset;

  position = (addr - Heap_Bottom);
  offset = (position % GC_DISK_BUFFER_SIZE);
  position = (position / GC_DISK_BUFFER_SIZE);
  position *= GC_BUFFER_BYTES;
  if (position != current_buffer_position)
  {
    flush_new_space_buffer();
    load_buffer(position, gc_disk_buffer_1,
		GC_BUFFER_BYTES, "the weak pair buffer");
    current_buffer_position = position;
  }
  return &gc_disk_buffer_1[offset];
}

/* For a description of the algorithm, see memmag.c.
   This has been modified only to account for the fact that new space
   is on disk.  Old space is in memory.
*/

Pointer Weak_Chain;

void
Fix_Weak_Chain()
{
  fast Pointer *Old_Weak_Cell, *Scan, Old_Car, Temp, *Old, *Low_Constant;

  initialize_new_space_buffer();
  Low_Constant = Constant_Space;
  while (Weak_Chain != NIL)
  {
    Old_Weak_Cell = Get_Pointer(Weak_Chain);
    Scan = guarantee_in_memory(Get_Pointer(*Old_Weak_Cell++));
    Weak_Chain = *Old_Weak_Cell;
    Old_Car = *Scan;
    Temp = Make_New_Pointer(Type_Code(Weak_Chain), Old_Car);
    Weak_Chain = Make_New_Pointer(TC_NULL, Weak_Chain);

    switch(GC_Type(Temp))
    { case GC_Non_Pointer:
        *Scan = Temp;
	continue;

      case GC_Special:
	if (Type_Code(Temp) != TC_REFERENCE_TRAP)
	{
	  /* No other special type makes sense here. */
	  goto fail;
	}
	if (Datum(Temp) <= TRAP_MAX_IMMEDIATE)
	{
	  *Scan = Temp;
	  continue;
	}
	/* Otherwise, it is a pointer.  Fall through */

      /* Normal pointer types, the broken heart is in the first word.
         Note that most special types are treated normally here.
	 The BH code updates *Scan if the object has been relocated.
	 Otherwise it falls through and we replace it with a full NIL.
	 Eliminating this assignment would keep old data (pl. of datum).
       */
      case GC_Cell:
      case GC_Pair:
      case GC_Triple:
      case GC_Quadruple:
      case GC_Vector:
	/* Old is still a pointer to old space */
	Old = Get_Pointer(Old_Car);
	if (Old >= Low_Constant)
	{
	  *Scan = Temp;
	  continue;
	}
	if (Type_Code(*Old) == TC_BROKEN_HEART)
	{
	  *Scan = Make_New_Pointer(Type_Code(Temp), *Old);
	  continue;
	}
	*Scan = NIL;
	continue;

      case GC_Compiled:
	/* Old is still a pointer to old space */
	Old = Get_Pointer(Old_Car);
	if (Old >= Low_Constant)
	{
	  *Scan = Temp;
	  continue;
	}
	/* Ditto */
	Old = Get_Compiled_Block(Old);
	if (Type_Code(*Old) == TC_BROKEN_HEART)
	{
	  *Scan = Relocate_Compiled(Temp, Get_Pointer(*Old), Old);
	  continue;
	}
	*Scan = NIL;
	continue;

      case GC_Undefined:
      default:			/* Non Marked Headers and Broken Hearts */
      fail:
        fprintf(stderr,
		"\nFix_Weak_Chain: Bad Object: Type = 0x%02x; Datum = %x\n",
		Type_Code(Temp), Datum(Temp));
	Microcode_Termination(TERM_INVALID_TYPE_CODE);
	/*NOTREACHED*/
    }
  }
  flush_new_space_buffer();
  return;
}

void
GC()
{
  Pointer *Root, *Result, *end_of_constant_area,
  	  The_Precious_Objects, *Root2;

  initialize_free_buffer();
  Free = Heap_Bottom;
  Set_Mem_Top(Heap_Top - GC_Reserve);
  Weak_Chain = NIL;

  /* Save the microcode registers so that they can be relocated */
  Terminate_Old_Stacklet();
  Terminate_Constant_Space(end_of_constant_area);

  Root = Free;
  The_Precious_Objects = Get_Fixed_Obj_Slot(Precious_Objects);
  Set_Fixed_Obj_Slot(Precious_Objects, NIL);
  Set_Fixed_Obj_Slot(Lost_Objects_Base, NIL);

  *free_buffer++ = Fixed_Objects;
  *free_buffer++ = Make_Pointer(TC_HUNK3, History);
  *free_buffer++ = Undefined_Externals;
  *free_buffer++ = Get_Current_Stacklet();
  *free_buffer++ = ((Prev_Restore_History_Stacklet == NULL) ?
		    NIL :
		    Make_Pointer(TC_CONTROL_POINT,
				 Prev_Restore_History_Stacklet));
  *free_buffer++ = Current_State_Point;
  *free_buffer++ = Fluid_Bindings;
  Free += (free_buffer - free_buffer_bottom);
  if (free_buffer >= free_buffer_top)
    free_buffer = dump_and_reset_free_buffer(free_buffer - free_buffer_top);

  /* The 4 step GC */
  Result = GCLoop(Constant_Space, &free_buffer, &Free);
  if (Result != end_of_constant_area)
  {
    fprintf(stderr, "\nGC: Constant Scan ended too early.\n");
    Microcode_Termination(TERM_EXIT);
    /*NOTREACHED*/
  }
  initialize_scan_buffer();
  Result = GCLoop(scan_buffer, &free_buffer, &Free);
  if (free_buffer != Result)
  {
    fprintf(stderr, "\nGC-1: Heap Scan ended too early.\n");
    Microcode_Termination(TERM_EXIT);
    /*NOTREACHED*/
  }
  Root2 = Free;
  *free_buffer++ = The_Precious_Objects;
  Free += (free_buffer - Result);
  if (free_buffer >= free_buffer_top)
    free_buffer = dump_and_reset_free_buffer(free_buffer - free_buffer_top);
  Result = GCLoop(Result, &free_buffer, &Free);
  if (free_buffer != Result)
  {
    fprintf(stderr, "\nGC-2: Heap Scan ended too early.\n");
    Microcode_Termination(TERM_EXIT);
    /*NOTREACHED*/
  }
  dump_buffer(scan_buffer_bottom, &scan_position, 1, "scan");
  free_position = scan_position;
  Fix_Weak_Chain();
  load_buffer(0, Heap_Bottom,
	      ((Free - Heap_Bottom) * sizeof(Pointer)),
	      "new space");

  /* Make the microcode registers point to the copies in new-space. */

  Fixed_Objects = *Root++;
  Set_Fixed_Obj_Slot(Precious_Objects, *Root2);
  Set_Fixed_Obj_Slot(Lost_Objects_Base, Make_Pointer(TC_ADDRESS, Root2));

  History = Get_Pointer(*Root++);
  Undefined_Externals = *Root++;
  Set_Current_Stacklet(*Root);
  Root += 1;			/* Set_Current_Stacklet is sometimes a No-Op! */
  if (*Root == NIL)
  {
    Prev_Restore_History_Stacklet = NULL;
    Root += 1;
  }
  else
    Prev_Restore_History_Stacklet = Get_Pointer(*Root++);
  Current_State_Point = *Root++;
  Fluid_Bindings = *Root++;
  Free_Stacklets = NULL;
  return;
}

/* (GARBAGE-COLLECT SLACK)
   Requests a garbage collection leaving the specified amount of slack
   for the top of heap check on the next GC.  The primitive ends by invoking
   the GC daemon if there is one.
*/

Built_In_Primitive(Prim_Garbage_Collect, 1, "GARBAGE-COLLECT", 0x3A)
{
  Pointer GC_Daemon_Proc;
  Primitive_1_Arg();

  Arg_1_Type(TC_FIXNUM);
  if (Free > Heap_Top)
  {
    fprintf(stderr,
	    "\nGC has been delayed too long; You are truly out of room!\n");
    fprintf(stderr,
	    "Free = 0x%x, MemTop = 0x%x, Heap_Top = 0x%x\n",
	    Free, MemTop, Heap_Top);
    Microcode_Termination(TERM_NO_SPACE);
    /*NOTREACHED*/
  }
  GC_Reserve = Get_Integer(Arg1);
  GC();
  IntCode &= ~INT_GC;
  Pop_Primitive_Frame(1);
  GC_Daemon_Proc = Get_Fixed_Obj_Slot(GC_Daemon);
  if (GC_Daemon_Proc == NIL)
  {
   Will_Push(CONTINUATION_SIZE);
    Store_Return(RC_NORMAL_GC_DONE);
    Store_Expression(Make_Unsigned_Fixnum(MemTop - Free));
    Save_Cont();
   Pushed();
    longjmp( *Back_To_Eval, PRIM_POP_RETURN);
    /*NOTREACHED*/
  }
 Will_Push(CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 1));
  Store_Return(RC_NORMAL_GC_DONE);
  Store_Expression(Make_Unsigned_Fixnum(MemTop - Free));
  Save_Cont();
  Push(GC_Daemon_Proc);
  Push(STACK_FRAME_HEADER);
 Pushed();
  longjmp(*Back_To_Eval, PRIM_APPLY);
  /* The following comment is by courtesy of LINT, your friendly sponsor. */
  /*NOTREACHED*/
}
