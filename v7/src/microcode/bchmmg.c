/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/bchmmg.c,v 9.63 1991/09/07 22:47:30 jinx Exp $

Copyright (c) 1987-1991 Massachusetts Institute of Technology

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

/* Memory management top level.  Garbage collection to disk.

   The algorithm is basically the same as for the 2 space collector,
   except that new space is on the disk, and there are two windows to
   it (the scan and free buffers).  The two windows are physically the
   same whent hey correspond to the same section of the disk.

   For information on the 2 space collector, read the comments in the
   replaced files.

   The memory management code is spread over 3 files:
   - bchmmg.c: initialization and top level.  Replaces memmag.c
   - bchgcl.c: main garbage collector loop.   Replaces gcloop.c
   - bchpur.c: constant/pure space hacking.   Replaces purify.c
   - bchdmp.c: object world image dumping.    Replaces fasdump.c

   Problems with this implementation right now:
   - Purify kills Scheme if there is not enough space in constant space
     for the new object.
   - It only works on Unix (or systems which support Unix i/o calls).
   - Dumpworld does not work because the file is not closed at dump time or
     reopened at restart time.
   - Command line supplied gc files are not locked, so two processes can try
     to share them and get very confused.
*/

#include "scheme.h"
#include "prims.h"
#include "bchgcc.h"
#include "option.h"
#include "limits.h"
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

/* Exports */
extern void EXFUN (Clear_Memory, (int, int, int));
extern void EXFUN (Setup_Memory, (int, int, int));
extern void EXFUN (Reset_Memory, (void));

char *
DEFUN (error_name, (code),
       int code)
{
  extern int sys_nerr;
  extern char *sys_errlist[];
  static char buf[512];

  if ((code >= 0) && (code <= sys_nerr))
    sprintf (&buf[0], "%d, %s", code, sys_errlist[code]);
  else
    sprintf (&buf[0], "%d, unknown error", code);
  return (&buf[0]);
}

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

int gc_file = -1;

unsigned long
  gc_buffer_size,
  gc_buffer_bytes,
  gc_buffer_shift,
  gc_buffer_mask,
  gc_buffer_byte_mask,
  gc_buffer_byte_shift;

static unsigned long
  gc_extra_buffer_size,
  gc_buffer_overlap_bytes,
  gc_buffer_remainder_bytes;

SCHEME_OBJECT
  * scan_buffer_top,
  * scan_buffer_bottom,
  * free_buffer_top,
  * free_buffer_bottom;

static Boolean can_dump_directly_p;
static long current_disk_position;
static long scan_position, free_position;
static SCHEME_OBJECT * gc_disk_buffer_1, * gc_disk_buffer_2;

static Boolean extension_overlap_p;
static long extension_overlap_length;

static char * gc_file_name;
static char gc_default_file_name[FILE_NAME_LENGTH];

/* Hacking the gc file */

void
DEFUN_VOID (close_gc_file)
{
  if ((gc_file != -1)
      && ((close (gc_file)) == -1))
  {
    fprintf (stderr,
	     "%s: Problems closing GC file \"%s\".\n",
	     scheme_program_name, gc_file_name);
  }
  return;
}

void
DEFUN (open_gc_file, (size), int size)
{
  extern char * EXFUN (mktemp, (char *));
  struct stat file_info;
  int position, flags;
  Boolean exists_p;

  gc_file_name = &gc_default_file_name[0];
  if (option_gc_file[0] == '/')
  {
    strcpy (gc_file_name, option_gc_file);
  }
  else
  {
    position = (strlen (option_gc_directory));
    if ((position == 0)
	|| (option_gc_directory[position - 1] != '/'))
      sprintf (gc_file_name, "%s/%s",
	       option_gc_directory,
	        option_gc_file);
    else
      sprintf (gc_file_name, "%s%s",
	       option_gc_directory,
	       option_gc_file);
  }

  /* mktemp supposedly only clobbers Xs from the end.
     If the string does not end in Xs, it is untouched. 
     This presents a quoting problem, but...
     Well, it seems to clobber the string if there are no Xs.
   */

#if 1
  position = (strlen (option_gc_file));
  if ((position >= 6)
      && ((strncmp ((option_gc_file + (position - 6)), "XXXXXX", 6)) == 0))
#endif
    (void) (mktemp (gc_file_name));

  flags = GC_FILE_FLAGS;

  if ((stat (gc_file_name, &file_info)) == -1)
  {
    exists_p = false;
    can_dump_directly_p = true;
    flags |= O_EXCL;
  }

  else
  {
    /* If it is S_IFCHR, it should determine the IO block
       size and make sure that it will work.
       I don't know how to do that.
       ustat(2) will do that for a mounted file system,
       but obviously, if a raw device file is used,
       there better not be a file system on the file.
       */

    exists_p = true;
    if ((file_info.st_mode & S_IFMT) == S_IFCHR)
    {
      can_dump_directly_p = false;
    }
    else if (((file_info.st_mode & S_IFMT) != S_IFREG)
	     && ((file_info.st_mode & S_IFMT) != S_IFBLK))
    {
      fprintf (stderr,
	       "\
%s: file \"%s\" cannot be used as a GC file (type = 0x%08x).\n",
	       scheme_program_name, gc_file_name,
	       ((int) (file_info.st_mode & S_IFMT)));
      termination_init_error ();
    }
    else
    {
      can_dump_directly_p = true;
    }
  }

  gc_file = (open (gc_file_name, flags, GC_FILE_MASK));
  if (gc_file == -1)
  {
    fprintf (stderr,
	     "%s: GC file \"%s\" cannot be opened (errno = %s); Aborting.\n",
	     scheme_program_name, gc_file_name, (error_name (errno)));
    termination_init_error ();
  }

#ifdef _HPUX
  if (!exists_p)
  {
    extern int EXFUN (prealloc, (int, unsigned int));
    extern long EXFUN (lseek, (int, long, int));

    (void) (prealloc (gc_file, size));

    if ((lseek (gc_file, 0, 0)) == -1)
    {
      fprintf (stderr,
	       "%s: cannot position at start of GC file \"%s\"; Aborting.\n",
	       scheme_program_name, gc_file_name);
      termination_init_error ();
    }
  }
#endif
  if (!exists_p && !option_gc_keep)
  {
    extern int EXFUN (unlink, (const char *));

    (void) (unlink (gc_file_name));
  }
  current_disk_position = 0;
  return;
}

int
DEFUN (next_exponent_of_two, (value), int value)
{
  unsigned int power;
  int exponent;

  if (value < 0)
    return (0);
  
  for (power = 1, exponent = 0;
       power < ((unsigned int) value);
       power = (power << 1), exponent += 1)
    ;
  return (exponent);
}

void
DEFUN (Clear_Memory, (heap_size, stack_size, constant_space_size),
       int heap_size AND
       int stack_size AND
       int constant_space_size)
{
  GC_Reserve = 4500;
  GC_Space_Needed = 0;
  Heap_Top = (Heap_Bottom + heap_size);
  SET_MEMTOP (Heap_Top - GC_Reserve);
  Free = Heap_Bottom;
  Constant_Top = (Constant_Space + constant_space_size);
  Initialize_Stack ();
  Free_Constant = Constant_Space;
  SET_CONSTANT_TOP ();
  return;
}

void
DEFUN (Setup_Memory, (heap_size, stack_size, constant_space_size),
       int heap_size AND
       int stack_size AND
       int constant_space_size)
{
  SCHEME_OBJECT test_value;
  int real_stack_size, fudge_space, exponent;
  unsigned long gc_total_buffer_size;

  /* Consistency check 1 */
  if (heap_size == 0)
  {
    fprintf (stderr,
	     "%s: Configuration won't hold initial data.\n",
	     scheme_program_name);
    termination_init_error ();
  }

  real_stack_size = (Stack_Allocation_Size (stack_size));

  exponent = (next_exponent_of_two (option_gc_window_size));
  gc_buffer_shift = (exponent + 10);		/* log(1024)/log(2) */
  gc_buffer_size = (((unsigned long) 1) << gc_buffer_shift);
  gc_buffer_bytes = (gc_buffer_size * (sizeof (SCHEME_OBJECT)));
  gc_buffer_mask = (gc_buffer_size - 1);
  gc_buffer_byte_mask = (~ (gc_buffer_bytes - 1));
  gc_buffer_byte_shift = (next_exponent_of_two (gc_buffer_bytes));
  if ((((unsigned long) 1) << gc_buffer_byte_shift) != gc_buffer_bytes)
  {
    fprintf (stderr,
	     "%s: gc_buffer_bytes (= %ld) is not a power of 2!\n",
	     scheme_program_name, gc_buffer_bytes);
    termination_init_error ();    
  }

  gc_extra_buffer_size = gc_buffer_size;
  gc_buffer_overlap_bytes = (gc_extra_buffer_size * (sizeof (SCHEME_OBJECT)));
  gc_buffer_remainder_bytes = (gc_buffer_bytes - gc_buffer_overlap_bytes);
  gc_total_buffer_size = (gc_buffer_size + gc_extra_buffer_size);

  /* Allocate in blocks of size gc_buffer_size. */

  fudge_space = (GC_BUFFER_BLOCK (HEAP_BUFFER_SPACE + 1));
  heap_size = (GC_BUFFER_BLOCK (heap_size));
  constant_space_size = (GC_BUFFER_BLOCK (constant_space_size));
  real_stack_size = (GC_BUFFER_BLOCK (real_stack_size));

  /* Allocate.
     The two GC buffers are not included in the valid Scheme memory.
  */

  ALLOCATE_HEAP_SPACE (real_stack_size + heap_size
		       + constant_space_size + (2 * gc_total_buffer_size)
		       + fudge_space);

  /* Consistency check 2 */
  if (Heap == NULL)
  {
    fprintf (stderr, "Not enough memory for this configuration.\n");
    termination_init_error ();
  }

  Heap += HEAP_BUFFER_SPACE;
  Heap = ((SCHEME_OBJECT *) (ALIGN_UP_TO_GC_BUFFER (Heap)));
  Constant_Space = (Heap + heap_size);
  gc_disk_buffer_1 = (Constant_Space + constant_space_size + real_stack_size);
  gc_disk_buffer_2 = (gc_disk_buffer_1 + gc_total_buffer_size);
  Highest_Allocated_Address = (gc_disk_buffer_1 - 1);

  /* Consistency check 3 */
  test_value =
    (MAKE_POINTER_OBJECT (LAST_TYPE_CODE, Highest_Allocated_Address));

  if (((OBJECT_TYPE (test_value)) != LAST_TYPE_CODE) ||
      ((OBJECT_ADDRESS (test_value)) != Highest_Allocated_Address))
  {
    fprintf (stderr,
	     "Largest address does not fit in datum field of object.\n");
    fprintf (stderr,
	     "Allocate less space or re-configure without HEAP_IN_LOW_MEMORY.\n");
    termination_init_error ();
  }
  /* This does not use INITIAL_ALIGN_HEAP because it would
     make Heap point to the previous GC_BUFFER frame.
     INITIAL_ALIGN_HEAP should have its phase changed so that it would
     be a NOP below, and constant space should use it too.
   */     

  ALIGN_FLOAT (Heap);
  ALIGN_FLOAT (Constant_Space);
  heap_size = (Constant_Space - Heap);
  constant_space_size = ((Highest_Allocated_Address - Constant_Space) - real_stack_size);

  Heap_Bottom = Heap;
  Clear_Memory (heap_size, stack_size, constant_space_size);

  open_gc_file (heap_size * (sizeof (SCHEME_OBJECT)));
  return;
}

void
DEFUN_VOID (Reset_Memory)
{
  close_gc_file ();
  return;
}

long
DEFUN (gc_file_operation, (operation, ptr, arg, success, name, errmsg),
       long EXFUN ((*operation), (int, long, long)) AND
       long ptr AND
       long arg AND
       Boolean *success AND
       CONST char * name AND
       CONST char *errmsg)
{
  extern char EXFUN (userio_choose_option,
		     (const char *, const char *, const char **));
  static CONST char * retry_choices [] =
    {
      "K = kill scheme",
      "Q = quit scheme",
      "R = retry the operation",
      "S = sleep for 1 minute and retry the operation",
      "X = exit scheme",
      0
      };
  long result;

  while ((result = ((*operation) (gc_file, ptr, arg)))
	 == -1)
  {
    if (success != ((Boolean *) NULL))
    {
      *success = false;
      return (result);
    }
    fprintf (stderr, errmsg, name, (error_name (errno)));
    switch (userio_choose_option
	      ("Choose one of the following actions:",
	       "Action -> ",
	       retry_choices))
    {
      case '\0':
        /* IO problems, assume everything is scrod. */
        fprintf (stderr, "Problems reading keyboard input -- exitting.\n");
	/* fall through */

      case 'K':
      case 'Q':
      case 'X':
	Microcode_Termination (TERM_EXIT);
	/*NOTREACHED*/

      case 'S':
	sleep (60);
	/* fall through */

      case 'R':
      default:
	break;
    }
  }
  return (result);
}

#define DEFINE_LONG_VERSION(long_name, name, rettype, type1, type2)	\
long									\
DEFUN (long_name, (fd, param1, param2),					\
       int fd AND							\
       long param1 AND							\
       long param2)							\
{									\
  extern rettype EXFUN (name, (int, type1, type2));			\
									\
  return ((long) (name (fd, ((type1) param1), ((type2) param2))));	\
}

DEFINE_LONG_VERSION(long_lseek, lseek, long, long, int)
DEFINE_LONG_VERSION(long_read, read, int, char *, int)
DEFINE_LONG_VERSION(long_write, write, int, char *, int)

void
DEFUN (dump_buffer, (from, position, nbuffers, name, success),
       SCHEME_OBJECT *from AND
       long *position AND
       long nbuffers AND
       char *name AND
       Boolean *success)
{
  long total_bytes_to_write, bytes_to_write, bytes_written;
  char *membuf;

  if ((current_disk_position != *position)
      && ((gc_file_operation (long_lseek, *position, 0,
			      success, name, "\
\nCould not position GC file to write the %s buffer (errno = %s).\n"))
	  == -1))
    return;

  total_bytes_to_write = (nbuffers << gc_buffer_byte_shift);
  bytes_to_write = total_bytes_to_write;
  membuf = ((char *) from);

  while ((bytes_to_write > 0)
	 && ((bytes_written
	      = (gc_file_operation (long_write, ((long) membuf), bytes_to_write,
				    success, name, "\
\nCould not write out the %s buffer (errno = %s).\n")))
	     != bytes_to_write))
  {
    if (bytes_written == -1)
      return;

    /* Short write, continue. */

    membuf += bytes_written;
    bytes_to_write -= bytes_written;
  }

  *position += total_bytes_to_write;
  current_disk_position = *position;
  return;
}

void
DEFUN (load_buffer, (position, to, nbytes, name),
       long position AND
       SCHEME_OBJECT *to AND
       long nbytes AND
       char *name)
{
  long bytes_to_read, bytes_read;
  char *membuf;

  if (current_disk_position != position)
  {
    (void) (gc_file_operation (long_lseek, position, 0,
			       ((Boolean *) NULL), name, "\
Could not position GC file to read %s (errno = %s).\n"));
    current_disk_position = position;
  }

  bytes_to_read = nbytes;
  membuf = ((char *) to);

  while ((bytes_to_read > 0)
	 && ((bytes_read
	      = (gc_file_operation (long_read, ((long) membuf), bytes_to_read,
				    ((Boolean *) NULL), name, "\
\nCould not read into %s (errno = %s).\n")))
	     != bytes_to_read))
  {
    if (bytes_read <= 0)
    {
      fprintf (stderr,
	       "\nInconsistency: data to be read into %s has disappeared!\n",
	       name);
      Microcode_Termination (TERM_EXIT);
    }

    /* Short read, continue. */

    membuf += bytes_read;
    bytes_to_read -= bytes_read;
  }

  current_disk_position += nbytes;
  return;
}

void
DEFUN_VOID (reload_scan_buffer)
{
  if (scan_position == free_position)
  {
    scan_buffer_bottom = free_buffer_bottom;
    scan_buffer_top = free_buffer_top;
    return;
  }
  load_buffer (scan_position, scan_buffer_bottom, gc_buffer_bytes,
	       "the scan buffer");
  *scan_buffer_top = (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
  return;
}

SCHEME_OBJECT *
DEFUN_VOID (initialize_scan_buffer)
{
  scan_position = 0;
  scan_buffer_bottom = ((free_buffer_bottom == gc_disk_buffer_1) ?
			gc_disk_buffer_2 :
			gc_disk_buffer_1);
  scan_buffer_top = (scan_buffer_bottom + gc_buffer_size);
  reload_scan_buffer ();
  return (scan_buffer_bottom);
}

/* This hacks the scan buffer also so that Scan is always below
   scan_buffer_top until the scan buffer is initialized.
   Various parts of the garbage collector depend on scan_buffer_top
   always pointing to a valid buffer.
*/
SCHEME_OBJECT *
DEFUN_VOID (initialize_free_buffer)
{
  free_position = 0;
  free_buffer_bottom = gc_disk_buffer_1;
  free_buffer_top = (free_buffer_bottom + gc_buffer_size);
  extension_overlap_p = false;
  scan_position = -1;
  scan_buffer_bottom = gc_disk_buffer_2;
  scan_buffer_top = (scan_buffer_bottom + gc_buffer_size);
  /* Force first write to do an lseek. */
  current_disk_position = -1;
  return (free_buffer_bottom);
}

void
DEFUN (end_transport, (success), Boolean *success)
{
  dump_buffer (scan_buffer_bottom, &scan_position, 1, "scan", success);
  free_position = scan_position;
  return;
}

/* These utilities are needed when pointers fall accross window boundaries.

   Between both they effectively do a dump_and_reload_scan_buffer, in two
   stages.

   Having bcopy would be nice here.
*/

void
DEFUN (extend_scan_buffer, (to_where, current_free),
       fast char *to_where AND
       SCHEME_OBJECT *current_free)
{
  long new_scan_position;

  new_scan_position = (scan_position + gc_buffer_bytes);

  /* Is there overlap?, ie. is the next bufferfull the one cached
     in the free pointer window?
   */

  if (new_scan_position == free_position)
  {
    fast char *source, *dest;
    long temp;

    extension_overlap_p = true;
    source = ((char *) free_buffer_bottom);
    dest = ((char *) scan_buffer_top);
    extension_overlap_length = (to_where - dest);
    temp = (((char *) current_free) - source);
    if (temp < extension_overlap_length)
    {
      /* This should only happen when Scan and Free are very close. */
      extension_overlap_length = temp;
    }

    while (dest < to_where)
    {
      *dest++ = *source++;
    }
  }
  else
  {
    extension_overlap_p = false;
    load_buffer (new_scan_position, scan_buffer_top,
		 gc_buffer_overlap_bytes, "the scan buffer");
  }
  return;
}

char *
DEFUN (end_scan_buffer_extension, (to_relocate), char *to_relocate)
{
  char *result;

  dump_buffer (scan_buffer_bottom, &scan_position, 1, "scan",
	       ((Boolean *) NULL));
  if (!extension_overlap_p)
  {
    /* There was no overlap */

    fast SCHEME_OBJECT *source, *dest, *limit;

    source = scan_buffer_top;
    dest = scan_buffer_bottom;
    limit = &source[gc_extra_buffer_size];
    result = (((char *) scan_buffer_bottom) +
	      (to_relocate - ((char *) scan_buffer_top)));

    while (source < limit)
    {
      *dest++ = *source++;
    }
    if (gc_buffer_remainder_bytes != 0)
    {
      load_buffer ((scan_position + gc_buffer_overlap_bytes),
		   dest,
		   gc_buffer_remainder_bytes,
		   "the scan buffer");
    }
    *scan_buffer_top =
      (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
  }
  else
  {
    fast char *source, *dest, *limit;

    source = ((char *) scan_buffer_top);
    limit = (source + extension_overlap_length);

    if (scan_position == free_position)
    {
      /* There was overlap, and there still is. */

      dest = ((char *) free_buffer_bottom);
      scan_buffer_bottom = free_buffer_bottom;
      scan_buffer_top = free_buffer_top;
    
    }
    else
    {
      /* There was overlap, but there no longer is. */

      dest = ((char *) scan_buffer_bottom);

      /* The following reads the old overlapped data, but will be aligned.
	 The garbage read will be overwritten with the goodies below.
       */

      load_buffer (scan_position,
		   ((SCHEME_OBJECT *) dest),
		   gc_buffer_bytes,
		   "the scan buffer");
    }

    result = (dest + (to_relocate - source));
    
    while (source < limit)
    {
      *dest++ = *source++;
    }

    if (scan_position != free_position)
      *scan_buffer_top =
	(MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
  }
  extension_overlap_p = false;
  return (result);
}

SCHEME_OBJECT *
DEFUN (dump_and_reload_scan_buffer, (number_to_skip, success),
       long number_to_skip AND
       Boolean *success)
{
  dump_buffer (scan_buffer_bottom, &scan_position, 1, "scan", success);
  if (number_to_skip != 0)
  {
    scan_position += (number_to_skip << gc_buffer_byte_shift);
  }
  reload_scan_buffer ();
  return (scan_buffer_bottom);
}

SCHEME_OBJECT *
DEFUN (dump_and_reset_free_buffer, (overflow, success),
       fast long overflow AND
       Boolean *success)
{
  fast SCHEME_OBJECT *into, *from;

  from = free_buffer_top;
  if (free_buffer_bottom == scan_buffer_bottom)
  {
    /* No need to dump now, it will be dumped when scan is dumped.
       Note that the next buffer may be dumped before this one,
       but there is no problem lseeking past the end of file.
     */
    free_position += gc_buffer_bytes;
    free_buffer_bottom = ((scan_buffer_bottom == gc_disk_buffer_1) ?
			  gc_disk_buffer_2 :
			  gc_disk_buffer_1);
    free_buffer_top = (free_buffer_bottom + gc_buffer_size);
  }
  else
    dump_buffer(free_buffer_bottom, &free_position, 1, "free", success);

  for (into = free_buffer_bottom; --overflow >= 0; )
    *into++ = *from++;

  /* This need only be done when free_buffer_bottom was scan_buffer_bottom,
     but it does not hurt otherwise unless we were in the
     extend_scan_buffer/end_scan_buffer_extension window.
     It must also be done after the for loop above.
   */
  if (!extension_overlap_p)
    *scan_buffer_top =
      (MAKE_POINTER_OBJECT (TC_BROKEN_HEART, scan_buffer_top));
  return (into);
}

void
DEFUN (dump_free_directly, (from, nbuffers, success),
       fast SCHEME_OBJECT *from AND
       fast long nbuffers AND
       Boolean *success)
{
  if (can_dump_directly_p || (ALIGNED_TO_GC_BUFFER_P (from)))
  {
    dump_buffer (from, &free_position, nbuffers, "free", success);
  }
  else
  {
    /* We are writing to a raw (character) device special file,
       and writes must be aligned.
       We don't know the real alignment size, we'll use the GC buffer size.
       This assumes that the free buffer has no valid data, so it can be
       used as scratch.
     */

    while ((--nbuffers) >= 0)
    {
      fast SCHEME_OBJECT *to, *bufend;

      for (to = free_buffer_bottom, bufend = free_buffer_top; to != bufend; )
	*to++ = *from++;

      dump_buffer (free_buffer_bottom, &free_position, 1, "free", success);
    }
  }
  return;
}

static long current_buffer_position;

void
DEFUN_VOID (initialize_new_space_buffer)
{
  current_buffer_position = -1;
  return;
}

void
DEFUN_VOID (flush_new_space_buffer)
{
  if (current_buffer_position == -1)
  {
    return;
  }
  dump_buffer (gc_disk_buffer_1, &current_buffer_position,
	       1, "weak pair buffer", NULL);
  current_buffer_position = -1;
  return;
}

SCHEME_OBJECT *
DEFUN (guarantee_in_memory, (addr), SCHEME_OBJECT *addr)
{
  long position, offset;

  if (addr >= Constant_Space)
  {
    return (addr);
  }

  position = (addr - Heap_Bottom);
  offset = (position & gc_buffer_mask);
  position = (position >> gc_buffer_shift);
  position = (position << gc_buffer_byte_shift);
  if (position != current_buffer_position)
  {
    flush_new_space_buffer ();
    load_buffer (position, gc_disk_buffer_1,
		 gc_buffer_bytes, "the weak pair buffer");
    current_buffer_position = position;
  }
  return (&gc_disk_buffer_1[offset]);
}

/* For a description of the algorithm, see memmag.c.
   This has been modified only to account for the fact that new space
   is on disk.  Old space is in memory.
*/

SCHEME_OBJECT Weak_Chain;

void
DEFUN_VOID (Fix_Weak_Chain)
{
  fast SCHEME_OBJECT *Old_Weak_Cell, *Scan, Old_Car, Temp, *Old, *Low_Constant;

  initialize_new_space_buffer();
  Low_Constant = Constant_Space;
  while (Weak_Chain != EMPTY_LIST)
  {
    Old_Weak_Cell = (OBJECT_ADDRESS (Weak_Chain));
    Scan = (guarantee_in_memory (OBJECT_ADDRESS (*Old_Weak_Cell++)));
    Weak_Chain = *Old_Weak_Cell;
    Old_Car = *Scan;
    Temp = (MAKE_OBJECT_FROM_OBJECTS (Weak_Chain, Old_Car));
    Weak_Chain = (OBJECT_NEW_TYPE (TC_NULL, Weak_Chain));

    switch (GC_Type (Temp))
    { case GC_Non_Pointer:
        *Scan = Temp;
	continue;

      case GC_Special:
	if ((OBJECT_TYPE (Temp)) != TC_REFERENCE_TRAP)
	{
	  /* No other special type makes sense here. */
	  goto fail;
	}
	if ((OBJECT_DATUM (Temp)) <= TRAP_MAX_IMMEDIATE)
	{
	  *Scan = Temp;
	  continue;
	}
	/* Otherwise, it is a pointer.  Fall through */

      /* Normal pointer types, the broken heart is in the first word.
         Note that most special types are treated normally here.
	 The BH code updates *Scan if the object has been relocated.
	 Otherwise it falls through and we replace it with a full SHARP_F.
	 Eliminating this assignment would keep old data (pl. of datum).
       */
      case GC_Cell:
      case GC_Pair:
      case GC_Triple:
      case GC_Quadruple:
      case GC_Vector:
	/* Old is still a pointer to old space */
	Old = (OBJECT_ADDRESS (Old_Car));
	if (Old >= Low_Constant)
	{
	  *Scan = Temp;
	  continue;
	}
	if ((OBJECT_TYPE (*Old)) == TC_BROKEN_HEART)
	{
	  *Scan = (MAKE_OBJECT_FROM_OBJECTS (Temp, *Old));
	  continue;
	}
	*Scan = SHARP_F;
	continue;

      case GC_Compiled:
	/* Old is still a pointer to old space */
	Old = (OBJECT_ADDRESS (Old_Car));
	if (Old >= Low_Constant)
	{
	  *Scan = Temp;
	  continue;
	}
	Compiled_BH (false, { *Scan = Temp; continue; });
	*Scan = SHARP_F;
	continue;

      case GC_Undefined:
	fprintf (stderr,
		 "\nFix_Weak_Chain: Clearing bad object 0x%08lx.\n",
		 Temp);
	*Scan = SHARP_F;
	continue;

      default:			/* Non Marked Headers and Broken Hearts */
      fail:
        fprintf (stderr,
		 "\nFix_Weak_Chain: Bad Object: 0x%08lx.\n",
		 Temp);
	Microcode_Termination (TERM_INVALID_TYPE_CODE);
	/*NOTREACHED*/
    }
  }
  flush_new_space_buffer ();
  return;
}

/* Here is the set up for the full garbage collection:

   - First it makes the constant space and stack into one large area
   by "hiding" the gap between them with a non-marked header.

   - Then it saves away all the relevant microcode registers into new
   space, making this the root for garbage collection.

   - Then it does the actual garbage collection in 4 steps:
     1) Trace constant space.
     2) Trace objects pointed out by the root and constant space.
     3) Trace the precious objects, remembering where consing started.
     4) Update all weak pointers.

   - Load new space to memory.

   - Finally it restores the microcode registers from the copies in
   new space.
*/

void
DEFUN (GC, (initial_weak_chain), SCHEME_OBJECT initial_weak_chain)
{
  SCHEME_OBJECT
    *Root, *Result, *end_of_constant_area,
    The_Precious_Objects, *Root2,
    *free_buffer, *block_start, *initial_free_buffer;

  free_buffer = (initialize_free_buffer ());
  Free = Heap_Bottom;
  block_start = ((SCHEME_OBJECT *) (ALIGN_DOWN_TO_GC_BUFFER (Free)));
  if (block_start != Free)
  {
    /* This assumes that the space between block_start and
       Heap_Bottom is not used at all.  Otherwise it won't be
       correctly preserved.
     */

    free_buffer += (Free - block_start);
  }
  initial_free_buffer = free_buffer;

  SET_MEMTOP (Heap_Top - GC_Reserve);
  Weak_Chain = initial_weak_chain;

  /* Save the microcode registers so that they can be relocated */

  Terminate_Old_Stacklet ();
  SEAL_CONSTANT_SPACE ();
  end_of_constant_area = (CONSTANT_SPACE_SEAL ());
  Root = Free;
  The_Precious_Objects = (Get_Fixed_Obj_Slot (Precious_Objects));
  Set_Fixed_Obj_Slot (Precious_Objects, SHARP_F);
  Set_Fixed_Obj_Slot (Lost_Objects_Base, SHARP_F);

  *free_buffer++ = Fixed_Objects;
  *free_buffer++ = (MAKE_POINTER_OBJECT (UNMARKED_HISTORY_TYPE, History));
  *free_buffer++ = Undefined_Primitives;
  *free_buffer++ = Undefined_Primitives_Arity;
  *free_buffer++ = Get_Current_Stacklet ();
  *free_buffer++ = ((Prev_Restore_History_Stacklet == NULL) ?
		    SHARP_F :
		    (MAKE_POINTER_OBJECT (TC_CONTROL_POINT,
					  Prev_Restore_History_Stacklet)));
  *free_buffer++ = Current_State_Point;
  *free_buffer++ = Fluid_Bindings;
  Free += (free_buffer - initial_free_buffer);
  if (free_buffer >= free_buffer_top)
    free_buffer =
      (dump_and_reset_free_buffer ((free_buffer - free_buffer_top),
				   NULL));

  /* The 4 step GC */

  Result = (GCLoop (Constant_Space, &free_buffer, &Free));
  if (Result != end_of_constant_area)
  {
    fprintf (stderr, "\nGC: Constant Scan ended too early.\n");
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }

  Result = (GCLoop (((initialize_scan_buffer ())
		     + (Heap_Bottom - block_start)),
		    &free_buffer, &Free));
  if (free_buffer != Result)
  {
    fprintf (stderr, "\nGC-1: Heap Scan ended too early.\n");
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }

  Root2 = Free;
  *free_buffer++ = The_Precious_Objects;
  Free += (free_buffer - Result);
  if (free_buffer >= free_buffer_top)
    free_buffer =
      (dump_and_reset_free_buffer ((free_buffer - free_buffer_top), NULL));

  Result = (GCLoop (Result, &free_buffer, &Free));
  if (free_buffer != Result)
  {
    fprintf (stderr, "\nGC-2: Heap Scan ended too early.\n");
    Microcode_Termination (TERM_EXIT);
    /*NOTREACHED*/
  }

  end_transport (NULL);

  Fix_Weak_Chain ();

  /* Load new space into memory. */

  load_buffer (0, block_start,
	       ((GC_BUFFER_BLOCK (Free - block_start))
		* sizeof(SCHEME_OBJECT)),
	       "new space");

  /* Make the microcode registers point to the copies in new-space. */

  Fixed_Objects = *Root++;
  Set_Fixed_Obj_Slot (Precious_Objects, *Root2);
  Set_Fixed_Obj_Slot
    (Lost_Objects_Base, (LONG_TO_UNSIGNED_FIXNUM (ADDRESS_TO_DATUM (Root2))));

  History = (OBJECT_ADDRESS (*Root++));
  Undefined_Primitives = *Root++;
  Undefined_Primitives_Arity = *Root++;

  Set_Current_Stacklet (*Root);
  Root += 1;
  if (*Root == SHARP_F)
  {
    Prev_Restore_History_Stacklet = NULL;
    Root += 1;
  }
  else
  {
    Prev_Restore_History_Stacklet = (OBJECT_ADDRESS (*Root++));
  }
  Current_State_Point = *Root++;
  Fluid_Bindings = *Root++;
  Free_Stacklets = NULL;
  COMPILER_TRANSPORT_END ();
  CLEAR_INTERRUPT (INT_GC);
  return;
}

/* (GARBAGE-COLLECT SLACK)
   Requests a garbage collection leaving the specified amount of slack
   for the top of heap check on the next GC.  The primitive ends by invoking
   the GC daemon if there is one.
*/

DEFINE_PRIMITIVE ("GARBAGE-COLLECT", Prim_garbage_collect, 1, 1, 0)
{
  long new_gc_reserve;
  extern unsigned long gc_counter;
  SCHEME_OBJECT GC_Daemon_Proc;
  PRIMITIVE_HEADER (1);
  PRIMITIVE_CANONICALIZE_CONTEXT ();

  STACK_SANITY_CHECK ("GC");
  new_gc_reserve = (arg_nonnegative_integer (1));
  if (Free > Heap_Top)
    termination_gc_out_of_space ();

  ENTER_CRITICAL_SECTION ("garbage collector");
  gc_counter += 1;
  GC_Reserve = new_gc_reserve;
  GC (EMPTY_LIST);
  POP_PRIMITIVE_FRAME (1);
  GC_Daemon_Proc = (Get_Fixed_Obj_Slot (GC_Daemon));

  RENAME_CRITICAL_SECTION ("garbage collector daemon");
  if (GC_Daemon_Proc == SHARP_F)
  {
   Will_Push (CONTINUATION_SIZE);
    Store_Return (RC_NORMAL_GC_DONE);
    Store_Expression (LONG_TO_UNSIGNED_FIXNUM(MemTop - Free));
    Save_Cont ();
   Pushed ();
    PRIMITIVE_ABORT (PRIM_POP_RETURN);
    /*NOTREACHED*/
  }
 Will_Push (CONTINUATION_SIZE + (STACK_ENV_EXTRA_SLOTS + 1));
  Store_Return (RC_NORMAL_GC_DONE);
  Store_Expression (LONG_TO_UNSIGNED_FIXNUM (MemTop - Free));
  Save_Cont ();
  STACK_PUSH (GC_Daemon_Proc);
  STACK_PUSH (STACK_FRAME_HEADER);
 Pushed ();
  PRIMITIVE_ABORT (PRIM_APPLY);
  /* The following comment is by courtesy of LINT, your friendly sponsor. */
  /*NOTREACHED*/
}
