/* -*-C-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/hppacach.c,v 1.1 1990/08/08 20:21:12 jinx Exp $

Copyright (c) 1990 Massachusetts Institute of Technology

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

#include <stdio.h>
#include <math.h>

#include <sys/stat.h>
#include <memory.h>

#include <nlist.h>

#include "hppacache.h"

#define true			1
#define false			0
#define boolean			int

#ifdef DEBUG

#define DEBUGGING(stmt) stmt

#else

#define DEBUGGING(stmt)							\
do {									\
}while (0)

#endif

/* File that contains the symbol table for the kernel */

#define KERNEL_FILE		"/hp-ux"

/* File where the kernel image lives */

#define KERNEL_MEMORY_FILE	"/dev/kmem"

#define UTSNAME_SYMBOL		"utsname"
#define PDC_CACHE_SYMBOL	"cache_tlb_parms"

/* File where cached locations (to avoid nlist search) live */

#define LOCATIONS_CACHE_FILE	"/tmp/hppa.cache"

/* Operating system name */

#define SYSNAME			"HP-UX"

static struct utsname sysinfo;
static char **the_argv;

void
io_error (format, fname)
     char *format, *fname;
{
  char errmsg[MAXPATHLEN + 257];
  char errstring[MAXPATHLEN + 257];

  sprintf (&errmsg[0], format, fname);
  sprintf (&errstring[0], "%s: %s", (the_argv[0]), (&errmsg[0]));
  perror (&errstring[0]);
  return;
}

void
io_lose (format, fname)
     char *format, *fname;
{
  io_error (format, fname);
  exit (1);
}

struct kernel_locations
{
  long utsname_location;
  long pdc_cache_location;
};

struct nlist nl[] =
{
  { UTSNAME_SYMBOL },
  { PDC_CACHE_SYMBOL },
  { 0 },
};

int
read_nlist (kloc)
     struct kernel_locations *kloc;
{
  DEBUGGING (printf ("reading nlist...\n"));

  if ((nlist (KERNEL_FILE, nl)) != 0)
  {
    io_error ("nlist: failed on %s", KERNEL_FILE);
    return (-1);
  }

  DEBUGGING (printf ("reading nlist done.\n"));

  kloc->utsname_location = nl[0].n_value;
  kloc->pdc_cache_location = nl[1].n_value;

  DEBUGGING (printf ("utsname location = 0x%x\n", kloc->utsname_location));
  DEBUGGING (printf ("pdc_cache location = 0x%x\n", kloc->pdc_cache_location));

  return (0);
}

void
read_model (pdc_cache)
     struct pdc_cache_dump *pdc_cache;
{
  int fd, read_result;
  static char filename[MAXPATHLEN + 1];

  sprintf ((&filename[0]),
	   MODELS_FILENAME,
	   CACHE_FILENAME_PATH);
  fprintf (stderr, "%s:\n", (the_argv[0]));
  fprintf (stderr,
	   "\tReading cache information from the model-based database (%s).\n",
	   (&filename[0]));
  fd = (open ((&filename[0]), O_RDONLY));
  if (fd < 0)
  {
    io_lose ("read_model: open (%s) failed", (&filename[0]));
  }

  while (true)
  {
    read_result = (read (fd, ((char *) pdc_cache), (sizeof (struct pdc_cache_dump))));
    if (read_result != (sizeof (struct pdc_cache_dump)))
    {
      break;
    }
    if ((strcmp (sysinfo.machine, pdc_cache->hardware)) == 0)
    {
      close (fd);
      fprintf (stderr,
	       "\tFound information for model %s in %s.\n",
	       sysinfo.machine, (&filename[0]));
      return;
    }
  }
  close (fd);
  fprintf (stderr,
	   "\tCould not find information for model %s in %s.\n",
	   sysinfo.machine, (&filename[0]));
  exit (1);
}

void
read_parameters (pdc_cache)
     struct pdc_cache_dump *pdc_cache;
{
  int kmem;

  struct kernel_locations kloc;
  struct utsname kerninfo;

  if ((read_nlist (&kloc)) < 0)
  {
    read_model (pdc_cache);
    return;
  }
  kmem = (open (KERNEL_MEMORY_FILE, O_RDONLY));
  if (kmem < 0)
  {
    io_error ("read_parameters: open (%s) failed", KERNEL_MEMORY_FILE);
    read_model (pdc_cache);
    return;
  }
  else
  {
    if ((lseek (kmem, kloc.utsname_location, 0)) < 0)
    {
      io_lose ("read_parameters: lseek (%s) failed", KERNEL_MEMORY_FILE);
    }

    if ((read (kmem, &kerninfo, (sizeof (kerninfo)))) !=
	(sizeof (kerninfo)))
    {
      io_lose ("read_parameters: read (%s) failed", KERNEL_MEMORY_FILE);
    }

    if ((memcmp (&kerninfo, &sysinfo, (sizeof (sysinfo)))) != 0)
    {
      fprintf (stderr, "read_parameters: uname and %s in %s differ.\n",
	       kloc.utsname_location, KERNEL_MEMORY_FILE);
    }

    strncpy (pdc_cache->hardware, (kerninfo.machine),
	     (sizeof (kerninfo.machine)));

    if ((lseek (kmem, (kloc.pdc_cache_location), 0)) < 0)
    {
      io_lose ("read_parameters: lseek (%s) failed", KERNEL_MEMORY_FILE);
    }

    if  ((read (kmem, pdc_cache->cache_format,
		(sizeof (pdc_cache->cache_format)))) !=
	 (sizeof (pdc_cache->cache_format)))
    {
      io_lose ("read_parameters: read (%s) failed", KERNEL_MEMORY_FILE);
    }

    if ((close (kmem)) < 0)
    {
      io_lose ("read_parameters: close (%s) failed", KERNEL_MEMORY_FILE);
    }
  }
  return;
}

void
print_sel (sel, name, pattern, op)
     unsigned int sel;
     char *name;
     char *pattern;
     char *op;
{
  switch (sel)
  {
    case 0:
      printf ("\n      Both ");
      printf (pattern, "D");
      printf (" and ");
      printf (pattern, "I");
      printf (" must be used to %s it.", op);
      break;

    case 1:
      printf ("\n      Only ");
      printf (pattern, "D");
      printf (" needs to be used to %s it.", op);
      break;

    case 2:
      printf ("\n      Only ");
      printf (pattern, "I");
      printf (" needs to be used to %s it.", op);
      break;

    case 3:
      printf ("\n      Either ");
      printf (pattern, "D");
      printf (" or ");
      printf (pattern, "I");
      printf (" can be used to %s it.", op);
      break;

    default:
      fprintf (stderr, "\n      Bad %s value %d.", name, (sel));
      break;
  }

  return;
}

void
print_cst (cst)
     unsigned int cst;
{
  switch (cst)
  {
    case 0:
      printf ("\n      It does not issue coherent operations.");
      break;

    case 1:
      printf ("\n      It issues coherent operations.");
      break;

    default:
      printf ("\n      It has a reserved cst value %d.", (cst));
      break;
  }
  return;
}

void
print_cache (info, name, write_only_p)
     struct cache_info *info;
     char *name;
     boolean write_only_p;
{
  printf ("\n");

  /* First print the user-readable information as a comment. */

  printf ("    /*\n");
  printf ("      %s-cache information:\n", name);

  printf ("\tsize\t\t%ld bytes (%ld K).\n", info->size, (info->size / 1024));
  printf ("\tconf\t\t0x%08lx\n", info->conf.word);
  printf ("\tbase\t\t0x%lx\n", info->base);
  printf ("\tstride\t\t%ld bytes.\n", info->stride);
  printf ("\tcount\t\t%ld entries.\n", info->count);
  printf ("\tloop\t\t%ld association%s per entry.\n",
	  info->loop, ((info->loop == 1) ? "" : "s"));

  printf ("\tblock size\t%d line%s.\n",
	  info->conf.bits.block,
	  ((info->conf.bits.block == 1) ? "" : "s"));
  printf ("\tline size\t%d (16-byte units).\n", info->conf.bits.line);

  if (write_only_p)
  {
    printf ("      It is a read-only cache.");
  }
  else if (info->conf.bits.wt == 0)
  {
    printf ("      It is a write-to cache.");
  }
  else
  {
    printf ("      It is a write-through cache.");
  }

  print_cst ((info->conf.bits.cst));
  print_sel ((info->conf.bits.fsel), "f-sel", "F%sC", "flush");

  /* Now print the C-readable information. */

  printf ("\n    */\n");
  printf ("    { %ld, 0x%08lx, 0x%lx, %ld, %ld, %ld }",
	  info->size, info->conf.word, info->base,
	  info->stride, info->count, info->loop);

  return;
}

void
print_tlb (info, name)
     struct tlb_info *info;
     char *name;
{
  printf ("\n");

  /* First print the user-readable information as a comment. */

  printf ("    /*\n");
  printf ("      %s-TLB information:\n", name);

  printf ("\tsize\t\t%ld bytes (%ld K).\n",
	  info->size, (info->size / 1024));
  printf ("\tconf\t\t0x%08lx.\n", info->conf.word);
  printf ("\tsp_base\t\t0x%lx.\n", info->sp_base);
  printf ("\tsp_stride\t%ld.\n", info->sp_stride);
  printf ("\tsp_count\t%ld.\n", info->sp_count);
  printf ("\toff_base\t0x%lx.\n", info->off_base);
  printf ("\toff_stride\t%ld.\n", info->off_stride);
  printf ("\toff_count\t%ld.\n", info->off_count);
  printf ("\tloop\t\t%ld association%s per entry.",
	  info->loop, ((info->loop == 1) ? "" : "s"));
  
  print_cst ((info->conf.bits.cst));
  print_sel ((info->conf.bits.psel), "p-sel", "P%sTLB", "purge");

  /* Now print the C-readable information. */

  printf ("\n    */\n");
  printf ("    { %ld, 0x%08lx, 0x%lx, %ld, %ld, 0x%lx, %ld, %ld, %ld }",
	  info->size, info->conf.word,
	  info->sp_base, info->sp_stride, info->sp_count,
	  info->off_base, info->off_stride, info->off_count,
	  info->loop);

  return;  
}

void
print_parameters (pdc_cache)
     struct pdc_cache_dump *pdc_cache;
{
  struct pdc_cache_result *io_arch_format;

  printf ("/* Emacs: Use -*- C -*- mode when editting this file. */\n\n");
  printf ("{\n  /* Cache description for %s, an HP PA %s processor. */\n\n",
	  sysinfo.nodename,
	  sysinfo.machine);

  io_arch_format = ((struct pdc_cache_result *) &(pdc_cache->cache_format));

  printf ("  \"%s\",\n\n  {", pdc_cache->hardware);

  print_cache (&(io_arch_format->I_info), "I", true);
  printf (",");
  print_cache (&(io_arch_format->D_info), "D", false);
  printf (",");

  print_tlb (&(io_arch_format->IT_info), "I");
  printf (",");
  print_tlb (&(io_arch_format->DT_info), "D");
  printf ("\n");

  printf ("  }};\n");
  return;
}

static char dumpname[MAXPATHLEN + 1];

void
dump_parameters (pdc_cache)
     struct pdc_cache_dump *pdc_cache;
{
  int file;

  sprintf (&dumpname[0],
	   CACHE_FILENAME,
	   CACHE_FILENAME_PATH,
	   sysinfo.nodename,
	   sysinfo.machine);

  file = open (&dumpname[0], (O_WRONLY | O_CREAT | O_TRUNC), 0664);
  if (file < 0)
  {
    io_lose ("dump_parameters: open (%s) failed", dumpname);
  }
  if ((write (file, pdc_cache, (sizeof (struct pdc_cache_dump)))) !=
      (sizeof (struct pdc_cache_dump)))
  {
    io_lose ("dump_parameters: write (%s) failed", dumpname);
  }
  if ((close (file)) != 0)
  {
    io_lose ("dump_parameters: close (%s) failed", dumpname);
  }
  return;
}

void
read_stored_parameters (old_pdc_cache)
     struct pdc_cache_dump *old_pdc_cache;
{
  int file;

  sprintf (&dumpname[0],
	   CACHE_FILENAME,
	   CACHE_FILENAME_PATH,
	   sysinfo.nodename,
	   sysinfo.machine);

  file = open (&dumpname[0], (O_RDONLY), 0);
  if (file < 0)
  {
    io_lose ("read_stored_parameters: open (%s) failed", dumpname);
  }
  if ((read (file, old_pdc_cache, (sizeof (struct pdc_cache_dump)))) !=
      (sizeof (struct pdc_cache_dump)))
  {
    io_lose ("read_stored_parameters: read (%s) failed", dumpname);
  }
  if ((close (file)) != 0)
  {
    io_lose ("read_stored_parameters: close (%s) failed", dumpname);
  }
  return;
}

void
verify_parameters (new_pdc_cache, old_pdc_cache)
     struct pdc_cache_dump *new_pdc_cache, *old_pdc_cache;
{
  boolean lose;

  lose = false;
  if ((strcmp (new_pdc_cache->hardware, old_pdc_cache->hardware)) != 0)
  {
    lose = true;
    printf ("Model differs: old = %s; current = %s.\n",
	    new_pdc_cache->hardware, old_pdc_cache->hardware);
  }
  if ((memcmp (&new_pdc_cache->cache_format,
	       &old_pdc_cache->cache_format,
	       (sizeof (struct pdc_cache_result)))) != 0)
  {
    lose = true;
    printf ("The stored cache information is incorrect.\n");
  }
  if (!lose)
  {
    printf ("The stored cache information is correct.\n");
  }
  return;
}

main (argc, argv)
     int argc;
     char **argv;
{
  int
    count,
    uname_result;
  struct pdc_cache_dump
    new_pdc_cache_s, old_pdc_cache_s,
    *new_pdc_cache, *old_pdc_cache;

  the_argv = argv;
  new_pdc_cache = ((struct pdc_cache_dump *) NULL);
  old_pdc_cache = ((struct pdc_cache_dump *) NULL);

  uname_result = (uname (&sysinfo));
  if (uname_result < 0)
  {
    fprintf (stderr, "%s: uname failed.\n", argv[0]);
    exit (1);
  }

  if (argc <= 1)
  {
    if (new_pdc_cache == ((struct pdc_cache_dump *) NULL))
    {
      read_parameters (&new_pdc_cache_s);
      new_pdc_cache = &new_pdc_cache_s;
    }
    dump_parameters (new_pdc_cache);
  }
  else
  {
    for (count = 1; count < argc; count++)
    {
      if ((strcmp ((argv[count]), "-printnew")) == 0)
      {
	if (new_pdc_cache == ((struct pdc_cache_dump *) NULL))
	{
	  read_parameters (&new_pdc_cache_s);
	  new_pdc_cache = &new_pdc_cache_s;
	}
	print_parameters (new_pdc_cache);
      }
      else if ((strcmp ((argv[count]), "-printold")) == 0)
      {
	if (old_pdc_cache == ((struct pdc_cache_dump *) NULL))
	{
	  read_stored_parameters (&old_pdc_cache_s);
	  old_pdc_cache = &old_pdc_cache_s;
	}
	print_parameters (old_pdc_cache);
      }
      else if ((strcmp ((argv[count]), "-dump")) == 0)
      {
	if (new_pdc_cache == ((struct pdc_cache_dump *) NULL))
	{
	  read_parameters (&new_pdc_cache_s);
	  new_pdc_cache = &new_pdc_cache_s;
	}
	dump_parameters (new_pdc_cache);
      }
      else if ((strcmp ((argv[count]), "-verify")) == 0)
      {
	if (new_pdc_cache == ((struct pdc_cache_dump *) NULL))
	{
	  read_parameters (&new_pdc_cache_s);
	  new_pdc_cache = &new_pdc_cache_s;
	}
	if (old_pdc_cache == ((struct pdc_cache_dump *) NULL))
	{
	  read_stored_parameters (&old_pdc_cache_s);
	  old_pdc_cache = &old_pdc_cache_s;
	}
	verify_parameters (new_pdc_cache, old_pdc_cache);
      }
      else
      {
	fprintf (stderr, "usage: %s [-dump] [-verify] [-printnew] [-printold]\n",
		 argv[0]);
	exit (1);
      }
    }
  }
  exit (0);
}
