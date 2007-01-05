/* -*-C-*-

$Id: hppacach.c,v 1.17 2007/01/05 21:19:25 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

#include <stdio.h>
#include <math.h>
#include <sys/stat.h>
#include <memory.h>
#include <nlist.h>
#include <unistd.h>
#include <errno.h>

#include "hppacach.h"

#define true			1
#define false			0
#define boolean			int

#ifdef DEBUG

#define DEBUGGING(stmt) stmt

#else

#define DEBUGGING(stmt) do						\
{									\
} while (0)

#endif

/* File that contains the symbol table for the kernel */

char * kernel_filenames [] =
{
  "/hp-ux",
  "/stand/vmunix",
  0
};

/* File where the kernel image lives */

#define KERNEL_MEMORY_FILE	"/dev/kmem"

#define UTSNAME_SYMBOL		"utsname"
#define PDC_CACHE_SYMBOL	"cache_tlb_parms"

static struct utsname sysinfo;
static char **the_argv;

void
io_error (pname, format, fname)
     char * pname;
     char * format;
     char * fname;
{
  char errmsg[MAXPATHLEN + 257];
  char errstring[MAXPATHLEN + 257];

  sprintf (&errmsg[0], format, fname);
  sprintf (&errstring[0], "%s: %s: %s", (the_argv[0]), pname, (&errmsg[0]));
  perror (&errstring[0]);
}

void
io_lose (pname, format, fname)
     char * pname;
     char * format;
     char * fname;
{
  io_error (pname, format, fname);
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

static char *
choose_kernel_file ()
{
  char ** p = kernel_filenames;
  while (1)
    {
      struct stat s;
      if ((stat ((*p), (&s))) == 0)
	return (*p);
      p += 1;
    }
  fprintf (stderr, "Unable to find kernel.\n");
  fflush (stderr);
  exit (1);
}

void
read_nlist (kloc)
     struct kernel_locations *kloc;
{
  char * kernel_filename = (choose_kernel_file ());
  DEBUGGING (printf ("reading nlist...\n"));

  if ((nlist (kernel_filename, nl)) != 0)
    io_lose ("read_nlist", "failed on both %s and %s", kernel_filename);

  DEBUGGING (printf ("reading nlist done.\n"));

  kloc->utsname_location = nl[0].n_value;
  kloc->pdc_cache_location = nl[1].n_value;

  DEBUGGING (printf ("utsname location = 0x%x\n", kloc->utsname_location));
  DEBUGGING (printf ("pdc_cache location = 0x%x\n", kloc->pdc_cache_location));
}

void
read_parameters (pdc_cache)
     struct pdc_cache_dump *pdc_cache;
{
  struct kernel_locations kloc;
  struct utsname kerninfo;
  int kmem = (open (KERNEL_MEMORY_FILE, O_RDONLY));
  if (kmem < 0)
    io_lose ("read_parameters", "open (%s) failed", KERNEL_MEMORY_FILE);
  read_nlist (&kloc);
  if ((lseek (kmem, kloc.utsname_location, SEEK_SET)) < 0)
    io_lose ("read_parameters", "lseek (%s) failed", KERNEL_MEMORY_FILE);
  if ((read (kmem, (&kerninfo), (sizeof (kerninfo)))) !=
      (sizeof (kerninfo)))
    io_lose ("read_parameters", "read (%s) failed", KERNEL_MEMORY_FILE);
  if ((memcmp ((&kerninfo), (&sysinfo), (sizeof (sysinfo)))) != 0)
    fprintf (stderr, "read_parameters: uname and %s in %s differ.\n",
	     kloc.utsname_location, KERNEL_MEMORY_FILE);
  strncpy (pdc_cache->hardware, (kerninfo.machine),
	   (sizeof (kerninfo.machine)));
  if ((lseek (kmem, (kloc.pdc_cache_location), SEEK_SET)) < 0)
    io_lose ("read_parameters", "lseek (%s) failed", KERNEL_MEMORY_FILE);
  if ((read (kmem, &pdc_cache->cache_format,
	     (sizeof (pdc_cache->cache_format)))) !=
      (sizeof (pdc_cache->cache_format)))
    io_lose ("read_parameters", "read (%s) failed", KERNEL_MEMORY_FILE);
  if ((close (kmem)) < 0)
    io_lose ("read_parameters", "close (%s) failed", KERNEL_MEMORY_FILE);
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
      printf (" must be used to %s.", op);
      break;

    case 1:
      printf ("\n      Only ");
      printf (pattern, "D");
      printf (" needs to be used to %s.", op);
      break;

    case 2:
      printf ("\n      Only ");
      printf (pattern, "I");
      printf (" needs to be used to %s.", op);
      break;

    case 3:
      printf ("\n      Either ");
      printf (pattern, "D");
      printf (" or ");
      printf (pattern, "I");
      printf (" can be used to %s.", op);
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

  printf ("\tsize\t\t%ld entries (%ld K).\n",
	  info->size, (info->size / 1024));
  printf ("\tconf\t\t0x%08lx\n", info->conf.word);
  printf ("\tsp_base\t\t0x%lx\n", info->sp_base);
  printf ("\tsp_stride\t%ld\n", info->sp_stride);
  printf ("\tsp_count\t%ld\n", info->sp_count);
  printf ("\toff_base\t0x%lx\n", info->off_base);
  printf ("\toff_stride\t%ld\n", info->off_stride);
  printf ("\toff_count\t%ld\n", info->off_count);
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
print_parameters (pdc_cache, node_p)
     struct pdc_cache_dump *pdc_cache;
     int node_p;
{
  struct pdc_cache_result *io_arch_format;

  if (node_p)
  {
    printf ("/* Emacs: Use -*- C -*- mode when editting this file. */\n\n");
    printf ("{\n  /* Cache description for %s, an HP PA %s processor. */\n\n",
	    sysinfo.nodename,
	    sysinfo.machine);
  }
  else
  {
    printf ("{\n");
  }

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

int
search_pdc_database (fd, pdc_cache, filename)
     int fd;
     struct pdc_cache_dump * pdc_cache;
     char * filename;
{
  while (1)
    {
      int scr =
	(read (fd, ((char *) pdc_cache), (sizeof (struct pdc_cache_dump))));
      if (scr < 0)
	io_lose ("search_pdc_database", "read (%s) failed", filename);
      if (scr != (sizeof (struct pdc_cache_dump)))
	{
	  if (scr == 0)
	    return (0);
	  fprintf (stderr, "%s: %s: incomplete read (%s)\n",
		   (the_argv[0]), "search_pdc_database", filename);
	  fflush (stderr);
	  exit (1);
	}
      if ((strcmp ((sysinfo . machine), (pdc_cache -> hardware))) == 0)
	return (1);
    }
}

#define MODE_ADD	0
#define MODE_REPLACE	1
#define MODE_PRINT	2

void
update_pdc_database (mode, pdc_cache, filename)
     int mode;
     struct pdc_cache_dump * pdc_cache;
     char * filename;
{
  int write_p = 1;
  int fd = (open (filename, (O_RDWR | O_CREAT), 0666));
  if (fd < 0)
  {
    if (errno != EACCES)
      io_lose ("update_pdc_database", "open (%s) failed", filename);
    fd = (open (filename, O_RDONLY));
    if (fd < 0)
      io_lose ("update_pdc_database", "open (%s) failed", filename);
    else
    {
      write_p = 0;
      if (mode != MODE_PRINT)
	fprintf (stderr, "Data base \"%s\" is write-protected.\n", filename);
    }
  }
  if (! (search_pdc_database (fd, pdc_cache, filename)))
    {
      read_parameters (pdc_cache);
      if (!write_p && (mode != MODE_PRINT))
	printf ("Could not write information to data base.\n");
      else
      {
	int scr =
	  (write (fd, ((char *) pdc_cache), (sizeof (struct pdc_cache_dump))));
	if (scr < 0)
	  io_lose ("update_pdc_database", "write (%s) failed", filename);
	if (scr != (sizeof (struct pdc_cache_dump)))
	  {
	    fprintf (stderr, "%s: %s: incomplete write (%s)\n",
		     (the_argv[0]), "update_pdc_database", filename);
	    fflush (stderr);
	    exit (1);
	  }
      }
    }
  else
  {
    struct pdc_cache_dump new_cache_s, * new_cache;

    new_cache = & new_cache_s;
    read_parameters (new_cache);
    if ((memcmp (new_cache, pdc_cache, (sizeof (struct pdc_cache_dump))))
	== 0)
    {
      if (mode != MODE_PRINT)
	printf ("Correct information for model %s is present in data base.\n",
		&new_cache->hardware[0]);
    }
    else
    {
      printf ("Data base contains different information for model %s.\n",
	      &new_cache->hardware[0]);
      switch (mode)
      {
	case MODE_REPLACE:
        {
	  if (write_p)
	  {
	    printf ("Keeping the new information.\n");
	    if ((lseek (fd, (- (sizeof (struct pdc_cache_dump))), SEEK_CUR))
		== -1)
	      io_lose ("update_pdc_database", "lseek (%s) failed", filename);
	    if ((write (fd, new_cache, (sizeof (struct pdc_cache_dump))))
		!= (sizeof (struct pdc_cache_dump)))
	      io_lose ("update_pdc_database", "write (%s) failed", filename);
	    break;
	  }
	}

	case MODE_ADD:
        {
	  printf ("Keeping the old information.\n");
	  break;
	}

	case MODE_PRINT:
        {
	  printf ("New information:\n");
	  print_parameters (new_cache, 1);
	  printf ("\n\nOld information:\n");
	}
	default:
	  fprintf (stderr, "%s error.  Unknown mode %d.\n", the_argv[0], mode);
      }
    }
  }
  if ((close (fd)) < 0)
    io_lose ("update_pdc_database", "close (%s) failed", filename);
  return;
}

void
print_pdc_database (filename)
     char * filename;
{
  struct pdc_cache_dump pdc_cache_s, *pdc_cache;
  int fd = (open (filename, (O_RDONLY), 0666));
  int first;

  if (fd < 0)
    io_lose ("print_pdc_database", "open (%s) failed", filename);

  pdc_cache = &pdc_cache_s;
  first = 1;
  while (1)
  {
    int scr =
      (read (fd, ((char *) pdc_cache), (sizeof (struct pdc_cache_dump))));
    if (scr < 0)
      io_lose ("print_pdc_database", "read (%s) failed", filename);
    if (scr != (sizeof (struct pdc_cache_dump)))
    {
      if (scr == 0)
	break;
      fprintf (stderr, "%s: %s: incomplete read (%s)\n",
	       (the_argv[0]), "print_pdc_database", filename);
      fflush (stderr);
      exit (1);
    }

    if (first == 0)
    {
      putchar ('\f');
      putchar ('\n');
    }
    else
    {
      first = 0;
    }
    print_parameters (pdc_cache, 0);
  }

  if ((close (fd)) < 0)
    io_lose ("print_pdc_database", "close (%s) failed", filename);
}

void
read_stored_parameters (pdc_cache, filename)
     struct pdc_cache_dump * pdc_cache;
     char * filename;
{
  int fd = (open (filename, (O_RDONLY), 0));
  if (fd < 0)
    io_lose ("read_stored_parameters", "open (%s) failed", filename);
  if (! (search_pdc_database (fd, pdc_cache, filename)))
    {
      fprintf (stderr, "%s: %s: unable to find entry in models database\n",
	       (the_argv[0]), "read_stored_parameters");
      fflush (stderr);
      exit (1);
    }
  if ((close (fd)) < 0)
    io_lose ("read_stored_parameters", "close (%s) failed", filename);
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

void
usage ()
{
  fprintf (stderr, "usage: one of:\n");
  fprintf (stderr, " %s -add FILENAME\n", (the_argv[0]));
  fprintf (stderr, " %s -replace FILENAME\n", (the_argv[0]));
  fprintf (stderr, " %s -verify FILENAME\n", (the_argv[0]));
  fprintf (stderr, " %s -print FILENAME\n", (the_argv[0]));
  fprintf (stderr, " %s -printall FILENAME\n", (the_argv[0]));
  fflush (stderr);
  exit (1);
}

void
main (argc, argv)
     int argc;
     char **argv;
{
  the_argv = argv;
  if ((uname (&sysinfo)) < 0)
    io_lose ("main", "uname failed", 0);
  if (argc != 3)
    usage ();
  {
    char * keyword = (argv[1]);
    char * filename = (argv[2]);
    if ((strcmp (keyword, "-add")) == 0)
      {
	struct pdc_cache_dump pdc_cache;
	update_pdc_database (MODE_ADD, (&pdc_cache), filename);
      }
    else if ((strcmp (keyword, "-replace")) == 0)
      {
	struct pdc_cache_dump pdc_cache;
	update_pdc_database (MODE_REPLACE, (&pdc_cache), filename);
      }
    else if ((strcmp (keyword, "-print")) == 0)
      {
	struct pdc_cache_dump pdc_cache;
	update_pdc_database (MODE_PRINT, (&pdc_cache), filename);
	print_parameters (&pdc_cache, 1);
      }
    else if ((strcmp (keyword, "-printall")) == 0)
      {
	print_pdc_database (filename);
      }
    else if ((strcmp (keyword, "-verify")) == 0)
      {
	struct pdc_cache_dump old_pdc_cache;
	struct pdc_cache_dump new_pdc_cache;
	read_stored_parameters ((&old_pdc_cache), filename);
	read_parameters (&new_pdc_cache);
	verify_parameters ((&new_pdc_cache), (&old_pdc_cache));
      }
    else
      usage ();
  }
  exit (0);
}
