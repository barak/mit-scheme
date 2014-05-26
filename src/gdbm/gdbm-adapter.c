/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

/* Adapters for the GDBM database library. */

#include <mit-scheme.h>
#include "gdbm-shim.h"

extern char *
alloc_gdbm_key (gdbm_args * args, int size)
{
  char * bytes;

  if (size <= args->key_allocation)
    {
      bytes = args->key.dptr;
    }
  else
    {
      if (args->key.dptr != NULL)
	free (args->key.dptr);
      bytes = args->key.dptr = malloc (size);
      args->key_allocation = size;
    }
  args->key.dsize = size;
  return (bytes);
}

extern char *
alloc_gdbm_content (gdbm_args * args, int size)
{
  char * bytes;

  if (size <= args->content_allocation)
    bytes = args->content.dptr;
  else
    {
      if (args->content.dptr != NULL)
	free (args->content.dptr);
      bytes = args->content.dptr = malloc (size);
      args->content_allocation = size;
    }
  args->content.dsize = size;
  return (bytes);
}

extern char *
get_gdbm_version (void)
{
  return (gdbm_version);
}

static void
fatal_error (const char * msg)
{
  outf_error ("\ngdbm: %s\n", msg);
  outf_flush_error ();
  error_external_return ();
}

extern gdbm_args *
do_gdbm_open (char * name, int block_size, int read_write, int mode)
{
  gdbm_args *args = (gdbm_args *) malloc (sizeof (gdbm_args));
  if (!args) return (args);

  args->key.dsize = 0;
  args->key.dptr = NULL;
  args->key_allocation = 0;
  args->content.dsize = 0;
  args->content.dptr = NULL;
  args->content_allocation = 0;
  args->gdbm_errno = 0;
  args->sys_errno = 0;
  args->dbf = gdbm_open (name, block_size, read_write, mode, &fatal_error);

  if (args->dbf == NULL)
    {
      args->gdbm_errno = gdbm_errno;
      args->sys_errno = errno;
    }
  return (args);
}

extern void
do_gdbm_close (gdbm_args * args)
{
  gdbm_close (args->dbf);
  if (args->key.dptr != NULL)
    free (args->key.dptr);
  if (args->content.dptr != NULL)
    free (args->content.dptr);
  free (args);
}

extern int
do_gdbm_store (gdbm_args * args, int flag)
{
  int ret = gdbm_store (args->dbf, args->key, args->content, flag);
  if (ret == -1)
    {
      args->gdbm_errno = gdbm_errno;
      args->sys_errno = errno;
    }
  return (ret);
}

extern void
do_gdbm_fetch (gdbm_args * args)
{
  if (args->content.dptr != NULL)
    free (args->content.dptr);
  args->content = gdbm_fetch (args->dbf, args->key);
  args->content_allocation = args->content.dsize;
}

extern int
do_gdbm_exists (gdbm_args * args)
{
  return (gdbm_exists (args->dbf, args->key));
}

extern int
do_gdbm_delete (gdbm_args * args)
{
  return (gdbm_delete (args->dbf, args->key));
}

extern void
do_gdbm_firstkey (gdbm_args * args)
{
  if (args->key.dptr != NULL)
    free (args->key.dptr);
  args->key = gdbm_firstkey (args->dbf);
  if (args->key.dptr != NULL)
    args->key_allocation = args->key.dsize;
  else
    args->key_allocation = 0;
}

extern int
do_gdbm_nextkey (gdbm_args * args)
{
  datum next = gdbm_nextkey (args->dbf, args->key);
  if (next.dptr == NULL)
    return (1);
  if (args->key.dptr != NULL)
    free (args->key.dptr);
  args->key = next;
  args->key_allocation = next.dsize;
  return (0);
}

extern int
do_gdbm_reorganize (gdbm_args * args)
{
  int ret = gdbm_reorganize (args->dbf);
  if (ret)
    {
      args->gdbm_errno = gdbm_errno;
      args->sys_errno = errno;
    }
  return (ret);
}

extern void
do_gdbm_sync (gdbm_args * args)
{
  gdbm_sync (args->dbf);
}

extern int
do_gdbm_setopt (gdbm_args * args, int option, int value)
{
  int ret = gdbm_setopt (args->dbf, option, &value, sizeof (int));
  if (ret)
    {
      args->gdbm_errno = gdbm_errno;
      args->sys_errno = errno;
    }
  return (ret);
}
