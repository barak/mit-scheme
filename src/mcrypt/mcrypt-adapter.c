/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

/* Adapters for the mcrypt cryptography library. */

#include <mit-scheme.h>
#include "mcrypt-shim.h"

#ifdef HAVE_PTHREADS
#include <pthread.h>

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

static void
scmcrypt_mutex_lock (void)
{
  int retval = pthread_mutex_lock (&mutex);

  if (retval != 0)
    {
      outf_error (";mcrypt mutex lock failed: %s\n", strerror (retval));
      outf_flush_error ();
    }
}

static void
scmcrypt_mutex_unlock (void)
{
  int retval = pthread_mutex_unlock (&mutex);

  if (retval != 0)
    {
      outf_error (";mcrypt mutex unlock failed: %s\n", strerror (retval));
      outf_flush_error ();
    }
}

static __thread const char * scmcrypt_ltdlerror = NULL;

static void
scmcrypt_set_ltdlerror (const char * errmsg)
{
  scmcrypt_ltdlerror = errmsg;
}
#endif

extern const char *
scmcrypt_get_ltdlerror (void)
{
#ifdef HAVE_PTHREADS
  return scmcrypt_ltdlerror;
#else
  return "consult lt_dlerror";
#endif
}

extern void
scmcrypt_mutex_register (void)
{
#ifdef HAVE_PTHREADS
  int retval = mcrypt_mutex_register (&scmcrypt_mutex_lock,
				      &scmcrypt_mutex_unlock,
				      &scmcrypt_set_ltdlerror,
				      &scmcrypt_get_ltdlerror);
  if (retval != 0)
    {
      outf_error (";mcrypt mutex registration failed\n");
      outf_flush_error ();
    }
#endif
}

extern void
scmcrypt_list_algorithms (struct mcrypt_list * mlist)
{
  mlist->elements = (void*) mcrypt_list_algorithms (NULL, &mlist->size);
}

extern void
scmcrypt_list_modes (struct mcrypt_list * mlist)
{
  mlist->elements = (void*) mcrypt_list_modes (NULL, &mlist->size);
}

extern int
scmdecrypt_generic (MCRYPT td, char *plaintext, int start, int end)
{
  return (mdecrypt_generic (td, plaintext+start, end - start));
}

extern int
scmcrypt_generic (MCRYPT td, char *plaintext, int start, int end)
{
  return (mcrypt_generic (td, plaintext+start, end - start));
}

extern void
scmcrypt_enc_get_supported_key_sizes (MCRYPT td, struct mcrypt_list * mlist)
{
  mlist->elements
    = (void*) mcrypt_enc_get_supported_key_sizes (td, &mlist->size);
}

extern void
scmcrypt_module_get_algo_supported_key_sizes (char* algorithm,
					      char* a_directory,
					      struct mcrypt_list * mlist)
{
  mlist->elements
    = (void*) (mcrypt_module_get_algo_supported_key_sizes
		 (algorithm, 0, &mlist->size));
}
