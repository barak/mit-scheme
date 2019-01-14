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

/* Interface to the mcrypt cryptography library. */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <mcrypt.h>

struct mcrypt_list {
  void* elements;
  int size;
};

extern void scmcrypt_mutex_register (void);
extern const char* scmcrypt_get_ltdlerror (void);
extern void scmcrypt_list_algorithms (struct mcrypt_list* mlist);
extern void scmcrypt_list_modes (struct mcrypt_list* mlist);
extern int scmdecrypt_generic (MCRYPT td, char* plaintext, int start, int end);
extern int scmcrypt_generic (MCRYPT td, char* plaintext, int start, int end);
extern void scmcrypt_enc_get_supported_key_sizes
		(MCRYPT td, struct mcrypt_list* mlist);
extern void scmcrypt_module_get_algo_supported_key_sizes
		(char* algorithm, char* a_directory, struct mcrypt_list* mlist);
