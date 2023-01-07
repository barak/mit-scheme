/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

#ifndef MITSCHEME_CTASSERT_H
#define MITSCHEME_CTASSERT_H

#if __STDC_VERSION__ >= 201112L

/* If C11 is available, just use _Static_assert.  */
#define CTASSERT(x) _Static_assert((x), #x)

#else

/*
 * If C11 is not available, expand __COUNTER__, or __INCLUDE_LEVEL__
 * and __LINE__, or just __LINE__, with an intermediate preprocessor
 * macro CTASSERT_EXPN, and then use CTASSERT_DECL to paste the
 * expansions together into a unique name.
 *
 * We use this name as a typedef of an array type with a positive
 * length if the assertion is true, and a negative length of the
 * assertion is false, which is invalid and hence triggers a compiler
 * error.
 */
#if defined(__COUNTER__)
#define CTASSERT(x) CTASSERT_EXPN((x), c, __COUNTER__)
#elif defined(__INCLUDE_LEVEL__)
#define CTASSERT(x) CTASSERT_EXPN((x), __INCLUDE_LEVEL__, __LINE__)
#else
/* hope it's unique enough */
#define CTASSERT(x) CTASSERT_EXPN((x), l, __LINE__)
#endif

#define CTASSERT_EXPN(x, a, b) CTASSERT_DECL(x, a, b)
#define CTASSERT_DECL(x, a, b) \
  typedef char mitscheme_ctassert_##a##_##b[(x) ? 1 : -1] UNUSED

#endif

#endif /* MITSCHEME_CTASSERT_H */
