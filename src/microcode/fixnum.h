/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

#ifndef SCM_FIXNUM_H_INCLUDED
#define SCM_FIXNUM_H_INCLUDED 1

#define MAX_BIT_SHIFT DATUM_LENGTH

#define RIGHT_SHIFT_UNSIGNED(source, number)				\
  (((number) > MAX_BIT_SHIFT)						\
   ? 0									\
   : ((((unsigned long) (source)) & DATUM_MASK) >> (number)))

#define RIGHT_SHIFT(source, number)					\
  (((number) > MAX_BIT_SHIFT)						\
   ? 0									\
   : ((source) >> (number)))

#define LEFT_SHIFT(source, number)					\
  (((number) > MAX_BIT_SHIFT)						\
   ? 0									\
   : ((source) << (number)))

#define FIXNUM_LSH(source, number)					\
  (((number) >= 0)							\
   ? (LEFT_SHIFT (source, number))					\
   : (RIGHT_SHIFT_UNSIGNED (source, (- (number)))))

#define FIXNUM_REMAINDER(source1, source2)				\
  (((source2) > 0)							\
   ? (((source1) >= 0)							\
      ? ((source1) % (source2))						\
      : (- ((- (source1)) % (source2))))				\
   : (((source1) >= 0)							\
      ? ((source1) % (- (source2)))					\
      : (- ((- (source1)) % (- (source2))))))

#define FIXNUM_QUOTIENT(source1, source2)				\
  (((source2) > 0)							\
   ? (((source1) >= 0)							\
      ? ((source1) / (source2))						\
      : (- ((- (source1)) / (source2))))				\
   : (((source1) >= 0)							\
      ? (- ((source1) / (- (source2))))					\
      : ((- (source1)) / (- (source2)))))

#endif /* !SCM_FIXNUM_H_INCLUDED */
