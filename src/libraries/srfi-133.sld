#| -*-Scheme-*-

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

|#

;;;; SRFI 133: Vector Library

(define-library (srfi 133)
  (import (scheme base)
	  (scheme cxr)
	  (only (srfi 8)
		receive)
	  (only (mit legacy runtime)
		error:bad-range-argument
		fix:+
		fix:-
		fix:<
		fix:<=
		fix:=
		fix:>
		fix:>=
		fix:end-index
		fix:min
		fix:quotient
		fix:start-index
		guarantee
		index-fixnum?
		unspecific))
  (export reverse-list->vector
	  reverse-vector->list
	  vector-any
	  vector-append-subvectors
	  vector-cumulate
	  vector-empty?
	  vector-every
	  vector-fold
	  vector-fold-right
	  vector-index
	  vector-index-right
	  vector-map!
	  vector-partition
	  vector-reverse!
	  vector-reverse-copy
	  vector-skip
	  vector-skip-right
	  vector-swap!
	  vector-unfold
	  vector-unfold!
	  vector-unfold-right
	  vector-unfold-right!
	  vector=
          vector-binary-search
          vector-concatenate
          vector-count
          vector-reverse-copy!)
  (include "srfi-133-impl.scm"))