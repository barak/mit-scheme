#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; UCD property: gc=Pe

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define (char-gc=punctuation:close? char)
  (char-in-set? char char-set:gc=punctuation:close))

(define-deferred char-set:gc=punctuation:close
  (char-set* '(41 93 125 3899 3901 5788 8262 8318 8334 8969 8971 9002 10089 10091 10093 10095 10097 10099 10101 10182 10215 10217 10219 10221 10223 10628 10630 10632 10634 10636 10638 10640 10642 10644 10646 10648 10713 10715 10749 11811 11813 11815 11817 12297 12299 12301 12303 12305 12309 12311 12313 12315 (12318 . 12320) 64830 65048 65078 65080 65082 65084 65086 65088 65090 65092 65096 65114 65116 65118 65289 65341 65373 65376 65379)))
