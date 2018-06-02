#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; UCD property: ccc (combining-class)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (ucd-ccc-value char)
  (or (let ((sv (char->integer char)))
        (vector-ref ucd-ccc-table-5 (bytevector-u8-ref ucd-ccc-table-4 (fix:or (fix:lsh (bytevector-u8-ref ucd-ccc-table-3 (fix:or (fix:lsh (bytevector-u8-ref ucd-ccc-table-2 (fix:or (fix:lsh (bytevector-u8-ref ucd-ccc-table-1 (fix:or (fix:lsh (bytevector-u8-ref ucd-ccc-table-0 (fix:lsh sv -16)) 4) (fix:and 15 (fix:lsh sv -12)))) 4) (fix:and 15 (fix:lsh sv -8)))) 4) (fix:and 15 (fix:lsh sv -4)))) 4) (fix:and 15 sv)))))
      0))

(define-deferred ucd-ccc-table-0
  (vector->bytevector '#(0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)))

(define-deferred ucd-ccc-table-1
  (vector->bytevector '#(0 1 2 3 4 4 4 4 4 4 5 4 4 4 4 6 7 8 4 4 4 4 9 4 4 4 4 10 4 11 12 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)))

(define-deferred ucd-ccc-table-2
  (vector->bytevector '#(0 0 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 0 0 15 0 0 0 16 17 18 19 20 21 22 0 0 23 0 0 0 0 0 0 0 0 0 0 0 24 25 0 0 26 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 27 0 28 29 30 31 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 0 33 0 0 34 35 36 0 0 0 0 0 0 37 0 0 0 0 0 38 39 40 41 42 43 44 45 0 0 0 0 46 0 0 0 0 0 0 0 0 0 0 0 0 0 47 48 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 49 0 0 0 0 50 51 0 0 0 0 0 0 0 0 0 0 0 0 0 52 0 0 0 0 0 0 0 53 54 0 0 0 0 0 0)))

(define-deferred ucd-ccc-table-3
  (vector->bytevector
   '#(0 0 0 0 0 0 0 0   0 0 0 0  0  0 0   0 1 2 3 4 5  6  7 0 0 0 0 0 0 0 0 0 0 0  0 0 0  0 0 0  8 0 0  0 0 0 0 0 0  0  0 0 0 0 0 0 0 9 10 11 12 0 0 0 0  13 0 0 14 15 0 16 0 0 0 0 0 17 18 0 0 19 0 20 21 0 0  0 0 0 0 0 0 0 22 23 0 24 25 0 0 26 0 0  0 0 0 0 0 27 28 29 0 0 0 30 31 32 0 0 0 0 0 30 31 0 0 0 0 0 0 30 31 0 0 0 0 0 0 30 31 0 0 0 0 0 0 30 31 0 0 0 0 0 0 0 31 0 0 0 0 0 0 0 31 33 0 0 0  0 0 30 31 0 0 0 0 0 0 0  31 0 0 0 0 0  0 0 34 0 0 0 0  0 0 35 36 0  0 0 0  0 0 37 38 0 0 0 0 39 0 40 0 0 0 41 42 0 0 0 43 0 0 0 0 0 0 44 0 0 0 0 45 0  0 0  0  0 0 0 0 0 0 0 0 46 0 0 0 0  0 0 0 0 0 0 0 47  0 47 0 0 0 0 0 0 0 0 0 48 0 0 0  0   0 0 0 0 0 0 0  0 49 0 0 0 0 0 0   0 0 50 0 0 0 0   0 0 0 0 0 0 0 0 0 51 0 0 0 0 52 53 0  0 0 54 0 0 0 0 0 0 0 55 47 0 56 57 0 0 58 0 0 0 59 60 0 0 0 61 0   0 0 0 0   0 0 0 0 62 63 64 0 0 0 0 0 0 0 0 0 0 0 0 65 66 1   67 0 0 0 0 0 0 0 0 0 0 0 0   0   68  69 70  0 0 0 0 0 0 0 0 0 0   0 0 0 0 71 72 0 0 0 0 0 0   0   73  0 0 0 0 0 0 1 1 0 0 74 0 0 0 0 0 0 75 0
      0 0 0 0 0 0 0 0   0 0 0 71 76 0 77  0 0 0 0 0 72 78 0 0 0 0 0 0 0 0 0 0 0 47 0 1 72 0 0 79 0 0 80 0 0 0 0 0 81 52 0 0 0 0 0 0 0 0 0  0  0  0 0 0 82 83 0 0 78 0  0 0  0 0 0 0 0 0  0  0 0 0  0 31 0  0 84 0 0 0 0 0 0 0 0  0  0 0  0  0 0 0  0 85 0 0 0 0 0 0  0  0  0 0 0 0  0  0  0 0 0 0 0 0  0  0 0 0 0 0 0 0  45 0 0 0 0 0 0 0  0  0 0 0 0 0 0 86 0  0 0 0 0 0 0 0 87 0 0 0 0 0 0 0 0  88 0 0 89 0 0 0  0  0 0 0 0 0 0 90 0  0 0 0 0 78 0 0 73 0 0 0 91 0 0 0  0  92 0 0 93 0 0 0  81 0 0 0 0 94 0 0  0 0 0 0  95 0 0 0 0  0 0 0 0 0 0 96 0 0 0 0 30 31 0 97 98 0 0 0 0 0 0 0 0 0  0 0 0 99 0 0 0 0 0 0 0 100 0 0  0 0 0 0 0 0 0 0 0 0  0 0 73 101 0 0 0 0 0 0 73 0 0  0 0 0 0 0 102 0 0 0  0 0 0 103 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0  0  73 0 0 0  0 0 0 0 0 0 0 0  0  0 0  0  0 0 0  0 0 0 0  0  0 0 0 0  104 0 0 0 105 0 0 0 0 0  0  0  0 0 0 0 0 0 0 0 0 0 0 0 0  0  106 0  0 0 0 0 0 0 0 0 0 0 0 107 108 109 0  110 0 0 0 0 0 0 0 0 0 111 0 0 0 0 0  0  0 0 0 0 0 112 113 114 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0  0
      0 0 0 0 0 0 0 115 0 0 0 0  0  0 116 0 0 0 0 0 0  0  0 0 0 0)))

(define-deferred ucd-ccc-table-4
  (vector->bytevector
   '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1  1  1  1 1 1 1  1 1 1 1 1 1 1 1 1 1 2 3 3 3 3 2 4 3 3 3 3 3  5 5  3  3 3 3 5 5 3 3 3 3 3 3  3 3 3  3 3 6 6 6 6 6 3 3  3  3 1 1 1 1 1 1 1 1 7 1 3 3 3  1  1 1 3 3 0  1 1 1 3 3 3 3 1 2 3 3 1  8 9 9  8 9 9 8 1 1 1 1 1 1 1 1 1 1  1  1  1 0 0 0 1 1 1  1  1 0 0 0 0 0  0  0  0  0 3 1 1 1 1 3 1 1 1 10 3 1  1  1 1  1  1 3 3 3 3 3  3 1 1 3 1 1  10 11 1  12 13 14 15 16 17 18 19 20 21 21 22 23 24 0 25 0 26 27 0 1 3 0 20 0 0 0 0 0 0 0 0 1 1 1 1 1 1  1  1 28 29 30 0 0 0 0  0  0  0  0 0 0  0 0 0 0  0 0 31 32 33 28 29 30 34 35 1 1 3 3 1 1 1 1 1 3 1 1  3 36 0 0  0 0 0 0 0 0 0 0 0  0 0  0  0  0 0 0 0 0 0 1 1 1 1 1 1 1  0 0 1 1 1 1 3 1 0 0 1  1 0 3 1 1 3 0 0 0 37 0  0  0 0  0  0  0  0 0 0 0 0 0 0 1 3  1  1 3 1 1  3  3 3 1 3 3 1 3 1 1 1 3 1 3 1 3 1 3 1 1 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1  1  1 1 1 3 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 1 1 1 1  0 1  1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1  1 1 0 0 0 0 0 0 0 0 0 0 0 3 3 3 0 0 0 0 0 0 0 0 1 1 1 1 1  1 1 1
      1 1 1 1 1 1 0 3 1 1 3 1 1 3 1 1 1 3 3 3 31 32 33 1 1 1 3  1 1 3 3 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 38 0 0  0  0 0 0 0 0 0 0 0 0 0 0  0 0 39 0 0 0 1 3 1 1 0 0  0  0 0 0 0 0 0 0 0 0 0 0 0 0 40 41 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0  0 0 39 0 0 0 0 0 0 0 0 0 0 0 0 0 42 42 39 0 0 0 0 0 0 0  0  0 0 0 0 0 43 43 43 43 0 0 0 0 0 0 0 0 0 0 0  0 44 44 0 0  0  0 0 0 0 0 0  0 0 0 0 0 45 45 45 45 0  0  0  0  0  0  0  0  0  0  0  0  3  3  0 0  0 0  0  0 0 0 0 0  0 3 0 3 0 4 0 0 0 0 0 0 0 46 47 0 48 0  0  0 0 0 47 47 47 47 0 0 47 0 1 1 39 0 1 1  0  0  0  0  0  0  0  0 0 0 0 0 0 0 3 0 0 0 0  0 0  0 0  0 0 0 0 0 0 0 0 38 0 39 39 0  0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 3 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 1  1  1  0 0  0  0  39 0 0 0 0 0 0 0 0 0  0  0 0 0 39 0  0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 1  3 0 0 0 0 0 0 0 0 0 0 0 1 3 0 0 0 0 0 0 0 39 0 0 0 0 0  0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1  1 1 1 1 1 0 0 3 1 1 1 1 1 3 3 3 3 3 3 1 1 3 0 0 0 0 0 0 38 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3  1  1  1 1 1 1  1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 39 39 0 0 0 0 0 0 0 0 0 0 38 0 0 0  0 0 0 0 0 0 0 0 39 39 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0  0 0 0 0 38 0 0 0 0 0 0 0 0 1 1 1 0  6 3 3  3 3 3 1 1 3 3 3 3 1 0 6 6 6  6  6  6 6 0 0 0 0 3  0  0 0 0 0 0 1  0  0  0  1 1 0 0 0 0 0 0 1 1 3  1 1  1  1 1  1  1 3 1 1 9 49 3 5 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  0 0  0 0  0  1 8 3 1 3  1 1 6 6 1 1 1 1 6 6 6 1 1 0  0  0 0  1  0  0 0 6 6  1  3  1  6 6 3  3 3 3 1  0 0 0  0  0  0  0  0  0  0  0 0 0 0 0 0 0 0 0 0 0 0  0 0  0 0  0 0 0 0 1 1 1 0 0  0 0  0  0  0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 39 0 0 0 0 0 0 0 0 0 0  50 11 2 10 51 51 0  0 0 0 0 0 0 0 0 52 52 0 0 0 0  0  0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0  39 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3  3 0 0 0 0  0 39 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 38 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 3 0 0 1 1 0 0 0 0 0 1 1 0  1 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0  0  0 0 0 53 0 1 1 1 1 1 1 1 3 3 3 3 3 3 3 1 1 3 0 0 0 0  0 0  0  0 0 0 0 0 0 0 0 0 0 0  0 0 0  1 1 1 1 1 0 0 0 0  0  0 0 0 0 0 0 0 0 0 0 0 0 0 3  0  1 0 0 0 0  0 0 0 0 1 6 3 0 0 0 0 39 0 0 0  0 0 1 3 0 0 0 0 0 0 0 0 0 0  0  0  0 0 0 0 0 0 39 38 0 0 0 0 0 1  1  1  0  0 0 0 0 0 0 0 0 0 0 0  0 0  0  0 39 39 0 0 0 0 0 0  0 0 0 0 0 39 0  0  0  0  0  0  0  0  0  38 0  0  0  0  0  0  0  0 0  0 39 38 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 38 39 0 0  0  0  0 0 0 0  0  0  0  1 1 1  1 1 1 1  0 0 0  1  1  1  1  1  0  0  0 0 0 0 0 0 0 0 0 0 0 39 0 0  0 38 0 0 0 0 0 0 0 0 0  0 0  39 38 0 0 0 0 0 0 0 0 0 0 0 0 38 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0  39 38 0 0  0  0  0  0 0 0 0 0 0 0 0 0  0  0 0 0 0  39 0 0 0 0 6 6 6 6 6 0 0 0 0 0 0 0 0 0 0 0 1 1  1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0  0  0 0 0 0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 4 4 6 6  6 0 0 0 54 4 4  4 4 4 0 0 0 0 0 0 0 0 3 3 3 3 3  3 3 3 0 0 1 1 1 1 1 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1  1 0 0
      0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1  1  1  0 1 1 1  1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1  1 0  1  1 0 1 1 1 1 1 0 0 0 0  0 3 3  3 3 3 3 3 0 0 0 0  0  0 0 0 0 0 0 0 0 1 1 1 1 1 1  38 0 0 0 0 0)))

(define-deferred ucd-ccc-table-5
  #(#f 230 232 220 216 202 1 240 233 234 222 228 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 30 31 32 27 28 29 33 34 35 36 7 9 84 91 103 107 118 122 129 130 132 214 218 224 8 26 226))
