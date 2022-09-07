#| -*-Scheme-*-

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

|#

;;;; UCD property: nt (numeric-type)

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define (ucd-nt-value char)
  (let ((sv (char->integer char)))
    (vector-ref ucd-nt-table-5 (bytevector-u8-ref ucd-nt-table-4 (fix:or (fix:lsh (bytevector-u8-ref ucd-nt-table-3 (fix:or (fix:lsh (bytevector-u8-ref ucd-nt-table-2 (fix:or (fix:lsh (bytevector-u8-ref ucd-nt-table-1 (fix:or (fix:lsh (bytevector-u8-ref ucd-nt-table-0 (fix:lsh sv -16)) 4) (fix:and 15 (fix:lsh sv -12)))) 4) (fix:and 15 (fix:lsh sv -8)))) 4) (fix:and 15 (fix:lsh sv -4)))) 4) (fix:and 15 sv))))))

(define ucd-nt-table-0
  '#u8(0 1 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3))

(define ucd-nt-table-1
  '#u8(0 1 2 3 4 5 6 7 8 9 10 11 11 11 11 12 13 14 15 11 11 11 16 11 11 11 11 11 11 17 18 19 20 11 21 22 11 11 23 11 11 11 11 11 11 11 11 24 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11))

(define ucd-nt-table-2
  '#u8(0 1 1 1 1 1 2 3 1 4 5 6 7 8 9 10 11 1 1 12 1 1 13 14 15 16 17 18 19 1 1 1 20 21 1 1 22 1 1 23 1 1 1 1 24 1 1 1 25 26 27 1 28 1 1 1 29 1 1 30 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 31 32 1 33 1 34 1 1 35 1 36 1 1 1 1 1 37 38 1 1 39 40 1 1 1 41 1 1 1 1 1 1 1 42 1 1 1 43 1 1 44 1 1 1 1 1 1 1 1 1 45 1 1 1 46 1 1 1 1 1 1 1 47 48 1 1 1 1 1 1 1 1 49 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 50 1 51 52 53 54 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 55 1 1 1 1 1 15 1 56 57 58 59 1 1 1 60 61 62 63 64 65 66 67 68 69 54 1 9 1 70 71 72 53 1 1 73 74 1 75 1 1 1 1 76 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 77 78 1 1 79 1 1 1 80 81 1 1 1 82 1 1 1 1 1 1 1 1 1 83 54 1 1 1 1 1 84 53 1 1 85 86 1 1 1 87 1 1 1 1 1 1 1 1 1 54 1 1 1 1 88 89 1 1 1 1 1 1 1 90 91 92 1 1 1 1 1 1 1 93 1 1 1 1 1 94 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 95 1 1 1 1 1 1 96 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 93 1 1 1 1 1 1 1))

(define ucd-nt-table-3
  '#u8(0 0 0 1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 3 4 0 0 0 0 0 0 3 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 3 5 0 0 0 0 0 0 3 6 0 0 0 0 0 0 3 7 0 0 0 0 0 0 3 0 0 0 0 0 0 7 3 8 0 0 0 0 0 0 3 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 9 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 11 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 15 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 17 18 0 0 0 0 0 0 0 0 0 0 0 0 19 19 19 20 0 0 0 0 0 0 0 0 0 0 0 0 0 21 22 23 24 0 0 0 0 25 26 0 0 0 0 0 0 0 27 28 29 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 30 31 0 32 33 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 34 0 0 0 0 0 0 0 0 15 0 35 36 0 0 15 0 0 36 0 0 0 0 37 0 0 0 0 0 0 0 38 0 0 0 0 0 0 0 0 0 39 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 30 0 0 0 0 0 0 0 0 0 0 0 40 0 0 0 0 30 0 0 41 42 0 43 14 43 44 0 30 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 45 0 0 0 46 0 47 0 0 0 0 0 0 0 0 0 0 0 0 0 48 0 0 0 0 0 0 0 49 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 50 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 51 0 0 0 0 0 0 0 39 0 0 0 0 0 0 0 13 52 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 53 0 0 0 0 41 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 54 0 0 46 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 46 0 0 0 0 0 0 0 0 0 0 0 0 0 53 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 46 0 0 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 53 38 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 55 0 56 0 0 44 0 0 0 0 0 0 0 46 0 0 1 0 0 0 0 0 0 0 0 0 0 0 57 0 0 0 0 58 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 50 59 0 0 0 54 0 60 0 30 61 19 19 10 19 19 19 8 62 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 36 63 0 0 10 0 64 0 0 0 0 0 0 0 0 65 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 35 0 66 0 0 61 0 0 0 0 67 0 68 0 0 0 0 0 0 0 0 0 69 19 70 19 19 0 0 0 0 71 0 0 72 0 73 0 0 0 0 67 0 0 0 0 0 0 35 0 35 0 0 66 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 74 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 21 75 0 0 0 0 0 0 0 0 0 73 76 0 0 49 0 0 0 0 0 0 77 0 0 0 0 0 0 0 0 78 79 0 0 0 0 0 0 0 0 1 0 0 0 3 0 0 0 0 0 0 0 0 0 1 36 80 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 81 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9 6 0 0 0 0 0 9 82 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 19 80 0 0 19 19 19 19 19 19 75 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 83 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 19 76 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 19 10 0 0 0 0 0 0 19 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 85 86 86 86 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 61 0 0 0 0 0 0 0 0 0 0 36 19 19 87 49 0 0 0 0 36 19 88 89 0 0 0 0 0 0 0 0 0 0 0 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 55 0 0 0 0 0 45 0 0 0 0 0 0 0 54 0 0 0 55 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 39 0 0 0 0 0 91 41 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 39 30 0 92 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 44 0 0 0 0 0 0 0 50 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 30 0 0 0 0 0 0 0 0 0))

(define ucd-nt-table-4
  '#u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 2 2 0 0 0 0 0 2 0 0 3 3 3 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 3 3 3 3 3 3 0 0 0 0 0 0 0 0 3 3 3 3 3 3 0 0 0 0 0 0 0 0 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 0 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 0 0 0 0 0 2 0 0 0 2 2 2 2 2 2 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 2 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 0 3 3 3 3 3 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 3 2 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 0 0 0 0 0 0 0 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 3 0 0 3 0 0 0 3 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 3 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 3 0 3 0 0 0 3 0 3 3 3 0 0 0 0 0 0 3 0 0 0 0 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 3 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 3 0 0 0 0 0 0 0 0 3 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 3 3 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 0 0 0 0 0 3 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3 0 0 0 0 0 0 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 3 3 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 3 0 0 0 1 1 1 1 1 1 1 1 1 1 0 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 0 2 2 2 2 2 2 2 2 2 2 2 3 3 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0))

(define ucd-nt-table-5
  '#(#f decimal digit numeric))
