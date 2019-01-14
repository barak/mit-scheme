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

;;;; UCD property: NFC_QC (nfc-quick-check)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (ucd-nfc_qc-value char)
  (let ((sv (char->integer char)))
    (vector-ref |ucd-NFC_QC-table-5| (bytevector-u8-ref |ucd-NFC_QC-table-4| (fix:or (fix:lsh (bytevector-u8-ref |ucd-NFC_QC-table-3| (fix:or (fix:lsh (bytevector-u8-ref |ucd-NFC_QC-table-2| (fix:or (fix:lsh (bytevector-u8-ref |ucd-NFC_QC-table-1| (fix:or (fix:lsh (bytevector-u8-ref |ucd-NFC_QC-table-0| (fix:lsh sv -16)) 4) (fix:and 15 (fix:lsh sv -12)))) 4) (fix:and 15 (fix:lsh sv -8)))) 4) (fix:and 15 (fix:lsh sv -4)))) 4) (fix:and 15 sv))))))

(define-deferred |ucd-NFC_QC-table-0|
  (vector->bytevector '#(0 1 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3)))

(define-deferred |ucd-NFC_QC-table-1|
  (vector->bytevector '#(0 1 2 3 4 4 4 4 4 4 4 4 4 4 4 5 4 6 4 4 4 4 4 4 4 4 4 4 4 7 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)))

(define-deferred |ucd-NFC_QC-table-2|
  (vector->bytevector '#(0 0 0 1 0 0 2 0 0 3 4 5 6 7 0 8 9 10 0 0 0 0 0 0 0 0 0 11 0 0 0 12 13 14 0 15 0 0 0 0 0 0 16 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 19 20 0 0 0 0 21 22 0 23 24 25 0 0 0 0 0 0 0 0 0 0 0 26 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 18 27 0 0 0 0 0)))

(define-deferred |ucd-NFC_QC-table-3|
  (vector->bytevector
   '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 3 4 5 0 0 6 7 0 0 0 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 9 0 10 0 0 0 0 0 11 0 12 0 0 0 0 0 13 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 11 0 15 0 0 0 0 0 11 0 16 0 0 0 0 0 0 0 17 0 0 0 0 0 0 18 19 0 0 0 0 0 11 0 16 0 0 0 0 0 0 20 21 0 0 0 0 0 0 22 23 24 25 26 22 23 24 0 0 0 0 0 0 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 27 28 0 0 29 30 31 0 0 0 0 0 0 32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 33 0 0 0 34 35 36 37 38 39 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 40 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 42 0 0 0 0 0 0 0 0 0 0 0 43 0 0 0 0 0 0 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 45 46 47 44 44 44 45 44 44 44 44 44 44 48 0 0 0 49 50 51 52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 53 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 11 0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 54 0 0 0 0 0 0 0 0 0 0 0 0 0 0 21 0 0 0 0 0 0 0 0 0 0 55 56 0 0 0 0 57 58 0 0 0 44 45 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define-deferred |ucd-NFC_QC-table-4|
  (vector->bytevector
   '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 0 0 1 0 1 0 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 1 1 0 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 2 2 1 2 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 2 2 0 2 0 0 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 0 0 2 0 0 0 0 0 0 0 1 1 0 0 0 0 2 2 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 2 0 0 0 0 2 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 2 0 2 2 0 2 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 2 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 0 2 2 0 0 0 0 0 0 0 0 0 2 0 2 0 2 0 0 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0 0 2 0 2 0 0 2 2 2 2 2 2 2 2 2 2 0 2 0 2 0 0 2 2 0 0 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 2 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 2 2 2 2 0 2 2 2 2 2 0 2 0 2 2 0 2 2 0 2 2 2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define-deferred |ucd-NFC_QC-table-5|
  #(yes maybe no))
