#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; UCD property: gc

;;; Generated from Unicode 9.0.0 UCD at 2017-02-07T20:26:42-08

(declare (usual-integrations))

;;; (497 740 746 22 22 22 22 22 22 22 22 22 22 22 750 753 753)
(define-deferred ucd-gc-value
  (let ((offsets (bytevector 241 1 228 2 234 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 238 2 241 2 241 2)))
    (named-lambda (ucd-gc-value sv)
      ((vector-ref ucd-gc-entries (bytevector-u16le-ref offsets (fix:and 62 (fix:lsh sv -15)))) sv ucd-gc-entries))))

(define (ucd-gc-entry-0 sv table)
  sv
  table
  "Cc")

(define (ucd-gc-entry-1 sv table)
  sv
  table
  "Zs")

(define (ucd-gc-entry-2 sv table)
  sv
  table
  "Po")

(define (ucd-gc-entry-3 sv table)
  sv
  table
  "Sc")

(define (ucd-gc-entry-4 sv table)
  sv
  table
  "Ps")

(define (ucd-gc-entry-5 sv table)
  sv
  table
  "Pe")

(define (ucd-gc-entry-6 sv table)
  sv
  table
  "Sm")

(define (ucd-gc-entry-7 sv table)
  sv
  table
  "Pd")

(define (ucd-gc-entry-8 sv table)
  sv
  table
  "Nd")

(define (ucd-gc-entry-9 sv table)
  sv
  table
  "Lu")

(define (ucd-gc-entry-10 sv table)
  sv
  table
  "Sk")

(define (ucd-gc-entry-11 sv table)
  sv
  table
  "Pc")

(define (ucd-gc-entry-12 sv table)
  sv
  table
  "Ll")

(define (ucd-gc-entry-13 sv table)
  sv
  table
  "So")

(define (ucd-gc-entry-14 sv table)
  sv
  table
  "Lo")

(define (ucd-gc-entry-15 sv table)
  sv
  table
  "Pi")

(define (ucd-gc-entry-16 sv table)
  sv
  table
  "Cf")

(define (ucd-gc-entry-17 sv table)
  sv
  table
  "No")

(define (ucd-gc-entry-18 sv table)
  sv
  table
  "Pf")

(define (ucd-gc-entry-19 sv table)
  sv
  table
  "Lt")

(define (ucd-gc-entry-20 sv table)
  sv
  table
  "Lm")

(define (ucd-gc-entry-21 sv table)
  sv
  table
  "Mn")

(define (ucd-gc-entry-22 sv table)
  sv
  table
  #f)

(define (ucd-gc-entry-23 sv table)
  sv
  table
  "Me")

(define (ucd-gc-entry-24 sv table)
  sv
  table
  "Mc")

(define (ucd-gc-entry-25 sv table)
  sv
  table
  "Nl")

(define (ucd-gc-entry-26 sv table)
  sv
  table
  "Zl")

(define (ucd-gc-entry-27 sv table)
  sv
  table
  "Zp")

(define (ucd-gc-entry-28 sv table)
  sv
  table
  "Co")

;;; (1 2 2 2 3 2 2 2 4 5 2 6 2 7 2 2)
(define-deferred ucd-gc-entry-29
  (let ((offsets (bytevector 1 2 2 2 3 2 2 2 4 5 2 6 2 7 2 2)))
    (named-lambda (ucd-gc-entry-29 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 2 2 6 6 6 2)
(define-deferred ucd-gc-entry-30
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 2 2 6 6 6 2)))
    (named-lambda (ucd-gc-entry-30 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-31
  (let ((offsets (bytevector 2 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-31 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 9 9 9 9 9 4 2 5 10 11)
(define-deferred ucd-gc-entry-32
  (let ((offsets (bytevector 9 9 9 9 9 9 9 9 9 9 9 4 2 5 10 11)))
    (named-lambda (ucd-gc-entry-32 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (10 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-33
  (let ((offsets (bytevector 10 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-33 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 4 6 5 6 0)
(define-deferred ucd-gc-entry-34
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 4 6 5 6 0)))
    (named-lambda (ucd-gc-entry-34 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (1 2 3 3 3 3 13 2 10 13 14 15 6 16 13 10)
(define-deferred ucd-gc-entry-35
  (let ((offsets (bytevector 1 2 3 3 3 3 13 2 10 13 14 15 6 16 13 10)))
    (named-lambda (ucd-gc-entry-35 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 6 17 17 10 12 2 2 10 17 14 18 17 17 17 2)
(define-deferred ucd-gc-entry-36
  (let ((offsets (bytevector 13 6 17 17 10 12 2 2 10 17 14 18 17 17 17 2)))
    (named-lambda (ucd-gc-entry-36 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 9 6 9 9 9 9 9 9 9 12)
(define-deferred ucd-gc-entry-37
  (let ((offsets (bytevector 9 9 9 9 9 9 9 6 9 9 9 9 9 9 9 12)))
    (named-lambda (ucd-gc-entry-37 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-38
  (let ((offsets (bytevector 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-38 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (0 0 29 30 31 32 33 34 0 0 35 36 9 37 12 38)
(define-deferred ucd-gc-entry-39
  (let ((offsets (bytevector 0 0 29 30 31 32 33 34 0 0 35 36 9 37 12 38)))
    (named-lambda (ucd-gc-entry-39 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (9 12 9 12 9 12 9 12 9 12 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-40
  (let ((offsets (bytevector 9 12 9 12 9 12 9 12 9 12 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-40 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 9 12 9 12 12 9 12 9 12 9 12 9)
(define-deferred ucd-gc-entry-41
  (let ((offsets (bytevector 9 12 9 12 9 12 9 12 12 9 12 9 12 9 12 9)))
    (named-lambda (ucd-gc-entry-41 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 9 12 9 12 9 12 9 12 12 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-42
  (let ((offsets (bytevector 12 9 12 9 12 9 12 9 12 12 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-42 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 9 12 9 12 9 9 12 9 12 9 12 12)
(define-deferred ucd-gc-entry-43
  (let ((offsets (bytevector 9 12 9 12 9 12 9 12 9 9 12 9 12 9 12 12)))
    (named-lambda (ucd-gc-entry-43 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 9 9 12 9 12 9 9 12 9 9 9 12 12 9 9)
(define-deferred ucd-gc-entry-44
  (let ((offsets (bytevector 12 9 9 12 9 12 9 9 12 9 9 9 12 12 9 9)))
    (named-lambda (ucd-gc-entry-44 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 12 9 9 12 9 9 9 12 12 12 9 9 12 9)
(define-deferred ucd-gc-entry-45
  (let ((offsets (bytevector 9 9 12 9 9 12 9 9 9 12 12 12 9 9 12 9)))
    (named-lambda (ucd-gc-entry-45 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 9 12 9 9 12 9 12 12 9 12 9 9)
(define-deferred ucd-gc-entry-46
  (let ((offsets (bytevector 9 12 9 12 9 12 9 9 12 9 12 12 9 12 9 9)))
    (named-lambda (ucd-gc-entry-46 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 9 9 9 12 9 12 9 9 12 12 14 9 12 12 12)
(define-deferred ucd-gc-entry-47
  (let ((offsets (bytevector 12 9 9 9 12 9 12 9 9 12 12 14 9 12 12 12)))
    (named-lambda (ucd-gc-entry-47 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 9 19 12 9 19 12 9 19 12 9 12 9)
(define-deferred ucd-gc-entry-48
  (let ((offsets (bytevector 14 14 14 14 9 19 12 9 19 12 9 19 12 9 12 9)))
    (named-lambda (ucd-gc-entry-48 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 9 12 9 12 9 12 9 12 9 12 9 12 12 9 12)
(define-deferred ucd-gc-entry-49
  (let ((offsets (bytevector 12 9 12 9 12 9 12 9 12 9 12 9 12 12 9 12)))
    (named-lambda (ucd-gc-entry-49 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 9 19 12 9 12 9 9 9 12 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-50
  (let ((offsets (bytevector 12 9 19 12 9 12 9 9 9 12 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-50 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (40 40 40 41 42 40 40 43 44 45 46 47 48 49 40 50)
(define-deferred ucd-gc-entry-51
  (let ((offsets (bytevector 40 40 40 41 42 40 40 43 44 45 46 47 48 49 40 50)))
    (named-lambda (ucd-gc-entry-51 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (9 12 9 12 12 12 12 12 12 12 9 9 12 9 9 12)
(define-deferred ucd-gc-entry-52
  (let ((offsets (bytevector 9 12 9 12 12 12 12 12 12 12 9 9 12 9 9 12)))
    (named-lambda (ucd-gc-entry-52 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 9 12 9 9 9 9 12 9 12 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-53
  (let ((offsets (bytevector 12 9 12 9 9 9 9 12 9 12 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-53 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 14 12 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-54
  (let ((offsets (bytevector 12 12 12 12 14 12 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-54 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 20 10 10 10 10 20 20 20 20 20 20 20 20 20 20)
(define-deferred ucd-gc-entry-55
  (let ((offsets (bytevector 20 20 10 10 10 10 20 20 20 20 20 20 20 20 20 20)))
    (named-lambda (ucd-gc-entry-55 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 20 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
(define-deferred ucd-gc-entry-56
  (let ((offsets (bytevector 20 20 10 10 10 10 10 10 10 10 10 10 10 10 10 10)))
    (named-lambda (ucd-gc-entry-56 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 20 20 20 20 10 10 10 10 10 10 10 20 10 20 10)
(define-deferred ucd-gc-entry-57
  (let ((offsets (bytevector 20 20 20 20 20 10 10 10 10 10 10 10 20 10 20 10)))
    (named-lambda (ucd-gc-entry-57 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (40 40 40 52 53 12 12 12 12 54 12 20 55 56 57 10)
(define-deferred ucd-gc-entry-58
  (let ((offsets (bytevector 40 40 40 52 53 12 12 12 12 54 12 20 55 56 57 10)))
    (named-lambda (ucd-gc-entry-58 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (9 12 9 12 20 10 9 12 22 22 20 12 12 12 2 9)
(define-deferred ucd-gc-entry-59
  (let ((offsets (bytevector 9 12 9 12 20 10 9 12 22 22 20 12 12 12 2 9)))
    (named-lambda (ucd-gc-entry-59 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 10 10 9 2 9 9 9 22 9 22 9 9)
(define-deferred ucd-gc-entry-60
  (let ((offsets (bytevector 22 22 22 22 10 10 9 2 9 9 9 22 9 22 9 9)))
    (named-lambda (ucd-gc-entry-60 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-61
  (let ((offsets (bytevector 12 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-61 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 22 9 9 9 9 9 9 9 9 9 12 12 12 12)
(define-deferred ucd-gc-entry-62
  (let ((offsets (bytevector 9 9 22 9 9 9 9 9 9 9 9 9 12 12 12 12)))
    (named-lambda (ucd-gc-entry-62 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 9)
(define-deferred ucd-gc-entry-63
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 9)))
    (named-lambda (ucd-gc-entry-63 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 9 9 9 12 12 12 9 12 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-64
  (let ((offsets (bytevector 12 12 9 9 9 12 12 12 9 12 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-64 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 9 12 6 9 12 9 9 12 12 9 9 9)
(define-deferred ucd-gc-entry-65
  (let ((offsets (bytevector 12 12 12 12 9 12 6 9 12 9 9 12 12 9 9 9)))
    (named-lambda (ucd-gc-entry-65 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 59 60 61 62 12 63 64 40 65)
(define-deferred ucd-gc-entry-66
  (let ((offsets (bytevector 21 21 21 21 21 21 21 59 60 61 62 12 63 64 40 65)))
    (named-lambda (ucd-gc-entry-66 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (9 12 13 21 21 21 21 21 23 23 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-67
  (let ((offsets (bytevector 9 12 13 21 21 21 21 21 23 23 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-67 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 12 9 12 9 12 9 12 9 12 9 12 9 12 12)
(define-deferred ucd-gc-entry-68
  (let ((offsets (bytevector 9 9 12 9 12 9 12 9 12 9 12 9 12 9 12 12)))
    (named-lambda (ucd-gc-entry-68 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 12 12 12 40 40 67 40 40 40 68 40 40 40)
(define-deferred ucd-gc-entry-69
  (let ((offsets (bytevector 9 9 9 12 12 12 40 40 67 40 40 40 68 40 40 40)))
    (named-lambda (ucd-gc-entry-69 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (22 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-70
  (let ((offsets (bytevector 22 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-70 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 9 22 22 20 2 2 2 2 2 2)
(define-deferred ucd-gc-entry-71
  (let ((offsets (bytevector 9 9 9 9 9 9 9 22 22 20 2 2 2 2 2 2)))
    (named-lambda (ucd-gc-entry-71 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-72
  (let ((offsets (bytevector 22 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-72 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 22 2 7 22 22 13 13 3)
(define-deferred ucd-gc-entry-73
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 22 2 7 22 22 13 13 3)))
    (named-lambda (ucd-gc-entry-73 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-74
  (let ((offsets (bytevector 22 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-74 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 21 21 21 21 21 7 21)
(define-deferred ucd-gc-entry-75
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 21 21 21 21 21 7 21)))
    (named-lambda (ucd-gc-entry-75 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 21 21 2 21 21 2 21 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-76
  (let ((offsets (bytevector 2 21 21 2 21 21 2 21 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-76 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 22 22 22 22 22)
(define-deferred ucd-gc-entry-77
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-77 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 2 2 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-78
  (let ((offsets (bytevector 14 14 14 2 2 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-78 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (40 40 40 70 9 71 72 12 73 74 21 75 76 14 77 78)
(define-deferred ucd-gc-entry-79
  (let ((offsets (bytevector 40 40 40 70 9 71 72 12 73 74 21 75 76 14 77 78)))
    (named-lambda (ucd-gc-entry-79 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (16 16 16 16 16 16 6 6 6 2 2 3 2 2 13 13)
(define-deferred ucd-gc-entry-80
  (let ((offsets (bytevector 16 16 16 16 16 16 6 6 6 2 2 3 2 2 13 13)))
    (named-lambda (ucd-gc-entry-80 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 21 21 2 16 22 2 2)
(define-deferred ucd-gc-entry-81
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 21 21 2 16 22 2 2)))
    (named-lambda (ucd-gc-entry-81 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 14 14 14 14 14 14 14 14 14 14 21 21 21 21 21)
(define-deferred ucd-gc-entry-82
  (let ((offsets (bytevector 20 14 14 14 14 14 14 14 14 14 14 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-82 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 2 2 2 2 14 14)
(define-deferred ucd-gc-entry-83
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 2 2 2 2 14 14)))
    (named-lambda (ucd-gc-entry-83 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-84
  (let ((offsets (bytevector 21 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-84 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 2 14 21 21 21 21 21 21 21 16 13 21)
(define-deferred ucd-gc-entry-85
  (let ((offsets (bytevector 14 14 14 14 2 14 21 21 21 21 21 21 21 16 13 21)))
    (named-lambda (ucd-gc-entry-85 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 20 20 21 21 13 21 21 21 21 14 14)
(define-deferred ucd-gc-entry-86
  (let ((offsets (bytevector 21 21 21 21 21 20 20 21 21 13 21 21 21 21 14 14)))
    (named-lambda (ucd-gc-entry-86 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 14 14 14 13 13 14)
(define-deferred ucd-gc-entry-87
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 14 14 14 13 13 14)))
    (named-lambda (ucd-gc-entry-87 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (80 81 14 14 82 21 83 84 14 14 14 14 14 85 86 87)
(define-deferred ucd-gc-entry-88
  (let ((offsets (bytevector 80 81 14 14 82 21 83 84 14 14 14 14 14 85 86 87)))
    (named-lambda (ucd-gc-entry-88 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (2 2 2 2 2 2 2 2 2 2 2 2 2 2 22 16)
(define-deferred ucd-gc-entry-89
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 2 2 2 2 2 2 22 16)))
    (named-lambda (ucd-gc-entry-89 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 21 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-90
  (let ((offsets (bytevector 14 21 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-90 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 21 21 22 22 14 14 14)
(define-deferred ucd-gc-entry-91
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 21 21 22 22 14 14 14)))
    (named-lambda (ucd-gc-entry-91 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 21 21 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-92
  (let ((offsets (bytevector 14 14 14 14 14 14 21 21 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-92 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 14 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-93
  (let ((offsets (bytevector 21 14 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-93 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-94
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-94 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 21 21 21 21 21)
(define-deferred ucd-gc-entry-95
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-95 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 20 20 13 2 2 2 20 22 22 22 22 22)
(define-deferred ucd-gc-entry-96
  (let ((offsets (bytevector 21 21 21 21 20 20 13 2 2 2 20 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-96 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (89 90 14 21 91 14 14 14 14 14 92 93 94 14 95 96)
(define-deferred ucd-gc-entry-97
  (let ((offsets (bytevector 89 90 14 21 91 14 14 14 14 14 92 93 94 14 95 96)))
    (named-lambda (ucd-gc-entry-97 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 21 21 21 21 20 21 21 21 21 21)
(define-deferred ucd-gc-entry-98
  (let ((offsets (bytevector 14 14 14 14 14 14 21 21 21 21 20 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-98 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 20 21 21 21 20 21 21 21 21 21 22 22)
(define-deferred ucd-gc-entry-99
  (let ((offsets (bytevector 21 21 21 21 20 21 21 21 20 21 21 21 21 21 22 22)))
    (named-lambda (ucd-gc-entry-99 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 22)
(define-deferred ucd-gc-entry-100
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 22)))
    (named-lambda (ucd-gc-entry-100 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 21 21 21 22 22 2 22)
(define-deferred ucd-gc-entry-101
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 21 21 21 22 22 2 22)))
    (named-lambda (ucd-gc-entry-101 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 22 14 14 14 14 14 14 14 14 22 22)
(define-deferred ucd-gc-entry-102
  (let ((offsets (bytevector 14 14 14 14 14 22 14 14 14 14 14 14 14 14 22 22)))
    (named-lambda (ucd-gc-entry-102 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 21 21 21 21 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-103
  (let ((offsets (bytevector 22 22 22 22 21 21 21 21 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-103 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 16 21 21 21 21 21 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-104
  (let ((offsets (bytevector 21 21 16 21 21 21 21 21 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-104 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 98 99 100 14 101 22 22 22 22 14 102 22 103 104 21)
(define-deferred ucd-gc-entry-105
  (let ((offsets (bytevector 14 98 99 100 14 101 22 22 22 22 14 102 22 103 104 21)))
    (named-lambda (ucd-gc-entry-105 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (21 21 21 24 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-106
  (let ((offsets (bytevector 21 21 21 24 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-106 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 21 24 21 14 24 24)
(define-deferred ucd-gc-entry-107
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 21 24 21 14 24 24)))
    (named-lambda (ucd-gc-entry-107 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 21 21 21 21 21 21 21 21 24 24 24 24 21 24 24)
(define-deferred ucd-gc-entry-108
  (let ((offsets (bytevector 24 21 21 21 21 21 21 21 21 24 24 24 24 21 24 24)))
    (named-lambda (ucd-gc-entry-108 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 21 21 21 21 21 21 21 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-109
  (let ((offsets (bytevector 14 21 21 21 21 21 21 21 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-109 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 21 21 2 2 8 8 8 8 8 8 8 8 8 8)
(define-deferred ucd-gc-entry-110
  (let ((offsets (bytevector 14 14 21 21 2 2 8 8 8 8 8 8 8 8 8 8)))
    (named-lambda (ucd-gc-entry-110 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 20 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-111
  (let ((offsets (bytevector 2 20 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-111 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 21 24 24 22 14 14 14 14 14 14 14 14 22 22 14)
(define-deferred ucd-gc-entry-112
  (let ((offsets (bytevector 14 21 24 24 22 14 14 14 14 14 14 14 14 22 22 14)))
    (named-lambda (ucd-gc-entry-112 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 22 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-113
  (let ((offsets (bytevector 14 22 22 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-113 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 22 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-114
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 22 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-114 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 14 22 22 22 14 14 14 14 22 22 21 14 24 24)
(define-deferred ucd-gc-entry-115
  (let ((offsets (bytevector 14 22 14 22 22 22 14 14 14 14 22 22 21 14 24 24)))
    (named-lambda (ucd-gc-entry-115 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 21 21 21 21 22 22 24 24 22 22 24 24 21 14 22)
(define-deferred ucd-gc-entry-116
  (let ((offsets (bytevector 24 21 21 21 21 22 22 24 24 22 22 24 24 21 14 22)))
    (named-lambda (ucd-gc-entry-116 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 24 22 22 22 22 14 14 22 14)
(define-deferred ucd-gc-entry-117
  (let ((offsets (bytevector 22 22 22 22 22 22 22 24 22 22 22 22 14 14 22 14)))
    (named-lambda (ucd-gc-entry-117 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 21 21 22 22 8 8 8 8 8 8 8 8 8 8)
(define-deferred ucd-gc-entry-118
  (let ((offsets (bytevector 14 14 21 21 22 22 8 8 8 8 8 8 8 8 8 8)))
    (named-lambda (ucd-gc-entry-118 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 3 3 17 17 17 17 17 17 13 3 22 22 22 22)
(define-deferred ucd-gc-entry-119
  (let ((offsets (bytevector 14 14 3 3 17 17 17 17 17 17 13 3 22 22 22 22)))
    (named-lambda (ucd-gc-entry-119 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (106 14 14 107 108 109 110 111 112 113 114 115 116 117 118 119)
(define-deferred ucd-gc-entry-120
  (let ((offsets (bytevector 106 14 14 107 108 109 110 111 112 113 114 115 116 117 118 119)))
    (named-lambda (ucd-gc-entry-120 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (22 21 21 24 22 14 14 14 14 14 14 22 22 22 22 14)
(define-deferred ucd-gc-entry-121
  (let ((offsets (bytevector 22 21 21 24 22 14 14 14 14 14 14 22 22 22 22 14)))
    (named-lambda (ucd-gc-entry-121 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 14 14 22 14 14 22 14 14 22 22 21 22 24 24)
(define-deferred ucd-gc-entry-122
  (let ((offsets (bytevector 14 22 14 14 22 14 14 22 14 14 22 22 21 22 24 24)))
    (named-lambda (ucd-gc-entry-122 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 21 21 22 22 22 22 21 21 22 22 21 21 21 22 22)
(define-deferred ucd-gc-entry-123
  (let ((offsets (bytevector 24 21 21 22 22 22 22 21 21 22 22 21 21 21 22 22)))
    (named-lambda (ucd-gc-entry-123 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 21 22 22 22 22 22 22 22 14 14 14 14 22 14 22)
(define-deferred ucd-gc-entry-124
  (let ((offsets (bytevector 22 21 22 22 22 22 22 22 22 14 14 14 14 22 14 22)))
    (named-lambda (ucd-gc-entry-124 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 8 8 8 8 8 8 8 8 8 8)
(define-deferred ucd-gc-entry-125
  (let ((offsets (bytevector 22 22 22 22 22 22 8 8 8 8 8 8 8 8 8 8)))
    (named-lambda (ucd-gc-entry-125 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 14 14 14 21 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-126
  (let ((offsets (bytevector 21 21 14 14 14 21 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-126 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 21 21 24 22 14 14 14 14 14 14 14 14 14 22 14)
(define-deferred ucd-gc-entry-127
  (let ((offsets (bytevector 22 21 21 24 22 14 14 14 14 14 14 14 14 14 22 14)))
    (named-lambda (ucd-gc-entry-127 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 22 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-128
  (let ((offsets (bytevector 14 14 22 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-128 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 14 14 22 14 14 14 14 14 22 22 21 14 24 24)
(define-deferred ucd-gc-entry-129
  (let ((offsets (bytevector 14 22 14 14 22 14 14 14 14 14 22 22 21 14 24 24)))
    (named-lambda (ucd-gc-entry-129 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 21 21 21 21 21 22 21 21 24 22 24 24 21 22 22)
(define-deferred ucd-gc-entry-130
  (let ((offsets (bytevector 24 21 21 21 21 21 22 21 21 24 22 24 24 21 22 22)))
    (named-lambda (ucd-gc-entry-130 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-131
  (let ((offsets (bytevector 14 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-131 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 3 22 22 22 22 22 22 22 14 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-132
  (let ((offsets (bytevector 2 3 22 22 22 22 22 22 22 14 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-132 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (121 113 114 122 123 124 125 126 127 128 114 129 130 131 118 132)
(define-deferred ucd-gc-entry-133
  (let ((offsets (bytevector 121 113 114 122 123 124 125 126 127 128 114 129 130 131 118 132)))
    (named-lambda (ucd-gc-entry-133 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (22 21 24 24 22 14 14 14 14 14 14 14 14 22 22 14)
(define-deferred ucd-gc-entry-134
  (let ((offsets (bytevector 22 21 24 24 22 14 14 14 14 14 14 14 14 22 22 14)))
    (named-lambda (ucd-gc-entry-134 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 14 14 22 14 14 14 14 14 22 22 21 14 24 21)
(define-deferred ucd-gc-entry-135
  (let ((offsets (bytevector 14 22 14 14 22 14 14 14 14 14 22 22 21 14 24 21)))
    (named-lambda (ucd-gc-entry-135 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 21 21 21 21 22 22 24 24 22 22 24 24 21 22 22)
(define-deferred ucd-gc-entry-136
  (let ((offsets (bytevector 24 21 21 21 21 22 22 24 24 22 22 24 24 21 22 22)))
    (named-lambda (ucd-gc-entry-136 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 21 24 22 22 22 22 14 14 22 14)
(define-deferred ucd-gc-entry-137
  (let ((offsets (bytevector 22 22 22 22 22 22 21 24 22 22 22 22 14 14 22 14)))
    (named-lambda (ucd-gc-entry-137 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 14 17 17 17 17 17 17 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-138
  (let ((offsets (bytevector 13 14 17 17 17 17 17 17 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-138 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 21 14 22 14 14 14 14 14 14 22 22 22 14 14)
(define-deferred ucd-gc-entry-139
  (let ((offsets (bytevector 22 22 21 14 22 14 14 14 14 14 14 22 22 22 14 14)))
    (named-lambda (ucd-gc-entry-139 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 14 14 14 14 22 22 22 14 14 22 14 22 14 14)
(define-deferred ucd-gc-entry-140
  (let ((offsets (bytevector 14 22 14 14 14 14 22 22 22 14 14 22 14 22 14 14)))
    (named-lambda (ucd-gc-entry-140 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 14 14 22 22 22 14 14 14 22 22 22 14 14)
(define-deferred ucd-gc-entry-141
  (let ((offsets (bytevector 22 22 22 14 14 22 22 22 14 14 14 22 22 22 14 14)))
    (named-lambda (ucd-gc-entry-141 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 22 22 22 22 24 24)
(define-deferred ucd-gc-entry-142
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 22 22 22 22 24 24)))
    (named-lambda (ucd-gc-entry-142 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 24 24 22 22 22 24 24 24 22 24 24 24 21 22 22)
(define-deferred ucd-gc-entry-143
  (let ((offsets (bytevector 21 24 24 22 22 22 24 24 24 22 24 24 24 21 22 22)))
    (named-lambda (ucd-gc-entry-143 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 22 22 22 22 22 24 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-144
  (let ((offsets (bytevector 14 22 22 22 22 22 22 24 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-144 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 13 13 13 13 13 13 3 13 22 22 22 22 22)
(define-deferred ucd-gc-entry-145
  (let ((offsets (bytevector 17 17 17 13 13 13 13 13 13 3 13 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-145 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (134 113 114 135 136 137 118 138 139 140 141 142 143 144 125 145)
(define-deferred ucd-gc-entry-146
  (let ((offsets (bytevector 134 113 114 135 136 137 118 138 139 140 141 142 143 144 125 145)))
    (named-lambda (ucd-gc-entry-146 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (21 24 24 24 22 14 14 14 14 14 14 14 14 22 14 14)
(define-deferred ucd-gc-entry-147
  (let ((offsets (bytevector 21 24 24 24 22 14 14 14 14 14 14 14 14 22 14 14)))
    (named-lambda (ucd-gc-entry-147 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-148
  (let ((offsets (bytevector 14 22 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-148 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 22 22 22 14 21 21)
(define-deferred ucd-gc-entry-149
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 22 22 22 14 21 21)))
    (named-lambda (ucd-gc-entry-149 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 24 24 24 24 22 21 21 21 22 21 21 21 21 22 22)
(define-deferred ucd-gc-entry-150
  (let ((offsets (bytevector 21 24 24 24 24 22 21 21 21 22 21 21 21 21 22 22)))
    (named-lambda (ucd-gc-entry-150 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 21 21 22 14 14 14 22 22 22 22 22)
(define-deferred ucd-gc-entry-151
  (let ((offsets (bytevector 22 22 22 22 22 21 21 22 14 14 14 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-151 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 17 17 17 17 17 17 17 13)
(define-deferred ucd-gc-entry-152
  (let ((offsets (bytevector 22 22 22 22 22 22 22 22 17 17 17 17 17 17 17 13)))
    (named-lambda (ucd-gc-entry-152 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 21 24 24 22 14 14 14 14 14 14 14 14 22 14 14)
(define-deferred ucd-gc-entry-153
  (let ((offsets (bytevector 14 21 24 24 22 14 14 14 14 14 14 14 14 22 14 14)))
    (named-lambda (ucd-gc-entry-153 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 22 14 14 14 14 14 22 22 21 14 24 21)
(define-deferred ucd-gc-entry-154
  (let ((offsets (bytevector 14 14 14 14 22 14 14 14 14 14 22 22 21 14 24 21)))
    (named-lambda (ucd-gc-entry-154 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 24 24 22 21 24 24 22 24 24 21 21 22 22)
(define-deferred ucd-gc-entry-155
  (let ((offsets (bytevector 24 24 24 24 24 22 21 24 24 22 24 24 21 21 22 22)))
    (named-lambda (ucd-gc-entry-155 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 24 24 22 22 22 22 22 22 22 14 22)
(define-deferred ucd-gc-entry-156
  (let ((offsets (bytevector 22 22 22 22 22 24 24 22 22 22 22 22 22 22 14 22)))
    (named-lambda (ucd-gc-entry-156 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 14 14 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-157
  (let ((offsets (bytevector 22 14 14 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-157 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (147 148 114 149 150 151 118 152 153 148 114 154 155 156 118 157)
(define-deferred ucd-gc-entry-158
  (let ((offsets (bytevector 147 148 114 149 150 151 118 152 153 148 114 154 155 156 118 157)))
    (named-lambda (ucd-gc-entry-158 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (22 21 24 24 22 14 14 14 14 14 14 14 14 22 14 14)
(define-deferred ucd-gc-entry-159
  (let ((offsets (bytevector 22 21 24 24 22 14 14 14 14 14 14 14 14 22 14 14)))
    (named-lambda (ucd-gc-entry-159 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 22 22 14 24 24)
(define-deferred ucd-gc-entry-160
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 22 22 14 24 24)))
    (named-lambda (ucd-gc-entry-160 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 21 21 21 21 22 24 24 24 22 24 24 24 21 14 13)
(define-deferred ucd-gc-entry-161
  (let ((offsets (bytevector 24 21 21 21 21 22 24 24 24 22 24 24 24 21 14 13)))
    (named-lambda (ucd-gc-entry-161 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 14 14 14 24 17 17 17 17 17 17 17 14)
(define-deferred ucd-gc-entry-162
  (let ((offsets (bytevector 22 22 22 22 14 14 14 24 17 17 17 17 17 17 17 14)))
    (named-lambda (ucd-gc-entry-162 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 17 17 17 13 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-163
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 17 13 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-163 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 24 24 22 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-164
  (let ((offsets (bytevector 22 22 24 24 22 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-164 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 22 22 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-165
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 22 22 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-165 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 22 14 14 14 14 14 14 14 14 14 22 14 22 22)
(define-deferred ucd-gc-entry-166
  (let ((offsets (bytevector 14 14 22 14 14 14 14 14 14 14 14 14 22 14 22 22)))
    (named-lambda (ucd-gc-entry-166 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 22 22 21 22 22 22 22 24)
(define-deferred ucd-gc-entry-167
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 22 22 21 22 22 22 22 24)))
    (named-lambda (ucd-gc-entry-167 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 21 21 21 22 21 22 24 24 24 24 24 24 24 24)
(define-deferred ucd-gc-entry-168
  (let ((offsets (bytevector 24 24 21 21 21 22 21 22 24 24 24 24 24 24 24 24)))
    (named-lambda (ucd-gc-entry-168 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 24 24 2 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-169
  (let ((offsets (bytevector 22 22 24 24 2 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-169 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (159 148 14 160 161 162 118 163 164 165 14 166 167 168 125 169)
(define-deferred ucd-gc-entry-170
  (let ((offsets (bytevector 159 148 14 160 161 162 118 163 164 165 14 166 167 168 125 169)))
    (named-lambda (ucd-gc-entry-170 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (22 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-171
  (let ((offsets (bytevector 22 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-171 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 21 14 14 21 21 21 21 21 21 21 22 22 22 22 3)
(define-deferred ucd-gc-entry-172
  (let ((offsets (bytevector 14 21 14 14 21 21 21 21 21 21 21 22 22 22 22 3)))
    (named-lambda (ucd-gc-entry-172 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 20 21 21 21 21 21 21 21 21 2)
(define-deferred ucd-gc-entry-173
  (let ((offsets (bytevector 14 14 14 14 14 14 20 21 21 21 21 21 21 21 21 2)))
    (named-lambda (ucd-gc-entry-173 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 2 2 22 22 22 22)
(define-deferred ucd-gc-entry-174
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 2 2 22 22 22 22)))
    (named-lambda (ucd-gc-entry-174 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 14 14 22 14 22 22 14 14 22 14 22 22 14 22 22)
(define-deferred ucd-gc-entry-175
  (let ((offsets (bytevector 22 14 14 22 14 22 22 14 14 22 14 22 22 14 22 22)))
    (named-lambda (ucd-gc-entry-175 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 14 14 14 14 22 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-176
  (let ((offsets (bytevector 22 22 22 22 14 14 14 14 22 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-176 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 14 14 14 22 14 22 14 22 22 14 14 22 14 14 14)
(define-deferred ucd-gc-entry-177
  (let ((offsets (bytevector 22 14 14 14 22 14 22 14 22 22 14 14 22 14 14 14)))
    (named-lambda (ucd-gc-entry-177 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 21 14 14 21 21 21 21 21 21 22 21 21 14 22 22)
(define-deferred ucd-gc-entry-178
  (let ((offsets (bytevector 14 21 14 14 21 21 21 21 21 21 22 21 21 14 22 22)))
    (named-lambda (ucd-gc-entry-178 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 22 20 22 21 21 21 21 21 21 22 22)
(define-deferred ucd-gc-entry-179
  (let ((offsets (bytevector 14 14 14 14 14 22 20 22 21 21 21 21 21 21 22 22)))
    (named-lambda (ucd-gc-entry-179 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 22 22 14 14 14 14)
(define-deferred ucd-gc-entry-180
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 22 22 14 14 14 14)))
    (named-lambda (ucd-gc-entry-180 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (171 14 14 172 173 174 22 22 175 176 177 178 179 180 22 22)
(define-deferred ucd-gc-entry-181
  (let ((offsets (bytevector 171 14 14 172 173 174 22 22 175 176 177 178 179 180 22 22)))
    (named-lambda (ucd-gc-entry-181 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 13 13 13 2 2 2 2 2 2 2 2 2 2 2 2)
(define-deferred ucd-gc-entry-182
  (let ((offsets (bytevector 14 13 13 13 2 2 2 2 2 2 2 2 2 2 2 2)))
    (named-lambda (ucd-gc-entry-182 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 13 2 13 13 13 21 21 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-183
  (let ((offsets (bytevector 2 2 2 13 2 13 13 13 21 21 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-183 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-184
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-184 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 13 21 13 21 13 21 4 5 4 5 24 24)
(define-deferred ucd-gc-entry-185
  (let ((offsets (bytevector 17 17 17 17 13 21 13 21 13 21 4 5 4 5 24 24)))
    (named-lambda (ucd-gc-entry-185 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 22 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-186
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 22 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-186 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 22 22 22)
(define-deferred ucd-gc-entry-187
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 22 22 22)))
    (named-lambda (ucd-gc-entry-187 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 21 21 21 21 21 21 21 21 21 21 21 21 21 21 24)
(define-deferred ucd-gc-entry-188
  (let ((offsets (bytevector 22 21 21 21 21 21 21 21 21 21 21 21 21 21 21 24)))
    (named-lambda (ucd-gc-entry-188 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 2 21 21 14 14 14 14 14 21 21 21)
(define-deferred ucd-gc-entry-189
  (let ((offsets (bytevector 21 21 21 21 21 2 21 21 14 14 14 14 14 21 21 21)))
    (named-lambda (ucd-gc-entry-189 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 22 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-190
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 22 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-190 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 21 21 21 21 22 13 13)
(define-deferred ucd-gc-entry-191
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 21 21 21 21 22 13 13)))
    (named-lambda (ucd-gc-entry-191 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 21 13 13 13 13 13 13 22 13 13)
(define-deferred ucd-gc-entry-192
  (let ((offsets (bytevector 13 13 13 13 13 13 21 13 13 13 13 13 13 22 13 13)))
    (named-lambda (ucd-gc-entry-192 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 13 13 13 13 2 2 22 22 22 22 22)
(define-deferred ucd-gc-entry-193
  (let ((offsets (bytevector 2 2 2 2 2 13 13 13 13 2 2 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-193 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (182 183 184 185 186 14 187 188 189 190 21 191 192 193 22 22)
(define-deferred ucd-gc-entry-194
  (let ((offsets (bytevector 182 183 184 185 186 14 187 188 189 190 21 191 192 193 22 22)))
    (named-lambda (ucd-gc-entry-194 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 24 24 21 21 21)
(define-deferred ucd-gc-entry-195
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 24 24 21 21 21)))
    (named-lambda (ucd-gc-entry-195 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 24 21 21 21 21 21 21 24 21 21 24 24 21 21 14)
(define-deferred ucd-gc-entry-196
  (let ((offsets (bytevector 21 24 21 21 21 21 21 21 24 21 21 24 24 21 21 14)))
    (named-lambda (ucd-gc-entry-196 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 2 2 2 2 2 2)
(define-deferred ucd-gc-entry-197
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 2 2 2 2 2 2)))
    (named-lambda (ucd-gc-entry-197 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 24 24 21 21 14 14 14 14 21 21)
(define-deferred ucd-gc-entry-198
  (let ((offsets (bytevector 14 14 14 14 14 14 24 24 21 21 14 14 14 14 21 21)))
    (named-lambda (ucd-gc-entry-198 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 14 24 24 24 14 14 24 24 24 24 24 24 24 14 14)
(define-deferred ucd-gc-entry-199
  (let ((offsets (bytevector 21 14 24 24 24 14 14 24 24 24 24 24 24 24 14 14)))
    (named-lambda (ucd-gc-entry-199 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 21 21 21 21 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-200
  (let ((offsets (bytevector 14 21 21 21 21 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-200 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 21 24 24 21 21 24 24 24 24 24 24 21 14 24)
(define-deferred ucd-gc-entry-201
  (let ((offsets (bytevector 14 14 21 24 24 21 21 24 24 24 24 24 24 21 14 24)))
    (named-lambda (ucd-gc-entry-201 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 24 24 24 21 13 13)
(define-deferred ucd-gc-entry-202
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 24 24 24 21 13 13)))
    (named-lambda (ucd-gc-entry-202 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 22 9 22 22 22 22 22 9 22 22)
(define-deferred ucd-gc-entry-203
  (let ((offsets (bytevector 9 9 9 9 9 9 22 9 22 22 22 22 22 9 22 22)))
    (named-lambda (ucd-gc-entry-203 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 2 20 14 14 14)
(define-deferred ucd-gc-entry-204
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 2 20 14 14 14)))
    (named-lambda (ucd-gc-entry-204 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 195 196 197 198 199 200 201 202 9 9 203 14 14 204)
(define-deferred ucd-gc-entry-205
  (let ((offsets (bytevector 14 14 195 196 197 198 199 200 201 202 9 9 203 14 14 204)))
    (named-lambda (ucd-gc-entry-205 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 22 14 14 14 14 22 22)
(define-deferred ucd-gc-entry-206
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 22 14 14 14 14 22 22)))
    (named-lambda (ucd-gc-entry-206 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 14 22 14 14 14 14 22 22)
(define-deferred ucd-gc-entry-207
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 14 22 14 14 14 14 22 22)))
    (named-lambda (ucd-gc-entry-207 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 14 14 14 14 22 22 14 14 14 14 14 14 14 22)
(define-deferred ucd-gc-entry-208
  (let ((offsets (bytevector 14 22 14 14 14 14 22 22 14 14 14 14 14 14 14 22)))
    (named-lambda (ucd-gc-entry-208 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 14 14 14 14 22 22 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-209
  (let ((offsets (bytevector 14 22 14 14 14 14 22 22 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-209 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-210
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-210 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 206 207 14 14 206 14 14 208 209 210 14 14)
(define-deferred ucd-gc-entry-211
  (let ((offsets (bytevector 14 14 14 14 206 207 14 14 206 14 14 208 209 210 14 14)))
    (named-lambda (ucd-gc-entry-211 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 22 22 21 21 21)
(define-deferred ucd-gc-entry-212
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 22 22 21 21 21)))
    (named-lambda (ucd-gc-entry-212 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 2 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-213
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 2 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-213 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 17 17 17 17 17 17 17 22 22 22)
(define-deferred ucd-gc-entry-214
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 17 17 17 17 17 22 22 22)))
    (named-lambda (ucd-gc-entry-214 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-215
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-215 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 22 22 12 12 12 12 12 12 22 22)
(define-deferred ucd-gc-entry-216
  (let ((offsets (bytevector 9 9 9 9 9 9 22 22 12 12 12 12 12 12 22 22)))
    (named-lambda (ucd-gc-entry-216 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 209 14 14 14 212 213 214 14 215 9 9 9 9 9 216)
(define-deferred ucd-gc-entry-217
  (let ((offsets (bytevector 14 209 14 14 14 212 213 214 14 215 9 9 9 9 9 216)))
    (named-lambda (ucd-gc-entry-217 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (7 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-218
  (let ((offsets (bytevector 7 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-218 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (218 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-219
  (let ((offsets (bytevector 218 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-219 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 2 2 14)
(define-deferred ucd-gc-entry-220
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 2 2 14)))
    (named-lambda (ucd-gc-entry-220 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (1 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-221
  (let ((offsets (bytevector 1 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-221 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 4 5 22 22 22)
(define-deferred ucd-gc-entry-222
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 4 5 22 22 22)))
    (named-lambda (ucd-gc-entry-222 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 2 2 2 25 25)
(define-deferred ucd-gc-entry-223
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 2 2 2 25 25)))
    (named-lambda (ucd-gc-entry-223 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (25 14 14 14 14 14 14 14 14 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-224
  (let ((offsets (bytevector 25 14 14 14 14 14 14 14 14 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-224 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 220 14 221 222 14 14 14 14 223 224)
(define-deferred ucd-gc-entry-225
  (let ((offsets (bytevector 14 14 14 14 14 14 220 14 221 222 14 14 14 14 223 224)))
    (named-lambda (ucd-gc-entry-225 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 22 14 14)
(define-deferred ucd-gc-entry-226
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 22 14 14)))
    (named-lambda (ucd-gc-entry-226 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 21 21 21 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-227
  (let ((offsets (bytevector 14 14 21 21 21 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-227 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 21 21 21 2 2 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-228
  (let ((offsets (bytevector 14 14 21 21 21 2 2 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-228 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 21 21 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-229
  (let ((offsets (bytevector 14 14 21 21 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-229 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 21 21 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-230
  (let ((offsets (bytevector 14 22 21 21 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-230 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 21 21 24 21 21 21 21 21 21 21 24 24)
(define-deferred ucd-gc-entry-231
  (let ((offsets (bytevector 14 14 14 14 21 21 24 21 21 21 21 21 21 21 24 24)))
    (named-lambda (ucd-gc-entry-231 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 24 24 24 21 24 24 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-232
  (let ((offsets (bytevector 24 24 24 24 24 24 21 24 24 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-232 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 2 2 2 20 2 2 2 3 14 21 22 22)
(define-deferred ucd-gc-entry-233
  (let ((offsets (bytevector 21 21 21 21 2 2 2 20 2 2 2 3 14 21 22 22)))
    (named-lambda (ucd-gc-entry-233 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-234
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-234 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 17 17 17 17 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-235
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 17 17 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-235 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (226 227 14 228 14 229 226 230 14 14 14 231 232 233 234 235)
(define-deferred ucd-gc-entry-236
  (let ((offsets (bytevector 226 227 14 228 14 229 226 230 14 14 14 231 232 233 234 235)))
    (named-lambda (ucd-gc-entry-236 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (2 2 2 2 2 2 7 2 2 2 2 21 21 21 16 22)
(define-deferred ucd-gc-entry-237
  (let ((offsets (bytevector 2 2 2 2 2 2 7 2 2 2 2 21 21 21 16 22)))
    (named-lambda (ucd-gc-entry-237 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 20 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-238
  (let ((offsets (bytevector 14 14 14 20 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-238 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-239
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-239 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 21 21 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-240
  (let ((offsets (bytevector 14 14 14 14 14 21 21 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-240 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 21 14 22 22 22 22 22)
(define-deferred ucd-gc-entry-241
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 21 14 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-241 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-242
  (let ((offsets (bytevector 14 14 14 14 14 14 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-242 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (237 234 14 14 238 14 14 239 240 14 241 14 14 14 14 242)
(define-deferred ucd-gc-entry-243
  (let ((offsets (bytevector 237 234 14 14 238 14 14 239 240 14 241 14 14 14 14 242)))
    (named-lambda (ucd-gc-entry-243 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 22)
(define-deferred ucd-gc-entry-244
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 22)))
    (named-lambda (ucd-gc-entry-244 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 24 24 24 24 21 21 24 24 24 22 22 22 22)
(define-deferred ucd-gc-entry-245
  (let ((offsets (bytevector 21 21 21 24 24 24 24 21 21 24 24 24 22 22 22 22)))
    (named-lambda (ucd-gc-entry-245 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 21 24 24 24 24 24 24 21 21 21 22 22 22 22)
(define-deferred ucd-gc-entry-246
  (let ((offsets (bytevector 24 24 21 24 24 24 24 24 24 21 21 21 22 22 22 22)))
    (named-lambda (ucd-gc-entry-246 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 22 22 22 2 2 8 8 8 8 8 8 8 8 8 8)
(define-deferred ucd-gc-entry-247
  (let ((offsets (bytevector 13 22 22 22 2 2 8 8 8 8 8 8 8 8 8 8)))
    (named-lambda (ucd-gc-entry-247 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 22 22)
(define-deferred ucd-gc-entry-248
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 22 22)))
    (named-lambda (ucd-gc-entry-248 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-249
  (let ((offsets (bytevector 14 14 14 14 14 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-249 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 22 22 22 22)
(define-deferred ucd-gc-entry-250
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 22 22 22 22)))
    (named-lambda (ucd-gc-entry-250 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-251
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-251 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 17 22 22 22 13 13)
(define-deferred ucd-gc-entry-252
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 17 22 22 22 13 13)))
    (named-lambda (ucd-gc-entry-252 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 244 245 246 247 14 248 249 14 14 250 14 251 252 13 13)
(define-deferred ucd-gc-entry-253
  (let ((offsets (bytevector 14 244 245 246 247 14 248 249 14 14 250 14 251 252 13 13)))
    (named-lambda (ucd-gc-entry-253 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 21 21 24 24 21 22 22 2 2)
(define-deferred ucd-gc-entry-254
  (let ((offsets (bytevector 14 14 14 14 14 14 14 21 21 24 24 21 22 22 2 2)))
    (named-lambda (ucd-gc-entry-254 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 24 21 24 21 21 21 21 21 21 21 22)
(define-deferred ucd-gc-entry-255
  (let ((offsets (bytevector 14 14 14 14 14 24 21 24 21 21 21 21 21 21 21 22)))
    (named-lambda (ucd-gc-entry-255 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 24 21 24 24 21 21 21 21 21 21 21 21 24 24 24)
(define-deferred ucd-gc-entry-256
  (let ((offsets (bytevector 21 24 21 24 24 21 21 21 21 21 21 21 21 24 24 24)))
    (named-lambda (ucd-gc-entry-256 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 21 21 21 21 21 21 21 21 21 21 22 22 21)
(define-deferred ucd-gc-entry-257
  (let ((offsets (bytevector 24 24 24 21 21 21 21 21 21 21 21 21 21 22 22 21)))
    (named-lambda (ucd-gc-entry-257 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 20 2 2 2 2 2 2 22 22)
(define-deferred ucd-gc-entry-258
  (let ((offsets (bytevector 2 2 2 2 2 2 2 20 2 2 2 2 2 2 22 22)))
    (named-lambda (ucd-gc-entry-258 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 21 21 21 21 21 23 22)
(define-deferred ucd-gc-entry-259
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 21 21 21 21 21 23 22)))
    (named-lambda (ucd-gc-entry-259 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 254 14 14 14 255 256 257 234 234 258 259 22 22 22 22)
(define-deferred ucd-gc-entry-260
  (let ((offsets (bytevector 0 240 0 0 0 241 242 243 220 220 244 245 8 8 8 8)))
    (named-lambda (ucd-gc-entry-260 sv table)
      ((vector-ref table (fix:+ 14 (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4))))) sv table))))

;;; (21 21 21 21 24 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-261
  (let ((offsets (bytevector 21 21 21 21 24 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-261 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 21 24 21 21 21 21 21 24 21 24 24 24)
(define-deferred ucd-gc-entry-262
  (let ((offsets (bytevector 14 14 14 14 21 24 21 21 21 21 21 24 21 24 24 24)))
    (named-lambda (ucd-gc-entry-262 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 21 24 24 14 14 14 14 14 14 14 22 22 22 22)
(define-deferred ucd-gc-entry-263
  (let ((offsets (bytevector 24 24 21 24 24 14 14 14 14 14 14 14 22 22 22 22)))
    (named-lambda (ucd-gc-entry-263 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 13 13 13 13 13 13 13 13 13 13 21 21 21 21 21)
(define-deferred ucd-gc-entry-264
  (let ((offsets (bytevector 2 13 13 13 13 13 13 13 13 13 13 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-264 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 13 13 13 13 13 13 13 13 13 22 22 22)
(define-deferred ucd-gc-entry-265
  (let ((offsets (bytevector 21 21 21 21 13 13 13 13 13 13 13 13 13 22 22 22)))
    (named-lambda (ucd-gc-entry-265 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 24 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-266
  (let ((offsets (bytevector 21 21 24 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-266 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 24 21 21 21 21 24 24 21 21 24 21 21 21 14 14)
(define-deferred ucd-gc-entry-267
  (let ((offsets (bytevector 14 24 21 21 21 21 24 24 21 21 24 21 21 21 14 14)))
    (named-lambda (ucd-gc-entry-267 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 21 24 21 21 24 24 24 21 24 21)
(define-deferred ucd-gc-entry-268
  (let ((offsets (bytevector 14 14 14 14 14 14 21 24 21 21 24 24 24 21 24 21)))
    (named-lambda (ucd-gc-entry-268 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 24 24 22 22 22 22 22 22 22 22 2 2 2 2)
(define-deferred ucd-gc-entry-269
  (let ((offsets (bytevector 21 21 24 24 22 22 22 22 22 22 22 22 2 2 2 2)))
    (named-lambda (ucd-gc-entry-269 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (261 14 14 262 263 197 264 265 266 14 267 94 14 14 268 269)
(define-deferred ucd-gc-entry-270
  (let ((offsets (bytevector 247 0 0 248 249 183 250 251 252 0 253 80 0 0 254 255)))
    (named-lambda (ucd-gc-entry-270 sv table)
      ((vector-ref table (fix:+ 14 (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4))))) sv table))))

;;; (14 14 14 14 24 24 24 24 24 24 24 24 21 21 21 21)
(define-deferred ucd-gc-entry-271
  (let ((offsets (bytevector 14 14 14 14 24 24 24 24 24 24 24 24 21 21 21 21)))
    (named-lambda (ucd-gc-entry-271 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 24 24 21 21 22 22 22 2 2 2 2 2)
(define-deferred ucd-gc-entry-272
  (let ((offsets (bytevector 21 21 21 21 24 24 21 21 22 22 22 2 2 2 2 2)))
    (named-lambda (ucd-gc-entry-272 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 22 22 22 14 14 14)
(define-deferred ucd-gc-entry-273
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 22 22 22 14 14 14)))
    (named-lambda (ucd-gc-entry-273 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 20 20 20 20 20 20 2 2)
(define-deferred ucd-gc-entry-274
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 20 20 20 20 20 20 2 2)))
    (named-lambda (ucd-gc-entry-274 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-275
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-275 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-276
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-276 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 2 21 21 21 21 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-277
  (let ((offsets (bytevector 21 21 21 2 21 21 21 21 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-277 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 24 21 21 21 21 21 21 21 14 14 14 14 21 14 14)
(define-deferred ucd-gc-entry-278
  (let ((offsets (bytevector 21 24 21 21 21 21 21 21 21 14 14 14 14 21 14 14)))
    (named-lambda (ucd-gc-entry-278 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 24 24 21 14 14 22 21 21 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-279
  (let ((offsets (bytevector 14 14 24 24 21 14 14 22 21 21 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-279 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 271 272 273 94 14 274 275 22 22 22 276 277 278 279)
(define-deferred ucd-gc-entry-280
  (let ((offsets (bytevector 14 0 14 0 15 1 16 1 17 1 94 0 14 0 18 1 19 1 22 0 22 0 22 0 20 1 21 1 22 1 23 1)))
    (named-lambda (ucd-gc-entry-280 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 12 20 20 20 20)
(define-deferred ucd-gc-entry-281
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 12 20 20 20 20)))
    (named-lambda (ucd-gc-entry-281 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 20 20 20 20 20 20 20 20 20 20 12 12 12 12 12)
(define-deferred ucd-gc-entry-282
  (let ((offsets (bytevector 20 20 20 20 20 20 20 20 20 20 20 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-282 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 20 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-283
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 20 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-283 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 20 20 20 20 20)
(define-deferred ucd-gc-entry-284
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 20 20 20 20 20)))
    (named-lambda (ucd-gc-entry-284 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 22 22 22 22 22 21 21 21 21 21)
(define-deferred ucd-gc-entry-285
  (let ((offsets (bytevector 21 21 21 21 21 21 22 22 22 22 22 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-285 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 281 20 20 20 282 283 12 284 20 20 21 21 21 285)
(define-deferred ucd-gc-entry-286
  (let ((offsets (bytevector 12 0 12 0 25 1 20 0 20 0 20 0 26 1 27 1 12 0 28 1 20 0 20 0 21 0 21 0 21 0 29 1)))
    (named-lambda (ucd-gc-entry-286 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (9 12 9 12 9 12 12 12 12 12 12 12 12 12 9 12)
(define-deferred ucd-gc-entry-287
  (let ((offsets (bytevector 9 12 9 12 9 12 12 12 12 12 12 12 12 12 9 12)))
    (named-lambda (ucd-gc-entry-287 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (40 40 40 40 40 40 40 40 40 287 40 40 40 40 40 40)
(define-deferred ucd-gc-entry-288
  (let ((offsets (bytevector 0 0 0 0 0 0 0 0 0 247 0 0 0 0 0 0)))
    (named-lambda (ucd-gc-entry-288 sv table)
      ((vector-ref table (fix:+ 40 (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4))))) sv table))))

;;; (12 12 12 12 12 12 12 12 9 9 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-289
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 9 9 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-289 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 22 22 9 9 9 9 9 9 22 22)
(define-deferred ucd-gc-entry-290
  (let ((offsets (bytevector 12 12 12 12 12 12 22 22 9 9 9 9 9 9 22 22)))
    (named-lambda (ucd-gc-entry-290 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 22 9 22 9 22 9 22 9)
(define-deferred ucd-gc-entry-291
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 22 9 22 9 22 9 22 9)))
    (named-lambda (ucd-gc-entry-291 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 12 12 12 22 22)
(define-deferred ucd-gc-entry-292
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 12 12 12 22 22)))
    (named-lambda (ucd-gc-entry-292 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 19 19 19 19 19 19 19 19)
(define-deferred ucd-gc-entry-293
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 19 19 19 19 19 19 19 19)))
    (named-lambda (ucd-gc-entry-293 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 22 12 12 9 9 9 9 19 10 12 10)
(define-deferred ucd-gc-entry-294
  (let ((offsets (bytevector 12 12 12 12 12 22 12 12 9 9 9 9 19 10 12 10)))
    (named-lambda (ucd-gc-entry-294 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (10 10 12 12 12 22 12 12 9 9 9 9 19 10 10 10)
(define-deferred ucd-gc-entry-295
  (let ((offsets (bytevector 10 10 12 12 12 22 12 12 9 9 9 9 19 10 10 10)))
    (named-lambda (ucd-gc-entry-295 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 22 22 12 12 9 9 9 9 22 10 10 10)
(define-deferred ucd-gc-entry-296
  (let ((offsets (bytevector 12 12 12 12 22 22 12 12 9 9 9 9 22 10 10 10)))
    (named-lambda (ucd-gc-entry-296 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 9 9 9 9 9 10 10 10)
(define-deferred ucd-gc-entry-297
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 9 9 9 9 9 10 10 10)))
    (named-lambda (ucd-gc-entry-297 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 12 12 12 22 12 12 9 9 9 9 19 10 10 22)
(define-deferred ucd-gc-entry-298
  (let ((offsets (bytevector 22 22 12 12 12 22 12 12 9 9 9 9 19 10 10 22)))
    (named-lambda (ucd-gc-entry-298 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (289 290 289 289 290 291 289 292 293 293 293 294 295 296 297 298)
(define-deferred ucd-gc-entry-299
  (let ((offsets (bytevector 0 1 0 0 1 2 0 3 4 4 4 5 6 7 8 9)))
    (named-lambda (ucd-gc-entry-299 sv table)
      ((vector-ref table (fix:+ 289 (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4))))) sv table))))

;;; (1 1 1 1 1 1 1 1 1 1 1 16 16 16 16 16)
(define-deferred ucd-gc-entry-300
  (let ((offsets (bytevector 1 1 1 1 1 1 1 1 1 1 1 16 16 16 16 16)))
    (named-lambda (ucd-gc-entry-300 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (7 7 7 7 7 7 2 2 15 18 4 15 15 18 4 15)
(define-deferred ucd-gc-entry-301
  (let ((offsets (bytevector 7 7 7 7 7 7 2 2 15 18 4 15 15 18 4 15)))
    (named-lambda (ucd-gc-entry-301 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 26 27 16 16 16 16 16 1)
(define-deferred ucd-gc-entry-302
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 26 27 16 16 16 16 16 1)))
    (named-lambda (ucd-gc-entry-302 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 2 15 18 2 2 2 2 11)
(define-deferred ucd-gc-entry-303
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 2 15 18 2 2 2 2 11)))
    (named-lambda (ucd-gc-entry-303 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (11 2 2 2 6 4 5 2 2 2 2 2 2 2 2 2)
(define-deferred ucd-gc-entry-304
  (let ((offsets (bytevector 11 2 2 2 6 4 5 2 2 2 2 2 2 2 2 2)))
    (named-lambda (ucd-gc-entry-304 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 6 2 11 2 2 2 2 2 2 2 2 2 2 1)
(define-deferred ucd-gc-entry-305
  (let ((offsets (bytevector 2 2 6 2 11 2 2 2 2 2 2 2 2 2 2 1)))
    (named-lambda (ucd-gc-entry-305 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (16 16 16 16 16 22 16 16 16 16 16 16 16 16 16 16)
(define-deferred ucd-gc-entry-306
  (let ((offsets (bytevector 16 16 16 16 16 22 16 16 16 16 16 16 16 16 16 16)))
    (named-lambda (ucd-gc-entry-306 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 20 22 22 17 17 17 17 17 17 6 6 6 4 5 20)
(define-deferred ucd-gc-entry-307
  (let ((offsets (bytevector 17 20 22 22 17 17 17 17 17 17 6 6 6 4 5 20)))
    (named-lambda (ucd-gc-entry-307 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 17 17 17 17 6 6 6 4 5 22)
(define-deferred ucd-gc-entry-308
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 17 17 6 6 6 4 5 22)))
    (named-lambda (ucd-gc-entry-308 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 20 20 20 20 20 20 20 20 20 20 20 20 22 22 22)
(define-deferred ucd-gc-entry-309
  (let ((offsets (bytevector 20 20 20 20 20 20 20 20 20 20 20 20 20 22 22 22)))
    (named-lambda (ucd-gc-entry-309 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 22)
(define-deferred ucd-gc-entry-310
  (let ((offsets (bytevector 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 22)))
    (named-lambda (ucd-gc-entry-310 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 21 21 21 21 23 23 23)
(define-deferred ucd-gc-entry-311
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 21 21 21 21 23 23 23)))
    (named-lambda (ucd-gc-entry-311 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (23 21 23 23 23 21 21 21 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-312
  (let ((offsets (bytevector 23 21 23 23 23 21 21 21 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-312 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-313
  (let ((offsets (bytevector 21 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-313 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (300 301 302 303 304 305 306 307 308 309 3 310 22 311 312 313)
(define-deferred ucd-gc-entry-314
  (let ((offsets (bytevector 44 1 45 1 46 1 47 1 48 1 49 1 50 1 51 1 52 1 53 1 3 0 54 1 22 0 55 1 56 1 57 1)))
    (named-lambda (ucd-gc-entry-314 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 9 13 13 13 13 9 13 13 12 9 9 9 12 12)
(define-deferred ucd-gc-entry-315
  (let ((offsets (bytevector 13 13 9 13 13 13 13 9 13 13 12 9 9 9 12 12)))
    (named-lambda (ucd-gc-entry-315 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 12 13 9 13 13 6 9 9 9 9 9 13 13)
(define-deferred ucd-gc-entry-316
  (let ((offsets (bytevector 9 9 9 12 13 9 13 13 6 9 9 9 9 9 13 13)))
    (named-lambda (ucd-gc-entry-316 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 9 13 9 13 9 13 9 9 9 9 13 12)
(define-deferred ucd-gc-entry-317
  (let ((offsets (bytevector 13 13 13 13 9 13 9 13 9 13 9 9 9 9 13 12)))
    (named-lambda (ucd-gc-entry-317 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 12 14 14 14 14 12 13 13 12 12 9 9)
(define-deferred ucd-gc-entry-318
  (let ((offsets (bytevector 9 9 9 9 12 14 14 14 14 12 13 13 12 12 9 9)))
    (named-lambda (ucd-gc-entry-318 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 6 6 6 9 12 12 12 12 13 6 13 13 12 13)
(define-deferred ucd-gc-entry-319
  (let ((offsets (bytevector 6 6 6 6 6 9 12 12 12 12 13 6 13 13 12 13)))
    (named-lambda (ucd-gc-entry-319 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (25 25 25 9 12 25 25 25 25 17 13 13 22 22 22 22)
(define-deferred ucd-gc-entry-320
  (let ((offsets (bytevector 25 25 25 9 12 25 25 25 25 17 13 13 22 22 22 22)))
    (named-lambda (ucd-gc-entry-320 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 6 6 6 13 13 13 13 13 6 6 13 13 13 13)
(define-deferred ucd-gc-entry-321
  (let ((offsets (bytevector 6 6 6 6 6 13 13 13 13 13 6 6 13 13 13 13)))
    (named-lambda (ucd-gc-entry-321 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 13 13 6 13 13 6 13 13 13 13 13 13 13 6 13)
(define-deferred ucd-gc-entry-322
  (let ((offsets (bytevector 6 13 13 6 13 13 6 13 13 13 13 13 13 13 6 13)))
    (named-lambda (ucd-gc-entry-322 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 13 6 6)
(define-deferred ucd-gc-entry-323
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 13 13 6 6)))
    (named-lambda (ucd-gc-entry-323 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 6 13 6 13 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-324
  (let ((offsets (bytevector 13 13 6 13 6 13 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-324 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 6 6 6 6 6 6 6 6 6 6 6 6)
(define-deferred ucd-gc-entry-325
  (let ((offsets (bytevector 13 13 13 13 6 6 6 6 6 6 6 6 6 6 6 6)))
    (named-lambda (ucd-gc-entry-325 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (315 316 317 318 319 17 25 25 320 321 322 13 323 324 13 325)
(define-deferred ucd-gc-entry-326
  (let ((offsets (bytevector 59 1 60 1 61 1 62 1 63 1 17 0 25 0 25 0 64 1 65 1 66 1 13 0 67 1 68 1 13 0 69 1)))
    (named-lambda (ucd-gc-entry-326 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 13 4 5 4 5 13 13 13 13)
(define-deferred ucd-gc-entry-327
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 4 5 4 5 13 13 13 13)))
    (named-lambda (ucd-gc-entry-327 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 13 13 13 13 13 13 13 4 5 13 13 13 13 13)
(define-deferred ucd-gc-entry-328
  (let ((offsets (bytevector 6 6 13 13 13 13 13 13 13 4 5 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-328 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 6 13 13 13)
(define-deferred ucd-gc-entry-329
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 6 13 13 13)))
    (named-lambda (ucd-gc-entry-329 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 6 6 6 6 6)
(define-deferred ucd-gc-entry-330
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 6 6 6 6 6)))
    (named-lambda (ucd-gc-entry-330 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 6 6 13 13 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-331
  (let ((offsets (bytevector 6 6 6 6 13 13 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-331 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 6 6 6 6)
(define-deferred ucd-gc-entry-332
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 6 6 6 6)))
    (named-lambda (ucd-gc-entry-332 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 13 13 13 13 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-333
  (let ((offsets (bytevector 6 6 13 13 13 13 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-333 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 22)
(define-deferred ucd-gc-entry-334
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 22)))
    (named-lambda (ucd-gc-entry-334 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (327 13 328 13 13 13 13 329 13 330 6 331 13 332 333 334)
(define-deferred ucd-gc-entry-335
  (let ((offsets (bytevector 71 1 13 0 72 1 13 0 13 0 13 0 13 0 73 1 13 0 74 1 6 0 75 1 13 0 76 1 77 1 78 1)))
    (named-lambda (ucd-gc-entry-335 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-336
  (let ((offsets (bytevector 13 13 13 13 13 13 13 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-336 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 22 22 22 22 22)
(define-deferred ucd-gc-entry-337
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-337 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 17 17 17 17 17 17 13 13 13 13)
(define-deferred ucd-gc-entry-338
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 17 17 17 17 13 13 13 13)))
    (named-lambda (ucd-gc-entry-338 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-339
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-339 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 336 22 337 22 17 17 17 338 13 13 13 13 339 17)
(define-deferred ucd-gc-entry-340
  (let ((offsets (bytevector 13 0 13 0 80 1 22 0 81 1 22 0 17 0 17 0 17 0 82 1 13 0 13 0 13 0 13 0 83 1 17 0)))
    (named-lambda (ucd-gc-entry-340 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 6 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-341
  (let ((offsets (bytevector 13 13 13 13 13 13 13 6 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-341 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 6 13 13 13 13 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-342
  (let ((offsets (bytevector 13 6 13 13 13 13 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-342 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 6 6 6 6 6 6 6 6)
(define-deferred ucd-gc-entry-343
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 6 6 6 6 6 6 6 6)))
    (named-lambda (ucd-gc-entry-343 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 341 342 13 13 343)
(define-deferred ucd-gc-entry-344
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 85 1 86 1 13 0 13 0 87 1)))
    (named-lambda (ucd-gc-entry-344 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 6)
(define-deferred ucd-gc-entry-345
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 6)))
    (named-lambda (ucd-gc-entry-345 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 345 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-346
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 13 0 89 1 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0)))
    (named-lambda (ucd-gc-entry-346 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 13 4 5 4 5 4 5 4 5)
(define-deferred ucd-gc-entry-347
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 4 5 4 5 4 5 4 5)))
    (named-lambda (ucd-gc-entry-347 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (4 5 4 5 4 5 17 17 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-348
  (let ((offsets (bytevector 4 5 4 5 4 5 17 17 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-348 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 13 13 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-349
  (let ((offsets (bytevector 17 17 17 17 13 13 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-349 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 6 6 6 4 5 6 6 6 6 6 6 6 6 6)
(define-deferred ucd-gc-entry-350
  (let ((offsets (bytevector 6 6 6 6 6 4 5 6 6 6 6 6 6 6 6 6)))
    (named-lambda (ucd-gc-entry-350 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 6 6 6 6 4 5 4 5 4 5 4 5 4 5)
(define-deferred ucd-gc-entry-351
  (let ((offsets (bytevector 6 6 6 6 6 6 4 5 4 5 4 5 4 5 4 5)))
    (named-lambda (ucd-gc-entry-351 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 347 348 17 349 13 13 350 6 351 6)
(define-deferred ucd-gc-entry-352
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 13 0 91 1 92 1 17 0 93 1 13 0 13 0 94 1 6 0 95 1 6 0)))
    (named-lambda (ucd-gc-entry-352 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (6 6 6 4 5 4 5 4 5 4 5 4 5 4 5 4)
(define-deferred ucd-gc-entry-353
  (let ((offsets (bytevector 6 6 6 4 5 4 5 4 5 4 5 4 5 4 5 4)))
    (named-lambda (ucd-gc-entry-353 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (5 4 5 4 5 4 5 4 5 6 6 6 6 6 6 6)
(define-deferred ucd-gc-entry-354
  (let ((offsets (bytevector 5 4 5 4 5 4 5 4 5 6 6 6 6 6 6 6)))
    (named-lambda (ucd-gc-entry-354 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 6 6 6 6 6 6 4 5 4 5 6 6 6 6)
(define-deferred ucd-gc-entry-355
  (let ((offsets (bytevector 6 6 6 6 6 6 6 6 4 5 4 5 6 6 6 6)))
    (named-lambda (ucd-gc-entry-355 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 6 6 6 6 6 6 6 6 6 6 4 5 6 6)
(define-deferred ucd-gc-entry-356
  (let ((offsets (bytevector 6 6 6 6 6 6 6 6 6 6 6 6 4 5 6 6)))
    (named-lambda (ucd-gc-entry-356 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 6 6 6 6 6 6 353 354 6 6 6 355 6 356)
(define-deferred ucd-gc-entry-357
  (let ((offsets (bytevector 6 0 6 0 6 0 6 0 6 0 6 0 6 0 6 0 97 1 98 1 6 0 6 0 6 0 99 1 6 0 100 1)))
    (named-lambda (ucd-gc-entry-357 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (6 6 6 6 6 13 13 6 6 6 6 6 6 13 13 13)
(define-deferred ucd-gc-entry-358
  (let ((offsets (bytevector 6 6 6 6 6 13 13 6 6 6 6 6 6 13 13 13)))
    (named-lambda (ucd-gc-entry-358 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 22 22 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-359
  (let ((offsets (bytevector 13 13 13 13 22 22 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-359 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 22 22 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-360
  (let ((offsets (bytevector 13 13 13 13 13 13 22 22 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-360 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 22 22 22 13 13 13)
(define-deferred ucd-gc-entry-361
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 22 22 22 13 13 13)))
    (named-lambda (ucd-gc-entry-361 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 22 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-362
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 22 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-362 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-363
  (let ((offsets (bytevector 13 13 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-363 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 22 22 22 22 13 13 13 13)
(define-deferred ucd-gc-entry-364
  (let ((offsets (bytevector 22 22 22 22 22 22 22 22 22 22 22 22 13 13 13 13)))
    (named-lambda (ucd-gc-entry-364 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 6 358 13 13 359 13 360 13 361 362 363 364 22)
(define-deferred ucd-gc-entry-365
  (let ((offsets (bytevector 13 0 13 0 13 0 6 0 102 1 13 0 13 0 103 1 13 0 104 1 13 0 105 1 106 1 107 1 108 1 22 0)))
    (named-lambda (ucd-gc-entry-365 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 22)
(define-deferred ucd-gc-entry-366
  (let ((offsets (bytevector 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 22)))
    (named-lambda (ucd-gc-entry-366 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 22)
(define-deferred ucd-gc-entry-367
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 22)))
    (named-lambda (ucd-gc-entry-367 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 9 9 12 12 9 12 9 12 9 12 9 9 9)
(define-deferred ucd-gc-entry-368
  (let ((offsets (bytevector 9 12 9 9 9 12 12 9 12 9 12 9 12 9 9 9)))
    (named-lambda (ucd-gc-entry-368 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 12 9 12 12 12 12 12 12 20 20 9 9)
(define-deferred ucd-gc-entry-369
  (let ((offsets (bytevector 9 12 9 12 12 9 12 12 12 12 12 12 20 20 9 9)))
    (named-lambda (ucd-gc-entry-369 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 12 13 13 13 13 13 13 9 12 9 12 21)
(define-deferred ucd-gc-entry-370
  (let ((offsets (bytevector 9 12 9 12 12 13 13 13 13 13 13 9 12 9 12 21)))
    (named-lambda (ucd-gc-entry-370 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 9 12 22 22 22 22 22 2 2 2 2 17 2 2)
(define-deferred ucd-gc-entry-371
  (let ((offsets (bytevector 21 21 9 12 22 22 22 22 22 2 2 2 2 17 2 2)))
    (named-lambda (ucd-gc-entry-371 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 366 12 12 367 368 369 40 40 40 40 40 40 370 371)
(define-deferred ucd-gc-entry-372
  (let ((offsets (bytevector 9 0 9 0 110 1 12 0 12 0 111 1 112 1 113 1 40 0 40 0 40 0 40 0 40 0 40 0 114 1 115 1)))
    (named-lambda (ucd-gc-entry-372 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (12 12 12 12 12 12 22 12 22 22 22 22 22 12 22 22)
(define-deferred ucd-gc-entry-373
  (let ((offsets (bytevector 12 12 12 12 12 12 22 12 22 22 22 22 22 12 22 22)))
    (named-lambda (ucd-gc-entry-373 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 22 22 22 22 22 22 22 20)
(define-deferred ucd-gc-entry-374
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 22 22 22 22 22 22 22 20)))
    (named-lambda (ucd-gc-entry-374 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 22 22 22 22 22 22 22 22 22 22 22 22 22 22 21)
(define-deferred ucd-gc-entry-375
  (let ((offsets (bytevector 2 22 22 22 22 22 22 22 22 22 22 22 22 22 22 21)))
    (named-lambda (ucd-gc-entry-375 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-376
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-376 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 14 14 14 14 14 14 14 22)
(define-deferred ucd-gc-entry-377
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 14 14 14 14 14 14 14 22)))
    (named-lambda (ucd-gc-entry-377 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 373 14 14 14 374 375 14 376 377 377 377 377 21 21)
(define-deferred ucd-gc-entry-378
  (let ((offsets (bytevector 12 0 12 0 117 1 14 0 14 0 14 0 118 1 119 1 14 0 120 1 121 1 121 1 121 1 121 1 21 0 21 0)))
    (named-lambda (ucd-gc-entry-378 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (2 2 15 18 15 18 2 2 2 15 18 2 15 18 2 2)
(define-deferred ucd-gc-entry-379
  (let ((offsets (bytevector 2 2 15 18 15 18 2 2 2 15 18 2 15 18 2 2)))
    (named-lambda (ucd-gc-entry-379 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 7 2 2 7 2 15 18 2 2)
(define-deferred ucd-gc-entry-380
  (let ((offsets (bytevector 2 2 2 2 2 2 2 7 2 2 7 2 15 18 2 2)))
    (named-lambda (ucd-gc-entry-380 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (15 18 4 5 4 5 4 5 4 5 2 2 2 2 2 20)
(define-deferred ucd-gc-entry-381
  (let ((offsets (bytevector 15 18 4 5 4 5 4 5 4 5 2 2 2 2 2 20)))
    (named-lambda (ucd-gc-entry-381 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 2 2 7 7 2 2 2 2)
(define-deferred ucd-gc-entry-382
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 2 2 7 7 2 2 2 2)))
    (named-lambda (ucd-gc-entry-382 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (7 2 4 2 2 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-383
  (let ((offsets (bytevector 7 2 4 2 2 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-383 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 22 13 13 13 13 13)
(define-deferred ucd-gc-entry-384
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 22 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-384 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-385
  (let ((offsets (bytevector 13 13 13 13 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-385 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (379 380 381 382 383 22 22 22 13 384 13 13 13 13 13 385)
(define-deferred ucd-gc-entry-386
  (let ((offsets (bytevector 123 1 124 1 125 1 126 1 127 1 22 0 22 0 22 0 13 0 128 1 13 0 13 0 13 0 13 0 13 0 129 1)))
    (named-lambda (ucd-gc-entry-386 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-387
  (let ((offsets (bytevector 13 13 13 13 13 13 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-387 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 22 22 22 22)
(define-deferred ucd-gc-entry-388
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 22 22 22 22)))
    (named-lambda (ucd-gc-entry-388 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 387 22 388)
(define-deferred ucd-gc-entry-389
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 131 1 22 0 132 1)))
    (named-lambda (ucd-gc-entry-389 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (1 2 2 2 13 20 14 25 4 5 4 5 4 5 4 5)
(define-deferred ucd-gc-entry-390
  (let ((offsets (bytevector 1 2 2 2 13 20 14 25 4 5 4 5 4 5 4 5)))
    (named-lambda (ucd-gc-entry-390 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (4 5 13 13 4 5 4 5 4 5 4 5 7 4 5 5)
(define-deferred ucd-gc-entry-391
  (let ((offsets (bytevector 4 5 13 13 4 5 4 5 4 5 4 5 7 4 5 5)))
    (named-lambda (ucd-gc-entry-391 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 25 25 25 25 25 25 25 25 25 21 21 21 21 24 24)
(define-deferred ucd-gc-entry-392
  (let ((offsets (bytevector 13 25 25 25 25 25 25 25 25 25 21 21 21 21 24 24)))
    (named-lambda (ucd-gc-entry-392 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (7 20 20 20 20 20 13 13 25 25 25 20 14 2 13 13)
(define-deferred ucd-gc-entry-393
  (let ((offsets (bytevector 7 20 20 20 20 20 13 13 25 25 25 20 14 2 13 13)))
    (named-lambda (ucd-gc-entry-393 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 22 21 21 10 10 20 20 14)
(define-deferred ucd-gc-entry-394
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 22 21 21 10 10 20 20 14)))
    (named-lambda (ucd-gc-entry-394 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 2 20 20 20 14)
(define-deferred ucd-gc-entry-395
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 2 20 20 20 14)))
    (named-lambda (ucd-gc-entry-395 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (390 391 392 393 171 14 14 14 14 394 218 14 14 14 14 395)
(define-deferred ucd-gc-entry-396
  (let ((offsets (bytevector 134 1 135 1 136 1 137 1 171 0 14 0 14 0 14 0 14 0 138 1 218 0 14 0 14 0 14 0 14 0 139 1)))
    (named-lambda (ucd-gc-entry-396 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (22 22 22 22 22 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-397
  (let ((offsets (bytevector 22 22 22 22 22 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-397 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 17 17 17 17 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-398
  (let ((offsets (bytevector 13 13 17 17 17 17 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-398 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (397 14 248 171 14 14 14 14 244 398 14 77 13 13 385 14)
(define-deferred ucd-gc-entry-399
  (let ((offsets (bytevector 141 1 14 0 248 0 171 0 14 0 14 0 14 0 14 0 244 0 142 1 14 0 77 0 13 0 13 0 129 1 14 0)))
    (named-lambda (ucd-gc-entry-399 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (17 17 17 17 17 17 17 17 17 17 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-400
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 17 17 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-400 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-401
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-401 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-402
  (let ((offsets (bytevector 13 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-402 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 334 400 13 401 402 13 13 400 13 13 402 13 13 13 334)
(define-deferred ucd-gc-entry-403
  (let ((offsets (bytevector 13 0 78 1 144 1 13 0 145 1 146 1 13 0 13 0 144 1 13 0 13 0 146 1 13 0 13 0 13 0 78 1)))
    (named-lambda (ucd-gc-entry-403 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 242 13 13 13 13)
(define-deferred ucd-gc-entry-404
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 242 13 13 13 13)))
    (named-lambda (ucd-gc-entry-404 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 242 22 22)
(define-deferred ucd-gc-entry-405
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 242 22 22)))
    (named-lambda (ucd-gc-entry-405 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 20 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-406
  (let ((offsets (bytevector 14 14 14 14 14 20 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-406 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 406 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-407
  (let ((offsets (bytevector 14 0 150 1 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0)))
    (named-lambda (ucd-gc-entry-407 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 187 13 13 13 336 14 14 274)
(define-deferred ucd-gc-entry-408
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 187 0 13 0 13 0 13 0 80 1 14 0 14 0 18 1)))
    (named-lambda (ucd-gc-entry-408 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 20 2 2 2)
(define-deferred ucd-gc-entry-409
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 20 2 2 2)))
    (named-lambda (ucd-gc-entry-409 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 14 14 22 22 22 22)
(define-deferred ucd-gc-entry-410
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 14 14 22 22 22 22)))
    (named-lambda (ucd-gc-entry-410 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 9 12 9 12 9 12 9 12 9 12 14 21)
(define-deferred ucd-gc-entry-411
  (let ((offsets (bytevector 9 12 9 12 9 12 9 12 9 12 9 12 9 12 14 21)))
    (named-lambda (ucd-gc-entry-411 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (23 23 23 2 21 21 21 21 21 21 21 21 21 21 2 20)
(define-deferred ucd-gc-entry-412
  (let ((offsets (bytevector 23 23 23 2 21 21 21 21 21 21 21 21 21 21 2 20)))
    (named-lambda (ucd-gc-entry-412 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 9 12 9 12 9 12 9 12 20 20 21 21)
(define-deferred ucd-gc-entry-413
  (let ((offsets (bytevector 9 12 9 12 9 12 9 12 9 12 9 12 20 20 21 21)))
    (named-lambda (ucd-gc-entry-413 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 25 25 25 25 25 25 25 25 25 25)
(define-deferred ucd-gc-entry-414
  (let ((offsets (bytevector 14 14 14 14 14 14 25 25 25 25 25 25 25 25 25 25)))
    (named-lambda (ucd-gc-entry-414 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 2 2 2 2 2 2 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-415
  (let ((offsets (bytevector 21 21 2 2 2 2 2 2 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-415 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (409 14 410 22 40 40 411 412 40 413 14 14 14 14 414 415)
(define-deferred ucd-gc-entry-416
  (let ((offsets (bytevector 153 1 14 0 154 1 22 0 40 0 40 0 155 1 156 1 40 0 157 1 14 0 14 0 14 0 14 0 158 1 159 1)))
    (named-lambda (ucd-gc-entry-416 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (10 10 10 10 10 10 10 20 20 20 20 20 20 20 20 20)
(define-deferred ucd-gc-entry-417
  (let ((offsets (bytevector 10 10 10 10 10 10 10 20 20 20 20 20 20 20 20 20)))
    (named-lambda (ucd-gc-entry-417 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (10 10 9 12 9 12 9 12 9 12 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-418
  (let ((offsets (bytevector 10 10 9 12 9 12 9 12 9 12 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-418 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 9 12 9 12 9 12 9 12 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-419
  (let ((offsets (bytevector 12 12 9 12 9 12 9 12 9 12 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-419 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 12 12 12 12 12 12 12 12 9 12 9 12 9 9 12)
(define-deferred ucd-gc-entry-420
  (let ((offsets (bytevector 20 12 12 12 12 12 12 12 12 9 12 9 12 9 9 12)))
    (named-lambda (ucd-gc-entry-420 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 9 12 9 12 20 10 10 9 12 9 12 14)
(define-deferred ucd-gc-entry-421
  (let ((offsets (bytevector 9 12 9 12 9 12 9 12 20 10 10 9 12 9 12 14)))
    (named-lambda (ucd-gc-entry-421 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 12 12 9 12 9 12 9 12 9 12 9 12)
(define-deferred ucd-gc-entry-422
  (let ((offsets (bytevector 9 12 9 12 12 12 9 12 9 12 9 12 9 12 9 12)))
    (named-lambda (ucd-gc-entry-422 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 12 9 12 9 12 9 12 9 12 9 9 9 9 9 22)
(define-deferred ucd-gc-entry-423
  (let ((offsets (bytevector 9 12 9 12 9 12 9 12 9 12 9 9 9 9 9 22)))
    (named-lambda (ucd-gc-entry-423 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 12 9 12 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-424
  (let ((offsets (bytevector 9 9 9 9 9 12 9 12 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-424 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 14 20 20 12 14 14 14 14 14)
(define-deferred ucd-gc-entry-425
  (let ((offsets (bytevector 22 22 22 22 22 22 22 14 20 20 12 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-425 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (10 417 418 419 40 40 40 420 421 422 423 424 22 22 22 425)
(define-deferred ucd-gc-entry-426
  (let ((offsets (bytevector 10 0 161 1 162 1 163 1 40 0 40 0 40 0 164 1 165 1 166 1 167 1 168 1 22 0 22 0 22 0 169 1)))
    (named-lambda (ucd-gc-entry-426 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 21 14 14 14 21 14 14 14 14 21 14 14 14 14)
(define-deferred ucd-gc-entry-427
  (let ((offsets (bytevector 14 14 21 14 14 14 21 14 14 14 14 21 14 14 14 14)))
    (named-lambda (ucd-gc-entry-427 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 24 24 21 21 24 13 13 13 13 22 22 22 22)
(define-deferred ucd-gc-entry-428
  (let ((offsets (bytevector 14 14 14 24 24 21 21 24 13 13 13 13 22 22 22 22)))
    (named-lambda (ucd-gc-entry-428 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 13 13 3 13 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-429
  (let ((offsets (bytevector 17 17 17 17 17 17 13 13 3 13 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-429 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 2 2 2 2 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-430
  (let ((offsets (bytevector 14 14 14 14 2 2 2 2 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-430 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-431
  (let ((offsets (bytevector 24 24 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-431 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 24 24 24 24 24 24 24 24 24 24 24 24)
(define-deferred ucd-gc-entry-432
  (let ((offsets (bytevector 14 14 14 14 24 24 24 24 24 24 24 24 24 24 24 24)))
    (named-lambda (ucd-gc-entry-432 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 24 21 21 22 22 22 22 22 22 22 22 2 2)
(define-deferred ucd-gc-entry-433
  (let ((offsets (bytevector 24 24 24 24 21 21 22 22 22 22 22 22 22 22 2 2)))
    (named-lambda (ucd-gc-entry-433 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 14 14 14 14 14 14 2 2 2 14 2 14 22 22)
(define-deferred ucd-gc-entry-434
  (let ((offsets (bytevector 21 21 14 14 14 14 14 14 2 2 2 14 2 14 22 22)))
    (named-lambda (ucd-gc-entry-434 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (427 14 428 429 14 14 14 430 431 14 14 432 433 234 21 434)
(define-deferred ucd-gc-entry-435
  (let ((offsets (bytevector 171 1 14 0 172 1 173 1 14 0 14 0 14 0 174 1 175 1 14 0 14 0 176 1 177 1 234 0 21 0 178 1)))
    (named-lambda (ucd-gc-entry-435 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 21 21 21 21 21 21 21 21 2 2)
(define-deferred ucd-gc-entry-436
  (let ((offsets (bytevector 14 14 14 14 14 14 21 21 21 21 21 21 21 21 2 2)))
    (named-lambda (ucd-gc-entry-436 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 21 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-437
  (let ((offsets (bytevector 14 14 14 14 14 14 14 21 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-437 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 24 24 22 22 22 22 22 22 22 22 22 22 22 2)
(define-deferred ucd-gc-entry-438
  (let ((offsets (bytevector 21 21 24 24 22 22 22 22 22 22 22 22 22 22 22 2)))
    (named-lambda (ucd-gc-entry-438 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 21 24 24 21 21 21 21 24 24 21 24 24 24)
(define-deferred ucd-gc-entry-439
  (let ((offsets (bytevector 14 14 14 21 24 24 21 21 21 21 24 24 21 24 24 24)))
    (named-lambda (ucd-gc-entry-439 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 2 2 2 2 2 2 2 2 2 2 2 2 2 22 20)
(define-deferred ucd-gc-entry-440
  (let ((offsets (bytevector 24 2 2 2 2 2 2 2 2 2 2 2 2 2 22 20)))
    (named-lambda (ucd-gc-entry-440 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 22 22 22 22 2 2)
(define-deferred ucd-gc-entry-441
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 22 22 22 22 2 2)))
    (named-lambda (ucd-gc-entry-441 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 21 20 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-442
  (let ((offsets (bytevector 14 14 14 14 14 21 20 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-442 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 14 14 14 14 14 22)
(define-deferred ucd-gc-entry-443
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 14 14 14 14 14 22)))
    (named-lambda (ucd-gc-entry-443 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (94 14 436 14 437 438 14 187 106 14 14 439 440 441 442 443)
(define-deferred ucd-gc-entry-444
  (let ((offsets (bytevector 94 0 14 0 180 1 14 0 181 1 182 1 14 0 187 0 106 0 14 0 14 0 183 1 184 1 185 1 186 1 187 1)))
    (named-lambda (ucd-gc-entry-444 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 21 21 21 21 21 21 24)
(define-deferred ucd-gc-entry-445
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 21 21 21 21 21 21 24)))
    (named-lambda (ucd-gc-entry-445 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 21 21 24 24 21 21 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-446
  (let ((offsets (bytevector 24 21 21 24 24 21 21 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-446 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 21 14 14 14 14 14 14 14 14 21 24 22 22)
(define-deferred ucd-gc-entry-447
  (let ((offsets (bytevector 14 14 14 21 14 14 14 14 14 14 14 14 21 24 22 22)))
    (named-lambda (ucd-gc-entry-447 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 22 22 2 2 2 2)
(define-deferred ucd-gc-entry-448
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 22 22 2 2 2 2)))
    (named-lambda (ucd-gc-entry-448 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 14 14 14 14 14 14 13 13 13 14 24 21 24 14 14)
(define-deferred ucd-gc-entry-449
  (let ((offsets (bytevector 20 14 14 14 14 14 14 13 13 13 14 24 21 24 14 14)))
    (named-lambda (ucd-gc-entry-449 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 14 21 21 21 14 14 21 21 14 14 14 14 14 21 21)
(define-deferred ucd-gc-entry-450
  (let ((offsets (bytevector 21 14 21 21 21 14 14 21 21 14 14 14 14 14 21 21)))
    (named-lambda (ucd-gc-entry-450 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 21 14 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-451
  (let ((offsets (bytevector 14 21 14 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-451 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 22 22 22 14 14 20 2 2)
(define-deferred ucd-gc-entry-452
  (let ((offsets (bytevector 22 22 22 22 22 22 22 22 22 22 22 14 14 20 2 2)))
    (named-lambda (ucd-gc-entry-452 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 24 21 21 24 24)
(define-deferred ucd-gc-entry-453
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 24 21 21 24 24)))
    (named-lambda (ucd-gc-entry-453 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 14 20 20 24 21 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-454
  (let ((offsets (bytevector 2 2 14 20 20 24 21 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-454 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 445 446 447 448 14 449 14 14 14 450 451 452 453 454)
(define-deferred ucd-gc-entry-455
  (let ((offsets (bytevector 14 0 14 0 189 1 190 1 191 1 192 1 14 0 193 1 14 0 14 0 14 0 194 1 195 1 196 1 197 1 198 1)))
    (named-lambda (ucd-gc-entry-455 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (22 14 14 14 14 14 14 22 22 14 14 14 14 14 14 22)
(define-deferred ucd-gc-entry-456
  (let ((offsets (bytevector 22 14 14 14 14 14 14 22 22 14 14 14 14 14 14 22)))
    (named-lambda (ucd-gc-entry-456 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 14 14 14 14 14 14 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-457
  (let ((offsets (bytevector 22 14 14 14 14 14 14 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-457 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 10 20 20 20 20)
(define-deferred ucd-gc-entry-458
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 10 20 20 20 20)))
    (named-lambda (ucd-gc-entry-458 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-459
  (let ((offsets (bytevector 12 12 12 12 12 12 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-459 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 24 24 21 24 24 21 24 24 2 24 21 22 22)
(define-deferred ucd-gc-entry-460
  (let ((offsets (bytevector 14 14 14 24 24 21 24 24 21 24 24 2 24 21 22 22)))
    (named-lambda (ucd-gc-entry-460 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (456 457 377 12 12 458 459 12 12 12 12 12 14 14 460 234)
(define-deferred ucd-gc-entry-461
  (let ((offsets (bytevector 200 1 201 1 121 1 12 0 12 0 202 1 203 1 12 0 12 0 12 0 12 0 12 0 14 0 14 0 204 1 234 0)))
    (named-lambda (ucd-gc-entry-461 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-462
  (let ((offsets (bytevector 14 14 14 14 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-462 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 22 22 22 14 14 14 14 14)
(define-deferred ucd-gc-entry-463
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 22 22 22 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-463 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 462 14 463 14 14 250)
(define-deferred ucd-gc-entry-464
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 206 1 14 0 207 1 14 0 14 0 250 0)))
    (named-lambda (ucd-gc-entry-464 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 248 14 14 14 14 14 14 251 22 22)
(define-deferred ucd-gc-entry-465
  (let ((offsets (bytevector 14 14 14 14 14 14 248 14 14 14 14 14 14 251 22 22)))
    (named-lambda (ucd-gc-entry-465 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (12 12 12 12 12 12 12 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-466
  (let ((offsets (bytevector 12 12 12 12 12 12 12 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-466 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 12 12 12 12 12 22 22 22 22 22 14 21 14)
(define-deferred ucd-gc-entry-467
  (let ((offsets (bytevector 22 22 22 12 12 12 12 12 22 22 22 22 22 14 21 14)))
    (named-lambda (ucd-gc-entry-467 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 6 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-468
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 6 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-468 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 14 14 14 14 14 22 14 22)
(define-deferred ucd-gc-entry-469
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 14 14 14 14 14 22 14 22)))
    (named-lambda (ucd-gc-entry-469 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 22 14 14 22 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-470
  (let ((offsets (bytevector 14 14 22 14 14 22 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-470 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
(define-deferred ucd-gc-entry-471
  (let ((offsets (bytevector 14 14 10 10 10 10 10 10 10 10 10 10 10 10 10 10)))
    (named-lambda (ucd-gc-entry-471 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (10 10 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-472
  (let ((offsets (bytevector 10 10 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-472 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-473
  (let ((offsets (bytevector 22 22 22 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-473 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (466 467 468 469 470 14 14 14 14 14 14 471 472 473 14 14)
(define-deferred ucd-gc-entry-474
  (let ((offsets (bytevector 210 1 211 1 212 1 213 1 214 1 14 0 14 0 14 0 14 0 14 0 14 0 215 1 216 1 217 1 14 0 14 0)))
    (named-lambda (ucd-gc-entry-474 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 5 4)
(define-deferred ucd-gc-entry-475
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 5 4)))
    (named-lambda (ucd-gc-entry-475 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-476
  (let ((offsets (bytevector 22 22 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-476 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 3 13 22 22)
(define-deferred ucd-gc-entry-477
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 3 13 22 22)))
    (named-lambda (ucd-gc-entry-477 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 475 22 14 14 14 14 476 14 14 239 22 22 477)
(define-deferred ucd-gc-entry-478
  (let ((offsets (bytevector 14 0 14 0 14 0 219 1 22 0 14 0 14 0 14 0 14 0 220 1 14 0 14 0 239 0 22 0 22 0 221 1)))
    (named-lambda (ucd-gc-entry-478 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (2 2 2 2 2 2 2 4 5 2 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-479
  (let ((offsets (bytevector 2 2 2 2 2 2 2 4 5 2 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-479 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 7 7 11 11 4 5 4 5 4 5 4 5 4 5 4)
(define-deferred ucd-gc-entry-480
  (let ((offsets (bytevector 2 7 7 11 11 4 5 4 5 4 5 4 5 4 5 4)))
    (named-lambda (ucd-gc-entry-480 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (5 4 5 4 5 2 2 4 5 2 2 2 2 11 11 11)
(define-deferred ucd-gc-entry-481
  (let ((offsets (bytevector 5 4 5 4 5 2 2 4 5 2 2 2 2 11 11 11)))
    (named-lambda (ucd-gc-entry-481 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 22 2 2 2 2 7 4 5 4 5 4 5 2)
(define-deferred ucd-gc-entry-482
  (let ((offsets (bytevector 2 2 2 22 2 2 2 2 7 4 5 4 5 4 5 2)))
    (named-lambda (ucd-gc-entry-482 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 6 7 6 6 6 22 2 3 2 2 22 22 22 22)
(define-deferred ucd-gc-entry-483
  (let ((offsets (bytevector 2 2 6 7 6 6 6 22 2 3 2 2 22 22 22 22)))
    (named-lambda (ucd-gc-entry-483 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 22 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-484
  (let ((offsets (bytevector 14 14 14 14 14 22 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-484 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 22 22 16)
(define-deferred ucd-gc-entry-485
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 22 22 16)))
    (named-lambda (ucd-gc-entry-485 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 479 21 480 481 482 483 484 14 14 14 14 14 14 14 485)
(define-deferred ucd-gc-entry-486
  (let ((offsets (bytevector 21 0 223 1 21 0 224 1 225 1 226 1 227 1 228 1 14 0 14 0 14 0 14 0 14 0 14 0 14 0 229 1)))
    (named-lambda (ucd-gc-entry-486 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (22 2 2 2 3 2 2 2 4 5 2 6 2 7 2 2)
(define-deferred ucd-gc-entry-487
  (let ((offsets (bytevector 22 2 2 2 3 2 2 2 4 5 2 6 2 7 2 2)))
    (named-lambda (ucd-gc-entry-487 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 4 6 5 6 4)
(define-deferred ucd-gc-entry-488
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 4 6 5 6 4)))
    (named-lambda (ucd-gc-entry-488 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (5 2 4 5 2 2 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-489
  (let ((offsets (bytevector 5 2 4 5 2 2 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-489 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-490
  (let ((offsets (bytevector 20 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-490 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 20 20)
(define-deferred ucd-gc-entry-491
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 20 20)))
    (named-lambda (ucd-gc-entry-491 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 14 14 14 14 14 14 22 22 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-492
  (let ((offsets (bytevector 22 22 14 14 14 14 14 14 22 22 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-492 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 14 14 14 14 14 14 22 22 14 14 14 22 22 22)
(define-deferred ucd-gc-entry-493
  (let ((offsets (bytevector 22 22 14 14 14 14 14 14 22 22 14 14 14 22 22 22)))
    (named-lambda (ucd-gc-entry-493 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (3 3 6 10 13 3 3 22 13 6 6 6 6 13 13 22)
(define-deferred ucd-gc-entry-494
  (let ((offsets (bytevector 3 3 6 10 13 3 3 22 13 6 6 6 6 13 13 22)))
    (named-lambda (ucd-gc-entry-494 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 22 16 16 16 13 13 22 22)
(define-deferred ucd-gc-entry-495
  (let ((offsets (bytevector 22 22 22 22 22 22 22 22 22 16 16 16 13 13 22 22)))
    (named-lambda (ucd-gc-entry-495 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (487 30 31 32 33 488 489 490 14 491 14 244 492 493 494 495)
(define-deferred ucd-gc-entry-496
  (let ((offsets (bytevector 231 1 30 0 31 0 32 0 33 0 232 1 233 1 234 1 14 0 235 1 14 0 244 0 236 1 237 1 238 1 239 1)))
    (named-lambda (ucd-gc-entry-496 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (39 51 58 66 69 79 88 97 105 120 133 146 158 170 181 194 205 14 211 217 219 14 225 236 243 253 260 270 280 286 288 299 314 326 6 335 340 344 346 352 13 357 6 365 372 378 386 389 396 399 403 13 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 404 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 405 407 14 14 14 408 14 416 426 435 444 455 461 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 464 22 22 22 22 22 22 22 22 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 14 465 474 14 478 486 496)
(define-deferred ucd-gc-entry-497
  (let ((offsets
         (bytevector 39
                     0
                     51
                     0
                     58
                     0
                     66
                     0
                     69
                     0
                     79
                     0
                     88
                     0
                     97
                     0
                     105
                     0
                     120
                     0
                     133
                     0
                     146
                     0
                     158
                     0
                     170
                     0
                     181
                     0
                     194
                     0
                     205
                     0
                     14
                     0
                     211
                     0
                     217
                     0
                     219
                     0
                     14
                     0
                     225
                     0
                     236
                     0
                     243
                     0
                     253
                     0
                     4
                     1
                     14
                     1
                     24
                     1
                     30
                     1
                     32
                     1
                     43
                     1
                     58
                     1
                     70
                     1
                     6
                     0
                     79
                     1
                     84
                     1
                     88
                     1
                     90
                     1
                     96
                     1
                     13
                     0
                     101
                     1
                     6
                     0
                     109
                     1
                     116
                     1
                     122
                     1
                     130
                     1
                     133
                     1
                     140
                     1
                     143
                     1
                     147
                     1
                     13
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     148
                     1
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     149
                     1
                     151
                     1
                     14
                     0
                     14
                     0
                     14
                     0
                     152
                     1
                     14
                     0
                     160
                     1
                     170
                     1
                     179
                     1
                     188
                     1
                     199
                     1
                     205
                     1
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     208
                     1
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     14
                     0
                     209
                     1
                     218
                     1
                     14
                     0
                     222
                     1
                     230
                     1
                     240
                     1)))
    (named-lambda (ucd-gc-entry-497 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 510 (fix:lsh sv -7)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 22 14 14 14)
(define-deferred ucd-gc-entry-498
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 22 14 14 14)))
    (named-lambda (ucd-gc-entry-498 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 22 14 14 22 14)
(define-deferred ucd-gc-entry-499
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 22 14 14 22 14)))
    (named-lambda (ucd-gc-entry-499 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (498 14 210 499 248 248 22 22 14 14 14 14 14 14 14 77)
(define-deferred ucd-gc-entry-500
  (let ((offsets (bytevector 242 1 14 0 210 0 243 1 248 0 248 0 22 0 22 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 77 0)))
    (named-lambda (ucd-gc-entry-500 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (2 2 2 22 22 22 22 17 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-501
  (let ((offsets (bytevector 2 2 2 22 22 22 22 17 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-501 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 22 22 22 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-502
  (let ((offsets (bytevector 17 17 17 17 22 22 22 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-502 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (25 25 25 25 25 17 17 17 17 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-503
  (let ((offsets (bytevector 25 25 25 25 25 17 17 17 17 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-503 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 17 17 13 13 13 22)
(define-deferred ucd-gc-entry-504
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 17 17 13 13 13 22)))
    (named-lambda (ucd-gc-entry-504 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-505
  (let ((offsets (bytevector 13 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-505 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 21 22 22)
(define-deferred ucd-gc-entry-506
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 13 21 22 22)))
    (named-lambda (ucd-gc-entry-506 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (501 17 17 502 25 25 25 503 504 388 505 22 22 13 13 506)
(define-deferred ucd-gc-entry-507
  (let ((offsets (bytevector 245 1 17 0 17 0 246 1 25 0 25 0 25 0 247 1 248 1 132 1 249 1 22 0 22 0 13 0 13 0 250 1)))
    (named-lambda (ucd-gc-entry-507 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-508
  (let ((offsets (bytevector 21 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-508 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 17 17 17 17 17 17 22 22 22 22)
(define-deferred ucd-gc-entry-509
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 17 17 17 17 22 22 22 22)))
    (named-lambda (ucd-gc-entry-509 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 14 187 14 14 14 131 508 509)
(define-deferred ucd-gc-entry-510
  (let ((offsets (bytevector 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 14 0 187 0 14 0 14 0 14 0 131 0 252 1 253 1)))
    (named-lambda (ucd-gc-entry-510 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (17 17 17 17 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-511
  (let ((offsets (bytevector 17 17 17 17 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-511 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 25 14 14 14 14 14 14 14 14 25 22 22 22 22 22)
(define-deferred ucd-gc-entry-512
  (let ((offsets (bytevector 14 25 14 14 14 14 14 14 14 14 25 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-512 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 21 21 21 21 21 22 22 22 22 22)
(define-deferred ucd-gc-entry-513
  (let ((offsets (bytevector 14 14 14 14 14 14 21 21 21 21 21 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-513 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 22 2)
(define-deferred ucd-gc-entry-514
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 22 2)))
    (named-lambda (ucd-gc-entry-514 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 22 22 22 22 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-515
  (let ((offsets (bytevector 14 14 14 14 22 22 22 22 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-515 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 25 25 25 25 25 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-516
  (let ((offsets (bytevector 2 25 25 25 25 25 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-516 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 511 14 512 14 14 513 14 514 14 14 515 516 22 22)
(define-deferred ucd-gc-entry-517
  (let ((offsets (bytevector 14 0 14 0 255 1 14 0 0 2 14 0 14 0 1 2 14 0 2 2 14 0 14 0 3 2 4 2 22 0 22 0)))
    (named-lambda (ucd-gc-entry-517 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (9 9 9 9 9 9 9 9 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-518
  (let ((offsets (bytevector 9 9 9 9 9 9 9 9 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-518 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 22 22 22 22 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-519
  (let ((offsets (bytevector 9 9 9 9 22 22 22 22 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-519 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 12 22 22 22 22)
(define-deferred ucd-gc-entry-520
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 12 22 22 22 22)))
    (named-lambda (ucd-gc-entry-520 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 518 12 12 14 14 14 14 248 234 9 9 519 12 520)
(define-deferred ucd-gc-entry-521
  (let ((offsets (bytevector 9 0 9 0 6 2 12 0 12 0 14 0 14 0 14 0 14 0 248 0 234 0 9 0 9 0 7 2 12 0 8 2)))
    (named-lambda (ucd-gc-entry-521 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 22 22 22 22 22 22 22 22 22 22 22 2)
(define-deferred ucd-gc-entry-522
  (let ((offsets (bytevector 14 14 14 14 22 22 22 22 22 22 22 22 22 22 22 2)))
    (named-lambda (ucd-gc-entry-522 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 239 14 14 14 522 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-523
  (let ((offsets (bytevector 14 0 14 0 239 0 14 0 14 0 14 0 10 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-523 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 376 14 242 239 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-524
  (let ((offsets (bytevector 14 0 14 0 14 0 120 1 14 0 242 0 239 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-524 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 22 22 14 22 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-525
  (let ((offsets (bytevector 14 14 14 14 14 14 22 22 14 22 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-525 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 22 14 14 22 22 22 14 22 22 14)
(define-deferred ucd-gc-entry-526
  (let ((offsets (bytevector 14 14 14 14 14 14 22 14 14 22 22 22 14 22 22 14)))
    (named-lambda (ucd-gc-entry-526 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 22 2 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-527
  (let ((offsets (bytevector 14 14 14 14 14 14 22 2 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-527 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 13 13 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-528
  (let ((offsets (bytevector 14 14 14 14 14 14 14 13 13 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-528 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 17 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-529
  (let ((offsets (bytevector 22 22 22 22 22 22 22 17 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-529 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 22 14 14 22 22 22 22 22 17 17 17 17 17)
(define-deferred ucd-gc-entry-530
  (let ((offsets (bytevector 14 14 14 22 14 14 22 22 22 22 22 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-530 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (525 14 14 526 14 527 14 528 14 244 529 22 22 22 14 530)
(define-deferred ucd-gc-entry-531
  (let ((offsets (bytevector 13 2 14 0 14 0 14 2 14 0 15 2 14 0 16 2 14 0 244 0 17 2 22 0 22 0 22 0 14 0 18 2)))
    (named-lambda (ucd-gc-entry-531 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 17 17 17 17 17 17 22 22 22 2)
(define-deferred ucd-gc-entry-532
  (let ((offsets (bytevector 14 14 14 14 14 14 17 17 17 17 17 17 22 22 22 2)))
    (named-lambda (ucd-gc-entry-532 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 22 22 22 22 22 2)
(define-deferred ucd-gc-entry-533
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 22 22 22 22 22 2)))
    (named-lambda (ucd-gc-entry-533 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 22 22 22 22 17 17 14 14)
(define-deferred ucd-gc-entry-534
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 22 22 22 22 17 17 14 14)))
    (named-lambda (ucd-gc-entry-534 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 17 17 17 17 17 17 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-535
  (let ((offsets (bytevector 22 22 17 17 17 17 17 17 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-535 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 532 14 533 22 22 22 22 14 14 14 534 17 535 17 17)
(define-deferred ucd-gc-entry-536
  (let ((offsets (bytevector 14 0 20 2 14 0 21 2 22 0 22 0 22 0 22 0 14 0 14 0 14 0 22 2 17 0 23 2 17 0 17 0)))
    (named-lambda (ucd-gc-entry-536 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 21 21 21 22 21 21 22 22 22 22 22 21 21 21 21)
(define-deferred ucd-gc-entry-537
  (let ((offsets (bytevector 14 21 21 21 22 21 21 22 22 22 22 22 21 21 21 21)))
    (named-lambda (ucd-gc-entry-537 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 22 14 14 14 22 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-538
  (let ((offsets (bytevector 14 14 14 14 22 14 14 14 22 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-538 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 22 22 22 22 21 21 21 22 22 22 22 21)
(define-deferred ucd-gc-entry-539
  (let ((offsets (bytevector 14 14 14 14 22 22 22 22 21 21 21 22 22 22 22 21)))
    (named-lambda (ucd-gc-entry-539 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 17 17 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-540
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-540 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 2 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-541
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 2 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-541 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 17 17 2)
(define-deferred ucd-gc-entry-542
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 17 17 2)))
    (named-lambda (ucd-gc-entry-542 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 17 17 17)
(define-deferred ucd-gc-entry-543
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 17 17 17)))
    (named-lambda (ucd-gc-entry-543 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 13 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-544
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 13 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-544 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 21 21 22 22 22 22 17 17 17 17 17)
(define-deferred ucd-gc-entry-545
  (let ((offsets (bytevector 14 14 14 14 14 21 21 22 22 22 22 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-545 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-546
  (let ((offsets (bytevector 2 2 2 2 2 2 2 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-546 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (537 538 14 539 540 541 14 542 14 543 22 22 544 14 545 546)
(define-deferred ucd-gc-entry-547
  (let ((offsets (bytevector 25 2 26 2 14 0 27 2 28 2 29 2 14 0 30 2 14 0 31 2 22 0 22 0 32 2 14 0 33 2 34 2)))
    (named-lambda (ucd-gc-entry-547 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 22 22 22 2 2 2 2 2 2 2)
(define-deferred ucd-gc-entry-548
  (let ((offsets (bytevector 14 14 14 14 14 14 22 22 22 2 2 2 2 2 2 2)))
    (named-lambda (ucd-gc-entry-548 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 22 22 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-549
  (let ((offsets (bytevector 14 14 14 14 14 14 22 22 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-549 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 22 22 22 22 22 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-550
  (let ((offsets (bytevector 14 14 14 22 22 22 22 22 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-550 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 22 22 22 22 22 22 22 2 2 2 2 22 22 22)
(define-deferred ucd-gc-entry-551
  (let ((offsets (bytevector 14 14 22 22 22 22 22 22 22 2 2 2 2 22 22 22)))
    (named-lambda (ucd-gc-entry-551 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 22 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-552
  (let ((offsets (bytevector 22 22 22 22 22 22 22 22 22 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-552 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 548 14 549 14 550 14 551 552 22 22 22 22 22)
(define-deferred ucd-gc-entry-553
  (let ((offsets (bytevector 14 0 14 0 14 0 36 2 14 0 37 2 14 0 38 2 14 0 39 2 40 2 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-553 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-554
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-554 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-555
  (let ((offsets (bytevector 9 9 9 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-555 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 22 22 22 22 22 22 22 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-556
  (let ((offsets (bytevector 12 12 12 22 22 22 22 22 22 22 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-556 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 554 22 22 22 9 9 9 555 12 12 12 556)
(define-deferred ucd-gc-entry-557
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 42 2 22 0 22 0 22 0 9 0 9 0 9 0 43 2 12 0 12 0 12 0 44 2)))
    (named-lambda (ucd-gc-entry-557 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 22)
(define-deferred ucd-gc-entry-558
  (let ((offsets (bytevector 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 22)))
    (named-lambda (ucd-gc-entry-558 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 17 558 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-559
  (let ((offsets (bytevector 22 0 22 0 22 0 22 0 22 0 22 0 17 0 46 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-559 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (24 21 24 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-560
  (let ((offsets (bytevector 24 21 24 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-560 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-561
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-561 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 2 2 2 2 2 2 2 22 22)
(define-deferred ucd-gc-entry-562
  (let ((offsets (bytevector 21 21 21 21 21 21 21 2 2 2 2 2 2 2 22 22)))
    (named-lambda (ucd-gc-entry-562 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 17 8 8 8 8 8 8 8 8 8 8)
(define-deferred ucd-gc-entry-563
  (let ((offsets (bytevector 17 17 17 17 17 17 8 8 8 8 8 8 8 8 8 8)))
    (named-lambda (ucd-gc-entry-563 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 21)
(define-deferred ucd-gc-entry-564
  (let ((offsets (bytevector 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 21)))
    (named-lambda (ucd-gc-entry-564 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 21 21 21 21 24 24 21 21 2 2 16 2 2)
(define-deferred ucd-gc-entry-565
  (let ((offsets (bytevector 24 24 24 21 21 21 21 24 24 21 21 2 2 16 2 2)))
    (named-lambda (ucd-gc-entry-565 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-566
  (let ((offsets (bytevector 2 2 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-566 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (560 14 14 561 562 535 563 564 266 14 14 565 566 14 554 234)
(define-deferred ucd-gc-entry-567
  (let ((offsets (bytevector 48 2 14 0 14 0 49 2 50 2 23 2 51 2 52 2 10 1 14 0 14 0 53 2 54 2 14 0 42 2 234 0)))
    (named-lambda (ucd-gc-entry-567 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 21 21 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-568
  (let ((offsets (bytevector 21 21 21 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-568 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 21 21 21 21 21 24 21 21 21)
(define-deferred ucd-gc-entry-569
  (let ((offsets (bytevector 14 14 14 14 14 14 14 21 21 21 21 21 24 21 21 21)))
    (named-lambda (ucd-gc-entry-569 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 22 8 8 8 8 8 8 8 8 8 8)
(define-deferred ucd-gc-entry-570
  (let ((offsets (bytevector 21 21 21 21 21 22 8 8 8 8 8 8 8 8 8 8)))
    (named-lambda (ucd-gc-entry-570 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-571
  (let ((offsets (bytevector 2 2 2 2 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-571 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 21 2 2 14 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-572
  (let ((offsets (bytevector 14 14 14 21 2 2 14 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-572 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 24 24 24 21 21 21 21 21 21 21 21 21 24)
(define-deferred ucd-gc-entry-573
  (let ((offsets (bytevector 14 14 14 24 24 24 21 21 21 21 21 21 21 21 21 24)))
    (named-lambda (ucd-gc-entry-573 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 14 14 14 14 2 2 2 2 2 21 21 21 2 22 22)
(define-deferred ucd-gc-entry-574
  (let ((offsets (bytevector 24 14 14 14 14 2 2 2 2 2 21 21 21 2 22 22)))
    (named-lambda (ucd-gc-entry-574 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 14 2 14 2 2 2)
(define-deferred ucd-gc-entry-575
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 14 2 14 2 2 2)))
    (named-lambda (ucd-gc-entry-575 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-576
  (let ((offsets (bytevector 22 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-576 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 17 17 17 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-577
  (let ((offsets (bytevector 17 17 17 17 17 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-577 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (568 14 569 570 571 14 14 572 266 14 14 573 574 575 576 577)
(define-deferred ucd-gc-entry-578
  (let ((offsets (bytevector 56 2 14 0 57 2 58 2 59 2 14 0 14 0 60 2 10 1 14 0 14 0 61 2 62 2 63 2 64 2 65 2)))
    (named-lambda (ucd-gc-entry-578 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 24 24 24 21)
(define-deferred ucd-gc-entry-579
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 24 24 24 21)))
    (named-lambda (ucd-gc-entry-579 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 24 24 21 24 21 21 2 2 2 2 2 2 21 22)
(define-deferred ucd-gc-entry-580
  (let ((offsets (bytevector 21 21 24 24 21 24 21 21 2 2 2 2 2 2 21 22)))
    (named-lambda (ucd-gc-entry-580 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 22 14 22 14 14 14 14 22 14)
(define-deferred ucd-gc-entry-581
  (let ((offsets (bytevector 14 14 14 14 14 14 14 22 14 22 14 14 14 14 22 14)))
    (named-lambda (ucd-gc-entry-581 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 22 14)
(define-deferred ucd-gc-entry-582
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 22 14)))
    (named-lambda (ucd-gc-entry-582 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 2 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-583
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 2 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-583 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 21)
(define-deferred ucd-gc-entry-584
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 21)))
    (named-lambda (ucd-gc-entry-584 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 21 21 21 21 21 21 21 21 22 22 22 22 22)
(define-deferred ucd-gc-entry-585
  (let ((offsets (bytevector 24 24 24 21 21 21 21 21 21 21 21 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-585 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 128 579 580 22 22 22 22 581 582 583 14 14 584 585 234)
(define-deferred ucd-gc-entry-586
  (let ((offsets (bytevector 14 0 128 0 67 2 68 2 22 0 22 0 22 0 22 0 69 2 70 2 71 2 14 0 14 0 72 2 73 2 234 0)))
    (named-lambda (ucd-gc-entry-586 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 21 24 24 22 14 14 14 14 14 14 14 14 22 22 14)
(define-deferred ucd-gc-entry-587
  (let ((offsets (bytevector 21 21 24 24 22 14 14 14 14 14 14 14 14 22 22 14)))
    (named-lambda (ucd-gc-entry-587 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 24 24 24 24 22 22 24 24 22 22 24 24 24 22 22)
(define-deferred ucd-gc-entry-588
  (let ((offsets (bytevector 21 24 24 24 24 22 22 24 24 22 22 24 24 24 22 22)))
    (named-lambda (ucd-gc-entry-588 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 22 22 22 22 22 22 24 22 22 22 22 22 14 14 14)
(define-deferred ucd-gc-entry-589
  (let ((offsets (bytevector 14 22 22 22 22 22 22 24 22 22 22 22 22 14 14 14)))
    (named-lambda (ucd-gc-entry-589 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 24 24 22 22 21 21 21 21 21 21 21 22 22 22)
(define-deferred ucd-gc-entry-590
  (let ((offsets (bytevector 14 14 24 24 22 22 21 21 21 21 21 21 21 22 22 22)))
    (named-lambda (ucd-gc-entry-590 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-591
  (let ((offsets (bytevector 21 21 21 21 21 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-591 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (587 113 114 129 588 589 590 591 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-592
  (let ((offsets (bytevector 75 2 113 0 114 0 129 0 76 2 77 2 78 2 79 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-592 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 24 24 24 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-593
  (let ((offsets (bytevector 14 14 14 14 14 24 24 24 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-593 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 21 21 21 24 21 14 14 14 14 2 2 2 2 2)
(define-deferred ucd-gc-entry-594
  (let ((offsets (bytevector 24 24 21 21 21 24 21 14 14 14 14 2 2 2 2 2)))
    (named-lambda (ucd-gc-entry-594 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 22 2 22 2 22 22)
(define-deferred ucd-gc-entry-595
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 22 2 22 2 22 22)))
    (named-lambda (ucd-gc-entry-595 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 21 21 21 21 21 21 24 21 24 24 24 24 21)
(define-deferred ucd-gc-entry-596
  (let ((offsets (bytevector 24 24 24 21 21 21 21 21 21 24 21 24 24 24 24 21)))
    (named-lambda (ucd-gc-entry-596 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 24 21 21 14 14 2 14 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-597
  (let ((offsets (bytevector 21 24 21 21 14 14 2 14 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-597 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 593 594 595 22 22 14 14 14 596 597 234 22 22)
(define-deferred ucd-gc-entry-598
  (let ((offsets (bytevector 14 0 14 0 14 0 81 2 82 2 83 2 22 0 22 0 14 0 14 0 14 0 84 2 85 2 234 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-598 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 24)
(define-deferred ucd-gc-entry-599
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 24)))
    (named-lambda (ucd-gc-entry-599 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 21 21 21 21 22 22 24 24 24 24 21 21 24 21)
(define-deferred ucd-gc-entry-600
  (let ((offsets (bytevector 24 24 21 21 21 21 22 22 24 24 24 24 21 21 24 21)))
    (named-lambda (ucd-gc-entry-600 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
(define-deferred ucd-gc-entry-601
  (let ((offsets (bytevector 21 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)))
    (named-lambda (ucd-gc-entry-601 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 14 14 14 14 21 21 22 22)
(define-deferred ucd-gc-entry-602
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 14 14 14 14 21 21 22 22)))
    (named-lambda (ucd-gc-entry-602 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 14 14 599 600 601 602 22 22)
(define-deferred ucd-gc-entry-603
  (let ((offsets (bytevector 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 14 0 14 0 87 2 88 2 89 2 90 2 22 0 22 0)))
    (named-lambda (ucd-gc-entry-603 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (24 24 24 21 21 21 21 21 21 21 21 24 24 21 24 21)
(define-deferred ucd-gc-entry-604
  (let ((offsets (bytevector 24 24 24 21 21 21 21 21 21 21 21 24 24 21 24 21)))
    (named-lambda (ucd-gc-entry-604 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 2 2 2 14 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-605
  (let ((offsets (bytevector 21 2 2 2 14 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-605 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 2 2 2 2 2 2 2 2 22 22 22)
(define-deferred ucd-gc-entry-606
  (let ((offsets (bytevector 2 2 2 2 2 2 2 2 2 2 2 2 2 22 22 22)))
    (named-lambda (ucd-gc-entry-606 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 21 24 21 24 24)
(define-deferred ucd-gc-entry-607
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 21 24 21 24 24)))
    (named-lambda (ucd-gc-entry-607 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 24 21 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-608
  (let ((offsets (bytevector 21 21 21 21 21 21 24 21 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-608 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 604 605 234 606 22 14 14 607 608 234 22 22 22)
(define-deferred ucd-gc-entry-609
  (let ((offsets (bytevector 14 0 14 0 14 0 92 2 93 2 234 0 94 2 22 0 14 0 14 0 95 2 96 2 234 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-609 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 22 22 22 21 21 21)
(define-deferred ucd-gc-entry-610
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 22 22 22 21 21 21)))
    (named-lambda (ucd-gc-entry-610 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 21 21 21 21 24 21 21 21 21 21 22 22 22 22)
(define-deferred ucd-gc-entry-611
  (let ((offsets (bytevector 24 24 21 21 21 21 24 21 21 21 21 21 22 22 22 22)))
    (named-lambda (ucd-gc-entry-611 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 17 17 2 2 2 13)
(define-deferred ucd-gc-entry-612
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 17 17 2 2 2 13)))
    (named-lambda (ucd-gc-entry-612 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 610 611 612 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-613
  (let ((offsets (bytevector 14 0 98 2 99 2 100 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-613 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (17 17 17 22 22 22 22 22 22 22 22 22 22 22 22 14)
(define-deferred ucd-gc-entry-614
  (let ((offsets (bytevector 17 17 17 22 22 22 22 22 22 22 22 22 22 22 22 14)))
    (named-lambda (ucd-gc-entry-614 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 22 22 9 9 12 12 184 614)
(define-deferred ucd-gc-entry-615
  (let ((offsets (bytevector 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 9 0 9 0 12 0 12 0 184 0 102 2)))
    (named-lambda (ucd-gc-entry-615 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (22 22 22 22 22 22 22 22 22 22 22 22 14 14 14 554)
(define-deferred ucd-gc-entry-616
  (let ((offsets (bytevector 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 14 0 14 0 14 0 42 2)))
    (named-lambda (ucd-gc-entry-616 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 21 21 21 21 21 21 22 21 21 21 21 21 21 24 21)
(define-deferred ucd-gc-entry-617
  (let ((offsets (bytevector 21 21 21 21 21 21 21 22 21 21 21 21 21 21 24 21)))
    (named-lambda (ucd-gc-entry-617 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 2 2 2 2 2 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-618
  (let ((offsets (bytevector 14 2 2 2 2 2 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-618 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-619
  (let ((offsets (bytevector 2 2 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-619 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 21 21 21 21 21 21 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-620
  (let ((offsets (bytevector 22 22 21 21 21 21 21 21 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-620 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 22 24 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-621
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 22 24 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-621 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 24 21 21 24 21 21 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-622
  (let ((offsets (bytevector 21 24 21 21 24 21 21 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-622 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (114 14 599 617 618 184 214 619 14 620 621 622 22 22 22 22)
(define-deferred ucd-gc-entry-623
  (let ((offsets (bytevector 114 0 14 0 87 2 105 2 106 2 184 0 214 0 107 2 14 0 108 2 109 2 110 2 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-623 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 251 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-624
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 251 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-624 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 22)
(define-deferred ucd-gc-entry-625
  (let ((offsets (bytevector 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 22)))
    (named-lambda (ucd-gc-entry-625 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (2 2 2 2 2 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-626
  (let ((offsets (bytevector 2 2 2 2 2 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-626 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (25 25 25 25 25 25 625 626 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-627
  (let ((offsets (bytevector 25 0 25 0 25 0 25 0 25 0 25 0 113 2 114 2 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0)))
    (named-lambda (ucd-gc-entry-627 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 462 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-628
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 206 1 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-628 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 244 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-629
  (let ((offsets (bytevector 14 14 244 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-629 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 376 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-630
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 120 1 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-630 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 21 21 21 21 2 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-631
  (let ((offsets (bytevector 21 21 21 21 21 2 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-631 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 554 14 244 441 22 22 22 22 22 22 14 248 631)
(define-deferred ucd-gc-entry-632
  (let ((offsets (bytevector 14 0 14 0 14 0 42 2 14 0 244 0 185 1 22 0 22 0 22 0 22 0 22 0 22 0 14 0 248 0 119 2)))
    (named-lambda (ucd-gc-entry-632 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 21 21 21 21 21 21 2 2 2 2 2 13 13 13 13)
(define-deferred ucd-gc-entry-633
  (let ((offsets (bytevector 21 21 21 21 21 21 21 2 2 2 2 2 13 13 13 13)))
    (named-lambda (ucd-gc-entry-633 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 20 20 20 2 13 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-634
  (let ((offsets (bytevector 20 20 20 20 2 13 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-634 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (8 8 8 8 8 8 8 8 8 8 22 17 17 17 17 17)
(define-deferred ucd-gc-entry-635
  (let ((offsets (bytevector 8 8 8 8 8 8 8 8 8 8 22 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-635 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (17 17 22 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-636
  (let ((offsets (bytevector 17 17 22 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-636 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 22 22 22 22 22 14 14 14)
(define-deferred ucd-gc-entry-637
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 22 22 22 22 22 14 14 14)))
    (named-lambda (ucd-gc-entry-637 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 633 634 635 636 637 14 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-638
  (let ((offsets (bytevector 14 0 14 0 14 0 121 2 122 2 123 2 124 2 125 2 14 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-638 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24)
(define-deferred ucd-gc-entry-639
  (let ((offsets (bytevector 14 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24)))
    (named-lambda (ucd-gc-entry-639 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 22)
(define-deferred ucd-gc-entry-640
  (let ((offsets (bytevector 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 22)))
    (named-lambda (ucd-gc-entry-640 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 20 20 20 20 20 20 20 20 20 20 20 20 20)
(define-deferred ucd-gc-entry-641
  (let ((offsets (bytevector 21 21 21 20 20 20 20 20 20 20 20 20 20 20 20 20)))
    (named-lambda (ucd-gc-entry-641 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (20 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-642
  (let ((offsets (bytevector 20 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-642 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 249 639 24 640 564 641 22 22 22 22 642 22)
(define-deferred ucd-gc-entry-643
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 249 0 127 2 24 0 128 2 52 2 129 2 22 0 22 0 22 0 22 0 130 2 22 0)))
    (named-lambda (ucd-gc-entry-643 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 187 22)
(define-deferred ucd-gc-entry-644
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 14 14 14 14 187 22)))
    (named-lambda (ucd-gc-entry-644 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-645
  (let ((offsets (bytevector 14 14 14 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-645 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 645)
(define-deferred ucd-gc-entry-646
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 133 2)))
    (named-lambda (ucd-gc-entry-646 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-647
  (let ((offsets (bytevector 14 14 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-647 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (647 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-648
  (let ((offsets (bytevector 135 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-648 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 22 22 13 21 21 2)
(define-deferred ucd-gc-entry-649
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 22 22 13 21 21 2)))
    (named-lambda (ucd-gc-entry-649 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (16 16 16 16 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-650
  (let ((offsets (bytevector 16 16 16 16 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-650 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 77 187 554 649 650 22 22 22 22 22)
(define-deferred ucd-gc-entry-651
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 14 0 14 0 77 0 187 0 42 2 137 2 138 2 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-651 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 387)
(define-deferred ucd-gc-entry-652
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 131 1)))
    (named-lambda (ucd-gc-entry-652 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 22 22 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-653
  (let ((offsets (bytevector 13 13 13 13 13 13 13 22 22 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-653 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 24 24 21 21 21 13 13 13 24 24 24)
(define-deferred ucd-gc-entry-654
  (let ((offsets (bytevector 13 13 13 13 13 24 24 21 21 21 13 13 13 24 24 24)))
    (named-lambda (ucd-gc-entry-654 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (24 24 24 16 16 16 16 16 16 16 16 21 21 21 21 21)
(define-deferred ucd-gc-entry-655
  (let ((offsets (bytevector 24 24 24 16 16 16 16 16 16 16 16 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-655 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 13 13 21 21 21 21 21 21 21 13 13 13 13)
(define-deferred ucd-gc-entry-656
  (let ((offsets (bytevector 21 21 21 13 13 21 21 21 21 21 21 21 13 13 13 13)))
    (named-lambda (ucd-gc-entry-656 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 21 21 21 21 13 13)
(define-deferred ucd-gc-entry-657
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 21 21 21 21 13 13)))
    (named-lambda (ucd-gc-entry-657 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-658
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-658 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 653 13 13 13 654 655 656 13 657 13 13 13 658 22)
(define-deferred ucd-gc-entry-659
  (let ((offsets (bytevector 13 0 13 0 141 2 13 0 13 0 13 0 142 2 143 2 144 2 13 0 145 2 13 0 13 0 13 0 146 2 22 0)))
    (named-lambda (ucd-gc-entry-659 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 21 21 21 13 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-660
  (let ((offsets (bytevector 13 13 21 21 21 13 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-660 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 660 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-661
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 148 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-661 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (17 17 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-662
  (let ((offsets (bytevector 17 17 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-662 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 336 17 662 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-663
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 80 1 17 0 150 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-663 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (9 9 9 9 9 9 9 9 9 9 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-664
  (let ((offsets (bytevector 9 9 9 9 9 9 9 9 9 9 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-664 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 9 9 9 9 9 9 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-665
  (let ((offsets (bytevector 12 12 12 12 9 9 9 9 9 9 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-665 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 9 9 9 9 9 9 9 9 12 12)
(define-deferred ucd-gc-entry-666
  (let ((offsets (bytevector 9 9 9 9 9 9 9 9 9 9 9 9 9 9 12 12)))
    (named-lambda (ucd-gc-entry-666 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 22 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-667
  (let ((offsets (bytevector 12 12 12 12 12 22 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-667 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 12 12 12 12 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-668
  (let ((offsets (bytevector 9 9 12 12 12 12 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-668 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 12 9 22 9 9)
(define-deferred ucd-gc-entry-669
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 12 9 22 9 9)))
    (named-lambda (ucd-gc-entry-669 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 9 22 22 9 9 22 22 9 9 9 9 22 9 9)
(define-deferred ucd-gc-entry-670
  (let ((offsets (bytevector 22 22 9 22 22 9 9 22 22 9 9 9 9 22 9 9)))
    (named-lambda (ucd-gc-entry-670 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 12 12 12 12 22 12 22 12 12 12)
(define-deferred ucd-gc-entry-671
  (let ((offsets (bytevector 9 9 9 9 9 9 12 12 12 12 22 12 22 12 12 12)))
    (named-lambda (ucd-gc-entry-671 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 22 12 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-672
  (let ((offsets (bytevector 12 12 12 12 22 12 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-672 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 664 12 665 666 667 289 9 668 669 670 671 672 9 664 12)
(define-deferred ucd-gc-entry-673
  (let ((offsets (bytevector 9 0 152 2 12 0 153 2 154 2 155 2 33 1 9 0 156 2 157 2 158 2 159 2 160 2 9 0 152 2 12 0)))
    (named-lambda (ucd-gc-entry-673 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (12 12 12 12 9 9 22 9 9 9 9 22 22 9 9 9)
(define-deferred ucd-gc-entry-674
  (let ((offsets (bytevector 12 12 12 12 9 9 22 9 9 9 9 22 22 9 9 9)))
    (named-lambda (ucd-gc-entry-674 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 22 9 9 9 9 9 9 9 22 12 12)
(define-deferred ucd-gc-entry-675
  (let ((offsets (bytevector 9 9 9 9 9 22 9 9 9 9 9 9 9 22 12 12)))
    (named-lambda (ucd-gc-entry-675 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 9 9 22 9 9 9 9 22)
(define-deferred ucd-gc-entry-676
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 9 9 22 9 9 9 9 22)))
    (named-lambda (ucd-gc-entry-676 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 22 9 22 22 22 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-677
  (let ((offsets (bytevector 9 9 9 9 9 22 9 22 22 22 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-677 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 22 12 12 12 12 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-678
  (let ((offsets (bytevector 9 22 12 12 12 12 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-678 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 12 9 9 9 9)
(define-deferred ucd-gc-entry-679
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 12 9 9 9 9)))
    (named-lambda (ucd-gc-entry-679 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-680
  (let ((offsets (bytevector 9 9 9 9 9 9 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-680 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (674 675 12 676 677 678 679 9 680 12 9 664 12 665 666 12)
(define-deferred ucd-gc-entry-681
  (let ((offsets (bytevector 162 2 163 2 12 0 164 2 165 2 166 2 167 2 9 0 168 2 12 0 9 0 152 2 12 0 153 2 154 2 12 0)))
    (named-lambda (ucd-gc-entry-681 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (12 12 12 12 12 12 22 22 9 9 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-682
  (let ((offsets (bytevector 12 12 12 12 12 12 22 22 9 9 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-682 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-683
  (let ((offsets (bytevector 9 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-683 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12)
(define-deferred ucd-gc-entry-684
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12)))
    (named-lambda (ucd-gc-entry-684 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 9 9 9 9 9 9 9 9 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-685
  (let ((offsets (bytevector 12 12 9 9 9 9 9 9 9 9 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-685 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 9 9 9 9 9 6 12 12 12 12)
(define-deferred ucd-gc-entry-686
  (let ((offsets (bytevector 9 9 9 9 9 9 9 9 9 9 9 6 12 12 12 12)))
    (named-lambda (ucd-gc-entry-686 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (289 9 668 679 9 680 12 9 664 12 682 9 683 684 685 686)
(define-deferred ucd-gc-entry-687
  (let ((offsets (bytevector 33 1 9 0 156 2 167 2 9 0 168 2 12 0 9 0 152 2 12 0 170 2 9 0 171 2 172 2 173 2 174 2)))
    (named-lambda (ucd-gc-entry-687 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (12 12 12 12 12 6 12 12 12 12 12 12 9 9 9 9)
(define-deferred ucd-gc-entry-688
  (let ((offsets (bytevector 12 12 12 12 12 6 12 12 12 12 12 12 9 9 9 9)))
    (named-lambda (ucd-gc-entry-688 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 6 12 12 12 12 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-689
  (let ((offsets (bytevector 9 9 9 9 9 6 12 12 12 12 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-689 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6)
(define-deferred ucd-gc-entry-690
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6)))
    (named-lambda (ucd-gc-entry-690 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 9 9 9 9 9 9 9 9 9 9)
(define-deferred ucd-gc-entry-691
  (let ((offsets (bytevector 12 12 12 12 12 12 9 9 9 9 9 9 9 9 9 9)))
    (named-lambda (ucd-gc-entry-691 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 6)
(define-deferred ucd-gc-entry-692
  (let ((offsets (bytevector 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 6)))
    (named-lambda (ucd-gc-entry-692 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-693
  (let ((offsets (bytevector 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-693 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 9 9 9 9 9 9 9 6 12 12 12 12 12 12)
(define-deferred ucd-gc-entry-694
  (let ((offsets (bytevector 9 9 9 9 9 9 9 9 9 6 12 12 12 12 12 12)))
    (named-lambda (ucd-gc-entry-694 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 12 12 6 12 12 12 12 12 12 9 12 22 22 8 8)
(define-deferred ucd-gc-entry-695
  (let ((offsets (bytevector 12 12 12 6 12 12 12 12 12 12 9 12 22 22 8 8)))
    (named-lambda (ucd-gc-entry-695 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (12 688 9 689 690 691 692 12 693 9 694 12 695 8 8 8)
(define-deferred ucd-gc-entry-696
  (let ((offsets (bytevector 12 0 176 2 9 0 177 2 178 2 179 2 180 2 12 0 181 2 9 0 182 2 12 0 183 2 8 0 8 0 8 0)))
    (named-lambda (ucd-gc-entry-696 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 21 21 21 21 21 21 13 13 13 13 21 21 21 21 21)
(define-deferred ucd-gc-entry-697
  (let ((offsets (bytevector 21 21 21 21 21 21 21 13 13 13 13 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-697 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 21 21 21 21 13 13 13)
(define-deferred ucd-gc-entry-698
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 21 21 21 21 13 13 13)))
    (named-lambda (ucd-gc-entry-698 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 21 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-699
  (let ((offsets (bytevector 13 13 13 13 13 21 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-699 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 21 13 13 2 2 2 2 2 22 22 22 22)
(define-deferred ucd-gc-entry-700
  (let ((offsets (bytevector 13 13 13 13 21 13 13 2 2 2 2 2 22 22 22 22)))
    (named-lambda (ucd-gc-entry-700 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 22 22 22 22 22 21 21 21 21 21)
(define-deferred ucd-gc-entry-701
  (let ((offsets (bytevector 22 22 22 22 22 22 22 22 22 22 22 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-701 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 697 21 21 698 699 700 701 74 22 22 22 22 22)
(define-deferred ucd-gc-entry-702
  (let ((offsets (bytevector 21 0 21 0 21 0 185 2 21 0 21 0 186 2 187 2 188 2 189 2 74 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-702 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 21 21 21 21 21 21 22 21 21 21 21 21 21 21 21)
(define-deferred ucd-gc-entry-703
  (let ((offsets (bytevector 21 21 21 21 21 21 21 22 21 21 21 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-703 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 22 22 21 21 21 21 21)
(define-deferred ucd-gc-entry-704
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 22 22 21 21 21 21 21)))
    (named-lambda (ucd-gc-entry-704 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 22 21 21 22 21 21 21 21 21 22 22 22 22 22)
(define-deferred ucd-gc-entry-705
  (let ((offsets (bytevector 21 21 22 21 21 22 21 21 21 21 21 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-705 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (703 704 705 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-706
  (let ((offsets (bytevector 191 2 192 2 193 2 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-706 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 14 22 22 17 17 17 17 17 17 17 17 17)
(define-deferred ucd-gc-entry-707
  (let ((offsets (bytevector 14 14 14 14 14 22 22 17 17 17 17 17 17 17 17 17)))
    (named-lambda (ucd-gc-entry-707 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (21 21 21 21 21 21 21 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-708
  (let ((offsets (bytevector 21 21 21 21 21 21 21 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-708 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 707 708 22 22)
(define-deferred ucd-gc-entry-709
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 195 2 196 2 22 0 22 0)))
    (named-lambda (ucd-gc-entry-709 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (12 12 12 12 21 21 21 21 21 21 21 22 22 22 22 22)
(define-deferred ucd-gc-entry-710
  (let ((offsets (bytevector 12 12 12 12 21 21 21 21 21 21 21 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-710 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (9 9 668 12 710 441 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-711
  (let ((offsets (bytevector 9 0 9 0 156 2 12 0 198 2 185 1 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-711 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 14 22 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-712
  (let ((offsets (bytevector 14 14 14 14 22 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-712 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 14 14 22 14 22 22 14 22 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-713
  (let ((offsets (bytevector 22 14 14 22 14 22 22 14 22 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-713 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 22 14 14 14 14 22 14 22 14 22 22 22 22)
(define-deferred ucd-gc-entry-714
  (let ((offsets (bytevector 14 14 14 22 14 14 14 14 22 14 22 14 22 22 22 22)))
    (named-lambda (ucd-gc-entry-714 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 14 22 22 22 22 14 22 14 22 14 22 14 14 14)
(define-deferred ucd-gc-entry-715
  (let ((offsets (bytevector 22 22 14 22 22 22 22 14 22 14 22 14 22 14 14 14)))
    (named-lambda (ucd-gc-entry-715 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 14 14 22 14 22 22 14 22 14 22 14 22 14 22 14)
(define-deferred ucd-gc-entry-716
  (let ((offsets (bytevector 22 14 14 22 14 22 22 14 22 14 22 14 22 14 22 14)))
    (named-lambda (ucd-gc-entry-716 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 14 14 22 14 22 22 14 14 14 14 22 14 14 14 14)
(define-deferred ucd-gc-entry-717
  (let ((offsets (bytevector 22 14 14 22 14 22 22 14 14 14 14 22 14 14 14 14)))
    (named-lambda (ucd-gc-entry-717 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 22 14 14 14 14 22 14 14 14 14 22 14 22)
(define-deferred ucd-gc-entry-718
  (let ((offsets (bytevector 14 14 14 22 14 14 14 14 22 14 14 14 14 22 14 22)))
    (named-lambda (ucd-gc-entry-718 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 22 14 14 14 14 14)
(define-deferred ucd-gc-entry-719
  (let ((offsets (bytevector 14 14 14 14 14 14 14 14 14 14 22 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-719 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 14 14 14 22 14 14 14 14 14 22 14 14 14 14 14)
(define-deferred ucd-gc-entry-720
  (let ((offsets (bytevector 22 14 14 14 22 14 14 14 14 14 22 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-720 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (6 6 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-721
  (let ((offsets (bytevector 6 6 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-721 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (712 14 713 714 715 716 717 718 719 250 720 250 22 22 22 721)
(define-deferred ucd-gc-entry-722
  (let ((offsets (bytevector 200 2 14 0 201 2 202 2 203 2 204 2 205 2 206 2 207 2 250 0 208 2 250 0 22 0 22 0 22 0 209 2)))
    (named-lambda (ucd-gc-entry-722 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (22 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-723
  (let ((offsets (bytevector 22 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-723 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 388 13 13 13 13 13 13 385 334 723 723 723 13 387)
(define-deferred ucd-gc-entry-724
  (let ((offsets (bytevector 13 0 13 0 132 1 13 0 13 0 13 0 13 0 13 0 13 0 129 1 78 1 211 2 211 2 211 2 13 0 131 1)))
    (named-lambda (ucd-gc-entry-724 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 22 22 22)
(define-deferred ucd-gc-entry-725
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 13 22 22 22)))
    (named-lambda (ucd-gc-entry-725 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 22 22 22 22 22 13 13 13 13 13 13 13 13 13 13)
(define-deferred ucd-gc-entry-726
  (let ((offsets (bytevector 22 22 22 22 22 22 13 13 13 13 13 13 13 13 13 13)))
    (named-lambda (ucd-gc-entry-726 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (214 13 334 13 13 13 388 13 13 13 725 22 22 22 726 13)
(define-deferred ucd-gc-entry-727
  (let ((offsets (bytevector 214 0 13 0 78 1 13 0 13 0 13 0 132 1 13 0 13 0 13 0 213 2 22 0 22 0 22 0 214 2 13 0)))
    (named-lambda (ucd-gc-entry-727 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-728
  (let ((offsets (bytevector 13 13 13 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-728 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (728 13 13 388 658 363 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-729
  (let ((offsets (bytevector 216 2 13 0 13 0 132 1 146 2 107 1 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-729 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 10 10 10 10 10)
(define-deferred ucd-gc-entry-730
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 10 10 10 10 10)))
    (named-lambda (ucd-gc-entry-730 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 730)
(define-deferred ucd-gc-entry-731
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 218 2)))
    (named-lambda (ucd-gc-entry-731 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 728 725 336)
(define-deferred ucd-gc-entry-732
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 13 0 216 2 213 2 80 1)))
    (named-lambda (ucd-gc-entry-732 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-733
  (let ((offsets (bytevector 13 13 13 13 13 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-733 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 385 13 13 13 13 13 733 22 22)
(define-deferred ucd-gc-entry-734
  (let ((offsets (bytevector 13 0 13 0 13 0 13 0 13 0 13 0 13 0 129 1 13 0 13 0 13 0 13 0 13 0 221 2 22 0 22 0)))
    (named-lambda (ucd-gc-entry-734 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 13 13 13 13 13 13 13 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-735
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-735 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (13 13 13 13 13 13 13 13 13 13 13 13 13 13 22 22)
(define-deferred ucd-gc-entry-736
  (let ((offsets (bytevector 13 13 13 13 13 13 13 13 13 13 13 13 13 13 22 22)))
    (named-lambda (ucd-gc-entry-736 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (388 13 13 13 735 215 13 13 735 13 736 22 22 22 22 22)
(define-deferred ucd-gc-entry-737
  (let ((offsets (bytevector 132 1 13 0 13 0 13 0 223 2 215 0 13 0 13 0 223 2 13 0 224 2 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-737 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (13 22 22 13 13 13 13 13 13 13 13 13 13 13 13 22)
(define-deferred ucd-gc-entry-738
  (let ((offsets (bytevector 13 22 22 13 13 13 13 13 13 13 13 13 13 13 13 22)))
    (named-lambda (ucd-gc-entry-738 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (22 334 735 738 388 334 22 22 13 363 22 22 505 22 22 22)
(define-deferred ucd-gc-entry-739
  (let ((offsets (bytevector 22 0 78 1 223 2 226 2 132 1 78 1 22 0 22 0 13 0 107 1 22 0 22 0 249 1 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-739 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (500 507 510 517 521 523 14 524 531 536 547 553 557 22 559 22 567 578 586 592 598 603 609 613 615 22 616 22 623 22 22 22 14 14 14 624 627 628 22 22 22 22 22 22 22 22 22 22 14 14 14 14 629 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 14 14 630 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 14 14 632 638 22 22 22 643 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 644 14 14 646 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 648 22 22 22 22 22 22 22 22 22 22 22 651 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 652 659 661 663 673 681 687 696 13 13 702 22 22 22 22 22 706 22 22 22 22 22 22 22 709 711 22 22 22 22 722 22 724 727 729 731 13 13 732 734 737 739 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-740
  (let ((offsets
         (bytevector 244
                     1
                     251
                     1
                     254
                     1
                     5
                     2
                     9
                     2
                     11
                     2
                     14
                     0
                     12
                     2
                     19
                     2
                     24
                     2
                     35
                     2
                     41
                     2
                     45
                     2
                     22
                     0
                     47
                     2
                     22
                     0
                     55
                     2
                     66
                     2
                     74
                     2
                     80
                     2
                     86
                     2
                     91
                     2
                     97
                     2
                     101
                     2
                     103
                     2
                     22
                     0
                     104
                     2
                     22
                     0
                     111
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     112
                     2
                     115
                     2
                     116
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     117
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     14
                     0
                     14
                     0
                     118
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     14
                     0
                     14
                     0
                     120
                     2
                     126
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     131
                     2
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     132
                     2
                     14
                     0
                     14
                     0
                     134
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     136
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     139
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     140
                     2
                     147
                     2
                     149
                     2
                     151
                     2
                     161
                     2
                     169
                     2
                     175
                     2
                     184
                     2
                     13
                     0
                     13
                     0
                     190
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     194
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     197
                     2
                     199
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     210
                     2
                     22
                     0
                     212
                     2
                     215
                     2
                     217
                     2
                     219
                     2
                     13
                     0
                     13
                     0
                     220
                     2
                     222
                     2
                     225
                     2
                     227
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0)))
    (named-lambda (ucd-gc-entry-740 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 510 (fix:lsh sv -7)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 376 22 22)
(define-deferred ucd-gc-entry-741
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 120 1 22 0 22 0)))
    (named-lambda (ucd-gc-entry-741 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 14 14 249 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-742
  (let ((offsets (bytevector 14 14 14 249 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-742 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 248 14 14 14 14 14 14 14 14 14 14 14 14 14 14)
(define-deferred ucd-gc-entry-743
  (let ((offsets (bytevector 14 248 14 14 14 14 14 14 14 14 14 14 14 14 14 14)))
    (named-lambda (ucd-gc-entry-743 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 647 22 22 22 22 22)
(define-deferred ucd-gc-entry-744
  (let ((offsets (bytevector 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 14 0 135 2 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-744 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (14 248 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-745
  (let ((offsets (bytevector 14 248 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-745 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 741 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 742 743 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 744 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 14 14 745 22 22 22 22 22)
(define-deferred ucd-gc-entry-746
  (let ((offsets
         (bytevector 14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     229
                     2
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     230
                     2
                     231
                     2
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     14
                     0
                     232
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     14
                     0
                     14
                     0
                     233
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0)))
    (named-lambda (ucd-gc-entry-746 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 510 (fix:lsh sv -7)))) sv table))))

;;; (22 16 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-747
  (let ((offsets (bytevector 22 16 22 22 22 22 22 22 22 22 22 22 22 22 22 22)))
    (named-lambda (ucd-gc-entry-747 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (747 22 16 16 16 16 16 16 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-748
  (let ((offsets (bytevector 235 2 22 0 16 0 16 0 16 0 16 0 16 0 16 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0 22 0)))
    (named-lambda (ucd-gc-entry-748 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 22)
(define-deferred ucd-gc-entry-749
  (let ((offsets (bytevector 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 22)))
    (named-lambda (ucd-gc-entry-749 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 (fix:lsh sv -4)))) sv table))))

;;; (748 749 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22)
(define-deferred ucd-gc-entry-750
  (let ((offsets
         (bytevector 236
                     2
                     237
                     2
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0
                     22
                     0)))
    (named-lambda (ucd-gc-entry-750 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 510 (fix:lsh sv -7)))) sv table))))

;;; (28 28 28 28 28 28 28 28 28 28 28 28 28 28 22 22)
(define-deferred ucd-gc-entry-751
  (let ((offsets (bytevector 28 28 28 28 28 28 28 28 28 28 28 28 28 28 22 22)))
    (named-lambda (ucd-gc-entry-751 sv table)
      ((vector-ref table (bytevector-u8-ref offsets (fix:and 15 sv))) sv table))))

;;; (28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 751)
(define-deferred ucd-gc-entry-752
  (let ((offsets (bytevector 28 0 28 0 28 0 28 0 28 0 28 0 28 0 28 0 28 0 28 0 28 0 28 0 28 0 28 0 28 0 239 2)))
    (named-lambda (ucd-gc-entry-752 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 30 (fix:lsh sv -3)))) sv table))))

;;; (28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 752)
(define-deferred ucd-gc-entry-753
  (let ((offsets
         (bytevector 28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     28
                     0
                     240
                     2)))
    (named-lambda (ucd-gc-entry-753 sv table)
      ((vector-ref table (bytevector-u16le-ref offsets (fix:and 510 (fix:lsh sv -7)))) sv table))))

(define ucd-gc-entries)

(add-boot-init! (lambda () (set! ucd-gc-entries (make-vector 754)) (initialize-ucd-gc-entries-0) (initialize-ucd-gc-entries-1) (initialize-ucd-gc-entries-2) (initialize-ucd-gc-entries-3) (initialize-ucd-gc-entries-4) (initialize-ucd-gc-entries-5) (initialize-ucd-gc-entries-6) (initialize-ucd-gc-entries-7)))

(define (initialize-ucd-gc-entries-0)
  (vector-set! ucd-gc-entries 0 ucd-gc-entry-0)
  (vector-set! ucd-gc-entries 1 ucd-gc-entry-1)
  (vector-set! ucd-gc-entries 2 ucd-gc-entry-2)
  (vector-set! ucd-gc-entries 3 ucd-gc-entry-3)
  (vector-set! ucd-gc-entries 4 ucd-gc-entry-4)
  (vector-set! ucd-gc-entries 5 ucd-gc-entry-5)
  (vector-set! ucd-gc-entries 6 ucd-gc-entry-6)
  (vector-set! ucd-gc-entries 7 ucd-gc-entry-7)
  (vector-set! ucd-gc-entries 8 ucd-gc-entry-8)
  (vector-set! ucd-gc-entries 9 ucd-gc-entry-9)
  (vector-set! ucd-gc-entries 10 ucd-gc-entry-10)
  (vector-set! ucd-gc-entries 11 ucd-gc-entry-11)
  (vector-set! ucd-gc-entries 12 ucd-gc-entry-12)
  (vector-set! ucd-gc-entries 13 ucd-gc-entry-13)
  (vector-set! ucd-gc-entries 14 ucd-gc-entry-14)
  (vector-set! ucd-gc-entries 15 ucd-gc-entry-15)
  (vector-set! ucd-gc-entries 16 ucd-gc-entry-16)
  (vector-set! ucd-gc-entries 17 ucd-gc-entry-17)
  (vector-set! ucd-gc-entries 18 ucd-gc-entry-18)
  (vector-set! ucd-gc-entries 19 ucd-gc-entry-19)
  (vector-set! ucd-gc-entries 20 ucd-gc-entry-20)
  (vector-set! ucd-gc-entries 21 ucd-gc-entry-21)
  (vector-set! ucd-gc-entries 22 ucd-gc-entry-22)
  (vector-set! ucd-gc-entries 23 ucd-gc-entry-23)
  (vector-set! ucd-gc-entries 24 ucd-gc-entry-24)
  (vector-set! ucd-gc-entries 25 ucd-gc-entry-25)
  (vector-set! ucd-gc-entries 26 ucd-gc-entry-26)
  (vector-set! ucd-gc-entries 27 ucd-gc-entry-27)
  (vector-set! ucd-gc-entries 28 ucd-gc-entry-28)
  (vector-set! ucd-gc-entries 29 ucd-gc-entry-29)
  (vector-set! ucd-gc-entries 30 ucd-gc-entry-30)
  (vector-set! ucd-gc-entries 31 ucd-gc-entry-31)
  (vector-set! ucd-gc-entries 32 ucd-gc-entry-32)
  (vector-set! ucd-gc-entries 33 ucd-gc-entry-33)
  (vector-set! ucd-gc-entries 34 ucd-gc-entry-34)
  (vector-set! ucd-gc-entries 35 ucd-gc-entry-35)
  (vector-set! ucd-gc-entries 36 ucd-gc-entry-36)
  (vector-set! ucd-gc-entries 37 ucd-gc-entry-37)
  (vector-set! ucd-gc-entries 38 ucd-gc-entry-38)
  (vector-set! ucd-gc-entries 39 ucd-gc-entry-39)
  (vector-set! ucd-gc-entries 40 ucd-gc-entry-40)
  (vector-set! ucd-gc-entries 41 ucd-gc-entry-41)
  (vector-set! ucd-gc-entries 42 ucd-gc-entry-42)
  (vector-set! ucd-gc-entries 43 ucd-gc-entry-43)
  (vector-set! ucd-gc-entries 44 ucd-gc-entry-44)
  (vector-set! ucd-gc-entries 45 ucd-gc-entry-45)
  (vector-set! ucd-gc-entries 46 ucd-gc-entry-46)
  (vector-set! ucd-gc-entries 47 ucd-gc-entry-47)
  (vector-set! ucd-gc-entries 48 ucd-gc-entry-48)
  (vector-set! ucd-gc-entries 49 ucd-gc-entry-49)
  (vector-set! ucd-gc-entries 50 ucd-gc-entry-50)
  (vector-set! ucd-gc-entries 51 ucd-gc-entry-51)
  (vector-set! ucd-gc-entries 52 ucd-gc-entry-52)
  (vector-set! ucd-gc-entries 53 ucd-gc-entry-53)
  (vector-set! ucd-gc-entries 54 ucd-gc-entry-54)
  (vector-set! ucd-gc-entries 55 ucd-gc-entry-55)
  (vector-set! ucd-gc-entries 56 ucd-gc-entry-56)
  (vector-set! ucd-gc-entries 57 ucd-gc-entry-57)
  (vector-set! ucd-gc-entries 58 ucd-gc-entry-58)
  (vector-set! ucd-gc-entries 59 ucd-gc-entry-59)
  (vector-set! ucd-gc-entries 60 ucd-gc-entry-60)
  (vector-set! ucd-gc-entries 61 ucd-gc-entry-61)
  (vector-set! ucd-gc-entries 62 ucd-gc-entry-62)
  (vector-set! ucd-gc-entries 63 ucd-gc-entry-63)
  (vector-set! ucd-gc-entries 64 ucd-gc-entry-64)
  (vector-set! ucd-gc-entries 65 ucd-gc-entry-65)
  (vector-set! ucd-gc-entries 66 ucd-gc-entry-66)
  (vector-set! ucd-gc-entries 67 ucd-gc-entry-67)
  (vector-set! ucd-gc-entries 68 ucd-gc-entry-68)
  (vector-set! ucd-gc-entries 69 ucd-gc-entry-69)
  (vector-set! ucd-gc-entries 70 ucd-gc-entry-70)
  (vector-set! ucd-gc-entries 71 ucd-gc-entry-71)
  (vector-set! ucd-gc-entries 72 ucd-gc-entry-72)
  (vector-set! ucd-gc-entries 73 ucd-gc-entry-73)
  (vector-set! ucd-gc-entries 74 ucd-gc-entry-74)
  (vector-set! ucd-gc-entries 75 ucd-gc-entry-75)
  (vector-set! ucd-gc-entries 76 ucd-gc-entry-76)
  (vector-set! ucd-gc-entries 77 ucd-gc-entry-77)
  (vector-set! ucd-gc-entries 78 ucd-gc-entry-78)
  (vector-set! ucd-gc-entries 79 ucd-gc-entry-79)
  (vector-set! ucd-gc-entries 80 ucd-gc-entry-80)
  (vector-set! ucd-gc-entries 81 ucd-gc-entry-81)
  (vector-set! ucd-gc-entries 82 ucd-gc-entry-82)
  (vector-set! ucd-gc-entries 83 ucd-gc-entry-83)
  (vector-set! ucd-gc-entries 84 ucd-gc-entry-84)
  (vector-set! ucd-gc-entries 85 ucd-gc-entry-85)
  (vector-set! ucd-gc-entries 86 ucd-gc-entry-86)
  (vector-set! ucd-gc-entries 87 ucd-gc-entry-87)
  (vector-set! ucd-gc-entries 88 ucd-gc-entry-88)
  (vector-set! ucd-gc-entries 89 ucd-gc-entry-89)
  (vector-set! ucd-gc-entries 90 ucd-gc-entry-90)
  (vector-set! ucd-gc-entries 91 ucd-gc-entry-91)
  (vector-set! ucd-gc-entries 92 ucd-gc-entry-92)
  (vector-set! ucd-gc-entries 93 ucd-gc-entry-93)
  (vector-set! ucd-gc-entries 94 ucd-gc-entry-94)
  (vector-set! ucd-gc-entries 95 ucd-gc-entry-95)
  (vector-set! ucd-gc-entries 96 ucd-gc-entry-96)
  (vector-set! ucd-gc-entries 97 ucd-gc-entry-97)
  (vector-set! ucd-gc-entries 98 ucd-gc-entry-98)
  (vector-set! ucd-gc-entries 99 ucd-gc-entry-99))

(define (initialize-ucd-gc-entries-1)
  (vector-set! ucd-gc-entries 100 ucd-gc-entry-100)
  (vector-set! ucd-gc-entries 101 ucd-gc-entry-101)
  (vector-set! ucd-gc-entries 102 ucd-gc-entry-102)
  (vector-set! ucd-gc-entries 103 ucd-gc-entry-103)
  (vector-set! ucd-gc-entries 104 ucd-gc-entry-104)
  (vector-set! ucd-gc-entries 105 ucd-gc-entry-105)
  (vector-set! ucd-gc-entries 106 ucd-gc-entry-106)
  (vector-set! ucd-gc-entries 107 ucd-gc-entry-107)
  (vector-set! ucd-gc-entries 108 ucd-gc-entry-108)
  (vector-set! ucd-gc-entries 109 ucd-gc-entry-109)
  (vector-set! ucd-gc-entries 110 ucd-gc-entry-110)
  (vector-set! ucd-gc-entries 111 ucd-gc-entry-111)
  (vector-set! ucd-gc-entries 112 ucd-gc-entry-112)
  (vector-set! ucd-gc-entries 113 ucd-gc-entry-113)
  (vector-set! ucd-gc-entries 114 ucd-gc-entry-114)
  (vector-set! ucd-gc-entries 115 ucd-gc-entry-115)
  (vector-set! ucd-gc-entries 116 ucd-gc-entry-116)
  (vector-set! ucd-gc-entries 117 ucd-gc-entry-117)
  (vector-set! ucd-gc-entries 118 ucd-gc-entry-118)
  (vector-set! ucd-gc-entries 119 ucd-gc-entry-119)
  (vector-set! ucd-gc-entries 120 ucd-gc-entry-120)
  (vector-set! ucd-gc-entries 121 ucd-gc-entry-121)
  (vector-set! ucd-gc-entries 122 ucd-gc-entry-122)
  (vector-set! ucd-gc-entries 123 ucd-gc-entry-123)
  (vector-set! ucd-gc-entries 124 ucd-gc-entry-124)
  (vector-set! ucd-gc-entries 125 ucd-gc-entry-125)
  (vector-set! ucd-gc-entries 126 ucd-gc-entry-126)
  (vector-set! ucd-gc-entries 127 ucd-gc-entry-127)
  (vector-set! ucd-gc-entries 128 ucd-gc-entry-128)
  (vector-set! ucd-gc-entries 129 ucd-gc-entry-129)
  (vector-set! ucd-gc-entries 130 ucd-gc-entry-130)
  (vector-set! ucd-gc-entries 131 ucd-gc-entry-131)
  (vector-set! ucd-gc-entries 132 ucd-gc-entry-132)
  (vector-set! ucd-gc-entries 133 ucd-gc-entry-133)
  (vector-set! ucd-gc-entries 134 ucd-gc-entry-134)
  (vector-set! ucd-gc-entries 135 ucd-gc-entry-135)
  (vector-set! ucd-gc-entries 136 ucd-gc-entry-136)
  (vector-set! ucd-gc-entries 137 ucd-gc-entry-137)
  (vector-set! ucd-gc-entries 138 ucd-gc-entry-138)
  (vector-set! ucd-gc-entries 139 ucd-gc-entry-139)
  (vector-set! ucd-gc-entries 140 ucd-gc-entry-140)
  (vector-set! ucd-gc-entries 141 ucd-gc-entry-141)
  (vector-set! ucd-gc-entries 142 ucd-gc-entry-142)
  (vector-set! ucd-gc-entries 143 ucd-gc-entry-143)
  (vector-set! ucd-gc-entries 144 ucd-gc-entry-144)
  (vector-set! ucd-gc-entries 145 ucd-gc-entry-145)
  (vector-set! ucd-gc-entries 146 ucd-gc-entry-146)
  (vector-set! ucd-gc-entries 147 ucd-gc-entry-147)
  (vector-set! ucd-gc-entries 148 ucd-gc-entry-148)
  (vector-set! ucd-gc-entries 149 ucd-gc-entry-149)
  (vector-set! ucd-gc-entries 150 ucd-gc-entry-150)
  (vector-set! ucd-gc-entries 151 ucd-gc-entry-151)
  (vector-set! ucd-gc-entries 152 ucd-gc-entry-152)
  (vector-set! ucd-gc-entries 153 ucd-gc-entry-153)
  (vector-set! ucd-gc-entries 154 ucd-gc-entry-154)
  (vector-set! ucd-gc-entries 155 ucd-gc-entry-155)
  (vector-set! ucd-gc-entries 156 ucd-gc-entry-156)
  (vector-set! ucd-gc-entries 157 ucd-gc-entry-157)
  (vector-set! ucd-gc-entries 158 ucd-gc-entry-158)
  (vector-set! ucd-gc-entries 159 ucd-gc-entry-159)
  (vector-set! ucd-gc-entries 160 ucd-gc-entry-160)
  (vector-set! ucd-gc-entries 161 ucd-gc-entry-161)
  (vector-set! ucd-gc-entries 162 ucd-gc-entry-162)
  (vector-set! ucd-gc-entries 163 ucd-gc-entry-163)
  (vector-set! ucd-gc-entries 164 ucd-gc-entry-164)
  (vector-set! ucd-gc-entries 165 ucd-gc-entry-165)
  (vector-set! ucd-gc-entries 166 ucd-gc-entry-166)
  (vector-set! ucd-gc-entries 167 ucd-gc-entry-167)
  (vector-set! ucd-gc-entries 168 ucd-gc-entry-168)
  (vector-set! ucd-gc-entries 169 ucd-gc-entry-169)
  (vector-set! ucd-gc-entries 170 ucd-gc-entry-170)
  (vector-set! ucd-gc-entries 171 ucd-gc-entry-171)
  (vector-set! ucd-gc-entries 172 ucd-gc-entry-172)
  (vector-set! ucd-gc-entries 173 ucd-gc-entry-173)
  (vector-set! ucd-gc-entries 174 ucd-gc-entry-174)
  (vector-set! ucd-gc-entries 175 ucd-gc-entry-175)
  (vector-set! ucd-gc-entries 176 ucd-gc-entry-176)
  (vector-set! ucd-gc-entries 177 ucd-gc-entry-177)
  (vector-set! ucd-gc-entries 178 ucd-gc-entry-178)
  (vector-set! ucd-gc-entries 179 ucd-gc-entry-179)
  (vector-set! ucd-gc-entries 180 ucd-gc-entry-180)
  (vector-set! ucd-gc-entries 181 ucd-gc-entry-181)
  (vector-set! ucd-gc-entries 182 ucd-gc-entry-182)
  (vector-set! ucd-gc-entries 183 ucd-gc-entry-183)
  (vector-set! ucd-gc-entries 184 ucd-gc-entry-184)
  (vector-set! ucd-gc-entries 185 ucd-gc-entry-185)
  (vector-set! ucd-gc-entries 186 ucd-gc-entry-186)
  (vector-set! ucd-gc-entries 187 ucd-gc-entry-187)
  (vector-set! ucd-gc-entries 188 ucd-gc-entry-188)
  (vector-set! ucd-gc-entries 189 ucd-gc-entry-189)
  (vector-set! ucd-gc-entries 190 ucd-gc-entry-190)
  (vector-set! ucd-gc-entries 191 ucd-gc-entry-191)
  (vector-set! ucd-gc-entries 192 ucd-gc-entry-192)
  (vector-set! ucd-gc-entries 193 ucd-gc-entry-193)
  (vector-set! ucd-gc-entries 194 ucd-gc-entry-194)
  (vector-set! ucd-gc-entries 195 ucd-gc-entry-195)
  (vector-set! ucd-gc-entries 196 ucd-gc-entry-196)
  (vector-set! ucd-gc-entries 197 ucd-gc-entry-197)
  (vector-set! ucd-gc-entries 198 ucd-gc-entry-198)
  (vector-set! ucd-gc-entries 199 ucd-gc-entry-199))

(define (initialize-ucd-gc-entries-2)
  (vector-set! ucd-gc-entries 200 ucd-gc-entry-200)
  (vector-set! ucd-gc-entries 201 ucd-gc-entry-201)
  (vector-set! ucd-gc-entries 202 ucd-gc-entry-202)
  (vector-set! ucd-gc-entries 203 ucd-gc-entry-203)
  (vector-set! ucd-gc-entries 204 ucd-gc-entry-204)
  (vector-set! ucd-gc-entries 205 ucd-gc-entry-205)
  (vector-set! ucd-gc-entries 206 ucd-gc-entry-206)
  (vector-set! ucd-gc-entries 207 ucd-gc-entry-207)
  (vector-set! ucd-gc-entries 208 ucd-gc-entry-208)
  (vector-set! ucd-gc-entries 209 ucd-gc-entry-209)
  (vector-set! ucd-gc-entries 210 ucd-gc-entry-210)
  (vector-set! ucd-gc-entries 211 ucd-gc-entry-211)
  (vector-set! ucd-gc-entries 212 ucd-gc-entry-212)
  (vector-set! ucd-gc-entries 213 ucd-gc-entry-213)
  (vector-set! ucd-gc-entries 214 ucd-gc-entry-214)
  (vector-set! ucd-gc-entries 215 ucd-gc-entry-215)
  (vector-set! ucd-gc-entries 216 ucd-gc-entry-216)
  (vector-set! ucd-gc-entries 217 ucd-gc-entry-217)
  (vector-set! ucd-gc-entries 218 ucd-gc-entry-218)
  (vector-set! ucd-gc-entries 219 ucd-gc-entry-219)
  (vector-set! ucd-gc-entries 220 ucd-gc-entry-220)
  (vector-set! ucd-gc-entries 221 ucd-gc-entry-221)
  (vector-set! ucd-gc-entries 222 ucd-gc-entry-222)
  (vector-set! ucd-gc-entries 223 ucd-gc-entry-223)
  (vector-set! ucd-gc-entries 224 ucd-gc-entry-224)
  (vector-set! ucd-gc-entries 225 ucd-gc-entry-225)
  (vector-set! ucd-gc-entries 226 ucd-gc-entry-226)
  (vector-set! ucd-gc-entries 227 ucd-gc-entry-227)
  (vector-set! ucd-gc-entries 228 ucd-gc-entry-228)
  (vector-set! ucd-gc-entries 229 ucd-gc-entry-229)
  (vector-set! ucd-gc-entries 230 ucd-gc-entry-230)
  (vector-set! ucd-gc-entries 231 ucd-gc-entry-231)
  (vector-set! ucd-gc-entries 232 ucd-gc-entry-232)
  (vector-set! ucd-gc-entries 233 ucd-gc-entry-233)
  (vector-set! ucd-gc-entries 234 ucd-gc-entry-234)
  (vector-set! ucd-gc-entries 235 ucd-gc-entry-235)
  (vector-set! ucd-gc-entries 236 ucd-gc-entry-236)
  (vector-set! ucd-gc-entries 237 ucd-gc-entry-237)
  (vector-set! ucd-gc-entries 238 ucd-gc-entry-238)
  (vector-set! ucd-gc-entries 239 ucd-gc-entry-239)
  (vector-set! ucd-gc-entries 240 ucd-gc-entry-240)
  (vector-set! ucd-gc-entries 241 ucd-gc-entry-241)
  (vector-set! ucd-gc-entries 242 ucd-gc-entry-242)
  (vector-set! ucd-gc-entries 243 ucd-gc-entry-243)
  (vector-set! ucd-gc-entries 244 ucd-gc-entry-244)
  (vector-set! ucd-gc-entries 245 ucd-gc-entry-245)
  (vector-set! ucd-gc-entries 246 ucd-gc-entry-246)
  (vector-set! ucd-gc-entries 247 ucd-gc-entry-247)
  (vector-set! ucd-gc-entries 248 ucd-gc-entry-248)
  (vector-set! ucd-gc-entries 249 ucd-gc-entry-249)
  (vector-set! ucd-gc-entries 250 ucd-gc-entry-250)
  (vector-set! ucd-gc-entries 251 ucd-gc-entry-251)
  (vector-set! ucd-gc-entries 252 ucd-gc-entry-252)
  (vector-set! ucd-gc-entries 253 ucd-gc-entry-253)
  (vector-set! ucd-gc-entries 254 ucd-gc-entry-254)
  (vector-set! ucd-gc-entries 255 ucd-gc-entry-255)
  (vector-set! ucd-gc-entries 256 ucd-gc-entry-256)
  (vector-set! ucd-gc-entries 257 ucd-gc-entry-257)
  (vector-set! ucd-gc-entries 258 ucd-gc-entry-258)
  (vector-set! ucd-gc-entries 259 ucd-gc-entry-259)
  (vector-set! ucd-gc-entries 260 ucd-gc-entry-260)
  (vector-set! ucd-gc-entries 261 ucd-gc-entry-261)
  (vector-set! ucd-gc-entries 262 ucd-gc-entry-262)
  (vector-set! ucd-gc-entries 263 ucd-gc-entry-263)
  (vector-set! ucd-gc-entries 264 ucd-gc-entry-264)
  (vector-set! ucd-gc-entries 265 ucd-gc-entry-265)
  (vector-set! ucd-gc-entries 266 ucd-gc-entry-266)
  (vector-set! ucd-gc-entries 267 ucd-gc-entry-267)
  (vector-set! ucd-gc-entries 268 ucd-gc-entry-268)
  (vector-set! ucd-gc-entries 269 ucd-gc-entry-269)
  (vector-set! ucd-gc-entries 270 ucd-gc-entry-270)
  (vector-set! ucd-gc-entries 271 ucd-gc-entry-271)
  (vector-set! ucd-gc-entries 272 ucd-gc-entry-272)
  (vector-set! ucd-gc-entries 273 ucd-gc-entry-273)
  (vector-set! ucd-gc-entries 274 ucd-gc-entry-274)
  (vector-set! ucd-gc-entries 275 ucd-gc-entry-275)
  (vector-set! ucd-gc-entries 276 ucd-gc-entry-276)
  (vector-set! ucd-gc-entries 277 ucd-gc-entry-277)
  (vector-set! ucd-gc-entries 278 ucd-gc-entry-278)
  (vector-set! ucd-gc-entries 279 ucd-gc-entry-279)
  (vector-set! ucd-gc-entries 280 ucd-gc-entry-280)
  (vector-set! ucd-gc-entries 281 ucd-gc-entry-281)
  (vector-set! ucd-gc-entries 282 ucd-gc-entry-282)
  (vector-set! ucd-gc-entries 283 ucd-gc-entry-283)
  (vector-set! ucd-gc-entries 284 ucd-gc-entry-284)
  (vector-set! ucd-gc-entries 285 ucd-gc-entry-285)
  (vector-set! ucd-gc-entries 286 ucd-gc-entry-286)
  (vector-set! ucd-gc-entries 287 ucd-gc-entry-287)
  (vector-set! ucd-gc-entries 288 ucd-gc-entry-288)
  (vector-set! ucd-gc-entries 289 ucd-gc-entry-289)
  (vector-set! ucd-gc-entries 290 ucd-gc-entry-290)
  (vector-set! ucd-gc-entries 291 ucd-gc-entry-291)
  (vector-set! ucd-gc-entries 292 ucd-gc-entry-292)
  (vector-set! ucd-gc-entries 293 ucd-gc-entry-293)
  (vector-set! ucd-gc-entries 294 ucd-gc-entry-294)
  (vector-set! ucd-gc-entries 295 ucd-gc-entry-295)
  (vector-set! ucd-gc-entries 296 ucd-gc-entry-296)
  (vector-set! ucd-gc-entries 297 ucd-gc-entry-297)
  (vector-set! ucd-gc-entries 298 ucd-gc-entry-298)
  (vector-set! ucd-gc-entries 299 ucd-gc-entry-299))

(define (initialize-ucd-gc-entries-3)
  (vector-set! ucd-gc-entries 300 ucd-gc-entry-300)
  (vector-set! ucd-gc-entries 301 ucd-gc-entry-301)
  (vector-set! ucd-gc-entries 302 ucd-gc-entry-302)
  (vector-set! ucd-gc-entries 303 ucd-gc-entry-303)
  (vector-set! ucd-gc-entries 304 ucd-gc-entry-304)
  (vector-set! ucd-gc-entries 305 ucd-gc-entry-305)
  (vector-set! ucd-gc-entries 306 ucd-gc-entry-306)
  (vector-set! ucd-gc-entries 307 ucd-gc-entry-307)
  (vector-set! ucd-gc-entries 308 ucd-gc-entry-308)
  (vector-set! ucd-gc-entries 309 ucd-gc-entry-309)
  (vector-set! ucd-gc-entries 310 ucd-gc-entry-310)
  (vector-set! ucd-gc-entries 311 ucd-gc-entry-311)
  (vector-set! ucd-gc-entries 312 ucd-gc-entry-312)
  (vector-set! ucd-gc-entries 313 ucd-gc-entry-313)
  (vector-set! ucd-gc-entries 314 ucd-gc-entry-314)
  (vector-set! ucd-gc-entries 315 ucd-gc-entry-315)
  (vector-set! ucd-gc-entries 316 ucd-gc-entry-316)
  (vector-set! ucd-gc-entries 317 ucd-gc-entry-317)
  (vector-set! ucd-gc-entries 318 ucd-gc-entry-318)
  (vector-set! ucd-gc-entries 319 ucd-gc-entry-319)
  (vector-set! ucd-gc-entries 320 ucd-gc-entry-320)
  (vector-set! ucd-gc-entries 321 ucd-gc-entry-321)
  (vector-set! ucd-gc-entries 322 ucd-gc-entry-322)
  (vector-set! ucd-gc-entries 323 ucd-gc-entry-323)
  (vector-set! ucd-gc-entries 324 ucd-gc-entry-324)
  (vector-set! ucd-gc-entries 325 ucd-gc-entry-325)
  (vector-set! ucd-gc-entries 326 ucd-gc-entry-326)
  (vector-set! ucd-gc-entries 327 ucd-gc-entry-327)
  (vector-set! ucd-gc-entries 328 ucd-gc-entry-328)
  (vector-set! ucd-gc-entries 329 ucd-gc-entry-329)
  (vector-set! ucd-gc-entries 330 ucd-gc-entry-330)
  (vector-set! ucd-gc-entries 331 ucd-gc-entry-331)
  (vector-set! ucd-gc-entries 332 ucd-gc-entry-332)
  (vector-set! ucd-gc-entries 333 ucd-gc-entry-333)
  (vector-set! ucd-gc-entries 334 ucd-gc-entry-334)
  (vector-set! ucd-gc-entries 335 ucd-gc-entry-335)
  (vector-set! ucd-gc-entries 336 ucd-gc-entry-336)
  (vector-set! ucd-gc-entries 337 ucd-gc-entry-337)
  (vector-set! ucd-gc-entries 338 ucd-gc-entry-338)
  (vector-set! ucd-gc-entries 339 ucd-gc-entry-339)
  (vector-set! ucd-gc-entries 340 ucd-gc-entry-340)
  (vector-set! ucd-gc-entries 341 ucd-gc-entry-341)
  (vector-set! ucd-gc-entries 342 ucd-gc-entry-342)
  (vector-set! ucd-gc-entries 343 ucd-gc-entry-343)
  (vector-set! ucd-gc-entries 344 ucd-gc-entry-344)
  (vector-set! ucd-gc-entries 345 ucd-gc-entry-345)
  (vector-set! ucd-gc-entries 346 ucd-gc-entry-346)
  (vector-set! ucd-gc-entries 347 ucd-gc-entry-347)
  (vector-set! ucd-gc-entries 348 ucd-gc-entry-348)
  (vector-set! ucd-gc-entries 349 ucd-gc-entry-349)
  (vector-set! ucd-gc-entries 350 ucd-gc-entry-350)
  (vector-set! ucd-gc-entries 351 ucd-gc-entry-351)
  (vector-set! ucd-gc-entries 352 ucd-gc-entry-352)
  (vector-set! ucd-gc-entries 353 ucd-gc-entry-353)
  (vector-set! ucd-gc-entries 354 ucd-gc-entry-354)
  (vector-set! ucd-gc-entries 355 ucd-gc-entry-355)
  (vector-set! ucd-gc-entries 356 ucd-gc-entry-356)
  (vector-set! ucd-gc-entries 357 ucd-gc-entry-357)
  (vector-set! ucd-gc-entries 358 ucd-gc-entry-358)
  (vector-set! ucd-gc-entries 359 ucd-gc-entry-359)
  (vector-set! ucd-gc-entries 360 ucd-gc-entry-360)
  (vector-set! ucd-gc-entries 361 ucd-gc-entry-361)
  (vector-set! ucd-gc-entries 362 ucd-gc-entry-362)
  (vector-set! ucd-gc-entries 363 ucd-gc-entry-363)
  (vector-set! ucd-gc-entries 364 ucd-gc-entry-364)
  (vector-set! ucd-gc-entries 365 ucd-gc-entry-365)
  (vector-set! ucd-gc-entries 366 ucd-gc-entry-366)
  (vector-set! ucd-gc-entries 367 ucd-gc-entry-367)
  (vector-set! ucd-gc-entries 368 ucd-gc-entry-368)
  (vector-set! ucd-gc-entries 369 ucd-gc-entry-369)
  (vector-set! ucd-gc-entries 370 ucd-gc-entry-370)
  (vector-set! ucd-gc-entries 371 ucd-gc-entry-371)
  (vector-set! ucd-gc-entries 372 ucd-gc-entry-372)
  (vector-set! ucd-gc-entries 373 ucd-gc-entry-373)
  (vector-set! ucd-gc-entries 374 ucd-gc-entry-374)
  (vector-set! ucd-gc-entries 375 ucd-gc-entry-375)
  (vector-set! ucd-gc-entries 376 ucd-gc-entry-376)
  (vector-set! ucd-gc-entries 377 ucd-gc-entry-377)
  (vector-set! ucd-gc-entries 378 ucd-gc-entry-378)
  (vector-set! ucd-gc-entries 379 ucd-gc-entry-379)
  (vector-set! ucd-gc-entries 380 ucd-gc-entry-380)
  (vector-set! ucd-gc-entries 381 ucd-gc-entry-381)
  (vector-set! ucd-gc-entries 382 ucd-gc-entry-382)
  (vector-set! ucd-gc-entries 383 ucd-gc-entry-383)
  (vector-set! ucd-gc-entries 384 ucd-gc-entry-384)
  (vector-set! ucd-gc-entries 385 ucd-gc-entry-385)
  (vector-set! ucd-gc-entries 386 ucd-gc-entry-386)
  (vector-set! ucd-gc-entries 387 ucd-gc-entry-387)
  (vector-set! ucd-gc-entries 388 ucd-gc-entry-388)
  (vector-set! ucd-gc-entries 389 ucd-gc-entry-389)
  (vector-set! ucd-gc-entries 390 ucd-gc-entry-390)
  (vector-set! ucd-gc-entries 391 ucd-gc-entry-391)
  (vector-set! ucd-gc-entries 392 ucd-gc-entry-392)
  (vector-set! ucd-gc-entries 393 ucd-gc-entry-393)
  (vector-set! ucd-gc-entries 394 ucd-gc-entry-394)
  (vector-set! ucd-gc-entries 395 ucd-gc-entry-395)
  (vector-set! ucd-gc-entries 396 ucd-gc-entry-396)
  (vector-set! ucd-gc-entries 397 ucd-gc-entry-397)
  (vector-set! ucd-gc-entries 398 ucd-gc-entry-398)
  (vector-set! ucd-gc-entries 399 ucd-gc-entry-399))

(define (initialize-ucd-gc-entries-4)
  (vector-set! ucd-gc-entries 400 ucd-gc-entry-400)
  (vector-set! ucd-gc-entries 401 ucd-gc-entry-401)
  (vector-set! ucd-gc-entries 402 ucd-gc-entry-402)
  (vector-set! ucd-gc-entries 403 ucd-gc-entry-403)
  (vector-set! ucd-gc-entries 404 ucd-gc-entry-404)
  (vector-set! ucd-gc-entries 405 ucd-gc-entry-405)
  (vector-set! ucd-gc-entries 406 ucd-gc-entry-406)
  (vector-set! ucd-gc-entries 407 ucd-gc-entry-407)
  (vector-set! ucd-gc-entries 408 ucd-gc-entry-408)
  (vector-set! ucd-gc-entries 409 ucd-gc-entry-409)
  (vector-set! ucd-gc-entries 410 ucd-gc-entry-410)
  (vector-set! ucd-gc-entries 411 ucd-gc-entry-411)
  (vector-set! ucd-gc-entries 412 ucd-gc-entry-412)
  (vector-set! ucd-gc-entries 413 ucd-gc-entry-413)
  (vector-set! ucd-gc-entries 414 ucd-gc-entry-414)
  (vector-set! ucd-gc-entries 415 ucd-gc-entry-415)
  (vector-set! ucd-gc-entries 416 ucd-gc-entry-416)
  (vector-set! ucd-gc-entries 417 ucd-gc-entry-417)
  (vector-set! ucd-gc-entries 418 ucd-gc-entry-418)
  (vector-set! ucd-gc-entries 419 ucd-gc-entry-419)
  (vector-set! ucd-gc-entries 420 ucd-gc-entry-420)
  (vector-set! ucd-gc-entries 421 ucd-gc-entry-421)
  (vector-set! ucd-gc-entries 422 ucd-gc-entry-422)
  (vector-set! ucd-gc-entries 423 ucd-gc-entry-423)
  (vector-set! ucd-gc-entries 424 ucd-gc-entry-424)
  (vector-set! ucd-gc-entries 425 ucd-gc-entry-425)
  (vector-set! ucd-gc-entries 426 ucd-gc-entry-426)
  (vector-set! ucd-gc-entries 427 ucd-gc-entry-427)
  (vector-set! ucd-gc-entries 428 ucd-gc-entry-428)
  (vector-set! ucd-gc-entries 429 ucd-gc-entry-429)
  (vector-set! ucd-gc-entries 430 ucd-gc-entry-430)
  (vector-set! ucd-gc-entries 431 ucd-gc-entry-431)
  (vector-set! ucd-gc-entries 432 ucd-gc-entry-432)
  (vector-set! ucd-gc-entries 433 ucd-gc-entry-433)
  (vector-set! ucd-gc-entries 434 ucd-gc-entry-434)
  (vector-set! ucd-gc-entries 435 ucd-gc-entry-435)
  (vector-set! ucd-gc-entries 436 ucd-gc-entry-436)
  (vector-set! ucd-gc-entries 437 ucd-gc-entry-437)
  (vector-set! ucd-gc-entries 438 ucd-gc-entry-438)
  (vector-set! ucd-gc-entries 439 ucd-gc-entry-439)
  (vector-set! ucd-gc-entries 440 ucd-gc-entry-440)
  (vector-set! ucd-gc-entries 441 ucd-gc-entry-441)
  (vector-set! ucd-gc-entries 442 ucd-gc-entry-442)
  (vector-set! ucd-gc-entries 443 ucd-gc-entry-443)
  (vector-set! ucd-gc-entries 444 ucd-gc-entry-444)
  (vector-set! ucd-gc-entries 445 ucd-gc-entry-445)
  (vector-set! ucd-gc-entries 446 ucd-gc-entry-446)
  (vector-set! ucd-gc-entries 447 ucd-gc-entry-447)
  (vector-set! ucd-gc-entries 448 ucd-gc-entry-448)
  (vector-set! ucd-gc-entries 449 ucd-gc-entry-449)
  (vector-set! ucd-gc-entries 450 ucd-gc-entry-450)
  (vector-set! ucd-gc-entries 451 ucd-gc-entry-451)
  (vector-set! ucd-gc-entries 452 ucd-gc-entry-452)
  (vector-set! ucd-gc-entries 453 ucd-gc-entry-453)
  (vector-set! ucd-gc-entries 454 ucd-gc-entry-454)
  (vector-set! ucd-gc-entries 455 ucd-gc-entry-455)
  (vector-set! ucd-gc-entries 456 ucd-gc-entry-456)
  (vector-set! ucd-gc-entries 457 ucd-gc-entry-457)
  (vector-set! ucd-gc-entries 458 ucd-gc-entry-458)
  (vector-set! ucd-gc-entries 459 ucd-gc-entry-459)
  (vector-set! ucd-gc-entries 460 ucd-gc-entry-460)
  (vector-set! ucd-gc-entries 461 ucd-gc-entry-461)
  (vector-set! ucd-gc-entries 462 ucd-gc-entry-462)
  (vector-set! ucd-gc-entries 463 ucd-gc-entry-463)
  (vector-set! ucd-gc-entries 464 ucd-gc-entry-464)
  (vector-set! ucd-gc-entries 465 ucd-gc-entry-465)
  (vector-set! ucd-gc-entries 466 ucd-gc-entry-466)
  (vector-set! ucd-gc-entries 467 ucd-gc-entry-467)
  (vector-set! ucd-gc-entries 468 ucd-gc-entry-468)
  (vector-set! ucd-gc-entries 469 ucd-gc-entry-469)
  (vector-set! ucd-gc-entries 470 ucd-gc-entry-470)
  (vector-set! ucd-gc-entries 471 ucd-gc-entry-471)
  (vector-set! ucd-gc-entries 472 ucd-gc-entry-472)
  (vector-set! ucd-gc-entries 473 ucd-gc-entry-473)
  (vector-set! ucd-gc-entries 474 ucd-gc-entry-474)
  (vector-set! ucd-gc-entries 475 ucd-gc-entry-475)
  (vector-set! ucd-gc-entries 476 ucd-gc-entry-476)
  (vector-set! ucd-gc-entries 477 ucd-gc-entry-477)
  (vector-set! ucd-gc-entries 478 ucd-gc-entry-478)
  (vector-set! ucd-gc-entries 479 ucd-gc-entry-479)
  (vector-set! ucd-gc-entries 480 ucd-gc-entry-480)
  (vector-set! ucd-gc-entries 481 ucd-gc-entry-481)
  (vector-set! ucd-gc-entries 482 ucd-gc-entry-482)
  (vector-set! ucd-gc-entries 483 ucd-gc-entry-483)
  (vector-set! ucd-gc-entries 484 ucd-gc-entry-484)
  (vector-set! ucd-gc-entries 485 ucd-gc-entry-485)
  (vector-set! ucd-gc-entries 486 ucd-gc-entry-486)
  (vector-set! ucd-gc-entries 487 ucd-gc-entry-487)
  (vector-set! ucd-gc-entries 488 ucd-gc-entry-488)
  (vector-set! ucd-gc-entries 489 ucd-gc-entry-489)
  (vector-set! ucd-gc-entries 490 ucd-gc-entry-490)
  (vector-set! ucd-gc-entries 491 ucd-gc-entry-491)
  (vector-set! ucd-gc-entries 492 ucd-gc-entry-492)
  (vector-set! ucd-gc-entries 493 ucd-gc-entry-493)
  (vector-set! ucd-gc-entries 494 ucd-gc-entry-494)
  (vector-set! ucd-gc-entries 495 ucd-gc-entry-495)
  (vector-set! ucd-gc-entries 496 ucd-gc-entry-496)
  (vector-set! ucd-gc-entries 497 ucd-gc-entry-497)
  (vector-set! ucd-gc-entries 498 ucd-gc-entry-498)
  (vector-set! ucd-gc-entries 499 ucd-gc-entry-499))

(define (initialize-ucd-gc-entries-5)
  (vector-set! ucd-gc-entries 500 ucd-gc-entry-500)
  (vector-set! ucd-gc-entries 501 ucd-gc-entry-501)
  (vector-set! ucd-gc-entries 502 ucd-gc-entry-502)
  (vector-set! ucd-gc-entries 503 ucd-gc-entry-503)
  (vector-set! ucd-gc-entries 504 ucd-gc-entry-504)
  (vector-set! ucd-gc-entries 505 ucd-gc-entry-505)
  (vector-set! ucd-gc-entries 506 ucd-gc-entry-506)
  (vector-set! ucd-gc-entries 507 ucd-gc-entry-507)
  (vector-set! ucd-gc-entries 508 ucd-gc-entry-508)
  (vector-set! ucd-gc-entries 509 ucd-gc-entry-509)
  (vector-set! ucd-gc-entries 510 ucd-gc-entry-510)
  (vector-set! ucd-gc-entries 511 ucd-gc-entry-511)
  (vector-set! ucd-gc-entries 512 ucd-gc-entry-512)
  (vector-set! ucd-gc-entries 513 ucd-gc-entry-513)
  (vector-set! ucd-gc-entries 514 ucd-gc-entry-514)
  (vector-set! ucd-gc-entries 515 ucd-gc-entry-515)
  (vector-set! ucd-gc-entries 516 ucd-gc-entry-516)
  (vector-set! ucd-gc-entries 517 ucd-gc-entry-517)
  (vector-set! ucd-gc-entries 518 ucd-gc-entry-518)
  (vector-set! ucd-gc-entries 519 ucd-gc-entry-519)
  (vector-set! ucd-gc-entries 520 ucd-gc-entry-520)
  (vector-set! ucd-gc-entries 521 ucd-gc-entry-521)
  (vector-set! ucd-gc-entries 522 ucd-gc-entry-522)
  (vector-set! ucd-gc-entries 523 ucd-gc-entry-523)
  (vector-set! ucd-gc-entries 524 ucd-gc-entry-524)
  (vector-set! ucd-gc-entries 525 ucd-gc-entry-525)
  (vector-set! ucd-gc-entries 526 ucd-gc-entry-526)
  (vector-set! ucd-gc-entries 527 ucd-gc-entry-527)
  (vector-set! ucd-gc-entries 528 ucd-gc-entry-528)
  (vector-set! ucd-gc-entries 529 ucd-gc-entry-529)
  (vector-set! ucd-gc-entries 530 ucd-gc-entry-530)
  (vector-set! ucd-gc-entries 531 ucd-gc-entry-531)
  (vector-set! ucd-gc-entries 532 ucd-gc-entry-532)
  (vector-set! ucd-gc-entries 533 ucd-gc-entry-533)
  (vector-set! ucd-gc-entries 534 ucd-gc-entry-534)
  (vector-set! ucd-gc-entries 535 ucd-gc-entry-535)
  (vector-set! ucd-gc-entries 536 ucd-gc-entry-536)
  (vector-set! ucd-gc-entries 537 ucd-gc-entry-537)
  (vector-set! ucd-gc-entries 538 ucd-gc-entry-538)
  (vector-set! ucd-gc-entries 539 ucd-gc-entry-539)
  (vector-set! ucd-gc-entries 540 ucd-gc-entry-540)
  (vector-set! ucd-gc-entries 541 ucd-gc-entry-541)
  (vector-set! ucd-gc-entries 542 ucd-gc-entry-542)
  (vector-set! ucd-gc-entries 543 ucd-gc-entry-543)
  (vector-set! ucd-gc-entries 544 ucd-gc-entry-544)
  (vector-set! ucd-gc-entries 545 ucd-gc-entry-545)
  (vector-set! ucd-gc-entries 546 ucd-gc-entry-546)
  (vector-set! ucd-gc-entries 547 ucd-gc-entry-547)
  (vector-set! ucd-gc-entries 548 ucd-gc-entry-548)
  (vector-set! ucd-gc-entries 549 ucd-gc-entry-549)
  (vector-set! ucd-gc-entries 550 ucd-gc-entry-550)
  (vector-set! ucd-gc-entries 551 ucd-gc-entry-551)
  (vector-set! ucd-gc-entries 552 ucd-gc-entry-552)
  (vector-set! ucd-gc-entries 553 ucd-gc-entry-553)
  (vector-set! ucd-gc-entries 554 ucd-gc-entry-554)
  (vector-set! ucd-gc-entries 555 ucd-gc-entry-555)
  (vector-set! ucd-gc-entries 556 ucd-gc-entry-556)
  (vector-set! ucd-gc-entries 557 ucd-gc-entry-557)
  (vector-set! ucd-gc-entries 558 ucd-gc-entry-558)
  (vector-set! ucd-gc-entries 559 ucd-gc-entry-559)
  (vector-set! ucd-gc-entries 560 ucd-gc-entry-560)
  (vector-set! ucd-gc-entries 561 ucd-gc-entry-561)
  (vector-set! ucd-gc-entries 562 ucd-gc-entry-562)
  (vector-set! ucd-gc-entries 563 ucd-gc-entry-563)
  (vector-set! ucd-gc-entries 564 ucd-gc-entry-564)
  (vector-set! ucd-gc-entries 565 ucd-gc-entry-565)
  (vector-set! ucd-gc-entries 566 ucd-gc-entry-566)
  (vector-set! ucd-gc-entries 567 ucd-gc-entry-567)
  (vector-set! ucd-gc-entries 568 ucd-gc-entry-568)
  (vector-set! ucd-gc-entries 569 ucd-gc-entry-569)
  (vector-set! ucd-gc-entries 570 ucd-gc-entry-570)
  (vector-set! ucd-gc-entries 571 ucd-gc-entry-571)
  (vector-set! ucd-gc-entries 572 ucd-gc-entry-572)
  (vector-set! ucd-gc-entries 573 ucd-gc-entry-573)
  (vector-set! ucd-gc-entries 574 ucd-gc-entry-574)
  (vector-set! ucd-gc-entries 575 ucd-gc-entry-575)
  (vector-set! ucd-gc-entries 576 ucd-gc-entry-576)
  (vector-set! ucd-gc-entries 577 ucd-gc-entry-577)
  (vector-set! ucd-gc-entries 578 ucd-gc-entry-578)
  (vector-set! ucd-gc-entries 579 ucd-gc-entry-579)
  (vector-set! ucd-gc-entries 580 ucd-gc-entry-580)
  (vector-set! ucd-gc-entries 581 ucd-gc-entry-581)
  (vector-set! ucd-gc-entries 582 ucd-gc-entry-582)
  (vector-set! ucd-gc-entries 583 ucd-gc-entry-583)
  (vector-set! ucd-gc-entries 584 ucd-gc-entry-584)
  (vector-set! ucd-gc-entries 585 ucd-gc-entry-585)
  (vector-set! ucd-gc-entries 586 ucd-gc-entry-586)
  (vector-set! ucd-gc-entries 587 ucd-gc-entry-587)
  (vector-set! ucd-gc-entries 588 ucd-gc-entry-588)
  (vector-set! ucd-gc-entries 589 ucd-gc-entry-589)
  (vector-set! ucd-gc-entries 590 ucd-gc-entry-590)
  (vector-set! ucd-gc-entries 591 ucd-gc-entry-591)
  (vector-set! ucd-gc-entries 592 ucd-gc-entry-592)
  (vector-set! ucd-gc-entries 593 ucd-gc-entry-593)
  (vector-set! ucd-gc-entries 594 ucd-gc-entry-594)
  (vector-set! ucd-gc-entries 595 ucd-gc-entry-595)
  (vector-set! ucd-gc-entries 596 ucd-gc-entry-596)
  (vector-set! ucd-gc-entries 597 ucd-gc-entry-597)
  (vector-set! ucd-gc-entries 598 ucd-gc-entry-598)
  (vector-set! ucd-gc-entries 599 ucd-gc-entry-599))

(define (initialize-ucd-gc-entries-6)
  (vector-set! ucd-gc-entries 600 ucd-gc-entry-600)
  (vector-set! ucd-gc-entries 601 ucd-gc-entry-601)
  (vector-set! ucd-gc-entries 602 ucd-gc-entry-602)
  (vector-set! ucd-gc-entries 603 ucd-gc-entry-603)
  (vector-set! ucd-gc-entries 604 ucd-gc-entry-604)
  (vector-set! ucd-gc-entries 605 ucd-gc-entry-605)
  (vector-set! ucd-gc-entries 606 ucd-gc-entry-606)
  (vector-set! ucd-gc-entries 607 ucd-gc-entry-607)
  (vector-set! ucd-gc-entries 608 ucd-gc-entry-608)
  (vector-set! ucd-gc-entries 609 ucd-gc-entry-609)
  (vector-set! ucd-gc-entries 610 ucd-gc-entry-610)
  (vector-set! ucd-gc-entries 611 ucd-gc-entry-611)
  (vector-set! ucd-gc-entries 612 ucd-gc-entry-612)
  (vector-set! ucd-gc-entries 613 ucd-gc-entry-613)
  (vector-set! ucd-gc-entries 614 ucd-gc-entry-614)
  (vector-set! ucd-gc-entries 615 ucd-gc-entry-615)
  (vector-set! ucd-gc-entries 616 ucd-gc-entry-616)
  (vector-set! ucd-gc-entries 617 ucd-gc-entry-617)
  (vector-set! ucd-gc-entries 618 ucd-gc-entry-618)
  (vector-set! ucd-gc-entries 619 ucd-gc-entry-619)
  (vector-set! ucd-gc-entries 620 ucd-gc-entry-620)
  (vector-set! ucd-gc-entries 621 ucd-gc-entry-621)
  (vector-set! ucd-gc-entries 622 ucd-gc-entry-622)
  (vector-set! ucd-gc-entries 623 ucd-gc-entry-623)
  (vector-set! ucd-gc-entries 624 ucd-gc-entry-624)
  (vector-set! ucd-gc-entries 625 ucd-gc-entry-625)
  (vector-set! ucd-gc-entries 626 ucd-gc-entry-626)
  (vector-set! ucd-gc-entries 627 ucd-gc-entry-627)
  (vector-set! ucd-gc-entries 628 ucd-gc-entry-628)
  (vector-set! ucd-gc-entries 629 ucd-gc-entry-629)
  (vector-set! ucd-gc-entries 630 ucd-gc-entry-630)
  (vector-set! ucd-gc-entries 631 ucd-gc-entry-631)
  (vector-set! ucd-gc-entries 632 ucd-gc-entry-632)
  (vector-set! ucd-gc-entries 633 ucd-gc-entry-633)
  (vector-set! ucd-gc-entries 634 ucd-gc-entry-634)
  (vector-set! ucd-gc-entries 635 ucd-gc-entry-635)
  (vector-set! ucd-gc-entries 636 ucd-gc-entry-636)
  (vector-set! ucd-gc-entries 637 ucd-gc-entry-637)
  (vector-set! ucd-gc-entries 638 ucd-gc-entry-638)
  (vector-set! ucd-gc-entries 639 ucd-gc-entry-639)
  (vector-set! ucd-gc-entries 640 ucd-gc-entry-640)
  (vector-set! ucd-gc-entries 641 ucd-gc-entry-641)
  (vector-set! ucd-gc-entries 642 ucd-gc-entry-642)
  (vector-set! ucd-gc-entries 643 ucd-gc-entry-643)
  (vector-set! ucd-gc-entries 644 ucd-gc-entry-644)
  (vector-set! ucd-gc-entries 645 ucd-gc-entry-645)
  (vector-set! ucd-gc-entries 646 ucd-gc-entry-646)
  (vector-set! ucd-gc-entries 647 ucd-gc-entry-647)
  (vector-set! ucd-gc-entries 648 ucd-gc-entry-648)
  (vector-set! ucd-gc-entries 649 ucd-gc-entry-649)
  (vector-set! ucd-gc-entries 650 ucd-gc-entry-650)
  (vector-set! ucd-gc-entries 651 ucd-gc-entry-651)
  (vector-set! ucd-gc-entries 652 ucd-gc-entry-652)
  (vector-set! ucd-gc-entries 653 ucd-gc-entry-653)
  (vector-set! ucd-gc-entries 654 ucd-gc-entry-654)
  (vector-set! ucd-gc-entries 655 ucd-gc-entry-655)
  (vector-set! ucd-gc-entries 656 ucd-gc-entry-656)
  (vector-set! ucd-gc-entries 657 ucd-gc-entry-657)
  (vector-set! ucd-gc-entries 658 ucd-gc-entry-658)
  (vector-set! ucd-gc-entries 659 ucd-gc-entry-659)
  (vector-set! ucd-gc-entries 660 ucd-gc-entry-660)
  (vector-set! ucd-gc-entries 661 ucd-gc-entry-661)
  (vector-set! ucd-gc-entries 662 ucd-gc-entry-662)
  (vector-set! ucd-gc-entries 663 ucd-gc-entry-663)
  (vector-set! ucd-gc-entries 664 ucd-gc-entry-664)
  (vector-set! ucd-gc-entries 665 ucd-gc-entry-665)
  (vector-set! ucd-gc-entries 666 ucd-gc-entry-666)
  (vector-set! ucd-gc-entries 667 ucd-gc-entry-667)
  (vector-set! ucd-gc-entries 668 ucd-gc-entry-668)
  (vector-set! ucd-gc-entries 669 ucd-gc-entry-669)
  (vector-set! ucd-gc-entries 670 ucd-gc-entry-670)
  (vector-set! ucd-gc-entries 671 ucd-gc-entry-671)
  (vector-set! ucd-gc-entries 672 ucd-gc-entry-672)
  (vector-set! ucd-gc-entries 673 ucd-gc-entry-673)
  (vector-set! ucd-gc-entries 674 ucd-gc-entry-674)
  (vector-set! ucd-gc-entries 675 ucd-gc-entry-675)
  (vector-set! ucd-gc-entries 676 ucd-gc-entry-676)
  (vector-set! ucd-gc-entries 677 ucd-gc-entry-677)
  (vector-set! ucd-gc-entries 678 ucd-gc-entry-678)
  (vector-set! ucd-gc-entries 679 ucd-gc-entry-679)
  (vector-set! ucd-gc-entries 680 ucd-gc-entry-680)
  (vector-set! ucd-gc-entries 681 ucd-gc-entry-681)
  (vector-set! ucd-gc-entries 682 ucd-gc-entry-682)
  (vector-set! ucd-gc-entries 683 ucd-gc-entry-683)
  (vector-set! ucd-gc-entries 684 ucd-gc-entry-684)
  (vector-set! ucd-gc-entries 685 ucd-gc-entry-685)
  (vector-set! ucd-gc-entries 686 ucd-gc-entry-686)
  (vector-set! ucd-gc-entries 687 ucd-gc-entry-687)
  (vector-set! ucd-gc-entries 688 ucd-gc-entry-688)
  (vector-set! ucd-gc-entries 689 ucd-gc-entry-689)
  (vector-set! ucd-gc-entries 690 ucd-gc-entry-690)
  (vector-set! ucd-gc-entries 691 ucd-gc-entry-691)
  (vector-set! ucd-gc-entries 692 ucd-gc-entry-692)
  (vector-set! ucd-gc-entries 693 ucd-gc-entry-693)
  (vector-set! ucd-gc-entries 694 ucd-gc-entry-694)
  (vector-set! ucd-gc-entries 695 ucd-gc-entry-695)
  (vector-set! ucd-gc-entries 696 ucd-gc-entry-696)
  (vector-set! ucd-gc-entries 697 ucd-gc-entry-697)
  (vector-set! ucd-gc-entries 698 ucd-gc-entry-698)
  (vector-set! ucd-gc-entries 699 ucd-gc-entry-699))

(define (initialize-ucd-gc-entries-7)
  (vector-set! ucd-gc-entries 700 ucd-gc-entry-700)
  (vector-set! ucd-gc-entries 701 ucd-gc-entry-701)
  (vector-set! ucd-gc-entries 702 ucd-gc-entry-702)
  (vector-set! ucd-gc-entries 703 ucd-gc-entry-703)
  (vector-set! ucd-gc-entries 704 ucd-gc-entry-704)
  (vector-set! ucd-gc-entries 705 ucd-gc-entry-705)
  (vector-set! ucd-gc-entries 706 ucd-gc-entry-706)
  (vector-set! ucd-gc-entries 707 ucd-gc-entry-707)
  (vector-set! ucd-gc-entries 708 ucd-gc-entry-708)
  (vector-set! ucd-gc-entries 709 ucd-gc-entry-709)
  (vector-set! ucd-gc-entries 710 ucd-gc-entry-710)
  (vector-set! ucd-gc-entries 711 ucd-gc-entry-711)
  (vector-set! ucd-gc-entries 712 ucd-gc-entry-712)
  (vector-set! ucd-gc-entries 713 ucd-gc-entry-713)
  (vector-set! ucd-gc-entries 714 ucd-gc-entry-714)
  (vector-set! ucd-gc-entries 715 ucd-gc-entry-715)
  (vector-set! ucd-gc-entries 716 ucd-gc-entry-716)
  (vector-set! ucd-gc-entries 717 ucd-gc-entry-717)
  (vector-set! ucd-gc-entries 718 ucd-gc-entry-718)
  (vector-set! ucd-gc-entries 719 ucd-gc-entry-719)
  (vector-set! ucd-gc-entries 720 ucd-gc-entry-720)
  (vector-set! ucd-gc-entries 721 ucd-gc-entry-721)
  (vector-set! ucd-gc-entries 722 ucd-gc-entry-722)
  (vector-set! ucd-gc-entries 723 ucd-gc-entry-723)
  (vector-set! ucd-gc-entries 724 ucd-gc-entry-724)
  (vector-set! ucd-gc-entries 725 ucd-gc-entry-725)
  (vector-set! ucd-gc-entries 726 ucd-gc-entry-726)
  (vector-set! ucd-gc-entries 727 ucd-gc-entry-727)
  (vector-set! ucd-gc-entries 728 ucd-gc-entry-728)
  (vector-set! ucd-gc-entries 729 ucd-gc-entry-729)
  (vector-set! ucd-gc-entries 730 ucd-gc-entry-730)
  (vector-set! ucd-gc-entries 731 ucd-gc-entry-731)
  (vector-set! ucd-gc-entries 732 ucd-gc-entry-732)
  (vector-set! ucd-gc-entries 733 ucd-gc-entry-733)
  (vector-set! ucd-gc-entries 734 ucd-gc-entry-734)
  (vector-set! ucd-gc-entries 735 ucd-gc-entry-735)
  (vector-set! ucd-gc-entries 736 ucd-gc-entry-736)
  (vector-set! ucd-gc-entries 737 ucd-gc-entry-737)
  (vector-set! ucd-gc-entries 738 ucd-gc-entry-738)
  (vector-set! ucd-gc-entries 739 ucd-gc-entry-739)
  (vector-set! ucd-gc-entries 740 ucd-gc-entry-740)
  (vector-set! ucd-gc-entries 741 ucd-gc-entry-741)
  (vector-set! ucd-gc-entries 742 ucd-gc-entry-742)
  (vector-set! ucd-gc-entries 743 ucd-gc-entry-743)
  (vector-set! ucd-gc-entries 744 ucd-gc-entry-744)
  (vector-set! ucd-gc-entries 745 ucd-gc-entry-745)
  (vector-set! ucd-gc-entries 746 ucd-gc-entry-746)
  (vector-set! ucd-gc-entries 747 ucd-gc-entry-747)
  (vector-set! ucd-gc-entries 748 ucd-gc-entry-748)
  (vector-set! ucd-gc-entries 749 ucd-gc-entry-749)
  (vector-set! ucd-gc-entries 750 ucd-gc-entry-750)
  (vector-set! ucd-gc-entries 751 ucd-gc-entry-751)
  (vector-set! ucd-gc-entries 752 ucd-gc-entry-752)
  (vector-set! ucd-gc-entries 753 ucd-gc-entry-753))
