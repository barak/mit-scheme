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

;;;; Features for cond-expand
;;; package: (runtime feature)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables))

(define (features)
  (append constant-features
	  computed-features
	  (compiler-features)))

(define (compiler-features)
  (let ((cf (global-value 'compiler-features)))
    (if cf
	(cf)
	'(target-arch=none))))

(define (global-value name)
  (and (eq? 'normal (environment-reference-type system-global-environment name))
       (environment-lookup system-global-environment name)))

(define constant-features
  '(mit
    mit/gnu

    ;; r7rs features
    r7rs
    exact-closed
    exact-complex
    ieee-float
    full-unicode
    ratio

    swank	;Provides SWANK module for SLIME
    srfi-0	;COND-EXPAND
    srfi-1	;List Library
    srfi-2	;AND-LET*
    srfi-6	;Basic String Ports
    srfi-8	;RECEIVE
    srfi-9	;DEFINE-RECORD-TYPE
    srfi-14	;Character-set Library
    srfi-23	;ERROR
    srfi-27	;Sources of Random Bits
    srfi-30	;Nested Multi-Line Comments (#| ... |#)
    srfi-39	;Parameter objects
    srfi-62	;S-expression comments
    srfi-69	;Basic Hash Tables
    srfi-115	;Scheme Regular Expressions
    srfi-124	;Ephemerons
    srfi-125	;Intermediate hash tables
    srfi-128	;Comparators (reduced)
    srfi-131	;ERR5RS Record Syntax (reduced)
    srfi-133	;Vector Library (R7RS-compatible)
    srfi-143	;Fixnums

    ;; SRFI 115 features
    regexp-unicode
    regexp-non-greedy))

(define computed-features)
(define (reset-computed-features!)
  (set! computed-features
	`(,(symbol 'mit/gnu- (get-subsystem-version-string "Release"))
	  ,@(case microcode-id/operating-system
	      ((nt) '(windows))
	      ((unix) '(unix posix))
	      (else '()))
	  ,@(let ((s microcode-id/operating-system-variant))
	      (cond ((string=? "OS X" s) '(darwin))
		    ((string=? "GNU/Linux" s) '(gnu-linux))
		    (else '())))
	  ,(string->symbol microcode-id/machine-type)
	  ,(symbol 'host-arch= microcode-id/compiled-code-type)
	  ,@(if (host-big-endian?)
		'(big-endian host-big-endian)
		'(little-endian host-little-endian))
	  ,@(case (bytes-per-object)
	      ((4) '(host-32-bit))
	      ((8) '(host-64-bit))
	      (else '()))))
  unspecific)

(add-boot-init!
 (lambda ()
   (run-now-and-after-restore! reset-computed-features!)))