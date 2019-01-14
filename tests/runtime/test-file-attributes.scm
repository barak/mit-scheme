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

;;;; Tests of file-attributes parser

(declare (usual-integrations))

(define test-cases
  '((" -*-Scheme-*-
This file is part of MIT/GNU Scheme.
"
     (mode . scheme))
    ("|-*- mode:lisp;
       package:(FOOBAR :USE (GLOBAL BAZ)
                       :SHADOW (CAR CDR CONS));
       base:10
   -*- |"
     (mode . lisp)
     (package foobar :use (global baz) :shadow (car cdr cons))
     (base . 10))
    (" -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*-"
     (mode . java)
     (tab-width . 4)
     (indent-tabs-mode . nil)
     (c-basic-offset . 2))
    (" -*- Mode: C; tab-width: 4; -*-"
     (mode . c)
     (tab-width . 4))
    (" For Emacs: -*- mode:cperl; mode:folding -*-"
     (mode . cperl)
     (mode . folding))
    (" -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-"
     (mode . lisp)
     (package . xlib)
     (syntax . common-lisp)
     (base . 10)
     (lowercase . t))
    (" -*-mode:C;tab-width:3-*-"
     (mode . c)
     (tab-width . 3))
    (" -*-mode:c; c-style:k&r; c-basic-offset:4; -*-"
     (mode . c)
     (c-style . k&r)
     (c-basic-offset . 4))
    ("-*-Mode:LISP;Syntax: Common-Lisp;Package:ib;Base:10-*-"
     (mode . lisp)
     (syntax . common-lisp)
     (package . ib)
     (base . 10))
    ("-*-mode:lisp;parser:read-*-"
     (mode . lisp)
     (parser . read))
    (" -*-Mode:Perl; perl-indent-level:8-*-"
     (mode . perl)
     (perl-indent-level . 8))
    (" -*-mode:JavaScript;coding:latin-1;-*- Time-stamp: \"2006-08-09 16:18:45 ADT\""
     (mode . javascript)
     (coding . latin-1))
    (" -*- Mode: C; indent-tabs-mode:nil; c-basic-offset: 8-*- */"
     (mode . c)
     (indent-tabs-mode . nil)
     (c-basic-offset . 8))
    (" -*- coding:utf-8;mode:python;mode:font-lock -*-"
     (coding . utf-8)
     (mode . python)
     (mode . font-lock))
    (" -*- test-case-name: twisted.test.test_htb -*-"
     (test-case-name . twisted.test.test_htb))
    (" -*- mode: C; c-file-style: \"gnu\" -*-"
     (mode . c)
     (c-file-style . "gnu"))
    ("-*- syntax:COMMON-LISP; Package: (ITERATE :use \"COMMON-LISP\" :colon-mode :external) -*-"
     (syntax . common-lisp)
     (package iterate :use "COMMON-LISP" :colon-mode :external))
    (" -*- package IDE-ini -*-"
     . #f)
    (" -*- Mode: Emacs-Lisp; outline-regexp: \" \\n;;;;+\" -*-"
     (mode . emacs-lisp)
     (outline-regexp . " \n;;;;+"))
    (" -*-*- encoding: utf-8 -*-*-"
     (encoding . utf-8))))

(define-test 'parse-file-attributes-string
  (map
   (lambda (p)
     (let ((string (car p))
           (expected-value (cdr p)))
       (lambda ()
         (with-test-properties
          (lambda ()
            (assert-equal (parse-file-attributes-string string)
                          expected-value))
          'expression `(parse-file-attributes-string ,string)))))
   test-cases))

(define (make-parser-tests string->comment test-cases)
  (map
   (lambda (p)
     (let ((contents (string-append (string->comment (car p)) "#f"))
           (expected-properties (cdr p)))
       (lambda ()
         (with-test-properties
          (lambda ()
	    (let ((port (open-input-string contents)))
	      (let ((value
		     (parameterize ((param:standard-warning-hook
				     (lambda (condition)
				       condition
				       unspecific)))
		       (read port))))
		(assert-false value)
		(assert-equal (port-property port 'reader-file-attributes #f)
			      expected-properties))))
          'expression `(read ,contents)))))
   test-cases))

(define-test 'parse-initial-multiline-comment
  (make-parser-tests (lambda (string) (string-append "#|" string "|#\n"))
		     test-cases))

(define-test 'parse-initial-comment
  (make-parser-tests (lambda (string) (string-append ";;; " string "\n"))
		     (remove (lambda (p)
			       (string-any (lambda (char)
					     (char=? char #\newline))
					   (car p)))
			     test-cases)))
