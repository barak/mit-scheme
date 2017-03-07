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
   '((" -*-Scheme-*-
This file is part of MIT/GNU Scheme.
"
      (mode . scheme))
     ("|-*- mode:lisp;
       package:(FOOBAR :USE (GLOBAL BAZ)
                       :SHADOW (CAR CDR CONS));
       base:10
   -*- |"
      (mode . lisp) (package foobar :use (global baz) :shadow (car cdr cons)) (base . 10))
     (" -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*-"
      (mode . java) (tab-width . 4) (indent-tabs-mode . nil) (c-basic-offset . 2))
     (" -*- Mode: C; tab-width: 4; -*-"
      (mode . c) (tab-width . 4))
     (" For Emacs: -*- mode:cperl; mode:folding -*-"
      (mode . cperl) (mode . folding))
     (" -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-"
      (mode . lisp) (package . xlib) (syntax . common-lisp) (base . 10) (lowercase . t))
     (" -*-mode:C;tab-width:3-*-"
      (mode . c) (tab-width . 3))
     (" -*-mode:c; c-style:k&r; c-basic-offset:4; -*-"
      (mode . c) (c-style . k&r) (c-basic-offset . 4))
     ("-*-Mode:LISP;Syntax: Common-Lisp;Package:ib;Base:10-*-"
      (mode . lisp) (syntax . common-lisp) (package . ib) (base . 10))
     ("-*-mode:lisp;parser:read-*-"
      (mode . lisp) (parser . read))
     (" -*-Mode:Perl; perl-indent-level:8-*-"
      (mode . perl) (perl-indent-level . 8))
     (" -*-mode:JavaScript;coding:latin-1;-*- Time-stamp: \"2006-08-09 16:18:45 ADT\""
      (mode . javascript) (coding . latin-1))
     (" -*- Mode: C; indent-tabs-mode:nil; c-basic-offset: 8-*- */"
      (mode . c) (indent-tabs-mode . nil) (c-basic-offset . 8))
     (" -*- coding:utf-8;mode:python;mode:font-lock -*-"
      (coding . utf-8) (mode . python) (mode . font-lock))
     (" -*- test-case-name: twisted.test.test_htb -*-"
      (test-case-name . twisted.test.test_htb))
     (" -*- mode: C; c-file-style: \"gnu\" -*-"
      (mode . c) (c-file-style . "gnu"))
     ("-*- syntax:COMMON-LISP; Package: (ITERATE :use \"COMMON-LISP\" :colon-mode :external) -*-"
      (syntax . common-lisp) (package iterate :use "COMMON-LISP" :colon-mode :external))
     (" -*- package IDE-ini -*-"
      . #f)
     (" -*- Mode: Emacs-Lisp; outline-regexp: \" \\n;;;;+\" -*-"
      (mode . emacs-lisp) (outline-regexp . " \n;;;;+"))
     (" -*-*- encoding: utf-8 -*-*-"
      (encoding . utf-8)))))
