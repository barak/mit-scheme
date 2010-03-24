#| -*- Mode: Scheme -*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; File attributes parser
;;; package: (runtime parser file-attributes)

(declare (usual-integrations))

;;; This file will parse "file attributes line" found in the first
;;; or second line of file and delimited by the special -*- sequence.
;;;
;;; It currently contains just a stub function that the parser will
;;; call when the delimiter is recognized within a comment.

(define (parse-file-attributes-line port db multiline)
  (declare (ignore port db multiline))
  unspecific)

(define (initialize-package!)
  unspecific)

;;; Here are sample attribute lines taken from various files
;;; found in the wild.  They won't be parsed because they are not
;;; in the first two lines.

#||-*- mode:lisp;
       package:(FOOBAR :USE (GLOBAL BAZ)
                       :SHADOW (CAR CDR CONS));
       base:10
   -*- ||#

;;; -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

;;; -*- Mode: C; tab-width: 4; -*- */

;;; -*-mode:C;tab-width:3-*-

;;; For Emacs: -*- mode:cperl; mode:folding -*-

;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; -*-mode:C;tab-width:3-*-

;;; -*-mode:c; c-style:k&r; c-basic-offset:4; -*- */

;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:ib;Base:10-*-

;;;-*-mode:lisp;parser:read-*-

;;; -*-Mode:Perl; perl-indent-level:8-*-

;;; -*-mode:JavaScript;coding:latin-1;-*- Time-stamp: "2006-08-09 16:18:45 ADT"

;;; -*- Mode: C; indent-tabs-mode:nil; c-basic-offset: 8-*- */

;;; -*- coding:utf-8;mode:python;mode:font-lock -*-

;;; -*- test-case-name: twisted.test.test_htb -*-

;;; -*- mode: C; c-file-style: "gnu" -*-

;;;-*- syntax:COMMON-LISP; Package: (ITERATE :use "COMMON-LISP" :colon-mode :external) -*-

;;; -*- package IDE-ini -*-

;;; -*- Mode: Emacs-Lisp; outline-regexp: " \n;;;;+" -*-

;;; It should surprise no one that the following comes from a python file.
;;; -*-*- encoding: utf-8 -*-*-
