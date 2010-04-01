#| -*-Scheme-*-

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

;;;; Keywords
;;; package: (runtime keyword)

(declare (usual-integrations))


;; Keywords are really interned symbols with a funny name.  We do it
;; this way because we need to keep eq-ness when fasdumping and
;; fasload them.  The self-evaluating property of keywords is handled
;; by in the syntaxer which simply doesn't recognize them as
;; identifiers.

(define-integrable keyword-prefix "#[keyword]")

(define (string->keyword string)
  (guarantee-string string 'string->keyword)
  (string->symbol (string-append keyword-prefix string)))

(define (keyword? object)
  (and (interned-symbol? object)
       (string-prefix? keyword-prefix (symbol->string object))))

(define-guarantee keyword "keyword")

(define (keyword->string keyword)
  (guarantee-keyword keyword 'keyword->string)
  (string-tail (symbol->string keyword) (string-length keyword-prefix)))