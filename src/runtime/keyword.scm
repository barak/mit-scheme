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

(define (initialize-package!)
  (set! *keyword-intern-table* (make-string-hash-table))
  unspecific)

(define *keyword-intern-table*)

(define-structure (keyword
		   (constructor %make-keyword (name))
		   (conc-name keyword/)
		   (print-procedure (lambda (state object)
				      (keyword-unparser state object))))
  ;; logically, the name is a string, but
  ;; we store it as a symbol so that the standard
  ;; symbol-quoting conventions work.
  (name #f read-only #t))

(define-guarantee keyword "Keyword object")

(define (keyword-unparser state object)
  (let ((port (unparser-state/port state)))
    (case *parser-keyword-style*
      ((PREFIX)
       (write-char #\: port)
       (write (keyword/name object) port))
      ((SUFFIX)
       (write (keyword/name object) port)
       (write-char #\: port))
      (else
       (write-string "#[keyword " port)
       (write (keyword/name object) port)
       (write-string "]" port)))))

(define (keyword->string keyword)
  (guarantee-keyword keyword 'keyword->string)
  (symbol->string (keyword/name keyword)))

(define (string->keyword string)
  (guarantee-string string 'string->keyword)
  (or (hash-table/get *keyword-intern-table* string #f)
      (let ((new-keyword (%make-keyword (string->symbol string))))
	(hash-table/put! *keyword-intern-table*
			 (string-copy string)
			 new-keyword)
	new-keyword)))

(define (symbol->keyword symbol)
  (guarantee-symbol symbol 'symbol->keyword)
  (string->keyword (symbol->string symbol)))