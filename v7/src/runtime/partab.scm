#| -*-Scheme-*-

$Id: partab.scm,v 14.8 2004/01/15 21:00:12 cph Exp $

Copyright 1988,1996,2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Parser Tables
;;; package: (runtime parser-table)

(declare (usual-integrations))

(define-structure (parser-table (constructor %make-parser-table)
				(conc-name parser-table/))
  (initial #f read-only #t)
  (special #f read-only #t))

(define (make-parser-table initial special)
  (if (not (and (vector? initial)
		(fix:= (vector-length initial) #x100)))
      (error:wrong-type-argument initial "dispatch vector" 'MAKE-PARSER-TABLE))
  (if (not (and (vector? special)
		(fix:= (vector-length special) #x100)))
      (error:wrong-type-argument special "dispatch vector" 'MAKE-PARSER-TABLE))
  (%make-parser-table initial special))

(define (guarantee-parser-table table caller)
  (if (not (parser-table? table))
      (error:wrong-type-argument table "parser table" caller))
  table)

(define (parser-table/copy table)
  (%make-parser-table (vector-copy (parser-table/initial table))
		      (vector-copy (parser-table/special table))))

(define (current-parser-table)
  *current-parser-table*)

(define (set-current-parser-table! table)
  (guarantee-parser-table table 'SET-CURRENT-PARSER-TABLE!)
  (set! *current-parser-table* table)
  unspecific)

(define (with-current-parser-table table thunk)
  (guarantee-parser-table table 'WITH-CURRENT-PARSER-TABLE)
  (fluid-let ((*current-parser-table* table))
    (thunk)))

(define *current-parser-table*)

(define (parser-table/entry table key)
  (receive (v n) (decode-key table key 'PARSER-TABLE/ENTRY)
    (vector-ref v n)))

(define (parser-table/set-entry! table key entry)
  (receive (v n) (decode-key table key 'PARSER-TABLE/SET-ENTRY!)
    (vector-set! v n entry)))

(define (decode-key table key caller)
  (cond ((char? key)
	 (values (parser-table/initial table)
		 (char->integer key)))
	((and (string? key)
	      (fix:= (string-length key) 1))
	 (values (parser-table/initial table)
		 (vector-8b-ref key 0)))
	((and (string? key)
	      (fix:= (string-length key) 2)
	      (char=? #\# (string-ref key 0)))
	 (values (parser-table/special table)
		 (vector-8b-ref key 1)))
	(else
	 (error:wrong-type-argument key "parser-table key" caller))))