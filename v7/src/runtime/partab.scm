#| -*-Scheme-*-

$Id: partab.scm,v 14.6 2002/11/20 19:46:22 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Parser Tables
;;; package: (runtime parser-table)

(declare (usual-integrations))

(define-structure (parser-table (constructor %make-parser-table)
				(conc-name parser-table/))
  (parse-object false read-only true)
  (collect-list false read-only true)
  (parse-object-special false read-only true)
  (collect-list-special false read-only true))

(define-integrable (guarantee-parser-table table procedure)
  (if (not (parser-table? table))
      (error:wrong-type-argument table "parser table" procedure))
  table)

(define (make-parser-table parse-object
			   collect-list
			   parse-object-special
			   collect-list-special)
  (%make-parser-table (make-vector 256 parse-object)
		      (make-vector 256 collect-list)
		      (make-vector 256 parse-object-special)
		      (make-vector 256 collect-list-special)))

(define (parser-table/copy table)
  (%make-parser-table (vector-copy (parser-table/parse-object table))
		      (vector-copy (parser-table/collect-list table))
		      (vector-copy (parser-table/parse-object-special table))
		      (vector-copy (parser-table/collect-list-special table))))

(define-integrable (current-parser-table)
  *current-parser-table*)

(define (set-current-parser-table! table)
  (guarantee-parser-table table 'SET-CURRENT-PARSER-TABLE!)
  (set! *current-parser-table* table))

(define (with-current-parser-table table thunk)
  (guarantee-parser-table table 'WITH-CURRENT-PARSER-TABLE)
  (fluid-let ((*current-parser-table* table))
    (thunk)))

(define *current-parser-table*)

(define (parser-table/entry table char receiver)
  (decode-parser-char table char
    (lambda (index parse-object-table collect-list-table)
      (receiver (vector-ref parse-object-table index)
		(vector-ref collect-list-table index)))))

(define (parser-table/set-entry! table char
				 parse-object #!optional collect-list)
  (let ((kernel
	 (let ((collect-list
		(if (default-object? collect-list)
		    (collect-list-wrapper parse-object)
		    collect-list)))
	   (lambda (char)
	     (decode-parser-char table char
	       (lambda (index parse-object-table collect-list-table)
		 (vector-set! parse-object-table index parse-object)
		 (vector-set! collect-list-table index collect-list)))))))
    (cond ((char-set? char) (for-each kernel (char-set-members char)))
	  ((pair? char) (for-each kernel char))
	  (else (kernel char)))))

(define (decode-parser-char table char receiver)
  (cond ((char? char)
	 (receiver (char->ascii char)
		   (parser-table/parse-object table)
		   (parser-table/collect-list table)))
	((string? char)
	 (cond ((= (string-length char) 1)
		(receiver (char->ascii (string-ref char 0))
			  (parser-table/parse-object table)
			  (parser-table/collect-list table)))
	       ((and (= (string-length char) 2)
		     (char=? #\# (string-ref char 0)))
		(receiver (char->ascii (string-ref char 1))
			  (parser-table/parse-object-special table)
			  (parser-table/collect-list-special table)))
	       (else
		(error "Bad character" char))))
	(else
	 (error "Bad character" char))))