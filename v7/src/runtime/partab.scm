#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/partab.scm,v 14.3 1988/07/13 18:41:33 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Parser Tables
;;; package: (runtime parser-table)

(declare (usual-integrations))

(define-structure (parser-table (constructor %make-parser-table)
				(conc-name parser-table/))
  (parse-object false read-only true)
  (collect-list false read-only true)
  (parse-object-special false read-only true)
  (collect-list-special false read-only true))

(define (guarantee-parser-table table)
  (if (not (parser-table? table))
      (error "Not a valid parser table" table))
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
  (guarantee-parser-table table)
  (set! *current-parser-table* table))

(define (with-current-parser-table table thunk)
  (guarantee-parser-table table)
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