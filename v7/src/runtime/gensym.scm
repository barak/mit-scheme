;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gensym.scm,v 13.41 1987/01/23 00:13:48 jinx Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; GENSYM

(declare (usual-integrations))

(define (make-name-generator prefix)
  (let ((counter 0))
    (named-lambda (name-generator)
      (string->uninterned-symbol
       (string-append prefix
		      (write-to-string
		       (let ((n counter))
			 (set! counter (1+ counter))
			 n)))))))

(define generate-uninterned-symbol
  (let ((name-counter 0)
	(name-prefix "G"))
    (define (get-number)
      (let ((result name-counter))
	(set! name-counter (1+ name-counter))
	result))
    (named-lambda (generate-uninterned-symbol #!optional argument)
      (if (not (unassigned? argument))
	  (cond ((symbol? argument)
		 (set! name-prefix (symbol->string argument)))
		((integer? argument)
		 (set! name-counter argument))
		(else
		 (error "Bad argument: GENERATE-UNINTERNED-SYMBOL"
			argument))))
      (string->uninterned-symbol
       (string-append name-prefix (write-to-string (get-number)))))))
