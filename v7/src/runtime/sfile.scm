;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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

;;;; Simple File Operations

(declare (usual-integrations))

(define copy-file
  (let ((p-copy-file (make-primitive-procedure 'COPY-FILE)))
    (named-lambda (copy-file from to)
      (p-copy-file (canonicalize-input-filename from)
		   (canonicalize-output-filename to)))))

(define rename-file
  (let ((p-rename-file (make-primitive-procedure 'RENAME-FILE)))
    (named-lambda (rename-file from to)
      (p-rename-file (canonicalize-input-filename from)
		     (canonicalize-output-filename to)))))

(define delete-file
  (let ((p-delete-file (make-primitive-procedure 'REMOVE-FILE)))
    (named-lambda (delete-file name)
      (p-delete-file (canonicalize-input-filename name)))))

(define file-exists?
  (let ((p-file-exists? (make-primitive-procedure 'FILE-EXISTS?)))
    (named-lambda (file-exists? name)
      (let ((pathname (->pathname name)))
	(if (eq? 'NEWEST (pathname-version pathname))
	    (pathname-newest pathname)
	    (p-file-exists?
