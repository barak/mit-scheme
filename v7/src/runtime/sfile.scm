#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/sfile.scm,v 14.6 1991/10/26 16:21:04 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Simple File Operations
;;; package: ()

(declare (usual-integrations))

(define (canonicalize-existing-filename filename)
  (pathname->string (canonicalize-existing-pathname filename)))

(define (canonicalize-existing-pathname filename)
  (let ((pathname (->pathname filename)))
    (or (pathname->existing-truename pathname)
	(canonicalize-existing-pathname (error:open-file pathname)))))

(define (pathname->existing-truename pathname)
  (let ((pathname (pathname->absolute-pathname pathname))
	(truename-exists?
	 (lambda (pathname)
	   ;; This primitive, a unix-specific one, is used, because it
	   ;; is the simplest way to do an lstat on the file.  The
	   ;; usual primitive, FILE-EXISTS?, does a stat.
	   (and ((ucode-primitive file-mod-time 1) (pathname->string pathname))
		pathname))))
    (cond ((not (eq? 'NEWEST (pathname-version pathname)))
	   (truename-exists? pathname))
	  ((not pathname-newest)
	   (truename-exists? (pathname-new-version pathname false)))
	  (else
	   (pathname-newest pathname)))))

(define (rename-file from to)
  ((ucode-primitive file-rename) (canonicalize-existing-filename from)
				 (canonicalize-output-filename to)))

(define (delete-file filename)
  (let ((truename (pathname->existing-truename (->pathname filename))))
    (and truename
	 (begin
	   ((ucode-primitive file-remove) (pathname->string truename))
	   true))))