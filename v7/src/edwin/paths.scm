#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/paths.scm,v 1.10 1991/07/16 21:00:02 cph Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

;;;; Edwin Pathnames

(declare (usual-integrations))

(define edwin-library-directory-pathname
  (let ((directory (pathname-as-directory (string->pathname "edwin"))))
    (lambda (name)
      (let ((pathname
	     (system-library-directory-pathname
	      (merge-pathnames (->pathname name) directory))))
	(if (not pathname)
	    (error "Can't find edwin library directory:" name))
	pathname))))

(define (edwin-etc-pathname filename)
  (let ((pathname
	 (merge-pathnames (->pathname filename) (edwin-etc-directory))))
    (if (not (file-exists? pathname))
	(error "Unable to find file:" (pathname->string pathname)))
    pathname))

(define (edwin-binary-directory)
  (edwin-library-directory-pathname "autoload"))

(define (edwin-info-directory)
  (edwin-library-directory-pathname "info"))

(define (edwin-etc-directory)
  (edwin-library-directory-pathname "etc"))

(define (edwin-tutorial-pathname)
  (edwin-etc-pathname "TUTORIAL"))