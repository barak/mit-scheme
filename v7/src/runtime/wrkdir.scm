#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/wrkdir.scm,v 14.3 1991/11/04 20:30:42 cph Exp $

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

;;;; Working Directory
;;; package: (runtime working-directory)

(declare (usual-integrations))

(define (initialize-package!)
  (reset!)
  (add-event-receiver! event:after-restore reset!))

(define (reset!)
  (let ((pathname
	 (simplify-directory
	  (pathname-as-directory
	   ((ucode-primitive working-directory-pathname))))))
    (set! *working-directory-pathname* pathname)
    (set! *default-pathname-defaults* pathname))
  (set! hook/set-working-directory-pathname!
	default/set-working-directory-pathname!)
  unspecific)

(define *working-directory-pathname*)

(define (working-directory-pathname)
  *working-directory-pathname*)

(define (set-working-directory-pathname! name)
  (let ((pathname
	 (pathname-as-directory
	  (merge-pathnames name *working-directory-pathname*))))
    (if (not (file-directory? pathname))
	(error "Not a valid directory:" pathname))
    (let ((pathname (simplify-directory pathname)))
      (if (eq? *default-pathname-defaults* *working-directory-pathname*)
	  (set! *default-pathname-defaults* pathname))
      (set! *working-directory-pathname* pathname)
      ((ucode-primitive set-working-directory-pathname! 1)
       (->namestring pathname))
      (hook/set-working-directory-pathname! pathname)
      pathname)))

(define hook/set-working-directory-pathname!)
(define (default/set-working-directory-pathname! pathname)
  pathname
  false)

(define (with-working-directory-pathname name thunk)
  (let ((old-pathname))
    (dynamic-wind (lambda ()
		    (set! old-pathname (working-directory-pathname))
		    (set-working-directory-pathname! name))
		  thunk
		  (lambda ()
		    (set! name (working-directory-pathname))
		    (set-working-directory-pathname! old-pathname)))))

(define (simplify-directory pathname)
  (or (and (implemented-primitive-procedure? (ucode-primitive file-eq? 2))
	   (let ((directory (pathname-directory pathname)))
	     (and (pair? directory)
		  (let ((directory*
			 (cons (car directory)
			       (reverse!
				(let loop
				    ((elements (reverse (cdr directory))))
				  (if (null? elements)
				      '()
				       (let ((head (car elements))
					     (tail (loop (cdr elements))))
					 (if (and (eq? head 'UP)
						  (not (null? tail))
						  (not (eq? (car tail) 'UP)))
					     (cdr tail)
					     (cons head tail)))))))))
		    (and (not (equal? directory directory*))
			 (let ((pathname*
				(pathname-new-directory pathname directory*)))
			   (and ((ucode-primitive file-eq? 2)
				 (->namestring pathname)
				 (->namestring pathname*))
				pathname*)))))))
      pathname))