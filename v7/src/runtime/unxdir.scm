#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unxdir.scm,v 14.9 1992/02/08 15:08:44 cph Exp $

Copyright (c) 1988-92 Massachusetts Institute of Technology

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

;;;; Directory Operations -- unix
;;; package: (runtime directory)

(declare (usual-integrations))

(define (directory-read pattern #!optional sort?)
  (if (if (default-object? sort?) true sort?)
      (sort (directory-read-nosort pattern) pathname<?)
      (directory-read-nosort pattern)))

(define (directory-read-nosort pattern)
  (let ((pattern
	 (let ((pattern (merge-pathnames pattern)))
	   (let ((name (pathname-name pattern))
		 (type (pathname-type pattern)))
	     (if (or name type)
		 pattern
		 (make-pathname (pathname-host pattern)
				(pathname-device pattern)
				(pathname-directory pattern)
				'WILD
				'WILD
				(pathname-version pattern)))))))
    (let ((directory-path (directory-pathname pattern)))
      (map (lambda (pathname)
	     (merge-pathnames pathname directory-path))
	   (let ((pathnames
		  (map ->pathname
		       (generate-directory-pathnames directory-path))))
	     (if (and (eq? (pathname-name pattern) 'WILD)
		      (eq? (pathname-type pattern) 'WILD))
		 pathnames
		 (list-transform-positive pathnames
		   (lambda (instance)
		     (and (match-component (pathname-name pattern)
					   (pathname-name instance))
			  (match-component (pathname-type pattern)
					   (pathname-type instance)))))))))))

(define (generate-directory-pathnames pathname)
  (let ((channel (directory-channel-open (->namestring pathname))))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons name result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (match-component pattern instance)
  (or (eq? pattern 'WILD)
      (equal? pattern instance)))

(define (pathname<? x y)
  (or (component<? (pathname-name x) (pathname-name y))
      (and (equal? (pathname-name x) (pathname-name y))
	   (component<? (pathname-type x) (pathname-type y)))))

(define (component<? x y)
  (and y
       (or (not x)
	   (and (string? y)
		(or (not (string? x))
		    (string<? x y))))))