#| -*-Scheme-*-

$Id: os2dir.scm,v 1.2 1994/11/28 05:46:16 cph Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; Directory Operations -- OS/2
;;; package: (runtime directory)

(declare (usual-integrations))

(define *expand-directory-prefixes?* true)

(define (directory-read pattern #!optional sort?)
  (if (if (default-object? sort?) true sort?)
      (sort (directory-read-nosort pattern) pathname<?)
      (directory-read-nosort pattern)))

(define (directory-read-nosort pattern)
  (let ((pattern
	 (let ((pattern (merge-pathnames pattern)))
	   (if (pathname-name pattern)
	       pattern
	       (make-pathname (pathname-host pattern)
			      (pathname-device pattern)
			      (pathname-directory pattern)
			      'WILD
			      'WILD
			      (pathname-version pattern))))))
    (map (let ((directory-path (directory-pathname pattern)))
	   (lambda (pathname)
	     (merge-pathnames pathname directory-path)))
	 (let ((fnames (generate-directory-pathnames pattern)))
	   (fluid-let ((*expand-directory-prefixes?* false))
	     (map ->pathname fnames))))))

(define (generate-directory-pathnames pathname)
  (let ((channel (directory-channel-open (->namestring pathname))))
    (let loop ((result '()))
      (let ((name (directory-channel-read channel)))
	(if name
	    (loop (cons name result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

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