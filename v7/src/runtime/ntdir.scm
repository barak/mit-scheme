#| -*-Scheme-*-

$Id: ntdir.scm,v 1.1 1997/01/05 23:43:10 cph Exp $

Copyright (c) 1997 Massachusetts Institute of Technology

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

(define *expand-directory-prefixes?* #t)

(define (directory-read pattern #!optional sort? full?)
  (let ((sort? (if (default-object? sort?) #t sort?))
	(full? (if (default-object? full?) #f full?)))
    (let ((entries
	   (if full?
	       (directory-read-full pattern)
	       (directory-read-nosort pattern))))
      (if (not sort?)
	  entries
	  (sort entries
		(if full?
		    (lambda (x y) (pathname<? (car x) (car y)))
		    pathname<?))))))

(define (pathname<? x y)
  (string-ci<? (file-namestring x) (file-namestring y)))

(define (directory-read-nosort pattern)
  (let ((pattern (merge-pathnames pattern)))
    (map (let ((directory-path (directory-pathname pattern)))
	   (lambda (pathname)
	     (merge-pathnames pathname directory-path)))
	 (let ((fnames (generate-directory-pathnames pattern)))
	   (fluid-let ((*expand-directory-prefixes?* #f))
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

(define (directory-read-full pattern)
  (let ((pattern (merge-pathnames pattern)))
    (map (let ((directory-path (directory-pathname pattern)))
	   (lambda (entry)
	     (cons (merge-pathnames (car entry) directory-path)
		   (cdr entry))))
	 (let ((entries (generate-directory-entries pattern)))
	   (fluid-let ((*expand-directory-prefixes?* #f))
	     (map (lambda (entry) (cons (->pathname (car entry)) (cdr entry)))
		  entries))))))

(define (generate-directory-entries pathname)
  (let ((channel (directory-channel-open (->namestring pathname))))
    (let loop ((result '()))
      (let ((entry (directory-channel-read-entry channel)))
	(if entry
	    (loop (cons entry result))
	    (begin
	      (directory-channel-close channel)
	      result))))))

(define (directory-channel-read-entry channel)
  ((ucode-primitive win32-directory-read 1)
   (directory-channel/descriptor channel)))