#| -*-Scheme-*-

$Id: ntdir.scm,v 1.4 2003/02/14 18:28:33 cph Exp $

Copyright (c) 1997, 1999 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

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