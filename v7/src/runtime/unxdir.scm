#| -*-Scheme-*-

$Id: unxdir.scm,v 14.13 2002/11/20 19:46:24 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Directory Operations -- unix
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
	   (if (directory-pathname? pattern)
	       (make-pathname (pathname-host pattern)
			      (pathname-device pattern)
			      (pathname-directory pattern)
			      'WILD
			      'WILD
			      (pathname-version pattern))
	       pattern))))
    (let ((directory-path (directory-pathname pattern)))
      (map (lambda (pathname)
	     (merge-pathnames pathname directory-path))
	   (let ((pathnames
		  (let ((fnames (generate-directory-pathnames directory-path)))
		    (fluid-let ((*expand-directory-prefixes?* false))
		      (map ->pathname fnames)))))
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