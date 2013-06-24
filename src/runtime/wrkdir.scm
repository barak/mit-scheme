#| -*-Scheme-*-

$Id: wrkdir.scm,v 14.10 2003/02/14 18:28:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; Working Directory
;;; package: (runtime working-directory)

(declare (usual-integrations))

(define (initialize-package!)
  (reset!)
  (add-event-receiver! event:after-restore reset!))

(define (reset!)
  (let ((pathname
	 (pathname-simplify
	  (pathname-as-directory
	   ((ucode-primitive working-directory-pathname))))))
    (set! *working-directory-pathname* pathname)
    (set! *default-pathname-defaults* pathname))
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
    (let ((pathname (pathname-simplify pathname)))
      (set! *working-directory-pathname* pathname)
      (set! *default-pathname-defaults*
	    (merge-pathnames pathname *default-pathname-defaults*))
      (cmdl/set-default-directory (nearest-cmdl) pathname)
      pathname)))

(define (with-working-directory-pathname name thunk)
  (let ((pathname
	 (pathname-as-directory
	  (merge-pathnames name *working-directory-pathname*))))
    (if (not (file-directory? pathname))
	(error "Not a valid directory:" pathname))
    (let ((pathname (pathname-simplify pathname)))
      (fluid-let ((*working-directory-pathname* pathname)
		  (*default-pathname-defaults*
		   (merge-pathnames pathname *default-pathname-defaults*)))
	(thunk)))))