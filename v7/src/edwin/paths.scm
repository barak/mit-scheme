#| -*-Scheme-*-

$Id: paths.scm,v 1.14 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Edwin Pathnames

(declare (usual-integrations))

(define edwin-library-directory-pathname
  (let ((directory (pathname-as-directory "edwin")))
    (lambda (envvar name)
      (cond ((get-environment-variable envvar)
	     => (lambda (name)
		  (pathname-as-directory (merge-pathnames name))))
	    ((system-library-directory-pathname
	      (merge-pathnames name directory)))
	    (else
	     (error "Can't find edwin library directory:" name))))))

(define (edwin-etc-pathname filename)
  (let ((pathname (merge-pathnames filename (edwin-etc-directory))))
    (if (not (file-exists? pathname))
	(error "Unable to find file:" (->namestring pathname)))
    pathname))

(define (edwin-binary-directory)
  (edwin-library-directory-pathname
   "EDWIN_BINARY_DIRECTORY"
   "autoload"))

(define (edwin-info-directory)
  (edwin-library-directory-pathname
   "EDWIN_INFO_DIRECTORY"
   "info"))

(define (edwin-etc-directory)
  (edwin-library-directory-pathname
   "EDWIN_ETC_DIRECTORY"
   "etc"))

(define (edwin-tutorial-pathname)
  (edwin-etc-pathname "TUTORIAL"))

(define default-homedir-pathname
  ;; This binding exists to allow uses of the "home" directory as a
  ;; default directory to be overridden.
  user-homedir-pathname)