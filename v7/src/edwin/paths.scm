#| -*-Scheme-*-

$Id: paths.scm,v 1.17 2002/11/20 19:46:02 cph Exp $

Copyright (c) 1989-2001 Massachusetts Institute of Technology

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

;;;; Edwin Pathnames

(declare (usual-integrations))

(define (edwin-library-directory-pathname envvar name required?)
  (let ((envval (get-environment-variable envvar)))
    (if envval
	(pathname-as-directory (merge-pathnames envval))
	(or (system-library-directory-pathname
	     (merge-pathnames name (pathname-as-directory "edwin")))
	    (and required?
		 (error "Can't find edwin library directory:" name))))))

(define (edwin-binary-directory)
  (edwin-library-directory-pathname "EDWIN_BINARY_DIRECTORY" "autoload" #t))

(define (edwin-info-directory)
  (edwin-library-directory-pathname "EDWIN_INFO_DIRECTORY" "info" #f))

(define (edwin-etc-directory)
  (edwin-library-directory-pathname "EDWIN_ETC_DIRECTORY" "etc" #t))

(define (edwin-etc-pathname filename)
  (let ((pathname (merge-pathnames filename (edwin-etc-directory))))
    (if (not (file-exists? pathname))
	(error "Unable to find file:" (->namestring pathname)))
    pathname))

(define (edwin-tutorial-pathname)
  (edwin-etc-pathname "TUTORIAL"))

(define default-homedir-pathname
  ;; This binding exists to allow uses of the "home" directory as a
  ;; default directory to be overridden.
  user-homedir-pathname)