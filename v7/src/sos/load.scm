;;; -*-Scheme-*-
;;;
;;; $Id: load.scm,v 1.6 1999/01/03 05:24:12 cph Exp $
;;;
;;; Copyright (c) 1995-1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(load-option 'HASH-TABLE)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (package/system-loader "sos" '() 'QUERY)))
(let ((install
       (let ((environment (package/environment (find-package '(SOS MACROS)))))
	 (lambda (mname tname)
	   (syntax-table/define system-global-syntax-table
				mname
				(lexical-reference environment tname))))))
  (install 'DEFINE-CLASS 'TRANSFORM:DEFINE-CLASS)
  (install 'DEFINE-GENERIC 'TRANSFORM:DEFINE-GENERIC)
  (install 'DEFINE-METHOD 'TRANSFORM:DEFINE-METHOD)
  (install 'DEFINE-COMPUTED-METHOD 'TRANSFORM:DEFINE-COMPUTED-METHOD)
  (install 'DEFINE-COMPUTED-EMP 'TRANSFORM:DEFINE-COMPUTED-EMP)
  ;;(install 'METHOD 'TRANSFORM:METHOD)
  )
(add-identification! "SOS" 1 6)