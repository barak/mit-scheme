;;; -*-Scheme-*-
;;;
;;; $Id: load.scm,v 1.15 2000/06/08 16:43:10 cph Exp $
;;;
;;; Copyright (c) 1999-2000 Massachusetts Institute of Technology
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

;;;; IMAIL mail reader: loader

(load-option 'HASH-TABLE)
(load-option 'MIME-CODECS)
(load-option 'REGULAR-EXPRESSION)
(load-option 'SOS)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (fluid-let ((*allow-package-redefinition?* #t))
      (package/system-loader "imail" '() 'QUERY))))
(add-subsystem-identification! "IMAIL" '(1 1))