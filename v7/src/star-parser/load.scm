;;; -*-Scheme-*-
;;;
;;; $Id: load.scm,v 1.11 2001/11/09 21:37:51 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

(load-option 'HASH-TABLE)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (package/system-loader "parser" '() 'QUERY)))
(add-subsystem-identification! "*Parser" '(0 10))