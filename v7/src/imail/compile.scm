;;; -*-Scheme-*-
;;;
;;; $Id: compile.scm,v 1.8 2000/06/01 18:23:55 cph Exp $
;;;
;;; Copyright (c) 2000 Massachusetts Institute of Technology
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

;;;; IMAIL mail reader: compilation

(load-option 'CREF)
(load-option 'SOS)
(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (for-each compile-file
	      '("imail-core"
		"imail-file"
		"imail-imap"
		"imail-rmail"
		"imail-umail"
		"imail-util"
		"imap-response"
		"imap-syntax"
		"mime-codec"
		"parser"
		"rexp"
		"rfc822"
		"url"))
    (for-each (let ((syntax-table
		     (access edwin-syntax-table (->environment '(EDWIN)))))
		(lambda (filename)
		  (compile-file filename '() syntax-table)))
	      '("imail-summary"
		"imail-top"))
    (cref/generate-constructors "imail")
    (sf "imail.con")
    (sf "imail.ldr")))