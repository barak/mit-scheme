;;; -*-Scheme-*-
;;;
;;; $Id: load.scm,v 1.1 1997/06/04 06:08:40 cph Exp $
;;;
;;; Copyright (c) 1995-97 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

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