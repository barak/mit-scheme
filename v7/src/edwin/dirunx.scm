;;; -*-Scheme-*-
;;;
;;;	$Id: dirunx.scm,v 1.1 1992/09/23 23:05:02 jinx Exp $
;;;
;;;	Copyright (c) 1992 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

;;;; Directory Editor
;; package: (edwin dired)

(declare (usual-integrations))

(define-command dired-chmod
  "Change mode of this file."
  "sChange to Mode"
  (lambda (mode) (dired-change-line "chmod" mode)))

(define-command dired-chgrp
  "Change group of this file."
  "sChange to Group"
  (lambda (group) (dired-change-line "chgrp" group)))

(define-command dired-chown
  "Change owner of this file."
  "sChange to Owner"
  (lambda (owner) (dired-change-line "chown" owner)))

(define-command dired-compress
  "Compress a file."
  '()
  (lambda ()
    (let ((pathname (dired-current-pathname)))
      (let ((directory (directory-pathname pathname)))
	(run-synchronous-process false false directory false
				 (find-program "compress" directory)
				 ""
				 (->namestring pathname)))
      (dired-redisplay
       (pathname-new-type 
	pathname
	(let ((old-type (pathname-type pathname)))
	  (cond ((not old-type)
		 "Z")
		((string=? old-type "Z")
		 old-type)
		(else
		 (string-append old-type ".Z")))))))))

(define-command dired-uncompress
  "Uncompress a file."
  '()
  (lambda ()
    (let ((pathname (dired-current-pathname)))
      (let ((directory (directory-pathname pathname)))
	(run-synchronous-process false false directory false
				 (find-program "uncompress" directory)
				 ""
				 (->namestring pathname)))
      (dired-redisplay
       (if (and (pathname-type pathname)
		(string=? (pathname-type pathname) "Z"))
	   (pathname-new-type pathname false)
	   pathname)))))

(define (dired-change-line program argument)
  (let ((pathname (dired-current-pathname)))
    (let ((directory (directory-pathname pathname)))
      (run-synchronous-process false false directory false
			       (find-program program directory)
			       argument
			       (->namestring pathname)))
    (dired-redisplay pathname)))