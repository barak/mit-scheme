;;; -*-Scheme-*-
;;;
;;;	$Id: bufset.scm,v 1.11 1993/08/10 23:27:48 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-93 Massachusetts Institute of Technology
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

;;;; Buffer Set Abstraction

(declare (usual-integrations))

(define-structure (bufferset (constructor %make-bufferset))
  buffer-list
  (names #f read-only #t))

(define (make-bufferset initial-buffer)
  (%make-bufferset (list initial-buffer)
		   (let ((names (make-string-table 16 false)))
		     (string-table-put! names
					(buffer-name initial-buffer)
					initial-buffer)
		     names)))

(define (bufferset-select-buffer! bufferset buffer)
  (if (memq buffer (bufferset-buffer-list bufferset))
      (set-bufferset-buffer-list!
       bufferset
       (cons buffer (delq! buffer (bufferset-buffer-list bufferset)))))
  unspecific)

(define (bufferset-bury-buffer! bufferset buffer)
  (if (memq buffer (bufferset-buffer-list bufferset))
      (set-bufferset-buffer-list!
       bufferset
       (append! (delq! buffer (bufferset-buffer-list bufferset))
		(list buffer))))
  unspecific)

(define (bufferset-guarantee-buffer! bufferset buffer)
  (if (not (memq buffer (bufferset-buffer-list bufferset)))
      (begin
	(string-table-put! (bufferset-names bufferset)
			   (buffer-name buffer)
			   buffer)
	(set-bufferset-buffer-list! bufferset
				    (append! (bufferset-buffer-list bufferset)
					     (list buffer)))))
  unspecific)

(define (bufferset-find-buffer bufferset name)
  (string-table-get (bufferset-names bufferset) name))

(define (bufferset-create-buffer bufferset name)
  (if (bufferset-find-buffer bufferset name)
      (error "Attempt to re-create buffer" name))
  (let ((buffer
	 (make-buffer name
		      (ref-variable editor-default-mode)
		      (if within-editor?
			  (buffer-default-directory (current-buffer))
			  (working-directory-pathname)))))
    (string-table-put! (bufferset-names bufferset) name buffer)
    (set-bufferset-buffer-list!
     bufferset
     (append! (bufferset-buffer-list bufferset) (list buffer)))
    buffer))

(define (bufferset-find-or-create-buffer bufferset name)
  (or (bufferset-find-buffer bufferset name)
      (bufferset-create-buffer bufferset name)))

(define (bufferset-kill-buffer! bufferset buffer)
  (if (not (memq buffer (bufferset-buffer-list bufferset)))
      (error "Attempt to kill unknown buffer" buffer))
  (set-bufferset-buffer-list! bufferset
			      (delq! buffer (bufferset-buffer-list bufferset)))
  (string-table-remove! (bufferset-names bufferset) (buffer-name buffer)))

(define (bufferset-rename-buffer bufferset buffer new-name)
  (if (not (memq buffer (bufferset-buffer-list bufferset)))
      (error "Attempt to rename unknown buffer" buffer))
  (if (bufferset-find-buffer bufferset new-name)
      (error "Attempt to rename buffer to existing buffer name" new-name))
  (let ((names (bufferset-names bufferset)))
    (string-table-remove! names (buffer-name buffer))
    (set-buffer-name! buffer new-name)
    (string-table-put! names new-name buffer)))