;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1989 Massachusetts Institute of Technology
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

;;;; Editor Data Abstraction

(declare (usual-integrations))

(define-structure (editor (constructor %make-editor))
  (name false read-only true)
  (screen false read-only true)
  (frame-window false read-only true)
  (bufferset false read-only true)
  (kill-ring false read-only true)
  (char-history false read-only true))

(define (make-editor name screen)
  (let ((initial-buffer (make-buffer initial-buffer-name interaction-mode)))
    (let ((bufferset (make-bufferset initial-buffer)))
      (let ((frame
	     (make-editor-frame screen
				initial-buffer
				(bufferset-create-buffer bufferset
							 " *Typein-0*"))))
	(set-screen-window! screen frame)
	(%make-editor name
		      screen
		      frame
		      bufferset
		      (make-ring 10)
		      (make-ring 100))))))

(define initial-buffer-name
  "*scratch*")

(define-integrable (current-screen)
  (editor-screen current-editor))

(define-integrable (current-editor-frame)
  (editor-frame-window current-editor))
(define-integrable (current-bufferset)
  (editor-bufferset current-editor))

(define-integrable (current-kill-ring)
  (editor-kill-ring current-editor))

(define-integrable (current-char-history)
  (editor-char-history current-editor))