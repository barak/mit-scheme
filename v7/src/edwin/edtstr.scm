;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/edtstr.scm,v 1.19 1992/02/04 04:02:41 cph Exp $
;;;
;;;	Copyright (c) 1989-92 Massachusetts Institute of Technology
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

;;;; Editor Data Abstraction

(declare (usual-integrations))

(define-structure (editor (constructor %make-editor))
  (name false read-only true)
  (display-type false read-only true)
  (screens '())
  (selected-screen false)
  (bufferset false read-only true)
  (char-history false read-only true)
  (halt-update? false read-only true)
  (char-ready? false read-only true)
  (peek-char false read-only true)
  (read-char false read-only true)
  (button-event false)
  (select-time 1))

(define (make-editor name display-type make-screen-args)
  (let ((initial-buffer
	 (make-buffer initial-buffer-name
		      (ref-mode-object fundamental)
		      (working-directory-pathname))))
    (let ((bufferset (make-bufferset initial-buffer))
	  (screen (display-type/make-screen display-type make-screen-args)))
      (initialize-screen-root-window! screen bufferset initial-buffer)
      (with-values
	  (lambda () (display-type/get-input-operations display-type screen))
	(lambda (halt-update? char-ready? peek-char read-char)
	  (%make-editor name
			display-type
			(list screen)
			screen
			bufferset
			(make-ring 100)
			halt-update?
			char-ready?
			peek-char
			read-char
			false
			1))))))

(define-integrable (current-display-type)
  (editor-display-type current-editor))

(define-integrable (with-editor-interrupts-enabled thunk)
  (display-type/with-interrupts-enabled (current-display-type) thunk))

(define-integrable (with-editor-interrupts-disabled thunk)
  (display-type/with-interrupts-disabled (current-display-type) thunk))

(define-integrable (current-bufferset)
  (editor-bufferset current-editor))

(define-integrable (current-char-history)
  (editor-char-history current-editor))

(define (increment-select-time!)
  (let ((time (editor-select-time current-editor)))
    (set-editor-select-time! current-editor (1+ time))
    time))

(define-structure (button-event (conc-name button-event/))
  (window false read-only true)
  (x false read-only true)
  (y false read-only true))

(define (current-button-event)
  (or (editor-button-event current-editor)
      ;; Create a "dummy" event at point.
      (let ((window (current-window)))
	(let ((coordinates (window-point-coordinates window)))
	  (make-button-event window
			     (car coordinates)
			     (cdr coordinates))))))

(define (with-current-button-event button-event thunk)
  (let ((old-button-event))
    (unwind-protect
     (lambda ()
       (set! old-button-event (editor-button-event current-editor))
       (set-editor-button-event! current-editor button-event)
       (set! button-event false)
       unspecific)
     thunk
     (lambda ()
       (set-editor-button-event! current-editor old-button-event)))))

(define button-record-type
  (make-record-type 'BUTTON '(NUMBER DOWN?)))

(define make-down-button)
(define make-up-button)
(let ((%make-button
       (let ((constructor
	      (record-constructor button-record-type '(NUMBER DOWN?))))
	 (lambda (buttons number down?)
	   (or (vector-ref buttons number)
	       (let ((button (constructor number down?)))
		 (vector-set! buttons number button)
		 button)))))
      (down-buttons '#())
      (up-buttons '#()))
  (set! make-down-button
	(lambda (number)
	  (if (>= number (vector-length down-buttons))
	      (set! down-buttons (vector-grow down-buttons (1+ number))))
	  (%make-button down-buttons number true)))
  (set! make-up-button
	(lambda (number)
	  (if (>= number (vector-length up-buttons))
	      (set! up-buttons (vector-grow up-buttons (1+ number))))
	  (%make-button up-buttons number false))))

(define (make-modified-button modifier button-number up-or-down)
  (let ((button
	 (+ button-number
	    (case modifier
	      ((shift) 5)
	      ((control) 10)
	      ((meta) 20)
	      (else (error "make-modified-button: Bad button modifier"
			   modifier))))))
    (cond ((eq? up-or-down 'DOWN)
	   (make-down-button button))
	  ((eq? up-or-down 'UP)
	   (make-up-button button))
	  (else (error "make-modified-button: Must specify UP or DOWN"
		       up-or-down)))))

(define button?
  (record-predicate button-record-type))

(define button/number
  (record-accessor button-record-type 'NUMBER))

(define button/down?
  (record-accessor button-record-type 'DOWN?))

(define (down-button? object)
  (and (button? object)
       (button/down? object)))

(define (up-button? object)
  (and (button? object)
       (not (button/down? object))))

(set-record-type-unparser-method! button-record-type
  (unparser/standard-method (record-type-name button-record-type)
    (lambda (state button)
      (unparse-string state (if (button/down? button) "down" "up"))
      (unparse-char state #\space)
      (unparse-object state (button/number button)))))