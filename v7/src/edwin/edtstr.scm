;;; -*-Scheme-*-
;;;
;;; $Id: edtstr.scm,v 1.24 2002/11/20 19:45:59 cph Exp $
;;;
;;; Copyright (c) 1989-1999 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT Scheme.
;;;
;;; MIT Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 2 of the License,
;;; or (at your option) any later version.
;;;
;;; MIT Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT Scheme; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

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
  (peek-no-hang false read-only true)
  (peek false read-only true)
  (read false read-only true)
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
	(lambda (halt-update? peek-no-hang peek read)
	  (%make-editor name
			display-type
			(list screen)
			screen
			bufferset
			(make-ring 100)
			halt-update?
			peek-no-hang
			peek
			read
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

;;;; Buttons

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