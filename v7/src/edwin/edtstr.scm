#| -*-Scheme-*-

$Id: edtstr.scm,v 1.30 2003/03/06 05:05:52 cph Exp $

Copyright 1989,1990,1991,1992,2003 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Editor Data Abstraction

(declare (usual-integrations))

(define-structure (editor (constructor %make-editor))
  (name #f read-only #t)
  (display-type #f read-only #t)
  (screens '())
  (selected-screen #f)
  (bufferset #f read-only #t)
  (char-history #f read-only #t)
  (halt-update? #f read-only #t)
  (peek-no-hang #f read-only #t)
  (peek #f read-only #t)
  (read #f read-only #t)
  (button-event #f)
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
			#f
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
  (window #f read-only #t)
  (x #f read-only #t)
  (y #f read-only #t))

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
    (dynamic-wind
     (lambda ()
       (set! old-button-event (editor-button-event current-editor))
       (set-editor-button-event! current-editor button-event)
       (set! button-event #f)
       unspecific)
     thunk
     (lambda ()
       (set-editor-button-event! current-editor old-button-event)))))

(define-record-type button-record-type
  (%%make-button number down?)
  button?
  (number button/number)
  (down? button/down?))

(define make-down-button)
(define make-up-button)
(let ((%make-button
       (lambda (buttons number down?)
	 (or (vector-ref buttons number)
	     (let ((button (%%make-button number down?)))
	       (vector-set! buttons number button)
	       button))))
      (down-buttons '#())
      (up-buttons '#()))
  (set! make-down-button
	(lambda (number)
	  (if (>= number (vector-length down-buttons))
	      (set! down-buttons (vector-grow down-buttons (+ number 1) #f)))
	  (%make-button down-buttons number #t)))
  (set! make-up-button
	(lambda (number)
	  (if (>= number (vector-length up-buttons))
	      (set! up-buttons (vector-grow up-buttons (+ number 1) #f)))
	  (%make-button up-buttons number #f))))

(define (down-button? object)
  (and (button? object)
       (button/down? object)))

(define (up-button? object)
  (and (button? object)
       (not (button/down? object))))

(define (button/bucky-bits button)
  button
  0)

(set-record-type-unparser-method! button-record-type
  (unparser/standard-method (record-type-name button-record-type)
    (lambda (state button)
      (unparse-string state (if (button/down? button) "down" "up"))
      (unparse-char state #\space)
      (unparse-object state (button/number button)))))