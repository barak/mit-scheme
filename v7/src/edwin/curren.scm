;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/curren.scm,v 1.82 1989/04/23 23:19:56 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Current State

(declare (usual-integrations))

;;;; Windows

(define-integrable (current-window)
  (editor-frame-selected-window (current-editor-frame)))

(define-integrable (current-window? window)
  (eq? window (current-window)))

(define-integrable (window0)
  (editor-frame-window0 (current-editor-frame)))

(define-integrable (typein-window)
  (editor-frame-typein-window (current-editor-frame)))

(define-integrable (typein-window? window)
  (eq? window (typein-window)))

(define (select-window window)
  (without-interrupts
   (lambda ()
     (let ((frame (current-editor-frame)))
       (%wind-local-bindings!
	(window-buffer (editor-frame-selected-window frame)))
       (editor-frame-select-window! frame window))
     (let ((buffer (window-buffer window)))
       (%wind-local-bindings! buffer)
       (perform-buffer-initializations! buffer)
       (bufferset-select-buffer! (current-bufferset) buffer)))))

(define-integrable (select-cursor window)
  (editor-frame-select-cursor! (current-editor-frame) window))

(define (window-list)
  (let ((window0 (window0)))
    (let loop ((window (window1+ window0)))
      (cons window
	    (if (eq? window window0)
		'()
		(loop (window1+ window)))))))

(define (window-visible? window)
  (or (typein-window? window)
      (let ((window0 (window0)))
	(let loop ((window* (window1+ window0)))
	  (or (eq? window window*)
	      (and (not (eq? window* window0))
		   (loop (window1+ window*))))))))

(define (other-window #!optional n)
  (let ((n (if (or (default-object? n) (not n)) 1 n))
	(window (current-window)))
    (cond ((positive? n)
	   (let loop ((n n) (window window))
	     (if (zero? n)
		 window
		 (loop (-1+ n)
		       (if (typein-window? window)
			   (window0)
			   (let ((window (window1+ window)))
			     (if (and (within-typein-edit?)
				      (eq? window (window0)))
				 (typein-window)
				 window)))))))
	  ((negative? n)
	   (let loop ((n n) (window window))
	     (if (zero? n)
		 window
		 (loop (1+ n)
		       (if (and (within-typein-edit?)
				(eq? window (window0)))
			   (typein-window)
			   (window-1+ (if (typein-window? window)
					  (window0)
					  window)))))))
	  (else
	   window))))

;;;; Buffers

(define-integrable (buffer-list)
  (list-copy (bufferset-buffer-list (current-bufferset))))

(define-integrable (buffer-alive? buffer)
  (memq buffer (bufferset-buffer-list (current-bufferset))))

(define-integrable (buffer-names)
  (bufferset-names (current-bufferset)))

(define-integrable (current-buffer)
  (window-buffer (current-window)))

(define-integrable (previous-buffer)
  (other-buffer (current-buffer)))

(define (other-buffer buffer)
  (let loop ((less-preferred false) (buffers (buffer-list)))
    (cond ((null? buffers)
	   less-preferred)
	  ((or (eq? buffer (car buffers))
	       (minibuffer? (car buffers)))
	   (loop less-preferred (cdr buffers)))
	  ((buffer-visible? (car buffers))
	   (loop (or less-preferred (car buffers)) (cdr buffers)))
	  (else
	   (car buffers)))))

(define-integrable (bury-buffer buffer)
  (bufferset-bury-buffer! (current-bufferset) buffer))

(define-integrable (find-buffer name)
  (bufferset-find-buffer (current-bufferset) name))

(define-integrable (create-buffer name)
  (bufferset-create-buffer (current-bufferset) name))

(define-integrable (find-or-create-buffer name)
  (bufferset-find-or-create-buffer (current-bufferset) name))

(define-integrable (rename-buffer buffer new-name)
  (bufferset-rename-buffer (current-bufferset) buffer new-name))

(define (kill-buffer buffer)
  (if (buffer-visible? buffer)
      (let ((new-buffer
	     (or (other-buffer buffer)
		 (error "Buffer to be killed has no replacement" buffer))))
	(for-each (lambda (window)
		    (set-window-buffer! window new-buffer false))
		  (buffer-windows buffer))))  (bufferset-kill-buffer! (current-bufferset) buffer))

(define-integrable (select-buffer buffer)
  (set-window-buffer! (current-window) buffer true))

(define-integrable (select-buffer-no-record buffer)
  (set-window-buffer! (current-window) buffer false))

(define-integrable (select-buffer-in-window buffer window)
  (set-window-buffer! window buffer true))

(define (set-window-buffer! window buffer record?)
  (without-interrupts
   (lambda ()
     (if (current-window? window)
	 (begin
	   (%wind-local-bindings! (window-buffer window))
	   (%set-window-buffer! window buffer)
	   (%wind-local-bindings! buffer)
	   (perform-buffer-initializations! buffer)	   (if record? (bufferset-select-buffer! (current-bufferset) buffer)))
	 (%set-window-buffer! window buffer)))))
(define (with-selected-buffer buffer thunk)
  (let ((old-buffer))
    (dynamic-wind (lambda ()
		    (let ((window (current-window)))
		      (set! old-buffer (window-buffer window))
		      (if (buffer-alive? buffer)
			  (set-window-buffer! window buffer true)))
		    (set! buffer)
		    unspecific)
		  thunk
		  (lambda ()
		    (let ((window (current-window)))
		      (set! buffer (window-buffer window))
		      (if (buffer-alive? old-buffer)
			  (set-window-buffer! window old-buffer true)))
		    (set! old-buffer)
		    unspecific))))

(define (select-buffer-other-window buffer)
  (let ((window
	 (let ((window (current-window)))
	   (if (window-has-no-neighbors? window)
	       (window-split-vertically! window false)
	       (or (list-search-negative (buffer-windows buffer)
		     (lambda (window*)
		       (eq? window window*)))
		   (window1+ window))))))
    (select-window window)
    (set-window-buffer! window buffer true)))

;;;; Point

(define-integrable (current-point)
  (window-point (current-window)))

(define-integrable (set-current-point! mark)
  (set-window-point! (current-window) mark))

(define (set-buffer-point! buffer mark)
  (if (buffer-visible? buffer)
      (for-each (lambda (window)
		  (set-window-point! window mark))
		(buffer-windows buffer))
      (%set-buffer-point! buffer mark)))

(define (with-current-point point thunk)
  (let ((old-point))
    (dynamic-wind (lambda ()
		    (let ((window (current-window)))
		      (set! old-point (window-point window))
		      (set-window-point! window point))
		    (set! point)
		    unspecific)
		  thunk
		  (lambda ()
		    (let ((window (current-window)))
		      (set! point (window-point window))
		      (set-window-point! window old-point))
		    (set! old-point)
		    unspecific))))

(define-integrable (current-column)
  (mark-column (current-point)))

;;;; Mark and Region

(define-integrable (current-mark)
  (buffer-mark (current-buffer)))

(define (buffer-mark buffer)
  (let ((ring (buffer-mark-ring buffer)))
    (if (ring-empty? ring)
	(editor-error)
	(ring-ref ring 0))))

(define (set-current-mark! mark)
  (set-buffer-mark! (current-buffer) (guarantee-mark mark)))

(define-integrable (set-buffer-mark! buffer mark)
  (ring-set! (buffer-mark-ring buffer) 0 (mark-right-inserting mark)))

(define-variable auto-push-point-notification
  "Message to display when point is pushed on the mark ring, or false."
  "Mark Set")

(define (push-current-mark! mark)
  (push-buffer-mark! (current-buffer) (guarantee-mark mark))
  (let ((notification (ref-variable auto-push-point-notification)))
    (if (and notification
	     (not *executing-keyboard-macro?*)
	     (not (typein-window? (current-window))))
	(temporary-message notification))))

(define-integrable (push-buffer-mark! buffer mark)
  (ring-push! (buffer-mark-ring buffer) (mark-right-inserting mark)))

(define-integrable (pop-current-mark!)
  (pop-buffer-mark! (current-buffer)))

(define-integrable (pop-buffer-mark! buffer)
  (ring-pop! (buffer-mark-ring buffer)))

(define-integrable (current-region)
  (make-region (current-point) (current-mark)))

(define (set-current-region! region)
  (set-current-point! (region-start region))
  (push-current-mark! (region-end region)))

(define (set-current-region-reversed! region)
  (push-current-mark! (region-start region))
  (set-current-point! (region-end region)))

;;;; Modes and Comtabs

(define-integrable (current-modes)
  (buffer-modes (current-buffer)))

(define-integrable (current-major-mode)
  (buffer-major-mode (current-buffer)))
(define-integrable (current-comtabs)
  (buffer-comtabs (current-buffer)))

(define-integrable (set-current-major-mode! mode)
  (set-buffer-major-mode! (current-buffer) mode))

(define-integrable (current-minor-mode? mode)
  (buffer-minor-mode? (current-buffer) mode))

(define-integrable (enable-current-minor-mode! mode)
  (enable-buffer-minor-mode! (current-buffer) mode))

(define-integrable (disable-current-minor-mode! mode)
  (disable-buffer-minor-mode! (current-buffer) mode))