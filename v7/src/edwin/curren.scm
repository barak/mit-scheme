;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Current State

(declare (usual-integrations)
	 (integrate-external "edb:editor.bin.0")
	 (integrate-external "edb:buffer.bin.0")
	 (integrate-external "edb:bufset.bin.0"))
(using-syntax edwin-syntax-table

;;;; Windows

(define (current-window)
  ((access editor-frame-selected-window window-package) (current-frame)))

(define (window0)
  ((access editor-frame-window0 window-package) (current-frame)))

(define (typein-window)
  ((access editor-frame-typein-window window-package) (current-frame)))

(define (typein-window? window)
  (eq? window (typein-window)))

(define (select-window window)
  (without-interrupts
   (lambda ()
     (exit-buffer (current-buffer))
     ((access editor-frame-select-window! window-package)
      (current-frame)
      window)
     (enter-buffer (window-buffer window)))))

(define (select-cursor window)
  ((access editor-frame-select-cursor! window-package) (current-frame) window))

(define ((window-buffer-setter enter-buffer exit-buffer) window buffer)
  (without-interrupts
   (lambda ()
     (let ((current (current-window)))
       (if (eq? window current)
	   (begin (exit-buffer (window-buffer current))
		  ((access set-window-buffer! window-package) window buffer)
		  (enter-buffer buffer))
	   ((access set-window-buffer! window-package) window buffer))))))

(define (window-list)
  (let ((window0 (window0)))
    (define (loop window)
      (if (eq? window window0)
	  (list window)
	  (cons window (loop (window1+ window)))))
    (loop (window1+ window0))))

(define (window-visible? window)
  (or (typein-window? window)
      (let ((window0 (window0)))
	(define (loop window*)
	  (or (eq? window window*)
	      (and (not (eq? window* window0))
		   (loop (window1+ window*)))))
	(loop (window1+ window0)))))

(define other-window
  (let ()
    (define (+loop n window)
      (if (zero? n)
	  window
	  (+loop (-1+ n)
		 (if (typein-window? window)
		     (window0)
		     (let ((window (window1+ window)))
		       (if (and (within-typein-edit?)
				(eq? window (window0)))
			   (typein-window)
			   window))))))
    (define (-loop n window)
      (if (zero? n)
	  window
	  (-loop (1+ n)
		 (if (and (within-typein-edit?)
			  (eq? window (window0)))
		     (typein-window)
		     (window-1+ (if (typein-window? window)
				    (window0)
				    window))))))
    (named-lambda (other-window #!optional n)
      (if (or (unassigned? n) (not n)) (set! n 1))
      (cond ((positive? n) (+loop n (current-window)))
	    ((negative? n) (-loop n (current-window)))
	    (else (current-window))))))

(define (window-delete! window)
  (if (typein-window? window)
      (editor-error "Attempt to delete the typein window"))
  (if (window-has-no-neighbors? window)
      (editor-error "Attempt to delete only window"))
  (if (eq? window (current-window))
      (begin (select-window (window1+ window))
	     (select-window ((access window-delete! window-package) window)))
      ((access window-delete! window-package) window)))

(define (window-grow-horizontally! window n)
  (if (typein-window? window)
      (editor-error "Can't grow the typein window"))
  (if (not (window-has-horizontal-neighbor? window))
      (editor-error "Can't grow this window horizontally"))
  ((access window-grow-horizontally! window-package) window n))

(define (window-grow-vertically! window n)
  (if (typein-window? window)
      (editor-error "Can't grow the typein window"))
  (if (not (window-has-vertical-neighbor? window))
      (editor-error "Can't grow this window vertically"))
  ((access window-grow-vertically! window-package) window n))

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

(define-integrable (select-buffer buffer)
  (set-window-buffer! (current-window) buffer))

(define-integrable (select-buffer-no-record buffer)
  (set-window-buffer-no-record! (current-window) buffer))

(define-integrable (select-buffer-in-window buffer window)
  (set-window-buffer! window buffer))

(define (select-buffer-other-window buffer)
  (define (expose-buffer window)
    (select-window window)
    (select-buffer buffer))

  (let ((window (current-window)))
    (if (window-has-no-neighbors? window)
	(expose-buffer (window-split-vertically! window #!FALSE))
	(let ((window* (get-buffer-window buffer)))
	  (if (and window* (not (eq? window window*)))
	      (begin (set-window-point! window* (buffer-point buffer))
		     (select-window window*))
	      (expose-buffer (window1+ window)))))))

(define (bury-buffer buffer)
  (bufferset-bury-buffer! (current-bufferset) buffer))

(define (enter-buffer buffer)
  (bufferset-select-buffer! (current-bufferset) buffer)
  (%wind-local-bindings! buffer)
  (perform-buffer-initializations! buffer))

(define (exit-buffer buffer)
  (bufferset-select-buffer! (current-bufferset) buffer)
  (%wind-local-bindings! buffer))

(define set-window-buffer!
  (window-buffer-setter enter-buffer exit-buffer))

(define (enter-buffer-no-record buffer)
  (%wind-local-bindings! buffer)
  (perform-buffer-initializations! buffer))

(define (exit-buffer-no-record buffer)
  (%wind-local-bindings! buffer))

(define set-window-buffer-no-record!
  (window-buffer-setter enter-buffer-no-record exit-buffer-no-record))

(define (with-selected-buffer buffer thunk)
  (define (switch)
    (let ((new-buffer (set! buffer (current-buffer))))
      (if (buffer-alive? new-buffer)
	  (select-buffer new-buffer))))
  (dynamic-wind switch thunk switch))

(define (other-buffer buffer)
  (define (loop less-preferred buffers)
    (cond ((null? buffers)
	   less-preferred)
	  ((or (eq? buffer (car buffers))
	       (minibuffer? (car buffers)))
	   (loop less-preferred (cdr buffers)))
	  ((buffer-visible? (car buffers))
	   (loop (or less-preferred (car buffers)) (cdr buffers)))
	  (else
	   (car buffers))))
  (loop #!FALSE (buffer-list)))

(define-integrable (find-buffer name)
  (bufferset-find-buffer (current-bufferset) name))

(define-integrable (create-buffer name)
  (bufferset-create-buffer (current-bufferset) name))

(define-integrable (find-or-create-buffer name)
  (bufferset-find-or-create-buffer (current-bufferset) name))

(define (kill-buffer buffer)
  (if (buffer-visible? buffer)
      (let ((new-buffer
	     (or (other-buffer buffer)
		 (error "Buffer to be killed has no replacement" buffer))))
	(for-each (lambda (window)
		    (set-window-buffer! window new-buffer))
		  (buffer-windows buffer))))  (bufferset-kill-buffer! (current-bufferset) buffer))

(define-integrable (rename-buffer buffer new-name)
  (bufferset-rename-buffer (current-bufferset) buffer new-name))

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
  (define (switch)
    (set-current-point! (set! point (current-point))))
  (dynamic-wind switch thunk switch))

;;;; Mark and Region

(define-integrable (current-mark)
  (buffer-mark (current-buffer)))

(define (buffer-mark buffer)
  (let ((ring (buffer-mark-ring buffer)))
    (if (ring-empty? ring) (editor-error))
    (ring-ref ring 0)))

(define (set-current-mark! mark)
  (if (not (mark? mark)) (error "New mark not a mark" mark))
  (set-buffer-mark! (current-buffer) mark))

(define (set-buffer-mark! buffer mark)
  (ring-set! (buffer-mark-ring buffer)
	     0
	     (mark-right-inserting mark)))

(define-variable "Auto Push Point Notification"
  "Message to display when point is pushed on the mark ring, or false."
  "Mark Set")

(define (push-current-mark! mark)
  (if (not (mark? mark)) (error "New mark not a mark" mark))
  (push-buffer-mark! (current-buffer) mark)
  (if (and (ref-variable "Auto Push Point Notification")
	   (not *executing-keyboard-macro?*)
	   (not (typein-window? (current-window))))
      (temporary-message (ref-variable "Auto Push Point Notification"))))

(define (push-buffer-mark! buffer mark)
  (ring-push! (buffer-mark-ring buffer)
	      (mark-right-inserting mark)))

(define-integrable (pop-current-mark!)
  (pop-buffer-mark! (current-buffer)))

(define (pop-buffer-mark! buffer)
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
(define-integrable (current-comtab)	;**** misnamed, should be plural.
  (buffer-comtabs (current-buffer)))

(define (set-current-major-mode! mode)
  (set-buffer-major-mode! (current-buffer) mode))

(define (current-minor-mode? mode)
  (buffer-minor-mode? (current-buffer) mode))

(define (enable-current-minor-mode! mode)
  (enable-buffer-minor-mode! (current-buffer) mode))

(define (disable-current-minor-mode! mode)
  (disable-buffer-minor-mode! (current-buffer) mode))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
