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

;;;; Editor Abstraction

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define edwin-editor)
(define restrict-editor-x-size #!FALSE)

(define edwin-reset)
(define edwin-reset-windows)
(in-package window-package

(set! edwin-reset
(named-lambda (edwin-reset)
  (cond ((unassigned? the-alpha-window)
	 (reset-alpha-window!))
	((not (unassigned? edwin-editor))
	 (delete-inferior! the-alpha-window
			   (editor-frame-window edwin-editor))))
  (set! edwin-editor
	(let ((x-size (window-x-size the-alpha-window))
	      (y-size (window-y-size the-alpha-window)))
	  (if (> y-size 24) (set! typein-y-size 2))
	  (make-editor "Edwin" the-alpha-window 0 0
		       (if restrict-editor-x-size
			   (min restrict-editor-x-size x-size)
			   x-size)
		       y-size)))
  (within-editor edwin-editor
    (lambda ()
      (add-buffer-initialization! (current-buffer)
	(lambda ()
	  (with-output-to-mark (current-point)
	    (lambda ()
	      (identify-world)
	      (write-string "

;You are in an interaction window of the Edwin editor.
;Type C-H for help.  C-H M will describe some useful commands.")))
	  (insert-interaction-prompt)
	  (set-window-start-mark! (current-window)
				  (buffer-start (current-buffer))
				  #!FALSE)))))
  *the-non-printing-object*))

(set! edwin-reset-windows
(named-lambda (edwin-reset-windows)
  (send the-alpha-window ':salvage!)))

)

(define (edwin)
  (if (or (unassigned? edwin-editor)
	  (not edwin-editor))
      (edwin-reset))
  (with-keyboard-interrupt-dispatch-table
   editor-keyboard-interrupt-dispatch-table
   (lambda ()
     (with-editor-interrupts-enabled
      (lambda ()
	(with-editor-input-port console-input-port
	  (lambda ()
	    (within-editor edwin-editor
	      (lambda ()
		(fluid-let (((access *error-hook* error-system)
			     edwin-error-hook))
		  (perform-buffer-initializations! (current-buffer))
		  (push-command-loop (lambda () 'DONE)
				     (lambda (state)
				       (update-alpha-window! #!TRUE)
				       (top-level-command-reader)
				       state)
				     'DUMMY-STATE))))))))))
  (tty-redraw-screen)
  *the-non-printing-object*)

(in-package system-global-environment

(define tty-redraw-screen
  (make-primitive-procedure 'TTY-REDRAW-SCREEN))

)

(define editor-continuation)
(define recursive-edit-continuation)
(define recursive-edit-level)
(define current-editor)
(define saved-error-hook)

(define (within-editor editor thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (fluid-let ((editor-continuation continuation)
		 (recursive-edit-continuation #!FALSE)
		 (recursive-edit-level 0)
		 (current-editor editor)
		 (saved-error-hook (access *error-hook* error-system)))
       (thunk)))))

(define (enter-recursive-edit)
  (let ((value
	 (call-with-current-continuation
	   (lambda (continuation)
	     (fluid-let ((recursive-edit-continuation continuation)
			 (recursive-edit-level (1+ recursive-edit-level)))
	       (dynamic-wind recursive-edit-event!
			     command-reader
			     recursive-edit-event!))))))
    (if (eq? value 'ABORT)
	(abort-current-command)
	(begin (reset-command-prompt!)
	       value))))

(define (recursive-edit-event!)
  (for-each (lambda (window)
	      (window-modeline-event! window 'RECURSIVE-EDIT))
	    (window-list)))

(define (exit-recursive-edit value)
  (if recursive-edit-continuation
      (recursive-edit-continuation value)
      (editor-abort value)))

(define (editor-abort value)
  (editor-continuation value))

(declare (integrate current-frame current-bufferset current-kill-ring))
(define (current-frame) (editor-frame-window current-editor))
(define (current-bufferset) (editor-bufferset current-editor))
(define (current-kill-ring) (editor-kill-ring current-editor))
(define (current-char-history) (editor-char-history current-editor))

(define processing-error?
  #!FALSE)

(define (edwin-error-hook environment message irritant
			  substitute-environment?)
  ((if processing-error?
       saved-error-hook
       (or (ref-variable "& Scheme Error Hook")
	   saved-error-hook))
   environment message irritant substitute-environment?))

(define-named-structure "Editor"
  name
  frame-window
  bufferset
  kill-ring
  char-history)

(define (make-editor name superior x-start y-start x-size y-size)
  (let ((initial-buffer (make-buffer initial-buffer-name interaction-mode)))
    (let ((bufferset (make-bufferset initial-buffer)))
      (let ((editor (%make-editor)))
	(vector-set! editor editor-index:name name)
	(vector-set! editor editor-index:frame-window
		     ((access make-editor-frame window-package)
		      superior x-start y-start x-size y-size
		      name initial-buffer
		      (bufferset-create-buffer bufferset " *Typein-0*")))
	(vector-set! editor editor-index:bufferset bufferset)
	(vector-set! editor editor-index:kill-ring (make-ring 10))
	(vector-set! editor editor-index:char-history (make-ring 100))
	editor))))

(define initial-buffer-name
  "*scratch*")

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; End:
