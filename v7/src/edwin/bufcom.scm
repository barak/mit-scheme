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

;;;; Buffer Commands

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define-command ("^R Buffer Not Modified" argument)
  "Pretend that this buffer hasn't been altered."
  (buffer-not-modified! (current-buffer)))

(define-command ("Select Buffer" argument)
  "Select buffer with specified name.
If the variable Select Buffer Create is true,
specifying a non-existent buffer will cause it to be created."
  (select-buffer (prompt-for-select-buffer "Select Buffer")))

(define-command ("Select Buffer Other Window" argument)
  "Select buffer in another window."
  (select-buffer-other-window
   (prompt-for-select-buffer "Select Buffer Other Window")))

(define-variable "Select Buffer Create"
  "If true, buffer selection commands may create new buffers."
  true)

(define (prompt-for-select-buffer prompt)
  ((if (ref-variable "Select Buffer Create")
       prompt-for-buffer prompt-for-existing-buffer)
   prompt (previous-buffer)))

(define-command ("Create Buffer" argument)
  "Create a new buffer with a given name, and select it."
  (let ((buffer (new-buffer (prompt-for-string "Create Buffer" false))))
    (set-buffer-major-mode! buffer (ref-variable "Editor Default Mode"))
    (select-buffer buffer)))

(define-command ("Insert Buffer" argument)
  "Insert the contents of a specified buffer at point."
  (let ((point (mark-right-inserting (current-point))))
    (region-insert-string!
     point
     (region->string
      (buffer-region (prompt-for-existing-buffer "Insert Buffer" false))))
    (push-current-mark! (current-point))
    (set-current-point! point)))

(define-command ("^R Twiddle Buffers" argument)
  "Select previous buffer."
  (let ((buffer (previous-buffer)))
    (if buffer
	(select-buffer buffer)
	(editor-error "No previous buffer to select"))))

(define-command ("Bury Current Buffer" argument)
  "Deselect the current buffer, putting it at the end of the buffer list."
  (let ((buffer (current-buffer))
	(previous (previous-buffer)))
    (if previous
	(begin (select-buffer previous)
	       (bury-buffer buffer)))))

(define-command ("Kill Buffer" argument)
  "Kill the buffer with specified name.
Does a completing read of the buffer name in the echo area.
If the buffer has changes in it, we offer to write it out."
  (kill-buffer-interactive
   (prompt-for-existing-buffer "Kill Buffer" (current-buffer))))

(define (kill-buffer-interactive buffer)
  (if (not (other-buffer buffer)) (editor-error "Only one buffer"))
  (save-buffer-changes buffer)
  (kill-buffer buffer))

(define-command ("Kill Some Buffers" argument)
  "For each buffer, ask whether to kill it."
  (kill-some-buffers true))

(define (kill-some-buffers prompt?)
  (for-each (lambda (buffer)
	      (if (and (not (minibuffer? buffer))
		       (or (not prompt?)
			   (prompt-for-confirmation?
			    (string-append "Kill buffer '"
					   (buffer-name buffer)
					   "'"))))
		  (if (other-buffer buffer)
		      (kill-buffer-interactive buffer)
		      (let ((dummy (new-buffer "*Dummy*")))
			(kill-buffer-interactive buffer)
			(set-buffer-major-mode!
			 (create-buffer initial-buffer-name)
			 (ref-variable "Editor Default Mode"))
			(kill-buffer dummy)))))
	    (buffer-list)))

(define-command ("Rename Buffer" argument)
  "Change the name of the current buffer.
Reads the new name in the echo area."
  (let ((buffer (current-buffer)))
    (let ((name
	   (prompt-for-string "Rename Buffer"
			      (let ((pathname (buffer-pathname buffer)))
				(and pathname
				     (pathname->buffer-name pathname))))))
      (if (find-buffer name)
	  (editor-error "Buffer named " name " already exists"))
      (rename-buffer buffer name))))

(define-command ("Normal Mode" argument)
  "Reset mode and local variable bindings to their default values.
Just like what happens when the file is first visited."
  (initialize-buffer! (current-buffer)))

(define (save-buffer-changes buffer)
  (if (and (buffer-pathname buffer)
	   (buffer-modified? buffer)
	   (buffer-writeable? buffer)
	   (prompt-for-yes-or-no?
	    (string-append "Buffer "
			   (buffer-name buffer)
			   " contains changes.  Write them out")))
      (write-buffer-interactive buffer)))

(define (new-buffer name)
  (define (search-loop n)
    (let ((new-name (string-append name "<" (write-to-string n) ">")))
      (if (find-buffer new-name)
	  (search-loop (1+ n))
	  new-name)))
  (create-buffer (let ((buffer (find-buffer name)))
		   (if buffer
		       (search-loop 2)
		       name))))
(define (with-output-to-temporary-buffer name thunk)
  (let ((buffer (temporary-buffer name)))
    (with-output-to-mark (buffer-point buffer) thunk)
    (set-buffer-point! buffer (buffer-start buffer))
    (buffer-not-modified! buffer)
    (pop-up-buffer buffer false)))

(define (temporary-buffer name)
  (let ((buffer (find-or-create-buffer name)))
    (buffer-reset! buffer)
    buffer))

(define (prompt-for-buffer prompt default-buffer)
  (let ((name (prompt-for-buffer-name prompt default-buffer)))
    (or (find-buffer name)
	(let ((buffer (create-buffer name)))
	  (set-buffer-major-mode! buffer (ref-variable "Editor Default Mode"))
	  (temporary-message "(New Buffer)")
	  buffer))))

(define (prompt-for-buffer-name prompt default-buffer)
  (prompt-for-completed-string prompt
			       (and default-buffer
				    (buffer-name default-buffer))
			       (if default-buffer
				   'VISIBLE-DEFAULT
				   'NO-DEFAULT)
			       (buffer-names)
			       'PERMISSIVE-COMPLETION))

(define (prompt-for-existing-buffer prompt default-buffer)
  (find-buffer
   (prompt-for-completed-string prompt
				(and default-buffer
				     (buffer-name default-buffer))
			       (if default-buffer
				   'VISIBLE-DEFAULT
				   'NO-DEFAULT)
				(buffer-names)
				'STRICT-COMPLETION)))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
