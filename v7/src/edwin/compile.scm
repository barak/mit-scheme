;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/compile.scm,v 1.1 1992/03/24 23:31:41 cph Exp $
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

;;;; Compilation Subprocess

(declare (usual-integrations))

(define-variable compile-command
  "Last shell command used to do a compilation; default for next compilation."
  "make -k")

(define-command compile
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*."
  (lambda ()
    (list (prompt-for-string "Compile command"
			     (ref-variable compile-command)
			     'INSERTED-DEFAULT)))
  (lambda (command)
    (set-variable! compile-command command)
    (run-compilation command)))

(define-command kill-compilation
  "Kill the process made by the \\[compile] command."
  ()
  (lambda ()
    (let ((process compilation-process))
      (if (and process (eq? (process-status process) 'RUN))
	  (interrupt-process process true)))))

(define (run-compilation command)
  ((ref-command save-some-buffers) false)
  (let ((process compilation-process))
    (if process
	(begin
	  (if (eq? (process-status process) 'RUN)
	      (begin
		(if (not (prompt-for-yes-or-no?
			  "A compilation process is running; kill it"))
		    (editor-error "Cannot have two compilation processes"))
		(interrupt-process process true)
		(sit-for 1000)))
	  (delete-process process))))
  (let ((buffer (temporary-buffer "*compilation*"))
	(directory (buffer-default-directory (current-buffer))))
    (disable-group-undo! (buffer-group buffer))
    (set-buffer-default-directory! buffer directory)
    (set-buffer-major-mode! buffer (ref-mode-object fundamental))
    (add-buffer-initialization!
     buffer
     (lambda ()
       (define-variable-local-value! buffer
	 (ref-variable-object mode-line-process)
	 '(": %s"))))
    (let ((mark (mark-left-inserting-copy (buffer-start buffer))))
      (let ((window (get-buffer-window buffer)))
	(if window
	    (set-window-start-mark! window mark true)))
      (insert-string "cd " mark)
      (insert-string (->namestring directory) mark)
      (insert-newline mark)
      (insert-string command mark)
      (insert-newline mark)
      (mark-temporary! mark))
    (let ((process
	   (start-process "compilation"
			  buffer
			  scheme-subprocess-environment
			  "/bin/sh"
			  "-c"
			  (string-append "exec " command))))
      (set-process-sentinel! process compilation-process-sentinel)
      (set! compilation-process process))
    (pop-up-buffer buffer false)))

(define (compilation-process-sentinel process status reason)
  (let ((buffer (process-buffer process)))
    (if buffer
	(if (memq (process-status process) '(EXIT SIGNAL))
	    (let ((mark (mark-left-inserting-copy (buffer-end buffer))))
	      (insert-newline mark)
	      (insert-string "Process " mark)
	      (insert-string (process-name process) mark)
	      (insert-string " " mark)
	      (insert-string (process-status-message status reason) mark)
	      (insert-newline mark)
	      (mark-temporary! mark)))))
  (without-interrupts
   (lambda ()
     (if (eq? process compilation-process)
	 (set! compilation-process false))))
  unspecific)

(define compilation-process
  false)