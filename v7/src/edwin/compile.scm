;;; -*-Scheme-*-
;;;
;;; $Id: compile.scm,v 1.5 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1992-1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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

(define-command grep
  "Run grep, with user-specified args, and collect output in a buffer."
  (lambda ()
    (list (prompt-for-string "Run grep (with args): "
			     previous-grep-arguments
			     'INSERTED-DEFAULT)))
  (lambda (command)
    (set! previous-grep-arguments command)
    (run-compilation (string-append "grep -n " command " /dev/null"))))

(define-command kill-compilation
  "Kill the process made by the \\[compile] command."
  ()
  (lambda ()
    (let ((process compilation-process))
      (if (and process (eq? (process-status process) 'RUN))
	  (interrupt-process process true)))))

(define-command kill-grep
  "Kill the process made by the \\[grep] command."
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
    (define-variable-local-value! buffer
	(ref-variable-object mode-line-process)
      '(": %s"))
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
	   (apply start-process
		  "compilation"
		  buffer
		  scheme-subprocess-environment
		  (ref-variable shell-file-name)
		  (os/form-shell-command command))))
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

(define previous-grep-arguments
  "")