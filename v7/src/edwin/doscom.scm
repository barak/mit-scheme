#| -*-Scheme-*-

$Id: doscom.scm,v 1.2 1993/02/25 02:56:52 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case.

NOTE: Parts of this program (Edwin) were created by translation from
corresponding parts of GNU Emacs.  Users should be aware that the GNU
GENERAL PUBLIC LICENSE may apply to these parts.  A copy of that
license should have been included along with this file. |#

;;;; Shell commands for DOS

(declare (usual-integrations))

(load-option 'DOSPROCESS)

(define-command shell-command
  "Execute string COMMAND in inferior shell; display output, if any.
Optional second arg true (prefix arg, if interactive) means
insert output in current buffer after point (leave mark after it)."
  "sShell command\nP"
  (lambda (command insert-at-point?)
    (let ((directory (buffer-default-directory (current-buffer))))
      (if insert-at-point?
	  (begin
	    (if (buffer-read-only? (current-buffer))
		(barf-if-read-only))
	    (let ((point (current-point)))
	      (push-current-mark! point)
	      (shell-command false point directory command))
	    ((ref-command exchange-point-and-mark)))
	  (shell-command-pop-up-output
	   (lambda (output-mark)
	      (shell-command false output-mark directory command)))))))

(define-command shell-command-on-region
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer;
Prefix arg means replace the region with it."
  "r\nsShell command on region\nP"
  (lambda (region command replace-region?)
    (let ((directory (buffer-default-directory (current-buffer))))
      (if replace-region?
	  (let ((point (current-point))
		(mark (current-mark)))
	    (let ((swap? (mark< point mark))
		  (temp))
	      (unwind-protect
	       (lambda ()
		 (set! temp (temporary-buffer " *shell-output*"))
		 unspecific)
	       (lambda ()
		 (shell-command (make-region point mark)
				(buffer-start temp)
				directory
				command)
		 (without-interrupts
		  (lambda ()
		    (delete-string point mark)
		    (insert-region (buffer-start temp)
				   (buffer-end temp)
				   (current-point)))))
	       (lambda ()
		 (kill-buffer temp)
		 (set! temp)
		 unspecific))
	      (if swap? ((ref-command exchange-point-and-mark)))))
	  (shell-command-pop-up-output
	   (lambda (output-mark)
	     (shell-command region output-mark directory command)))))))

(define (shell-command-pop-up-output generate-output)
  (let ((buffer (temporary-buffer "*Shell Command Output*")))
    (let ((start (buffer-start buffer)))
      (generate-output start)
      (set-buffer-point! buffer start)
      (if (mark< start (buffer-end buffer))
	  (pop-up-buffer buffer false)
	  (message "(Shell Command completed with no output)")))))

(define (shell-command input-region output-mark directory command)
  (with-real-working-directory-pathname directory
    (lambda ()
      (let ((core
	     (lambda (input-port)
	       (run-subprocess command
			       input-port
			       (mark->output-port output-mark)))))
	(if input-region
	    (core (make-buffer-input-port (region-start input-region)
					  (region-end input-region)))
	    (call-with-input-file "\\dev\\nul" core))))))