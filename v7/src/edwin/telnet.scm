#| -*-Scheme-*-

$Id: telnet.scm,v 1.13 1997/11/20 05:51:14 cph Exp $

Copyright (c) 1991-97 Massachusetts Institute of Technology

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
|#

;;;; Run Telnet in a buffer

(declare (usual-integrations))

(define-variable telnet-prompt-pattern
  "#f or Regexp to match prompts in telnet buffers."
  #f)				    

(define-major-mode telnet comint "Telnet"
  "Major mode for interacting with the telnet program.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.

Customization: Entry to this mode runs the hooks on comint-mode-hook
and telnet-mode-hook, in that order."
  (lambda (buffer)
    (define-variable-local-value! buffer
	(ref-variable-object comint-prompt-regexp)
      (or (ref-variable telnet-prompt-pattern buffer)
	  (ref-variable shell-prompt-pattern buffer)))
    (event-distributor/invoke! (ref-variable telnet-mode-hook buffer) buffer)))

(define-key 'telnet #\C-m 'telnet-send-input)
(define-key 'telnet '(#\C-c #\C-c) 'telnet-self-send)
(define-key 'telnet '(#\C-c #\C-d) 'telnet-self-send)
(define-key 'telnet '(#\C-c #\C-g) 'telnet-self-send)
(define-key 'telnet '(#\C-c #\C-q) 'telnet-send-character)
(define-key 'telnet '(#\C-c #\C-z) 'telnet-self-send)
(define-key 'telnet '(#\C-c #\C-\\) 'telnet-self-send)

;;;moved to "loadef.scm".
;;;(define-variable telnet-mode-hook
;;;  "An event distributor that is invoked when entering Telnet mode."
;;;  (make-event-distributor))

(define-command telnet
  "Run telnet in a buffer.
With a prefix argument, it unconditionally creates a new telnet connection.
If port number is typed after hostname (separated by a space),
use it instead of the default."
  "sTelnet to host\nP"
  (lambda (host new-process?)
    (select-buffer
     (let ((mode (ref-mode-object telnet))
	   (buffer-name
	     (let ((buffer-name (string-append "*" host "-telnet*")))
	       (if (not new-process?)
		   buffer-name
		   (new-buffer buffer-name)))))
       (if (re-string-match "\\([^ ]+\\) \\([^ ]+\\)" host)
	   (let ((host
		  (substring host
			     (re-match-start-index 1)
			     (re-match-end-index 1)))
		 (port
		  (substring host
			     (re-match-start-index 2)
			     (re-match-end-index 2))))
	     (if (not (exact-nonnegative-integer? (string->number port)))
		 (editor-error "Port must be a positive integer: " port))
	     (make-comint mode buffer-name "telnet" host port))
	   (make-comint mode buffer-name "telnet" host))))))

(add-event-receiver! (ref-variable telnet-mode-hook)
		     comint-strip-carriage-returns)

(define-command telnet-send-input
  "Send input to telnet process.
The input is entered in the history ring."
  ()
  (lambda () (comint-send-input "\n" true)))

(define-command telnet-self-send
  "Immediately send the last command key to the telnet process.
Typically bound to C-c <char> where char is an interrupt key for the process
running remotely."
  ()
  (lambda () (process-send-char (current-process) (last-command-key))))

(define-command telnet-send-character
  "Read a character and send it to the telnet process.
With prefix arg, the character is repeated that many times."
  "p"
  (lambda (argument)
    (let ((char (read-quoted-char "Send Character: "))
	  (process (current-process)))
      (cond ((= argument 1)
	     (process-send-char process char))
	    ((> argument 1)
	     (process-send-string process (make-string argument char)))))))