#| -*-Scheme-*-

$Id: webster.scm,v 1.1 1998/08/31 04:15:00 cph Exp $

Copyright (c) 1998 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy and modify this software, to
redistribute either the original software or a modified version, and
to use this software for any purpose is granted, subject to the
following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
   in full.

2. Users of this software agree to make their best efforts (a) to
   return to the MIT Scheme project any improvements or extensions
   that they make, so that these may be included in future releases;
   and (b) to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
   software shall duly acknowledge such use, in accordance with the
   usual standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
   this software will be error-free, and MIT is under no obligation to
   provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
   there shall be no use of the name of the Massachusetts Institute of
   Technology nor of any adaptation thereof in any advertising,
   promotional, or sales literature without prior written consent from
   MIT in each case.

NOTE: Parts of this program (Edwin) were created by translation
from corresponding parts of GNU Emacs.  Users should be aware that
the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
of that license should have been included along with this file.
|#

;;;; Webster Server Interface

;;; Translated from Noah Friedman's Emacs implementation:
;;; webster.el,v 1.2 1995/01/04 00:41:51

(declare (usual-integrations))

(define (webster-send request word)
  (guarantee-webster-server-port (selected-buffer))
  (write-string request webster-server-port)
  (write-string " " webster-server-port)
  (write-string word webster-server-port)
  (newline webster-server-port)
  (flush-output webster-server-port)
  (let ((line (read-line webster-server-port)))
    (cond ((string=? "SPELLING 0" line)
	   (message "Word not found."))
	  ((string=? "SPELLING 1" line)
	   (message "Word spelled correctly."))
	  ((string=? "MATCHS 0" line)
	   (message "No endings for this word."))
	  ((or (string=? "SPELLING" line)
	       (string=? "MATCHS" line)
	       (string-prefix? "DEFINITION " line))
	   (let loop ((lines '()))
	     (call-with-values
		 (lambda () (webster-read-line webster-server-port))
	       (lambda (line end?)
		 (cond ((not end?)
			(loop (cons line lines)))
		       ((null? lines)
			(message line))
		       (else
			 (webster-show-output
			  (reverse! (cons line lines)))))))))
	  (else
	   (error "Unrecognized response from Webster server:" line)))))

(define (webster-read-line port)
  (let ((line (read-string webster-line-delimiters port)))
    (values line
	    (let ((delim (read-char port)))
	      (or (eof-object? delim)
		  (not (char=? #\newline delim)))))))

(define webster-line-delimiters
  (char-set #\newline (integer->char 0) (integer->char #o200)))

(define webster-server-port #f)

(define (guarantee-webster-server-port buffer)
  (if (or (not webster-server-port)
	  (input-port/eof? webster-server-port))
      (let ((server
	     (or (ref-variable webster-server buffer)
		 (editor-error "Variable webster-server not set."))))
	(message "Opening webster connection to " server "...")
	(set! webster-server-port
	      (open-tcp-stream-socket server
				      (ref-variable webster-port buffer)
				      4096))
	(append-message "done")
	(global-window-modeline-event!
	 (lambda (window) window 'WEBSTER-CONNECTION-STATUS)))))

(define (input-port/eof? port)
  ((port/operation port 'EOF?) port))

(define (close-webster-server-port)
  (let ((port webster-server-port))
    (set! webster-server-port #f)
    (if port (close-port port)))
  (global-window-modeline-event!
   (lambda (window) window 'WEBSTER-CONNECTION-STATUS)))

(define (webster-show-output lines)
  (let ((buffer (find-or-create-buffer (ref-variable webster-buffer-name))))
    (set-buffer-major-mode! buffer (ref-mode-object webster))
    (add-kill-buffer-hook buffer webster-kill-buffer-hook)
    (if (not (fix:= 0 (buffer-length buffer)))
	(guarantee-newlines 2 (buffer-end buffer)))
    (let ((m (mark-right-inserting-copy (buffer-end buffer)))
	  (p (mark-left-inserting-copy (buffer-end buffer))))
      (for-each (lambda (line)
		  (insert-string line p)
		  (insert-newline p))
		lines)
      (mark-temporary! p)
      (let ((window
	     (let ((windows (buffer-windows buffer)))
	       (if (null? windows)
		   (begin
		     (pop-up-buffer buffer #f #f)
		     (car (buffer-windows buffer)))
		   (car windows)))))
	(set-window-point! window m)
	(set-window-start-mark! window m #t))
      (mark-temporary! m))))

(define (webster-kill-buffer-hook buffer)
  buffer
  (close-webster-server-port))

(define-major-mode webster read-only "Webster"
  "Major mode for interacting with webster server.
Commands:

\\[webster-define]	look up the definition of a word
\\[webster-spellings]	look up possible correct spellings for a word
\\[webster-define]	look up possible endings for a word
\\[webster-quit]	close connection to the Webster server

Use webster-mode-hook for customization."
  (lambda (buffer)
    (local-set-variable!
     mode-line-process
     (lambda (window)
       (if (and webster-server-port
		(not (input-port/eof? webster-server-port)))
	   ": connected"
	   ": disconnected"))
     buffer)
    (event-distributor/invoke! (ref-variable webster-mode-hook buffer)
			       buffer)))

(define-key 'webster #\? 'describe-mode)
(define-key 'webster #\d 'webster-define)
(define-key 'webster #\e 'webster-endings)
(define-key 'webster #\h 'describe-mode)
(define-key 'webster #\q 'webster-quit)
(define-key 'webster #\s 'webster-spellings)

(define (webster-prompt prompt)
  (lambda ()
    (list (prompt-for-string prompt (webster-current-word)))))

(define (webster-current-word)
  (let* ((p (current-point))
	 (s (backward-word p 1 'LIMIT))
	 (e (forward-word s 1 'LIMIT)))
    (if (mark>= e p)
	(extract-string s e)
	(let* ((e* (forward-word p 1 'LIMIT))
	       (s* (backward-word e* 1 'LIMIT)))
	  (if (mark<= s* p)
	      (extract-string s* e*)
	      #f)))))

(define-command webster-define
  "Look up a word in Webster's dictionary."
  (webster-prompt "Look up word")
  (lambda (word) (webster-send "DEFINE" word)))
(copy-command 'webster (ref-command-object webster-define))

(define-command webster-endings
  "Look up possible endings for a word in Webster's dictionary."
  (webster-prompt "Find endings for word")
  (lambda (word) (webster-send "ENDINGS" word)))

(define-command webster-spellings
  "Look up possible correct spellings for a word in Webster's dictionary."
  (webster-prompt "Possible correct spellings for word")
  (lambda (word) (webster-send "SPELL" word)))

(define-command webster-quit
  "Close connection to webster server.
Buffer is not deleted."
  ()
  (lambda () (close-webster-server-port)))