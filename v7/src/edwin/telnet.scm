#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/telnet.scm,v 1.2 1991/10/02 09:18:48 cph Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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

  (set-variable! comint-prompt-regexp
		 (or (ref-variable telnet-prompt-pattern)
		     (ref-variable shell-prompt-pattern)))
  (let ((process (current-process)))
    (if process
	(set-process-filter! process
			     (make-telnet-filter process))))
  (event-distributor/invoke! (ref-variable telnet-mode-hook)))

(define-key 'telnet #\C-m 'telnet-send-input)
(define-key 'telnet '(#\C-c #\C-c) 'telnet-self-send)
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
With a prefix argument, it unconditionally creates a new telnet connection."
  "sTelnet to Host\nP"
  (lambda (host #!optional arg)
    (select-buffer
     (make-comint (ref-mode-object telnet)
		  (let ((default (string-append host "-telnet")))
		    (if (or (default-object? arg)
			    (not arg))
			default
			(list default)))
		  "telnet"
		  host))))

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
  (lambda ()
    (process-send-char (current-process)
		       (last-command-key))))

(define-command telnet-send-character
  "Reads a character and sends it to the telnet process."
  "p"
  (lambda (argument)
    (let ((char (read-quoted-char "Send Character: "))
	  (process (current-process)))
      (if (= argument 1)
	  (process-send-char process char)
	  (process-send-string process
			       (make-string argument char))))))


(define (telnet-filter-substring string start end)
  (substring-substitute string start end
			(ref-variable telnet-replacee)
			(ref-variable telnet-replacement)))

(define-variable telnet-replacee
  "String to replace in telnet output."
  (string #\return))

(define-variable telnet-replacement
  "String to use as replacement in telnet output."
  "")

(define (make-telnet-filter process)
  (lambda (string start end)
    (let ((mark (process-mark process)))
      (and mark
	   (let ((index (mark-index mark))
		 (new-string (telnet-filter-substring string start end)))
	     (let ((new-length (string-length new-string)))
	       (group-insert-substring! (mark-group mark)
					index new-string 0 new-length)
	       (set-mark-index! mark (+ index new-length))
	       true))))))

(define (substring-substitute string start end source target)
  (let ((length (fix:- end start))
	(slength (string-length source))
	(tlength (string-length target)))
    (if (fix:zero? slength)
	(error "substring-replace: Empty source" source))
    (let ((alloc-length
	   (fix:+ length
		  (fix:* (fix:quotient length slength)
			 tlength)))
	  (char (string-ref source 0)))
      (let ((result (string-allocate alloc-length)))

	(define (done copy-index write-index)
	  (if (fix:< copy-index end)
	      (substring-move-right! string copy-index end
				     result write-index))
	  (set-string-length! result
			      (fix:+ write-index
				     (fix:- end copy-index)))
	  result)

	(define (loop copy-index read-index write-index)
	  (if (fix:>= read-index end)
	      (done copy-index write-index)
	      (let ((index (substring-find-next-char string
						     read-index end
						     char)))
		(cond ((not index)
		       (done copy-index write-index))
		      ((or (fix:= slength 1)
			   (substring-prefix? source 0 slength string
					      index end))
		       (substring-move-right! string copy-index index
					      result write-index)
		       (let ((next-write
			      (fix:+ write-index (fix:- index copy-index)))
			     (next-read (fix:+ index slength)))
			 (if (not (fix:= tlength 0))
			     (substring-move-right! target 0 tlength
						    result next-write))
			 (loop next-read
			       next-read
			       (fix:+ next-write tlength))))
		      (else
		       (loop copy-index (fix:+ index 1) write-index))))))

	(loop start start 0)))))