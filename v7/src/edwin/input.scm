;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/input.scm,v 1.92 1992/02/17 22:09:14 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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

;;;; Keyboard Input

(declare (usual-integrations))

#|

The interaction between command prompts and messages is complicated.
Here is a description of the state transition graph.

State variables:

a : there is a command prompt
b : the command prompt is displayed
c : there is a message
d : the message should be erased (also implies it is displayed)

Constraints:

b implies a
d implies c
b implies (not d)
c implies (not b)

Valid States:

abcd  Hex  Description
0000  0  : idle state
0010  2  : message
0011  3  : temporary message
1000  8  : undisplayed command prompt
1010  A  : message with undisplayed command prompt
1011  B  : temporary message with undisplayed command prompt
1100  C  : displayed command prompt

Transition operations:

0: reset-command-prompt
1: set-command-prompt
2: message
3: temporary-message
4: clear-message
5: timeout

Transition table.  Each row is labeled with initial state, each column
with a transition operation.  Each element is the new state for the
given starting state and transition operation.

  012345
0 082300
8 08238C
C *C23CC	* is special -- see the code.
2 2A2302
3 3B2300
A 2AAB8C
B 3BAB8C

|#

(define command-prompt-string)
(define command-prompt-displayed?)
(define message-string)
(define message-should-be-erased?)
(define auto-save-keystroke-count)

(define (initialize-typeout!)
  (set! command-prompt-string false)
  (set! command-prompt-displayed? false)
  (set! message-string false)
  (set! message-should-be-erased? false)
  (set! auto-save-keystroke-count 0)
  unspecific)

(define (reset-command-prompt!)
  ;; Should only be called by the command reader.  This prevents
  ;; carryover from one command to the next.
  (set! command-prompt-string false)
  (if command-prompt-displayed?
      ;; To make it more visible, the command prompt is erased after
      ;; timeout instead of right away.
      (begin
	(set! command-prompt-displayed? false)
	(set! message-should-be-erased? true)))
  unspecific)

(define-integrable (command-prompt)
  (or command-prompt-string ""))

(define (set-command-prompt! string)
  (if (not (string-null? string))
      (begin
	(set! command-prompt-string string)
	(if command-prompt-displayed?
	    (set-current-message! string)))))

(define (append-command-prompt! string)
  (if (not (string-null? string))
      (set-command-prompt! (string-append (command-prompt) string))))

(define (message . args)
  (%message (message-args->string args) false))

(define (temporary-message . args)
  (%message (message-args->string args) true))

(define (%message string temporary?)
  (if command-prompt-displayed?
      (begin
	(set! command-prompt-string false)
	(set! command-prompt-displayed? false)))
  (set! message-string string)
  (set! message-should-be-erased? temporary?)
  (set-current-message! string))

(define (message-args->string args)
  (apply string-append
	 (map (lambda (x) (if (string? x) x (write-to-string x)))
	      args)))

(define (append-message . args)
  (if (not message-string)
      (error "Attempt to append to nonexistent message"))
  (let ((string (string-append message-string (message-args->string args))))
    (set! message-string string)
    (set-current-message! string)))

(define (clear-message)
  (if message-string
      (begin
	(set! message-string false)
	(set! message-should-be-erased? false)
	(if (not command-prompt-displayed?)
	    (clear-current-message!)))))

(define (keyboard-peek)
  (if *executing-keyboard-macro?*
      (keyboard-macro-peek-key)
      (keyboard-read-1 (editor-peek current-editor))))

(define (keyboard-read)
  (set! keyboard-keys-read (1+ keyboard-keys-read))
  (if *executing-keyboard-macro?*
      (keyboard-macro-read-key)
      (let ((key (keyboard-read-1 (editor-read current-editor))))
	(set! auto-save-keystroke-count (fix:+ auto-save-keystroke-count 1))
	(ring-push! (current-char-history) key)
	(if *defining-keyboard-macro?* (keyboard-macro-write-key key))
	key)))

(define (keyboard-peek-no-hang)
  ((editor-peek-no-hang current-editor)))

(define (keyboard-read-char)
  (let loop ((key (keyboard-read)))
    (if (char? key)
	key
	(loop (keyboard-read)))))

(define read-key-timeout/fast 500)
(define read-key-timeout/slow 2000)

(define (keyboard-read-1 reader)
  (remap-alias-key
   (let ((peek-no-hang (editor-peek-no-hang current-editor)))
     (if (not (peek-no-hang))
	 (begin
	   (if (let ((interval (ref-variable auto-save-interval))
		     (count auto-save-keystroke-count))
		 (and (fix:> count 20)
		      (> interval 0)
		      (> count interval)))
	       (begin
		 (do-auto-save)
		 (set! auto-save-keystroke-count 0)))
	   (update-screens! false)))
     (let ((wait
	    (lambda (timeout)
	      (let ((t (+ (real-time-clock) timeout)))
		(let loop ()
		  (cond ((peek-no-hang) false)
			((>= (real-time-clock) t) true)
			(else (loop))))))))
       ;; Perform the appropriate juggling of the minibuffer message.
       (cond ((within-typein-edit?)
	      (if message-string
		  (begin
		    (wait read-key-timeout/slow)
		    (set! message-string false)
		    (set! message-should-be-erased? false)
		    (clear-current-message!))))
	     ((and (or message-should-be-erased?
		       (and command-prompt-string
			    (not command-prompt-displayed?)))
		   (wait read-key-timeout/fast))
	      (set! message-string false)
	      (set! message-should-be-erased? false)
	      (if command-prompt-string
		  (begin
		    (set! command-prompt-displayed? true)
		    (set-current-message! command-prompt-string))
		  (clear-current-message!)))))
     (reader))))