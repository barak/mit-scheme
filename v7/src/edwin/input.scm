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

;;;; Keyboard Input

(declare (usual-integrations))
(using-syntax edwin-syntax-table

(define editor-input-port)

(define (set-editor-input-port! port)
  (set! editor-input-port port))

(define (with-editor-input-port new-port thunk)
  (fluid-let ((editor-input-port new-port))
    (thunk)))

(define (%keyboard-peek-char)
  (remap-alias-char (peek-char editor-input-port)))

(define (%keyboard-read-char)
  (let ((char (remap-alias-char (read-char editor-input-port))))
    (ring-push! (current-char-history) char)
    (if *defining-keyboard-macro?*
	(keyboard-macro-write-char char))
    char))

(define keyboard-active?
  (make-primitive-procedure 'TTY-READ-CHAR-READY?))

(define reset-command-prompt!)
(define command-prompt)
(define set-command-prompt!)

(define (append-command-prompt! string)
  (set-command-prompt! (string-append (command-prompt) string)))

(define message)
(define temporary-message)
(define append-message)
(define clear-message)

(define keyboard-read-char)
(define keyboard-peek-char)

(define keyboard-package
  (make-environment

#|

The interaction between command prompts and messages is complicated.
Here is a description of the state transition graph.

State variables:

a : there is a command prompt
b : the command prompt is displayed
c : there is a message
d : the message should be erased

Constraints:

b implies a
d implies c
b implies (not d)
c implies (not b)

Valid States:

abcd
0000 0 : idle state
0010 2 : message
0011 3 : temporary message
1000 8 : undisplayed command prompt
1010 A : message with undisplayed command prompt
1011 B : temporary message with undisplayed command prompt
1100 C : displayed command prompt

Transition operations:

0: reset-command-prompt
1: set-command-prompt
2: message
3: temporary-message
4: clear-message
5: timeout

Transition table:

  012345
0 082300
8 08230C
C *C230C	* is special -- see the code.
2 2A2302
3 3B2300
A 2AAB8C
B 3BAB8C

|#

(define command-prompt-string false)
(define command-prompt-displayed? false)
(define message-string false)
(define message-should-be-erased? false)

;;; Should only be called by the command reader.  This prevents
;;; carryover from one command to the next.
(set! reset-command-prompt!
(named-lambda (reset-command-prompt!)
  (set! command-prompt-string false)
  (if command-prompt-displayed?
      ;; To make it more visible, the command prompt is erased after
      ;; timeout instead of right away.
      (begin (set! command-prompt-displayed? false)
	     (set! message-should-be-erased? true)))))

(set! command-prompt
(named-lambda (command-prompt)
  (or command-prompt-string "")))

(set! set-command-prompt!
(named-lambda (set-command-prompt! string)
  (if (not (string-null? string))
      (begin (set! command-prompt-string string)
	     (if command-prompt-displayed?
		 ((access set-message! prompt-package) string))))))

(define ((message-writer temporary?) . args)
  (if command-prompt-displayed?
      (begin (set! command-prompt-string false)
	     (set! command-prompt-displayed? false)))
  (set! message-string (apply string-append args))
  (set! message-should-be-erased? temporary?)
  ((access set-message! prompt-package) message-string))

(set! message (message-writer false))
(set! temporary-message (message-writer true))

(set! append-message
(named-lambda (append-message . args)
  (if (not message-string)
      (error "Attempt to append to nonexistent message"))
  (set! message-string
	(string-append message-string
		       (apply string-append args)))
  ((access set-message! prompt-package) message-string)))

(set! clear-message
(named-lambda (clear-message)
  (set! command-prompt-string false)
  (set! command-prompt-displayed? false)
  (set! message-string false)
  (set! message-should-be-erased? false)
  ((access clear-message! prompt-package))))

(declare (compilable-primitive-functions
	  (keyboard-active? tty-read-char-ready?)))

(define ((keyboard-reader macro-read-char read-char))
  (if *executing-keyboard-macro?*
      (macro-read-char)
      (begin
       (if (not (keyboard-active? 0))
	   (begin (update-alpha-window! false)
		  (if (and (positive? (ref-variable "Auto Save Interval"))
			   (> *auto-save-keystroke-count*
			      (ref-variable "Auto Save Interval"))
			   (> *auto-save-keystroke-count* 20))
		      (begin (do-auto-save)
			     (set! *auto-save-keystroke-count* 0)))))
       (set! *auto-save-keystroke-count* (1+ *auto-save-keystroke-count*))
       (cond ((within-typein-edit?)
	      (if message-string
		  (begin (keyboard-active?
			  (if message-should-be-erased? 50 200))
			 (set! message-string false)
			 (set! message-should-be-erased? false)
			 ((access clear-message! prompt-package)))))
	     ((and (or message-should-be-erased?
		       (and command-prompt-string
			    (not command-prompt-displayed?)))
		   (not (keyboard-active? 50)))
	      (begin (set! message-string false)
		     (set! message-should-be-erased? false)
		     (if command-prompt-string
			 (begin (set! command-prompt-displayed? true)
				((access set-message! prompt-package)
				 command-prompt-string))
			 ((access clear-message! prompt-package))))))
       (read-char))))

(set! keyboard-read-char
      (keyboard-reader (lambda () (keyboard-macro-read-char))
		       %keyboard-read-char))

(set! keyboard-peek-char
      (keyboard-reader (lambda () (keyboard-macro-peek-char))
		       %keyboard-peek-char))

))

(define char-controlify)
(define char-metafy)
(define char-control-metafy)
(define char-base)
(let ()

(set! char-controlify
(named-lambda (char-controlify char)
  (make-char (char-code char)
	     (controlify (char-bits char)))))

(set! char-metafy
(named-lambda (char-metafy char)
  (make-char (char-code char)
	     (metafy (char-bits char)))))

(set! char-control-metafy
(named-lambda (char-control-metafy char)
  (make-char (char-code char)
	     (controlify (metafy (char-bits char))))))

(set! char-base
(named-lambda (char-base char)
  (make-char (char-code char) 0)))

(define (controlify i)
  (if (>= (remainder i #x2) #x1) i (+ #x1 i)))

(define (metafy i)
  (if (>= (remainder i #x4) #x2) i (+ #x2 i)))

)

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; Scheme Syntax Table: edwin-syntax-table
;;; Tags Table Pathname: (access edwin-tags-pathname edwin-package)
;;; End:
