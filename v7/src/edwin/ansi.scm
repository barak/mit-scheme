#| -*-Scheme-*-

$Id: ansi.scm,v 1.6 1994/11/01 23:04:22 adams Exp $

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
MIT in each case. |#

;;;; Hard-coded ANSI terminal type for lack of termcap on DOS/NT

(declare (usual-integrations))

(define (make-ansi-terminal-description columns lines)
  (define (get-numstring base-name)
    (or (get-environment-variable (string-append "EDWIN_" base-name))
	(get-environment-variable base-name)))

  (define (valid-mode? color-string)
    (and (string? color-string)
	 (= (string-length color-string) 2)
	 (member (string-ref color-string 0)
		 '(#\3 #\4))
	 (member (string-ref color-string 1)
		 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))))

  (define (invert-mode color-string)
    (make-mode (string (if (eqv? (string-ref color-string 0) #\3)
			   #\4
			   #\3)
		       (string-ref color-string 1))))

  (define (make-mode color-string)
    (or (and (valid-mode? color-string)
	     (string-append ";" color-string))
	""))

  (let ((foregnd (get-numstring "FOREGROUND"))
	(backgnd (get-numstring "BACKGROUND")))
    (let ((full? (not (eq? (intern microcode-id/operating-system-name) "dos")))
	  (normal
	   (string-append "\033[0"
			  (make-mode foregnd)
			  (make-mode backgnd)
			  "m"))
	  (standout
	   (if (or (not (valid-mode? foregnd)) (not (valid-mode? backgnd)))
	       (string-append "\033[7"
			      (make-mode foregnd)
			      (make-mode backgnd)
			      "m")
	       (string-append "\033[0"
			      (invert-mode backgnd)
			      (invert-mode foregnd)
			      "m"))))

      (%make-termcap-description
       "ansi.sys"		        ; terminal-type-name
       false				; delete-is-insert-mode?
       false				; enter/exit-standout-mode-same?
       (and full? "\033[P")		; insert/delete-char-ok?
       (and full? "\033[M")		; insert/delete-line-ok?
       false				; scroll-region-ok?
       #t				; tf-automatic-wrap
       false				; tf-cursor-backwards-wrap
       false				; tf-generic
       false				; tf-hardcopy
       false				; tf-hazeltine
       false				; tf-insert-mode-motion
       false				; tf-lose-wrap
       false				; tf-magic-wrap
       false				; tf-memory-above-screen
       false				; tf-memory-below-screen
       false				; tf-meta-key
       false				; tf-must-write-spaces
       false				; tf-newline-doesnt-scroll
       false				; tf-overstrike
       false				; tf-overstrike-space-erase
       false				; tf-overwrite-preserves-standout
       false				; tf-standout-mode-motion
       false				; tf-teleray
       false				; tf-underscore
       false				; tn-memory-lines
       false				; tn-minimum-padding-speed
       false				; tn-standout-marker-width
       columns				; tn-x-size
       lines				; tn-y-size
       "\a"				; ts-audible-bell
       "\033[K"				; ts-clear-line
       false				; ts-clear-multi-char
       "\033[H\033[J"			; ts-clear-screen
       "\033[J"				; ts-clear-to-bottom
       "\n"				; ts-cursor-down
       false				; ts-cursor-down-multi
       "\b"				; ts-cursor-left
       false				; ts-cursor-left-multi
       "\r"				; ts-cursor-line-start
       false				; ts-cursor-lower-left
       "\033[%i%d;%dH"			; ts-cursor-move
       false				; ts-cursor-move-x
       "\033[C"				; ts-cursor-right
       (and full? "\033[%dC")		; ts-cursor-right-multi
       "\033[A"				; ts-cursor-up
       (and full? "\033[%dA")		; ts-cursor-up-multi
       "\033[H"				; ts-cursor-upper-left
       (and full? "\033[P")		; ts-delete-char
       (and full? "\033[M")		; ts-delete-line
       (and full? "\033[%dP")		; ts-delete-multi-char
       (and full? "\033[%dM")		; ts-delete-multi-line
       false				; ts-enhance-cursor
       false				; ts-enter-delete-mode
       false ;"\033[4h"			; ts-enter-insert-mode
       standout;"\033[7m"		; ts-enter-standout-mode
       (and full? "\033[1p")		; ts-enter-termcap-mode
       false				; ts-exit-delete-mode
       false ;"\033[4l"			; ts-exit-insert-mode
       normal;"\033[0m"			; ts-exit-standout-mode
       (and full? "\033[0p")		; ts-exit-termcap-mode
       "\n"				; ts-forward-scroll
       false				; ts-forward-scroll-multi
       (and full? "\033[@")		; ts-insert-char
       (and full? "\033[L")		; ts-insert-line
       (and full? "\033[%d@")		; ts-insert-multi-char
       (and full? "\033[%dL")		; ts-insert-multi-line
       false				; ts-invisible-cursor
       false				; ts-normal-cursor
       false				; ts-pad-char
       false				; ts-pad-inserted-char
       false				; ts-reverse-scroll
       false				; ts-reverse-scroll-multi
       false				; ts-set-scroll-region
       false				; ts-set-scroll-region-1
       false				; ts-set-window
       false				; ts-visible-bell
       '()                              ; termcap-description-keys
       ))))