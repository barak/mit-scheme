#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/ansi.scm,v 1.1 1992/04/22 21:20:37 mhwu Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; Hard-coded ANSI terminal type for lack of termcap on DOS

(declare (usual-integrations))

(define (make-ansi-terminal-description columns lines)
  (define (defined? sym)
    (not (lexical-unreferenceable? (the-environment) sym)))
  (define (get-numstring scheme shell)
    (or (and (defined? scheme)
	     (exact-nonnegative-integer?
	      (lexical-reference (the-environment) scheme))
	     (number->string
	      (lexical-reference (the-environment) scheme)))
	(get-environment-variable shell)))
  (define (make-mode color-string)
    (or (and (string? color-string)
	     (string->number color-string)
	     (string-append ";" color-string))
	""))
  (let ((foregnd (get-numstring 'edwin:foreground-color "FOREGROUND"))
	(backgnd (get-numstring 'edwin:background-color "BACKGROUND")))
    (let ((standout
	   (string-append "\033[7" (make-mode foregnd) (make-mode backgnd) "m"))
	  (normal
	   (string-append "\033[0" (make-mode foregnd) (make-mode backgnd) "m")))
      (%make-termcap-description
       "ansi.sys"		        ; terminal-type-name
       false				; delete-is-insert-mode?
       false				; enter/exit-standout-mode-same?
       false ;"\033[P"			; insert/delete-char-ok?
       false ;"\033[M"			; insert/delete-line-ok?
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
       "\a"					; ts-audible-bell
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
       false				; ts-cursor-right-multi
       "\033[A"				; ts-cursor-up
       false				; ts-cursor-up-multi
       "\033[H"				; ts-cursor-upper-left
       false ;"\033[P"			; ts-delete-char
       false ;"\033[M"			; ts-delete-line
       false				; ts-delete-multi-char
       false				; ts-delete-multi-line
       false				; ts-enhance-cursor
       false				; ts-enter-delete-mode
       false ;"\033[4h"			; ts-enter-insert-mode
       standout;"\033[7m"		; ts-enter-standout-mode
       false				; ts-enter-termcap-mode
       false				; ts-exit-delete-mode
       false ;"\033[4l"			; ts-exit-insert-mode
       normal;"\033[0m"			; ts-exit-standout-mode
       false				; ts-exit-termcap-mode
       "\n"					; ts-forward-scroll
       false				; ts-forward-scroll-multi
       false ;"\033[@"			; ts-insert-char
       false ;"\033[L"			; ts-insert-line
       false ;"\033[%d@"			; ts-insert-multi-char
       false				; ts-insert-multi-line
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
       ))))


