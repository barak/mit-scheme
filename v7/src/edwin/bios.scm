#| -*-Scheme-*-

$Id: bios.scm,v 1.2 1992/10/17 23:14:22 jinx Exp $

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

;;;; IBM-PC BIOS Screen Implementation
;;; package: (edwin console-screen)

(declare (usual-integrations))

(define (make-bios-screen)
  ;; What is the baud rate needed for?  It's not even meaningful.
  (let ((baud-rate (output-port/baud-rate console-output-port))
	(x-size (output-port/x-size console-output-port))
	(y-size (output-port/y-size console-output-port)))
    (make-screen (cons (fix:-1+ y-size) (fix:-1+ x-size))
		 bios-console-beep
		 bios-console-clear-line!
		 bios-console-clear-rectangle!
		 bios-console-clear-screen!
		 bios-console-discard!
		 bios-console-enter!
		 bios-console-exit!
		 bios-console-flush!
		 bios-console-modeline-event!
		 bios-console-discretionary-flush
		 bios-console-scroll-lines-down!
		 bios-console-scroll-lines-up!
		 bios-console-wrap-update!
		 bios-console-write-char!
		 bios-console-write-cursor!
		 bios-console-write-substring!
		 (fix:1+ (fix:quotient baud-rate 2400))
		 x-size
		 y-size)))

(define (bios-available?)
  (and (implemented-primitive-procedure? bios:can-use-bios?)
       (bios:can-use-bios?)
       (let ((term (get-environment-variable "TERM")))
	 (and term
	      (string-ci=? term "ibm_pc_bios")))))

(define bios-display-type)

(define (bios-initialize-package!)
  (set! bios-display-type
	(make-display-type 'IBM-PC-BIOS
			   false
			   bios-available?
			   make-bios-screen
			   (lambda (screen)
			     screen
			     (get-console-input-operations))
			   with-console-grabbed
			   with-console-interrupts-enabled
			   with-console-interrupts-disabled))
  unspecific)

;;;; Customized IBM-PC BIOS console operations

(define-primitives
  (bios:beep 0)
  (bios:can-use-bios? 0)
  (bios:clear-line! 3)
  (bios:clear-rectangle! 5)
  (bios:clear-screen! 0)
  (bios:exit! 0)
  (bios:initialize! 2)
  (bios:scroll-lines-up! 5)
  (bios:scroll-lines-down! 5)
  (bios:write-char! 2)
  (bios:write-cursor! 2)
  (bios:write-substring! 4))

(define (bios-console-discard! screen)
  screen
  unspecific)

(define (bios-console-enter! screen)
  (define (default-attribute variable default)
    (let ((val (get-environment-variable variable)))
      (cond ((not val) default)
	    ((string? val) (string->number val))
	    (else val))))

  (bios:initialize!
   (default-attribute "EDWIN_FOREGROUND" 37)	; white foreground
   (default-attribute "EDWIN_BACKGROUND" 40))	; black background
  unspecific)

(define (bios-console-exit! screen)
  (bios:exit!)
  (bios-move-cursor screen 0 (fix:-1+ (screen-y-size screen))))

(define (bios-console-modeline-event! screen window type)
  screen window type
  unspecific)

(define (bios-console-wrap-update! screen thunk)
  screen
  (thunk))

(define (bios-console-discretionary-flush screen)
  screen
  unspecific)

(define (bios-console-beep screen)
  screen
  (bios:beep))

(define (bios-console-flush! screen)
  screen
  unspecific)

(define-integrable (bios-move-cursor screen x y)
  screen
  (bios:write-cursor! x y))

(define (bios-console-write-cursor! screen x y)
  (bios-move-cursor screen x y))

(define (bios-console-write-char! screen x y char highlight)
  (if (not (and (fix:= y (car (screen-state screen)))
		(fix:= x (cdr (screen-state screen)))))
      (begin
	(bios-move-cursor screen x y)
	(bios:write-char! char highlight))))

(define (bios-console-write-substring! screen x y string start end highlight)
  (let ((end
	 (let ((delta (fix:- end start)))
	   (if (and (fix:= y (car (screen-state screen)))
		    (fix:= (fix:+ x delta)
			   (screen-x-size screen)))
	       (fix:-1+ end)
	       end))))
    (if (fix:< start end)
	(begin
	  (bios-move-cursor screen x y)
	  (bios:write-substring! string start end highlight)))))

(define (bios-console-clear-line! screen x y first-unused-x)
  (bios:clear-line! x y (fix:-1+ first-unused-x))
  (bios-move-cursor screen x y))

(define (bios-console-clear-screen! screen)
  (bios:clear-screen!)
  (bios-move-cursor screen 0 0))

(define (bios-console-clear-rectangle! screen xl xu yl yu highlight)
  screen
  (bios:clear-rectangle! xl xu yl yu highlight))

(define (bios-console-scroll-lines-down! screen xl xu yl yu amount)
  screen
  (bios:scroll-lines-down! xl (fix:-1+ xu) yl (fix:-1+ yu) amount)
  'CLEARED)

(define (bios-console-scroll-lines-up! screen xl xu yl yu amount)
  screen
  (bios:scroll-lines-up! xl (fix:-1+ xu) yl (fix:-1+ yu) amount)
  'CLEARED)