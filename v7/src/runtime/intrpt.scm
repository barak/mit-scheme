;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/intrpt.scm,v 13.42 1987/02/15 15:43:59 cph Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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

;;;; Interrupt System

(declare (usual-integrations)
	 (integrate-primitive-procedures set-fixed-objects-vector!))

(define with-external-interrupts-handler)

(define timer-interrupt
  (let ((setup-timer-interrupt
	 (make-primitive-procedure 'setup-timer-interrupt #T)))
    (named-lambda (timer-interrupt)
      (setup-timer-interrupt '() '())
      (error "Unhandled Timer interrupt received"))))

(define interrupt-system
  (make-package interrupt-system
		((get-next-interrupt-character
		  (make-primitive-procedure 'GET-NEXT-INTERRUPT-CHARACTER))
		 (check-and-clean-up-input-channel
		  (make-primitive-procedure 'CHECK-AND-CLEAN-UP-INPUT-CHANNEL))
		 (index:interrupt-vector
		  (fixed-objects-vector-slot 'SYSTEM-INTERRUPT-VECTOR))
		 (index:termination-vector
		  (fixed-objects-vector-slot
		   'MICROCODE-TERMINATIONS-PROCEDURES))
		 (^Q-Hook '()))

;;;; Soft interrupts

;;; Timer interrupts

(define (timer-interrupt-handler interrupt-code interrupt-enables)
  (timer-interrupt))

;;; Keyboard Interrupts

(define (external-interrupt-handler interrupt-code interrupt-enables)
  (let ((interrupt-character (get-next-interrupt-character)))
    ((vector-ref keyboard-interrupts interrupt-character) interrupt-character
							  interrupt-enables)))

(define (losing-keyboard-interrupt interrupt-character interrupt-enables)
  (error "Bad interrupt character" interrupt-character))

(define keyboard-interrupts
  (vector-cons 256 losing-keyboard-interrupt))

(define (install-keyboard-interrupt! interrupt-char handler)
  (vector-set! keyboard-interrupts
	       (char->ascii interrupt-char)
	       handler))

(define (remove-keyboard-interrupt! interrupt-char)
  (vector-set! keyboard-interrupts
	       (char->ascii interrupt-char)
	       losing-keyboard-interrupt))

(define until-most-recent-interrupt-character 0)	;for Pascal, ugh!
(define multiple-copies-only 1)

(define ((flush-typeahead kernel) interrupt-character interrupt-enables)
  (if (check-and-clean-up-input-channel until-most-recent-interrupt-character
					interrupt-character)
      (kernel interrupt-character interrupt-enables)))

(define ((keep-typeahead kernel) interrupt-character interrupt-enables)
  (if (check-and-clean-up-input-channel multiple-copies-only
					interrupt-character)
      (kernel interrupt-character interrupt-enables)))

(define ^B-interrupt-handler
  (keep-typeahead
   (lambda (interrupt-character interrupt-enables)
     (with-standard-proceed-point
      (lambda ()
	(breakpoint "^B interrupt" (rep-environment)))))))

; (define ^S-interrupt-handler
;   (keep-typeahead
;    (lambda (interrupt-character interrupt-enables)
;      (if (null? ^Q-Hook)
; 	 (begin (set-interrupt-enables! interrupt-enables)
; 		(beep)
; 		(call-with-current-continuation
; 		 (lambda (stop-^S-wait)
; 		   (fluid-let ((^Q-Hook Stop-^S-Wait))
; 		     (let busy-wait () (busy-wait))))))))))
; 
; (define ^Q-interrupt-handler
;   (keep-typeahead
;    (lambda (interrupt-character interrupt-enables)
;      (if (not (null? ^Q-Hook))
; 	 (begin (set-interrupt-enables! interrupt-enables)
; 		(^Q-Hook 'GO-ON))))))
; 
; (define ^P-interrupt-handler
;   (flush-typeahead
;    (lambda (interrupt-character interrupt-enables)
;      (set-interrupt-enables! interrupt-enables)
;      (proceed))))
; 
; (define ^Z-interrupt-handler
;   (flush-typeahead
;    (lambda (interrupt-character interrupt-enables)
;      (set-interrupt-enables! interrupt-enables)
;      (edit))))

(define ^G-interrupt-handler
  (flush-typeahead
   (lambda (interrupt-character interrupt-enables)
     (abort-to-top-level-driver "Quit!"))))

(define ^U-interrupt-handler
  (flush-typeahead
   (lambda (interrupt-character interrupt-enables)
     (abort-to-previous-driver "Up!"))))

(define ^X-interrupt-handler
  (flush-typeahead
   (lambda (interrupt-character interrupt-enables)
     (abort-to-nearest-driver "Abort!"))))

(define (gc-out-of-space-handler . args)
  (abort-to-nearest-driver "Aborting! Out of memory"))

(install-keyboard-interrupt! #\G ^G-interrupt-handler)
(install-keyboard-interrupt! #\B ^B-interrupt-handler)
; (install-keyboard-interrupt! #\P ^P-interrupt-handler)
(install-keyboard-interrupt! #\U ^U-interrupt-handler)
(install-keyboard-interrupt! #\X ^X-interrupt-handler)
; (install-keyboard-interrupt! #\Z ^Z-interrupt-handler)
; (install-keyboard-interrupt! #\S ^S-interrupt-handler)
; (install-keyboard-interrupt! #\Q ^Q-interrupt-handler)

(define STACK-OVERFLOW-SLOT	0)
(define GC-SLOT			2)
(define CHARACTER-SLOT		4)
(define TIMER-SLOT		6)

(define (install)
  (with-interrupts-reduced INTERRUPT-MASK-GC-OK
   (lambda (old-mask)
     (let ((old-system-interrupt-vector
	    (vector-ref (get-fixed-objects-vector) index:interrupt-vector))
	   (old-termination-vector
	    (vector-ref (get-fixed-objects-vector) index:termination-vector)))
       (let ((previous-gc-interrupt
	      (vector-ref old-system-interrupt-vector GC-SLOT))
	     (previous-stack-interrupt
	      (vector-ref old-system-interrupt-vector STACK-OVERFLOW-SLOT))
	     (system-interrupt-vector
	      (vector-cons (vector-length old-system-interrupt-vector)
			   default-interrupt-handler))
	     (termination-vector
	      (if old-termination-vector
		  (if (> number-of-microcode-terminations
			 (vector-length old-termination-vector))
		      (vector-grow old-termination-vector
				   number-of-microcode-terminations)
		      old-termination-vector)
		  (vector-cons number-of-microcode-terminations #F))))

	 (vector-set! system-interrupt-vector GC-SLOT previous-gc-interrupt)
	 (vector-set! system-interrupt-vector STACK-OVERFLOW-SLOT
		      previous-stack-interrupt)
	 (vector-set! system-interrupt-vector CHARACTER-SLOT
		      external-interrupt-handler)
	 (vector-set! system-interrupt-vector TIMER-SLOT
		      timer-interrupt-handler)

	 ;; slots 4-15 unused.
	 
		  ;; install the new vector atomically
	 (vector-set! (get-fixed-objects-vector)
		      index:interrupt-vector
		      system-interrupt-vector)

	 (vector-set! termination-vector
		      (microcode-termination 'GC-OUT-OF-SPACE)
		      gc-out-of-space-handler)

	 (vector-set! (get-fixed-objects-vector)
		      index:termination-vector
		      termination-vector)

	 (set-fixed-objects-vector! (get-fixed-objects-vector)))))))

(define (default-interrupt-handler interrupt-code interrupt-enables)
  (write-string "Anomalous Interrupt: ") (write interrupt-code)
  (write-string " Mask: ") (write interrupt-enables))

(set! with-external-interrupts-handler
(named-lambda (with-external-interrupts-handler handler code)
  (define (interrupt-routine interrupt-code interrupt-enables)
    (let ((character (get-next-interrupt-character)))
      (check-and-clean-up-input-channel
       until-most-recent-interrupt-character
       character)
      (handler character interrupt-enables)))

  (define old-handler interrupt-routine)

  (define interrupt-vector
    (vector-ref (get-fixed-objects-vector) index:interrupt-vector))

  (dynamic-wind
   (lambda ()
     (set! old-handler
	   (vector-set! interrupt-vector CHARACTER-SLOT old-handler)))
   code
   (lambda ()
     (vector-set! interrupt-vector CHARACTER-SLOT
		  (set! old-handler
			(vector-ref interrupt-vector CHARACTER-SLOT)))))))

;;; end INTERRUPT-SYSTEM package.
(the-environment)))