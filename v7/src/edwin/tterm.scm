#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/tterm.scm,v 1.4 1991/03/11 01:14:47 cph Exp $

Copyright (c) 1990-91 Massachusetts Institute of Technology

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

;;;; Termcap(3) Screen Implementation

(declare (usual-integrations))

(define-primitives
  (baud-rate->index 1)
  (tty-get-interrupt-enables 0)
  (tty-set-interrupt-enables 1))

(define (output-port/baud-rate port)
  (let ((channel (output-port/channel port)))
    (and channel
	 (channel-type=terminal? channel)
	 (terminal-output-baud-rate channel))))

(define (output-port/buffered-chars port)
  (let ((operation (output-port/operation port 'BUFFERED-CHARS)))
    (if operation
	(operation port)
	0)))

(define (output-port/y-size port)
  ((output-port/custom-operation port 'Y-SIZE) port))

(define (console-available?)
  (let ((description (console-termcap-description)))
    (and (termcap-description? description)
	 (sufficiently-powerful? description)
	 (no-undesirable-characteristics? description))))

(define (make-console-screen)
  (let ((description (console-termcap-description)))
    (cond ((not (output-port/baud-rate console-output-port))
	   (error "standard output not a terminal"))
	  ((not description)
	   (error "terminal type not set"))
	  ((not (termcap-description? description))
	   (error "unknown terminal type" description))
	  ((not (sufficiently-powerful? description))
	   (error "terminal type not powerful enough"
		  (terminal-type-name description)))
	  ((not (no-undesirable-characteristics? description))
	   (error "terminal type has undesirable characteristics"
		  (terminal-type-name description))))
    (make-screen (let ((baud-rate (output-port/baud-rate console-output-port)))
		   (let ((baud-rate-index (baud-rate->index baud-rate)))
		     (make-terminal-state
		      description
		      baud-rate-index
		      baud-rate
		      (fix:1+ (fix:quotient baud-rate 2400)))))
		 console-beep
		 console-clear-line!
		 console-clear-rectangle!
		 console-clear-screen!
		 console-discard!
		 console-enter!
		 console-exit!
		 console-flush!
		 console-modeline-event!
		 console-preempt-update?
		 console-scroll-lines-down!
		 console-scroll-lines-up!
		 console-wrap-update!
		 console-write-char!
		 console-write-cursor!
		 console-write-substring!
		 (output-port/x-size console-output-port)
		 (output-port/y-size console-output-port))))

(define (console-termcap-description)
  (if (eq? console-description 'UNKNOWN)
      (set! console-description
	    (let ((term ((ucode-primitive get-environment-variable 1) "TERM")))
	      (and term
		   (or (and (output-port/baud-rate console-output-port)
			    (make-termcap-description term))
		       term)))))
  console-description)

(define (sufficiently-powerful? description)
  (and (let ((x-size (tn-x-size description)))
	 (and x-size
	      (> x-size 0)))
       (let ((y-size (tn-y-size description)))
	 (and y-size
	      (> y-size 0)))
       (ts-cursor-move description)))

(define (no-undesirable-characteristics? description)
  (not (or (tf-hazeltine description)
	   (tf-teleray description)
	   (tf-underscore description))))

(define-integrable input-buffer-size 16)

(define (get-console-input-operations screen)
  screen				;ignored
  (let ((channel (input-port/channel console-input-port))
	(string (make-string input-buffer-size))
	(start input-buffer-size)
	(end input-buffer-size))
    (let ((fill-buffer
	   (lambda (block?)
	     (let ((eof (lambda () "Reached EOF in keyboard input.")))
	       (if (fix:= end 0) (eof))
	       (if block?
		   (channel-blocking channel)
		   (channel-nonblocking channel))
	       (let ((n (channel-read channel string 0 input-buffer-size)))
		 (cond (n
			(if (fix:= n 0) (eof))
			(set! start 0)
			(set! end n)
			(if transcript-port
			    (write-string (substring string 0 n)
					  transcript-port)))
		       (block? (error "Blocking read returned #F.")))
		 n)))))
      (values
       (lambda ()			;char-ready?
	 (if (fix:< start end)
	     true
	     (fill-buffer false)))
       (lambda ()			;peek-char
	 (if (not (fix:< start end)) (fill-buffer true))
	 (string-ref string start))
       (lambda ()			;read-char
	 (if (not (fix:< start end)) (fill-buffer true))
	 (let ((char (string-ref string start)))
	   (set! start (fix:+ start 1))
	   char))))))

(define (signal-interrupt! interrupt-enables)
  interrupt-enables			; ignored
  ;; (editor-beep)			; kbd beeps by itself
  (temporary-message "Quit")
  (^G-signal))

(define (with-console-interrupts-enabled thunk)
  (with-console-interrupt-state 2 thunk))

(define (with-console-interrupts-disabled thunk)
  (with-console-interrupt-state 0 thunk))

(define (with-console-interrupt-state state thunk)
  (let ((outside)
	(inside state))
    (dynamic-wind (lambda ()
		    (set! outside (tty-get-interrupt-enables))
		    (tty-set-interrupt-enables inside))
		  thunk
		  (lambda ()
		    (set! inside (tty-get-interrupt-enables))
		    (tty-set-interrupt-enables outside)))))

(define console-display-type)
(define console-description)

(define (initialize-package!)
  (set! console-display-type
	(make-display-type 'CONSOLE
			   false
			   console-available?
			   make-console-screen
			   get-console-input-operations
			   with-console-grabbed
			   with-console-interrupts-enabled
			   with-console-interrupts-disabled))
  (set! console-description 'UNKNOWN)
  unspecific)

(define (with-console-grabbed receiver)
  (bind-console-state false
    (lambda (get-outside-state)
      (terminal-operation terminal-raw-input
			  (input-port/channel console-input-port))
      (terminal-operation terminal-raw-output
			  (output-port/channel console-output-port))
      (set! hook/^g-interrupt signal-interrupt!)
      (tty-set-interrupt-enables 2)
      (receiver
       (lambda (thunk)
	 (bind-console-state (get-outside-state)
	   (lambda (get-inside-state)
	     get-inside-state
	     (thunk))))))))

(define (bind-console-state state receiver)
  (let ((outside-state)
	(inside-state state))
    (dynamic-wind (lambda ()
		    (set! outside-state (console-state))
		    (if inside-state
			(set-console-state! inside-state))
		    (set! inside-state false)
		    unspecific)
		  (lambda ()
		    (receiver (lambda () outside-state)))
		  (lambda ()
		    (set! inside-state (console-state))
		    (set-console-state! outside-state)
		    (set! outside-state false)
		    unspecific))))

(define (console-state)
  (vector (channel-state (input-port/channel console-input-port))
	  (channel-state (output-port/channel console-output-port))
	  hook/^g-interrupt
	  (tty-get-interrupt-enables)))

(define (set-console-state! state)
  (set-channel-state! (input-port/channel console-input-port)
		      (vector-ref state 0))
  (set-channel-state! (output-port/channel console-output-port)
		      (vector-ref state 1))
  (set! hook/^g-interrupt (vector-ref state 2))
  (tty-set-interrupt-enables (vector-ref state 3)))

(define (channel-state channel)
  (and channel
       (channel-type=terminal? channel)
       (cons (channel-blocking? channel)
	     (terminal-get-state channel))))

(define (set-channel-state! channel state)
  (if (and channel
	   (channel-type=terminal? channel)
	   state)
      (begin
	(if (car state)
	    (channel-blocking channel)
	    (channel-nonblocking channel))
	(terminal-set-state channel (cdr state)))))

(define (terminal-operation operation channel)
  (if (and channel
	   (channel-type=terminal? channel))
      (operation channel)))

;;;; Terminal State

(define-structure (terminal-state
		   (constructor make-terminal-state
				(description
				 baud-rate-index
				 baud-rate
				 preemption-modulus))
		   (conc-name terminal-state/))
  (description false read-only true)
  (baud-rate-index false read-only true)
  (baud-rate false read-only true)
  (preemption-modulus false read-only true)
  (cursor-x false)
  (cursor-y false)
  (standout-mode? false)
  (insert-mode? false)
  (delete-mode? false)
  (scroll-region false))

(define-integrable (screen-description screen)
  (terminal-state/description (screen-state screen)))

(define-integrable (screen-baud-rate-index screen)
  (terminal-state/baud-rate-index (screen-state screen)))

(define-integrable (screen-baud-rate screen)
  (terminal-state/baud-rate (screen-state screen)))

(define-integrable (screen-preemption-modulus screen)
  (terminal-state/preemption-modulus (screen-state screen)))

(define-integrable (screen-cursor-x screen)
  (terminal-state/cursor-x (screen-state screen)))

(define-integrable (set-screen-cursor-x! screen cursor-x)
  (set-terminal-state/cursor-x! (screen-state screen) cursor-x))

(define-integrable (screen-cursor-y screen)
  (terminal-state/cursor-y (screen-state screen)))

(define-integrable (set-screen-cursor-y! screen cursor-y)
  (set-terminal-state/cursor-y! (screen-state screen) cursor-y))

(define-integrable (screen-standout-mode? screen)
  (terminal-state/standout-mode? (screen-state screen)))

(define-integrable (set-screen-standout-mode?! screen standout-mode?)
  (set-terminal-state/standout-mode?! (screen-state screen) standout-mode?))

(define-integrable (screen-insert-mode? screen)
  (terminal-state/insert-mode? (screen-state screen)))

(define-integrable (set-screen-insert-mode?! screen insert-mode?)
  (set-terminal-state/insert-mode?! (screen-state screen) insert-mode?))

(define-integrable (screen-delete-mode? screen)
  (terminal-state/delete-mode? (screen-state screen)))

(define-integrable (set-screen-delete-mode?! screen delete-mode?)
  (set-terminal-state/delete-mode?! (screen-state screen) delete-mode?))

(define-integrable (screen-scroll-region screen)
  (terminal-state/scroll-region (screen-state screen)))

(define-integrable (set-screen-scroll-region! screen scroll-region)
  (set-terminal-state/scroll-region! (screen-state screen) scroll-region))

;;;; Console Screen Operations

(define (console-discard! screen)
  screen
  (set! console-description 'UNKNOWN)
  unspecific)

(define (console-enter! screen)
  (maybe-output screen (ts-enter-termcap-mode (screen-description screen)))
  (set-screen-cursor-x! screen false)
  (set-screen-cursor-y! screen false))

(define (console-exit! screen)
  (let ((description (screen-description screen)))
    (move-cursor screen 0 (fix:-1+ (screen-y-size screen)))
    (exit-standout-mode screen)
    (exit-insert-mode screen)
    (maybe-output screen (ts-exit-termcap-mode description)))
  (output-port/flush-output console-output-port))

(define (console-modeline-event! screen window type)
  screen window type
  unspecific)

(define (console-wrap-update! screen thunk)
  screen
  (thunk)
  (output-port/flush-output console-output-port))

(define (console-preempt-update? screen y)
  (and (fix:= 0 (fix:remainder y (screen-preemption-modulus screen)))
       (begin
	 (let ((n (output-port/buffered-chars console-output-port)))
	   (if (fix:< 20 n)
	       (begin
		 (output-port/flush-output console-output-port)
		 (let ((baud-rate (screen-baud-rate screen)))
		   (if (fix:< baud-rate 2400)
		       (let ((msec (quotient (* n 10000) baud-rate)))
			 (if (>= msec 1000)
			     (let ((t (+ (real-time-clock) msec)))
			       (let loop ()
				 (if (< (real-time-clock) t)
				     (loop)))))))))))
	 true)))

(define (console-beep screen)
  (output-1 screen (ts-audible-bell (screen-description screen))))

(define (console-flush! screen)
  screen
  (output-port/flush-output console-output-port))

(define (console-write-cursor! screen x y)
  (move-cursor screen x y))

(define (console-write-char! screen x y char highlight)
  (if (let ((description (screen-description screen)))
	(not (and (tf-automatic-wrap description)
		  (fix:= x (fix:-1+ (screen-x-size screen)))
		  (fix:= y (fix:-1+ (screen-y-size screen))))))
      (begin
	(exit-insert-mode screen)
	(move-cursor screen x y)
	(highlight-if-desired screen highlight)
	(output-char screen char)
	(record-cursor-after-output screen (fix:1+ x)))))

(define (console-write-substring! screen x y string start end highlight)
  (if (fix:< start end)
      (begin
	(exit-insert-mode screen)
	(move-cursor screen x y)
	(highlight-if-desired screen highlight)
	(let ((end
	       (if (let ((description (screen-description screen)))
		     (and (tf-automatic-wrap description)
			  (fix:= y (fix:-1+ (screen-y-size screen)))
			  (fix:= (fix:+ x (fix:- end start))
				 (screen-x-size screen))))
		   (fix:-1+ end)
		   end)))
	  (do ((i start (fix:1+ i)))
	      ((fix:= i end))
	    (output-char screen (string-ref string i)))
	  (record-cursor-after-output screen (fix:+ x (fix:- end start)))))))

(define (console-clear-line! screen x y first-unused-x)
  (move-cursor screen x y)
  (clear-line screen first-unused-x))

(define (console-clear-screen! screen)
  (clear-screen screen))

(define (console-clear-rectangle! screen xl xu yl yu highlight)
  (let ((x-size (screen-x-size screen))
	(y-size (screen-y-size screen)))
    (cond ((not (fix:= xu x-size))
	   (let ((n (fix:- xu xl)))
	     (do ((y yl (fix:1+ y)))
		 ((fix:= y yu))
	       (move-cursor screen xl y)
	       (clear-multi-char screen n))))
	  ((fix:= yl (fix:1+ yu))
	   (move-cursor screen xl yl)
	   (clear-line screen x-size))
	  ((and (fix:= xl 0) (fix:= yu y-size))
	   (if (fix:= yl 0)
	       (clear-screen screen)
	       (begin
		 (move-cursor screen 0 yl)
		 (clear-to-bottom screen))))
	  (else
	   (do ((y yl (fix:1+ y)))
	       ((fix:= y yu))
	     (move-cursor screen xl y)
	     (clear-line screen x-size))))))

(define (console-scroll-lines-down! screen xl xu yl yu amount)
  (let ((description (screen-description screen)))
    (and (insert/delete-line-ok? description)
	 (fix:= xl 0)
	 (fix:= xu (screen-x-size screen))
	 (begin
	   (let ((y-size (screen-y-size screen)))
	     (if (or (fix:= yu y-size)
		     (scroll-region-ok? description))
		 (insert-lines screen yl yu amount)
		 (begin
		   (delete-lines screen (fix:- yu amount) y-size amount)
		   (insert-lines screen yl y-size amount))))
	   'CLEARED))))

(define (console-scroll-lines-up! screen xl xu yl yu amount)
  (let ((description (screen-description screen)))
    (and (insert/delete-line-ok? description)
	 (fix:= xl 0)
	 (fix:= xu (screen-x-size screen))
	 (begin
	   (let ((y-size (screen-y-size screen)))
	     (if (or (fix:= yu y-size)
		     (scroll-region-ok? description))
		 (delete-lines screen yl yu amount)
		 (begin
		   (delete-lines screen yl y-size amount)
		   (insert-lines screen (fix:- yu amount) y-size amount))))
	   'CLEARED))))

;;;; Termcap Commands

(define (clear-screen screen)
  (let ((description (screen-description screen)))
    (let ((ts-clear-screen (ts-clear-screen description)))
      (if ts-clear-screen
	  (begin
	    (exit-standout-mode screen)
	    (output-n screen ts-clear-screen (screen-y-size screen))
	    (set-screen-cursor-x! screen 0)
	    (set-screen-cursor-y! screen 0))
	  (begin
	    (move-cursor screen 0 0)
	    (clear-to-bottom screen))))))

(define (clear-to-bottom screen)
  (let ((description (screen-description screen)))
    (let ((ts-clear-to-bottom (ts-clear-to-bottom description)))
      (if ts-clear-to-bottom
	  (begin
	    (exit-standout-mode screen)
	    (output screen ts-clear-to-bottom))
	  (let ((x-size (screen-x-size screen))
		(y-size (screen-y-size screen)))
	    (do ((y (screen-cursor-y screen) (fix:1+ y)))
		((fix:= y y-size))
	      (move-cursor screen 0 y)
	      (clear-line screen x-size)))))))

(define (clear-line screen first-unused-x)
  (exit-standout-mode screen)
  (let ((description (screen-description screen)))
    (let ((ts-clear-line (ts-clear-line description)))
      (if ts-clear-line
	  (output-1 screen ts-clear-line)
	  (begin
	    (exit-insert-mode screen)
	    (let ((first-unused-x
		   (if (and (tf-automatic-wrap description)
			    (fix:= first-unused-x (screen-x-size screen))
			    (fix:= (screen-cursor-y screen)
				   (fix:-1+ (screen-y-size screen))))
		       (fix:-1+ first-unused-x)
		       first-unused-x)))
	      (do ((x (screen-cursor-x screen) (fix:1+ x)))
		  ((fix:= x first-unused-x))
		(output-space screen))
	      (record-cursor-after-output screen first-unused-x)))))))

(define (clear-multi-char screen n)
  (exit-standout-mode screen)
  (let ((description (screen-description screen)))
    (let ((ts-clear-multi-char (ts-clear-multi-char description)))
      (if ts-clear-multi-char
	  (output-1 screen (parameterize-1 ts-clear-multi-char n))
	  (begin
	    (exit-insert-mode screen)
	    (let ((cursor-x (screen-cursor-x screen)))
	      (let ((x-end
		     (let ((x-end (fix:+ cursor-x n))
			   (x-size (screen-x-size screen)))
		       (if (fix:> x-end x-size)
			   (error "can't clear past end of line"))
		       (if (and (fix:= x-end x-size)
				(tf-automatic-wrap description)
				(fix:= (screen-cursor-y screen)
				       (fix:-1+ (screen-y-size screen))))
			   (fix:-1+ x-size)
			   x-end))))
		(do ((x cursor-x (fix:1+ x)))
		    ((fix:= x x-end))
		  (output-space screen))
		(record-cursor-after-output screen x-end))))))))

(define (insert-lines screen yl yu n)
  (let ((description (screen-description screen))
	(n-lines (fix:- yu yl)))
    (let ((y-size (screen-y-size screen)))
      (cond ((ts-insert-line description)
	     =>
	     (lambda (ts-insert-line)
	       (if (not (fix:= yu y-size))
		   (set-scroll-region screen yl yu))
	       (move-cursor screen 0 yl)
	       (exit-standout-mode screen)
	       (let ((ts-insert-multi-line (ts-insert-multi-line description)))
		 (if (and (fix:> n 1) ts-insert-multi-line)
		     (output-n screen
			       (parameterize-1 ts-insert-multi-line n)
			       n-lines)
		     (do ((i 0 (fix:1+ i)))
			 ((fix:= i n))
		       (output-n screen ts-insert-line n-lines))))
	       (clear-scroll-region screen)))
	    ((ts-reverse-scroll description)
	     =>
	     (lambda (ts-reverse-scroll)
	       (set-scroll-region screen yl yu)
	       (move-cursor screen 0 yl)
	       (exit-standout-mode screen)
	       (do ((i 0 (fix:1+ i)))
		   ((fix:= i n))
		 (output-n screen ts-reverse-scroll n-lines))
	       (clear-scroll-region screen)
	       (if (and (tf-memory-above-screen description)
			(fix:= yl 0)
			(fix:= yu y-size))
		   (let ((x-size (screen-x-size screen)))
		     (do ((y 0 (fix:1+ y)))
			 ((fix:= y n))
		       (move-cursor screen 0 y)
		       (clear-line screen x-size))))))
	    (else
	     (error "can't insert lines" screen))))))

(define (delete-lines screen yl yu n)
  (let ((description (screen-description screen))
	(n-lines (fix:- yu yl)))
    (let ((y-size (screen-y-size screen)))
      (cond ((ts-delete-line description)
	     =>
	     (lambda (ts-delete-line)
	       (if (not (fix:= yu y-size))
		   (set-scroll-region screen yl yu))
	       (move-cursor screen 0 yl)
	       (exit-standout-mode screen)
	       (let ((ts-delete-multi-line (ts-delete-multi-line description)))
		 (if (and (fix:> n 1) ts-delete-multi-line)
		     (output-n screen
			       (parameterize-1 ts-delete-multi-line n)
			       n-lines)
		     (do ((i 0 (fix:1+ i)))
			 ((fix:= i n))
		       (output-n screen ts-delete-line n-lines))))))
	    ((ts-forward-scroll description)
	     =>
	     (lambda (ts-forward-scroll)
	       (set-scroll-region screen yl yu)
	       (move-cursor screen 0 (fix:-1+ yu))
	       (exit-standout-mode screen)
	       (do ((i 0 (fix:1+ i)))
		   ((fix:= i n))
		 (output-n screen ts-forward-scroll n-lines))))
	    (else
	     (error "can't delete lines" screen)))
      (if (and (tf-memory-below-screen description)
	       (not (screen-scroll-region screen))
	       (fix:> n 0))
	  (begin
	    (move-cursor screen 0 (fix:- y-size n))
	    (clear-to-bottom screen)))
      (clear-scroll-region screen))))

(define (set-scroll-region screen yl yu)
  (let ((y-size (tn-y-size (screen-description screen))))
    (if (and (fix:= yl 0) (fix:= yu y-size))
	(clear-scroll-region screen)
	(if (let ((scroll-region (screen-scroll-region screen)))
	      (not (and scroll-region
			(fix:= yl (car scroll-region))
			(fix:= yu (cdr scroll-region)))))
	    (begin
	      (%set-scroll-region screen yl yu)
	      (set-screen-scroll-region! screen (cons yl yu)))))))

(define (clear-scroll-region screen)
  (let ((scroll-region (screen-scroll-region screen)))
    (if scroll-region
	(begin
	  (%set-scroll-region screen 0 (tn-y-size (screen-description screen)))
	  (set-screen-scroll-region! screen false)))))

(define (%set-scroll-region screen yl yu)
  (output-1 screen
	    (let ((description (screen-description screen)))
	      (cond ((ts-set-scroll-region description)
		     =>
		     (lambda (ts-set-scroll-region)
		       (parameterize-2 ts-set-scroll-region yl (fix:-1+ yu))))
		    ((ts-set-scroll-region-1 description)
		     =>
		     (lambda (ts-set-scroll-region-1)
		       (let ((y-size (screen-y-size screen)))
			 (parameterize-4 ts-set-scroll-region-1
					 y-size
					 yl
					 (fix:- y-size yu)
					 y-size))))
		    ((ts-set-window description)
		     =>
		     (lambda (ts-set-window)
		       (parameterize-4 ts-set-window
				       yl (fix:-1+ yu)
				       0 (fix:-1+ (screen-x-size screen)))))
		    (else
		     (error "can't set scroll region" screen)))))
  (set-screen-cursor-x! screen false)
  (set-screen-cursor-y! screen false))

(define (highlight-if-desired screen highlight)
  (if highlight
      (enter-standout-mode screen)
      (exit-standout-mode screen)))

(define-integrable (enter-standout-mode screen)
  ;; If the terminal uses standout markers, don't use standout.
  ;; It's too complicated to bother with.
  (if (and (not (screen-standout-mode? screen))
	   (not (tn-standout-marker-width (screen-description screen))))
      (begin
	(set-screen-standout-mode?! screen true)
	(maybe-output-1
	 screen
	 (ts-enter-standout-mode (screen-description screen))))))

(define-integrable (exit-standout-mode screen)
  (if (screen-standout-mode? screen)
      (begin
	(set-screen-standout-mode?! screen false)
	(maybe-output-1 screen
			(ts-exit-standout-mode (screen-description screen))))))

(define-integrable (enter-insert-mode screen)
  (if (not (screen-insert-mode? screen))
      (begin
	(set-screen-insert-mode?! screen true)
	(maybe-output-1 screen
			(ts-enter-insert-mode (screen-description screen))))))

(define-integrable (exit-insert-mode screen)
  (if (screen-insert-mode? screen)
      (begin
	(set-screen-insert-mode?! screen false)
	(maybe-output-1 screen
			(ts-exit-insert-mode (screen-description screen))))))

(define-integrable (enter-delete-mode screen)
  (if (not (screen-delete-mode? screen))
      (begin
	(set-screen-delete-mode?! screen true)
	(maybe-output-1 screen
			(ts-enter-delete-mode (screen-description screen))))))

(define-integrable (exit-delete-mode screen)
  (if (screen-delete-mode? screen)
      (begin
	(set-screen-delete-mode?! screen false)
	(maybe-output-1 screen
			(ts-exit-delete-mode (screen-description screen))))))

(define (move-cursor screen x y)
  (let ((description (screen-description screen))
	(cursor-x (screen-cursor-x screen))
	(cursor-y (screen-cursor-y screen)))
    (if (not (and cursor-x (fix:= x cursor-x) (fix:= y cursor-y)))
	(let ((y-size (screen-y-size screen))
	      (trivial-command (lambda (command) (output-1 screen command)))
	      (general-case
	       (lambda ()
		 (output-1 screen
			   (parameterize-2 (ts-cursor-move description)
					   y x)))))
	  (if (not (tf-standout-mode-motion description))
	      (exit-standout-mode screen))
	  (if (not (tf-insert-mode-motion description))
	      (exit-insert-mode screen))
	  (cond ((and (fix:= x 0)
		      (fix:= y 0)
		      (ts-cursor-upper-left description))
		 => trivial-command)
		((and (fix:= x 0)
		      (fix:= y (fix:-1+ y-size))
		      (ts-cursor-lower-left description))
		 => trivial-command)
		((not cursor-x)
		 (general-case))
		((fix:= y cursor-y)
		 (cond ((and (fix:= x (fix:-1+ cursor-x))
			     (ts-cursor-left description))
			=> trivial-command)
		       ((and (fix:= x (fix:1+ cursor-x))
			     (ts-cursor-right description))
			=> trivial-command)
		       ((and (fix:= x 0)
			     (ts-cursor-line-start description))
			=> trivial-command)
		       ((ts-cursor-move-x description)
			=>
			(lambda (ts-cursor-move-x)
			  (output-1 screen
				    (parameterize-1 ts-cursor-move-x x))))
		       (else
			(general-case))))
		((fix:= x cursor-x)
		 (cond ((and (fix:= y (fix:-1+ cursor-y))
			     (ts-cursor-up description))
			=> trivial-command)
		       ((and (fix:= y (fix:1+ cursor-y))
			     (ts-cursor-down description))
			=> trivial-command)
		       (else
			(general-case))))
		(else
		 (general-case)))
	  (set-screen-cursor-x! screen x)
	  (set-screen-cursor-y! screen y)))))

(define (record-cursor-after-output screen cursor-x)
  (let ((description (screen-description screen)))
    (let ((x-size (screen-x-size screen)))
      (cond ((fix:< cursor-x x-size)
	     (set-screen-cursor-x! screen cursor-x))
	    ((fix:> cursor-x x-size)
	     (error "wrote past end of line" cursor-x x-size))
	    ((or (tf-magic-wrap description)
		 (tf-lose-wrap description))
	     (set-screen-cursor-x! screen false)
	     (set-screen-cursor-y! screen false))
	    ((tf-automatic-wrap description)
	     (set-screen-cursor-x! screen 0)
	     (set-screen-cursor-y! screen (fix:1+ (screen-cursor-y screen))))
	    (else
	     (set-screen-cursor-x! screen (fix:-1+ x-size)))))))

(define (pad-string screen string n-lines)
  (termcap-pad-string string
		      n-lines
		      (screen-baud-rate-index screen)
		      (ts-pad-char (screen-description screen))))

(define (goto-string screen string x y)
  (let ((description (screen-description screen)))
    (termcap-goto-string string x y
			 (ts-cursor-left description)
			 (ts-cursor-up description))))

(define-integrable (parameterize-1 string p1)
  (termcap-param-string string p1 0 0 0))

(define-integrable (parameterize-2 string p1 p2)
  (termcap-param-string string p1 p2 0 0))

(define-integrable (parameterize-4 string p1 p2 p3 p4)
  (termcap-param-string string p1 p2 p3 p4))

(define (output screen command)
  (output-n screen
	    command
	    (fix:- (let ((scroll-region (screen-scroll-region screen)))
		     (if scroll-region
			 (cdr scroll-region)
			 (tn-y-size (screen-description screen))))
		   (screen-cursor-y screen))))

(define-integrable (output-1 screen command)
  (output-n screen command 1))

(define-integrable (output-n screen command n-lines)
  (output-port/write-string console-output-port
			    (pad-string screen command n-lines)))

(define (maybe-output screen command)
  (if command
      (output screen command)))

(define-integrable (maybe-output-1 screen command)
  (maybe-output-n screen command 1))

(define (maybe-output-n screen command n-lines)
  (if command
      (output-n screen command n-lines)))

(define-integrable (output-char screen char)
  screen
  (output-port/write-char console-output-port char))

(define-integrable (output-space screen)
  (output-char screen #\space))