#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/tterm.scm,v 1.15 1992/02/25 23:32:03 cph Exp $

Copyright (c) 1990-92 Massachusetts Institute of Technology

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
    (let ((baud-rate (output-port/baud-rate console-output-port))
	  (x-size (output-port/x-size console-output-port))
	  (y-size (output-port/y-size console-output-port)))
      (make-screen (with-values
		       (lambda ()
			 (compute-scrolling-costs description
						  baud-rate
						  x-size
						  y-size))
		     (lambda (insert-line-cost
			      insert-line-next-cost
			      delete-line-cost
			      delete-line-next-cost
			      scroll-region-cost)
		       (make-terminal-state description
					    (baud-rate->index baud-rate)
					    baud-rate
					    insert-line-cost
					    insert-line-next-cost
					    delete-line-cost
					    delete-line-next-cost
					    scroll-region-cost)))
		   console-beep
		   console-clear-line!
		   console-clear-rectangle!
		   console-clear-screen!
		   console-discard!
		   console-enter!
		   console-exit!
		   console-flush!
		   console-modeline-event!
		   console-discretionary-flush
		   console-scroll-lines-down!
		   console-scroll-lines-up!
		   console-wrap-update!
		   console-write-char!
		   console-write-cursor!
		   console-write-substring!
		   (fix:1+ (fix:quotient baud-rate 2400))
		   x-size
		   y-size))))

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

(define (console-termcap-description)
  (if (eq? console-description 'UNKNOWN)
      (set! console-description
	    (let ((term (get-environment-variable "TERM")))
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

(define (get-console-input-operations)
  (let ((channel (input-port/channel console-input-port))
	(string (make-string input-buffer-size))
	(start input-buffer-size)
	(end input-buffer-size)
	(pending-event false))
    (let ((read-event
	   (lambda (block?)
	     (let ((event pending-event))
	       (if event
		   (begin
		     (set! pending-event false)
		     event)
		   (let loop ()
		     (if block?
			 (channel-blocking channel)
			 (channel-nonblocking channel))
		     (let ((n
			    (channel-select-then-read
			     channel string 0 input-buffer-size)))
		       (cond ((not n)
			      (if block?
				  (error "#F returned from blocking read"))
			      false)
			     ((fix:> n 0)
			      (set! start 0)
			      (set! end n)
			      (if transcript-port
				  (output-port/write-substring
				   transcript-port string 0 n))
			      (string-ref string 0))
			     ((or (fix:= n event:process-output)
				  (fix:= n event:process-status))
			      n)
			     ((fix:= n event:interrupt)
			      (if inferior-thread-changes? n (loop)))
			     ((fix:= n 0)
			      (error "Reached EOF in keyboard input."))
			     (else
			      (error "Illegal return value:" n))))))))))
      (let ((read-until-result
	     (lambda (block?)
	       (let loop ()
		 (or (fix:< start end)
		     (let ((event
			    (if block?
				(or (read-event false)
				    (begin
				      (update-screens! false)
				      (read-event true)))
				(read-event false))))
		       (if (fix:fixnum? event)
			   (begin
			     (process-change-event event)
			     (loop))
			   event)))))))
	(values
	 (lambda ()			;halt-update?
	   (or pending-event
	       (fix:< start end)
	       (let ((event (read-event false)))
		 (if (fix:fixnum? event)
		     (set! pending-event event))
		 event)))
	 (lambda ()			;peek-no-hang
	   (read-until-result false))
	 (lambda ()			;peek
	   (read-until-result true)
	   (string-ref string start))
	 (lambda ()			;read
	   (read-until-result true)
	   (let ((char (string-ref string start)))
	     (set! start (fix:+ start 1))
	     char)))))))

(define-integrable input-buffer-size 16)
(define-integrable event:process-output -2)
(define-integrable event:process-status -3)
(define-integrable event:interrupt -4)

(define (process-change-event event)
  (cond ((fix:= event event:process-output) (accept-process-output))
	((fix:= event event:process-status) (handle-process-status-changes))
	((fix:= event event:interrupt) (accept-thread-output))
	(else (error "Illegal change event:" event))))

(define (signal-interrupt!)
  (signal-thread-event editor-thread
    (lambda ()
      ;; (editor-beep)			; kbd beeps by itself
      (temporary-message "Quit")
      (^G-signal))))

(define (with-console-interrupts-enabled thunk)
  (with-console-interrupt-state 2 thunk))

(define (with-console-interrupts-disabled thunk)
  (with-console-interrupt-state 0 thunk))

(define (with-console-interrupt-state inside thunk)
  (let ((outside))
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
			   (lambda (screen)
			     screen
			     (get-console-input-operations))
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
      (tty-set-interrupt-enables 2)
      (receiver
       (lambda (thunk)
	 (bind-console-state (get-outside-state)
	   (lambda (get-inside-state)
	     get-inside-state
	     (thunk))))
       `((INTERRUPT/ABORT-TOP-LEVEL ,signal-interrupt!))))))

(define (bind-console-state inside-state receiver)
  (let ((outside-state))
    (dynamic-wind (lambda ()
		    (set! outside-state (console-state))
		    (if inside-state
			(set-console-state! inside-state)))
		  (lambda ()
		    (receiver (lambda () outside-state)))
		  (lambda ()
		    (set! inside-state (console-state))
		    (set-console-state! outside-state)))))

(define (console-state)
  (vector (channel-state (input-port/channel console-input-port))
	  (channel-state (output-port/channel console-output-port))
	  (tty-get-interrupt-enables)))

(define (set-console-state! state)
  (set-channel-state! (input-port/channel console-input-port)
		      (vector-ref state 0))
  (set-channel-state! (output-port/channel console-output-port)
		      (vector-ref state 1))
  (tty-set-interrupt-enables (vector-ref state 2)))

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
				 insert-line-cost
				 insert-line-next-cost
				 delete-line-cost
				 delete-line-next-cost
				 scroll-region-cost))
		   (conc-name terminal-state/))
  (description false read-only true)
  (baud-rate-index false read-only true)
  (baud-rate false read-only true)
  (insert-line-cost false read-only true)
  (insert-line-next-cost false read-only true)
  (delete-line-cost false read-only true)
  (delete-line-next-cost false read-only true)
  (scroll-region-cost false read-only true)
  (cursor-x false)
  (cursor-y false)
  (standout-mode? false)
  (insert-mode? false)
  (delete-mode? false)
  (scroll-region false))

(let-syntax ((define-accessor
	       (macro (name)
		 `(DEFINE-INTEGRABLE (,(symbol-append 'SCREEN- name) SCREEN)
		    (,(symbol-append 'TERMINAL-STATE/ name)
		     (SCREEN-STATE SCREEN)))))
	     (define-updater
	       (macro (name)
		 `(DEFINE-INTEGRABLE
		    (,(symbol-append 'SET-SCREEN- name '!) SCREEN ,name)
		    (,(symbol-append 'SET-TERMINAL-STATE/ name '!)
		     (SCREEN-STATE SCREEN)
		     ,name)))))
  (define-accessor description)
  (define-accessor baud-rate-index)
  (define-accessor baud-rate)
  (define-accessor insert-line-cost)
  (define-accessor insert-line-next-cost)
  (define-accessor delete-line-cost)
  (define-accessor delete-line-next-cost)
  (define-accessor scroll-region-cost)
  (define-accessor cursor-x)
  (define-updater  cursor-x)
  (define-accessor cursor-y)
  (define-updater  cursor-y)
  (define-accessor standout-mode?)
  (define-updater  standout-mode?)
  (define-accessor insert-mode?)
  (define-updater  insert-mode?)
  (define-accessor delete-mode?)
  (define-updater  delete-mode?)
  (define-accessor scroll-region)
  (define-updater  scroll-region))

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

(define (console-discretionary-flush screen)
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
			      (loop))))))))))))

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
	(output-port/write-char console-output-port char)
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
	  (output-port/write-substring console-output-port string start end)
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
	 (let ((y-size (screen-y-size screen))
	       (yu* (fix:- yu amount)))
	   (let ((draw-cost (scroll-draw-cost screen yl yu*)))
	     (if (or (fix:= yu y-size)
		     (scroll-region-ok? description))
		 (and (fix:< (insert-lines-cost screen yl yu amount) draw-cost)
		      (begin
			(insert-lines screen yl yu amount)
			'CLEARED))
		 (and (fix:<
		       (fix:+ (delete-lines-cost screen yu* y-size amount)
			      (insert-lines-cost screen yl y-size amount))
		       draw-cost)
		      (begin
			(delete-lines screen yu* y-size amount)
			(insert-lines screen yl y-size amount)
			'CLEARED))))))))

(define (console-scroll-lines-up! screen xl xu yl yu amount)
  (let ((description (screen-description screen)))
    (and (insert/delete-line-ok? description)
	 (fix:= xl 0)
	 (fix:= xu (screen-x-size screen))
	 (let ((y-size (screen-y-size screen))
	       (draw-cost (scroll-draw-cost screen (fix:+ yl amount) yu)))
	   (if (or (fix:= yu y-size)
		   (scroll-region-ok? description))
	       (and (fix:< (delete-lines-cost screen yl yu amount) draw-cost)
		    (begin
		      (delete-lines screen yl yu amount)
		      'CLEARED))
	       (let ((yu* (fix:- yu amount)))
		 (and (fix:<
		       (fix:+ (delete-lines-cost screen yl y-size amount)
			      (insert-lines-cost screen yu* y-size amount))
		       draw-cost)
		      (begin
			(delete-lines screen yl y-size amount)
			(insert-lines screen yu* y-size amount)
			'CLEARED))))))))

(define (scroll-draw-cost screen yl yu)
  (do ((yl yl (fix:+ yl 1))
       (cost 0 (fix:+ cost (screen-line-draw-cost screen yl))))
      ((fix:= yl yu) cost)))

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
		(output-port/write-char console-output-port #\space))
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
		  (output-port/write-char console-output-port #\space))
		(record-cursor-after-output screen x-end))))))))

(define (insert-lines screen yl yu n)
  (let ((y-size (screen-y-size screen))
	(description (screen-description screen))
	(n-lines (fix:- yu yl)))
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
	   (error "can't insert lines" screen)))))

(define (insert-lines-cost screen yl yu n)
  (if (and (ts-insert-line (screen-description screen))
	   (fix:= yu (screen-y-size screen)))
      (fix:+ (vector-ref (screen-insert-line-cost screen) yl)
	     (fix:* (vector-ref (screen-insert-line-next-cost screen) yl)
		    (fix:- n 1)))
      (fix:+ (screen-scroll-region-cost screen)
	     (let ((yl (fix:+ yl (fix:- (screen-y-size screen) yu))))
	       (fix:+ (vector-ref (screen-insert-line-cost screen) yl)
		      (fix:* (vector-ref (screen-insert-line-next-cost screen)
					 yl)
			     (fix:- n 1)))))))

(define (delete-lines screen yl yu n)
  (let ((y-size (screen-y-size screen))
	(description (screen-description screen))
	(n-lines (fix:- yu yl)))
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
    (clear-scroll-region screen)))

(define (delete-lines-cost screen yl yu n)
  (if (and (ts-delete-line (screen-description screen))
	   (fix:= yu (screen-y-size screen)))
      (fix:+ (vector-ref (screen-delete-line-cost screen) yl)
	     (fix:* (vector-ref (screen-delete-line-next-cost screen) yl)
		    (fix:- n 1)))
      (fix:+ (screen-scroll-region-cost screen)
	     (let ((yl (fix:+ yl (fix:- (screen-y-size screen) yu))))
	       (fix:+ (vector-ref (screen-delete-line-cost screen) yl)
		      (fix:* (vector-ref (screen-delete-line-next-cost screen)
					 yl)
			     (fix:- n 1)))))))

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
	    (let ((s
		   (%set-scroll-region-string (screen-description screen)
					      (screen-x-size screen)
					      (screen-y-size screen)
					      yl
					      yu)))
	      (if (not s)
		  (error "can't set scroll region" screen))
	      s))
  (set-screen-cursor-x! screen false)
  (set-screen-cursor-y! screen false))

(define (%set-scroll-region-string description x-size y-size yl yu)
  (cond ((ts-set-scroll-region description)
	 =>
	 (lambda (ts-set-scroll-region)
	   (parameterize-2 ts-set-scroll-region yl (fix:-1+ yu))))
	((ts-set-scroll-region-1 description)
	 =>
	 (lambda (ts-set-scroll-region-1)
	   (parameterize-4 ts-set-scroll-region-1
			   y-size yl (fix:- y-size yu) y-size)))
	((ts-set-window description)
	 =>
	 (lambda (ts-set-window)
	   (parameterize-4 ts-set-window yl (fix:-1+ yu) 0 (fix:-1+ x-size))))
	(else false)))

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

(define-integrable (move-cursor screen x y)
  (if (not (and (screen-cursor-x screen)
		(fix:= x (screen-cursor-x screen))
		(fix:= y (screen-cursor-y screen))))
      (%move-cursor screen x y)))

(define (%move-cursor screen x y)
  (let ((description (screen-description screen))
	(cursor-x (screen-cursor-x screen))
	(cursor-y (screen-cursor-y screen))
	(y-size (screen-y-size screen))
	(trivial-command (lambda (command) (output-1 screen command))))
    (let ((general-case
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
	     (general-case)))))
  (set-screen-cursor-x! screen x)
  (set-screen-cursor-y! screen y))

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

(define (compute-scrolling-costs description baud-rate x-size y-size)
  (with-values
      (lambda ()
	(i/d-line-cost-vectors description
			       baud-rate
			       y-size
			       (ts-insert-multi-line description)
			       (or (ts-insert-line description)
				   (ts-reverse-scroll description))))
    (lambda (insert-line-cost insert-line-next-cost)
      (with-values
	  (lambda ()
	    (i/d-line-cost-vectors description
				   baud-rate
				   y-size
				   (ts-delete-multi-line description)
				   (or (ts-delete-line description)
				       (ts-forward-scroll description))))
	(lambda (delete-line-cost delete-line-next-cost)
	  (values insert-line-cost
		  insert-line-next-cost
		  delete-line-cost
		  delete-line-next-cost
		  (let ((string
			 (%set-scroll-region-string description
						    x-size
						    y-size
						    0
						    y-size)))
		    (if string
			(fix:* 2 (string-cost description baud-rate string 0))
			0))))))))

(define (i/d-line-cost-vectors description baud-rate y-size
			       multi-line one-line)
  (let ((extra
	 ;; Discourage long scrolls slightly on fast lines.  This
	 ;; says that scrolling nearly the full length of the screen
	 ;; is not worth it if reprinting takes less than 1/4
	 ;; second.
	 (fix:quotient baud-rate (fix:* 40 y-size)))
	(string-cost
	 (lambda (string n)
	   (string-cost description baud-rate string n))))
    (cond (multi-line
	   (let ((c (string-cost multi-line 0)))
	     (scrolling-vectors y-size
				c
				(fix:- (string-cost multi-line 10) c)
				extra
				0)))
	  (one-line
	   (let ((c (string-cost one-line 0)))
	     (scrolling-vectors y-size
				0
				0
				(fix:+ c extra)
				(fix:- (string-cost one-line 10) c))))
	  (else
	   (values false false)))))

(define-integrable (string-cost description baud-rate string n-lines)
  (string-length
   (termcap-pad-string string
		       n-lines
		       (baud-rate->index baud-rate)
		       (ts-pad-char description))))

#| Calculate the insert and delete line costs.

We keep the ID costs in a precomputed array based on the position at
which the I or D is performed.  Also, there are two kinds of ID costs:
the "once-only" and the "repeated".  This is to handle both those
terminals that are able to insert N lines at a time (once-only) and
those that must repeatedly insert one line.

The cost to insert N lines at line L (0-origin indexing) is

	(+ (+ IL-OV1 (* IL-PF1 (- Y-SIZE L)))
	   (* N (+ IL-OVN (* IL-PFN (- Y-SIZE L)))))

IL-OV1 represents the basic insert line overhead.  IL-PF1 is the
padding required to allow the terminal time to move a line: insertion
at line L changes (- Y-SIZE L) lines.

The first subexpression above is the overhead; the second is the
multiply factor.  Both are dependent only on the position at which the
insert is performed.  We store the overhead in INSERT-LINE-COST and
the multiply factor in INSERT-LINE-NEXT-COST.  Note however that any
insertion must include at least one multiply factor.  Rather than
compute this as INSERT-LINE-COST[line]+INSERT-LINE-NEXT-COST[line], we
add INSERT-LINE-NEXT-COST into INSERT-LINE-COST.  This is reasonable
because of the particular algorithm used.

Deletion is essentially the same as insertion. 

Note that the multiply factors are in tenths of characters.  |#

(define (scrolling-vectors y-size overhead-1 factor-1 overhead-n factor-n)
  (let ((overhead (make-vector y-size))
	(factor (make-vector y-size)))
    (let loop
	((y 0)
	 (o (fix:+ (fix:* overhead-1 10) (fix:* factor-1 y-size)))
	 (n (fix:+ (fix:* overhead-n 10) (fix:* factor-n y-size))))
      (if (fix:< y y-size)
	  (begin
	    (vector-set! factor y (fix:quotient n 10))
	    (let ((n (fix:- n factor-n)))
	      (vector-set! overhead y (fix:quotient (fix:+ o n) 10))
	      (loop (fix:1+ y) (fix:- o factor-1) n)))))
    (values overhead factor)))