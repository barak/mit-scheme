;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/screen.scm,v 1.94 1991/07/09 22:52:18 cph Exp $
;;;
;;;	Copyright (c) 1989-91 Massachusetts Institute of Technology
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

;;;; Screen Abstraction

(declare (usual-integrations))

(define-structure (screen
		   (constructor make-screen
				(state
				 operation/beep
				 operation/clear-line!
				 operation/clear-rectangle!
				 operation/clear-screen!
				 operation/discard!
				 operation/enter!
				 operation/exit!
				 operation/flush!
				 operation/modeline-event!
				 operation/discretionary-flush
				 operation/scroll-lines-down!
				 operation/scroll-lines-up!
				 operation/wrap-update!
				 operation/write-char!
				 operation/write-cursor!
				 operation/write-substring!
				 preemption-modulus
				 x-size
				 y-size)))
  (state false read-only true)
  (operation/beep false read-only true)
  (operation/clear-line! false read-only true)
  (operation/clear-rectangle! false read-only true)
  (operation/clear-screen! false read-only true)
  (operation/discard! false read-only true)
  (operation/enter! false read-only true)
  (operation/exit! false read-only true)
  (operation/flush! false read-only true)
  (operation/modeline-event! false read-only true)
  (operation/discretionary-flush false read-only true)
  (operation/scroll-lines-down! false read-only true)
  (operation/scroll-lines-up! false read-only true)
  (operation/wrap-update! false read-only true)
  (operation/write-char! false read-only true)
  (operation/write-cursor! false read-only true)
  (operation/write-substring! false read-only true)
  (preemption-modulus false read-only true)
  (root-window false)
  (needs-update? false)
  (in-update? false)
  (x-size false)
  (y-size false)

  ;; Description of actual screen contents.
  current-matrix

  ;; Description of desired screen contents.
  new-matrix

  ;; Set this variable in the debugger to force a display preemption.
  (debug-preemption-y false)

  ;; Set this variable in the debugger to trace interesting events.
  (debug-trace false))

(define (initialize-screen-root-window! screen bufferset buffer)
  (set-screen-root-window!
   screen
   (make-editor-frame
    screen
    buffer
    (bufferset-find-or-create-buffer bufferset (make-typein-buffer-name -1))))
  (set-screen-current-matrix! screen (make-matrix screen))
  (set-screen-new-matrix! screen (make-matrix screen)))

(define (screen-beep screen)
  ((screen-operation/beep screen) screen))

(define (screen-enter! screen)
  ((screen-operation/enter! screen) screen)
  (screen-modeline-event! screen
			  (screen-selected-window screen)
			  'SELECT-SCREEN))

(define (screen-exit! screen)
  ((screen-operation/exit! screen) screen)
  (screen-modeline-event! screen
			  (screen-selected-window screen)
			  'DESELECT-SCREEN))

(define (screen-discard! screen)
  (for-each (lambda (window) (send window ':kill!))
	    (screen-window-list screen))
  ((screen-operation/discard! screen) screen))

(define (screen-modeline-event! screen window type)
  ((screen-operation/modeline-event! screen) screen window type))

(define-integrable (screen-selected-window screen)
  (editor-frame-selected-window (screen-root-window screen)))

(define (screen-select-window! screen window)
  (editor-frame-select-window! (screen-root-window screen) window)
  (screen-modeline-event! screen window 'SELECT-WINDOW))

(define-integrable (screen-select-cursor! screen window)
  (editor-frame-select-cursor! (screen-root-window screen) window))

(define-integrable (screen-window-list screen)
  (editor-frame-windows (screen-root-window screen)))

(define-integrable (screen-window0 screen)
  (editor-frame-window0 (screen-root-window screen)))

(define-integrable (screen-typein-window screen)
  (editor-frame-typein-window (screen-root-window screen)))

(define (window-screen window)
  (editor-frame-screen (window-root-window window)))

(define (update-screen! screen display-style)
  (if display-style (screen-force-update screen))
  (with-screen-in-update screen display-style
    (lambda ()
      (editor-frame-update-display! (screen-root-window screen)
				    display-style))))

;;; Interface from update optimizer to terminal:

(define-integrable (terminal-scroll-lines-down screen xl xu yl yu amount)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'scroll-lines-down
				   xl xu yl yu amount))
  ((screen-operation/scroll-lines-down! screen) screen xl xu yl yu amount))

(define-integrable (terminal-scroll-lines-up screen xl xu yl yu amount)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'scroll-lines-up
				   xl xu yl yu amount))
  ((screen-operation/scroll-lines-up! screen) screen xl xu yl yu amount))

(define-integrable (terminal-flush screen)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'flush))
  ((screen-operation/flush! screen) screen))

(define-integrable (terminal-move-cursor screen x y)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'move-cursor x y))
  ((screen-operation/write-cursor! screen) screen x y))

(define-integrable (terminal-clear-screen screen)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'clear-screen))
  ((screen-operation/clear-screen! screen) screen))

(define-integrable (terminal-clear-line screen x y first-unused-x)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'clear-line
				   x y first-unused-x))
  ((screen-operation/clear-line! screen) screen x y first-unused-x))

(define-integrable (terminal-output-char screen x y char highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'output-char
				   x y char highlight))
  ((screen-operation/write-char! screen) screen x y char highlight))

(define-integrable (terminal-output-substring screen x y string start end
					      highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'terminal screen 'output-substring
				   x y (string-copy string) start end
				   highlight))
  ((screen-operation/write-substring! screen) screen x y string start end
					      highlight))

;;;; Update Optimization

(define-structure (matrix (constructor %make-matrix ()))
  ;; Vector of line contents.
  ;; (string-ref (vector-ref (matrix-contents m) y) x) is the
  ;; character at position X, Y.
  contents

  ;; Vector of line highlights.
  ;; (boolean-vector-ref (vector-ref (matrix-highlight m) y) x) is the
  ;; highlight at position X, Y.
  highlight

  ;; Boolean-vector indicating, for each line, whether its contents
  ;; mean anything.
  enable

  ;; Boolean-vector indicating, for each line, whether there is any
  ;; highlighting on the line.
  highlight-enable

  ;; Cursor position.
  cursor-x
  cursor-y)

(define (make-matrix screen)
  (let ((matrix (%make-matrix))
	(x-size (screen-x-size screen))
	(y-size (screen-y-size screen)))
    (let ((contents (make-vector y-size))
	  (highlight (make-vector y-size))
	  (enable (make-boolean-vector y-size))
	  (highlight-enable (make-boolean-vector y-size)))
      (do ((i 0 (fix:1+ i)))
	  ((fix:= i y-size))
	(vector-set! contents i (make-string x-size))
	(vector-set! highlight i (make-boolean-vector x-size)))
      (boolean-vector-fill! enable false)
      (set-matrix-contents! matrix contents)
      (set-matrix-highlight! matrix highlight)
      (set-matrix-enable! matrix enable)
      (set-matrix-highlight-enable! matrix highlight-enable))
    (set-matrix-cursor-x! matrix false)
    (set-matrix-cursor-y! matrix false)
    matrix))

(define (set-screen-size! screen x-size y-size)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'set-size! x-size y-size))
  (without-interrupts
   (lambda ()
     (set-screen-x-size! screen x-size)
     (set-screen-y-size! screen y-size)
     (set-screen-current-matrix! screen (make-matrix screen))
     (set-screen-new-matrix! screen (make-matrix screen))
     (send (screen-root-window screen) ':set-size! x-size y-size))))

(define (screen-move-cursor screen x y)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'move-cursor x y))
  (let ((new-matrix (screen-new-matrix screen)))
    (set-matrix-cursor-x! new-matrix x)
    (set-matrix-cursor-y! new-matrix y)))

(define (screen-direct-output-move-cursor screen x y)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'direct-output-move-cursor
				   x y))
  (terminal-move-cursor screen x y)
  (terminal-flush screen)
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen)))
    (set-matrix-cursor-x! current-matrix x)
    (set-matrix-cursor-y! current-matrix y)
    (set-matrix-cursor-x! new-matrix x)
    (set-matrix-cursor-y! new-matrix y)))

(define (screen-output-char screen x y char highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'output-char
				   x y char highlight))
  (let ((new-matrix (screen-new-matrix screen)))
    (if (not (boolean-vector-ref (matrix-enable new-matrix) y))
	(begin
	  (boolean-vector-set! (matrix-enable new-matrix) y true)
	  (set-screen-needs-update?! screen true)
	  (initialize-new-line-contents screen y)))
    (string-set! (vector-ref (matrix-contents new-matrix) y) x char)
    (cond ((boolean-vector-ref (matrix-highlight-enable new-matrix) y)
	   (boolean-vector-set! (vector-ref (matrix-highlight new-matrix) y)
				x highlight))
	  (highlight
	   (boolean-vector-set! (matrix-highlight-enable new-matrix) y true)
	   (initialize-new-line-highlight screen y)
	   (boolean-vector-set! (vector-ref (matrix-highlight new-matrix) y)
				x highlight)))))

(define (screen-output-substring screen x y string start end highlight)
  (substring-move-left! string start end
			(screen-get-output-line screen y x
						(fix:+ x (fix:- end start))
						highlight)
			x))

(define (screen-get-output-line screen y xl xu highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'output-line
				   y xl xu highlight))
  (let ((new-matrix (screen-new-matrix screen)))
    (let ((full-line? (and (fix:= xl 0) (fix:= xu (screen-x-size screen)))))
      (if (not (boolean-vector-ref (matrix-enable new-matrix) y))
	  (begin
	    (boolean-vector-set! (matrix-enable new-matrix) y true)
	    (set-screen-needs-update?! screen true)
	    (if (not full-line?) (initialize-new-line-contents screen y))))
      (cond ((boolean-vector-ref (matrix-highlight-enable new-matrix) y)
	     (if (and full-line? (not highlight))
		 (boolean-vector-set! (matrix-highlight-enable new-matrix)
				      y false)
		 (boolean-subvector-fill!
		  (vector-ref (matrix-highlight new-matrix) y)
		  xl xu highlight)))
	    (highlight
	     (boolean-vector-set! (matrix-highlight-enable new-matrix) y true)
	     (if (not full-line?) (initialize-new-line-highlight screen y))
	     (boolean-subvector-fill!
	      (vector-ref (matrix-highlight new-matrix) y)
	      xl xu highlight))))
    (vector-ref (matrix-contents new-matrix) y)))

(define-integrable (initialize-new-line-contents screen y)
  (if (boolean-vector-ref (matrix-enable (screen-current-matrix screen)) y)
      (string-move!
       (vector-ref (matrix-contents (screen-current-matrix screen)) y)
       (vector-ref (matrix-contents (screen-new-matrix screen)) y))
      (string-fill!
       (vector-ref (matrix-contents (screen-new-matrix screen)) y)
       #\space)))

(define-integrable (initialize-new-line-highlight screen y)
  (if (boolean-vector-ref
       (matrix-highlight-enable (screen-current-matrix screen))
       y)
      (boolean-vector-move!
       (vector-ref (matrix-highlight (screen-current-matrix screen)) y)
       (vector-ref (matrix-highlight (screen-new-matrix screen)) y))
      (boolean-vector-fill!
       (vector-ref (matrix-highlight (screen-new-matrix screen)) y)
       false)))

(define (screen-clear-rectangle screen xl xu yl yu highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'clear-rectangle
				   xl xu yl yu highlight))
  (let ((new-matrix (screen-new-matrix screen)))
    (let ((new-contents (matrix-contents new-matrix))
	  (new-hl (matrix-highlight new-matrix))
	  (new-enable (matrix-enable new-matrix))
	  (new-hl-enable (matrix-highlight-enable new-matrix)))
      (cond ((not (and (fix:= xl 0) (fix:= xu (screen-x-size screen))))
	     (let ((current-matrix (screen-current-matrix screen)))
	       (let ((current-contents (matrix-contents current-matrix))
		     (current-hl (matrix-highlight current-matrix))
		     (current-enable (matrix-enable current-matrix))
		     (current-hl-enable
		      (matrix-highlight-enable current-matrix)))
		 (do ((y yl (fix:1+ y)))
		     ((fix:= y yu))
		   (if (not (boolean-vector-ref new-enable y))
		       (begin
			 (boolean-vector-set! new-enable y true)
			 (if (boolean-vector-ref current-enable y)
			     (begin
			       (string-move! (vector-ref current-contents y)
					     (vector-ref new-contents y))
			       (substring-fill! (vector-ref new-contents y)
						xl xu #\space))
			     (string-fill! (vector-ref new-contents y)
					   #\space)))
		       (substring-fill! (vector-ref new-contents y)
					xl xu #\space))
		   (cond ((boolean-vector-ref new-hl-enable y)
			  (boolean-subvector-fill! (vector-ref new-hl y)
						   xl xu highlight))
			 (highlight
			  (boolean-vector-set! new-hl-enable y true)
			  (if (boolean-vector-ref current-hl-enable y)
			      (boolean-vector-move! (vector-ref current-hl y)
						    (vector-ref new-hl y))
			      (boolean-vector-fill! (vector-ref new-hl y)
						    false))
			  (boolean-subvector-fill! (vector-ref new-hl y)
						   xl xu highlight))
			 ((boolean-vector-ref current-hl-enable y)
			  (let ((nhl (vector-ref new-hl y)))
			    (boolean-vector-move! (vector-ref current-hl y)
						  nhl)
			    (boolean-subvector-fill! nhl xl xu false)
			    (if (not (boolean-vector-all-elements? nhl false))
				(boolean-vector-set! new-hl-enable y
						     true)))))))))
	    (highlight
	     (do ((y yl (fix:1+ y)))
		 ((fix:= y yu))
	       (string-fill! (vector-ref new-contents y) #\space)
	       (boolean-vector-fill! (vector-ref new-hl y) true)
	       (boolean-vector-set! new-enable y true)
	       (boolean-vector-set! new-hl-enable y true)))
	    (else
	     (do ((y yl (fix:1+ y)))
		 ((fix:= y yu))
	       (string-fill! (vector-ref new-contents y) #\space)
	       (boolean-vector-set! new-enable y true)
	       (boolean-vector-set! new-hl-enable y false))))))
  (set-screen-needs-update?! screen true))

(define (screen-direct-output-char screen x y char highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'direct-output-char
				   x y char highlight))
  (let ((cursor-x (fix:1+ x))
	(current-matrix (screen-current-matrix screen)))
    (terminal-output-char screen x y char highlight)
    (terminal-move-cursor screen cursor-x y)
    (terminal-flush screen)
    (string-set! (vector-ref (matrix-contents current-matrix) y) x char)
    (cond ((boolean-vector-ref (matrix-highlight-enable current-matrix) y)
	   (boolean-vector-set! (vector-ref (matrix-highlight current-matrix)
					    y)
				x highlight))
	  (highlight
	   (boolean-vector-set! (matrix-highlight-enable current-matrix)
				y true)
	   (boolean-vector-set! (vector-ref (matrix-highlight current-matrix)
					    y)
				x highlight)))
    (set-matrix-cursor-x! current-matrix cursor-x)
    (set-matrix-cursor-x! (screen-new-matrix screen) cursor-x)))

(define (screen-direct-output-substring screen x y string start end highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'direct-output-substring
				   x y (string-copy string) start end
				   highlight))
  (let ((cursor-x (fix:+ x (fix:- end start)))
	(current-matrix (screen-current-matrix screen)))
    (terminal-output-substring screen x y string start end highlight)
    (terminal-move-cursor screen cursor-x y)
    (terminal-flush screen)
    (substring-move-left! string start end
			  (vector-ref (matrix-contents current-matrix) y) x)
    (cond ((boolean-vector-ref (matrix-highlight-enable current-matrix) y)
	   (boolean-subvector-fill!
	    (vector-ref (matrix-highlight current-matrix) y)
	    x cursor-x highlight))
	  (highlight
	   (boolean-vector-set! (matrix-highlight-enable current-matrix)
				y true)
	   (boolean-subvector-fill!
	    (vector-ref (matrix-highlight current-matrix) y)
	    x cursor-x highlight)))
    (set-matrix-cursor-x! current-matrix cursor-x)
    (set-matrix-cursor-x! (screen-new-matrix screen) cursor-x)))

(define (screen-force-update screen)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'force-update))
  (let ((y-size (screen-y-size screen))
	(current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen)))
    (terminal-clear-screen screen)
    (let ((current-contents (matrix-contents current-matrix))
	  (current-hl (matrix-highlight current-matrix))
	  (current-enable (matrix-enable current-matrix))
	  (current-hl-enable (matrix-highlight-enable current-matrix))
	  (new-contents (matrix-contents new-matrix))
	  (new-hl (matrix-highlight new-matrix))
	  (new-enable (matrix-enable new-matrix))
	  (new-hl-enable (matrix-highlight-enable new-matrix)))
      (do ((y 0 (fix:1+ y)))
	  ((fix:= y y-size))
	(if (not (boolean-vector-ref new-enable y))
	    (begin
	      (let ((c (vector-ref new-contents y)))
		(vector-set! new-contents y (vector-ref current-contents y))
		(vector-set! current-contents y c))
	      (boolean-vector-set! new-enable y true)
	      (if (boolean-vector-ref current-hl-enable y)
		  (begin
		    (let ((h (vector-ref current-hl y)))
		      (vector-set! new-hl y (vector-ref current-hl y))
		      (vector-set! current-hl y h))
		    (boolean-vector-set! new-hl-enable y true)))))
	(string-fill! (vector-ref current-contents y) #\space)
	(boolean-vector-set! current-enable y true)
	(boolean-vector-set! current-hl-enable y false))))
  (set-screen-needs-update?! screen true))

(define (screen-scroll-lines-down screen xl xu yl yu amount)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'scroll-lines-down
				   xl xu yl yu amount))
  (let ((current-matrix (screen-current-matrix screen)))
    (and (boolean-subvector-all-elements? (matrix-enable current-matrix)
					  yl yu true)
	 (not (screen-needs-update? screen))
	 (let ((scrolled?
		(terminal-scroll-lines-down screen xl xu yl yu amount)))
	   (and scrolled?
		(begin
		  (let ((contents (matrix-contents current-matrix))
			(hl (matrix-highlight current-matrix))
			(hl-enable (matrix-highlight-enable current-matrix)))
		    (do ((y (fix:-1+ (fix:- yu amount)) (fix:-1+ y))
			 (y* (fix:-1+ yu) (fix:-1+ y*)))
			((fix:< y yl))
		      (substring-move-left! (vector-ref contents y) xl xu
					    (vector-ref contents y*) xl)
		      (cond ((boolean-vector-ref hl-enable y)
			     (boolean-vector-set! hl-enable y* true)
			     (boolean-subvector-move-left!
			      (vector-ref hl y) xl xu
			      (vector-ref hl y*) xl))
			    ((boolean-vector-ref hl-enable y*)
			     (boolean-subvector-fill! (vector-ref hl y*) xl xu
						      false))))
		    (if (eq? scrolled? 'CLEARED)
			(let ((yu (fix:+ yl amount)))
			  (if (and (fix:= xl 0)
				   (fix:= xu (screen-x-size screen)))
			      (do ((y yl (fix:1+ y)))
				  ((fix:= y yu))
				(substring-fill! (vector-ref contents y) xl xu
						 #\space)
				(boolean-vector-set! hl-enable y false))
			      (do ((y yl (fix:1+ y)))
				  ((fix:= y yu))
				(substring-fill! (vector-ref contents y) xl xu
						 #\space)
				(if (boolean-vector-ref hl-enable y)
				    (boolean-subvector-fill! (vector-ref hl y)
							     xl xu false)))))))
		  scrolled?))))))

(define (screen-scroll-lines-up screen xl xu yl yu amount)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'scroll-lines-up
				   xl xu yl yu amount))
  (let ((current-matrix (screen-current-matrix screen)))
    (and (boolean-subvector-all-elements? (matrix-enable current-matrix)
					  yl yu true)
	 (not (screen-needs-update? screen))
	 (let ((scrolled?
		(terminal-scroll-lines-up screen xl xu yl yu amount)))
	   (and scrolled?
		(begin
		  (let ((contents (matrix-contents current-matrix))
			(hl (matrix-highlight current-matrix))
			(hl-enable (matrix-highlight-enable current-matrix)))
		    (do ((y yl (fix:1+ y))
			 (y* (fix:+ yl amount) (fix:1+ y*)))
			((fix:= y* yu))
		      (substring-move-left! (vector-ref contents y*) xl xu
					    (vector-ref contents y) xl)
		      (cond ((boolean-vector-ref hl-enable y*)
			     (boolean-vector-set! hl-enable y true)
			     (boolean-subvector-move-left!
			      (vector-ref hl y*) xl xu
			      (vector-ref hl y) xl))
			    ((boolean-vector-ref hl-enable y)
			     (boolean-subvector-fill! (vector-ref hl y) xl xu
						      false))))
		    (if (eq? scrolled? 'CLEARED)
			(if (and (fix:= xl 0)
				 (fix:= xu (screen-x-size screen)))
			    (do ((y (fix:- yu amount) (fix:1+ y)))
				((fix:= y yu))
			      (substring-fill! (vector-ref contents y) xl xu
					       #\space)
			      (boolean-vector-set! hl-enable y false))
			    (do ((y (fix:- yu amount) (fix:1+ y)))
				((fix:= y yu))
			      (substring-fill! (vector-ref contents y) xl xu
					       #\space)
			      (if (boolean-vector-ref hl-enable y)
				  (boolean-subvector-fill! (vector-ref hl y)
							   xl xu false))))))
		  scrolled?))))))

(define (with-screen-in-update screen display-style thunk)
  (without-interrupts
   (lambda ()
     (let ((old-flag))
       (dynamic-wind (lambda ()
		       (set! old-flag (screen-in-update? screen))
		       (set-screen-in-update?! screen true))
		     (lambda ()
		       ((screen-operation/wrap-update! screen)
			screen
			(lambda ()
			  (and (thunk)
			       (screen-update screen display-style)))))
		     (lambda ()
		       (set-screen-in-update?! screen old-flag)))))))

(define (screen-update screen force?)
  ;; Update the actual terminal screen based on the data in `new-matrix'.
  ;; Value is #F if redisplay stopped due to pending input.
  ;; FORCE? true means do not stop for pending input.
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'update force?))
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen))
	(y-size (screen-y-size screen))
	(preemption-modulus (screen-preemption-modulus screen))
	(discretionary-flush (screen-operation/discretionary-flush screen))
	(halt-update? (editor-halt-update? current-editor)))
    (let ((enable (matrix-enable new-matrix)))
      (let loop ((y 0))
	(cond ((fix:= y y-size)
	       (let ((x (matrix-cursor-x new-matrix))
		     (y (matrix-cursor-y new-matrix)))
		 (terminal-move-cursor screen x y)
		 (set-matrix-cursor-x! current-matrix x)
		 (set-matrix-cursor-y! current-matrix y))
	       (set-screen-needs-update?! screen false)
	       true)
	      ((and (fix:= 0 (fix:remainder y preemption-modulus))
		    (begin
		      (if discretionary-flush (discretionary-flush screen))
		      true)
		    (not force?)
		    (or (halt-update?)
			(eq? (screen-debug-preemption-y screen) y)))
	       (terminal-move-cursor screen
				     (matrix-cursor-x current-matrix)
				     (matrix-cursor-y current-matrix))
	       (if (screen-debug-trace screen)
		   ((screen-debug-trace screen) 'screen screen
						'update-preemption y))
	       false)
	      (else
	       (if (boolean-vector-ref enable y)
		   (update-line screen y))
	       (loop (fix:1+ y))))))))

(define (update-line screen y)
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen))
	(x-size (screen-x-size screen)))
    (let ((current-contents (matrix-contents current-matrix))
	  (current-hl (matrix-highlight current-matrix))
	  (current-enable (matrix-enable current-matrix))
	  (current-hl-enable (matrix-highlight-enable current-matrix))
	  (new-contents (matrix-contents new-matrix))
	  (new-hl (matrix-highlight new-matrix))
	  (new-hl-enable (matrix-highlight-enable new-matrix)))
      (let ((ccy (vector-ref current-contents y))
	    (chy (vector-ref current-hl y))
	    (ncy (vector-ref new-contents y))
	    (nhy (vector-ref new-hl y))
	    (nhey (boolean-vector-ref new-hl-enable y)))
	(cond ((or (not (boolean-vector-ref current-enable y))
		   (if (boolean-vector-ref current-hl-enable y)
		       (not nhey)
		       nhey))
	       (if nhey
		   (update-line-ignore-current screen y ncy nhy x-size)
		   (update-line-trivial screen y ncy x-size)))
	      (nhey
	       (update-line-highlight screen y ccy chy ncy nhy x-size))
	      (else
	       (update-line-no-highlight screen y ccy ncy x-size)))
	(vector-set! current-contents y ncy)
	(boolean-vector-set! current-enable y true)
	(vector-set! new-contents y ccy)
	(boolean-vector-set! (matrix-enable new-matrix) y false)
	(if nhey
	    (begin
	      (vector-set! current-hl y nhy)
	      (boolean-vector-set! current-hl-enable y true)
	      (vector-set! new-hl y chy)
	      (boolean-vector-set! new-hl-enable y false))
	    (boolean-vector-set! current-hl-enable y false))))))

(define (update-line-ignore-current screen y nline highlight x-size)
  (cond ((not (boolean-subvector-uniform? highlight 0 x-size))
	 (let loop ((x 0))
	   (let ((hl (boolean-vector-ref highlight x)))
	     (let ((x*
		    (boolean-subvector-find-next highlight (fix:1+ x) x-size
						 (not hl))))
	       (if x*
		   (begin
		     (terminal-output-substring screen x y nline x x* hl)
		     (loop x*))
		   (terminal-output-substring screen x y nline x x-size
					      hl))))))
	((boolean-vector-ref highlight 0)
	 (terminal-output-substring screen 0 y nline 0 x-size true))
	(else
	 (update-line-trivial screen y nline x-size))))

(define (update-line-trivial screen y nline x-size)
  (let ((xe (substring-non-space-end nline 0 x-size)))
    (if (fix:< 0 xe)
	(terminal-output-substring screen 0 y nline 0 xe false))
    (if (fix:< xe x-size)
	(terminal-clear-line screen xe y x-size))))

(define (update-line-no-highlight screen y oline nline x-size)
  (let ((olen (substring-non-space-end oline 0 x-size))
	(nlen (substring-non-space-end nline 0 x-size)))
    (cond ((fix:= 0 olen)
	   (let ((nstart (substring-non-space-start nline 0 nlen)))
	     (if (fix:< nstart nlen)
		 (terminal-output-substring screen nstart y
					    nline nstart nlen false))))
	  ((fix:= 0 nlen)
	   (terminal-clear-line screen nlen y olen))
	  (else
	   (let ((len (fix:min olen nlen)))
	     (let find-mismatch ((x 0))
	       (cond ((fix:= x len)
		      (if (fix:< x nlen)
			  (terminal-output-substring screen x y
						     nline x nlen false)))
		     ((fix:= (vector-8b-ref oline x)
			     (vector-8b-ref nline x))
		      (find-mismatch (fix:+ x 1)))
		     (else
		      (let find-match ((x* (fix:+ x 1)))
			(cond ((fix:= x* len)
			       (terminal-output-substring
				screen x y nline x nlen false))
			      ((not (fix:= (vector-8b-ref oline x*)
					   (vector-8b-ref nline x*)))
			       (find-match (fix:+ x* 1)))
			      (else
			       ;; Ignore matches of 4 characters or less.
			       ;; The overhead of moving the cursor and
			       ;; drawing the characters is too much except
			       ;; for very slow terminals.
			       (let find-end-match ((x** (fix:+ x* 1)))
				 (cond ((fix:= x** len)
					(if (fix:< (fix:- x** x*) 5)
					    (terminal-output-substring
					     screen x y nline x nlen false)
					    (begin
					      (terminal-output-substring
					       screen x y nline x x* false)
					      (if (fix:< x** nlen)
						  (terminal-output-substring
						   screen x** y
						   nline x** nlen false)))))
				       ((fix:= (vector-8b-ref oline x**)
					       (vector-8b-ref nline x**))
					(find-end-match (fix:+ x** 1)))
				       ((fix:< (fix:- x** x*) 5)
					(find-match x**))
				       (else
					(terminal-output-substring
					 screen x y nline x x* false)
					(find-mismatch x**)))))))))))
	   (if (fix:< nlen olen)
	       (terminal-clear-line screen nlen y olen))))))

(define (screen-line-draw-cost screen y)
  (let ((line (vector-ref (matrix-contents (screen-current-matrix screen)) y)))
    (let ((end (substring-non-space-end line 0 (string-length line))))
      (if (fix:= 0 end)
	  0
	  (fix:- end (substring-non-space-start line 0 end))))))

(define (update-line-highlight screen y oline ohl nline nhl x-size)
  (let find-mismatch ((x 0))
    (if (not (fix:= x x-size))
	(if (and (fix:= (vector-8b-ref oline x) (vector-8b-ref nline x))
		 (eq? (boolean-vector-ref ohl x) (boolean-vector-ref nhl x)))
	    (find-mismatch (fix:+ x 1))
	    (let ((hl (boolean-vector-ref nhl x)))
	      (let find-match ((x* (fix:+ x 1)))
		(cond ((fix:= x* x-size)
		       (terminal-output-substring screen x y nline x x* hl))
		      ((or (not (eq? hl (boolean-vector-ref nhl x*)))
			   (and (eq? hl (boolean-vector-ref ohl x*))
				(fix:= (vector-8b-ref oline x*)
				       (vector-8b-ref nline x*))))
		       ;; Either found a match, or the highlight
		       ;; changed.  In either case, output the current
		       ;; segment and continue from the top.
		       (terminal-output-substring screen x y nline x x* hl)
		       (find-mismatch x*))
		      (else
		       (find-match (fix:+ x* 1))))))))))

(define-integrable (fix:min x y) (if (fix:< x y) x y))
(define-integrable (fix:max x y) (if (fix:> x y) x y))

(define-integrable (substring-non-space-start string start end)
  (do ((index start (fix:+ index 1)))
      ((or (fix:= end index)
	   (not (fix:= (vector-8b-ref string index)
		       (char->integer #\space))))
       index)))

(define-integrable (substring-non-space-end string start end)
  (do ((index end (fix:- index 1)))
      ((or (fix:= start index)
	   (not (fix:= (vector-8b-ref string (fix:- index 1))
		       (char->integer #\space))))
       index)))

(define (string-move! x y)
  (substring-move-left! x 0 (string-length x) y 0))

(define-integrable (boolean-vector-ref vector index)
  (fix:= (char->integer #\t) (vector-8b-ref vector index)))

(define-integrable (boolean-vector-set! vector index value)
  (vector-8b-set! vector index (boolean->ascii value)))

(define (boolean-vector-all-elements? vector value)
  (boolean-subvector-all-elements? vector 0 (boolean-vector-length vector)
				   value))

(define (boolean-subvector-all-elements? vector start end value)
  (if (vector-8b-find-next-char vector start end (boolean->ascii (not value)))
      false
      true))

(define (boolean-subvector-uniform? vector start end)
  (if (and (fix:< start end)
	   (vector-8b-find-next-char
	    vector start end
	    (boolean->ascii (not (boolean-vector-ref vector start)))))
      false
      true))

(define-integrable (boolean-subvector-find-next vector start end value)
  (vector-8b-find-next-char vector start end (boolean->ascii value)))

(define-integrable make-boolean-vector string-allocate)
(define-integrable boolean-vector-length string-length)
(define-integrable boolean-vector=? string=?)
(define-integrable boolean-subvector-move-right! substring-move-right!)
(define-integrable boolean-subvector-move-left! substring-move-left!)
(define-integrable boolean-vector-move! string-move!)
(define-integrable boolean-vector-copy string-copy)

(define-integrable (boolean-subvector-fill! vector start end value)
  (vector-8b-fill! vector start end (boolean->ascii value)))

(define (boolean-vector-fill! vector value)
  (boolean-subvector-fill! vector 0 (boolean-vector-length vector) value))

(define-integrable (boolean->ascii boolean)
  (if boolean (char->integer #\t) (char->integer #\f)))