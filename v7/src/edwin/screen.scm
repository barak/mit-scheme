;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/screen.scm,v 1.86 1991/01/15 13:59:08 cph Exp $
;;;
;;;	Copyright (c) 1989, 1990, 1991 Massachusetts Institute of Technology
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
				 operation/preempt-update?
				 operation/scroll-lines-down!
				 operation/scroll-lines-up!
				 operation/wrap-update!
				 operation/write-char!
				 operation/write-cursor!
				 operation/write-substring!
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
  (operation/preempt-update? false read-only true)
  (operation/scroll-lines-down! false read-only true)
  (operation/scroll-lines-up! false read-only true)
  (operation/wrap-update! false read-only true)
  (operation/write-char! false read-only true)
  (operation/write-cursor! false read-only true)
  (operation/write-substring! false read-only true)
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

(define-integrable (terminal-preempt-update? screen y)
  ((screen-operation/preempt-update? screen) screen y))

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

  ;; Cursor position.
  cursor-x
  cursor-y)

(define (make-matrix screen)
  (let ((matrix (%make-matrix))
	(x-size (screen-x-size screen))
	(y-size (screen-y-size screen)))
    (let ((contents (make-vector y-size))
	  (highlight (make-vector y-size))
	  (enable (make-boolean-vector y-size)))
      (do ((i 0 (fix:1+ i)))
	  ((fix:= i y-size))
	(vector-set! contents i (make-string x-size))
	(vector-set! highlight i (make-boolean-vector x-size)))
      (boolean-vector-fill! enable false)
      (set-matrix-contents! matrix contents)
      (set-matrix-highlight! matrix highlight)
      (set-matrix-enable! matrix enable))
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
	  (guarantee-display-line screen y)))
    (string-set! (vector-ref (matrix-contents new-matrix) y) x char)
    (boolean-vector-set! (vector-ref (matrix-highlight new-matrix) y)
			 x
			 highlight)))

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
    (boolean-vector-set! (vector-ref (matrix-highlight current-matrix) y)
			 x
			 highlight)
    (set-matrix-cursor-x! current-matrix cursor-x)
    (set-matrix-cursor-x! (screen-new-matrix screen) cursor-x)))

(define (screen-output-substring screen x y string start end highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'output-substring
				   x y (string-copy string) start end
				   highlight))
  (let ((new-matrix (screen-new-matrix screen)))
    (if (not (boolean-vector-ref (matrix-enable new-matrix) y))
	(begin
	  (boolean-vector-set! (matrix-enable new-matrix) y true)
	  (set-screen-needs-update?! screen true)
	  (guarantee-display-line screen y)))
    (substring-move-left! string start end
			  (vector-ref (matrix-contents new-matrix) y) x)
    (boolean-subvector-fill! (vector-ref (matrix-highlight new-matrix) y)
			     x (fix:+ x (fix:- end start)) highlight)))

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
    (boolean-subvector-fill! (vector-ref (matrix-highlight current-matrix) y)
			     x cursor-x highlight)
    (set-matrix-cursor-x! current-matrix cursor-x)
    (set-matrix-cursor-x! (screen-new-matrix screen) cursor-x)))

(define (guarantee-display-line screen y)
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen)))
    (if (boolean-vector-ref (matrix-enable current-matrix) y)
	(begin
	  (string-move! (vector-ref (matrix-contents current-matrix) y)
			(vector-ref (matrix-contents new-matrix) y))
	  (boolean-vector-move!
	   (vector-ref (matrix-highlight current-matrix) y)
	   (vector-ref (matrix-highlight new-matrix) y)))
	(begin
	  (string-fill! (vector-ref (matrix-contents new-matrix) y) #\space)
	  (boolean-vector-fill! (vector-ref (matrix-highlight new-matrix) y)
				false)))))

(define (screen-clear-rectangle screen xl xu yl yu highlight)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'clear-rectangle
				   xl xu yl yu highlight))
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen)))
    (let ((current-contents (matrix-contents current-matrix))
	  (current-highlight (matrix-highlight current-matrix))
	  (current-enable (matrix-enable current-matrix))
	  (new-contents (matrix-contents new-matrix))
	  (new-highlight (matrix-highlight new-matrix))
	  (new-enable (matrix-enable new-matrix)))
      (if (and (fix:= xl 0) (fix:= xu (screen-x-size screen)))
	  (do ((y yl (fix:1+ y)))
	      ((fix:= y yu))
	    (string-fill! (vector-ref new-contents y) #\space)
	    (boolean-vector-fill! (vector-ref new-highlight y) highlight)
	    (boolean-vector-set! new-enable y true))
	  (do ((y yl (fix:1+ y)))
	      ((fix:= y yu))
	    (let ((nl (vector-ref new-contents y))
		  (nh (vector-ref new-highlight y)))
	      (if (boolean-vector-ref new-enable y)
		  (begin
		    (substring-fill! nl xl xu #\space)
		    (boolean-subvector-fill! nh xl xu highlight))
		  (begin
		    (boolean-vector-set! new-enable y true)
		    (set-screen-needs-update?! screen true)
		    (if (boolean-vector-ref current-enable y)
			(begin
			  (string-move! (vector-ref current-contents y) nl)
			  (boolean-vector-move!
			   (vector-ref current-highlight y)
			   nh)
			  (substring-fill! nl xl xu #\space)
			  (boolean-subvector-fill! nh xl xu highlight))
			(begin
			  (string-fill! nl #\space)
			  (boolean-vector-fill! nh false)
			  (if highlight
			      (boolean-subvector-fill! nh xl xu
						       highlight))))))))))))

(define (screen-force-update screen)
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'force-update))
  (let ((y-size (screen-y-size screen))
	(current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen)))
    (terminal-clear-screen screen)
    (let ((current-contents (matrix-contents current-matrix))
	  (current-highlight (matrix-highlight current-matrix))
	  (current-enable (matrix-enable current-matrix))
	  (new-contents (matrix-contents new-matrix))
	  (new-highlight (matrix-highlight new-matrix))
	  (new-enable (matrix-enable new-matrix)))
      (do ((y 0 (fix:1+ y)))
	  ((fix:= y y-size))
	(if (boolean-vector-ref current-enable y)
	    (begin
	      (boolean-vector-set! current-enable y false)
	      (if (not (boolean-vector-ref new-enable y))
		  (begin
		    (string-move! (vector-ref current-contents y)
				  (vector-ref new-contents y))
		    (boolean-vector-move! (vector-ref current-highlight y)
					  (vector-ref new-highlight y))))))
	(string-fill! (vector-ref current-contents y) #\space)
	(boolean-vector-fill! (vector-ref current-highlight y) false))
      (boolean-vector-fill! current-enable true)))
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
			(highlight (matrix-highlight current-matrix)))
		    (do ((y (fix:-1+ (fix:- yu amount)) (fix:-1+ y))
			 (y* (fix:-1+ yu) (fix:-1+ y*)))
			((fix:< y yl))
		      (substring-move-left! (vector-ref contents y) xl xu
					    (vector-ref contents y*) xl)
		      (boolean-subvector-move-left!
		       (vector-ref highlight y) xl xu
		       (vector-ref highlight y*) xl)))
		  (if (eq? scrolled? 'CLEARED)
		      (matrix-clear-rectangle current-matrix
					      xl xu yl (fix:+ yl amount)
					      false))
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
			(highlight (matrix-highlight current-matrix)))
		    (do ((y yl (fix:1+ y))
			 (y* (fix:+ yl amount) (fix:1+ y*)))
			((fix:= y* yu))
		      (substring-move-left! (vector-ref contents y*) xl xu
					    (vector-ref contents y) xl)
		      (boolean-subvector-move-left!
		       (vector-ref highlight y*) xl xu
		       (vector-ref highlight y) xl)))
		  (if (eq? scrolled? 'CLEARED)
		      (matrix-clear-rectangle current-matrix
					      xl xu (fix:- yu amount) yu
					      false))
		  scrolled?))))))

(define (matrix-clear-rectangle matrix xl xu yl yu hl)
  (let ((contents (matrix-contents matrix))
	(highlight (matrix-highlight matrix)))
    (do ((y yl (fix:1+ y)))
	((fix:= y yu))
      (substring-fill! (vector-ref contents y) xl xu #\space)
      (boolean-subvector-fill! (vector-ref highlight y) xl xu hl))))

(define (with-screen-in-update screen display-style thunk)
  (without-interrupts
   (lambda ()
     (call-with-current-continuation
      (lambda (continuation)
	(let ((old-flag))
	  (dynamic-wind (lambda ()
			  (set! old-flag (screen-in-update? screen))
			  (set-screen-in-update?! screen
						  (or old-flag continuation)))
			(lambda ()
			  ((screen-operation/wrap-update! screen)
			   screen
			   (lambda ()
			     (and (thunk)
				  (screen-update screen display-style)))))
			(lambda ()
			  (set-screen-in-update?! screen old-flag)
			  (set! old-flag)
			  unspecific))))))))

(define (screen-update screen force?)
  ;; Update the actual terminal screen based on the data in `new-matrix'.
  ;; Value is #F if redisplay stopped due to pending input.
  ;; FORCE? true means do not stop for pending input.
  (if (screen-debug-trace screen)
      ((screen-debug-trace screen) 'screen screen 'update force?))
  (let ((current-matrix (screen-current-matrix screen))
	(new-matrix (screen-new-matrix screen))
	(y-size (screen-y-size screen)))
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
	      ((and (terminal-preempt-update? screen y)
		    ;; `terminal-preempt-update?' has side-effects,
		    ;; and it must be run regardless of `force?'.
		    (not force?)
		    (or (keyboard-active? 0)
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
    (let ((current-contents (vector-ref (matrix-contents current-matrix) y))
	  (current-highlight (vector-ref (matrix-highlight current-matrix) y))
	  (new-contents (vector-ref (matrix-contents new-matrix) y))
	  (new-highlight (vector-ref (matrix-highlight new-matrix) y)))
      (cond ((not (and (boolean-vector-ref (matrix-enable current-matrix) y)
		       (boolean-vector=? current-highlight new-highlight)))
	     (update-line-ignore-current screen y
					 new-contents new-highlight x-size))
	    ((string=? current-contents new-contents)
	     unspecific)
	    ((boolean-vector-all-elements? new-highlight false)
	     (update-line-no-highlight screen y current-contents new-contents))
	    (else
	     (update-line-ignore-current screen y
					 new-contents new-highlight x-size)))
      ;; Update current-matrix to contain the new line.
      (vector-set! (matrix-contents current-matrix) y new-contents)
      (vector-set! (matrix-highlight current-matrix) y new-highlight)
      (boolean-vector-set! (matrix-enable current-matrix) y true)
      ;; Move the old line to new-matrix so that it can be reused.
      (vector-set! (matrix-contents new-matrix) y current-contents)
      (vector-set! (matrix-highlight new-matrix) y current-highlight)
      (boolean-vector-set! (matrix-enable new-matrix) y false))))

(define (update-line-no-highlight screen y oline nline)
  (let ((x-size (screen-x-size screen)))
    (let ((olen (substring-non-space-end oline 0 x-size))
	  (nlen (substring-non-space-end nline 0 x-size)))
      (let ((len (fix:min olen nlen)))
	(let loop ((x 0))
	  (let ((x
		 (fix:+ x (substring-match-forward oline x len nline x len))))
	    (if (fix:= x len)
		(if (fix:< x nlen)
		    (terminal-output-substring screen x y
					       nline x nlen false))
		(let find-match ((x* (fix:1+ x)))
		  (cond ((fix:= x* len)
			 (if (fix:< x nlen)
			     (terminal-output-substring screen x y
							nline x nlen false)))
			((fix:= (vector-8b-ref oline x*)
				(vector-8b-ref nline x*))
			 (let ((n
				(substring-match-forward oline x* len
							 nline x* len)))
			   ;; Ignore matches of 4 characters or less.  The
			   ;; overhead of moving the cursor and drawing
			   ;; the characters is too much except for very
			   ;; slow terminals.
			   (if (fix:< n 5)
			       (find-match (fix:+ x* n))
			       (begin
				 (terminal-output-substring screen x y
							    nline x x* false)
				 (loop (fix:+ x* n))))))
			(else
			 (find-match (fix:1+ x*)))))))))
      (if (fix:< nlen olen)
	  (terminal-clear-line screen nlen y olen)))))

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
	 (let ((xe (substring-non-space-end nline 0 x-size)))
	   (if (fix:< 0 xe)
	       (terminal-output-substring screen 0 y nline 0 xe false))
	   (if (fix:< xe x-size)
	       (terminal-clear-line screen xe y x-size))))))

(define-integrable (fix:min x y) (if (fix:< x y) x y))
(define-integrable (fix:max x y) (if (fix:> x y) x y))

(define (substring-non-space-end string start end)
  (let ((index
	 (substring-find-previous-char-in-set string start end
					      char-set/not-space)))
    (if index
	(fix:1+ index)
	start)))

(define-integrable (substring-blank? string start end)
  (not (substring-find-next-char-in-set string start end char-set/not-space)))

(define char-set/not-space
  (char-set-invert (char-set #\space)))

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