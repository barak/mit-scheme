;;; -*-Scheme-*-
;;;
;;;	$Id: os2term.scm,v 1.1 1994/12/19 19:46:29 cph Exp $
;;;
;;;	Copyright (c) 1994 Massachusetts Institute of Technology
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

;;;; OS/2 Presentation Manager Interface
;;; Package: (edwin screen os2-screen)

(declare (usual-integrations))

(define os2-display-type)
(define screen-list)
(define event-queue)
(define virtual-key-table)
(define signal-interrupts?)
(define event-descriptor)
(define previewer-registration)
(define reading-event?)
(define desktop-width)
(define desktop-height)

(define (initialize-package!)
  (set! os2-display-type
	(make-display-type 'PM
			   #t
			   (lambda () #t)
			   make-os2-screen
			   get-os2-input-operations
			   with-editor-interrupts-from-os2
			   with-os2-interrupts-enabled
			   with-os2-interrupts-disabled))
  (set! screen-list '())
  (set! event-queue (make-queue))
  (set! virtual-key-table (make-virtual-key-table))
  (set! event-descriptor (os2win-event-descriptor))
  unspecific)

(define (with-editor-interrupts-from-os2 receiver)
  (fluid-let ((reading-event? #f)
	      (signal-interrupts? #t)
	      (previewer-registration))
    (dynamic-wind (lambda ()
		    (preview-event-stream)
		    (set! desktop-width (os2win-desktop-width))
		    (set! desktop-height (os2win-desktop-height))
		    (os2win-set-state (os2win-console-wid) window-state:hide))
		  (lambda ()
		    (receiver (lambda (thunk) (thunk)) '()))
		  (lambda ()
		    (deregister-input-thread-event previewer-registration)))))

(define (with-os2-interrupts-enabled thunk)
  (with-signal-interrupts #t thunk))

(define (with-os2-interrupts-disabled thunk)
  (with-signal-interrupts #f thunk))

(define (with-signal-interrupts enabled? thunk)
  (let ((old))
    (dynamic-wind (lambda ()
		    (set! old signal-interrupts?)
		    (set! signal-interrupts? enabled?)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! enabled? signal-interrupts?)
		    (set! signal-interrupts? old)
		    unspecific))))

(define (make-os2-screen)
  (call-with-values open-window
    (lambda (state x-size y-size)
      (let ((screen
	     (make-screen state
			  os2-screen/beep
			  os2-screen/clear-line!
			  os2-screen/clear-rectangle!
			  os2-screen/clear-screen!
			  os2-screen/discard!
			  os2-screen/enter!
			  os2-screen/exit!
			  os2-screen/flush!
			  os2-screen/modeline-event!
			  #f
			  os2-screen/scroll-lines-down!
			  os2-screen/scroll-lines-up!
			  os2-screen/wrap-update!
			  os2-screen/write-char!
			  os2-screen/write-cursor!
			  os2-screen/write-substring!
			  8
			  x-size
			  y-size)))
	(set! screen-list (cons screen screen-list))
	screen))))

(define (open-window)
  (let ((wid (os2win-open "Edwin")))
    (let ((metrics (set-normal-font! wid current-font)))
      (os2win-set-colors wid
			 (face-foreground-color normal-face)
			 (face-background-color normal-face))
      (os2win-show-cursor wid #t)
      (os2win-show wid #t)
      (os2win-activate wid)
      (let ((w.h (os2win-get-size wid)))
	(let ((x-size (fix:quotient (car w.h) (font-metrics/width metrics)))
	      (y-size (fix:quotient (cdr w.h) (font-metrics/height metrics))))
	  (let ((size (fix:* x-size y-size)))
	    (values (make-screen-state wid
				       metrics
				       (car w.h)
				       (cdr w.h)
				       (make-string size #\space)
				       (make-vector size normal-face))
		    x-size
		    y-size)))))))

(define (os2-screen/beep screen)
  screen
  (os2win-beep 880 50))

(define (os2-screen/clear-line! screen x y first-unused-x)
  (let ((start (screen-char-index screen x y))
	(end (screen-char-index screen first-unused-x y))
	(face (screen-normal-face screen)))
    (substring-fill! (screen-char-map screen) start end #\space)
    (subvector-fill! (screen-face-map screen) start end face)
    (set-screen-face! screen face))
  (os2win-clear (screen-wid screen)
		(cxl->xl screen x)
		(cxh->xh screen first-unused-x)
		(cyh->yl screen (fix:+ y 1))
		(cyl->yh screen y)))

(define (os2-screen/clear-rectangle! screen xl xu yl yu highlight)
  (if (fix:< xl xu)
      (let ((char-map (screen-char-map screen))
	    (face-map (screen-face-map screen))
	    (face (screen-face screen highlight))
	    (x-size (screen-x-size screen))
	    (width (fix:- xu xl)))
	(do ((y yl (fix:+ y 1))
	     (start (screen-char-index screen xl yl) (fix:+ start x-size)))
	    ((fix:= y yu))
	  (let ((end (fix:+ start width)))
	    (substring-fill! char-map start end #\space)
	    (subvector-fill! face-map start end face)))
	(set-screen-face! screen face)
	(os2win-clear (screen-wid screen)
		      (cxl->xl screen xl) (cxh->xh screen xu)
		      (cyh->yl screen yu) (cyl->yh screen yl)))))

(define (os2-screen/clear-screen! screen)
  (let ((face (screen-normal-face screen)))
    (string-fill! (screen-char-map screen) #\space)
    (vector-fill! (screen-face-map screen) face)
    (set-screen-face! screen face))
  (os2win-clear (screen-wid screen)
		0 (screen-pel-width screen)
		0 (screen-pel-height screen)))

(define (os2-screen/discard! screen)
  (set! screen-list (delq! screen screen-list))
  (os2win-close (screen-wid screen)))

(define (os2-screen/enter! screen)
  screen
  unspecific)

(define (os2-screen/exit! screen)
  screen
  unspecific)

(define (os2-screen/flush! screen)
  screen
  unspecific)

(define (os2-screen/modeline-event! screen window type)
  screen window type
  unspecific)

(define (os2-screen/wrap-update! screen thunk)
  screen
  (thunk))

(define (os2-screen/write-cursor! screen x y)
  (os2win-move-cursor (screen-wid screen) (cx->x screen x) (cy->y screen y)))

(define (os2-screen/write-char! screen x y char highlight)
  (let ((char-map (screen-char-map screen))
	(index (screen-char-index screen x y))
	(face (screen-face screen highlight)))
    (string-set! char-map index char)
    (vector-set! (screen-face-map screen) index face)
    (set-screen-face! screen face)
    (os2win-write (screen-wid screen)
		  (cx->x screen x)
		  (fix:+ (cy->y screen y) (screen-char-descender screen))
		  char-map
		  index
		  (fix:+ index 1))))

(define (os2-screen/write-substring! screen x y string start end highlight)
  (let ((start* (screen-char-index screen x y))
	(face (screen-face screen highlight)))
    (%substring-move! string start end (screen-char-map screen) start*)
    (subvector-fill! (screen-face-map screen)
		     start*
		     (fix:+ start* (fix:- end start))
		     face)
    (set-screen-face! screen face)
    (os2win-write (screen-wid screen)
		  (cx->x screen x)
		  (fix:+ (cy->y screen y) (screen-char-descender screen))
		  string start end)))

(define use-scrolling? #t)

(define (os2-screen/scroll-lines-down! screen xl xu yl yu amount)
  (and use-scrolling?
       (begin
	 (let ((char-map (screen-char-map screen))
	       (face-map (screen-face-map screen))
	       (x-size (screen-x-size screen))
	       (width (fix:- xu xl))
	       (y-from (fix:- yu amount)))
	   (if (fix:= x-size width)
	       (let ((start (fix:* x-size yl))
		     (end (fix:* x-size y-from))
		     (start* (fix:* x-size (fix:+ yl amount))))
		 (%substring-move! char-map start end char-map start*)
		 (subvector-move-right! face-map start end face-map start*))
	       (let ((delta (fix:* x-size amount))
		     (end (screen-char-index screen xl (fix:- yl 1))))
		 (do ((from (screen-char-index screen xl (fix:- y-from 1))
			    (fix:- from x-size)))
		     ((fix:= from end))
		   (let ((from-end (fix:+ from width))
			 (to (fix:+ from delta)))
		     (%substring-move! char-map from from-end char-map to)
		     (subvector-move-right! face-map from from-end
					    face-map to))))))
	 (os2win-scroll (screen-wid screen)
			(cxl->xl screen xl)
			(cxh->xh screen xu)
			(cyh->yl screen (fix:- yu amount))
			(cyl->yh screen yl)
			0
			(fix:- 0 (fix:* amount (screen-char-height screen))))
	 'UNCHANGED)))

(define (os2-screen/scroll-lines-up! screen xl xu yl yu amount)
  (and use-scrolling?
       (begin
	 (let ((char-map (screen-char-map screen))
	       (face-map (screen-face-map screen))
	       (x-size (screen-x-size screen))
	       (width (fix:- xu xl))
	       (y-from (fix:+ yl amount)))
	   (if (fix:= x-size width)
	       (let ((start (fix:* x-size y-from))
		     (end (fix:* x-size yu))
		     (start* (fix:* x-size yl)))
		 (%substring-move! char-map start end char-map start*)
		 (subvector-move-left! face-map start end face-map start*))
	       (let ((delta (fix:* x-size amount))
		     (end (screen-char-index screen xl yu)))
		 (do ((from (screen-char-index screen xl y-from)
			    (fix:+ from x-size)))
		     ((fix:= from end))
		   (let ((from-end (fix:+ from width))
			 (to (fix:- from delta)))
		     (%substring-move! char-map from from-end char-map to)
		     (subvector-move-left! face-map from from-end
					   face-map to))))))
	 (os2win-scroll (screen-wid screen)
			(cxl->xl screen xl)
			(cxh->xh screen xu)
			(cyh->yl screen yu)
			(cyl->yh screen (fix:+ yl amount))
			0
			(fix:* amount (screen-char-height screen)))
	 'UNCHANGED)))

(define-integrable (screen-face screen highlight)
  (if highlight
      (screen-highlight-face screen)
      (screen-normal-face screen)))

(define (set-screen-face! screen face)
  (if (not (eq? face (screen-current-face screen)))
      (begin
	(os2win-set-colors (screen-wid screen)
			   (face-foreground-color face)
			   (face-background-color face))
	(set-screen-current-face! screen face))))

(define-structure face
  (foreground-color #f read-only #t)
  (background-color #f read-only #t))

(define current-font "4.System VIO")
(define normal-face (make-face #x000000 #xFFFFFF))
(define highlight-face (make-face #xFFFFFF #x000000))

(define-integrable (screen-normal-face screen) screen normal-face)
(define-integrable (screen-highlight-face screen) screen highlight-face)

(define (os2-screen/set-foreground-color! screen color)
  screen
  (set! normal-face
	(make-face color (face-background-color normal-face)))
  (set! highlight-face
	(make-face (face-foreground-color highlight-face) color))
  unspecific)

(define (os2-screen/set-background-color! screen color)
  screen
  (set! normal-face
	(make-face (face-foreground-color normal-face) color))
  (set! highlight-face
	(make-face color (face-background-color highlight-face)))
  unspecific)

(define (os2-screen/set-font! screen font)
  (set-screen-font-metrics! screen (set-normal-font! (screen-wid screen) font))
  (set! current-font font)
  (let ((resize (screen-resize-thunk screen)))
    (if resize
	(resize))))

(define (set-normal-font! wid font)
  (let ((metrics (os2win-set-font wid 1 font)))
    (if (not metrics)
	(error "Unknown font name:" font))
    (let ((width (font-metrics/width metrics))
	  (height (font-metrics/height metrics)))
      (os2win-set-grid wid width height)
      (os2win-shape-cursor wid width height
			   (fix:or CURSOR_SOLID CURSOR_FLASH)))
    metrics))

(define (os2-screen/set-size! screen x-size y-size)
  (os2win-set-size (screen-wid screen)
		   (fix:* x-size (screen-char-width screen))
		   (fix:* y-size (screen-char-height screen))))

(define (os2-screen/get-position screen)
  (let ((x.y (os2win-get-pos (screen-wid screen))))
    (values (car x.y)
	    (cdr x.y))))

(define (os2-screen/set-position! screen x y)
  (os2win-set-pos (screen-wid screen) x y))

(define (os2-screen/raise! screen)
  (os2win-set-state (screen-wid screen) window-state:top))

(define (os2-screen/lower! screen)
  (os2win-set-state (screen-wid screen) window-state:bottom))

(define (os2-screen/hide! screen)
  (os2win-set-state (screen-wid screen) window-state:hide))

(define (os2-screen/minimize! screen)
  (os2win-set-state (screen-wid screen) window-state:minimize))

(define (os2-screen/maximize! screen)
  (os2win-set-state (screen-wid screen) window-state:maximize))

(define (os2-screen/restore! screen)
  (os2win-set-state (screen-wid screen) window-state:restore))

(define (os2/desktop-width)
  desktop-width)

(define (os2/desktop-height)
  desktop-height)

(define-integrable (cx->x screen cx)
  ;; Returns leftmost pel of cell.
  (fix:* cx (screen-char-width screen)))

(define-integrable (cy->y screen cy)
  ;; Returns bottommost pel of cell.
  (cyl->yh screen (fix:+ cy 1)))

(define-integrable (cyl->yh screen cy)
  ;; Returns bottommost pel of cell above.
  (fix:* (fix:- (screen-y-size screen) cy) (screen-char-height screen)))

(define-integrable cxl->xl cx->x)
(define-integrable cxh->xh cx->x)
(define-integrable cyh->yl cyl->yh)

(define (x->cx screen x)
  (let ((cx (fix:quotient x (screen-char-width screen)))
	(xs (screen-x-size screen)))
    (if (fix:> cx xs)
	xs
	cx)))

(define (y->cy screen y)
  (let ((cy
	 (fix:- (fix:- (screen-y-size screen) 1)
		(fix:quotient y (screen-char-height screen)))))
    (if (fix:< cy 0)
	0
	cy)))

(define (xl->cxl screen xl)
  (let ((cx (fix:quotient xl (screen-char-width screen)))
	(xs (screen-x-size screen)))
    (if (fix:> cx xs)
	xs
	cx)))

(define (xh->cxh screen xh)
  (let ((cx
	 (let ((cw (screen-char-width screen)))
	   (let ((cx (fix:quotient xh cw)))
	     (if (fix:= 0 (fix:remainder xh cw))
		 cx
		 (fix:+ cx 1)))))
	(xs (screen-x-size screen)))
    (if (fix:> cx xs)
	xs
	cx)))

(define (yl->cyh screen yl)
  (let ((cy
	 (fix:- (screen-y-size screen)
		(fix:quotient yl (screen-char-height screen)))))
    (if (fix:< cy 0)
	0
	cy)))

(define (yh->cyl screen yh)
  (let ((cy
	 (let ((ch (screen-char-height screen)))
	   (let ((cy (fix:- (screen-y-size screen) (fix:quotient yh ch))))
	     (if (fix:= 0 (fix:remainder yh ch))
		 cy
		 (fix:- cy 1))))))
    (if (fix:< cy 0)
	0
	cy)))

(define-integrable (width->x-size screen width)
  (fix:quotient width (screen-char-width screen)))

(define-integrable (height->y-size screen height)
  (fix:quotient height (screen-char-height screen)))

(define-structure (os2-screen-state
		   (constructor
		    make-screen-state
		    (wid font-metrics pel-width pel-height char-map face-map))
		   (predicate screen-state?)
		   (conc-name screen-state/))
  (wid #f read-only #t)
  font-metrics
  (pel-width 0)
  (pel-height 0)
  (char-map "")
  (face-map '#())
  (current-face normal-face))

(define-integrable (screen-wid screen)
  (screen-state/wid (screen-state screen)))

(define-integrable (screen-font-metrics screen)
  (screen-state/font-metrics (screen-state screen)))

(define-integrable (set-screen-font-metrics! screen metrics)
  (set-screen-state/font-metrics! (screen-state screen) metrics))

(define-integrable (screen-pel-width screen)
  (screen-state/pel-width (screen-state screen)))

(define-integrable (set-screen-pel-width! screen width)
  (set-screen-state/pel-width! (screen-state screen) width))

(define-integrable (screen-pel-height screen)
  (screen-state/pel-height (screen-state screen)))

(define-integrable (set-screen-pel-height! screen height)
  (set-screen-state/pel-height! (screen-state screen) height))

(define-integrable (screen-char-map screen)
  (screen-state/char-map (screen-state screen)))

(define-integrable (set-screen-char-map! screen char-map)
  (set-screen-state/char-map! (screen-state screen) char-map))

(define-integrable (screen-face-map screen)
  (screen-state/face-map (screen-state screen)))

(define-integrable (set-screen-face-map! screen face-map)
  (set-screen-state/face-map! (screen-state screen) face-map))

(define-integrable (screen-current-face screen)
  (screen-state/current-face (screen-state screen)))

(define-integrable (set-screen-current-face! screen face)
  (set-screen-state/current-face! (screen-state screen) face))

(define-structure (font-metrics (type vector) (conc-name font-metrics/))
  (width #f read-only #t)
  (height #f read-only #t)
  (descender #f read-only #t))

(define-integrable (screen-char-width screen)
  (font-metrics/width (screen-font-metrics screen)))

(define-integrable (screen-char-height screen)
  (font-metrics/height (screen-font-metrics screen)))

(define-integrable (screen-char-descender screen)
  (font-metrics/descender (screen-font-metrics screen)))

(define-integrable (screen-char-index screen x y)
  (fix:+ (fix:* y (screen-x-size screen)) x))

(define (wid->screen wid)
  (let loop ((screens screen-list))
    (and (not (null? screens))
	 (if (fix:= wid (screen-wid (car screens)))
	     (car screens)
	     (loop (cdr screens))))))

(define (get-os2-input-operations screen)
  screen
  (let ((pending #f)
	(repeat 0))

    (define (halt-update?)
      (setup-pending 'IN-UPDATE)
      pending)

    (define (peek-no-hang)
      (setup-pending #f)
      pending)

    (define (peek)
      (setup-pending #t)
      pending)

    (define (read)
      (setup-pending #t)
      (let ((result pending))
	(if (fix:> repeat 1)
	    (set! repeat (fix:- repeat 1))
	    (set! pending #f))
	result))

    (define (setup-pending block?)
      (if (not pending)
	  (let loop ()
	    (let ((event (read-event block?)))
	      (cond ((not event)
		     (set! pending #f))
		    ((not (vector? event))
		     (let ((flag (process-change-event event)))
		       (if flag
			   (begin
			     (set! pending
				   (make-input-event
				    (if (eq? flag 'FORCE-RETURN)
					'RETURN
					'UPDATE)
				    update-screens!
				    #f))
			     (set! repeat 1))
			   (loop))))
		    ((fix:= event-type:key (event-type event))
		     (set! pending (translate-key-event event))
		     (set! repeat (key-event/repeat event))
		     (cond ((fix:= 0 repeat)
			    (set! pending #f))
			   ((and (char? pending)
				 (char=? pending #\BEL)
				 signal-interrupts?)
			    (set! pending #f)
			    (signal-interrupt!)))
		     (if (not pending)
			 (loop)))
		    (else
		     (set! pending (process-special-event event))
		     (if pending
			 (set! repeat 1)
			 (loop))))))))

    (values halt-update? peek-no-hang peek read)))

(define (read-event block?)
  (let loop ()
    (set! reading-event? #t)
    (let ((event
	   (if (queue-empty? event-queue)
	       (if (eq? 'IN-UPDATE block?)
		   (os2win-get-event #f)
		   (read-event-1 block?))
	       (dequeue!/unsafe event-queue))))
      (set! reading-event? #f)
      (if (and (vector? event)
	       (fix:= (vector-ref event 0) event-type:paint))
	  (begin
	    (process-paint-event event)
	    (loop))
	  event))))

(define (read-event-1 block?)
  (or (os2win-get-event #f)
      (let loop ()
	(let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	  (cond (inferior-thread-changes?
		 (set-interrupt-enables! interrupt-mask)
		 event:inferior-thread-output)
		(else
		 (let ((flag
			(test-for-input-on-descriptor event-descriptor
						      block?)))
		   (set-interrupt-enables! interrupt-mask)
		   (case flag
		     ((#F) #f)
		     ((PROCESS-STATUS-CHANGE) event:process-status)
		     ((INTERRUPT) (loop))
		     (else (read-event-1 block?))))))))))

(define (preview-event-stream)
  (set! previewer-registration
	(permanently-register-input-thread-event
	 event-descriptor
	 (current-thread)
	 (lambda ()
	   (if (not reading-event?)
	       (let ((event (os2win-get-event #f)))
		 (if event
		     (if (and signal-interrupts?
			      (vector? event)
			      (fix:= event-type:key (event-type event))
			      ;; This tests for CTRL on, ALT off, and
			      ;; not a virtual key:
			      (fix:= #x10
				     (fix:and #x32 (key-event/flags event)))
			      (let ((code (key-event/code event)))
				(or (fix:= code (char->integer #\G))
				    (fix:= code (char->integer #\g)))))
			 (begin
			   (clean-event-queue event-queue)
			   (signal-interrupt!))
			 (enqueue!/unsafe event-queue event))))))))
  unspecific)

(define (clean-event-queue queue)
  ;; Flush keyboard and mouse events from the input queue.  Other
  ;; events are harmless and must be processed regardless.
  (do ((events (let loop ()
		 (if (queue-empty? queue)
		     '()
		     (let ((event (dequeue!/unsafe queue)))
		       (if (and (vector? event)
				(let ((type (event-type event)))
				  (or (fix:= type event-type:button)
				      (fix:= type event-type:key))))
			   (loop)
			   (cons event (loop))))))
	       (cdr events)))
      ((null? events))
    (enqueue!/unsafe queue (car events))))

(define (signal-interrupt!)
  (editor-beep)
  (temporary-message "Quit")
  (^G-signal))

(define (translate-key-event event)
  (let ((code (key-event/code event))
	(flags (key-event/flags event)))
    (let ((control (if (fix:= 0 (fix:and flags KC_CTRL)) 0 2))
	  (meta (if (fix:= 0 (fix:and flags KC_ALT)) 0 1)))
      (let ((process-code
	     (lambda (code)
	       (if (and (fix:<= #o040 code) (not (fix:= 0 control)))
		   (make-char (fix:and code #o037) meta)
		   (make-char code (fix:or meta control))))))
	(if (fix:= 0 (fix:and flags KC_VIRTUALKEY))
	    (and (fix:< code #o200)
		 (process-code code))
	    (let ((key
		   (and (fix:< code (vector-length virtual-key-table))
			(vector-ref virtual-key-table code))))
	      (and key
		   (if (fix:fixnum? key)
		       (process-code key)
		       (make-special-key key (fix:or meta control))))))))))

(define (process-change-event event)
  (cond ((fix:= event event:process-output) (accept-process-output))
	((fix:= event event:process-status) (handle-process-status-changes))
	((fix:= event event:inferior-thread-output) (accept-thread-output))
	(else (error "Illegal change event:" event))))

(define (process-paint-event event)
  (let ((wid (event-wid event)))
    (let ((screen (wid->screen wid)))
      (if screen
	  (let ((cxl (xl->cxl screen (paint-event/xl event)))
		(cxh (xh->cxh screen (paint-event/xh event)))
		(cyl (yh->cyl screen (paint-event/yh event)))
		(cyh (yl->cyh screen (paint-event/yl event)))
		(char-map (screen-char-map screen))
		(face-map (screen-face-map screen))
		(x-size (screen-x-size screen))
		(char-height (screen-char-height screen)))
	    (if (fix:< cxl cxh)
		(let ((size (fix:- cxh cxl)))
		  (do ((cy cyl (fix:+ cy 1))
		       (y (fix:+ (cy->y screen cyl)
				 (screen-char-descender screen))
			  (fix:- y char-height))
		       (start (screen-char-index screen cxl cyl)
			      (fix:+ start x-size)))
		      ((fix:= cy cyh))
		    (let ((end (fix:+ start size)))
		      (let outer ((start start) (cxl cxl))
			(let ((face (vector-ref face-map start)))
			  (let inner ((index (fix:+ start 1)))
			    (if (or (fix:= index end)
				    (not (eq? face
					      (vector-ref face-map index))))
				(begin
				  (set-screen-face! screen face)
				  (os2win-write wid (cx->x screen cxl) y
						char-map start end)
				  (if (not (fix:= index end))
				      (outer index
					     (fix:+ cxl (fix:- index start)))))
				(inner (fix:+ index 1)))))))))))))))

(define (process-special-event event)
  (let ((handler (vector-ref event-handlers (event-type event)))
	(screen (wid->screen (event-wid event))))
    (and handler
	 screen
	 (handler screen event))))

(define event-handlers
  (make-vector number-of-event-types false))

(define-integrable (define-event-handler event-type handler)
  (vector-set! event-handlers event-type handler))

(define-event-handler event-type:button
  (lambda (screen event)
    (and (eq? button-event-type:down (button-event/type event))
	 (if (os2win-focus? (screen-wid screen))
	     (make-input-event 'BUTTON
			       execute-button-command
			       screen
			       (make-down-button (button-event/number event))
			       (x->cx screen (button-event/x event))
			       (y->cy screen (button-event/y event)))
	     (begin
	       (os2win-activate (screen-wid screen))
	       #f)))))

(define-event-handler event-type:close
  (lambda (screen event)
    event
    (and (not (screen-deleted? screen))
	 (make-input-event 'DELETE-SCREEN delete-screen! screen))))

(define-event-handler event-type:focus
  (lambda (screen event)
    (and (focus-event/gained? event)
	 (not (selected-screen? screen))
	 (make-input-event 'SELECT-SCREEN select-screen screen))))

(define-event-handler event-type:resize
  (lambda (screen event)
    (set-screen-pel-width! screen (resize-event/width event))
    (set-screen-pel-height! screen (resize-event/height event))
    (let ((thunk (screen-resize-thunk screen)))
      (and thunk
	   (make-input-event 'SET-SCREEN-SIZE
			     (lambda (screen)
			       (thunk)
			       (update-screen! screen #t))
			     screen)))))

(define (screen-resize-thunk screen)
  (let ((width (screen-pel-width screen))
	(height (screen-pel-height screen)))
    (let ((x-size (width->x-size screen width))
	  (y-size (height->y-size screen height)))
      (and (not (and (= x-size (screen-x-size screen))
		     (= y-size (screen-y-size screen))))
	   (lambda ()
	     (let ((size (fix:* x-size y-size)))
	       (set-screen-char-map! screen (make-string size #\space))
	       (set-screen-face-map!
		screen
		(make-vector size (screen-current-face screen))))
	     (set-screen-size! screen x-size y-size))))))

(define-event-handler event-type:visibility
  (lambda (screen event)
    (and (not (screen-deleted? screen))
	 (if (visibility-event/shown? event)
	     (begin
	       (set-screen-visibility! screen 'VISIBLE)	;don't really know
	       (make-input-event 'UPDATE update-screen! screen #t))
	     (begin
	       (set-screen-visibility! screen 'UNMAPPED)
	       (and (selected-screen? screen)
		    (let ((screen (other-screen screen #f)))
		      (and screen
			   (make-input-event 'SELECT-SCREEN
					     select-screen
					     screen)))))))))

(define (make-virtual-key-table)
  ;; Shift keys are commented out, causing them to be ignored.
  (let ((table (make-vector virtual-key-supremum #f)))
    (vector-set! table VK_BUTTON1	'BUTTON1)
    (vector-set! table VK_BUTTON2	'BUTTON2)
    (vector-set! table VK_BUTTON3	'BUTTON3)
    (vector-set! table VK_BREAK		'BREAK)
    (vector-set! table VK_BACKSPACE	(char-code #\rubout))
    (vector-set! table VK_TAB		(char-code #\tab))
    (vector-set! table VK_BACKTAB	'BACKTAB)
    (vector-set! table VK_NEWLINE	(char-code #\return))
    ;;(vector-set! table VK_SHIFT		'SHIFT)
    ;;(vector-set! table VK_CTRL		'CTRL)
    ;;(vector-set! table VK_ALT		'ALT)
    ;;(vector-set! table VK_ALTGRAF	'ALTGRAF)
    (vector-set! table VK_PAUSE		'PAUSE)
    ;;(vector-set! table VK_CAPSLOCK	'CAPS-LOCK)
    (vector-set! table VK_ESC		(char-code #\escape))
    (vector-set! table VK_SPACE		(char-code #\space))
    (vector-set! table VK_PAGEUP	'PAGE-UP)
    (vector-set! table VK_PAGEDOWN	'PAGE-DOWN)
    (vector-set! table VK_END		'END)
    (vector-set! table VK_HOME		'HOME)
    (vector-set! table VK_LEFT		'LEFT)
    (vector-set! table VK_UP		'UP)
    (vector-set! table VK_RIGHT		'RIGHT)
    (vector-set! table VK_DOWN		'DOWN)
    (vector-set! table VK_PRINTSCRN	'PRINT-SCREEN)
    (vector-set! table VK_INSERT	'INSERT)
    (vector-set! table VK_DELETE	'DELETE)
    ;;(vector-set! table VK_SCRLLOCK	'SCRL-LOCK)
    ;;(vector-set! table VK_NUMLOCK	'NUM-LOCK)
    (vector-set! table VK_ENTER		(char-code #\return))
    (vector-set! table VK_SYSRQ		'SYSRQ)
    (vector-set! table VK_F1		'F1)
    (vector-set! table VK_F2		'F2)
    (vector-set! table VK_F3		'F3)
    (vector-set! table VK_F4		'F4)
    (vector-set! table VK_F5		'F5)
    (vector-set! table VK_F6		'F6)
    (vector-set! table VK_F7		'F7)
    (vector-set! table VK_F8		'F8)
    (vector-set! table VK_F9		'F9)
    (vector-set! table VK_F10		'F10)
    (vector-set! table VK_F11		'F11)
    (vector-set! table VK_F12		'F12)
    (vector-set! table VK_F13		'F13)
    (vector-set! table VK_F14		'F14)
    (vector-set! table VK_F15		'F15)
    (vector-set! table VK_F16		'F16)
    (vector-set! table VK_F17		'F17)
    (vector-set! table VK_F18		'F18)
    (vector-set! table VK_F19		'F19)
    (vector-set! table VK_F20		'F20)
    (vector-set! table VK_F21		'F21)
    (vector-set! table VK_F22		'F22)
    (vector-set! table VK_F23		'F23)
    (vector-set! table VK_F24		'F24)
    (vector-set! table VK_ENDDRAG	'END-DRAG)
    (vector-set! table VK_CLEAR		'CLEAR)
    (vector-set! table VK_EREOF		'EREOF)
    (vector-set! table VK_PA1		'PA1)
    table))

(define-primitives
  (os2win-beep 2)
  (os2win-open 1)
  (os2win-close 1)
  (os2win-show 2)
  (os2win-write 6)
  (os2win-move-cursor 3)
  (os2win-shape-cursor 4)
  (os2win-show-cursor 2)
  (os2win-clear 5)
  (os2win-scroll 7)
  (os2win-invalidate 5)
  (os2win-set-font 3)
  (os2win-set-grid 3)
  (os2win-activate 1)
  (os2win-get-pos 1)
  (os2win-set-pos 3)
  (os2win-get-size 1)
  (os2win-set-size 3)
  (os2win-focus? 1)
  (os2win-set-state 2)
  (os2win-set-colors 3)
  (os2win-get-event 1)
  (os2win-event-ready? 1)
  (os2win-event-descriptor 0)
  (os2win-console-wid 0)
  (os2win-desktop-width 0)
  (os2win-desktop-height 0))

(define-integrable event:process-output -2)
(define-integrable event:process-status -3)
(define-integrable event:inferior-thread-output -4)

(define-integrable (event-type event) (vector-ref event 0))
(define-integrable (event-wid event) (vector-ref event 1))

(define-macro (define-event name type . slots)
  `(BEGIN
     (DEFINE-INTEGRABLE ,(symbol-append 'EVENT-TYPE: name) ,type)
     ,@(let loop ((slots slots) (index 2))
	 (if (null? slots)
	     '()
	     (cons `(DEFINE-INTEGRABLE
		      (,(symbol-append name '-EVENT/ (car slots)) EVENT)
		      (VECTOR-REF EVENT ,index))
		   (loop (cdr slots) (+ index 1)))))))

;; These must match "microcode/pros2pm.c"
(define-event button     0 number type x y flags)
(define-event close      1)
(define-event focus      2 gained?)
(define-event key        3 code flags repeat)
(define-event paint      4 xl xh yl yh)
(define-event resize     5 width height)
(define-event visibility 6 shown?)

(define-integrable number-of-event-types 7)

(define-integrable button-event-type:down 0)
(define-integrable button-event-type:up 1)
(define-integrable button-event-type:click 2)
(define-integrable button-event-type:double-click 3)

;;; Constants from OS/2 header file "pmwin.h":

(define-integrable CURSOR_SOLID		#x0000)
(define-integrable CURSOR_HALFTONE	#x0001)
(define-integrable CURSOR_FRAME		#x0002)
(define-integrable CURSOR_FLASH		#x0004)

(define-integrable VK_BUTTON1		#x01)
(define-integrable VK_BUTTON2		#x02)
(define-integrable VK_BUTTON3		#x03)
(define-integrable VK_BREAK		#x04)
(define-integrable VK_BACKSPACE		#x05)
(define-integrable VK_TAB		#x06)
(define-integrable VK_BACKTAB		#x07)
(define-integrable VK_NEWLINE		#x08)
(define-integrable VK_SHIFT		#x09)
(define-integrable VK_CTRL		#x0A)
(define-integrable VK_ALT		#x0B)
(define-integrable VK_ALTGRAF		#x0C)
(define-integrable VK_PAUSE		#x0D)
(define-integrable VK_CAPSLOCK		#x0E)
(define-integrable VK_ESC		#x0F)
(define-integrable VK_SPACE		#x10)
(define-integrable VK_PAGEUP		#x11)
(define-integrable VK_PAGEDOWN		#x12)
(define-integrable VK_END		#x13)
(define-integrable VK_HOME		#x14)
(define-integrable VK_LEFT		#x15)
(define-integrable VK_UP		#x16)
(define-integrable VK_RIGHT		#x17)
(define-integrable VK_DOWN		#x18)
(define-integrable VK_PRINTSCRN		#x19)
(define-integrable VK_INSERT		#x1A)
(define-integrable VK_DELETE		#x1B)
(define-integrable VK_SCRLLOCK		#x1C)
(define-integrable VK_NUMLOCK		#x1D)
(define-integrable VK_ENTER		#x1E)
(define-integrable VK_SYSRQ		#x1F)
(define-integrable VK_F1		#x20)
(define-integrable VK_F2		#x21)
(define-integrable VK_F3		#x22)
(define-integrable VK_F4		#x23)
(define-integrable VK_F5		#x24)
(define-integrable VK_F6		#x25)
(define-integrable VK_F7		#x26)
(define-integrable VK_F8		#x27)
(define-integrable VK_F9		#x28)
(define-integrable VK_F10		#x29)
(define-integrable VK_F11		#x2A)
(define-integrable VK_F12		#x2B)
(define-integrable VK_F13		#x2C)
(define-integrable VK_F14		#x2D)
(define-integrable VK_F15		#x2E)
(define-integrable VK_F16		#x2F)
(define-integrable VK_F17		#x30)
(define-integrable VK_F18		#x31)
(define-integrable VK_F19		#x32)
(define-integrable VK_F20		#x33)
(define-integrable VK_F21		#x34)
(define-integrable VK_F22		#x35)
(define-integrable VK_F23		#x36)
(define-integrable VK_F24		#x37)
(define-integrable VK_ENDDRAG		#x38)
(define-integrable VK_CLEAR		#x39)
(define-integrable VK_EREOF		#x3A)
(define-integrable VK_PA1		#x3B)
(define-integrable virtual-key-supremum #x3C)

(define-integrable KC_NONE		#x0000)
(define-integrable KC_CHAR		#x0001)
(define-integrable KC_VIRTUALKEY	#x0002)
(define-integrable KC_SCANCODE		#x0004)
(define-integrable KC_SHIFT		#x0008)
(define-integrable KC_CTRL		#x0010)
(define-integrable KC_ALT		#x0020)
(define-integrable KC_KEYUP		#x0040)
(define-integrable KC_PREVDOWN		#x0080)
(define-integrable KC_LONEKEY		#x0100)
(define-integrable KC_DEADKEY		#x0200)
(define-integrable KC_COMPOSITE		#x0400)
(define-integrable KC_INVALIDCOMP	#x0800)
(define-integrable KC_TOGGLE		#x1000)
(define-integrable KC_INVALIDCHAR	#x2000)

(define-integrable window-state:top        0)
(define-integrable window-state:bottom     1)
(define-integrable window-state:show       2)
(define-integrable window-state:hide       3)
(define-integrable window-state:activate   4)
(define-integrable window-state:deactivate 5)
(define-integrable window-state:minimize   6)
(define-integrable window-state:maximize   7)
(define-integrable window-state:restore    8)