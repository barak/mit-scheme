;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/wincom.scm,v 1.89 1989/03/14 08:03:47 cph Exp $
;;;
;;;	Copyright (c) 1987, 1989 Massachusetts Institute of Technology
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

;;;; Window Commands

(declare (usual-integrations))

(define-variable "Cursor Centering Point"
  "The distance from the top of the window at which to center the point.
This number is a percentage, where 0 is the window's top and 100 the bottom."
  35)

(define-variable "Cursor Centering Threshold"
  "If point moves offscreen by more than this many lines, recenter.
Otherwise, the screen is scrolled to put point at the edge it moved over."
  0)

(define-variable "Window Minimum Width"
  "Delete any window less than this wide.
Do not set this variable below 2."
  2)

(define-variable "Window Minimum Height"
  "Delete any window less than this high.
The modeline is not included in this figure.
Do not set this variable below 1."
  1)

(define-variable "Next Screen Context Lines"
  "Number of lines of continuity when scrolling by screenfuls."
  2)

(define-variable "Mode Line Inverse Video"
  "If true, the mode line is highlighted."
  true)

(define-command ("^R New Window" argument)
  "Choose new window putting point at center, top or bottom.
With no argument, chooses a window to put point at the center
\(\"Cursor Centering Point\" says where).
An argument gives the line to put point on;
negative args count from the bottom.
C-U as argument redisplays the line containing point."
  (let ((window (current-window)))
    (let ((size (window-y-size window)))
      (if (not argument)
	  (begin
	    (window-redraw! window false)
	    (update-window-screen! window true))
	  (window-scroll-y-absolute! window
				     (let ((n (remainder argument size)))
				       (if (negative? n)
					   (+ n size)
					   n)))))))

(define-command ("^R Move to Screen Edge" argument)
  "Jump to top or bottom of screen.
Like \\[^R New Window] except that point is changed instead of the window.
With no argument, jumps to the center, according to \"Cursor Centering Point\".
An argument specifies the number of lines from the top;
negative args count from the bottom."
  (let ((window (current-window)))
    (let ((mark
	   (or (window-coordinates->mark
		window 0
		(if (not argument)
		    (window-y-center window)
		    (let ((y-size (window-y-size window)))
		      (let ((n (remainder argument y-size)))
			(if (negative? n)
			    (+ n y-size)
			    n)))))
	       (window-coordinates->mark
		window 0
		(window-mark->y window
				(buffer-end (window-buffer window)))))))
      (set-current-point! (if (group-start? mark)
			      (group-start mark)
			      mark)))))

(define-command ("^R Next Screen" argument)
  "Move down to display next screenful of text.
With argument, moves window down that many lines (negative moves up).
Just minus as an argument moves up a full screen."
  (let ((window (current-window)))
    (scroll-window window
		   (standard-scroll-window-argument window argument 1))))

(define-command ("^R Previous Screen" argument)
  "Move up to display previous screenful of text.
With argument, moves window up that many lines (negative moves down).
Just minus as an argument moves down a full screen."
  (let ((window (current-window)))
    (scroll-window window
		   (standard-scroll-window-argument window argument -1))))

(define-command ("^R Next Several Screens" argument)
  "Move down to display next screenful of text.
With argument, move window down that many screenfuls (negative moves up).
Just minus as an argument moves up a full screen."
  (let ((window (current-window)))
    (scroll-window window
		   (multi-scroll-window-argument window argument 1))))

(define-command ("^R Previous Several Screens" argument)
  "Move up to display previous screenful of text.
With argument, move window down that many screenfuls (negative moves down).
Just minus as an argument moves down full screen."
  (let ((window (current-window)))
    (scroll-window window
		   (multi-scroll-window-argument window argument -1))))

(define (scroll-window window n #!optional limit)
  (if (if (negative? n)
	  (mark= (window-start-mark window)
		 (buffer-start (window-buffer window)))
	  (mark= (window-end-mark window)
		 (buffer-end (window-buffer window))))      ((if (default-object? limit) editor-error limit))
      (window-scroll-y-relative! window n)))

(define (standard-scroll-window-argument window argument factor)
  (* factor
     (let ((quantum
	    (- (window-y-size window)
	       (ref-variable "Next Screen Context Lines"))))
       (cond ((not argument) quantum)
	     ((command-argument-negative-only?) (- quantum))
	     (else argument)))))

(define (multi-scroll-window-argument window argument factor)
  (* factor
     (let ((quantum
	    (- (window-y-size window)
	       (ref-variable "Next Screen Context Lines"))))
       (cond ((not argument) quantum)
	     ((command-argument-negative-only?)
	      (- quantum))
	     (else (* argument quantum))))))

(define-command ("^R Screen Video" (argument 0))
  "Toggle the screen's use of inverse video.
With a positive argument, inverse video is forced.
With a negative argument, normal video is forced."
  (screen-inverse-video!
   (or (positive? argument)
       (not (or (negative? argument)
		(screen-inverse-video! false)))))
  (update-screens! true))

(define-command ("What Cursor Position")
  "Print various things about where cursor is.
Print the X position, the Y position,
the ASCII code for the following character,
point absolutely and as a percentage of the total file size,
and the virtual boundaries, if any."
  (let ((buffer (current-buffer))
	(point (current-point)))
    (let ((position (mark-index point))
	  (total (group-length (buffer-group buffer))))
      (message (if (group-end? point)
		   ""
		   (let ((char (mark-right-char point)))
		     (string-append "Char: " (char-name char)
				    " (0"
				    (number->string (char->ascii char)
						    '(HEUR (RADIX O S)
							   (EXACTNESS S)))
				    ") ")))
	       "point=" (write-to-string position)
	       " of " (write-to-string total)
	       "("
	       (write-to-string (if (zero? total)
				    0
				    (round (* 100 (/ position total)))))
	       "%) "
	       (let ((group (mark-group point)))
		 (let ((start (group-start-index group))
		       (end (group-end-index group)))
		   (if (and (zero? start) (= end total))
		       ""
		       (string-append "<" (write-to-string start)
				      " - " (write-to-string end)
				      "> "))))
	       "x=" (write-to-string (mark-column point))))))

;;;; Multiple Windows

(define-command ("^R Split Window Vertically" argument)
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally."
  (disallow-typein)
  (window-split-vertically! (current-window) argument))

(define-command ("^R Split Window Horizontally" argument)
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets
ARG lines.  No arg means split equally."
  (disallow-typein)
  (window-split-horizontally! (current-window) argument))

(define-command ("^R Enlarge Window Vertically" (argument 1))
  "Makes current window ARG lines bigger."
  (disallow-typein)
  (window-grow-vertically! (current-window) argument))

(define-command ("^R Shrink Window Vertically" (argument 1))
  "Makes current window ARG lines smaller."
  (disallow-typein)
  (window-grow-vertically! (current-window) (- argument)))

(define-command ("^R Enlarge Window Horizontally" (argument 1))
  "Makes current window ARG columns wider."
  (disallow-typein)
  (window-grow-horizontally! (current-window) argument))

(define-command ("^R Shrink Window Horizontally" (argument 1))
  "Makes current window ARG columns narrower."
  (disallow-typein)
  (window-grow-horizontally! (current-window) (- argument)))

(define-command ("^R Delete Window")
  "Delete the current window from the screen."
  (window-delete! (current-window)))

(define-command ("^R Delete Other Windows")
  "Make the current window fill the screen."
  (delete-other-windows (current-window)))

(define-command ("^R Other Window" argument)
  "Select the ARG'th different window."
  (select-window (other-window-interactive argument)))

(define (other-window-interactive n)
  (let ((window (other-window n)))
    (if (eq? window (current-window))
	(editor-error "No other window")
	window)))

(define (disallow-typein)
  (if (typein-window? (current-window))
      (editor-error "Not implemented for typein window")))

(define-command ("^R Scroll Other Window" argument)
  "Scroll text of next window up ARG lines, or near full screen if no arg."
  (let ((window (other-window-interactive 1)))
    (scroll-window window
		   (standard-scroll-window-argument window argument 1))))

(define-command ("^R Scroll Other Window Several Screens" argument)
  "Scroll other window up several screenfuls.
Specify the number as a numeric argument, negative for down.
The default is one screenful up.  Just minus as an argument
means scroll one screenful down."
  (let ((window (other-window-interactive 1)))
    (scroll-window window
		   (multi-scroll-window-argument window argument 1))))

;;;; Pop-up Buffers

(define-variable "Pop Up Windows"
  "If false, this disables the use of pop-up windows."
  true)

(define-variable "Preserve Window Arrangement"
  "If true, commands that normally change the window arrangement do not."
  false)

(define-variable "Split Height Threshold"
  "Pop-up windows prefer to split the largest window if it is this large.
If there is only one window, it is split regardless of this value."
  500)

(define-command ("Kill Pop Up Buffer")
  "Kills the most recently popped up buffer, if one exists.
Also kills any pop up window it may have created."
  (let ((buffer (object-unhash *previous-popped-up-buffer*))
	(window (object-unhash *previous-popped-up-window*)))
    (if (and window (window-visible? window))
	(window-delete! window))
    (if (and buffer (buffer-alive? buffer))
	(kill-buffer-interactive buffer)
	(editor-error "No previous pop up buffer"))))

(define *previous-popped-up-buffer* (object-hash false))
(define *previous-popped-up-window* (object-hash false))

(define (pop-up-buffer buffer #!optional select?)
  ;; If some new window is created by this procedure, it is returned
  ;; as the value.  Otherwise the value is false.
  (let ((select? (and (not (default-object? select?)) select?)))
    (define (pop-up-window window)
      (let ((window (window-split-vertically! window false)))
	(pop-into-window window)
	window))
    (define (pop-into-window window)
      (set-window-buffer! window buffer true)
      (if select? (select-window window))
      false)
    (if (< (ref-variable "Window Minimum Height") 2)
	(set-variable! "Window Minimum Height" 2))
    (let ((window
	   (let ((window (get-buffer-window buffer)))
	     (if window
		 (begin (set-window-point! window (buffer-point buffer))
			(if select? (select-window window))
			false)
		 (let ((limit (* 2 (ref-variable "Window Minimum Height"))))
		   (if (< (ref-variable "Split Height Threshold") limit)
		       (set-variable! "Split Height Threshold" limit))
		   (cond ((ref-variable "Preserve Window Arrangement")
			  (pop-into-window (largest-window)))
			 ((ref-variable "Pop Up Windows")
			  (or (let ((window (largest-window)))
				(and (>= (window-y-size window)
					 (ref-variable
					  "Split Height Threshold"))
				     (not
				      (window-has-horizontal-neighbor? window))
				     (pop-up-window window)))
			      (let ((window (lru-window))
				    (current (current-window)))
				(if (and (or (eq? window current)
					     (and (typein-window? current)
						  (eq? window
						       (window1+ window))))
					 (>= (window-y-size window) limit))
				    (pop-up-window window)
				    (pop-into-window window)))))
			 (else
			  (pop-into-window (lru-window)))))))))
      (set! *previous-popped-up-window* (object-hash window))
      (set! *previous-popped-up-buffer* (object-hash buffer))
      window)))

(define (get-buffer-window buffer)
  (let ((start (window0)))
    (if (eq? buffer (window-buffer start))
	start
	(let loop ((window (window1+ start)))
	  (and (not (eq? window start))
	       (if (eq? buffer (window-buffer window))
		   window
		   (loop (window1+ window))))))))

(define (largest-window)
  (let ((start (window0)))
    (define (loop window largest largest-area)
      (if (eq? window start)
	  largest
	  (let ((area (* (window-x-size window) (window-y-size window))))
	    (if (> area largest-area)
		(loop (window1+ window) window area)
		(loop (window1+ window) largest largest-area)))))
    (loop (window1+ start)
	  start
	  (* (window-x-size start) (window-y-size start)))))

(define (lru-window)
  (let ((start (window0)))
    (define (search-full-width window smallest smallest-time)
      (let ((next (window1+ window))
	    (time (window-select-time window)))
	(let ((consider-window?
	       (and (not (window-has-horizontal-neighbor? window))
		    (or (not smallest)
			(< time smallest-time)))))
	  (if (eq? window start)
	      (if consider-window?
		  window
		  (or smallest
		      (search-all next
				  start
				  (window-select-time start))))
	      (if consider-window?
		  (search-full-width next window time)
		  (search-full-width next smallest smallest-time))))))

    (define (search-all window smallest smallest-time)
      (if (eq? window start)
	  smallest
	  (let ((time (window-select-time window)))
	    (if (< time smallest-time)
		(search-all (window1+ window) window time)
		(search-all (window1+ window) smallest smallest-time)))))

    (search-full-width (window1+ start) false false)))

(define (delete-other-windows start)
  (define (loop window)
    (if (not (eq? window start))
	(begin (window-delete! window)
	       (loop (window1+ window)))))
  (loop (window1+ start)))