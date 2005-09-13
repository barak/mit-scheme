;;; -*- Scheme -*-

#| This file defines the SCXL library for interfacing with X.  It is
similar to Joel Bartlett's Scheme-to-C X library, except that it also
provides support for cleaning up X objects when the corresponding
Scheme objects are garbage collected.  The low-level primitives (e.g.
%XDrawline, %XPending) are Scheme entries to the corresponding X
library rooutines.  Microcode support for these is defined in the file
scxl.c.  Users should almost never call these routines directly, but
instead use the SCXL level routines (e.g., XDrawline, XPending), which
operate on Scheme objects rather than bare addresses.

We are writing this library for use in UITK (called from mit-xhooks).
We have tried to organize it so that it can be used as a general X
library, but this has not been really tested. |#

;;;; Hooks that the library calls -- these should be redefined by
;;;; systems that use the library

#| FLUSH-DISPLAY-HOOK is called at the end of every SCXL command that
might require a display flush.  In this file, it is defined as a
no-op.  The UITK library redefines it to wake the thread responsible
for handling this stuff. |#

(define (flush-display-hook)
  '(when not under UITK this does nothing)
  '(replaced by mit-xhooks))

#| *XCLOSEDISPLAYCALLBACKS* is a list of thunks that should be run
before a display is closed.  |#


#| SCXL Primitives operate on Scheme objects that are "wrapped around"
cells containing the bare numbers that represent X server objects, via
a call to SCXL-WRAP.  This permits us to perform finalization of X
objects when the Scheme representatives are garbage-collected using
the protection list mechanism described below. We use cells containing
the numbers, rather than the numbers themselves, to permit these "bare
objects" to be shared (e.g., in protection lists) and mutated (e.g.,
marked as destroyed).

One exception to the wrapping convention is that X events are
represented at the lowest level as Scheme strings, and so we needn't
worry about their garbage collection.  These low-level events (called
OS-events in UITK) are components in higher-level UITK event structures.

We maintain a list of strong dependents for these objects to represent
facts like "if you are holding on to this graphics context, then you
must also hold on to its display."  This prevents the GC from
prematurely releasing displays (and other objects).

|#

;;;; SCXL Wrapping and Unwrapping

;;; create a wrapped object and (optionally) place it on a protection
;;; list for GC finalization.  The wrapped object may also have
;;; additional stuff associated with it (for example, a display will
;;; contain information used to clean up its windows and fonts)
;;; The "bare object" kept on the protection list is held in a cell to
;;; allow for mutation (in particular, to mark things as being destroyed).

(define (SCXL-WRAP protection-list type object strong-dependents . rest)
  (let* ((cell (make-cell object))
	 (result (make-scxl-wrapper type cell strong-dependents rest)))
    (if protection-list
	(add-to-protection-list! protection-list result cell))
    result))

(define (SCXL-WRAPPED? obj) (scxl-wrapper? obj))

(define (type-check-wrapped-object type object)
  (if (SCXL-DESTROYED? object)
      (error "attempt to reference destroyed object" object)
      (if (and (SCXL-WRAPPED? object)
	       (eq? (scxl-wrapper.type object) type))
	  'OK
	  (error "wrong type wrapped-object" type object))))

(define (is-type-wrapped-object type)
  (lambda (object)
    (if (SCXL-DESTROYED? object)
	(error "attempt to reference destroyed object" object)
	(and (SCXL-WRAPPED? object)
	     (eq? (scxl-wrapper.type object) type)))))

;;;  (SCXL-UNWRAP
;;;     (SCXL-WRAP protection-list type object dependents data1 data2 ...)
;;;        (lambda (object . data-values) ....))

(define (SCXL-UNWRAP wrapped receiver)
  ;; Note: this doesn't return the strong dependents
  (apply receiver (cell-contents (scxl-wrapper.wrapped-object wrapped))
	          (scxl-wrapper.other-stuff wrapped)))

#| A destroyed object is the Scheme representitive of an X object that
has been destroyed.  It is generally an error to attempt to use a
destroyed object any SCXL primitive that has the effect of destroying
a resource on the server will call SCXL-DESTROY! on the corresponding
Scheme representative |#

(define (SCXL-DESTROY! obj)
  (if (scxl-destroyed? obj)
      'done
      (begin
	(set-cell-contents! (scxl-wrapper.wrapped-object obj) #F)
	(set-scxl-wrapper.other-stuff!
	 obj
	 (make-list (length (scxl-wrapper.other-stuff obj))
		    #F))
	(set-scxl-wrapper.strong-dependents! obj '()))))

(define (SCXL-DESTROYED? obj)
  (and (scxl-wrapped? obj)
       (eq? (cell-contents (scxl-wrapper.wrapped-object obj)) #F)))

#| wrap-with-SCXL-DESTROY! runs some core procedure on a wrapped
object and then destroys the object.  The procedure will not be run if
the object is already destroyed.  The procedure may also have
arguments other than the object to be destroyed.  Nargs is the number
of args to the core procedure.  Arg-num is the number of the arg that
is the object to be destroyed. |#

(define (wrap-with-SCXL-DESTROY! nargs arg-num core)
  ;; (declare (integrable core))
  (cond ((= nargs 1)
	 (lambda (arg)
	   arg-num			; Not used
	   (if (not (SCXL-WRAPPED? arg))
	       (error "not a wrapped object: wrap-with-SCXL-DESTROY" arg))
	   (if (not (SCXL-DESTROYED? arg))
	       (begin
		 (core arg)
		 (SCXL-DESTROY! arg))
	       #T)))
	((= nargs 2)
	 (lambda (arg1 arg2)
	   (define interesting-arg (if (= arg-num 0) arg1 arg2))
	   (if (not (SCXL-WRAPPED? interesting-arg))
	       (error "not a wrapped object: wrap-with-SCXL-DESTROY" interesting-arg))
	   (if (not (SCXL-DESTROYED? interesting-arg))
	       (begin (core arg1 arg2)
		      (SCXL-DESTROY! interestring-arg))
	       #T)))
	((= nargs 3)
	 (lambda (arg1 arg2 arg3)
	   (define interesting-arg
	     (cond ((= arg-num 0) arg1)
		   ((= arg-num 1) arg2)
		   (else arg3)))
	   (if (not (SCXL-WRAPPED? interesting-arg))
	       (error "not a wrapped object: wrap-with-SCXL-DESTROY" interesting-arg))
	   (if (not (SCXL-DESTROYED? interesting-arg))
	       (begin (core arg1 arg2 arg3)
		      (SCXL-DESTROY! interestring-arg))
	       #T)))
	(else
	 (lambda args
	   (define interesting-arg
	     (list-ref args arg-num))
	   (if (not (SCXL-WRAPPED? interesting-arg))
	       (error "not a wrapped object: wrap-with-SCXL-DESTROY" interesting-arg))
	   (if (not (SCXL-DESTROYED? interesting-arg))
	       (begin (apply core args)
		      (SCXL-DESTROY! interestring-arg))
	       #T)))))

;;;; Protection lists

#| A protection list is a list of weak pairs (scheme-object . microcode-object) 
the list is scanned at GC time so the system can finalize the
microcode objects whose associated Scheme objects have been GC'd away |#

(define (make-protection-list)
  (list 'PROTECTION-LIST))

(define (add-to-protection-list! list scheme-object microcode-object)
  (with-absolutely-no-interrupts
   (lambda ()
     (set-cdr! list
	       (cons (weak-cons scheme-object microcode-object)
		     (cdr list))))))

(define (remove-from-protection-list! list scheme-object)
  (with-absolutely-no-interrupts
   (lambda ()
     (let loop ((associations (cdr list)) (previous list))
       (if (not (null? associations))
	   (if (eq? scheme-object (weak-pair/car? (car associations)))
	       (set-cdr! previous (cdr associations))
	       (loop (cdr associations) associations)))))))

(define (clean-lost-protected-objects list cleaner)
  (let ((to-be-cleaned
	 (with-absolutely-no-interrupts
	  (lambda ()
	    (let loop ((result '())
		       (associations (cdr list))
		       (previous list))
	      (if (null? associations)
		  result
		  (if (weak-pair/car? (car associations))
		      (loop result (cdr associations) associations)
		      (let ((next (cdr associations)))
			(set-cdr! previous next)
			;; Re-use associations so we don't CONS
			(set-car! associations (weak-cdr (car associations)))
			(set-cdr! associations result)
			(loop associations next previous)))))))))
    (for-each cleaner to-be-cleaned)))

;;; In general, the microcode objects in SCXL are held in cells.
;;; clean-lost-celled-objects clears the cell and runs the cleaner on the
;;; object in the cell.

(define (clean-lost-celled-objects protection-list fn)
  (clean-lost-protected-objects
   protection-list
   (lambda (cell)
     (let ((obj (atomic-read-and-clear-cell! cell)))
       (if obj (fn obj))))))

(define (atomic-read-and-clear-cell! cell)
  (with-absolutely-no-interrupts
   (lambda ()
     (let ((result (cell-contents cell)))
       (set-cell-contents! cell #F)
       result))))

(define (search-protection-list list predicate)
  (with-absolutely-no-interrupts
   (lambda ()
     (let loop ((associations (cdr list)))
       (and (not (null? associations))
	    (let ((scheme-object (weak-car (car associations))))
	      (if (and scheme-object (predicate scheme-object))
		  scheme-object
		  (loop (cdr associations)))))))))

(define (find-in-protection-list list scheme-element)
  ;; Returns the pair whose weak-car is scheme-element
  (with-absolutely-no-interrupts
   (lambda ()
     (let loop ((associations (cdr list)))
       (and (not (null? associations))
	    (let ((scheme-object (weak-car (car associations))))
	      (if (and scheme-object (eq? scheme-element scheme-object))
		  (car associations)
		  (loop (cdr associations)))))))))

(define (protection-list-referenced-elements list)
  ;; Returns a list of the Scheme-visible objects which are still
  ;; referenced
  (with-absolutely-no-interrupts
   (lambda ()
     (let loop ((associations (cdr list)))
       (cond ((null? associations)
	      '())
	     ((weak-pair/car? (car associations))
	      (cons (weak-car (car associations))
		    (loop (cdr associations))))
	     (else
	      (loop (cdr associations))))))))

(define (protection-list-all-elements
	 list dereference-ucode-object-fn)
  ;; Returns a mixed list: returns the Scheme-visible object if it 
  ;; is still referenced.  Otherwise returns the associated microcode
  ;; reference.
  (with-absolutely-no-interrupts
   (lambda ()
     (let loop ((associations (cdr list)))
       (cond ((null? associations)
	      '())
	     ((weak-pair/car? (car associations))
	      (cons (weak-car (car associations))
		    (loop (cdr associations))))
	     (else
	      (cons (dereference-ucode-object-fn
		     (weak-cdr (car associations)))
		    (loop (cdr associations)))))))))

;;; Protection lists for SCXL objects

(define display-protection-list 'INITIALIZED-LATER)
(define region-protection-list 'INITIALIZED-LATER)


;;;; Standard wrapping procedures

;;; A display is wrapped along with protection lists for its windows,
;;; fonts, etc.  We need to keep track of these because when the
;;; display is closed, these objects will be destroyed, soe need to
;;; mark the Scheme representitives as destroyed.

(define (wrap-display xdisplay)
  (define (find-default-colormaps wrapped-display)
    (let ((nscreens (%XScreenCount xdisplay)))
      (let ((vect (make-vector (+ nscreens 1) #F)))
	(do ((this-screen 0 (+ this-screen 1)))
	    ((= this-screen nscreens) vect)
	  (vector-set! vect this-screen
		       (wrap-colormap
			wrapped-display
			(%XDefaultColormap xdisplay this-screen)))))))
  (let ((me
	 (SCXL-WRAP display-protection-list
		    'scxl-display
		    xdisplay
		    '()			; No strong dependents
		    (make-protection-list) ; Windows
		    (make-protection-list) ; Fonts (not used now)
		    (make-protection-list) ; Colormaps
		    (make-protection-list) ; GCs
		    #F			; Later ...
		    #F)))		; Later ...
    (let ((stuff (scxl-wrapper.other-stuff me)))
      (set-cdr! (list-tail stuff 3)
		(list
		 (wrap-window me (%XDefaultRootWindow xdisplay))
		 (Find-Default-Colormaps me))))
    me))

(define (scxl-display? object)
  (and (SCXL-WRAPPED? object)
       (eq? (scxl-wrapper.type object) 'scxl-display)))

(define (unwrap-display dsp receiver)
  ;; Anyone who uses this must know the order of the protection lists
  ;; within a display object!
  (type-check-wrapped-object 'scxl-display dsp)
  (SCXL-UNWRAP dsp receiver))

(define (display/display user-visible-display)
  (type-check-wrapped-object 'scxl-display user-visible-display)
  (SCXL-UNWRAP user-visible-display
	       (lambda (display window-list font-list colormap-list gc-list
				default-root-window default-colormaps)
		 window-list font-list colormap-list gc-list
		 default-root-window default-colormaps ; Not used
		 display)))

(define (display/window-list user-visible-display)
  (type-check-wrapped-object 'scxl-display user-visible-display)
  (SCXL-UNWRAP user-visible-display
	       (lambda (display window-list font-list colormap-list gc-list
				default-root-window default-colormaps)
		 display font-list colormap-list gc-list
		 default-root-window default-colormaps ; Not used
		 window-list)))

(define (display/font-list user-visible-display)
  (type-check-wrapped-object 'scxl-display user-visible-display)
  (error "display/font-list: Fonts aren't implemented yet."))

(define (display/colormap-list user-visible-display)
  (type-check-wrapped-object 'scxl-display user-visible-display)
  (SCXL-UNWRAP user-visible-display
	       (lambda (display window-list font-list colormap-list gc-list
				default-root-window default-colormaps)
		 display window-list font-list gc-list
		 default-root-window default-colormaps ; Not used
		 colormap-list)))

(define (display/gc-list user-visible-display)
  (type-check-wrapped-object 'scxl-display user-visible-display)
  (SCXL-UNWRAP user-visible-display
	       (lambda (display window-list font-list colormap-list gc-list
				default-root-window default-colormaps)
		 display window-list font-list colormap-list
		 default-root-window default-colormaps ; Not used
		 gc-list)))

(define (display/screen/default-color-map user-visible-display)
  (type-check-wrapped-object 'scxl-display user-visible-display)
  (SCXL-UNWRAP user-visible-display
	       (lambda (display window-list font-list colormap-list gc-list
				default-root-window default-colormaps)
		 display window-list font-list colormap-list gc-list
		 default-root-window	; Not used
		 default-colormaps)))

(define (display/default-root-window user-visible-display)
  (type-check-wrapped-object 'scxl-display user-visible-display)
  (SCXL-UNWRAP user-visible-display
	       (lambda (display window-list font-list colormap-list gc-list
				default-root-window default-colormaps)
		 display window-list font-list colormap-list gc-list
		 default-colormaps ; Not used
		 default-root-window)))

;;; alternate name
;;; in general, these ->names are used to extract the bare X pointers
;;; from the SCXL objects
(define ->xdisplay display/display)

;;; notice that windows, fonts, etc. are wrapped with a protection
;;; list for their associated displays.  We are not making a separate
;;; protection list for windows, because we aren't keeping track of
;;; the window hierarchy pointers that X maintains.  Therefore we
;;; cannot safely destroy a window just because its Scheme
;;; representitive disappears.  On the other other hand, when the
;;; display is closed, all the wondows go away, so we destroy their
;;; Scheme represeatitives.

(define (wrap-window display window)
  (SCXL-WRAP (display/window-list display)
	     'scxl-window
	     window
	     display))			; Strong dependent

(define (window/window user-visible-window)
  (type-check-wrapped-object 'scxl-window user-visible-window)
  (SCXL-UNWRAP user-visible-window (lambda (window) window)))

(define ->xwindow window/window)

#| Fonts aren't implemented yet.  They have to be protected both on
   the display and the graphics context to correctly reflect the
   pointers maintained by X.

(define (wrap-font display font)
  (SCXL-WRAP (display/font-list display) 'scxl-font
	     font
	     display))			; Strong dependent

(define font? (is-type-wrapped-object 'scxl-font))

(define (font/font font)
  (type-check-wrapped-object 'scxl-font font)
  (SCXL-UNWRAP font (lambda (font) font)))

(define ->xfont font/font)

|#

;;; NOTE: GCs maintain a lot of state in C.  Their Scheme shadows
;;; should have strong dependencies on these things, like the XRegion,
;;; Foreground and Background Pixel, font, etc.

(define (wrap-graphics-context display gc)
  (SCXL-WRAP (display/gc-list display)
	     'scxl-graphics-context
	     GC
	     display			; Strong dependent
	     (make-cell #F)))		; No region

(define (gc/gc gc)
  (type-check-wrapped-object 'scxl-graphics-context gc)
  (SCXL-UNWRAP gc (lambda (gc region-cell) region-cell gc)))

(define (gc/region gc)
  (type-check-wrapped-object 'scxl-graphics-context gc)
  ;; Note gc/region returns a wrapped object
  (cell-contents (SCXL-UNWRAP gc
			      (lambda (gc region-cell)
				gc	; Unused
				region-cell))))

(define (set-gc/region! gc region)
  (type-check-wrapped-object 'scxl-graphics-context gc)
  (set-cell-contents!
   (SCXL-UNWRAP gc
		(lambda (gc region-cell)
		  gc			; Unused
		  region-cell))
   region)
  'MUNGED)

(define (->xgc user-visible-gc)
  (type-check-wrapped-object 'scxl-graphics-context user-visible-gc)
  (cond ((SCXL-WRAPPED? user-visible-gc)
	 (gc/gc user-visible-gc))
	((number? user-visible-gc) user-visible-gc)
	(else (error "->XGc: not a gc" user-visible-gc))))

(define (wrap-colormap display colormap)
  (SCXL-WRAP (display/colormap-list display)
	     'scxl-colormap
	     colormap
	     display			; Strong dependent
	     (make-protection-list)))	; Pixel list

(define (colormap/colormap colormap)
  (type-check-wrapped-object 'scxl-colormap colormap)
  (SCXL-UNWRAP colormap
	       (lambda (colormap pixels)
		 pixels			; Not used
		 colormap)))

(define (colormap/pixel-list colormap)
  (type-check-wrapped-object 'scxl-colormap colormap)
  (SCXL-UNWRAP colormap
	       (lambda (colormap pixels)
		 colormap		; Not used
		 pixels)))

(define ->xcolormap colormap/colormap)

(define (wrap-pixel display colormap pixel)
  (SCXL-WRAP (colormap/pixel-list colormap)
	     'scxl-color
	     pixel
	     (list display colormap)))	; Strong dependents

(define color? (is-type-wrapped-object 'scxl-color))

(define (pixel/pixel pixel)
  (type-check-wrapped-object 'scxl-color pixel)
  (SCXL-UNWRAP pixel (lambda (pixel) pixel)))

(define ->xpixel pixel/pixel)

(define (wrap-region region)
  (SCXL-WRAP region-protection-list
	     'scxl-region
	     region
	     #F))			; No strong dependents

(define (region/region region)
  (type-check-wrapped-object 'scxl-region region)
  (SCXL-UNWRAP region (lambda (region) region)))

(define ->xregion region/region)

#|  *****************************
Debugging kludge
This will print a whole lot of crap.  Break glass in case of emergency only.

(define D_EVAL			0)
(define D_HEX_INPUT		1)
(define D_FILE_LOAD		2)
(define D_RELOC			3)
(define D_INTERN		4)
(define D_CONT			5)
(define D_PRIMITIVE		6)
(define D_LOOKUP		7)
(define D_DEFINE		8)
(define D_GC			9)
(define D_UPGRADE		10)
(define D_DUMP			11)
(define D_TRACE_ON_ERROR	12)
(define D_PER_FILE		13)
(define D_BIGNUM		14)
(define D_FLUIDS		15)

(define *spew-crap-out?* #T)


(define check-space
  (let ((set-debug-flags! (make-primitive-procedure 'set-debug-flags!)))
    (lambda ()
      (let* ((status (gc-space-status))
	     (free (vector-ref status 5))
	     (top (vector-ref status 6))
	     (space (- top free)))
	(write-line `(space ,space))
	(if (< space 2800000)
	    (if *spew-crap-out?*
		(with-absolutely-no-interrupts
		 (lambda ()
		   (debug-print 'space-low!)
		   (set-debug-flags! D_EVAL #T)
		   (set! *spew-crap-out?* #F)
		   'OK))
		(begin
		  (debug-print 'end 'print)
		  (set-debug-flags! D_EVAL #F)
		  'OK)))))))

*****************************************|#


#| Finalization of SCXL objects.  In UITK, this is scheduled to be
done by the UITK thread.  Other systems may schedule it to be done
differently, or done immediately.  If this is scheduled in the GC, the
assumptions about (not) interrupt locking the protection lists may be
incorrect.  WATCH OUT!

Note that this daemon has to call the lowest level % primitives rather
than the wrapped version for lost objects in the cases where the
wrappers have been lost and we have only the X pointers remianing.
|#

(define (close-lost-displays-daemon)
  ;; Step one: clean up any displays that have GCed away
  (clean-lost-celled-objects display-protection-list XCloseDisplayByNumber)
  ;; Step two: clean up any regions that have GCed away
  (clean-lost-celled-objects region-protection-list %XDestroyRegion)
  ;; Step three: run through all displays that we DO have
  ;; handles on, and release fonts/colomaps/gcs that we do
  ;; NOT have handles on.
  ;; *NOTE*: We do >>not<< close windows that we've lost handles on,
  ;; because X maintains gazillions of pointers to them that we aren't
  ;; tracking.
  (for-each
   cleanup-vanished-objects-for-display
   (protection-list-referenced-elements display-protection-list)))

(define (cleanup-vanished-objects-for-display display)
  (if (not (SCXL-DESTROYED? display))
      (unwrap-display
       display
       (lambda (display-number windows fonts colormaps gcs
			       default-root-window default-colormaps)
	 windows fonts default-root-window default-colormaps ; Don't GC these!
	 (clean-lost-celled-objects
	  colormaps
	  (lambda (colormap-number)
	    (%XFreeColorMap display-number colormap-number)))
	 #| ****************************************
	 By rights, we should release unused colors from all
	 of the allocated color maps *and* the default color
	 map of each screen.  However, this would require us
	 to keep track of the foreground and background
	 color in graphics contexts since X keeps these
	 pointers for us.
         When we fix this, it should use the protection list iterators
	 (if colormaps
	     (do ((associations (cdr colormaps) (cdr associations)))
		 ((null? associations))
	       (let* ((colormap (weak-car (car associations)))
		      (pixels (colormap/pixel-list colormap)))
		 (if (not (null? pixels))
		     (clean-lost-celled-objects
		      pixels
		      (lambda (pixel)
			(%XFreeColor display-number **doens't work
				     (->colormap colormap)
				     (-> pixel pixel))))))))
	 ******************************************** |#
	 (clean-lost-celled-objects
	  gcs
	  (lambda (gc-number) (%XFreeGC display-number gc-number)))))))



;;;; Primitives written in C

(define-primitives			; X Operations
  (%XAllocNamedColor 5)
  (%XChangeWindowAttributes 4)
  (%XCheckMaskEvent 3)
  (%XClearArea 7)
  (%XClearWindow 2)
  (%XCloseDisplay 1)
  (%XConnectionNumber 1)
  (%XCreateGC 4)
  (%XCreateRegion 0)
  (%XCreateSimpleWindow 9)
  (%XDecodeButtonEvent 2)
  (%XDecodeConfigureEvent 2)
  (%XDecodeCrossingEvent 2)
  (%XDecodeExposeEvent 2)
  (%XDecodeKeyEvent 2)
  (%XDecodeMotionEvent 2)
  (%XDecodeUnknownEvent 2)
  (%XDecodeWindowAttributes 2)
  (%XDecodeXColor 2)
  (%XDefaultColormap 2)
  (%XDefaultRootWindow 1)
  (%XDefaultScreen 1)
  (%XDestroyRegion 1)
  (%XDestroyWindow 2)
  (%XDrawArc 9)
  (%XDrawLine 7)
  (%XDrawRectangle 7)
  (%XFillArc 9)
  (%XFillRectangle 7)
  (%XFlush 1)
  (%XFreeColormap 2)
  (%XFreeGC 2)
  (%XGetDefault 3)
  (%XGetWindowAttributes 3)
  (%XInitSCXL! 0)
  (%XIntersectRegion 3)
  (%XLoadFont 2)
  (%XMapWindow 2)
  (%XNextEvent 2)
  (%XOpenDisplay 1)
  (%XPending 1)
  (%XPutBackEvent 2)
  (%XQueryPointer 3)
  (%XQueryTree 2)
  (%XScreenCount 1)
  (%XSetForeground 3)
  (%XSetFunction 3)
  (%XSetRegion 3)
  (%XStoreName 3)
  (%XSubtractRegion 3)
  (%XSync 2)
  (%XSynchronize 2)
  (%XTranslateCoordinates 6)
  (%XUnionRegion 3)
  (%XUnionRectSpecsWithRegion! 6)
  (%XUnloadFont 2))

(define-primitives			; X Data Structure constructors
  (%XMake-Color 0)
  (%XMake-Event 0)
  (%XMake-GCValues 0)
  (%XMake-GetWindowAttributes 0)
  (%XMake-SetWindowAttributes 0)
  (%XMake-Window 0)			; Hold a window identifier
  (%XMake-XY 0))			; Hold an X/Y value

(define-primitives			; X data structure mutators
  (%XSetWindowAttributes-Event_Mask! 2))

(define (XColor.Pixel xcolor)
  (let ((components (make-vector 5)))
    ;; Returns #(pixel, red, green, blue, flags)
    (XDecodeXColor xcolor components)
    (vector-ref components 0)))

;;;; SCXL-level calls to the X primitives

(define (XAllocNamedColor display colormap color-string)
  ;; Returns list: (Status Color-Allocated Exact-Color)
  ;; where color-allocated and exact-color are each the cons of a
  ;; wrapped pixel and a string that is the direct coding of the X
  ;; color structure.
  (let ((allocated (%XMake-Color))
	(exact (%XMake-Color)))
    (let ((result
	   (%XAllocNamedColor (->XDisplay display)
			      (->XColormap colormap)
			      color-string
			      allocated
			      exact)))
      (list result
	    (wrap-pixel display colormap (XColor.Pixel allocated))
	    (wrap-pixel display colormap (XColor.Pixel exact))))))

(define (XChangeWindowAttributes display window mask attributes)
  (%XChangeWindowAttributes (->XDisplay display)
			    (->XWindow window)
			    mask
			    attributes)
  (flush-display-hook))

(define (XCheckMaskEvent!? display event-mask returned-event)
  (%XCheckMaskEvent (->XDisplay display)
		    event-mask
		    returned-event))

(define (XClearArea display window x y width height exposures?)
  (%XClearArea (->XDisplay display)
	       (->XWindow window)
	       x y width height exposures?)
  (flush-display-hook))

(define (XClearWindow display window)
  (%XClearWindow (->XDisplay display)
		 (->XWindow window))
  (flush-display-hook))

#| XCloseDisplay closes the display and destroys the Scheme shadows of
the associated server objects, and closes tk objects (which aren't
automatically closed by X).  In UITK, XCloseDisplay is NEVER called.  The
lower-level XCloseDisplayByNumber is called from the GC.  Anyone who
writes programs that call XCloseDisplay needs to think carefully about the
dependencies (on both the Scheme and C side) of objects and processes
involved in calling destroy operations.  Tread carefully here. |#

(define XCloseDisplay
  (wrap-with-SCXL-DESTROY!
   1 0
   (lambda (dsp)
     (define (kill-protected-objects protection-list)
       (for-each SCXL-DESTROY!
		 (protection-list-referenced-elements protection-list)))
     ;;SCXL-destroy the Scheme representitives of the X objects associated
     ;;to the display (which are killed by X when the
     ;;display is closed).
     (unwrap-display
      dsp
      (lambda (xdisplay windows fonts colormaps gcs
			defaultwindow defaultcolormaps)
	defaultwindow defaultcolormaps	; Should be, but aren't, used
	(for-each kill-protected-objects (list windows fonts colormaps gcs))
	;;do someting about the default window and colormap
	(XCloseDisplayByNumber xdisplay))))))

#| *********************************************

XCloseDisplayByNumber is scheduled to be called by the gc-daemon when
all references to the Scheme display have been lost.

Closing the display will close the (C) windows, GC's, fonts, etc.  But
we do NOT need to destroy their Scheme-side reflections, because we
have arranged for these to point to the display.  Therefore, they
cannot be around if the Scheme display object has been lost.

*XclosedisplayCallBacks is a list of thinks that should be called when
a display is closed.  In UITK, for example, we need to 
shut down the even server for the display.

  ************************************************ |#

(define *XCloseDisplayCallBacks* '())

(define (XCloseDisplayByNumber display-number)
  (for-each (lambda (proc) (proc display-number))
	    *XCloseDisplayCallBacks*)
  (%XCloseDisplay display-number))

(define (SCXL-Install-XCloseDisplay-Callback proc)
  (set! *XCloseDisplayCallbacks*
	(cons proc *XCloseDisplayCallBacks*)))

(define (XConnectionNumber display)
  (%XConnectionNumber (->XDisplay display)))

(define (XCreateGC display window mask gcvalues)
  (wrap-graphics-context display
			 (%XCreateGC (->XDisplay display)
				     (->XWindow window)
				     mask
				     gcvalues)))

(define (XCreateRegion)
  (wrap-region (%XCreateRegion)))

(define (XCreateSimpleWindow
	 display parent x y width height
	 border-width border-pixel background-pixel)
  (wrap-window display 
	       (%XCreateSimpleWindow (->XDisplay display)
				     (->XWindow parent)
				     x y width height border-width
				     (->XPixel border-pixel)
				     (->XPixel background-pixel))))
  
(define (XDecodeButtonEvent event vect)
  (%XDecodeButtonEvent event vect))

(define (XDecodeConfigureEvent event vect)
  (%XDecodeConfigureEvent event vect))

(define (XDecodeCrossingEvent event vect)
  (%XDecodeCrossingEvent event vect))

(define (XDecodeExposeEvent event vect)
  (%XDecodeExposeEvent event vect))

(define (XDecodeKeyEvent event vect)
  (%XDecodeKeyEvent event vect))

(define (XDecodeMotionEvent event vect)
  (%XDecodeMotionEvent event vect))

(define (XDecodeUnknownEvent event vect)
  (%XDecodeUnknownEvent event vect))

(define (XDecodeWindowAttributes attributes vect)
  (%XDecodeWindowAttributes attributes vect))

(define (XDecodeXColor XColor vect)
  (%XDecodeXColor xcolor vect))

(define (XDefaultColormap display screen)
  (vector-ref (display/screen/default-color-map display) screen))

(define (XDefaultRootWindow display)
  (display/default-root-window display))

;;; Screens are not wrapped -- they are just integers, because there
;;; is no real resource that is being allocated here.

(define (XDefaultScreen display)
  (%XDefaultScreen (->XDisplay display)))

(define XDestroyRegion
  (wrap-with-SCXL-DESTROY! 1 0
    (lambda (region)
      (%XDestroyRegion (->XRegion region)))))

(define XDestroyWindow
  (wrap-with-SCXL-DESTROY! 2 1
    (lambda (display window)
      (%XDestroyWindow (->XDisplay display) (->XWindow window)))))

(define (XDrawArc display window graphics-context
		  x y width height angle1 angle2)
  (%XDrawArc (->XDisplay display)
	     (->XWindow window)
	     (->XGC graphics-context)
	     x y width height angle1 angle2)
  (flush-display-hook))

(define (XDrawLine display window graphics-context x1 y1 x2 y2)
  (%XDrawLine (->XDisplay display)
	      (->XWindow window)
	      (->XGC graphics-context)
	      x1 y1 x2 y2)
  (flush-display-hook))

(define (XDrawRectangle display window graphics-context x y width height)
  (%XDrawRectangle (->XDisplay display)
		   (->XWindow window)
		   (->XGC graphics-context)
		   x y width height)
  (flush-display-hook))

(define (XFillArc display window graphics-context
		  x y width height angle1 angle2)
  (%XFillArc (->XDisplay display)
	     (->XWindow window)
	     (->XGC graphics-context)
	     x y width height angle1 angle2)
  (flush-display-hook))

(define (XFillRectangle display window graphics-context x y width height)
  (%XFillRectangle (->XDisplay display)
		   (->XWindow window)
		   (->XGC graphics-context)
		   x y width height)
  (flush-display-hook))

(define (XFlush display)
  (%XFlush (->XDisplay display)))

;;; When we free a colormap, we have to destroy the Scheme
;;; representitives of the pixels allocated in it.

(define XFreeColormap
  (wrap-with-SCXL-DESTROY! 2 1
    (lambda (display colormap)
      (%XFreeColormap (->XDisplay display) (->XColormap colormap))
      (if (SCXL-WRAPPED? font)
	  (for-each SCXL-DESTROY!
		    (protection-list-referenced-elements
		     (colormap/pixel-list colormap)))))))

(define XFreeGC
  (wrap-with-SCXL-DESTROY! 2 1
   (lambda (display gc)
     (%XFreeGC (->XDisplay display) (->XGC gc)))))

;;; XGetDefault returns a bare X pointer.  Anyone who calls it needs to know
;;; what is expected and wrap it at the next level up.

(define (XGetDefault display program option)
  (%XGetDefault (->XDisplay display) program option))

(define (XGetWindowAttributes display window)
  ;; Returns a list of (Status Attributes)
  (let ((attributes (%XMake-GetWindowAttributes)))
    (list (%XGetWindowAttributes (->XDisplay display)
				 (->XWindow window)
				 attributes)
	  attributes)))

(define (XIntersectRegion! source-1 source-2 dest)
  (%XIntersectRegion
   (->XRegion source-1)
   (->XRegion source-2)
   (->XRegion dest)))

(define (XLoadFont display name)
  (wrap-font display
	     (%XLoadFont (->Xdisplay display) name)))

(define (XMapWindow display window)
  (%XMapWindow (->XDisplay display) (->XWindow window))
  (flush-display-hook))

(define (XNextEvent display)
  (let ((event (%XMake-Event)))
    (%XNextEvent (->XDisplay display) event)
    event))

;;;This is a version that doesn't build up garbage
(define (XNextEvent! display event-string)
  (%XNextEvent (->XDisplay display) event-string)
  event-string)

;;;This is a flag that forces the X server into synchronous mode.  It is
;;;useful for debugging, but slows things down alot.

(define *Synchronizing?* #f)

(define (XOpenDisplay string)
  (wrap-display
   (let ((result (%XOpenDisplay string)))
     (%XSynchronize result *Synchronizing?*)
     result)))

(define (XPending display)
  (%XPending (->XDisplay display)))

(define (XPutBackEvent display event)
  (%XPutBackEvent (->XDisplay display) event))

(define (XQueryTree display window)
  (%XQueryTree (->XDisplay display)
	       (->XWindow window)))

(define (XQueryPointer display window)
  (let ((result (make-vector 8)))
    (%XQueryPointer (->XDisplay display)
		    (->XWindow window)
		    result)
    result))

(define (XScreenCount display)
  (%XScreenCount (->XDisplay display)))

(define (XSetForeground display graphics-context pixel-value)
  (%XSetForeground (->XDisplay display)
		   (->XGC graphics-context)
		   (->XPixel pixel-value))
  (flush-display-hook))


(define (XSetFunction display graphics-context function-number)
  (%XSetFunction (->XDisplay display)
		 (->XGC graphics-context)
		 function-number))

(define (XSetRegion display graphics-context region)
  (%XSetRegion (->XDisplay display)
	       (->XGC graphics-context)
	       (->XRegion region))
  (if (SCXL-WRAPPED? graphics-context)
      (set-gc/region! graphics-context region)))

(define (XStoreName display window title)
  (%XStoreName (->XDisplay display)
	       (->XWindow window)
	       title))

(define (XSubtractRegion! source-1 source-2 dest)
  (%XSubtractRegion
   (->XRegion source-1)
   (->XRegion source-2)
   (->XRegion dest)))

(define (XTranslateCoordinates display from-window to-window x y)
  ;; Returns a vector (Status X Y Child-Window)
  (let ((return-vector (make-vector 4)))
    (%XTranslateCoordinates (->XDisplay display)
			    (->XWindow from-window)
			    (->XWindow to-window)
			    x y return-vector)
    return-vector))

(define (XUnionRegion! source-1 source-2 dest)
  (%XUnionRegion
   (->XRegion source-1)
   (->XRegion source-2)
   (->XRegion dest)))

(define (XUnionRectSpecsWithRegion! x y width height in-region out-region)
  (%XUnionRectSpecsWithRegion! x y width height
			       (->XRegion in-region)
			       (->XRegion out-region)))

(define XUnloadFont
  (wrap-with-SCXL-DESTROY! 2 1
    (lambda (display font)
      (%XUnloadFont (->XDisplay display) (->XFont font)))))

;;;; Constructors

(define (XMake-Color) (%XMake-Color))
(define (XMake-Event) (%XMake-Event))
(define (XMake-GCValues) (%XMake-GCValues))
(define (XMake-SetWindowAttributes) (%XMake-SetWindowAttributes))

;;;; Mutators

(define (XSetWindowAttributes-Event_Mask! object value)
  (%XSetWindowAttributes-Event_Mask! object value))

;;;; Auxilliary

(define (XCopy-Event event)
  (string-copy event))

(define (initialize-scxl!)
  (%XInitSCXL!)
  (set! display-protection-list (make-protection-list))
  (set! region-protection-list (make-protection-list))
  ;; Warning: UITK (in mit-xhooks.scm) knows that
  ;; close-lost-displays-daemon is *the* daemon associated with scxl.
  ;; It arranges to run it in another thread and removes it from the
  ;; gc-daemon list.
  ;;
  ;; THIS SHOULD BE PUT BACK!
  ;; (add-gc-daemon! close-lost-displays-daemon)
  )

(initialize-scxl!)

