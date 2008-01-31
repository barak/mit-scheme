#| -*- Scheme -*-

$Id: wabbit.scm,v 1.7 2008/01/30 20:02:40 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Wabbit Hunting and Headhunting GC
;;; package: (gc-wabbit)

(declare (usual-integrations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;									    ;;;
;;; WABBIT -- Wabbit hunting and headhunting frobbery.			    ;;;
;;;									    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-package!)
  (set! index:gc-wabbit-descwiptor
	(fixed-objects-vector-slot 'GC-WABBIT-DESCWIPTOR))
  (install))


(define (wabbit-hunt wabbit-descwiptor #!optional fudd-thunk)
  "(WABBIT-DESCWIPTOR #!optional FUDD-THUNK)

   Procedure behavior:
   ------------------

   Open wabbit season on wabbits matching WABBIT-DESCWIPTOR and go wabbit
   hunting. Once all the wabbits have been wounded up, invoke FUDD-THUNK,
   weturning the wesult of FUDD-THUNK as the wesult of the wabbit hunt.
   
   The optional FUDD-THUNK pawameter defaults to the value of the fluid
   vawiable: *DEFAULT-FUDD-THUNK*, which defaults to just weturning the
   wabbit buffer (which will have been swabbed upon return!).

   Explanation of parameters:
   -------------------------

   A ``wabbit descwiptor'' is a 4-element vector:
       ------------------------------------------------------------------------
       0. Boolean hunt disable flag -- (a.k.a. ``duck season'' flag)
				       avoid wabbit hunting and/or headhunting
				       upon the next GC flip.

       1. Wabbit vector -- vector of object references to target objects
			   (a.k.a. ``wabbits'')

       2. Wabbit buffer -- vector into which wabbit sightings are recorded.
			   This must be of length (2 + twice wabbit vect len).

       3. Boolean headhunt enable flag -- if FALSE, no headhunt is performed.
					  else this slot will be replaced by a
					   headhunt collection upon completion
					   of the headhunting wabbit hunt.
       ------------------------------------------------------------------------
   ****
    NB	a) Both the WABBIT-VECTOR and the WABBIT-BUFFER must reside in the heap
   ****	     i.e., they may *not* reside in constant space or on the stack.
	b) Both the wabbit buffer and the headhunt collection slots are zeroed
	    upon return, since they may contain unsafe pointers. Moreover, it
	    is unsafe for the FUDD-THUNK to return them or otherwise close over
	    them. Consider them only to be very fluid parameter sources for the
	    FUDD-THUNK.

   The ``wabbit buffer'' should be a vector of FALSEs before the wabbit hunting
   is initiated. At the end of the wabbit hunt, the wabbit buffer contents will
   be laid out as follows:
     --------------------------------------------------------------------------
     slot 0 = Boolean flag: TRUE  if all wabbit sightings were recorded in the
				     wabbit buffer
			    FALSE if the wabbit buffer was too small to accomo-
				     date a record for each wabbit sighting.
				     (In this case, the FUDD-THUNK should do a
				      bit of cleanup work so the same wabbit
				      hunt can be re-initiated later.)
     slot 1 = Fixnum: number of wabbit sightings recorded in the wabbit buffer
     slot 2 = Object reference: cite of first wabbit sighting (``wabbit hole'')
     slot 3 = Number: offset into first sighting object where wabbit is hiding
     --------------------------------------------------------------------------
   ...and so on, with even-index slots containing wabbit holes and odd-index
   slots, indices. Note that slot 1 should hold the index of the first even
   slot that holds FALSE and all slots thereafter should likewise hold FALSE.

   A ``wabbit hole'' is normally a headed object reference (a pointer) but it
   may in very rare circumstances be a ``wascally wabbit hole''. There are only
   three kinds of wascally wabbit holes:
    ---------------------------------------------------------------------------
    1. Characters: these indicate references to wabbit holes in constant space.
		   To reify the character into a cell whose contents holds the
		   wabbit, apply CELLIFY to the slot ref that holds the char.
		   (NB: the char as printed holds only part of the addr; you
			must vector-ref into the wabbit buffer to get all the
			addr bits. This is incredible magic.)
    2. Null  Refs: these indicate headless objects. They should never appear.
    3. Stack Refs: these indicate objects on the control stack. Since we reify
		   the stack into the heap as part of the call to WABBIT-HUNT,
		   these too should never appear unless you are doing something
		   painfully obscure (and dangerous!).

    If you ever encounter Null or Stack wabbit holes, you may want to send a
    friendly bug report (?) to bug-cscheme@zurich.ai.mit.edu with a repeatable
    test script.
    ---------------------------------------------------------------------------

   The ``headhunt collection'' is a vector of arbitrary (fixnum) length. It is
   intended to contain a pointer to the head of every object in the heap which
   has an object header (spec., numbers, Booleans, etc not included). If all
   headed heap objects fit in the space available after the GC flip, then slot
   0 of this headhunt collection is TRUE. If not, slot 0 is FALSE and the vec-
   tor contains as many object head references as actually did fit.

 ************ Be verwy verwy careful when headhunting... if you are not careful
 ** CAVEAT ** to release the headhunt collection (e.g., SET! it to FALSE) or if
 ************ you gobble up too much intermediate state in traversing it, you
	      will exhaust the available heap space and go down in flames. This
	      is a very fragile system memory feature intended for only the
	      most ginger-fingered discriminating systems wizards. For instance
	      it may some day lead to a post-GC garbage scavenger. Nonetheless,
	      it readily lends itself to self abuse if not treated reverently.
  "

  (cond ((or (default-object?	   fudd-thunk)
	     (not		   fudd-thunk))
	 (set!			   fudd-thunk
			  *default-fudd-thunk*)))
  (let (;;
	;; Uhm... force stack refs into heap during wabbit season; undo at exit
	;;	  and should be careful not to hunt wabbits out of season
	;;
	(call-within-wabbit-season-with-duck-season-return-continuation
	 call-with-current-continuation)
	;;
	;; gc-flip is the raw low-level wabbit hunt mechanism... the hunt flag
	;;	   enabled in the wabbit-descwiptor forces an alternative
	;;	   ucode gc-loop which goes a-huntin' varmits.
	(%waw-wabbit-hunt gc-flip)
	)
    (wabbit-season! wabbit-descwiptor)
    (call-within-wabbit-season-with-duck-season-return-continuation
     (lambda (return-to-duck-season)
       (%waw-wabbit-hunt)
       (let ((killed-da-wittle-bunny-wabbits
	      (dynamic-wind
	       (lambda () 'unwind-protect)
	       fudd-thunk
	       ;;
	       ;; Make sure unsafe buffers are cleared before returning...
	       ;;
	       (lambda () (%swab-wad wabbit-descwiptor)))))
	 (return-to-duck-season killed-da-wittle-bunny-wabbits))))))


(define *default-fudd-thunk*)		; See install below
(define (default-fudd-thunk)
  (wabbit-descwiptor/wabbit-buffer (get-wabbit-descwiptor)))


(define-integrable (%swab-wad wad)	; swab the wabbit descwiptor but good
  ;;
  ;; Nullify wabbit buffer, leaving found-all-flag and first-null-index intact
  ;;
  (let ((wabbit-buffer (wabbit-descwiptor/wabbit-buffer	      wad)))
    (cond ((vector? wabbit-buffer)
	   (let ((buflen (vector-length wabbit-buffer)))
	     (subvector-fill! wabbit-buffer
			      (min 2 buflen) ; fuddge
			      buflen
			      false)))))
  ;;
  ;; Drop headhunt collection by replacing it w/ the length of the collection,
  ;;   negated if not a complete headhunt collection.
  ;;
  (let ((headhunt-coll (wabbit-descwiptor/headhunt-collection wad)))
    (cond ((vector? headhunt-coll)
	   (let ((head-len  (vector-length headhunt-coll))
		 (complete? (vector-ref	   headhunt-coll 0)))
	     (set-wabbit-descwiptor/headhunt-collection! wad
							 (if complete?
							     head-len
							     (- head-len)))))))
  unspecific)

;; Wabbit Season and Duck Season

(define (wabbit-season! wabbit-descwiptor)
  "(WABBIT-DESCWIPTOR)
   Declare open season on wabbits matching our target descwiptor.
   Returns the old wabbit descwiptor (possibly FALSE).
  "
  (%stuff-gc-wabbit-descwiptor! wabbit-descwiptor))

(define (duck-season!)
  "()
   Disable wabbit hunting... returns descwiptor from latest wabbit hunt.
  "
  (let ((current-wd (get-wabbit-descwiptor)))
    (cond ((wabbit-descwiptor? current-wd)
	   (set-wabbit-descwiptor/hunt-disable-flag! current-wd true)
	   current-wd)
	  (else
	   (%stuff-gc-wabbit-descwiptor! false)))))

;; Misc

(define (duck-season?)
  (let ((current-wd (get-wabbit-descwiptor)))
    (or (false? current-wd)
	(not (wabbit-descwiptor? current-wd)) ; should not arise, but guard
	(wabbit-descwiptor/hunt-disable-flag current-wd))))

(define (wabbit-season?)
  (not	  (duck-season?)))


;; Low-level bits

(define index:gc-wabbit-descwiptor)	; See initialize-package! above

(define-integrable (get-wabbit-descwiptor)
  (vector-ref (get-fixed-objects-vector) index:gc-wabbit-descwiptor))

(define-integrable (%stuff-gc-wabbit-descwiptor! value)
  (let* ((fov (get-fixed-objects-vector))
	 (old (vector-ref fov index:gc-wabbit-descwiptor)))
    (vector-set! fov index:gc-wabbit-descwiptor value)
    old))


;; Very precarious indeed!

(define (cellify object)		
  ((ucode-primitive primitive-object-set-type 2) (ucode-type cell)
						 object))

;;;
;;; Wabbit descwiptor data abstraction-- NB: 4-elt vector rep (ucode depend'cy)
;;;

(define-integrable (wabbit-descwiptor? object)
  (and (vector? object) (fix:= (vector-length object) 4)))

(define-structure
  (		    wabbit-descwiptor
   (conc-name	    wabbit-descwiptor/)
   ;;(name	   'wabbit-descriptor) ;; unnamed [i.e., not tagged]
   (type vector))
  (hunt-disable-flag	true		 READ-ONLY false TYPE boolean)
  (wabbit-vector	(vector)	 READ-ONLY false TYPE vector)
  (wabbit-buffer	(vector false 2) READ-ONLY false TYPE vector)
  (headhunt-enable-flag false		 READ-ONLY false TYPE boolean)
  )

;; Structure accessor aliases...

;; after the hunt, the flag is replaced by a headhunt collection

(define-integrable
  (wabbit-descwiptor/headhunt-collection  wabbit-descwiptor)
  (wabbit-descwiptor/headhunt-enable-flag wabbit-descwiptor))

(define-integrable
  (set-wabbit-descwiptor/headhunt-collection!  wabbit-descwiptor new-value)
  (set-wabbit-descwiptor/headhunt-enable-flag! wabbit-descwiptor new-value))

;;;
;;; Headhunting frobbery... special case of wabbit hunting: no wascally wabbits
;;;

(define (headhunt  #!optional headhunt-fudd-thunk headhunt-wabbit-descwiptor)
  (cond ((or (default-object? headhunt-fudd-thunk)
	     (not	      headhunt-fudd-thunk))
	 (set!		      headhunt-fudd-thunk
		     *default-headhunt-fudd-thunk*))
	)
  (cond ((or (default-object?			  headhunt-wabbit-descwiptor)
	     (not				  headhunt-wabbit-descwiptor))
	 (set!					  headhunt-wabbit-descwiptor
					 *default-headhunt-wabbit-descwiptor*))
	)
  (wabbit-hunt					  headhunt-wabbit-descwiptor
			      headhunt-fudd-thunk))


(define *default-headhunt-fudd-thunk*)	      ; See install below
(define (default-headhunt-fudd-thunk)
  ;;   ,
  ;; Tres unsafe raven... lets headhunt collection escape the headhunt!
  ;;
  (wabbit-descwiptor/headhunt-collection (get-wabbit-descwiptor)))

(define *default-headhunt-wabbit-descwiptor*) ; See install below
(define (default-headhunt-wabbit-descwiptor)
  (make-wabbit-descwiptor false		 ; hunt	   disable flag disabled
			  (vector)	 ; wabbit descwiptor null
			  (vector '? 'N) ; wabbit buffer     null-ish
			  true		 ; headhunt enable flag	 enabled
			  ))



;;; fini

(define (install)
  (set!		 *default-fudd-thunk*
		  default-fudd-thunk)
  (set!		 *default-headhunt-fudd-thunk*
		  default-headhunt-fudd-thunk)
  (set!		 *default-headhunt-wabbit-descwiptor*
		 (default-headhunt-wabbit-descwiptor))
  )

;;;
;;; Sample usage (and mis-usage)
;;;

;; handy util for debuggery
;;
;;(define memory-ref (make-primitive-procedure 'primitive-object-ref))


#| Sample wreckless wabbit hunt... (does not swab the wabbit buffer)
  --------------------------------
(define foobarbaz (cons 'a 'b))

(begin
  (wabbit-season!
   (make-wabbit-descwiptor false	       ; hunt	 disable flag disabled
			   (vector foobarbaz)  ; wabbit vector
			   (make-vector 10 #f) ; wabbit buffer
			   false	       ; headhunt enable flag disabled
			   ))
  'be-careful!)

(gc-flip)

(define done (duck-season!))

(pp done)  ; lookin' for trouble

;returns: #(#t #((a . b)) #(#t 4 (foobarbaz a . b) 1 () () () () () ()) ())
|#


#| Sample non-wreckless wabbit hunt... (safe wabbit hole count)
  ------------------------------------
(wabbit-hunt
 (make-wabbit-descwiptor false		     ; hunt    disable flag disabled
			 (vector foobarbaz)  ; wabbit vector
			 (make-vector 10 #f) ; wabbit buffer
			 false		     ; headhunt enable flag disabled
			 ))

; evaluated repeatedly... (stable wabbit hole count... holes swabbed upon exit)
;
;Value 31: #(#t 6 () () () () () () () ())  ; - 6 = wabbit hole count + 2
;Value 32: #(#t 6 () () () () () () () ())
;Value 33: #(#t 6 () () () () () () () ())
|#

#| Sample dangerous wabbit hunt... (fudd thunk exposes the wabbit holes...hash)
  -----------------------------
(wabbit-hunt
 (make-wabbit-descwiptor false		     ; hunt    disable flag disabled
			 (vector foobarbaz)  ; wabbit vector
			 (make-vector 10 #f) ; wabbit buffer
			 false		     ; headhunt enable flag disabled
			 )
 (named-lambda (exposing-fudd-thunk)
   (let* ((wabbuf (wabbit-descwiptor/wabbit-buffer (get-wabbit-descwiptor)))
	  (got-em-all?	   (vector-ref wabbuf 0))
	  (last-hole-index (vector-ref wabbuf 1)))
     (display "\n; #(")
     (do ((index 2 (1+ index)))
	 ((>= index last-hole-index)
	  (if got-em-all?
	      (display ")\n; Th-th-th-that's all folks!")
	      (display ")\n; And many more.... maybe?!?"))
	  (newline))
       (write (vector-ref wabbuf index)) ; DANGER! WRITE hashes output.
       (write-char #\Space)))))

; evaluated repeatedly... (stable display)

; #((foobarbaz a . b) 1 #((a . b)) 1 )
; Th-th-th-that's all folks!
;No value

; #((foobarbaz a . b) 1 #((a . b)) 1 )
; Th-th-th-that's all folks!
;No value

; #((foobarbaz a . b) 1 #((a . b)) 1 )
; Th-th-th-that's all folks!
;No value

; #((foobarbaz a . b) 1 #((a . b)) 1 )
; Th-th-th-that's all folks!
;No value
|#

#| Sample semi-wreckless headhunt... (default headhunt-fudd-thunk exposes coll)
  -------------------------------

(begin (headhunt)
       (wabbit-descwiptor/headhunt-enable-flag (get-wabbit-descwiptor)))

; evaluated repeatedly... (stable head count... if negative, partial count)
;
;Value: 23648
;Value: 23648
;Value: 23648
|#
