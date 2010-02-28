#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; PC Sampling Code Blocks (i.e., compiled procedure profiling)
;;; package: (pc-sample code-blocks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                    ;;;
;;;;; THIS CODE IS HEAVILY SNARFED FROM PCSIPROC.SCM ;;;;;
;;;                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Of course, this means I really should  ;;;;;;;;;
;;;;;;;;; abstract all this common structure out ;;;;;;;;;
;;;;;;;;; but first, let's just make it work, OK ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
 |=============================================================================
 | TODO:
 |	- DBG info should be groveled only at display time, not at hash time.
 |
 |=============================================================================
 |#

(declare (usual-integrations))

(define (initialize-package!)
  (set!  *purified-proc-cobl-profile-table* ( proc-cobl-profile-table/make))
  (set!   *heathen-proc-cobl-profile-table* ( proc-cobl-profile-table/make))
  (set!   *purified-dbg-cobl-profile-table* (  dbg-cobl-profile-table/make))
  (set!    *heathen-dbg-cobl-profile-table* (  dbg-cobl-profile-table/make))
  (set!   *purified-raw-cobl-profile-table* (  raw-cobl-profile-table/make))
  (set!    *heathen-raw-cobl-profile-table* (  raw-cobl-profile-table/make))
  (set! *purified-trampoline-profile-table* (trampoline-profile-table/make))
  (set!  *heathen-trampoline-profile-table* (trampoline-profile-table/make))
  ;; microlevel buffer install
  (install-code-block-profile-buffers/length)
  ;; Bozo test
  (if (not (compiled-code-address? reconstruct-compiled-procedure))
      (warn
       "pcscobl is unhappy: reconstruct-compiled-procedure is interpreted")))

(define-primitives
  (purified-code-block-profile-buffer/empty? 0)
  ( heathen-code-block-profile-buffer/empty? 0)
  (purified-code-block-profile-buffer/next-empty-slot-index 0)
  ( heathen-code-block-profile-buffer/next-empty-slot-index 0)
  (purified-code-block-profile-buffer/slack 0)
  ( heathen-code-block-profile-buffer/slack 0)
  (purified-code-block-profile-buffer/slack-increment 0)
  ( heathen-code-block-profile-buffer/slack-increment 0)
  (purified-code-block-profile-buffer/set-slack           1)
  ( heathen-code-block-profile-buffer/set-slack           1)
  (purified-code-block-profile-buffer/set-slack-increment 1)
  ( heathen-code-block-profile-buffer/set-slack-increment 1)
  (purified-code-block-profile-buffer/extend-noisy?   0)
  ( heathen-code-block-profile-buffer/extend-noisy?   0)
  (purified-code-block-profile-buffer/flush-noisy?    0)
  ( heathen-code-block-profile-buffer/flush-noisy?    0)
  (purified-code-block-profile-buffer/overflow-noisy? 0)
  ( heathen-code-block-profile-buffer/overflow-noisy? 0)
  (purified-code-block-profile-buffer/extend-noisy?/toggle!   0)
  ( heathen-code-block-profile-buffer/extend-noisy?/toggle!   0)
  (purified-code-block-profile-buffer/flush-noisy?/toggle!    0)
  ( heathen-code-block-profile-buffer/flush-noisy?/toggle!    0)
  (purified-code-block-profile-buffer/overflow-noisy?/toggle! 0)
  ( heathen-code-block-profile-buffer/overflow-noisy?/toggle! 0)
  ;; microcode magic: don't look. Fnord!
  (%pc-sample/PCBPB-overflow-count       0)
  (%pc-sample/HCBPB-overflow-count       0)
  (%pc-sample/PCBPB-overflow-count/reset 0)
  (%pc-sample/HCBPB-overflow-count/reset 0)
  (%pc-sample/PCBPB-monitoring?         0)
  (%pc-sample/HCBPB-monitoring?         0)
  (%pc-sample/PCBPB-monitoring?/toggle! 0)
  (%pc-sample/HCBPB-monitoring?/toggle! 0)
  )

(define (profile-buffer/with-mumble-notification!     noise? thunk
						  x/f-noisy? toggle-noise!)
  (let ((already-noisy? (x/f-noisy?))
	(want-no-noise? (not noise?)))		; coerce to Boolean
    (if (eq? already-noisy? want-no-noise?) 	; xor want and got
	(dynamic-wind toggle-noise! thunk toggle-noise!)
	(thunk))))

(define (purified-code-block-profile-buffer/with-extend-notification!   noise?
								        thunk)
  (profile-buffer/with-mumble-notification! noise? thunk
	 purified-code-block-profile-buffer/extend-noisy?
	 purified-code-block-profile-buffer/extend-noisy?/toggle!))

(define ( heathen-code-block-profile-buffer/with-extend-notification!   noise?
								        thunk)
  (profile-buffer/with-mumble-notification! noise? thunk
	  heathen-code-block-profile-buffer/extend-noisy?
	  heathen-code-block-profile-buffer/extend-noisy?/toggle!))

(define (purified-code-block-profile-buffer/with-flush-notification!    noise?
								        thunk)
  (profile-buffer/with-mumble-notification! noise? thunk
	 purified-code-block-profile-buffer/flush-noisy?
	 purified-code-block-profile-buffer/flush-noisy?/toggle!))

(define ( heathen-code-block-profile-buffer/with-flush-notification!    noise?
								        thunk)
  (profile-buffer/with-mumble-notification! noise? thunk
	  heathen-code-block-profile-buffer/flush-noisy?
	  heathen-code-block-profile-buffer/flush-noisy?/toggle!))

(define (purified-code-block-profile-buffer/with-overflow-notification! noise?
									thunk)
  (profile-buffer/with-mumble-notification! noise? thunk
	 purified-code-block-profile-buffer/overflow-noisy?
	 purified-code-block-profile-buffer/overflow-noisy?/toggle!))

(define ( heathen-code-block-profile-buffer/with-overflow-notification! noise?
									thunk)
  (profile-buffer/with-mumble-notification! noise? thunk
	  heathen-code-block-profile-buffer/overflow-noisy?
	  heathen-code-block-profile-buffer/overflow-noisy?/toggle!))

;;; Code Block Profile Buffers buffer up sightings of compiled procs
;;;   that are not yet hashed into the Code Block Profile (Hash) Tables
;;;
;;; Purified code blocks are distinguished from non-purified (``heathen'') ones
;;;   because, well, it seemd like the thing to do at the time and I couldn't
;;;   think of a very good reason not to.

(define *purified-code-block-profile-block-buffer* #F)  ; software cache o' FOV
(define  *heathen-code-block-profile-block-buffer* #F)  ; software cache o' FOV

(define *purified-code-block-profile-offset-buffer* #F) ; software cache o' FOV
(define  *heathen-code-block-profile-offset-buffer* #F) ; software cache o' FOV

(define (code-block-profiling-disabled?)
  (not (or *purified-code-block-profile-block-buffer* ; should all be synch'd
	    *heathen-code-block-profile-block-buffer*
	   *purified-code-block-profile-offset-buffer*
	    *heathen-code-block-profile-offset-buffer*)))

(define *purified-code-block-profile-buffer/length/initial*)
(define  *heathen-code-block-profile-buffer/length/initial*)

(define  (install-code-block-profile-buffers/length/initial)
  (set! *purified-code-block-profile-buffer/length/initial*
  (* 4  (purified-code-block-profile-buffer/slack)))
  (set!  *heathen-code-block-profile-buffer/length/initial*
  (* 4  ( heathen-code-block-profile-buffer/slack)))
  )

(define *purified-code-block-profile-buffer/length*)
(define  *heathen-code-block-profile-buffer/length*)

(define  (install-code-block-profile-buffers/length)
  (       install-code-block-profile-buffers/length/initial)
  (set! *purified-code-block-profile-buffer/length*
	*purified-code-block-profile-buffer/length/initial*)
  (set!  *heathen-code-block-profile-buffer/length*
	 *heathen-code-block-profile-buffer/length/initial*)
  )

(define (purified-code-block-profile-buffer/length)
        *purified-code-block-profile-buffer/length*)
(define ( heathen-code-block-profile-buffer/length)
         *heathen-code-block-profile-buffer/length*)

(define (purified-code-block-profile-buffer/length/set! new-value)
  (set! *purified-code-block-profile-buffer/length*     new-value))
(define ( heathen-code-block-profile-buffer/length/set! new-value)
  (set!  *heathen-code-block-profile-buffer/length*     new-value))

(define (code-block-profile-buffer/status)
  "()\n\
   Returns a list of two elements:\n\
     0) the purified code block profile buffer status, and\n\
     1) the  heathen code block profile buffer status\n\
   each of which is a dotted pair of buffer length cross buffer slack.\
  "
  (list (purified-code-block-profile-buffer/status)
	( heathen-code-block-profile-buffer/status)))

(define (purified-code-block-profile-buffer/status)
  "()\n\
  Returns a CONS pair of the length and `slack' of the profile buffer for\n\
  purified code blocks.\
  "
  (cons (purified-code-block-profile-buffer/length)
	(purified-code-block-profile-buffer/slack)))
(define ( heathen-code-block-profile-buffer/status)
  "()\n\
  Returns a CONS pair of the length and `slack' of the profile buffer for\n\
  heathen code blocks.\
  "
  (cons ( heathen-code-block-profile-buffer/length)
	( heathen-code-block-profile-buffer/slack)))


(define (code-block-profile-buffer/status/previous)
  "()\n\
   Returns the status of the profile buffer before the last modification to\n\
   its length and/or slack.\n\
   \n\
   This status is a list of two elements:\n\
     0) the purified code block profile buffer status, and\n\
     1) the  heathen code block profile buffer status\n\
   each of which is a dotted pair of buffer length cross buffer slack.\
  "
  (list (purified-code-block-profile-buffer/status/previous)
	( heathen-code-block-profile-buffer/status/previous)))

(define *purified-code-block-profile-buffer/status/old* '(0 . 0))
(define (purified-code-block-profile-buffer/status/previous)
  "()\n\
   Returns the status of the profile buffer before the last modification to\n\
   its length and/or slack.\
  "
        *purified-code-block-profile-buffer/status/old*)
(define  *heathen-code-block-profile-buffer/status/old* '(0 . 0))
(define ( heathen-code-block-profile-buffer/status/previous)
  "()\n\
   Returns the status of the profile buffer before the last modification to\n\
   its length and/or slack.\
  "
         *heathen-code-block-profile-buffer/status/old*)

;;; Purified Code Blocks

;;; TODO: flush/reset/spill/extend should all employ double buffering of the
;;;       code block profile buffers.

(define            *purified-code-block-profile-buffer/extend-count?* #F)
(define-integrable (purified-code-block-profile-buffer/extend-count?)
                   *purified-code-block-profile-buffer/extend-count?*)
(define-integrable (purified-code-block-profile-buffer/extend-count?/toggle!)
  (set!            *purified-code-block-profile-buffer/extend-count?*
	      (not *purified-code-block-profile-buffer/extend-count?*)))
(define            (purified-code-block-profile-buffer/with-extend-count! count?
									  thunk)
  (fluid-let     ((*purified-code-block-profile-buffer/extend-count?*     count?))
    (thunk)))
(define		   *purified-code-block-profile-buffer/extend-count* 0)
(define-integrable (purified-code-block-profile-buffer/extend-count)
		   *purified-code-block-profile-buffer/extend-count*)
(define-integrable (purified-code-block-profile-buffer/extend-count/reset)
  (set!		   *purified-code-block-profile-buffer/extend-count* 0))
(define-integrable (purified-code-block-profile-buffer/extend-count/1+)
  (set!		   *purified-code-block-profile-buffer/extend-count*
	       (1+ *purified-code-block-profile-buffer/extend-count*)))

(define (purified-code-block-profile-buffer/extend)
  (let ((stop/restart-sampler? (and (not *pc-sample/sample-sampler?*)
				    (pc-sample/started?))))
    ;; stop if need be
    (cond (stop/restart-sampler? (fluid-let ((*pc-sample/noisy?* #F))
				   (pc-sample/stop))))
    ;; count if willed to
    (cond ((purified-code-block-profile-buffer/extend-count?)
	   (purified-code-block-profile-buffer/extend-count/1+)))
    ;; No need to disable during extend since we build an extended copy of the
    ;;  buffers then install them in one swell foop...
    ;; Of course, any profile samples made during the extend will be discarded.
    ;; For this reason, we go ahead and disable buffering anyway since
    ;;  it would be a waste of time.
    (fixed-purified-code-block-profile-buffers/disable)
    (cond ((purified-code-block-profile-buffer/extend-noisy?)
	   (with-output-to-port console-output-port ; in case we're in Edwin
	     (lambda ()
	       (display "\n;> > > > > PCBPB Extend Request being serviced.")))
	   (output-port/flush-output console-output-port)))
    (let* ((slack             (purified-code-block-profile-buffer/slack ))
	   (old-buffer-length (purified-code-block-profile-buffer/length))
	   (new-buffer-length (+ old-buffer-length slack)                )
	   (new-block-buffer 
	    (vector-grow *purified-code-block-profile-block-buffer*
			 new-buffer-length))
	   (new-offset-buffer
	    (vector-grow *purified-code-block-profile-offset-buffer*
			 new-buffer-length)))
      ;; INVARIANT: unused slots o purified-code-block-profile-buffer must = #F
      (do ((index   old-buffer-length  (1+ index)))
	  ((= index new-buffer-length))
	(vector-set! new-block-buffer  index #F)
	(vector-set! new-offset-buffer index #F)
	)
      ;; Install new-buffers
      (set! *purified-code-block-profile-block-buffer*  new-block-buffer)
      (set! *purified-code-block-profile-offset-buffer* new-offset-buffer)
      ;; synch length cache
      (purified-code-block-profile-buffer/length/set! new-buffer-length))
    ;; Re-enable... synch kludge
    (fixed-purified-code-block-profile-buffers/install
          *purified-code-block-profile-block-buffer*
	  *purified-code-block-profile-offset-buffer*)
    ;; restart if need be
    (cond (stop/restart-sampler? (fluid-let ((*pc-sample/noisy?* #F))
				   (pc-sample/start)))))
  unspecific)

(define            *purified-code-block-profile-buffer/flush-count?* #F)
(define-integrable (purified-code-block-profile-buffer/flush-count?)
                   *purified-code-block-profile-buffer/flush-count?*)
(define-integrable (purified-code-block-profile-buffer/flush-count?/toggle!)
  (set!            *purified-code-block-profile-buffer/flush-count?*
	      (not *purified-code-block-profile-buffer/flush-count?*)))
(define            (purified-code-block-profile-buffer/with-flush-count! count?
									 thunk)
  (fluid-let     ((*purified-code-block-profile-buffer/flush-count?*     count?))
    (thunk)))
(define		   *purified-code-block-profile-buffer/flush-count* 0)
(define-integrable (purified-code-block-profile-buffer/flush-count)
		   *purified-code-block-profile-buffer/flush-count*)
(define-integrable (purified-code-block-profile-buffer/flush-count/reset)
  (set!		   *purified-code-block-profile-buffer/flush-count* 0))
(define-integrable (purified-code-block-profile-buffer/flush-count/1+)
  (set!		   *purified-code-block-profile-buffer/flush-count*
	       (1+ *purified-code-block-profile-buffer/flush-count*)))

(define-integrable (purified-code-block-profile-buffer/flush)
  (cond
   ((and *purified-code-block-profile-block-buffer*  ; not disabled
	 *purified-code-block-profile-offset-buffer* ; (should be synch'd)
	 (purified-code-block-profile-buffer/flush?))
    (purified-code-block-profile-buffer/spill-into-code-block-profile-tables)))
  unspecific)

(define (purified-code-block-profile-buffer/reset)
  ;; It is important to disable the buffers during reset so we don't have any
  ;;  random ignored samples dangling in the buffer.
  (let ((next-mt-slot-index
	 ;; Bletch: need to disable buffers but must sniff next-mt-slot-index
	 ;;         first, then must ensure nothing new is buffered.
	 (without-interrupts
	  (lambda () 
	    (let ((nmtsi
		   (purified-code-block-profile-buffer/next-empty-slot-index)))
	      ;; NB: No interrupts between LET rhs and following assignments
	      (fixed-purified-code-block-profile-buffers/disable)
	      nmtsi)))))
    ;; It is useful to keep a global var as a handle on this object.
    (cond ((and *purified-code-block-profile-block-buffer*
		*purified-code-block-profile-offset-buffer*) ;(should B synchd)
	   ;; Already initialized so avoid CONS-ing
	   (subvector-fill! *purified-code-block-profile-block-buffer*
			    0 next-mt-slot-index #F)
	   (subvector-fill! *purified-code-block-profile-offset-buffer*
			    0 next-mt-slot-index #F)
	   )
	  (else
	   ;; Else initialize them
	   (set! *purified-code-block-profile-block-buffer*
		 (pc-sample/code-block-buffer/make/purified-blocks))
	   (set! *purified-code-block-profile-offset-buffer*
		 (pc-sample/code-block-buffer/make/purified-offsets))
	   )))
  ;; Re-enable... synch kludge
  (fixed-purified-code-block-profile-buffers/install
        *purified-code-block-profile-block-buffer*
	*purified-code-block-profile-offset-buffer*)
  (cond ((pc-sample/uninitialized?)
	 (pc-sample/set-state! 'RESET)))
  'RESET)

(define (purified-code-block-profile-buffer/flush?)
  (not  (purified-code-block-profile-buffer/empty?)))

(define (purified-code-block-profile-buffer/spill-into-code-block-profile-tables)
  (let ((stop/restart-sampler? (and (not *pc-sample/sample-sampler?*)
				    (pc-sample/started?))))
    ;; stop if need be
    (cond (stop/restart-sampler? (fluid-let ((*pc-sample/noisy?* #F))
				   (pc-sample/stop))))
    ;; count if willed to
    (cond ((purified-code-block-profile-buffer/flush-count?)
	   (purified-code-block-profile-buffer/flush-count/1+)))
    ;; It is important to disable the buffers during spillage so we don't have
    ;;  random ignored samples dangling in the buffer.
    (let ((next-mt-slot-index
	   ;; Bletch: need to disable buffers but must sniff next-mt-slot-index
	   ;;         first, then must ensure nothing new is buffered.
	   (without-interrupts
	    (lambda () 
	      (let 
		 ((nmtsi
		   (purified-code-block-profile-buffer/next-empty-slot-index)))
		;; NB: No interrupts between LET rhs and following assignments
		(fixed-purified-code-block-profile-buffers/disable)
		nmtsi)))))
      (cond ((purified-code-block-profile-buffer/flush-noisy?)
	     (with-output-to-port console-output-port ; in case we're in Edwin
	       (lambda ()
		 (display "\n;> > > > > PCBPB Flush Request being serviced.")))
	     (output-port/flush-output console-output-port)))
      (do ((index 0 (1+ index)))
	  ((= index next-mt-slot-index))
	;; copy from buffer into hash table
	(purified-code-block-profile-tables/hash-entry
	 (vector-ref *purified-code-block-profile-block-buffer*  index)
	 (vector-ref *purified-code-block-profile-offset-buffer* index))
	;; Adios, amigos
	(vector-set! *purified-code-block-profile-block-buffer*  index #F)
	(vector-set! *purified-code-block-profile-offset-buffer* index #F)
	))
    ;; Re-enable... synch kludge
    (fixed-purified-code-block-profile-buffers/install
          *purified-code-block-profile-block-buffer*
	  *purified-code-block-profile-offset-buffer*)
    ;; restart if need be
    (cond (stop/restart-sampler? (fluid-let ((*pc-sample/noisy?* #F))
				   (pc-sample/start)))))
  unspecific)



(define-integrable (purified-code-block-profile-buffer/overflow-count?)
                                         (%pc-sample/PCBPB-monitoring?))
(define-integrable (purified-code-block-profile-buffer/overflow-count?/toggle!)
                                         (%pc-sample/PCBPB-monitoring?/toggle!))

(define (purified-code-block-profile-buffer/with-overflow-count! count? thunk)
  (let ((counting?      (purified-code-block-profile-buffer/overflow-count?))
	(want-no-count? (not count?)))	; coerce to Boolean
    (if (eq? counting? want-no-count?)	; xor want and got
	(dynamic-wind purified-code-block-profile-buffer/overflow-count?/toggle!
		      thunk
		      purified-code-block-profile-buffer/overflow-count?/toggle!)
	(thunk))))

(define-integrable (purified-code-block-profile-buffer/overflow-count      )
                                     (%pc-sample/PCBPB-overflow-count      ))
(define-integrable (purified-code-block-profile-buffer/overflow-count/reset)
                                     (%pc-sample/PCBPB-overflow-count/reset))

;;; Heathen Code Blocks

;;; TODO: flush/reset/spill/extend should all employ double buffering of the
;;;       code block profile buffers.

(define            *heathen-code-block-profile-buffer/extend-count?* #F)
(define-integrable (heathen-code-block-profile-buffer/extend-count?)
                   *heathen-code-block-profile-buffer/extend-count?*)
(define-integrable (heathen-code-block-profile-buffer/extend-count?/toggle!)
  (set!            *heathen-code-block-profile-buffer/extend-count?*
	      (not *heathen-code-block-profile-buffer/extend-count?*)))
(define            (heathen-code-block-profile-buffer/with-extend-count! count?
									 thunk)
  (fluid-let     ((*heathen-code-block-profile-buffer/extend-count?*     count?))
    (thunk)))
(define		   *heathen-code-block-profile-buffer/extend-count* 0)
(define-integrable (heathen-code-block-profile-buffer/extend-count)
		   *heathen-code-block-profile-buffer/extend-count*)
(define-integrable (heathen-code-block-profile-buffer/extend-count/reset)
  (set!		   *heathen-code-block-profile-buffer/extend-count* 0))
(define-integrable (heathen-code-block-profile-buffer/extend-count/1+)
  (set!		   *heathen-code-block-profile-buffer/extend-count*
	       (1+ *heathen-code-block-profile-buffer/extend-count*)))

(define (heathen-code-block-profile-buffer/extend)
  (let ((stop/restart-sampler? (and (not *pc-sample/sample-sampler?*)
				    (pc-sample/started?))))
    ;; stop if need be
    (cond (stop/restart-sampler? (fluid-let ((*pc-sample/noisy?* #F))
				   (pc-sample/stop))))
    ;; count if willed to
    (cond ((heathen-code-block-profile-buffer/extend-count?)
	   (heathen-code-block-profile-buffer/extend-count/1+)))
    ;; No need to disable during extend since we build an extended copy of the
    ;;  buffers then install them in one swell foop...
    ;; Of course, any profile samples made during the extend will be discarded.
    ;; For this reason, we go ahead and disable buffering anyway since
    ;;  it would be a waste of time.
    (fixed-heathen-code-block-profile-buffers/disable)
    (cond ((heathen-code-block-profile-buffer/extend-noisy?)
	   (with-output-to-port console-output-port ; in case we're in Edwin
	     (lambda ()
	       (display "\n;> > > > > HCBPB Extend Request being serviced.")))
	   (output-port/flush-output console-output-port)))
    (let* ((slack             (heathen-code-block-profile-buffer/slack ))
	   (old-buffer-length (heathen-code-block-profile-buffer/length))
	   (new-buffer-length (+ old-buffer-length slack)               )
	   (new-block-buffer 
	    (vector-grow *heathen-code-block-profile-block-buffer*
			 new-buffer-length))
	   (new-offset-buffer
	    (vector-grow *heathen-code-block-profile-offset-buffer*
			 new-buffer-length)))
      ;; INVARIANT: unused slots o heathen-code-block-profile-buffer must be #F
      (do ((index   old-buffer-length  (1+ index)))
	  ((= index new-buffer-length))
	(vector-set! new-block-buffer  index #F)
	(vector-set! new-offset-buffer index #F)
	)
      ;; Install new-buffers
      (set! *heathen-code-block-profile-block-buffer*  new-block-buffer)
      (set! *heathen-code-block-profile-offset-buffer* new-offset-buffer)
      ;; synch length cache
      (heathen-code-block-profile-buffer/length/set! new-buffer-length))
    ;; Re-enable ... synch kludge
    (fixed-heathen-code-block-profile-buffers/install
          *heathen-code-block-profile-block-buffer*
	  *heathen-code-block-profile-offset-buffer*)
    ;; restart if need be
    (cond (stop/restart-sampler? (fluid-let ((*pc-sample/noisy?* #F))
				   (pc-sample/start)))))
  unspecific)

(define            *heathen-code-block-profile-buffer/flush-count?* #F)
(define-integrable (heathen-code-block-profile-buffer/flush-count?)
                   *heathen-code-block-profile-buffer/flush-count?*)
(define-integrable (heathen-code-block-profile-buffer/flush-count?/toggle!)
  (set!            *heathen-code-block-profile-buffer/flush-count?*
	      (not *heathen-code-block-profile-buffer/flush-count?*)))
(define            (heathen-code-block-profile-buffer/with-flush-count! count?
									thunk)
  (fluid-let     ((*heathen-code-block-profile-buffer/flush-count?*     count?))
    (thunk)))
(define		   *heathen-code-block-profile-buffer/flush-count* 0)
(define-integrable (heathen-code-block-profile-buffer/flush-count)
		   *heathen-code-block-profile-buffer/flush-count*)
(define-integrable (heathen-code-block-profile-buffer/flush-count/reset)
  (set!		   *heathen-code-block-profile-buffer/flush-count* 0))
(define-integrable (heathen-code-block-profile-buffer/flush-count/1+)
  (set!		   *heathen-code-block-profile-buffer/flush-count*
	       (1+ *heathen-code-block-profile-buffer/flush-count*)))

(define-integrable (heathen-code-block-profile-buffer/flush)
  (cond
   ((and *heathen-code-block-profile-block-buffer*  ; not disabled
	 *heathen-code-block-profile-offset-buffer* ; (should be synch'd)
	 (heathen-code-block-profile-buffer/flush?))
    (heathen-code-block-profile-buffer/spill-into-code-block-profile-tables)))
  unspecific)

(define (heathen-code-block-profile-buffer/reset)
  ;; It is important to disable the buffers during reset so we don't have any
  ;;  random ignored samples dangling in the buffer.
  (let ((next-mt-slot-index
	 ;; Bletch: need to disable buffers but must sniff next-mt-slot-index
	 ;;         first, then must ensure nothing new is buffered.
	 (without-interrupts
	  (lambda () 
	    (let ((nmtsi
		   (heathen-code-block-profile-buffer/next-empty-slot-index)))
	      ;; NB: No interrupts between LET rhs and following assignments
	      (fixed-heathen-code-block-profile-buffers/disable)
	      nmtsi)))))
    ;; It is useful to keep a global var as a handle on this object.
    (cond ((and *heathen-code-block-profile-block-buffer*
		*heathen-code-block-profile-offset-buffer*) ;(should B synch'd)
	   ;; Already initialized so avoid CONS-ing
	   (subvector-fill! *heathen-code-block-profile-block-buffer*
			    0 next-mt-slot-index #F)
	   (subvector-fill! *heathen-code-block-profile-offset-buffer*
			    0 next-mt-slot-index #F)
	   )
	  (else
	   ;; Else initialize them
	   (set! *heathen-code-block-profile-block-buffer*
		 (pc-sample/code-block-buffer/make/heathen-blocks))
	   (set! *heathen-code-block-profile-offset-buffer*
		 (pc-sample/code-block-buffer/make/heathen-offsets))
	   )))
  ;; Re-enable ... synch kludge
  (fixed-heathen-code-block-profile-buffers/install
        *heathen-code-block-profile-block-buffer*
	*heathen-code-block-profile-offset-buffer*)
  (cond ((pc-sample/uninitialized?)
	 (pc-sample/set-state! 'RESET)))
  'RESET)

(define (heathen-code-block-profile-buffer/flush?)
  (not  (heathen-code-block-profile-buffer/empty?)))

(define (heathen-code-block-profile-buffer/spill-into-code-block-profile-tables)
  (let ((stop/restart-sampler? (and (not *pc-sample/sample-sampler?*)
				    (pc-sample/started?))))
    ;; stop if need be
    (cond (stop/restart-sampler? (fluid-let ((*pc-sample/noisy?* #F))
				   (pc-sample/stop))))
    ;; count if willed to
    (cond ((heathen-code-block-profile-buffer/flush-count?)
	   (heathen-code-block-profile-buffer/flush-count/1+)))
    ;; It is important to disable the buffers during spillage so we don't have
    ;;  any random ignored samples dangling in the buffer.
    (let ((next-mt-slot-index
	   ;; Bletch: need to disable buffers but must sniff next-mt-slot-index
	   ;;         first, then must ensure nothing new is buffered.
	   (without-interrupts
	    (lambda () 
	      (let
		  ((nmtsi
		    (heathen-code-block-profile-buffer/next-empty-slot-index)))
		;; NB: No interrupts between LET rhs and following assignments
		(fixed-heathen-code-block-profile-buffers/disable)
		nmtsi)))))
      (cond ((heathen-code-block-profile-buffer/flush-noisy?)
	     (with-output-to-port console-output-port ; in case we're in Edwin
	       (lambda ()
		 (display "\n;> > > > > HCBPB Flush Request being serviced.")))
	     (output-port/flush-output console-output-port)))
      (do ((index 0 (1+ index)))
	  ((= index next-mt-slot-index))
	;; copy from buffer into hash table
	(heathen-code-block-profile-tables/hash-entry
	 (vector-ref *heathen-code-block-profile-block-buffer*  index)
	 (vector-ref *heathen-code-block-profile-offset-buffer* index))
	;; Siyonara, Banzai!
	(vector-set! *heathen-code-block-profile-block-buffer*  index #F)
	(vector-set! *heathen-code-block-profile-offset-buffer* index #F)
	))
    ;; Re-enable... synch kludge
    (fixed-heathen-code-block-profile-buffers/install
          *heathen-code-block-profile-block-buffer*
	  *heathen-code-block-profile-offset-buffer*)
    ;; restart if need be
    (cond (stop/restart-sampler? (fluid-let ((*pc-sample/noisy?* #F))
				   (pc-sample/start)))))
  unspecific)



(define-integrable (heathen-code-block-profile-buffer/overflow-count?)
                                        (%pc-sample/HCBPB-monitoring?))
(define-integrable (heathen-code-block-profile-buffer/overflow-count?/toggle!)
                                        (%pc-sample/HCBPB-monitoring?/toggle!))

(define (heathen-code-block-profile-buffer/with-overflow-count! count? thunk)
  (let ((counting?      (heathen-code-block-profile-buffer/overflow-count?))
	(want-no-count? (not count?)))	; coerce to Boolean
    (if (eq? counting? want-no-count?)	; xor want and got
	(dynamic-wind heathen-code-block-profile-buffer/overflow-count?/toggle!
		      thunk
		      heathen-code-block-profile-buffer/overflow-count?/toggle!)
	(thunk))))

(define-integrable (heathen-code-block-profile-buffer/overflow-count      )
                                    (%pc-sample/HCBPB-overflow-count      ))
(define-integrable (heathen-code-block-profile-buffer/overflow-count/reset)
                                    (%pc-sample/HCBPB-overflow-count/reset))

;;; Code Block Profile (Hash) Tables are where compiled procs are profiled...
;;;   but the profile trap handler cannot CONS so if the current profiled
;;;   proc is not already hashed, we must buffer it in the Code Block Profile
;;;   Buffer until the GC Daemon gets around to hashing it.    
;;;
;;; Notice too that we maintain four distinct profile tables for each of the
;;;   two kinds of code blocks (purified and heathen). These four tables
;;;   are:
;;;         proc-cobl - code-block proc was completely isolated and identified
;;;          dbg-cobl - code-block proc not isolated but found debugging info
;;;          raw-cobl - code-block proc was not isolated and no debugging info
;;;        trampoline - trampoline code (e.g., manifest 
;;;
;;; This is because we may occasionally be unable to determine just which cobl
;;;  proc within a code block we were about to execute (e.g., may have been
;;;  in the head of the code block just when we sampled so did not yet jump
;;;  to proc in the code block). In such cases, we cannot profile the precise
;;;  cobl proc we were about to enter, so we just profile the code block as a
;;;  whole. These instances should be statistically fairly improbable.
;;;  The cases were we could not isolate the proc because the debugging info
;;;  was not available will be nil if all the ducky inf files are around...
;;;  but if some bozo deletes them all, we should at least not crash.
;;;  And until we teach the trampoline code to be more accomodating we will
;;;  keep it around after class to torture it at our leisure.

(define  *purified-proc-cobl-profile-table*)
(define   *heathen-proc-cobl-profile-table*)
(define   *purified-dbg-cobl-profile-table*)
(define    *heathen-dbg-cobl-profile-table*)
(define   *purified-raw-cobl-profile-table*)
(define    *heathen-raw-cobl-profile-table*)
(define *purified-trampoline-profile-table*)
(define  *heathen-trampoline-profile-table*)

(define ( proc-cobl-profile-table/make) (make-profile-hash-table 4096))
(define (  dbg-cobl-profile-table/make) (make-profile-hash-table 1024))
(define (  raw-cobl-profile-table/make) (make-profile-hash-table 2048))
(define (trampoline-profile-table/make) (make-profile-hash-table  512))

(define (code-block-profile-table)
  (vector ( purified-proc-cobl-profile-table)
	  (  purified-dbg-cobl-profile-table)
	  (  purified-raw-cobl-profile-table)
	  (purified-trampoline-profile-table)
	  (  heathen-proc-cobl-profile-table)
	  (   heathen-dbg-cobl-profile-table)
	  (   heathen-raw-cobl-profile-table)
	  ( heathen-trampoline-profile-table)
	  ))

(define (purified-proc-cobl-profile-table)
  (purified-code-block-profile-buffer/flush)
  (hash-table->alist *purified-proc-cobl-profile-table*))
(define ( heathen-proc-cobl-profile-table)
  ( heathen-code-block-profile-buffer/flush)
  (hash-table->alist  *heathen-proc-cobl-profile-table*))

(define (purified-dbg-cobl-profile-table)
  (purified-code-block-profile-buffer/flush)
  (hash-table->alist *purified-dbg-cobl-profile-table*))
(define ( heathen-dbg-cobl-profile-table)
  ( heathen-code-block-profile-buffer/flush)
  (hash-table->alist  *heathen-dbg-cobl-profile-table*))

(define (purified-raw-cobl-profile-table)
  (purified-code-block-profile-buffer/flush)
  (hash-table->alist *purified-raw-cobl-profile-table*))
(define ( heathen-raw-cobl-profile-table)
  ( heathen-code-block-profile-buffer/flush)
  (hash-table->alist  *heathen-raw-cobl-profile-table*))

(define (purified-trampoline-profile-table)
  (purified-code-block-profile-buffer/flush)
  (hash-table->alist *purified-trampoline-profile-table*))
(define ( heathen-trampoline-profile-table)
  ( heathen-code-block-profile-buffer/flush)
  (hash-table->alist  *heathen-trampoline-profile-table*))


(define (code-block-profile-table/old)
  (vector ( purified-proc-cobl-profile-table/old)
	  (  purified-dbg-cobl-profile-table/old)
	  (  purified-raw-cobl-profile-table/old)
	  (purified-trampoline-profile-table/old)
	  (  heathen-proc-cobl-profile-table/old)
	  (   heathen-dbg-cobl-profile-table/old)
	  (   heathen-raw-cobl-profile-table/old)
	  ( heathen-trampoline-profile-table/old)
	  ))

(define *purified-proc-cobl-profile-table/old* #F)
(define (purified-proc-cobl-profile-table/old)
        *purified-proc-cobl-profile-table/old*)
(define  *heathen-proc-cobl-profile-table/old* #F)
(define ( heathen-proc-cobl-profile-table/old)
         *heathen-proc-cobl-profile-table/old*)

(define *purified-dbg-cobl-profile-table/old* #F)
(define (purified-dbg-cobl-profile-table/old)
        *purified-dbg-cobl-profile-table/old*)
(define  *heathen-dbg-cobl-profile-table/old* #F)
(define ( heathen-dbg-cobl-profile-table/old)
         *heathen-dbg-cobl-profile-table/old*)

(define *purified-raw-cobl-profile-table/old* #F)
(define (purified-raw-cobl-profile-table/old)
        *purified-raw-cobl-profile-table/old*)
(define  *heathen-raw-cobl-profile-table/old* #F)
(define ( heathen-raw-cobl-profile-table/old)
         *heathen-raw-cobl-profile-table/old*)

(define *purified-trampoline-profile-table/old* #F)
(define (purified-trampoline-profile-table/old)
        *purified-trampoline-profile-table/old*)
(define  *heathen-trampoline-profile-table/old* #F)
(define ( heathen-trampoline-profile-table/old)
         *heathen-trampoline-profile-table/old*)


(define (code-block-profile-tables/reset #!optional disable?)
  (cond ((or (default-object? disable?) (not disable?))
	 (purified-code-block-profile-tables/reset)
	 ( heathen-code-block-profile-tables/reset))
	(else
	 (purified-code-block-profile-tables/reset disable?)
	 ( heathen-code-block-profile-tables/reset disable?))))

(define (purified-code-block-profile-tables/reset #!optional disable?)
  (set!  *purified-proc-cobl-profile-table/old*
	( purified-proc-cobl-profile-table))
  (set!   *purified-dbg-cobl-profile-table/old*
	  (purified-dbg-cobl-profile-table))
  (set!   *purified-raw-cobl-profile-table/old*
	  (purified-raw-cobl-profile-table))
  (set! *purified-trampoline-profile-table/old*
	(purified-trampoline-profile-table))
  (hash-table/clear!  *purified-proc-cobl-profile-table*)
  (hash-table/clear!   *purified-dbg-cobl-profile-table*)
  (hash-table/clear!   *purified-raw-cobl-profile-table*)
  (hash-table/clear! *purified-trampoline-profile-table*)
  (set!   *purified-code-block-profile-buffer/status/old*
	  (purified-code-block-profile-buffer/status))
  (cond ((and (not (default-object? disable?)) disable?)
	 ;; Disabling buffer disables table
	 (set! *purified-code-block-profile-block-buffer*  #F)
	 (set! *purified-code-block-profile-offset-buffer* #F)
	 (fixed-purified-code-block-profile-buffers/disable)
	 (if (pc-sample/initialized?)
	     'RESET-AND-DISABLED
	     'STILL-UNINITIALIZED))
	;; Disabled but wanna enable?
	((or (not *purified-code-block-profile-block-buffer*);(should B synchd)
	     (not *purified-code-block-profile-offset-buffer*))
	 (purified-code-block-profile-buffer/reset))
	(else
	 'RESET)))

(define (heathen-code-block-profile-tables/reset #!optional disable?)
  (set!  *heathen-proc-cobl-profile-table/old*
	( heathen-proc-cobl-profile-table))
  (set!   *heathen-dbg-cobl-profile-table/old*
	  (heathen-dbg-cobl-profile-table))
  (set!   *heathen-raw-cobl-profile-table/old*
	  (heathen-raw-cobl-profile-table))
  (set! *heathen-trampoline-profile-table/old*
        (heathen-trampoline-profile-table))
  (hash-table/clear!  *heathen-proc-cobl-profile-table*)
  (hash-table/clear!   *heathen-dbg-cobl-profile-table*)
  (hash-table/clear!   *heathen-raw-cobl-profile-table*)
  (hash-table/clear! *heathen-trampoline-profile-table*)
  (set! *heathen-code-block-profile-buffer/status/old*
	(heathen-code-block-profile-buffer/status))
  (cond ((and (not (default-object? disable?)) disable?)
	 ;; Disabling buffer disables table
	 (set! *heathen-code-block-profile-block-buffer*  #F)
	 (set! *heathen-code-block-profile-offset-buffer* #F)
	 (fixed-heathen-code-block-profile-buffers/disable)
	 (if (pc-sample/initialized?)
	     'RESET-AND-DISABLED
	     'STILL-UNINITIALIZED))
	;; Disabled but wanna enable?
	((or (not *heathen-code-block-profile-block-buffer*);(should be synchd)
	     (not *heathen-code-block-profile-offset-buffer*))
	 (heathen-code-block-profile-buffer/reset))
	(else
	 'RESET)))

(define    (code-block-profile-tables/enable)
  (purified-code-block-profile-tables/enable)
  ( heathen-code-block-profile-tables/enable))

(define (purified-code-block-profile-tables/enable)
        (purified-code-block-profile-tables/reset))
(define ( heathen-code-block-profile-tables/enable)
        ( heathen-code-block-profile-tables/reset))


(define    (code-block-profile-tables/disable)
  (purified-code-block-profile-tables/disable)
  ( heathen-code-block-profile-tables/disable))

(define (purified-code-block-profile-tables/disable)
        (purified-code-block-profile-tables/reset 'DISABLE))
(define ( heathen-code-block-profile-tables/disable)
        ( heathen-code-block-profile-tables/reset 'DISABLE))


;; Following three abstractions belong in udata.scm

(define-integrable (compiled-code-block/trampoline? block)
  (or         (not (compiled-code-block/normal?     block))
	      (trampoline/return-to-interpreter?    block)))

(define-integrable (compiled-code-block/normal? block)
  (object-type?
   (ucode-type manifest-vector)
   ;; This combination returns an unsafe object, but since it
   ;; is used as an argument to a primitive, I can get away
   ;; with not turning off the garbage collector.
   ((ucode-primitive primitive-object-ref 2) block 0)))

(define-integrable (trampoline/return-to-interpreter? block)
  ;;
  ;; Format of special magic return_to_interpreter trampoline:
  ;;  looks normal at first glance but really isn't... two constants in
  ;;  linkage section are small positive integers.. hence typecode 0
  ;;
  (and (fix:zero? (object-type (compiled-code-block/debugging-info block)))
       (fix:zero? (object-type (compiled-code-block/environment    block)))))


(define (purified-code-block-profile-tables/hash-entry cobl offset)
  "(code-block offset)\n\
   Hashes a purified code block and offset into the purified code block\n\
   profile table (actually, one of four: proc-cobl, dbg-cobl, raw-cobl, or\n\
   trampoline---\n\
   The proc-cobl hashes a compiled-procedure, dbg-cobl hashes debugging-info\n\
   descriptor [see runtime/infutl.scm read-debugging-info], and raw-cobl\n\
   hashes code block objects as does trampoline.\
  "
  ;; ``Purified'' code blocks are those which have been moved into constant
  ;;  space and therefore will not be moved by the garbage collector. Thus,
  ;;  it is possible to hash them by their absolute address. This can be more
  ;;  efficient than resorting to the underlying Scheme object hashing.
  (if (compiled-code-block/trampoline? cobl)
      (profile-hash-table/update-entry cobl
				       *purified-trampoline-profile-table*)
      (let ((cobl-dbg-info (compiled-code-block/dbg-info cobl 'demand-load)))
	(if (not cobl-dbg-info)		; Sigh. Debug info not accessible
	    (if (not (compiled-code-block/debugging-info? cobl))
		(profile-hash-table/update-entry
		   cobl
		   *purified-raw-cobl-profile-table*)
		(let ((debugging-key
		       ;; NB: Currently, the debugging info is stored in the
		       ;;     cobl so repeated accesses return EQ structures:
		       ;;     Hash on it
		       (compiled-code-block/debugging-info cobl)))
		  (profile-hash-table/update-entry
		     debugging-key
		     *purified-dbg-cobl-profile-table*)))
	    (let* ((cobl-procv (dbg-info/procedures cobl-dbg-info))
		   ;; Invariant: cobl-procv is a non-null vector
		   (cobl-proc 
		    (let ((last-index (-1+ (vector-length cobl-procv))))
		      (do ((index 0 (1+ index)))
			  ((or (= index last-index) ; last proc is it
			       (let ((next-proc (vector-ref cobl-procv
							    (1+ index))))
				 (> (dbg-procedure/label-offset next-proc)
				    offset)))
			   (vector-ref cobl-procv index))))))
	      ;; Paranoia for tracking down renegade samples
;;;	      (pp `(((cobl--- ,cobl)
;;;		     (datum-- ,(object-datum cobl))
;;;		     (offset- ,offset))
;;;		     (cprocv- ,cobl-procv)
;;;		     (cproc-- ,cobl-proc )
;;;		    ))
;;;	      (pp (reconstruct-compiled-procedure cobl cobl-proc))
	      (profile-hash-table/update-entry
	         (reconstruct-compiled-procedure cobl cobl-proc)
		 *purified-proc-cobl-profile-table*)
	      )))))

(define (heathen-code-block-profile-tables/hash-entry cobl offset)
  "(code-block offset)\n\
   Hashes a  heathen code block and offset into the  heathen code block\n\
   profile table (actually, one of four: proc-cobl, dbg-cobl, raw-cobl,\n\
   or trampoline---\n\
   The proc-cobl hashes a compiled-procedure, dbg-cobl hashes debugging-info\n\
   descriptor [see runtime/infutl.scm read-debugging-info], and raw-cobl\n\
   hashes code block objects as does trampoline.\
  "
  ;; ``Heathen'' code blocks are those which have not been ``purified'' into
  ;;  constant space so they can be moved about by the garbage collector.
  ;;  For that reason we cannot hash them off their absolute address because
  ;;  that can change. Instead, we use the usual hashing method.
  (if (compiled-code-block/trampoline? cobl)
      (profile-hash-table/update-entry cobl *heathen-trampoline-profile-table*)
      (let ((cobl-dbg-info (compiled-code-block/dbg-info cobl 'demand-load)))
	(if (not cobl-dbg-info)		; Sigh. Debug info not accessible
	    (if (not (compiled-code-block/debugging-info? cobl))
		(profile-hash-table/update-entry
		   cobl
		   *heathen-raw-cobl-profile-table*)
		(let ((debugging-key
		       ;; NB: Currently, the debugging info is stored in the
		       ;;     cobl so repeated accesses return EQ structures:
		       ;;     Hash on it
		       (compiled-code-block/debugging-info cobl)))
		  (profile-hash-table/update-entry
		     debugging-key
		     *heathen-dbg-cobl-profile-table*)))
	    (let* ((cobl-procv (dbg-info/procedures cobl-dbg-info))
		   ;; Invariant: cobl-procv is a non-null vector
		   (cobl-proc 
		    (let ((last-index (-1+ (vector-length cobl-procv))))
		      (do ((index 0 (1+ index)))
			  ((or (= index last-index) ; last proc is it
			       (let ((next-proc (vector-ref cobl-procv
							    (1+ index))))
				 (> (dbg-procedure/label-offset next-proc)
				    offset)))
			   (vector-ref cobl-procv index))))))
	      (profile-hash-table/update-entry
	         (reconstruct-compiled-procedure cobl cobl-proc)
		 *heathen-proc-cobl-profile-table*)
	      )))))

;;; *** Warning: This must be compiled to avoid a call to
;;; ***          with-absolutely-no-interrupts

(define (reconstruct-compiled-procedure cobl dbg-proc)
  (let ((offset (dbg-procedure/label-offset  dbg-proc)))
    (with-absolutely-no-interrupts
     (lambda ()
       ((ucode-primitive primitive-object-set-type)
	(ucode-type compiled-entry)
	(make-non-pointer-object
	 (+ offset (object-datum cobl))))))))
			      

(define (profile-hash-table/update-entry entry-key-obj profile-hash-table)
  (cond ((hash-table/get profile-hash-table entry-key-obj false)
	 =>
	 (lambda (datum)		; found
	   (code-block-profile-datum/update! datum)))
	(else				; not found
	 (hash-table/put! profile-hash-table
			  entry-key-obj
			  (code-block-profile-datum/make)))))

;;; Code Block Profile Datum

(define-structure (code-block-profile-datum
		   (conc-name code-block-profile-datum/)
		   (constructor code-block-profile-datum/make
				(#!optional count histogram rank utility)))
  (count     (code-block-profile-datum/count/make))
  (histogram (code-block-profile-datum/histogram/make))
  (rank      (code-block-profile-datum/rank/make))
  (utility   (code-block-profile-datum/utility/make))
  ;... more to come (?)
  )

(define (code-block-profile-datum/count/make)      1.0)	; FLONUM
(define (code-block-profile-datum/histogram/make) '#())
(define (code-block-profile-datum/rank/make)         0)
(define (code-block-profile-datum/utility/make)    0.0)	; FLONUM
;... more to come (?)

(define (code-block-profile-datum/update! datum)
  (set-code-block-profile-datum/count! 
     datum
     (flo:+ 1.0 (code-block-profile-datum/count datum))) ; FLONUM
  ;; histogram not yet implemented
  ;; rank      not yet implemented
  ;; utility   not yet implemented

  ;; NB: returns datum
  datum)

;;; fini
