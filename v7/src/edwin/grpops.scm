;;; -*-Scheme-*-
;;;
;;;	$Id: grpops.scm,v 1.22 1995/04/17 21:46:10 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-95 Massachusetts Institute of Technology
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

;;;; Group Operations

;;; These high-performance ops deal directly with groups and indices
;;; for speed and the least consing.  Since indices are not in general
;;; valid across modifications to the group, they can only be used in
;;; limited ways.  To save an index across a modification, it must be
;;; consed into a permanent mark.

(declare (usual-integrations string-allocate))

;;;; Extractions

(define (group-extract-string group start end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(string (string-allocate (fix:- end start))))
    (cond ((fix:<= end gap-start)
	   (%substring-move! text start end string 0))
	  ((fix:>= start gap-start)
	   (%substring-move! text
			     (fix:+ start (group-gap-length group))
			     (fix:+ end (group-gap-length group))
			     string
			     0))
	  (else
	   (%substring-move! text start gap-start string 0)
	   (%substring-move! text
			     (group-gap-end group)
			     (fix:+ end (group-gap-length group))
			     string
			     (fix:- gap-start start))))
    string))

(define (group-copy-substring! group start end string start*)
  (let ((text (group-text group))
	(gap-start (group-gap-start group)))
    (cond ((fix:<= end gap-start)
	   (%substring-move! text start end string start*))
	  ((fix:>= start gap-start)
	   (%substring-move! text
			     (fix:+ start (group-gap-length group))
			     (fix:+ end (group-gap-length group))
			     string
			     start*))
	  (else
	   (%substring-move! text start gap-start string start*)
	   (%substring-move! text
			     (group-gap-end group)
			     (fix:+ end (group-gap-length group))
			     string
			     (fix:+ start* (fix:- gap-start start)))))))

(define (group-left-char group index)
  (string-ref (group-text group)
	      (fix:- (group-index->position-integrable group index false) 1)))

(define (group-right-char group index)
  (string-ref (group-text group)
	      (group-index->position-integrable group index true)))

(define (group-extract-and-delete-string! group start end)
  (let ((string (group-extract-string group start end)))
    (group-delete! group start end)
    string))

;;;; Insertions

(define (group-insert-char! group index char)
  (group-insert-chars! group index char 1))

(define (group-insert-chars! group index char n)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (prepare-gap-for-insert! group index n)
    (let ((text (group-text group))
	  (end (fix:+ index n)))
      (do ((index index (fix:+ index 1)))
	  ((fix:= index end))
	(string-set! text index char)))
    (finish-group-insert! group index n)
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (group-insert-string! group index string)
  (group-insert-substring! group index string 0 (string-length string)))

(define (group-insert-substring! group index string start end)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((n (fix:- end start)))
      (prepare-gap-for-insert! group index n)
      (%substring-move! string start end (group-text group) index)
      (finish-group-insert! group index n))
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (prepare-gap-for-insert! group new-start n)
  (if (or (group-read-only? group)
	  (and (group-text-properties group)
	       (text-not-insertable? group new-start)))
      (barf-if-read-only))
  (if (not (group-modified? group)) (check-first-group-modification group))
  (cond ((fix:< (group-gap-length group) n)
	 (grow-group! group new-start n))
	((fix:< new-start (group-gap-start group))
	 (let ((new-end (fix:+ new-start (group-gap-length group))))
	   (%substring-move! (group-text group)
			     new-start
			     (group-gap-start group)
			     (group-text group)
			     new-end)
	   (vector-set! group group-index:gap-start new-start)
	   (vector-set! group group-index:gap-end new-end)))
	((fix:> new-start (group-gap-start group))
	 (let ((new-end (fix:+ new-start (group-gap-length group))))
	   (%substring-move! (group-text group)
			     (group-gap-end group)
			     new-end
			     (group-text group)
			     (group-gap-start group))
	   (vector-set! group group-index:gap-start new-start)
	   (vector-set! group group-index:gap-end new-end)))))

(define (finish-group-insert! group index n)
  (vector-set! group group-index:gap-start (fix:+ index n))
  (vector-set! group group-index:gap-length (fix:- (group-gap-length group) n))
  (if (group-start-changes-index group)
      (begin
	(if (fix:< index (group-start-changes-index group))
	    (set-group-start-changes-index! group index))
	(set-group-end-changes-index!
	 group
	 (if (fix:> index (group-end-changes-index group))
	     (fix:+ index n)
	     (fix:+ (group-end-changes-index group) n))))
      (begin
	(set-group-start-changes-index! group index)
	(set-group-end-changes-index! group (fix:+ index n))))
  (do ((marks (group-marks group) (system-pair-cdr marks)))
      ((null? marks))
    (if (and (system-pair-car marks)
	     (or (fix:> (mark-index (system-pair-car marks)) index)
		 (and (fix:= (mark-index (system-pair-car marks)) index)
		      (mark-left-inserting? (system-pair-car marks)))))
	(set-mark-index! (system-pair-car marks)
			 (fix:+ (mark-index (system-pair-car marks)) n))))
  (vector-set! group group-index:modified-tick
	       (fix:+ (group-modified-tick group) 1))
  (undo-record-insertion! group index (fix:+ index n))
  ;; The MODIFIED? bit must be set *after* the undo recording.
  (set-group-modified?! group true)
  (if (group-text-properties group)
      (update-intervals-for-insertion! group index n)))

;;;; Deletions

(define (group-delete-left-char! group index)
  (group-delete! group (fix:- index 1) index))

(define (group-delete-right-char! group index)
  (group-delete! group index (fix:+ index 1)))

(define (group-delete! group start end)
  (if (not (fix:= start end))
      (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	(let ((text (group-text group))
	      (gap-length (group-gap-length group)))
	  (if (or (group-read-only? group)
		  (and (group-text-properties group)
		       (text-not-deleteable? group start end)))
	      (barf-if-read-only))
	  (if (not (group-modified? group))
	      (check-first-group-modification group))
	  ;; Guarantee that the gap is between START and END.  This is
	  ;; best done before the undo recording.
	  (cond ((fix:< (group-gap-start group) start)
		 (%substring-move! text
				   (group-gap-end group)
				   (fix:+ start gap-length)
				   text
				   (group-gap-start group)))
		((fix:> (group-gap-start group) end)
		 (%substring-move! text
				   end
				   (group-gap-start group)
				   text
				   (fix:+ end gap-length))))
	  ;; The undo recording must occur *before* the deletion.
	  (undo-record-deletion! group start end)
	  (let ((gap-end (fix:+ end gap-length)))
	    (vector-set! group group-index:gap-start start)
	    (vector-set! group group-index:gap-end gap-end)
	    (vector-set! group group-index:gap-length (fix:- gap-end start))
	    (if (and (group-shrink-length group)
		     (fix:<= (fix:- (string-length text)
				    (fix:- gap-end start))
			     (group-shrink-length group)))
		(shrink-group! group))))
	(let ((n (fix:- end start)))
	  (if (group-start-changes-index group)
	      (begin
		(if (fix:< start (group-start-changes-index group))
		    (set-group-start-changes-index! group start))
		(set-group-end-changes-index!
		 group
		 (if (fix:>= end (group-end-changes-index group))
		     start
		     (fix:- (group-end-changes-index group) n))))
	      (begin
		(set-group-start-changes-index! group start)
		(set-group-end-changes-index! group start)))
	  (do ((marks (group-marks group) (system-pair-cdr marks)))
	      ((null? marks))
	    (cond ((or (not (system-pair-car marks))
		       (fix:<= (mark-index (system-pair-car marks)) start))
		   unspecific)
		  ((fix:<= (mark-index (system-pair-car marks)) end)
		   (set-mark-index! (system-pair-car marks) start))
		  (else
		   (set-mark-index!
		    (system-pair-car marks)
		    (fix:- (mark-index (system-pair-car marks)) n))))))
	(vector-set! group group-index:modified-tick
		     (fix:+ (group-modified-tick group) 1))
	;; The MODIFIED? bit must be set *after* the undo recording.
	(set-group-modified?! group true)
	(if (group-text-properties group)
	    (update-intervals-for-deletion! group start end))
	(set-interrupt-enables! interrupt-mask)
	unspecific)))

;;;; Resizing

(define (grow-group! group new-gap-start n)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(gap-end (group-gap-end group))
	(reallocation-factor (group-reallocation-factor group)))
    (let ((text-length (string-length text))
	  (gap-delta (- new-gap-start gap-start)))
      (let ((n-chars (- text-length (group-gap-length group))))
	(let ((new-text-length
	       (let ((minimum-text-length (+ n-chars n)))
		 (let loop ((length (if (= text-length 0) 1 text-length)))
		   (let ((length (ceiling (* length reallocation-factor))))
		     (if (< length minimum-text-length)
			 (loop length)
			 length))))))
	  (let ((new-text (string-allocate new-text-length))
		(new-gap-length (- new-text-length n-chars)))
	    (let ((new-gap-end (+ new-gap-start new-gap-length)))
	      (cond ((= gap-delta 0)
		     (%substring-move! text 0 gap-start new-text 0)
		     (%substring-move! text gap-end text-length
				       new-text new-gap-end))
		    ((< gap-delta 0)
		     (%substring-move! text 0 new-gap-start new-text 0)
		     (%substring-move! text new-gap-start gap-start
				       new-text new-gap-end)
		     (%substring-move! text gap-end text-length
				       new-text (- new-gap-end gap-delta)))
		    (else
		     (let ((ngsp (+ gap-end gap-delta)))
		       (%substring-move! text 0 gap-start new-text 0)
		       (%substring-move! text gap-end ngsp new-text gap-start)
		       (%substring-move! text ngsp text-length
					 new-text new-gap-end))))
	      (vector-set! group group-index:text new-text)
	      (vector-set! group group-index:gap-start new-gap-start)
	      (vector-set! group group-index:gap-end new-gap-end)
	      (vector-set! group group-index:gap-length new-gap-length))))))
    (memoize-shrink-length! group reallocation-factor)))

(define (shrink-group! group)
  (let ((text (group-text group))
	(gap-length (group-gap-length group))
	(reallocation-factor (group-reallocation-factor group)))
    (let ((text-length (string-length text)))
      (let ((n-chars (- text-length gap-length)))
	(let ((new-text-length
	       (if (= n-chars 0)
		   0
		   (let loop ((length text-length))
		     (let ((length (floor (/ length reallocation-factor))))
		       (let ((sl
			      (compute-shrink-length length
						     reallocation-factor)))
			 (if (< sl n-chars)
			     length
			     (loop length)))))))
	      (gap-end (group-gap-end group)))
	  (let ((delta (- text-length new-text-length)))
	    (let ((new-gap-end (- gap-end delta)))
	      (%substring-move! text gap-end text-length text new-gap-end)
	      (vector-set! group group-index:gap-end new-gap-end)
	      (vector-set! group group-index:gap-length (- gap-length delta))))
	  (set-string-maximum-length! text new-text-length))))
    (memoize-shrink-length! group reallocation-factor)))

(define (memoize-shrink-length! group reallocation-factor)
  (vector-set! group group-index:shrink-length
	       (compute-shrink-length (string-length (group-text group))
				      reallocation-factor)))

(define (compute-shrink-length length reallocation-factor)
  (floor (/ (floor (/ length reallocation-factor)) reallocation-factor)))

(define (group-reallocation-factor group)
  ;; We assume the result satisfies (LAMBDA (G) (AND (REAL? G) (> G 1)))
  (inexact->exact (ref-variable buffer-reallocation-factor group)))