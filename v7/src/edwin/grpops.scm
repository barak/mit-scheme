;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/grpops.scm,v 1.5 1989/04/28 03:56:02 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Group Operations

(declare (usual-integrations))

;;; These high-performance ops deal directly with groups and indices
;;; for speed and the least consing.  Since indices are not in general
;;; valid across modifications to the group, they can only be used in
;;; limited ways.  To save an index across a modification, it must be
;;; consed into a permanent mark.

;;; This parameter controls how much extra space (in characters) is
;;; allocated when the gap is too small to contain a given insertion.
(define gap-allocation-extra 2000)

;;; This parameter controls how large the gap is allowed to be between
;;; operations.  It must be at least `gap-allocation-extra'.
(define gap-maximum-extra 20000)

(define (group-extract-string group start end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(length (group-gap-length group)))
    (cond ((<= end gap-start)
	   (substring text start end))
	  ((>= start gap-start)
	   (substring text (+ start length) (+ end length)))
	  (else
	   (let ((string (string-allocate (- end start))))
	     (substring-move-right! text start gap-start string 0)
	     (substring-move-right! text (group-gap-end group) (+ end length)
				    string (- gap-start start))
	     string)))))

(define (group-left-char group index)
  (string-ref (group-text group)
	      (-1+ (group-index->position group index false))))

(define (group-right-char group index)
  (string-ref (group-text group)
	      (group-index->position group index true)))

(define (group-insert-char! group index char)
  (without-interrupts
   (lambda ()
     (%group-insert-char! group index char)
     (record-insertion! group index (group-gap-start group)))))

(define-integrable (%group-insert-char! group index char)
  (if (group-read-only? group) (barf-if-read-only))
  (move-gap-to! group index)
  (guarantee-gap-length! group 1)
  (string-set! (group-text group) index char)
  (vector-set! group group-index:gap-length (-1+ (group-gap-length group)))
  (let ((gap-start* (1+ index)))
    (vector-set! group group-index:gap-start gap-start*)
    (undo-record-insertion! group index gap-start*)))

(define (group-insert-string! group index string)
  (group-insert-substring! group index string 0 (string-length string)))

(define (group-insert-substring! group index string start end)
  (without-interrupts
   (lambda ()
     (%group-insert-substring! group index string start end)
     (record-insertion! group index (group-gap-start group)))))

(define-integrable (%group-insert-substring! group index string start end)
  (if (group-read-only? group) (barf-if-read-only))
  (move-gap-to! group index)
  (let ((n (- end start)))
    (guarantee-gap-length! group n)
    (substring-move-right! string start end (group-text group) index)
    (vector-set! group group-index:gap-length (- (group-gap-length group) n))
    (let ((gap-start* (+ index n)))
      (vector-set! group group-index:gap-start gap-start*)
      (undo-record-insertion! group index gap-start*))))

(define (group-delete-left-char! group index)
  (group-delete! group (-1+ index) index))

(define (group-delete-right-char! group index)
  (group-delete! group index (1+ index)))

(define (group-delete! group start end)
  (without-interrupts
   (lambda ()
     (if (not (= start end))
	 (begin
	   (if (group-read-only? group) (barf-if-read-only))
	   ;; Guarantee that the gap is between START and END.
	   (let ((gap-start (group-gap-start group)))
	     (cond ((< gap-start start) (move-gap-to-right! group start))
		   ((> gap-start end) (move-gap-to-left! group end))))
	   (undo-record-deletion! group start end)
	   (record-deletion! group start end)
	   (let* ((end (+ end (group-gap-length group)))
		  (length (- end start))
		  (max-length gap-maximum-extra))
	     (if (> length max-length)
		 (let* ((new-end (+ start max-length))
			(difference (- length max-length))
			(text (group-text group))
			(end* (string-length text))
			(new-end* (- end* difference)))
		   (substring-move-left! text end end* text new-end)
		   (set-string-maximum-length! text new-end*)
		   (for-each-mark group
		     (lambda (mark)
		       (let ((position (mark-position mark)))
			 (cond ((> position end)
				(set-mark-position! mark
						    (- position difference)))
			       ((<= start position)
				(set-mark-position!
				 mark
				 (if (mark-left-inserting? mark)
				     new-end
				     start)))))))
		   (vector-set! group group-index:gap-start start)
		   (vector-set! group group-index:gap-end new-end)
		   (vector-set! group group-index:gap-length max-length))
		 (begin
		   (for-each-mark group
		     (lambda (mark)
		       (let ((position (mark-position mark)))
			 (if (and (<= start position)
				  (<= position end))
			     (set-mark-position!
			      mark
			      (if (mark-left-inserting? mark) end start))))))
		   (vector-set! group group-index:gap-start start)
		   (vector-set! group group-index:gap-end end)
		   (vector-set! group group-index:gap-length length))))
	     unspecific)))))

;;;; The Gap

(define (move-gap-to! group index)
  (let ((gap-start (group-gap-start group)))
    (cond ((< index gap-start) (move-gap-to-left! group index))
	  ((> index gap-start) (move-gap-to-right! group index)))))

(define (move-gap-to-left! group new-start)
  (let ((start (group-gap-start group))
	(length (group-gap-length group))
	(text (group-text group)))
    (let ((new-end (+ new-start length)))
      (for-each-mark group
	(lambda (mark)
	  (let ((position (mark-position mark)))
	    (cond ((and (< new-start position) (<= position start))
		   (set-mark-position! mark (+ position length)))
		  ((and (mark-left-inserting? mark) (= new-start position))
		   (set-mark-position! mark new-end))))))
      (substring-move-right! text new-start start text new-end)
      (vector-set! group group-index:gap-start new-start)
      (vector-set! group group-index:gap-end new-end)))
  unspecific)

(define (move-gap-to-right! group new-start)
  (let ((start (group-gap-start group))
	(end (group-gap-end group))
	(length (group-gap-length group))
	(text (group-text group)))
    (let ((new-end (+ new-start length)))
      (for-each-mark group
	(lambda (mark)
	  (let ((position (mark-position mark)))
	    (cond ((and (> new-end position) (>= position end))
		   (set-mark-position! mark (- position length)))
		  ((and (not (mark-left-inserting? mark)) (= new-end position))
		   (set-mark-position! mark new-start))))))
      (substring-move-left! text end new-end text start)
      (vector-set! group group-index:gap-start new-start)
      (vector-set! group group-index:gap-end new-end)))
  unspecific)

(define (guarantee-gap-length! group n)
  (if (< (group-gap-length group) n)
      (let ((n (+ n gap-allocation-extra))
	    (text (group-text group))
	    (start (group-gap-start group))
	    (end (group-gap-end group))
	    (length (group-gap-length group)))
	(let ((end* (string-length text)))
	  (let ((text* (string-allocate (+ end* n)))
		(new-end (+ end n)))
	    (substring-move-right! text 0 start text* 0)
	    (substring-move-right! text end end* text* new-end)
	    (vector-set! group group-index:text text*)
	    (vector-set! group group-index:gap-end new-end)
	    (if (zero? length)
		(for-each-mark group
		  (lambda (mark)
		    (let ((position (mark-position mark)))
		      (cond ((> position end)
			     (set-mark-position! mark (+ position n)))
			    ((= position end)
			     (set-mark-position!
			      mark
			      (if (mark-left-inserting? mark)
				  new-end
				  start)))))))
		(for-each-mark group
		  (lambda (mark)
		    (let ((position (mark-position mark)))
		      (if (>= position end)
			  (set-mark-position! mark (+ position n)))))))))
	(vector-set! group group-index:gap-length (+ length n))))
  unspecific)