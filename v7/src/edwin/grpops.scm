;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/grpops.scm,v 1.11 1991/04/12 23:19:05 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-91 Massachusetts Institute of Technology
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

;;;; Extractions

(define (group-extract-string group start end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(string (make-string (fix:- end start))))
    (cond ((fix:<= end gap-start)
	   (substring-move-right! text start end string 0))
	  ((fix:>= start gap-start)
	   (substring-move-right! text
				  (fix:+ start (group-gap-length group))
				  (fix:+ end (group-gap-length group))
				  string
				  0))
	  (else
	   (substring-move-right! text start gap-start string 0)
	   (substring-move-right! text
				  (group-gap-end group)
				  (fix:+ end (group-gap-length group))
				  string
				  (fix:- gap-start start))))
    string))

(define (group-left-char group index)
  (string-ref (group-text group)
	      (fix:-1+ (group-index->position-integrable group index false))))

(define (group-right-char group index)
  (string-ref (group-text group)
	      (group-index->position-integrable group index true)))

;;;; Insertions

(define (group-insert-char! group index char)
  (without-interrupts
   (lambda ()
     (%group-insert-char! group index char)
     (record-insertion! group index (group-gap-start group)))))

(define-integrable (%group-insert-char! group index char)
  (if (group-read-only? group) (barf-if-read-only))
  (move-gap-to! group index)
  (guarantee-gap-length! group 1)
  (let ((gap-start* (fix:1+ index)))
    (undo-record-insertion! group index gap-start*)
    (vector-set! group group-index:gap-start gap-start*))
  (vector-set! group group-index:gap-length (fix:-1+ (group-gap-length group)))
  (string-set! (group-text group) index char)
  (for-each-mark group
    (lambda (mark)
      (let ((index* (mark-index mark)))
	(if (or (fix:> index* index)
		(and (fix:= index* index)
		     (mark-left-inserting? mark)))
	    (set-mark-index! mark (fix:+ index* 1)))))))

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
  (let ((n (fix:- end start)))
    (guarantee-gap-length! group n)
    (let ((gap-start* (fix:+ index n)))
      (undo-record-insertion! group index gap-start*)
      (vector-set! group group-index:gap-start gap-start*))
    (vector-set! group
		 group-index:gap-length
		 (fix:- (group-gap-length group) n))
    (substring-move-right! string start end (group-text group) index)
    (for-each-mark group
      (lambda (mark)
	(let ((index* (mark-index mark)))
	  (if (or (fix:> index* index)
		  (and (fix:= index* index)
		       (mark-left-inserting? mark)))
	      (set-mark-index! mark (fix:+ index* n))))))))

;;;; Deletions

(define (group-delete-left-char! group index)
  (group-delete! group (fix:-1+ index) index))

(define (group-delete-right-char! group index)
  (group-delete! group index (fix:1+ index)))

(define (group-delete! group start end)
  (without-interrupts
   (lambda ()
     (if (not (fix:= start end))
	 (begin
	   (if (group-read-only? group) (barf-if-read-only))
	   ;; Guarantee that the gap is between START and END.
	   (let ((gap-start (group-gap-start group)))
	     (cond ((fix:< gap-start start) (move-gap-to-right! group start))
		   ((fix:> gap-start end) (move-gap-to-left! group end))))
	   (undo-record-deletion! group start end)
	   (record-deletion! group start end)
	   (let ((length (fix:- end start)))
	     (for-each-mark group
	       (lambda (mark)
		 (let ((index (mark-index mark)))
		   (cond ((fix:> index end)
			  (set-mark-index! mark (fix:- index length)))
			 ((fix:>= index start)
			  (set-mark-index! mark start)))))))
	   (vector-set! group group-index:gap-start start)
	   (let ((gap-end (fix:+ end (group-gap-length group)))
		 (max-gap-length gap-maximum-extra))
	     (if (fix:> (fix:- gap-end start) max-gap-length)
		 (let* ((new-gap-end (fix:+ start max-gap-length))
			(text (group-text group))
			(text-end (string-length text))
			(new-text-end
			 (fix:- text-end
				(fix:- (fix:- gap-end start) max-gap-length))))
		   (substring-move-left! text gap-end text-end
					 text new-gap-end)
		   (set-string-maximum-length! text new-text-end)
		   (vector-set! group group-index:gap-end new-gap-end)
		   (vector-set! group group-index:gap-length max-gap-length))
		 (begin
		   (vector-set! group group-index:gap-end gap-end)
		   (vector-set! group group-index:gap-length
				(fix:- gap-end start))))))))))

;;;; The Gap

(define (move-gap-to! group index)
  (let ((gap-start (group-gap-start group)))
    (cond ((fix:< index gap-start) (move-gap-to-left! group index))
	  ((fix:> index gap-start) (move-gap-to-right! group index)))))

(define (move-gap-to-left! group new-start)
  (let ((start (group-gap-start group))
	(length (group-gap-length group))
	(text (group-text group)))
    (let ((new-end (fix:+ new-start length)))
      (substring-move-right! text new-start start text new-end)
      (vector-set! group group-index:gap-start new-start)
      (vector-set! group group-index:gap-end new-end))))

(define (move-gap-to-right! group new-start)
  (let ((start (group-gap-start group))
	(end (group-gap-end group))
	(length (group-gap-length group))
	(text (group-text group)))
    (let ((new-end (fix:+ new-start length)))
      (substring-move-left! text end new-end text start)
      (vector-set! group group-index:gap-start new-start)
      (vector-set! group group-index:gap-end new-end))))

(define (guarantee-gap-length! group n)
  (if (fix:< (group-gap-length group) n)
      (let ((n
	     (fix:+ (fix:- n (group-gap-length group))
		    gap-allocation-extra))
	    (text (group-text group))
	    (start (group-gap-start group))
	    (end (group-gap-end group))
	    (length (group-gap-length group)))
	(let ((end* (string-length text)))
	  (let ((text* (string-allocate (fix:+ end* n)))
		(new-end (fix:+ end n)))
	    (substring-move-right! text 0 start text* 0)
	    (substring-move-right! text end end* text* new-end)
	    (vector-set! group group-index:text text*)
	    (vector-set! group group-index:gap-end new-end)))
	(vector-set! group group-index:gap-length (fix:+ length n)))))