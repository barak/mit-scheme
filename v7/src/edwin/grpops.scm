;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/grpops.scm,v 1.16 1992/04/04 13:07:09 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989-92 Massachusetts Institute of Technology
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
(define-integrable gap-allocation-extra 2000)

;;; This parameter controls how large the gap is allowed to be between
;;; operations.  It must be at least `gap-allocation-extra'.
(define-integrable gap-maximum-extra 20000)

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

(define (group-copy-substring! group start end string start*)
  (let ((text (group-text group))
	(gap-start (group-gap-start group)))
    (cond ((fix:<= end gap-start)
	   (substring-move-right! text start end string start*))
	  ((fix:>= start gap-start)
	   (substring-move-right! text
				  (fix:+ start (group-gap-length group))
				  (fix:+ end (group-gap-length group))
				  string
				  start*))
	  (else
	   (substring-move-right! text start gap-start string start*)
	   (substring-move-right! text
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
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (declare (integrate %group-insert-char!))
    (%group-insert-char! group index char)
    (if (not (null? (group-insert-daemons group)))
	(invoke-group-daemons! (group-insert-daemons group)
			       group index (group-gap-start group)))
    (set-interrupt-enables! interrupt-mask)))

(define (%group-insert-char! group index char)
  (if (group-read-only? group)
      (barf-if-read-only))
  (if (not (group-modified? group))
      (check-first-group-modification group))
  (if (group-undo-data group)
      (undo-record-insertion! group index (fix:+ index 1)))
  (prepare-gap-for-insert! group index 1)
  (string-set! (group-text group) index char)
  (finish-group-insert! group index 1))

(define (group-insert-string! group index string)
  (group-insert-substring! group index string 0 (string-length string)))

(define (group-insert-substring! group index string start end)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (declare (integrate %group-insert-substring!))
    (%group-insert-substring! group index string start end)
    (if (not (null? (group-insert-daemons group)))
	(invoke-group-daemons! (group-insert-daemons group)
			       group index (group-gap-start group)))
    (set-interrupt-enables! interrupt-mask)))

(define (%group-insert-substring! group index string start end)
  (if (group-read-only? group)
      (barf-if-read-only))
  (if (not (group-modified? group))
      (check-first-group-modification group))
  (let ((n (fix:- end start)))
    (if (group-undo-data group)
	(undo-record-insertion! group index (fix:+ index n)))
    (prepare-gap-for-insert! group index n)
    (substring-move-right! string start end (group-text group) index)
    (finish-group-insert! group index n)))

(define-integrable (prepare-gap-for-insert! group new-start n)
  (cond ((fix:< new-start (group-gap-start group))
	 (let ((new-end (fix:+ new-start (group-gap-length group))))
	   (substring-move-right! (group-text group)
				  new-start
				  (group-gap-start group)
				  (group-text group)
				  new-end)
	   (vector-set! group group-index:gap-start new-start)
	   (vector-set! group group-index:gap-end new-end)))
	((fix:> new-start (group-gap-start group))
	 (let ((new-end (fix:+ new-start (group-gap-length group))))
	   (substring-move-left! (group-text group)
				 (group-gap-end group)
				 new-end
				 (group-text group)
				 (group-gap-start group))
	   (vector-set! group group-index:gap-start new-start)
	   (vector-set! group group-index:gap-end new-end))))
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

(define-integrable (finish-group-insert! group index n)
  (vector-set! group group-index:gap-start (fix:+ index n))
  (vector-set! group group-index:gap-length (fix:- (group-gap-length group) n))
  (do ((marks (group-marks group) (system-pair-cdr marks)))
      ((null? marks))
    (if (and (system-pair-car marks)
	     (or (fix:> (mark-index (system-pair-car marks)) index)
		 (and (fix:= (mark-index (system-pair-car marks)) index)
		      (mark-left-inserting? (system-pair-car marks)))))
	(set-mark-index! (system-pair-car marks)
			 (fix:+ (mark-index (system-pair-car marks)) n))))
  ;; The MODIFIED? bit must not be set until after the undo record is made.
  (set-group-modified! group true))

;;;; Deletions

(define (group-delete-left-char! group index)
  (group-delete! group (fix:- index 1) index))

(define (group-delete-right-char! group index)
  (group-delete! group index (fix:+ index 1)))

(define (group-delete! group start end)
  (if (not (fix:= start end))
      (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
	(if (group-read-only? group)
	    (barf-if-read-only))
	(if (not (group-modified? group))
	    (check-first-group-modification group))
	(if (group-undo-data group)
	    (undo-record-deletion! group start end))
	(if (not (null? (group-delete-daemons group)))
	    (invoke-group-daemons! (group-delete-daemons group)
				   group start end))
	;; The MODIFIED? bit must not be set until after the undo
	;; record is made.
	(set-group-modified! group true)
	(let ((length (fix:- end start)))
	  (do ((marks (group-marks group) (system-pair-cdr marks)))
	      ((null? marks))
	    (cond ((or (not (system-pair-car marks))
		       (fix:< (mark-index (system-pair-car marks)) start))
		   unspecific)
		  ((fix:<= (mark-index (system-pair-car marks)) end)
		   (set-mark-index! (system-pair-car marks) start))
		  (else
		   (set-mark-index!
		    (system-pair-car marks)
		    (fix:- (mark-index (system-pair-car marks)) length))))))
	;; Guarantee that the gap is between START and END.
	(cond ((fix:< (group-gap-start group) start)
	       (let ((text (group-text group))
		     (new-end (fix:+ start (group-gap-length group))))
		 (do ((index (group-gap-end group) (fix:+ index 1))
		      (index* (group-gap-start group) (fix:+ index* 1)))
		     ((not (fix:< index new-end)))
		   (string-set! text index* (string-ref text index)))))
	      ((fix:> (group-gap-start group) end)
	       (let ((text (group-text group)))
		 (do ((index (group-gap-start group) (fix:- index 1))
		      (index* (group-gap-end group) (fix:- index* 1)))
		     ((not (fix:< end index)))
		   (string-set! text
				(fix:- index* 1)
				(string-ref text (fix:- index 1)))))))
	(vector-set! group group-index:gap-start start)
	(let ((gap-end (fix:+ end (group-gap-length group))))
	  (if (fix:> (fix:- gap-end start) gap-maximum-extra)
	      (let* ((new-gap-end (fix:+ start gap-allocation-extra))
		     (text (group-text group))
		     (text-end (string-length text)))
		(substring-move-left! text gap-end text-end
				      text new-gap-end)
		(set-string-maximum-length! text
					    (fix:+ new-gap-end
						   (fix:- text-end gap-end)))
		(vector-set! group group-index:gap-end new-gap-end)
		(vector-set! group group-index:gap-length
			     gap-allocation-extra))
	      (begin
		(vector-set! group group-index:gap-end gap-end)
		(vector-set! group group-index:gap-length
			     (fix:- gap-end start)))))
	(set-interrupt-enables! interrupt-mask))))