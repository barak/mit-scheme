;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/struct.scm,v 1.78 1991/05/02 01:14:34 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989-91 Massachusetts Institute of Technology
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

;;;; Text Data Structures

(declare (usual-integrations))

;;; This file describes the data structures used to represent and
;;; manipulate text within the editor.

;;; The basic unit of text is the GROUP, which is essentially a type
;;; of character string with some special operations.  Normally a
;;; group is modified by side effect; unlike character strings, groups
;;; will grow and shrink appropriately under such operations.  Also,
;;; it is possible to have pointers into a group, called MARKs, which
;;; continue to point to the "same place" under these operations; this
;;; would not be true of a string, elements of which are pointed at by
;;; indices.

;;; As is stressed in the EMACS manual, marks point between characters
;;; rather than directly at them.  This perhaps counter-intuitive
;;; concept may aid understanding.

;;; Besides acting as pointers into a group, marks may be compared.
;;; All of the marks within a group are totally ordered, and the
;;; standard order predicates are supplied for them.  In addition,
;;; marks in different groups are unordered with respect to one
;;; another.  The standard predicates have been extended to be false
;;; in this case, and another predicate, which indicates whether they
;;; are related, is supplied.

;;; Marks may be paired into units called REGIONs.  Each region has a
;;; START mark and an END mark, and it must be the case that START is
;;; less than or equal to END in the mark ordering.  While in one
;;; sense this pairing of marks is trivial, it can also be used to
;;; reduce overhead in the implementation since a region guarantees
;;; that its marks satisfy this very basic relation.

;;; As in most other editors of this type, there is a distinction
;;; between "temporary" and "permanent" marks.  The purpose for this
;;; distinction is that temporary marks require less overhead to
;;; create.  Conversely, temporary marks do not remain valid when
;;; their group is modified.  They are intended for local use when it
;;; is known that the group will remain unchanged.

;;;; Groups

(define-named-structure "Group"
  ;; The microcode file "edwin.h" depends on the fields TEXT,
  ;; GAP-START, GAP-LENGTH, GAP-END, START-MARK, and END-MARK.
  text
  gap-start
  gap-length
  gap-end
  marks
  start-mark
  end-mark
  read-only?
  display-start
  display-end
  insert-daemons
  delete-daemons
  clip-daemons
  undo-data
  modified?
  point
  buffer
  )

(define (make-group string buffer)
  (let ((group (%make-group))
	(n (string-length string)))
    (vector-set! group group-index:text string)
    (vector-set! group group-index:gap-start n)
    (vector-set! group group-index:gap-length 0)
    (vector-set! group group-index:gap-end n)
    (vector-set! group group-index:marks '())
    (let ((start (make-permanent-mark group 0 false)))
      (vector-set! group group-index:start-mark start)
      (vector-set! group group-index:display-start start))
    (let ((end (make-permanent-mark group n true)))
      (vector-set! group group-index:end-mark end)
      (vector-set! group group-index:display-end end))
    (vector-set! group group-index:read-only? false)
    (vector-set! group group-index:insert-daemons '())
    (vector-set! group group-index:delete-daemons '())
    (vector-set! group group-index:clip-daemons '())
    (vector-set! group group-index:undo-data false)
    (vector-set! group group-index:modified? false)
    (vector-set! group group-index:point (make-permanent-mark group 0 true))
    (vector-set! group group-index:buffer buffer)
    group))

(define (group-length group)
  (fix:- (string-length (group-text group)) (group-gap-length group)))

(define-integrable (group-start-index group)
  (mark-index (group-start-mark group)))

(define-integrable (group-end-index group)
  (mark-index (group-end-mark group)))

(define-integrable (group-start-index? group index)
  (fix:<= index (group-start-index group)))

(define-integrable (group-end-index? group index)
  (fix:>= index (group-end-index group)))

(define-integrable (group-display-start-index group)
  (mark-index (group-display-start group)))

(define-integrable (group-display-end-index group)
  (mark-index (group-display-end group)))

(define-integrable (group-display-start-index? group index)
  (fix:<= index (group-display-start-index group)))

(define-integrable (group-display-end-index? group index)
  (fix:>= index (group-display-end-index group)))

(define-integrable (set-group-read-only! group)
  (vector-set! group group-index:read-only? true))

(define-integrable (set-group-writeable! group)
  (vector-set! group group-index:read-only? false))

(define-integrable (set-group-marks! group marks)
  (vector-set! group group-index:marks marks))

(define (group-region group)
  (%make-region (group-start-mark group) (group-end-mark group)))

(define (group-position->index group position)
  (group-position->index-integrable group position))

(define-integrable (group-position->index-integrable group position)
  (cond ((fix:<= position (group-gap-start group))
	 position)
	((fix:> position (group-gap-end group))
	 (fix:- position (group-gap-length group)))
	(else
	 (group-gap-start group))))

(define (group-index->position group index left-inserting?)
  (group-index->position-integrable group index left-inserting?))

(define-integrable (group-index->position-integrable group index
						     left-inserting?)
  (cond ((fix:< index (group-gap-start group))
	 index)
	((fix:> index (group-gap-start group))
	 (fix:+ index (group-gap-length group)))
	(left-inserting?
	 (group-gap-end group))
	(else
	 (group-gap-start group))))

(define-integrable (set-group-undo-data! group undo-data)
  (vector-set! group group-index:undo-data undo-data))

(define-integrable (set-group-modified! group sense)
  (vector-set! group group-index:modified? sense))

(define-integrable (set-group-point! group point)
  (vector-set! group group-index:point (mark-left-inserting-copy point)))

(define (group-absolute-start group)
  (make-temporary-mark group 0 false))

(define (group-absolute-end group)
  (make-temporary-mark group (group-length group) true))

;;;; Text Clipping

;;; Changes the group's start and end points, but doesn't affect the
;;; display.

(define (with-text-clipped start end thunk)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (with-group-text-clipped! (mark-group start)
			    (mark-index start)
			    (mark-index end)
			    thunk))

(define (text-clip start end)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (group-text-clip (mark-group start) (mark-index start) (mark-index end)))

(define (with-group-text-clipped! group start end thunk)
  (let ((old-text-start)
	(old-text-end)
	(new-text-start (make-permanent-mark group start false))
	(new-text-end (make-permanent-mark group end true)))
    (dynamic-wind (lambda ()
		    (set! old-text-start (group-start-mark group))
		    (set! old-text-end (group-end-mark group))
		    (vector-set! group group-index:start-mark new-text-start)
		    (vector-set! group group-index:end-mark new-text-end))
		  thunk
		  (lambda ()
		    (set! new-text-start (group-start-mark group))
		    (set! new-text-end (group-end-mark group))
		    (vector-set! group group-index:start-mark old-text-start)
		    (vector-set! group group-index:end-mark old-text-end)))))

(define (group-text-clip group start end)
  (let ((start (make-permanent-mark group start false))
	(end (make-permanent-mark group end true)))
    (vector-set! group group-index:start-mark start)
    (vector-set! group group-index:end-mark end)))

(define (invoke-group-daemons! daemons group start end)
  (let loop ((daemons daemons))
    (if (not (null? daemons))
	(begin
	  ((car daemons) group start end)
	  (loop (cdr daemons))))))

(define (record-insertion! group start end)
  (invoke-group-daemons! (group-insert-daemons group) group start end))

(define (add-group-insert-daemon! group daemon)
  (vector-set! group
	       group-index:insert-daemons
	       (cons daemon (vector-ref group group-index:insert-daemons))))

(define (remove-group-insert-daemon! group daemon)
  (vector-set! group
	       group-index:insert-daemons
	       (delq! daemon (vector-ref group group-index:insert-daemons))))

(define (record-deletion! group start end)
  (invoke-group-daemons! (group-delete-daemons group) group start end))

(define (add-group-delete-daemon! group daemon)
  (vector-set! group
	       group-index:delete-daemons
	       (cons daemon (vector-ref group group-index:delete-daemons))))

(define (remove-group-delete-daemon! group daemon)
  (vector-set! group
	       group-index:delete-daemons
	       (delq! daemon (vector-ref group group-index:delete-daemons))))

(define (record-clipping! group start end)
  (let ((buffer (group-buffer group)))
    (if (and buffer
	     (let ((display-start (buffer-display-start buffer)))
	       (and display-start
		    (let ((display-start (mark-index display-start)))
		      (or (fix:< display-start start)
			  (fix:> display-start end))))))
	(set-buffer-display-start! buffer false)))
  (invoke-group-daemons! (group-clip-daemons group) group start end))

(define (add-group-clip-daemon! group daemon)
  (vector-set! group
	       group-index:clip-daemons
	       (cons daemon (vector-ref group group-index:clip-daemons))))

(define (remove-group-clip-daemon! group daemon)
  (vector-set! group
	       group-index:clip-daemons
	       (delq! daemon (vector-ref group group-index:clip-daemons))))

(define (group-local-ref group variable)
  (variable-local-value (let ((buffer (group-buffer group)))
			  (if (not buffer)
			      (error:bad-range-argument group
							'GROUP-LOCAL-REF))
			  buffer)
			variable))

(define-integrable (group-tab-width group)
  (group-local-ref group (ref-variable-object tab-width)))

(define-integrable (group-case-fold-search group)
  (group-local-ref group (ref-variable-object case-fold-search)))

(define-integrable (group-syntax-table group)
  (group-local-ref group (ref-variable-object syntax-table)))

;;;; Marks

(define-structure (mark
		   (constructor make-temporary-mark)
		   (print-procedure
		    (unparser/standard-method 'MARK
		      (lambda (state mark)
			(unparse-object state
					(or (mark-buffer mark)
					    (mark-group mark)))
			(unparse-string state " ")
			(unparse-object state (mark-index mark))
			(unparse-string state
					(if (mark-left-inserting? mark)
					    " left"
					    " right"))))))
  ;; The microcode file "edwin.h" depends on the definition of this
  ;; structure.
  (group false read-only true)
  (index false)
  (left-inserting? false read-only true))

(define (guarantee-mark mark)
  (if (not (mark? mark)) (error "not a mark" mark))
  mark)

(define-integrable (make-mark group index)
  (make-temporary-mark group index true))

(define (move-mark-to! mark target)
  (set-mark-index! mark (mark-index target)))

(define (mark-temporary-copy mark)
  (make-temporary-mark (mark-group mark)
		       (mark-index mark)
		       (mark-left-inserting? mark)))

(define-integrable (mark-permanent-copy mark)
  (mark-permanent! (mark-temporary-copy mark)))

(define (mark-right-inserting mark)
  (if (mark-left-inserting? mark)
      (make-permanent-mark (mark-group mark) (mark-index mark) false)
      (mark-permanent! mark)))

(define (mark-right-inserting-copy mark)
  (make-permanent-mark (mark-group mark) (mark-index mark) false))

(define (mark-left-inserting mark)
  (if (mark-left-inserting? mark)
      (mark-permanent! mark)
      (make-permanent-mark (mark-group mark) (mark-index mark) true)))

(define (mark-left-inserting-copy mark)
  (make-permanent-mark (mark-group mark) (mark-index mark) true))

(define (make-permanent-mark group index left-inserting?)
  (let ((mark (make-temporary-mark group index left-inserting?)))
    (set-group-marks! group
		      (system-pair-cons (ucode-type weak-cons)
					mark
					(group-marks group)))
    mark))

(define (mark-permanent! mark)
  (let ((group (mark-group mark)))
    (if (not (weak-memq mark (group-marks group)))
	(set-group-marks! group
			  (system-pair-cons (ucode-type weak-cons)
					    mark
					    (group-marks group)))))
  mark)

(define-integrable (mark-local-ref mark variable)
  (group-local-ref (mark-group mark) variable))

(define-integrable (mark~ mark1 mark2)
  (eq? (mark-group mark1) (mark-group mark2)))

(define-integrable (mark/~ mark1 mark2)
  (not (mark~ mark1 mark2)))

(define (mark= mark1 mark2)
  (and (mark~ mark1 mark2)
       (fix:= (mark-index mark1) (mark-index mark2))))

(define (mark/= mark1 mark2)
  (and (mark~ mark1 mark2)
       (not (fix:= (mark-index mark1) (mark-index mark2)))))

(define (mark< mark1 mark2)
  (and (mark~ mark1 mark2)
       (fix:< (mark-index mark1) (mark-index mark2))))

(define (mark<= mark1 mark2)
  (and (mark~ mark1 mark2)
       (not (fix:> (mark-index mark1) (mark-index mark2)))))

(define (mark> mark1 mark2)
  (and (mark~ mark1 mark2)
       (fix:> (mark-index mark1) (mark-index mark2))))

(define (mark>= mark1 mark2)
  (and (mark~ mark1 mark2)
       (not (fix:< (mark-index mark1) (mark-index mark2)))))

(define-integrable (mark-buffer mark)
  (group-buffer (mark-group mark)))

(define-integrable (group-start mark)
  (group-start-mark (mark-group mark)))

(define-integrable (group-end mark)
  (group-end-mark (mark-group mark)))

(define (group-start? mark)
  (group-start-index? (mark-group mark) (mark-index mark)))

(define (group-end? mark)
  (group-end-index? (mark-group mark) (mark-index mark)))

(define (group-display-start? mark)
  (group-display-start-index? (mark-group mark) (mark-index mark)))

(define (group-display-end? mark)
  (group-display-end-index? (mark-group mark) (mark-index mark)))

(define-integrable (mark-absolute-start mark)
  (group-absolute-start (mark-group mark)))

(define-integrable (mark-absolute-end mark)
  (group-absolute-end (mark-group mark)))

;;; The next few procedures are simple algorithms that are haired up
;;; the wazoo for maximum speed.

(define (clean-group-marks! group)

  (define (scan-head marks)
    (cond ((null? marks)
	   (set-group-marks! group '()))
	  ((not (system-pair-car marks))
	   (scan-head (system-pair-cdr marks)))
	  (else
	   (set-group-marks! group marks)
	   (scan-tail marks (system-pair-cdr marks)))))

  (define (scan-tail previous marks)
    (cond ((null? marks)
	   unspecific)
	  ((not (system-pair-car marks))
	   (skip-nulls previous (system-pair-cdr marks)))
	  (else
	   (scan-tail marks (system-pair-cdr marks)))))

  (define (skip-nulls previous marks)
    (cond ((null? marks)
	   (system-pair-set-cdr! previous '()))
	  ((not (system-pair-car marks))
	   (skip-nulls previous (system-pair-cdr marks)))
	  (else
	   (system-pair-set-cdr! previous marks)
	   (scan-tail marks (system-pair-cdr marks)))))

  (let ((marks (group-marks group)))
    (cond ((null? marks)
	   unspecific)
	  ((not (system-pair-car marks))
	   (scan-head (system-pair-cdr marks)))
	  (else
	   (scan-tail marks (system-pair-cdr marks))))))

(define (mark-temporary! mark)
  ;; I'd think twice about using this one.
  (let ((group (mark-group mark)))

    (define (scan-head marks)
      (if (null? marks)
	  (set-group-marks! group '())
	  (let ((mark* (system-pair-car marks)))
	    (cond ((not mark*)
		   (scan-head (system-pair-cdr marks)))
		  ((eq? mark mark*)
		   (set-group-marks! group (system-pair-cdr marks)))
		  (else
		   (set-group-marks! group marks)
		   (scan-tail marks (system-pair-cdr marks)))))))

    (define (scan-tail previous marks)
      (if (not (null? marks))
	  (let ((mark* (system-pair-car marks)))
	    (cond ((not mark*)
		   (skip-nulls previous (system-pair-cdr marks)))
		  ((eq? mark mark*)
		   (system-pair-set-cdr! previous marks))
		  (else
		   (scan-tail marks (system-pair-cdr marks)))))))

    (define (skip-nulls previous marks)
      (if (null? marks)
	  (system-pair-set-cdr! previous '())
	  (let ((mark* (system-pair-car marks)))
	    (cond ((not mark*)
		   (skip-nulls previous (system-pair-cdr marks)))
		  ((eq? mark mark*)
		   (system-pair-set-cdr! previous (system-pair-cdr marks)))
		  (else
		   (system-pair-set-cdr! previous marks)
		   (scan-tail marks (system-pair-cdr marks)))))))

    (let ((marks (group-marks group)))
      (if (not (null? marks))
	  (let ((mark* (system-pair-car marks)))
	    (cond ((not mark*)
		   (scan-head (system-pair-cdr marks)))
		  ((eq? mark mark*)
		   (set-group-marks! group (system-pair-cdr marks)))
		  (else
		   (scan-tail marks (system-pair-cdr marks)))))))))

(define (find-permanent-mark group index left-inserting?)

  (define (scan-head marks)
    (if (null? marks)
	(begin
	  (set-group-marks! group '())
	  false)
	(let ((mark (system-pair-car marks)))
	  (cond ((not mark)
		 (scan-head (system-pair-cdr marks)))
		((and (if (mark-left-inserting? mark)
			  left-inserting?
			  (not left-inserting?))
		      (fix:= (mark-index mark) index))
		 mark)
		(else
		 (set-group-marks! group marks)
		 (scan-tail marks (system-pair-cdr marks)))))))

  (define (scan-tail previous marks)
    (and (not (null? marks))
	 (let ((mark (system-pair-car marks)))
	   (cond ((not mark)
		  (skip-nulls previous (system-pair-cdr marks)))
		 ((and (if (mark-left-inserting? mark)
			   left-inserting?
			   (not left-inserting?))
		       (fix:= (mark-index mark) index))
		  mark)
		 (else
		  (scan-tail marks (system-pair-cdr marks)))))))

  (define (skip-nulls previous marks)
    (if (null? marks)
	(begin
	  (system-pair-set-cdr! previous '())
	  false)
	(let ((mark (system-pair-car marks)))
	  (if (not mark)
	      (skip-nulls previous (system-pair-cdr marks))
	      (begin
		(system-pair-set-cdr! previous marks)
		(if (and (if (mark-left-inserting? mark)
			     left-inserting?
			     (not left-inserting?))
			 (fix:= (mark-index mark) index))
		    mark
		    (scan-tail marks (system-pair-cdr marks))))))))

  (let ((marks (group-marks group)))
    (and (not (null? marks))
	 (let ((mark (system-pair-car marks)))
	   (cond ((not mark)
		  (scan-head (system-pair-cdr marks)))
		 ((and (if (mark-left-inserting? mark)
			   left-inserting?
			   (not left-inserting?))
		       (fix:= (mark-index mark) index))
		  mark)
		 (else
		  (scan-tail marks (system-pair-cdr marks))))))))

(define (for-each-mark group procedure)

  (define (scan-head marks)
    (if (null? marks)
	(set-group-marks! group '())
	(let ((mark (system-pair-car marks))
	      (rest (system-pair-cdr marks)))
	  (if mark
	      (begin
		(set-group-marks! group marks)
		(procedure mark)
		(scan-tail marks rest))
	      (scan-head rest)))))

  (define (scan-tail previous marks)
    (if (not (null? marks))
	(let ((mark (system-pair-car marks))
	      (rest (system-pair-cdr marks)))
	  (if mark
	      (begin
		(procedure mark)
		(scan-tail marks rest))
	      (skip-nulls previous rest)))))

  (define (skip-nulls previous marks)
    (if (null? marks)
	(system-pair-set-cdr! previous '())
	(let ((mark (system-pair-car marks))
	      (rest (system-pair-cdr marks)))
	  (if mark
	      (begin
		(system-pair-set-cdr! previous marks)
		(procedure mark)
		(scan-tail marks rest))
	      (skip-nulls previous rest)))))

  (let ((marks (group-marks group)))
    (if (not (null? marks))
	(let ((mark (system-pair-car marks))
	      (rest (system-pair-cdr marks)))
	  (if mark
	      (begin
		(procedure mark)
		(scan-tail marks rest))
	      (scan-head rest))))))

;;;; Regions

(define-integrable %make-region cons)
(define-integrable region-start car)
(define-integrable region-end cdr)

(define (make-region start end)
  (cond ((not (eq? (mark-group start) (mark-group end)))
	 (error "Marks not related" start end))
	((fix:<= (mark-index start) (mark-index end))
	 (%make-region start end))
	(else
	 (%make-region end start))))

(define-integrable (region-group region)
  (mark-group (region-start region)))

(define-integrable (region-start-index region)
  (mark-index (region-start region)))

(define-integrable (region-end-index region)
  (mark-index (region-end region)))