;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/struct.scm,v 1.65 1989/03/14 08:03:01 cph Exp $
;;;
;;;	Copyright (c) 1985, 1989 Massachusetts Institute of Technology
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

;;; The implementation of marks is different from previous
;;; implementations.  In particular, it is not possible to tell
;;; whether a mark is temporary or permanent.  Instead, a "caller
;;; saves"-like convention is used.  Whenever any given mark needs to
;;; be permanent, one merely calls a procedure which "permanentizes"
;;; it.  All marks are created temporary by default.

;;;; Groups

(define-named-structure "Group"
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
  )

(define (make-group string)
  (let ((group (%make-group))
	(n (string-length string)))
    (vector-set! group group-index:text string)
    (vector-set! group group-index:gap-start n)
    (vector-set! group group-index:gap-length 0)
    (vector-set! group group-index:gap-end n)
    (vector-set! group group-index:marks '())
    (let ((start (%make-permanent-mark group 0 false)))
      (vector-set! group group-index:start-mark start)
      (vector-set! group group-index:display-start start))
    (let ((end (%make-permanent-mark group n true)))
      (vector-set! group group-index:end-mark end)
      (vector-set! group group-index:display-end end))
    (vector-set! group group-index:read-only? false)
    (vector-set! group group-index:insert-daemons '())
    (vector-set! group group-index:delete-daemons '())
    (vector-set! group group-index:clip-daemons '())
    (vector-set! group group-index:undo-data false)
    (vector-set! group group-index:modified? false)
    (vector-set! group group-index:point (%make-permanent-mark group 0 true))
    group))

(define (group-length group)
  (- (string-length (group-text group)) (group-gap-length group)))

(define-integrable (group-start-index group)
  (mark-index (group-start-mark group)))

(define-integrable (group-end-index group)
  (mark-index (group-end-mark group)))

(define-integrable (group-start-index? group index)
  (<= index (group-start-index group)))

(define-integrable (group-end-index? group index)
  (>= index (group-end-index group)))

(define-integrable (set-group-read-only! group)
  (vector-set! group group-index:read-only? true)
  unspecific)

(define-integrable (set-group-writeable! group)
  (vector-set! group group-index:read-only? false)
  unspecific)

(define (group-region group)
  (%make-region (group-start-mark group) (group-end-mark group)))

(define (group-position->index group position)
  (cond ((> position (group-gap-end group))
	 (- position (group-gap-length group)))
	((> position (group-gap-start group))
	 (group-gap-start group))
	(else
	 position)))

(define (group-index->position group index left-inserting?)
  (cond ((> index (group-gap-start group))
	 (+ index (group-gap-length group)))
	((= index (group-gap-start group))
	 (if left-inserting?
	     (group-gap-end group)
	     (group-gap-start group)))
	(else
	 index)))

(define-integrable (set-group-undo-data! group undo-data)
  (vector-set! group group-index:undo-data undo-data)
  unspecific)

(define-integrable (set-group-modified! group sense)
  (vector-set! group group-index:modified? sense)
  unspecific)

(define-integrable (set-group-point! group point)
  (vector-set! group group-index:point (mark-left-inserting point))
  unspecific)

(define (with-narrowed-region! region thunk)
  (with-group-text-clipped! (region-group region)
			    (region-start-index region)
			    (region-end-index region)
			    thunk))

(define (with-group-text-clipped! group start end thunk)
  (let ((old-text-start)
	(old-text-end)
	(new-text-start (%make-permanent-mark group start false))
	(new-text-end (%make-permanent-mark group end true)))
    (dynamic-wind (lambda ()
		    (set! old-text-start (group-start-mark group))
		    (set! old-text-end (group-end-mark group))
		    (vector-set! group group-index:start-mark new-text-start)
		    (vector-set! group group-index:end-mark new-text-end)
		    unspecific)
		  thunk
		  (lambda ()
		    (set! new-text-start (group-start-mark group))
		    (set! new-text-end (group-end-mark group))
		    (vector-set! group group-index:start-mark old-text-start)
		    (vector-set! group group-index:end-mark old-text-end)
		    unspecific))))

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
	       (cons daemon (vector-ref group group-index:insert-daemons)))
  unspecific)

(define (remove-group-insert-daemon! group daemon)
  (vector-set! group
	       group-index:insert-daemons
	       (delq! daemon (vector-ref group group-index:insert-daemons)))
  unspecific)

(define (record-deletion! group start end)
  (invoke-group-daemons! (group-delete-daemons group) group start end))

(define (add-group-delete-daemon! group daemon)
  (vector-set! group
	       group-index:delete-daemons
	       (cons daemon (vector-ref group group-index:delete-daemons)))
  unspecific)

(define (remove-group-delete-daemon! group daemon)
  (vector-set! group
	       group-index:delete-daemons
	       (delq! daemon (vector-ref group group-index:delete-daemons)))
  unspecific)

(define (record-clipping! group start end)
  (invoke-group-daemons! (group-clip-daemons group) group start end))

(define (add-group-clip-daemon! group daemon)
  (vector-set! group
	       group-index:clip-daemons
	       (cons daemon (vector-ref group group-index:clip-daemons)))
  unspecific)

(define (remove-group-clip-daemon! group daemon)
  (vector-set! group
	       group-index:clip-daemons
	       (delq! daemon (vector-ref group group-index:clip-daemons)))
  unspecific)

;;;; Marks

(define-named-structure "Mark"
  group position left-inserting?)

(define (guarantee-mark mark procedure-name)
  (if (not (mark? mark)) (error "not a mark" mark procedure-name)))

(define-integrable (make-mark group index)
  (%make-temporary-mark group index true))

(define-integrable (%make-permanent-mark group index left-inserting?)
  (mark-permanent! (%make-temporary-mark group index left-inserting?)))

(define (%make-temporary-mark group index left-inserting?)
  (%%make-mark group 
	       (group-index->position group index left-inserting?)
	       left-inserting?))

(define-integrable (%%make-mark group position left-inserting?)
  (let ((mark (%make-mark)))
    (vector-set! mark mark-index:group group)
    (vector-set! mark mark-index:position position)
    (vector-set! mark mark-index:left-inserting? left-inserting?)
    mark))

(define (mark-index mark)
  (group-position->index (mark-group mark) (mark-position mark)))

(define-integrable (%set-mark-position! mark position)
  (vector-set! mark mark-index:position position)
  unspecific)

(define-integrable (mark~ mark1 mark2)
  (eq? (mark-group mark1) (mark-group mark2)))

(define-integrable (mark/~ mark1 mark2)
  (not (mark~ mark1 mark2)))

(define (mark-right-inserting mark)
  (mark-permanent!
   (if (mark-left-inserting? mark)
       (%make-temporary-mark (mark-group mark) (mark-index mark) false)
       mark)))

(define (mark-left-inserting mark)
  (mark-permanent!
   (if (mark-left-inserting? mark)
       mark
       (%make-temporary-mark (mark-group mark) (mark-index mark) true))))

;;; The marks list is cleaned every time that FOR-EACH-MARK! is
;;; called.  It may be necessary to do this a little more often.

(define (mark-permanent! mark)
  (let ((group (mark-group mark)))
    (let ((marks (group-marks group)))
      (if (not (weak-memq mark marks))
	  (vector-set! group group-index:marks (weak-cons mark marks)))))
  mark)

(define (for-each-mark group procedure)
  (let loop
      ((marks (group-marks group))
       (set-holder!
	(lambda (new-marks) (vector-set! group group-index:marks new-marks))))
    (if (not (null? marks))
	(loop (weak-cdr marks)
	      (let ((mark (weak-car marks)))
		(if mark
		    (begin
		      (procedure mark)
		      (lambda (new-cdr) (weak-set-cdr! marks new-cdr)))
		    (begin
		      (set-holder! (weak-cdr marks))
		      set-holder!)))))))

(define (mark= mark1 mark2)
  (and (mark~ mark1 mark2)
       (= (mark-index mark1) (mark-index mark2))))

(define (mark/= mark1 mark2)
  (and (mark~ mark1 mark2)
       (not (= (mark-index mark1) (mark-index mark2)))))

(define (mark< mark1 mark2)
  (and (mark~ mark1 mark2)
       (< (mark-index mark1) (mark-index mark2))))

(define (mark<= mark1 mark2)
  (and (mark~ mark1 mark2)
       (<= (mark-index mark1) (mark-index mark2))))

(define (mark> mark1 mark2)
  (and (mark~ mark1 mark2)
       (> (mark-index mark1) (mark-index mark2))))

(define (mark>= mark1 mark2)
  (and (mark~ mark1 mark2)
       (>= (mark-index mark1) (mark-index mark2))))

(define-integrable (group-start mark)
  (group-start-mark (mark-group mark)))

(define-integrable (group-end mark)
  (group-end-mark (mark-group mark)))

(define (group-start? mark)
  (group-start-index? (mark-group mark) (mark-index mark)))

(define (group-end? mark)
  (group-end-index? (mark-group mark) (mark-index mark)))

;;;; Regions

(define-integrable %make-region cons)
(define-integrable region-start car)
(define-integrable region-end cdr)

(define (make-region start end)
  (cond ((mark<= start end) (%make-region start end))
	((mark<= end start) (%make-region end start))
	(else (error "Marks not related" start end))))
(define-integrable (region-group region)
  (mark-group (region-start region)))

(define-integrable (region-start-index region)
  (mark-index (region-start region)))

(define-integrable (region-end-index region)
  (mark-index (region-end region)))