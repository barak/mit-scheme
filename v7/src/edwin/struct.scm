;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Text Data Structures

(declare (usual-integrations))
(using-syntax edwin-syntax-table

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
  text gap-start gap-length gap-end
  marks start-mark end-mark read-only?
  display-start display-end
  insert-daemons delete-daemons clip-daemons
  undo-data modified? point)

(define-unparser %group-tag
  (lambda (group)
    (write-string "Group ")
    (write (primitive-datum group))))

(define (make-group string)
  (let ((group (%make-group))
	(n (string-length string)))
    (vector-set! group group-index:text string)
    (vector-set! group group-index:gap-start n)
    (vector-set! group group-index:gap-length 0)
    (vector-set! group group-index:gap-end n)
    (vector-set! group group-index:marks '())
    (let ((start (%make-permanent-mark group 0 #!FALSE)))
      (vector-set! group group-index:start-mark start)
      (vector-set! group group-index:display-start start))
    (let ((end (%make-permanent-mark group n #!TRUE)))
      (vector-set! group group-index:end-mark end)
      (vector-set! group group-index:display-end end))
    (vector-set! group group-index:read-only? #!FALSE)
    (vector-set! group group-index:insert-daemons '())
    (vector-set! group group-index:delete-daemons '())
    (vector-set! group group-index:clip-daemons '())
    (vector-set! group group-index:undo-data #!FALSE)
    (vector-set! group group-index:modified? #!FALSE)
    (vector-set! group group-index:point (%make-permanent-mark group 0 #!TRUE))
    group))

(declare (integrate group-start-index group-end-index
		    group-start-index? group-end-index?))

(define (group-length group)
  (- (string-length (group-text group)) (group-gap-length group)))

(define (group-start-index group)
  (declare (integrate group))
  (mark-index (group-start-mark group)))

(define (group-end-index group)
  (declare (integrate group))
  (mark-index (group-end-mark group)))

(define (group-start-index? group index)
  (declare (integrate group index))
  (<= index (group-start-index group)))

(define (group-end-index? group index)
  (declare (integrate group index))
  (>= index (group-end-index group)))

(define (set-group-read-only! group)
  (vector-set! group group-index:read-only? #!TRUE))

(define (set-group-writeable! group)
  (vector-set! group group-index:read-only? #!FALSE))

(define (group-region group)
  (%make-region (group-start-mark group) (group-end-mark group)))

(define (group-position->index group position)
  (cond ((> position (group-gap-end group))
	 (- position (group-gap-length group)))
	((> position (group-gap-start group))
	 (group-gap-start group))
	(else position)))

(define (group-index->position group index left-inserting?)
  (cond ((> index (group-gap-start group))
	 (+ index (group-gap-length group)))
	((= index (group-gap-start group))
	 (if left-inserting?
	     (group-gap-end group)
	     (group-gap-start group)))
	(else index)))

(define (set-group-undo-data! group undo-data)
  (vector-set! group group-index:undo-data undo-data))

(define (set-group-modified! group sense)
  (vector-set! group group-index:modified? sense))

(define (set-group-point! group point)
  (vector-set! group group-index:point (mark-left-inserting point)))

(define (with-narrowed-region! region thunk)
  (with-group-text-clipped! (region-group region)
			    (region-start-index region)
			    (region-end-index region)
			    thunk))

(define (with-group-text-clipped! group start end thunk)
  (define old-text-start)
  (define old-text-end)
  (define new-text-start (%make-permanent-mark group start #!FALSE))
  (define new-text-end (%make-permanent-mark group end #!TRUE))
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
		  (vector-set! group group-index:end-mark old-text-end))))

(define (record-insertion! group start end)
  (define (loop daemons)
    (if (not (null? daemons))
	(begin ((car daemons) group start end)
	       (loop (cdr daemons)))))
  (loop (group-insert-daemons group)))

(define (add-group-insert-daemon! group daemon)
  (vector-set! group group-index:insert-daemons
	       (cons daemon (vector-ref group group-index:insert-daemons))))

(define (remove-group-insert-daemon! group daemon)
  (vector-set! group group-index:insert-daemons
	       (delq! daemon (vector-ref group group-index:insert-daemons))))

(define (record-deletion! group start end)
  (define (loop daemons)
    (if (not (null? daemons))
	(begin ((car daemons) group start end)
	       (loop (cdr daemons)))))
  (loop (group-delete-daemons group)))

(define (add-group-delete-daemon! group daemon)
  (vector-set! group group-index:delete-daemons
	       (cons daemon (vector-ref group group-index:delete-daemons))))

(define (remove-group-delete-daemon! group daemon)
  (vector-set! group group-index:delete-daemons
	       (delq! daemon (vector-ref group group-index:delete-daemons))))

(define (record-clipping! group start end)
  (define (loop daemons)
    (if (not (null? daemons))
	(begin ((car daemons) group start end)
	       (loop (cdr daemons)))))
  (loop (group-clip-daemons group)))

(define (add-group-clip-daemon! group daemon)
  (vector-set! group group-index:clip-daemons
	       (cons daemon (vector-ref group group-index:clip-daemons))))

(define (remove-group-clip-daemon! group daemon)
  (vector-set! group group-index:clip-daemons
	       (delq! daemon (vector-ref group group-index:clip-daemons))))

;;;; Marks

(define-named-structure "Mark"
  group position left-inserting?)

(declare (integrate make-mark %make-permanent-mark %%make-mark
		    %set-mark-position! mark~))

(define (make-mark group index)
  (declare (integrate group index))
  (%make-temporary-mark group index #!TRUE))

(define (%make-permanent-mark group index left-inserting?)
  (declare (integrate group index left-inserting?))
  (mark-permanent! (%make-temporary-mark group index left-inserting?)))

(define (%make-temporary-mark group index left-inserting?)
  (%%make-mark group 
	       (group-index->position group index left-inserting?)
	       left-inserting?))

(define (%%make-mark group position left-inserting?)
  (declare (integrate group position left-inserting?))
  (let ((mark (%make-mark)))
    (vector-set! mark mark-index:group group)
    (vector-set! mark mark-index:position position)
    (vector-set! mark mark-index:left-inserting? left-inserting?)
    mark))

(define (mark-index mark)
  (group-position->index (mark-group mark) (mark-position mark)))

(define (%set-mark-position! mark position)
  (declare (integrate mark position))
  (vector-set! mark mark-index:position position))

(define (mark~ mark1 mark2)
  (declare (integrate mark1 mark2))
  (eq? (mark-group mark1) (mark-group mark2)))

(define (mark-right-inserting mark)
  (mark-permanent!
   (if (mark-left-inserting? mark)
       (%make-temporary-mark (mark-group mark) (mark-index mark) #!FALSE)
       mark)))

(define (mark-left-inserting mark)
  (mark-permanent!
   (if (mark-left-inserting? mark)
       mark
       (%make-temporary-mark (mark-group mark) (mark-index mark) #!TRUE))))

;;; The marks list is cleaned every time that FOR-EACH-MARK! is
;;; called.  It may be necessary to do this a little more often.

(declare (compilable-primitive-functions object-hash))

(define (mark-permanent! mark)
  (let ((n (object-hash mark))
	(marks (group-marks (mark-group mark))))
    (if (not (memq n marks))
	(vector-set! (mark-group mark) group-index:marks (cons n marks))))
  mark)

(define (for-each-mark group procedure)
  (define (loop-1 marks)
    (if (not (null? marks))
	(let ((mark (object-unhash (car marks))))
	  (if mark
	      (begin (procedure mark)
		     (loop-2 marks (cdr marks)))
	      (begin (vector-set! group group-index:marks (cdr marks))
		     (loop-1 (cdr marks)))))))

  (define (loop-2 previous marks)
    (if (not (null? marks))
	(let ((mark (object-unhash (car marks))))
	  (if mark
	      (begin (procedure mark)
		     (loop-2 marks (cdr marks)))
	      (begin (set-cdr! previous (cddr previous))
		     (loop-2 previous (cdr previous)))))))

  (loop-1 (group-marks group)))

(define (mark/~ mark1 mark2)
  (not (mark~ mark1 mark2)))

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

(declare (integrate group-start group-end))

(define (group-start mark)
  (declare (integrate mark))
  (group-start-mark (mark-group mark)))

(define (group-end mark)
  (declare (integrate mark))
  (group-end-mark (mark-group mark)))

(define (group-start? mark)
  (group-start-index? (mark-group mark) (mark-index mark)))

(define (group-end? mark)
  (group-end-index? (mark-group mark) (mark-index mark)))

;;;; Regions

(declare (integrate %make-region region-start region-end))

(define %make-region cons)
(define region-start car)
(define region-end cdr)

(define (make-region start end)
  (cond ((mark<= start end) (%make-region start end))
	((mark<= end start) (%make-region end start))
	(else (error "Marks not related" start end))))
(declare (integrate region-group region-start-index region-end-index))

(define (region-group region)
  (declare (integrate region))
  (mark-group (region-start region)))

(define (region-start-index region)
  (declare (integrate region))
  (mark-index (region-start region)))

(define (region-end-index region)
  (declare (integrate region))
  (mark-index (region-end region)))

;;; end USING-SYNTAX
)