#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

(define-structure (group
		   (type vector)
		   (named)
		   (constructor %make-group (buffer)))
  ;; The microcode file "edwin.h" depends on this structure being a
  ;; named vector, and knows the indexes of the fields TEXT,
  ;; GAP-START, GAP-LENGTH, GAP-END, START-MARK, END-MARK, and
  ;; MODIFIED?.
  text
  (gap-start 0)
  (gap-length 0)
  (gap-end 0)
  (marks (weak-list-set eq?) read-only #t)
  start-mark
  end-mark
  (writeable? #t)
  display-start
  display-end
  (start-changes-index #f)
  (end-changes-index #f)
  (modified-tick 0)
  (clip-daemons '())
  (undo-data #f)
  (modified? #f)
  %point
  buffer
  (shrink-length 0)
  (text-properties #f)
  (%hash-number #f))

(define-integrable group-point group-%point)

(define (make-group buffer)
  (let ((group (%make-group buffer)))
    (set-group-text! group (allocate-buffer-storage 0))
    (let ((start (make-permanent-mark group 0 #f)))
      (set-group-start-mark! group start)
      (set-group-display-start! group start))
    (let ((end (make-permanent-mark group 0 #t)))
      (set-group-end-mark! group end)
      (set-group-display-end! group end))
    (set-group-%point! group (make-permanent-mark group 0 #t))
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

(define-integrable (set-group-writeable! group)
  (set-group-writeable?! group #t))

(define-integrable (set-group-read-only! group)
  (set-group-writeable?! group #f))

(define-integrable (group-read-only? group)
  (not (group-writeable? group)))

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

(define-integrable (set-group-point! group point)
  (set-group-point-index! group (mark-index point)))

(define-integrable (set-group-point-index! group index)
  ;; Optimization causes lossage.  -- cph
  ;; (set-mark-index! (group-point group) index)
  (set-group-%point! group (make-permanent-mark group index #t)))

(define (group-absolute-start group)
  (make-temporary-mark group 0 #f))

(define (group-absolute-end group)
  (make-temporary-mark group (group-length group) #t))

(define (group-hash-number group)
  (or (group-%hash-number group)
      (let ((n (hash-object group)))
	(set-group-%hash-number! group n)
	n)))

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

(define (without-text-clipped group thunk)
  (let ((group
	 (cond ((group? group) group)
	       ((buffer? group) (buffer-group group))
	       ((mark? group) (mark-group group))
	       (else
		(error:wrong-type-argument group "text group"
					   'WITHOUT-TEXT-CLIPPED)))))
    (with-group-text-clipped! group 0 (group-length group) thunk)))

(define (text-clip start end)
  (if (not (mark<= start end))
      (error "Marks incorrectly related:" start end))
  (group-text-clip (mark-group start) (mark-index start) (mark-index end)))

(define (with-group-text-clipped! group start end thunk)
  (let ((old-text-start)
	(old-text-end)
	(new-text-start (make-permanent-mark group start #f))
	(new-text-end (make-permanent-mark group end #t)))
    (dynamic-wind (lambda ()
		    (set! old-text-start (group-start-mark group))
		    (set! old-text-end (group-end-mark group))
		    (set-group-start-mark! group new-text-start)
		    (set-group-end-mark! group new-text-end))
		  thunk
		  (lambda ()
		    (set! new-text-start (group-start-mark group))
		    (set! new-text-end (group-end-mark group))
		    (set-group-start-mark! group old-text-start)
		    (set-group-end-mark! group old-text-end)))))

(define (group-text-clip group start end)
  (let ((start (make-permanent-mark group start #f))
	(end (make-permanent-mark group end #t)))
    (set-group-start-mark! group start)
    (set-group-end-mark! group end)))

(define (record-clipping! group start end)
  (let ((buffer (group-buffer group)))
    (if (and buffer
	     (let ((display-start (buffer-display-start buffer)))
	       (and display-start
		    (let ((display-start (mark-index display-start)))
		      (or (fix:< display-start start)
			  (fix:> display-start end))))))
	(set-buffer-display-start! buffer #f)))
  (invoke-group-daemons! (group-clip-daemons group) group start end))

(define (invoke-group-daemons! daemons group start end)
  (let loop ((daemons daemons))
    (if (not (null? daemons))
	(begin
	  ((car daemons) group start end)
	  (loop (cdr daemons))))))

(define (add-group-clip-daemon! group daemon)
  (set-group-clip-daemons! group (cons daemon (group-clip-daemons group))))

(define (remove-group-clip-daemon! group daemon)
  (set-group-clip-daemons! group (delq! daemon (group-clip-daemons group))))

(define (group-local-ref group variable)
  (variable-local-value (let ((buffer (group-buffer group)))
			  (if (not buffer)
			      (error:bad-range-argument group
							'GROUP-LOCAL-REF))
			  buffer)
			variable))

(define-integrable (group-tab-width group)
  (group-local-ref group (ref-variable-object tab-width)))

(define-integrable (group-char-image-strings group)
  (group-local-ref group (ref-variable-object char-image-strings)))

(define-integrable (group-case-fold-search group)
  (group-local-ref group (ref-variable-object case-fold-search)))

(define-integrable (group-syntax-table group)
  (group-local-ref group (ref-variable-object syntax-table)))

;;;; Marks

(define-structure (mark
		   (constructor make-temporary-mark)
		   (print-procedure
		    (standard-print-method 'MARK
		      (lambda (mark)
			(list (or (mark-buffer mark)
				  (mark-group mark))
			      (mark-index mark)
			      (if (mark-left-inserting? mark)
				  'left
				  'right))))))
  ;; The microcode file "edwin.h" depends on the definition of this
  ;; structure.
  (group #f read-only #t)
  (index #f)
  (left-inserting? #f read-only #t))

(define (guarantee-mark mark)
  (if (not (mark? mark)) (error "not a mark" mark))
  mark)

(define-integrable (make-mark group index)
  (make-temporary-mark group index #t))

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
      (make-permanent-mark (mark-group mark) (mark-index mark) #f)
      (mark-permanent! mark)))

(define (mark-right-inserting-copy mark)
  (make-permanent-mark (mark-group mark) (mark-index mark) #f))

(define (mark-left-inserting mark)
  (if (mark-left-inserting? mark)
      (mark-permanent! mark)
      (make-permanent-mark (mark-group mark) (mark-index mark) #t)))

(define (mark-left-inserting-copy mark)
  (make-permanent-mark (mark-group mark) (mark-index mark) #t))

(define (make-permanent-mark group index left-inserting?)
  (let ((mark (make-temporary-mark group index left-inserting?)))
    (weak-list-set-add! mark (group-marks group))
    mark))

(define (mark-permanent! mark)
  (weak-list-set-add! mark (group-marks (mark-group mark)))
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

(define (clean-group-marks! group)
  (weak-list-set-clean! (group-marks group)))

(define (mark-temporary! mark)
  ;; I'd think twice about using this one.
  (weak-list-set-delete! mark (group-marks (mark-group mark))))

(define (find-permanent-mark group index left-inserting?)
  (weak-list-set-find (if left-inserting?
			  (lambda (mark)
			    (and (mark-left-inserting? mark)
				 (fix:= index (mark-index mark))))
			  (lambda (mark)
			    (and (not (mark-left-inserting? mark))
				 (fix:= index (mark-index mark)))))
		      (group-marks group)))

(define (for-each-mark group procedure)
  (weak-list-set-for-each procedure (group-marks group)))

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

(define (region? object)
  (and (pair? object)
       (mark? (car object))
       (mark? (cdr object))
       (mark<= (car object) (cdr object))))

(define-integrable (region-group region)
  (mark-group (region-start region)))

(define-integrable (region-start-index region)
  (mark-index (region-start region)))

(define-integrable (region-end-index region)
  (mark-index (region-end region)))