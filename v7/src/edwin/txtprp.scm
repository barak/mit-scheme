;;; -*-Scheme-*-
;;;
;;;	$Id: txtprp.scm,v 1.5 1993/08/13 23:20:31 cph Exp $
;;;
;;;	Copyright (c) 1993 Massachusetts Institute of Technology
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

;;;; Based on the text-properties in GNU Emacs

(declare (usual-integrations))

(define-structure (interval
		   (constructor make-interval
				(total-length start properties size)))
  (left false)
  (right false)
  (parent false)
  total-length
  start
  properties
  size
  )

;; export
(define (add-text-properties group start end plist)
  (record-property-changes!
   (step (if (group-text-properties group)
	     (find-interval group start)
	     (create-initial-interval group))
	 start
	 end
	 (lambda (i)
	   (add-properties plist i))
	 (lambda (i)
	   (not (add-properties? plist (interval-properties i))))
	 group)
   group))

;; export
(define (set-text-properties group start end plist)
  (record-property-changes!
   (step (if (group-text-properties group)
	     (find-interval group start)
	     (create-initial-interval group))
	 start
	 end
	 (lambda (i)
	   (set-properties plist i))
	 (lambda (i)
	   (not (set-properties? plist (interval-properties i))))
	 group)
   group))

;; export
(define (remove-text-properties group start end plist)
  (record-property-changes!
   (step (if (group-text-properties group)
	     (find-interval group start)
	     (create-initial-interval group))
	 start
	 end
	 (lambda (i)
	   (remove-properties plist i))
	 (lambda (i)
	   (not (remove-properties? plist (interval-properties i))))
	 group)
   group))

(define (record-property-changes! p group)
  ;; Return false if no changes were actually made.
  (if p
      (begin
	(undo-record-property-changes! group p)
	(set-group-modified?! group true)
	(vector-set! group group-index:modified-tick
		     (fix:+ (group-modified-tick group) 1))
	true)
      false))

;; export
(define (text-properties-at index group)
  (if (group-text-properties group)
      (interval-properties (find-interval group index))
      default-properties))

(define (get-property-at prop index group)
  (if (group-text-properties group)
      (get-property prop (interval-properties (find-interval group index)))
      #f))

(define (local-comtabs mark)
  (let ((property
	 (get-property-at 'COMMAND-TABLE (mark-index mark) (mark-group mark))))
    (and property
	 (cadr property))))

;;; The next four procedures are all about the same
;;; and none have been tested.

;; export
(define (next-property-change group index)
  (and (group-text-properties group)
       (let ((z (find-interval group index)))
	 (let ((p1 (interval-properties z)))
	   (let loop ((next (next-interval z)))
	     (and next
		  (if (interval-properties-equal? p1
						  (interval-properties next))
		      (loop (next-interval next))
		      (interval-start next))))))))

;; export
(define (next-specific-property-change group index prop)
  (and (group-text-properties group)
       (let ((z (find-interval group index)))
	 (let ((p (assq prop (interval-properties z))))
	   (let loop ((next (next-interval z)))
	     (and next
		  (if (eqv? p (assq prop (interval-properties next)))
		      (loop (next-interval next))
		      (interval-start next))))))))

;; export
(define (previous-property-change group index)
  (and (group-text-properties group)
       (let ((z (find-interval group index)))
	 (let ((p1 (interval-properties z)))
	   (let loop ((prev (previous-interval z)))
	     (and prev
		  (if (interval-properties-equal? p1
						  (interval-properties prev))
		      (loop (previous-interval prev))
		      (interval-start prev))))))))
;; export
(define (prev-specific-property-change group index prop)
  (and (group-text-properties group)
       (let ((z (find-interval group index)))
	 (let ((p (assq prop (interval-properties z))))
	   (let loop ((prev (previous-interval z)))
	     (and prev
		  (if (equal? p (assq prop (interval-properties prev)))
		      (loop (previous-interval prev))
		      (interval-start prev))))))))

(define (interval-properties-equal? p1 p2)
  ;; Slow but effective.
  (let ((subset?
	 (lambda (p1 p2)
	   (let loop ((p1 p1))
	     (or (null? p1)
		 (and (assq (caar p1) p2)
		      (loop (cdr p1))))))))
    (and (subset? p1 p2)
	 (subset? p2 p1))))

;;; The READ-ONLY property is applied to a contiguous region of
;;; characters.  No insertions are allowed within that region, and no
;;; deletions may intersect that region.  However, insertions may
;;; occur at either end of the region.

;;; This behavior is implemented by using a unique datum for the
;;; READ-ONLY property of a given contiguous region.  The code for
;;; insertion checks the READ-ONLY properties to the left and right of
;;; the insertion point, and disallows insertion only when they are
;;; the same.  If two different READ-ONLY regions are placed
;;; immediately adjacent to one another, insertions may occur in
;;; between the regions, but not inside of them.

;; export
(define (text-not-insertable? group start)
  (and (not (let ((root (group-text-properties group)))
	      (or (not root)
		  (fix:= start 0)
		  (fix:= start (interval-total-length root)))))
       (not (eq? 'FULLY (group-writable? group)))
       (let ((interval (find-interval group start)))
	 (let ((datum (interval-property interval 'READ-ONLY)))
	   (and datum
		(if (fix:= start (interval-start interval))
		    (eq? datum
			 (interval-property (previous-interval interval)
					    'READ-ONLY))
		    (or (fix:< start (interval-end interval))
			(eq? datum
			     (interval-property (next-interval interval)
						'READ-ONLY)))))))))

;; export
#|
(define (update-intervals-for-insertion! group start amount)
  (let ((root (group-text-properties group)))
    (cond ((not root)
	   unspecific)
	  ((fix:= start 0)
	   (left-insert (leftmost-interval root) amount group))
	  ((fix:= start (interval-total-length root))
	   (right-insert (rightmost-interval root) amount group))
	  (else
	   (let ((interval (find-interval group start)))
	     (cond ((fix:= start (interval-start interval))
		    (left-insert interval amount group))
		   ((fix:< start (interval-end interval))
		    (add-amount-up-tree interval amount))
		   (else
		    (right-insert interval amount group))))))))
|#
(define (update-intervals-for-insertion! group start amount)
  (if (group-text-properties group)
      (begin
	(add-amount-up-tree (find-interval group start) amount)
	(set-text-properties group start (fix:+ start amount) '()))))
  
;; export
(define (text-not-deleteable? group start end)
  (and (group-text-properties group)
       (not (eq? 'FULLY (group-writable? group)))
       (let loop ((interval (find-interval group start)))
	 (or (interval-property interval 'READ-ONLY)
	     (let ((next (next-interval interval)))
	       (and next
		    (fix:> end (interval-start next))
		    (loop next)))))))

;; export
#|
(define (update-intervals-for-deletion! group start end)
  (if (group-text-properties group)
      (let loop ((start start))
	(let ((interval (find-interval group start)))
	  (let ((start* (interval-start interval))
		(length (interval-length interval)))
	    (let ((end* (fix:+ start* length)))
	      (if (fix:<= end end*)
		  (if (and (fix:= start start*)
			   (fix:= end end*))
		      (delete-interval interval group)
		      (add-amount-up-tree interval
					  (fix:- 0 (fix:- end start))))
		  (begin
		    (if (fix:= start start*)
			(delete-interval interval group)
			(add-amount-up-tree interval
					    (fix:- 0 (fix:- end* start))))
		    (loop end*)))))))))
|#
(define (update-intervals-for-deletion! group start end)
  (if (group-text-properties group)
      (letrec ((loop
		;; we know that we are starting on an interval boundary
		(lambda (interval amount)
		  (let ((amount* (interval-length interval)))
		    (cond ((fix:= amount amount*)
			   (add-amount-up-tree interval (fix:- 0 amount))
			   (delete-interval interval group))
			  ((fix:> amount amount*)
			   (add-amount-up-tree interval (fix:- 0 amount*))
			   (let ((next (next-interval interval)))
			     (delete-interval interval group)
			     (loop next (fix:- amount amount*))))
			  (else
			   (add-amount-up-tree interval (fix:- 0 amount))))))))
	(let ((amount (fix:- end start)))
	  (let* ((interval (find-interval group start))
		 (start* (interval-start interval)))
	    (if (fix:= start* start)
		(loop interval amount)
		(let ((amount* (fix:- (interval-length interval)
				      (fix:- start start*))))
		  (if (fix:>= amount* amount)
		      (add-amount-up-tree interval (fix:- 0 amount))
		      (begin
			(add-amount-up-tree interval (fix:- 0 amount*))
			(loop (next-interval interval)
			      (fix:- amount amount*)))))))))))

;;; These procedures are called from the undo code to preserve the
;;; properties in text that is being deleted.

;; export
(define (group-extract-properties group start end)
  (and (group-text-properties group)
       (let loop ((interval (find-interval group start))
		  (start start))
	 (let ((ie (interval-end interval)))
	   (if (fix:<= end ie)
	       (cons (vector start end (interval-properties interval))
		     '())
	       (cons (vector start
			     ie
			     (interval-properties interval))
		     (let ((next (next-interval interval)))
		       (loop next (interval-start next)))))))))

;; export
(define (group-reinsert-properties! group index end-index properties)
  index
  end-index
  (if properties
      (for-each (lambda (x) (set-text-properties
			     group
			     (vector-ref x 0)
			     (vector-ref x 1)
			     (vector-ref x 2)))
		properties)))

;;; this also needs to test weather or not the left split is necessary.
;;; maybe rather than a seperate test we could grab the plist before,
;;; let the proc do its magic, and then take action afterword.
(define (step i start end proc test group)

  (define (loop i lst)
    ;; we now know that we are starting on the begining of an interval
    (let ((next (next-interval i))
	  (p (interval-properties i))
	  (start (interval-start i)))
      (let ((end* (if next
		      (interval-start next)
		      (fix:+ (interval-start i)
			     (interval-length i)))))
	(cond ((fix:= end end*)
	       (if (proc i)
		   (cons (list start end p) lst)
		   lst))
	      ((fix:< end end*)
	       (if (proc (split-interval-left i end group))
		   (cons (list start end lst) lst)
		   lst))
	      (else
	       (loop next
		     (if (proc i)
			 (cons (list start end* p) lst)
			 lst)))))))
  (let ((start* (interval-start i)))
    (cond ((group-start-changes-index group)
	   (lambda (gsc)
	     (set-group-start-changes-index!
	      group
	      (if (fix:< start gsc) start gsc))
	     (set-group-end-changes-index!
	      group
	      (let ((gec (group-end-changes-index group)))
		(if (fix:> end gec) end gec)))))
	  (else
	   (set-group-start-changes-index! group start)
	   (set-group-end-changes-index! group end)))
    (if (fix:= start start*)
	(loop i '())
	(let ((no-split? (test i))
	      (next (next-interval i)))
	  (cond ((and no-split?
		      (or (not next) (<= end (interval-start next))))
		 '())
		(no-split?
		 (loop next
		       '()))
		(else
		 (loop (split-interval-right i start group)
		       '())))))))

;;;; Property Lists
;;;; these are complicated becase of the desire to recognize
;;;; unnecessary changes
(define-integrable default-properties '())

;; export
(define (get-property prop plist)
  (assq prop plist))

(define (remove-property prop plist)
  (del-assq prop plist))

(define (add-properties? plist plist2)
  (there-exists? plist
    (lambda (p1)
      (let ((p2 (get-property (car p1) plist2)))
	(not (and p2 (eq? (cdr p1) (cdr p2))))))))

(define (add-properties plist interval)
  (let ((plist2 (interval-properties interval)))
    (cond ((add-properties? plist plist2)
	    (set-interval-properties!
	     interval
	     (append plist
		     (append-map
		      (lambda (p2)
			(if (get-property (car p2) plist)
			    '()
			    (list p2)))
		      plist2)))
	    true)
	  (else false))))

(define (remove-properties? plist plist2)
  (there-exists? plist
    (lambda (p1)
      (get-property (car p1) plist2))))

(define (remove-properties plist interval)
  (let ((plist2 (interval-properties interval)))
    (cond ((remove-properties? plist plist2)
	   (set-interval-properties!
	    interval
	    (append-map
	     (lambda (p2)
	       (if (get-property (car p2) plist)
		   '()
		   (list p2)))
	     plist2))
	   true)
	  (else false))))

(define (set-properties? plist plist2)
  (cond ((not (= (length plist)
		 (length plist2)))
	 true)
	(else
	 (there-exists? plist
	   (lambda (p)
	     (let ((p2 (get-property (car p) plist2)))
	       (if (and p2 (or
			    (eq? (cdr p2) (cdr p))
			    (eq? (cadr p2) (cadr p))))
		   false
		   true)))))))

(define (set-properties plist interval)
  (let ((plist2 (interval-properties interval)))
    (cond ((set-properties? plist plist2)
	   (set-interval-properties!
	    interval
	    plist)
	   true)
	  (else false))))

(define-integrable (interval-property interval key)
  (get-property key (interval-properties interval)))

(define-integrable (null-right-child? t)
  (not (interval-right t)))

(define-integrable (null-left-child? t)
  (not (interval-left t)))

(define (null-parent? t)
  (not (interval-parent t)))

(define-integrable (total-length i)
  (if (not i)
      0
      (interval-total-length i)))

(define-integrable (left-total-length t)
  (if (interval-left t)
      (interval-total-length (interval-left t))
      0))

(define-integrable (right-total-length t)
  (if (interval-right t)
      (interval-total-length (interval-right t))
      0))

(define-integrable (interval-length i)
  (if (not i)
      0
      (fix:- (interval-total-length i)
	     (fix:+ (right-total-length i)
		    (left-total-length i)))))

(define-integrable (interval-end i)
  (fix:+ (interval-start i)
	 (interval-length i)))

(define-integrable (connect-left! parent child)
  (set-interval-left! parent child)
  (if child
      (set-interval-parent! child parent)))

(define-integrable (connect-right! parent child)
  (set-interval-right! parent child)
  (if child
      (set-interval-parent! child parent)))

(define-integrable (interval-add-amount! i amt)
  (set-interval-total-length!
   i
   (fix:+ (interval-total-length i) amt))
  amt)

(define (create-initial-interval group)
  (let ((i (make-interval (group-length group) 0 default-properties 1)))
    (set-group-text-properties! group i)
    i))

(define-integrable (size i)
  (if i (interval-size i) 0))

(define (add-amount-up-tree interval amt)
  (let loop ((interval interval))
    (if (not interval)
	true				; return true on purpose
	(begin
	  (set-interval-total-length!
	   interval
	   (fix:+ (interval-total-length interval) amt))
	  (loop (interval-parent interval))))))

(define (find-interval group index)
  ;; Find the interval in GROUP that contains INDEX.  Assumes that
  ;; GROUP has non-empty GROUP-TEXT-PROPERTIES and that INDEX is
  ;; strictly less than GROUP-LENGTH.  The interval returned has a
  ;; valid INTERVAL-START, and INDEX is guaranteed to be between
  ;; INTERVAL-START (inclusive) and INTERVAL-END (exclusive).
  (let loop ((relative-index index) (interval (group-text-properties group)))
    ;;(let ((left (interval-left interval))))
    (if (and (interval-left interval)
	     (fix:< relative-index (interval-total-length
				    (interval-left interval))))
	(loop relative-index (interval-left interval))
	;;(let ((right (interval-right interval))))
	(if (and (interval-right interval)
		 (fix:>= relative-index
			 (fix:- (interval-total-length interval)
				(interval-total-length
				 (interval-right interval)))))
	    (loop (fix:- relative-index
			 (fix:- (interval-total-length interval)
				(interval-total-length
				 (interval-right interval))))
		  (interval-right interval))
	    (begin
	      (set-interval-start! interval
				   (fix:+ (fix:- index relative-index)
					  (if (interval-left interval)
					      (interval-total-length
					       (interval-left interval))
					      0)))
	      interval)))))

(define (next-interval interval)
  (let ((right (interval-right interval))
	(finish
	 (lambda (interval*)
	   (set-interval-start! interval*
				;; changed from fix:- to fix:+
				(fix:+ (interval-start interval)
				       (interval-length interval)))
	   interval*)))
    (if right
	(finish (leftmost-interval right))
	(let loop ((interval interval))
	  (let ((parent (interval-parent interval)))
	    (and parent
		 (if (eq? interval (interval-left parent))
		     (finish parent)
		     (loop parent))))))))

(define (previous-interval interval)
  (let ((left (interval-left interval))
	(finish
	 (lambda (interval*)
	   (set-interval-start! interval*
				(fix:- (interval-start interval)
				       (interval-length interval*)))
	   interval*)))
    (if left
	(finish (rightmost-interval left))
	(let loop ((interval interval))
	  (let ((parent (interval-parent interval)))
	    (and parent
		 (if (eq? interval (interval-right parent))
		     (finish parent)
		     (loop parent))))))))

(define (leftmost-interval t)
  (let ((l (interval-left t)))
    (if l
	(leftmost-interval l)
	t)))

(define (rightmost-interval t)
  (let ((r (interval-right t)))
    (if r
	(rightmost-interval r)
	t)))

#|
(define (left-insert interval amt group)
  (let ((i (make-interval amt false default-properties)))
    (if (null-left-child? interval)
	(connect-left! interval i)
	(begin
	  (connect-left! i (interval-left interval))
	  (connect-left! interval i)
	  (set-interval-total-length!
	   i (fix:+ (left-total-length i) amt))))
    (add-amount-up-tree interval amt)
    (balance i group)))

(define (right-insert interval amt group)
  (let ((i (make-interval amt false default-properties)))
    (if (null-right-child? interval)
	(connect-right! interval i)
	(begin
	  (connect-right! i (interval-right interval))
	  (connect-right! interval i)
	  (set-interval-total-length!
	   i (fix:+ (right-total-length i) amt))))
    (add-amount-up-tree interval amt)
    (balance i group)))
|#

;;;
;;;     interval                  interval
;;;      /    \       --->         /    \
;;;   left   right                i    right
;;;                              /
;;;                            left
(define (split-interval-right interval start* group)
  (let ((start (interval-start interval))
	(left (interval-left interval)))
    (let ((i (make-interval (fix:+ (if left (interval-total-length left) 0)
				   (fix:- start* start))
			    start
			    (interval-properties interval)
			    (size left))))
      (if left (connect-left! i left))
      (connect-left! interval i)
      (set-interval-start! interval start*)
      (balance i group 1)
      interval)))

;;; Do the same operation as above but return a different node.
(define (split-interval-left interval end* group)
  (let ((start (interval-start interval))
	(left (interval-left interval)))
    (let ((i (make-interval (fix:+ (if left (interval-total-length left) 0)
				   (fix:- end* start))
			    start
			    (interval-properties interval)
			    (size left))))
      (if left (connect-left! i left))
      (connect-left! interval i)
      (set-interval-start! interval end*)
      (balance i group 1)
      i)))

(define (delete-interval i group)
  (define (delete-node i)
    (cond ((null-left-child? i)
	   (interval-right i))
	  ((null-right-child? i)
	   (interval-left i))
	  (else
	   ;;; this creates a balancing problem
	   ;;; we should do some balancing along the way
	   (let* ((l (interval-left i))
		  (r (interval-right i))
		  (amt (interval-total-length l))
		  (s (interval-size l)))
	     (let loop ((this r))
	       (set-interval-total-length!
		this (fix:+ (interval-total-length this) amt))
	       (set-interval-size!
		this (fix:+ (interval-size this) s))
	       (if (interval-left this)
		   (loop (interval-left this))
		   (begin
		     (connect-left! this l)
		     r)))))))
  (let ((new (delete-node i)))
    (cond ((am-left-child? i)
	   (connect-left! (interval-parent i) new))
	  ((am-right-child? i)
	   (connect-right! (interval-parent i) new))
	  (else
	   (if new (set-interval-parent! new false))
	   (set-group-text-properties! group new)))
    (balance (interval-parent i) group -1)))

(define (am-left-child? i)
  (let ((p (interval-parent i)))
    (and p
	 (eq? (interval-left p) i))))

(define (am-right-child? i)
  (let ((p (interval-parent i)))
    (and p
	 (eq? (interval-right p) i))))

;;; Balance by the number of interval nodes.  There does not appear to be
;;; a good way to balance based on total-length because it does not tell
;;; us anything about the sub-intervals.  The balancing works by walking
;;; up the tree from the point of change rotating as necessary.
(define (balance t group size-inc-amount)

  (define-integrable (smart-connect! parent child other)
    (if parent
	(if (eq? other (interval-left parent))
	    (connect-left! parent child)
	    (connect-right! parent child))
	(begin
	  (set-interval-parent! child false)
	  (set-group-text-properties! group child))))

  ;;     a             b
  ;;    / \           / \
  ;;   X   b   -->   a   Z
  ;;      / \       / \
  ;;     Y   Z     X   Y
  (define-integrable (single-left a)
    (let ((b (interval-right a))
	  (p (interval-parent a))
	  (lx (left-total-length a))
	  (la (interval-length a)))
      (let ((y (interval-left b))
	    (lb (interval-length b))
	    (ly (left-total-length b))
	    (lz (right-total-length b))
	    (nx (size (interval-left a)))
	    (ny (size (interval-left b)))
	    (nz (size (interval-right b))))
	(smart-connect! p b a)
	(connect-left! b a)
	(connect-right! a y)
	
	(set-interval-total-length! a (fix:+ (fix:+ lx la) ly))
	(set-interval-total-length! b (fix:+ (fix:+ (fix:+ lx la) ly)
					     (fix:+ lz lb)))
	(set-interval-size! a (fix:+ (fix:+ nx ny) 1))
	(set-interval-size! b (fix:+ (fix:+ (fix:+ nx ny) nz) 2))
	(balance p group size-inc-amount))))

  ;;     a                 b
  ;;    / \               / \
  ;;   X   c             /   \ 
  ;;      / \   -->     a     c
  ;;     b   Z         / \   / \ 
  ;;    / \           X  Y1 Y2  Z
  ;;   Y1  Y2
  (define-integrable (double-left a)
    (let* ((c (interval-right a))
	   (b (interval-left c))
	   (y1 (interval-left b))
	   (y2 (interval-right b))
	   (p (interval-parent a))

	   (la (interval-length a))
	   (lb (interval-length b))
	   (lc (interval-length c))
	   (lx (left-total-length a))
	   (ly1 (left-total-length b))
	   (ly2 (right-total-length b)) 
	   (lz (right-total-length c))
	   (nx (size (interval-left a)))
	   (ny1 (size (interval-left b)))
	   (ny2 (size (interval-right b)))
	   (nz (size (interval-right c))))
      (smart-connect! p b a)
      (connect-left! b a)
      (connect-right! b c)
      (connect-right! a y1)
      (connect-left! c y2)
      
      (set-interval-total-length! a (fix:+ (fix:+ lx ly1) la))
      (set-interval-total-length! b
				  (fix:+ (fix:+ (fix:+ lx ly1) (fix:+ ly2 lz))
					 (fix:+ (fix:+ la lc) lb)))
      (set-interval-total-length! c (fix:+ (fix:+ ly2 lz) lc))
      (set-interval-size! a (fix:+ (fix:+ nx ny1) 1))
      (set-interval-size! c (fix:+ (fix:+ ny2 nz) 1))
      (set-interval-size! b (fix:+ (fix:+ (fix:+ nx ny1) (fix:+ ny2 nz)) 3))
      (balance p group size-inc-amount)))

  ;;      a            b
  ;;     / \          / \
  ;;    b   X  -->   Z   a
  ;;   / \              / \
  ;;  Z   Y            Y   X
  (define-integrable (single-right a)
    (let ((b (interval-left a))
	  (p (interval-parent a))
	  (lx (right-total-length a))
	  (la (interval-length a)))
      (let ((y (interval-right b))
	    (lb (interval-length b))
	    (ly (right-total-length b))
	    (lz (left-total-length b))
	    (nz (size (interval-left b)))
	    (ny (size (interval-right b)))
	    (nx (size (interval-right a))))
	(smart-connect! p b a)
	(connect-right! b a)
	(connect-left! a y)

	(set-interval-total-length! a (fix:+ (fix:+ lx la) ly))
	(set-interval-total-length! b (fix:+ (fix:+ (fix:+ lx la) ly)
					     (fix:+ lb lz)))
	(set-interval-size! a (fix:+ (fix:+ ny nx) 1))
	(set-interval-size! b (fix:+ (fix:+ (fix:+ ny nx) nz) 2))
	(balance p group size-inc-amount))))

  ;;      a              b
  ;;     / \            / \
  ;;    c   X          /   \
  ;;   / \     -->    c     a
  ;;  Z   b          / \   / \
  ;;     / \        Z  Y2 Y1  X
  ;;    Y2 Y1
  (define-integrable (double-right a)
    (let* ((c (interval-left a))
	   (b (interval-right c))
	   (y2 (interval-left b))
	   (y1 (interval-right b))
	   (p (interval-parent a))

	   (nx (size (interval-right a)))
	   (nz (size (interval-left c)))
	   (ny1 (size (interval-right b)))
	   (ny2 (size (interval-left b)))

	   (la (interval-length a))
	   (lb (interval-length b))
	   (lc (interval-length c))
	   (lx (right-total-length a))
	   (ly1 (right-total-length b))
	   (ly2 (left-total-length b))
	   (lz (left-total-length c))
	   )
      (smart-connect! p b a)
      (connect-right! b a)
      (connect-left! b c)
      (connect-left! a y1)
      (connect-right! c y2)

      (set-interval-total-length! a (fix:+ (fix:+ lx ly1) la))
      (set-interval-total-length! b
				  (fix:+ (fix:+ (fix:+ lx ly1) (fix:+ ly2 lz))
					 (fix:+ (fix:+ la lb) lc)))
      (set-interval-total-length! c (fix:+ (fix:+ ly2 lz) lc))

      (set-interval-size! a (fix:+ (fix:+ ny1 nx) 1))
      (set-interval-size! c (fix:+ (fix:+ nz ny2) 1))
      (set-interval-size! b (fix:+ (fix:+ (fix:+ ny1 nx) (fix:+ nz ny2))
				   3))
      (balance p group size-inc-amount)))

  (if (not t)
      true
      (let ((ln (size (interval-left t)))
	    (rn (size (interval-right t))))
	(cond ((fix:< (fix:+ ln rn) 2)
	       (set-interval-size! t (fix:+ (interval-size t)
					    size-inc-amount))
	       (balance (interval-parent t) group size-inc-amount))
	      ((fix:> rn (fix:* 5 ln))	; right is too big
	       (let ((rln (size (interval-left (interval-right t))))
		     (rrn (size (interval-right (interval-right t)))))
		 (if (fix:< rln rrn)
		     (single-left t)
		     (double-left t))))
	      ((fix:> ln (fix:* 5 rn))	; left is too big
	       (let ((lln (size (interval-left (interval-left t))))
		     (lrn (size (interval-right (interval-left t)))))
		 (if (fix:< lrn lln)
		     (single-right t)
		     (double-right t))))
	      (else
	       (set-interval-size! t (fix:+ (interval-size t)
					    size-inc-amount))
	       (balance (interval-parent t) group size-inc-amount))))))