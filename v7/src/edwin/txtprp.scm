;;; -*-Scheme-*-
;;;
;;;	$Id: txtprp.scm,v 1.10 1993/09/09 21:41:25 cph Exp $
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

(define (add-text-property group start end key datum)
  (validate-region-arguments group start end 'ADD-TEXT-PROPERTY)
  (validate-symbol-argument key 'ADD-TEXT-PROPERTY)
  (step group start end
	(lambda (alist)
	  (let ((entry (assq key alist)))
	    (and entry
		 (eq? (cdr entry) datum))))
	(lambda (alist)
	  (let loop ((alist alist))
	    (cond ((null? alist)
		   (list (cons key datum)))
		  ((eq? key (caar alist))
		   (cons (cons key datum) (cdr alist)))
		  (else
		   (cons (car alist) (loop (cdr alist)))))))))

(define (add-text-properties group start end alist)
  (validate-region-arguments group start end 'ADD-TEXT-PROPERTIES)
  (validate-alist-argument alist 'ADD-TEXT-PROPERTIES)
  (step group start end
	(lambda (alist*)
	  (alist-subset? alist alist*))
	(lambda (alist*)
	  (append (alist-copy alist)
		  (list-transform-negative alist*
		    (lambda (association)
		      (assq (car association) alist)))))))

(define (remove-text-property group start end key)
  (validate-region-arguments group start end 'REMOVE-TEXT-PROPERTY)
  (validate-symbol-argument key 'REMOVE-TEXT-PROPERTY)
  (step group start end
	(lambda (alist)
	  (not (assq key alist)))
	(lambda (alist)
	  (let loop ((alist alist))
	    (cond ((null? alist) '())
		  ((eq? key (caar alist)) (cdr alist))
		  (else (cons (car alist) (loop (cdr alist)))))))))

(define (remove-text-properties group start end keys)
  (validate-region-arguments group start end 'REMOVE-TEXT-PROPERTIES)
  (if (not (and (list? keys)
		(for-all? keys symbol?)))
      (error:wrong-type-argument keys "list of symbols"
				 'REMOVE-TEXT-PROPERTIES))
  (step group start end
	(lambda (alist*)
	  (let loop ((keys keys))
	    (or (null? keys)
		(and (not (assq (car keys) alist*))
		     (loop (cdr keys))))))
	(lambda (alist*)
	  (list-transform-negative alist*
	    (lambda (association)
	      (memq (car association) keys))))))

(define (set-text-properties group start end alist)
  (validate-region-arguments group start end 'SET-TEXT-PROPERTIES)
  (validate-alist-argument alist 'SET-TEXT-PROPERTIES)
  (step group start end
	(lambda (alist*)
	  (alist-same-set? alist alist*))
	(lambda (alist*)
	  alist*
	  (alist-copy alist))))

(define (validate-region-arguments group start end procedure)
  (validate-group group procedure)
  (validate-group-index group start procedure)
  (validate-group-index group end procedure)
  (if (not (fix:<= start end))
      (error "Indexes incorrectly related:" start end procedure)))

(define (validate-point-arguments group index procedure)
  (validate-group group procedure)
  (validate-group-index group index procedure))

(define (validate-group group procedure)
  (if (not (group? group))
      (error:wrong-type-argument group "group" procedure)))

(define (validate-group-index group index procedure)
  (if (not (fix:fixnum? index))
      (error:wrong-type-argument index "fixnum" procedure))
  (if (not (and (fix:<= (group-start-index group) index)
		(fix:<= index (group-end-index group))))
      (error:bad-range-argument index procedure)))

(define (validate-alist-argument alist procedure)
  (if (not (alist? alist))
      (error:wrong-type-argument alist "alist" procedure))
  (if (not (let loop ((alist alist))
	     (or (null? alist)
		 (and (symbol? (caar alist))
		      (not (assq (caar alist) (cdr alist)))
		      (loop (cdr alist))))))
      (error:bad-range-argument alist procedure)))

(define (validate-symbol-argument key procedure)
  (if (not (symbol? key))
      (error:wrong-type-argument key "symbol" procedure)))

(define (alist-subset? x y)
  (let loop ((x x))
    (or (null? x)
	(let ((entry (assq (caar x) y)))
	  (and entry
	       (eq? (cdar x) (cdr entry))
	       (loop (cdr x)))))))

(define (alist-same-set? x y)
  ;; Slow but effective.
  (and (alist-subset? x y)
       (alist-subset? y x)))

(define (step group start end dont-modify? modify-alist)
  (define (loop i lst)
    ;; we now know that we are starting on the begining of an interval
    (let ((next (next-interval i))
	  (p (interval-properties i))
	  (start (interval-start i)))
      (let ((end* (if next (interval-start next) (interval-end i))))
	(if (fix:> end end*)
	    (loop next
		  (if (dont-modify? p)
		      lst
		      (begin
			(set-interval-properties! i (modify-alist p))
			(cons (list start end* p) lst))))
	    (let ((i
		   (if (fix:< end end*) (split-interval-left i end group) i)))
	      (if (dont-modify? p)
		  lst
		  (begin
		    (set-interval-properties! i (modify-alist p))
		    (cons (list start end p) lst))))))))
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((p
	   (let ((i
		  (if (group-text-properties group)
		      (find-interval group start)
		      (create-initial-interval group))))
	     (let ((start* (interval-start i)))
	       (if (fix:= start start*)
		   (loop i '())
		   (let ((dont-split? (dont-modify? (interval-properties i)))
			 (next (next-interval i)))
		     (if (and dont-split?
			      (or (not next)
				  (fix:<= end (interval-start next))))
			 '()
			 (loop (if dont-split?
				   next
				   (split-interval-right i start group))
			       '()))))))))
      (cond ((group-start-changes-index group)
	     =>
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
      (if (not (null? p))
	  (begin
	    (undo-record-property-changes! group p)
	    (set-group-modified?! group true)
	    (vector-set! group group-index:modified-tick
			 (fix:+ (group-modified-tick group) 1)))))
    (set-interrupt-enables! interrupt-mask)))

(define (get-text-properties group index)
  (validate-point-arguments group index 'GET-TEXT-PROPERTIES)
  (if (group-text-properties group)
      (alist-copy (interval-properties (find-interval group index)))
      '()))

(define (get-text-property group index key default)
  (validate-point-arguments group index 'GET-TEXT-PROPERTY)
  (validate-symbol-argument key 'GET-TEXT-PROPERTY)
  (if (group-text-properties group)
      (interval-property (find-interval group index) key default)
      default))

(define (local-comtabs mark)
  (get-text-property (mark-group mark) (mark-index mark) 'COMMAND-TABLE #f))

;;; The next four procedures are all about the same
;;; and none have been tested.

(define (next-property-change group index)
  (validate-point-arguments group index 'NEXT-PROPERTY-CHANGE)
  (and (group-text-properties group)
       (let ((z (find-interval group index)))
	 (let ((p1 (interval-properties z)))
	   (let loop ((next (next-interval z)))
	     (and next
		  (if (alist-same-set? p1 (interval-properties next))
		      (loop (next-interval next))
		      (interval-start next))))))))

(define (next-specific-property-change group index prop)
  (validate-point-arguments group index 'NEXT-SPECIFIC-PROPERTY-CHANGE)
  (and (group-text-properties group)
       (let ((z (find-interval group index)))
	 (let ((p (assq prop (interval-properties z))))
	   (let loop ((next (next-interval z)))
	     (and next
		  (if (eq? p (assq prop (interval-properties next)))
		      (loop (next-interval next))
		      (interval-start next))))))))

(define (previous-property-change group index)
  (validate-point-arguments group index 'PREVIOUS-PROPERTY-CHANGE)
  (and (group-text-properties group)
       (let ((z (find-interval group index)))
	 (let ((p1 (interval-properties z)))
	   (let loop ((prev (previous-interval z)))
	     (and prev
		  (if (alist-same-set? p1 (interval-properties prev))
		      (loop (previous-interval prev))
		      (interval-start prev))))))))

(define (prev-specific-property-change group index prop)
  (validate-point-arguments group index 'PREV-SPECIFIC-PROPERTY-CHANGE)
  (and (group-text-properties group)
       (let ((z (find-interval group index)))
	 (let ((p (assq prop (interval-properties z))))
	   (let loop ((prev (previous-interval z)))
	     (and prev
		  (if (eq? p (assq prop (interval-properties prev)))
		      (loop (previous-interval prev))
		      (interval-start prev))))))))

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

(define (text-not-insertable? group start)
  (and (not (let ((root (group-text-properties group)))
	      (or (not root)
		  (fix:= start 0)
		  (fix:= start (interval-total-length root)))))
       (not (eq? 'FULLY (group-writable? group)))
       (let ((interval (find-interval group start)))
	 (let ((datum (interval-property interval 'READ-ONLY #f)))
	   (and datum
		(if (fix:= start (interval-start interval))
		    (eq? datum
			 (interval-property (previous-interval interval)
					    'READ-ONLY #f))
		    (or (fix:< start (interval-end interval))
			(eq? datum
			     (interval-property (next-interval interval)
						'READ-ONLY #f)))))))))

(define (update-intervals-for-insertion! group start amount)
  (if (group-text-properties group)
      (begin
	(add-amount-up-tree (find-interval group start) amount)
	(set-text-properties group start (fix:+ start amount) '()))))

(define (text-not-deleteable? group start end)
  (and (group-text-properties group)
       (not (eq? 'FULLY (group-writable? group)))
       (let loop ((interval (find-interval group start)))
	 (or (interval-property interval 'READ-ONLY #f)
	     (let ((next (next-interval interval)))
	       (and next
		    (fix:> end (interval-start next))
		    (loop next)))))))

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

(define (group-reinsert-properties! group index end-index properties)
  index
  end-index
  (if properties
      (for-each (lambda (x)
		  (set-text-properties group
				       (vector-ref x 0)
				       (vector-ref x 1)
				       (vector-ref x 2)))
		properties)))

(define-structure (interval
		   (constructor make-interval
				(total-length start properties size)))
  (left false)
  (right false)
  (parent false)
  total-length
  start
  properties
  size)

(define-integrable (interval-property interval key default)
  (let ((entry (assq key (interval-properties interval))))
    (if entry
	(cdr entry)
	default)))

(define-integrable (null-right-child? t)
  (not (interval-right t)))

(define-integrable (null-left-child? t)
  (not (interval-left t)))

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

(define (create-initial-interval group)
  (let ((i (make-interval (group-length group) 0 '() 1)))
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