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

;;;; Combination Windows

(declare (usual-integrations)
	 (integrate-external "edb:window.bin.0"))
(using-syntax class-syntax-table

;;; Combination windows are used to split a window into vertically or
;;; horizontally divided areas.  That window's initial superior must
;;; support the :NEW-ROOT-WINDOW! operation, but is otherwise not
;;; constrained.

;;; (=> WINDOW :NEW-ROOT-WINDOW! WINDOW*)

;;; This is called whenever the root is changed.  It need not do
;;; anything at all, but it is useful to keep track of the root.

;;; What happens is that the initial window may be split horizontally
;;; or vertically, as many times as desired.  The combination windows
;;; organize those splits into a tree.  The leaves of the tree are not
;;; combination windows, but are created from one of the other leaves
;;; by the :MAKE-LEAF operation.  Of course, the initial window is a
;;; leaf window too.

;;; If there is just one leaf window in the tree, then it is the root
;;; window also.  Otherwise, the root is a combination window.

;;; The leaf windows must be subclasses of COMBINATION-LEAF-WINDOW,
;;; and they must support these operations:

;;; (=> WINDOW :MAKE-LEAF)

;;; Make a new leaf which can be placed next to WINDOW.  For example,
;;; if WINDOW is a buffer window, the new window should also be a
;;; buffer window, visiting the same buffer, and sharing the same
;;; superior.

;;; (=> WINDOW :MINIMUM-X-SIZE)
;;; (=> WINDOW :MINIMUM-Y-SIZE)

;;; These define how small the window is allowed to be.  Since the
;;; combination window operations change the sizes of leaf windows,
;;; they need some idea of how small the leaves are allowed to get.
;;; So, no window will ever be set to a size that is below its minimum
;;; -- it will be deleted from the heirarchy instead.

;;; The values of these operations may depend on the window's position
;;; in the heirarchy, i.e. the SUPERIOR, NEXT-WINDOW, and
;;; PREVIOUS-WINDOW.  These are carefully arranged in the target
;;; configuration before the operations are invoked. This is intended
;;; to allow the leaves to have different minimums when there are
;;; optional borders which depend on their placement.

;;; Under no circumstances should the :MINIMUM-SIZE depend on the
;;; current size of a leaf window.

(define window+)
(define window-)
(define window1+)
(define window-1+)
(define window0)
(define window-has-no-neighbors?)
(define window-has-horizontal-neighbor?)
(define window-has-vertical-neighbor?)
(define window-has-right-neighbor?)
(define window-has-left-neighbor?)
(define window-has-up-neighbor?)
(define window-has-down-neighbor?)
(define window-split-horizontally!)
(define window-split-vertically!)
(define window-delete!)
(define window-grow-horizontally!)
(define window-grow-vertically!)

(define-class combination-leaf-window vanilla-window
  (next-window previous-window))

(define combination-package
  (make-environment

(declare (integrate window-next set-window-next!
		    window-previous set-window-previous!))

(define-procedure combination-leaf-window (window-next window)
  (declare (integrate window))
  next-window)

(define-procedure combination-leaf-window (set-window-next! window window*)
  (declare (integrate window window*))
  (set! next-window window*))

(define-procedure combination-leaf-window (window-previous window)
  (declare (integrate window))
  previous-window)

(define-procedure combination-leaf-window (set-window-previous! window window*)
  (declare (integrate window window*))
  (set! previous-window window*))

(define (link-windows! previous next)
  (set-window-previous! next previous)
  (set-window-next! previous next))

(define-class combination-window combination-leaf-window
  (vertical? child))

(declare (integrate combination-vertical? set-combination-vertical!
		    combination-child combination? leaf? check-leaf-window))

(define-procedure combination-window (combination-vertical? window)
  (declare (integrate window))
  vertical?)

(define-procedure combination-window (set-combination-vertical! window v)
  (declare (integrate window v))
  (set! vertical? v))

(define-procedure combination-window (combination-child window)
  (declare (integrate window))
  child)

(define-procedure combination-window (set-combination-child! window window*)
  (set! child window*)
  (set-window-previous! window* #!FALSE))

(define (combination? window)
  (declare (integrate window))
  (object-of-class? combination-window window))

(define (leaf? window)
  (declare (integrate window))
  (and (object? window)
       (subclass? (object-class window) combination-leaf-window)
       (not (eq? (object-class window) combination-window))))

(define (check-leaf-window window name)
  (declare (integrate window name))
  (if (not (leaf? window))
      (error "Not a leaf window" name window)))

;;;; Leaf Ordering

(set! window+
(named-lambda (window+ leaf n)
  (check-leaf-window leaf 'WINDOW+)
  (cond ((positive? n) (%window+ leaf n))
	((negative? n) (%window- leaf (- n)))
	(else leaf))))

(set! window-
(named-lambda (window- leaf n)
  (check-leaf-window leaf 'WINDOW-)
  (cond ((positive? n) (%window- leaf n))
	((negative? n) (%window+ leaf (- n)))
	(else leaf))))

(define (%window+ leaf n)
  (if (= n 1)
      (%window1+ leaf)
      (%window+ (%window1+ leaf) (-1+ n))))

(define (%window- leaf n)
  (if (= n 1)
      (%window-1+ leaf)
      (%window- (%window-1+ leaf) (-1+ n))))

(set! window1+
(named-lambda (window1+ leaf)
  (check-leaf-window leaf 'WINDOW1+)
  (%window1+ leaf)))

(set! window-1+
(named-lambda (window-1+ leaf)
  (check-leaf-window leaf 'WINDOW-1+)
  (%window-1+ leaf)))

(set! window0
(named-lambda (window0 window)
  (if (not (and (object? window)
		(subclass? (object-class window) combination-leaf-window)))
      (error "WINDOW0: Window neither combination nor leaf" window))
  (window-leftmost-leaf (window-root window))))

(define (%window1+ leaf)
  (window-leftmost-leaf
   (or (window-next leaf)
       (if (combination? (window-superior leaf))
	   (find-window-with-next (window-superior leaf))
	   leaf))))

(define (%window-1+ leaf)
  (window-rightmost-leaf
   (or (window-previous leaf)
       (if (combination? (window-superior leaf))
	   (find-window-with-previous (window-superior leaf))
	   leaf))))

(define (find-window-with-next combination)
  (or (window-next combination)
      (if (combination? (window-superior combination))
	  (find-window-with-next (window-superior combination))
	  combination)))

(define (find-window-with-previous combination)
  (or (window-previous combination)
      (if (combination? (window-superior combination))
	  (find-window-with-previous (window-superior combination))
	  combination)))

(define (window-first window)
  (if (window-previous window)
      (window-first (window-previous window))
      window))

(define (window-last window)
  (if (window-next window)
      (window-last (window-next window))
      window))

(define (window-root window)
  (if (combination? (window-superior window))
      (window-root (window-superior window))
      window))

(define (window-leftmost-leaf window)
  (if (combination? window)
      (window-leftmost-leaf (combination-child window))
      window))

(define (window-rightmost-leaf window)
  (if (combination? window)
      (window-rightmost-leaf (window-last (combination-child window)))
      window))

(set! window-has-no-neighbors?
(named-lambda (window-has-no-neighbors? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-NO-NEIGHBORS?)
  (not (combination? (window-superior leaf)))))

(set! window-has-horizontal-neighbor?
(named-lambda (window-has-horizontal-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-HORIZONTAL-NEIGHBOR?)
  (%window-has-horizontal-neighbor? leaf)))

(define (%window-has-horizontal-neighbor? window)
  (and (combination? (window-superior window))
       (or (not (combination-vertical? (window-superior window)))
	   (%window-has-horizontal-neighbor? (window-superior window)))))

(set! window-has-vertical-neighbor?
(named-lambda (window-has-vertical-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-VERTICAL-NEIGHBOR?)
  (%window-has-vertical-neighbor? leaf)))

(define (%window-has-vertical-neighbor? window)
  (and (combination? (window-superior window))
       (or (combination-vertical? (window-superior window))
	   (%window-has-vertical-neighbor? (window-superior window)))))

(set! window-has-right-neighbor?
(named-lambda (window-has-right-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-RIGHT-NEIGHBOR?)
  (%window-has-right-neighbor? leaf)))

(define (%window-has-right-neighbor? window)
  (and (combination? (window-superior window))
       (or (and (not (combination-vertical? (window-superior window)))
		(window-next window))
	   (%window-has-right-neighbor? (window-superior window)))))

(set! window-has-left-neighbor?
(named-lambda (window-has-left-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-LEFT-NEIGHBOR?)
  (%window-has-left-neighbor? leaf)))

(define (%window-has-left-neighbor? window)
  (and (combination? (window-superior window))
       (or (and (not (combination-vertical? (window-superior window)))
		(window-previous window))
	   (%window-has-left-neighbor? (window-superior window)))))

(set! window-has-up-neighbor?
(named-lambda (window-has-up-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-UP-NEIGHBOR?)
  (%window-has-up-neighbor? leaf)))

(define (%window-has-up-neighbor? window)
  (and (combination? (window-superior window))
       (or (and (combination-vertical? (window-superior window))
		(window-next window))
	   (%window-has-up-neighbor? (window-superior window)))))

(set! window-has-down-neighbor?
(named-lambda (window-has-down-neighbor? leaf)
  (check-leaf-window leaf 'WINDOW-HAS-DOWN-NEIGHBOR?)
  (%window-has-down-neighbor? leaf)))

(define (%window-has-down-neighbor? window)
  (and (combination? (window-superior window))
       (or (and (combination-vertical? (window-superior window))
		(window-next window))
	   (%window-has-down-neighbor? (window-superior window)))))

;;;; Creation

(set! window-split-horizontally!
(named-lambda (window-split-horizontally! leaf #!optional n)
  (check-leaf-window leaf 'WINDOW-SPLIT-HORIZONTALLY!)
  (if (or (unassigned? n) (not n))
      (set! n (quotient (window-x-size leaf) 2)))
  (let ((x (window-x-size leaf))
	(y (window-y-size leaf)))
    (let ((n* (- x n)))
      (let ((new (allocate-leaf! leaf #!FALSE)))
	(let ((combination (window-superior leaf)))
	  (inferior-start (window-inferior combination leaf)
	    (lambda (x y)
	      (set-inferior-start! (window-inferior combination new)
				   (+ x n) y))))
	(if (or (< n (=> leaf :minimum-x-size))
		(< n* (=> new :minimum-x-size)))
	    (begin (deallocate-leaf! new)
		   #!FALSE)
	    (begin (=> leaf :set-x-size! n)
		   (=> new :set-size! n* y)
		   new)))))))

(set! window-split-vertically!
(named-lambda (window-split-vertically! leaf #!optional n)
  (check-leaf-window leaf 'WINDOW-SPLIT-VERTICALLY!)
  (if (or (unassigned? n) (not n))
      (set! n (quotient (window-y-size leaf) 2)))
  (let ((x (window-x-size leaf))
	(y (window-y-size leaf)))
    (let ((n* (- y n)))
      (let ((new (allocate-leaf! leaf #!TRUE)))
	(let ((combination (window-superior leaf)))
	  (inferior-start (window-inferior combination leaf)
	    (lambda (x y)
	      (set-inferior-start! (window-inferior combination new)
				   x (+ y n)))))
	(if (or (< n (=> leaf :minimum-y-size))
		(< n* (=> new :minimum-y-size)))
	    (begin (deallocate-leaf! new)
		   #!FALSE)
	    (begin (=> leaf :set-y-size! n)
		   (=> new :set-size! x n*)
		   new)))))))

(define (allocate-leaf! leaf v)
  (let ((superior (window-superior leaf)))
    (if (or (not (combination? superior))
	    (not (eq? v (combination-vertical? superior))))
	(let ((combination (=> superior :make-inferior combination-window)))
	  (=> superior :set-inferior-position! combination
	      (=> superior :inferior-position leaf))
	  (set-combination-vertical! combination v)
	  (window-replace! leaf combination)
	  (set-combination-child! combination leaf)
	  (set-window-next! leaf #!FALSE)
	  (=> superior :delete-inferior! leaf)
	  (add-inferior! combination leaf)
	  (set-inferior-start! (window-inferior combination leaf) 0 0)
	  (set-window-size! combination
			    (window-x-size leaf)
			    (window-y-size leaf)))))
  (let ((new (=> leaf :make-leaf)))
    (set-window-next! new (window-next leaf))
    (if (window-next leaf) (set-window-previous! (window-next leaf) new))
    (link-windows! leaf new)
    new))

(define (deallocate-leaf! leaf)
  (unlink-leaf! leaf)
  (maybe-delete-combination! (window-superior leaf)))

;;;; Deletion

(set! window-delete!
(named-lambda (window-delete! leaf)
  (check-leaf-window leaf 'WINDOW-DELETE!)
  (let ((superior (window-superior leaf)))
    (define (adjust-size! window)
      (if (combination-vertical? superior)
	  (=> window :set-y-size!
	      (+ (window-y-size window) (window-y-size leaf)))
	  (=> window :set-x-size!
	      (+ (window-x-size window) (window-x-size leaf)))))

    (if (not (combination? superior))
	(error "Attempt to delete top window"))
    (unlink-leaf! leaf)
    (let ((value
	   (cond ((window-next leaf)
		  (adjust-size! (window-next leaf))
		  (let ((inferior
			 (window-inferior superior (window-next leaf))))
		    (if (combination-vertical? superior)
			(set-inferior-y-start! inferior
					       (- (inferior-y-start inferior)
						  (window-y-size leaf)))
			(set-inferior-x-start! inferior
					       (- (inferior-x-start inferior)
						  (window-x-size leaf)))))
		  (window-next leaf))
		 ((window-previous leaf)
		  (adjust-size! (window-previous leaf))
		  (window-previous leaf))
		 (else
		  (error "Combination with single child -- WINDOW-DELETE!"
			 superior)))))
      (maybe-delete-combination! superior)
      value))))

(define (unlink-leaf! leaf)
  (let ((combination (window-superior leaf))
	(next (window-next leaf))
	(previous (window-previous leaf)))
    (delete-inferior! combination leaf)
    (=> leaf :kill!)
    (if previous
	(set-window-next! previous next)
	(set-combination-child! combination next))
    (if next
	(set-window-previous! next previous))))

(define (maybe-delete-combination! combination)
  (let ((child (combination-child combination)))
    (if (not (window-next child))
	(begin (delete-inferior! combination child)
	       (=> (window-superior combination) :replace-inferior!
		   combination child)
	       (window-replace! combination child)))))

(define-procedure combination-leaf-window (window-replace! old new)
  (cond ((not (combination? superior))
	 (=> superior :new-root-window! new))
	((and (combination? new)
	      (eq? (combination-vertical? superior)
		   (combination-vertical? new)))
	 (let ((first (combination-child new)))
	   (inferior-start (window-inferior superior new)
	     (lambda (xs ys)
	       (define (loop window)
		 (add-inferior! superior window)
		 (inferior-start (window-inferior new window)
		   (lambda (x y)
		     (set-inferior-start! (window-inferior superior window)
					  (+ xs x) (+ ys y))))
		 (if (window-next window)
		     (loop (window-next window))))
	       (delete-inferior! superior new)
	       (loop first)))
	   (if next-window
	       (link-windows! (window-last first) next-window))
	   (if previous-window
	       (link-windows! previous-window first)
	       (set-combination-child! superior first))))
	(else
	 (if next-window
	     (link-windows! new next-window))
	 (if previous-window
	     (link-windows! previous-window new)
	     (set-combination-child! superior new)))))

;;;; Sizing

(define (window-grow! leaf delta
		      v size min-size
		      set-c-size! set-w-size!
		      start set-start!)
  (check-leaf-window leaf 'WINDOW-GROW!)
  (let ((combination (window-superior leaf)))
    (define (loop)
      (if (not (combination? combination))
	  (error "No siblings of this window" leaf))
      (if (not (eq? v (combination-vertical? combination)))
	  (begin (set! leaf combination)
		 (set! combination (window-superior combination))
		 (loop))))
    (loop)
    (let ((new-size (+ (size leaf) delta))
	  (next (window-next leaf))
	  (previous (window-previous leaf)))
      (if (> new-size (size combination))
	  (begin (set! new-size (size combination))
		 (set! delta (- new-size (size leaf)))))
      (cond ((< new-size (min-size leaf))
	     (window-delete! leaf))
	    ((and next (>= (- (size next) delta) (min-size next)))
	     (let ((inferior (window-inferior combination next)))
	       (set-start! inferior (+ (start inferior) delta)))
	     (set-w-size! next (- (size next) delta))
	     (set-w-size! leaf new-size))
	    ((and previous
		  (>= (- (size previous) delta) (min-size previous)))
	     (let ((inferior (window-inferior combination leaf)))
	       (set-start! inferior (- (start inferior) delta)))
	     (set-w-size! previous (- (size previous) delta))
	     (set-w-size! leaf new-size))
	    (else
	     (scale-combination-inferiors! combination
					   (- (size combination) new-size)
					   leaf v size min-size
					   set-c-size! set-w-size!
					   set-start!)
	     ;; Scaling may have deleted all other inferiors.
	     ;; If so, leaf has replaced combination.
	     (set-w-size! leaf
			  (if (eq? combination (window-superior leaf))
			      new-size
			      (size combination))))))))

(set! window-grow-horizontally!
(named-lambda (window-grow-horizontally! leaf delta)
  (window-grow! leaf delta #!FALSE
		window-x-size window-min-x-size
		set-window-x-size! send-window-x-size!
		inferior-x-start set-inferior-x-start!)))

(set! window-grow-vertically!
(named-lambda (window-grow-vertically! leaf delta)
  (window-grow! leaf delta #!TRUE
		window-y-size window-min-y-size
		set-window-y-size! send-window-y-size!
		inferior-y-start set-inferior-y-start!)))

(define (scale-combination-inferiors-x! combination x except)
  (scale-combination-inferiors! combination x except #!FALSE
				window-x-size window-min-x-size
				set-window-x-size! send-window-x-size!
				set-inferior-x-start!))

(define (scale-combination-inferiors-y! combination y except)
  (scale-combination-inferiors! combination y except #!TRUE
				window-y-size window-min-y-size
				set-window-y-size! send-window-y-size!
				set-inferior-y-start!))

(define (window-min-x-size window)
  (=> window :minimum-x-size))

(define (send-window-x-size! window x)
  (=> window :set-x-size! x))

(define (window-min-y-size window)
  (=> window :minimum-y-size))

(define (send-window-y-size! window y)
  (=> window :set-y-size! y))

(define-method combination-window (:minimum-x-size combination)
  (=> (window-leftmost-leaf combination) :minimum-x-size))

(define-method combination-window (:minimum-y-size combination)
  (=> (window-leftmost-leaf combination) :minimum-y-size))

(define (set-combination-x-size! combination x)
  (scale-combination-inferiors-x! combination x #!FALSE)
  (set-window-x-size! combination x))

(define (set-combination-y-size! combination y)
  (scale-combination-inferiors-y! combination y #!FALSE)
  (set-window-y-size! combination y))

(define (set-combination-size! combination x y)
  (scale-combination-inferiors-x! combination x #!FALSE)
  (scale-combination-inferiors-y! combination y #!FALSE)
  (set-window-size! combination x y))

(define-method combination-window :set-x-size! set-combination-x-size!)
(define-method combination-window :set-y-size! set-combination-y-size!)
(define-method combination-window :set-size! set-combination-size!)

(define (scale-combination-inferiors! combination new-room except
				      v size min-size
				      set-c-size! set-w-size!
				      set-start!)
  ;; Change all of the inferiors of COMBINATION (except EXCEPT) to
  ;; use NEW-ROOM's worth of space.  EXCEPT, if given, should not be
  ;; changed in size, but should be moved if its neighbors change.
  ;; It is assumed that EXCEPT is given only for case where the
  ;; combination's VERTICAL? flag is the same as V.

  ;; General strategy:
  ;; If the window is growing, we can simply change the sizes of the
  ;; inferiors.  However, if it is shrinking, we must be more careful
  ;; because some or all of the inferiors can be deleted.  So in that
  ;; case, before any sizes are changed, we find those inferiors that
  ;; will be deleted and delete them.  If we delete all of the
  ;; inferiors, then we are done: this window has also been deleted.
  ;; Otherwise, we can then perform all of the changes, knowing that
  ;; no window will grow too small.

  (let ((c-size (size combination))
	(same? (eq? (combination-vertical? combination) v))
	(child (combination-child combination)))
    (let ((old-room (if (and same? except) (- c-size (size except)) c-size)))

      (define (diff-start)
	(diff-loop child))

      (define (diff-loop window)
	(set-w-size! window new-room)
	(if (window-next window)
	    (diff-loop (window-next window))))

      (define (diff-deletions)
	(for-each window-delete! (diff-collect child))
	(if (not (null? (window-inferiors combination))) (diff-start)))

      (define (diff-collect window)
	(let ((deletions
	       (if (window-next window)
		   (diff-collect (window-next window))
		   '())))
	  (if (< new-room (min-size window))
	      (cons window deletions)
	      deletions)))

      (define (same-start)
	(same-loop child 0 old-room new-room))

      (define (same-loop window start old-room new-room)
	(set-start! (window-inferior combination window) start)
	(cond ((eq? window except)
	       (if (window-next window)
		   (same-loop (window-next window) start old-room new-room)))
	      ((not (window-next window))
	       (set-w-size! window new-room))
	      (else
	       (let ((old-s (size window)))
		 (let ((new-s (truncate (* old-s (/ new-room old-room)))))
		   (set-w-size! window new-s)
		   (same-loop (window-next window)
			      (+ start new-s)
			      (- old-room old-s)
			      (- new-room new-s)))))))

      (define (same-deletions)
	(for-each window-delete! (same-collect child old-room new-room))
	(if (not (null? (window-inferiors combination))) (same-start)))

      (define (same-collect window old-room new-room)
	(cond ((eq? window except)
	       (if (window-next window)
		   (same-collect (window-next window) old-room new-room)
		   '()))
	      ((not (window-next window))
	       (if (< new-room (min-size window))
		   (list window)
		   '()))
	      (else
	       (let ((old-s (size window)))
		 (let ((new-s (truncate (* old-s (/ new-room old-room)))))
		   (let ((deletions (same-collect (window-next window)
						  (- old-room old-s)
						  (- new-room new-s))))
		     (if (< new-s (min-size window))
			 (cons window deletions)
			 deletions)))))))

      (cond ((< old-room new-room)
	     ((if same? same-start diff-start)))
	    ((> old-room new-room)
	     ((if same? same-deletions diff-deletions)))))))

;;; end COMBINATION-PACKAGE
)))