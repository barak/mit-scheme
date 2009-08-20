;;;;; -*- Scheme -*-
;;;;;
;;;;; $Id$
;;;;; derived from geometry.sc,v 1.1 1993/02/16 14:04:09 jmiller Exp $

;; The box makers - one for horizontal, one for vertical

(define (make-hbox . kids)
  (make-box h-size h-arrange h-get-hglue h-get-vglue kids))

(define (make-vbox  . kids)
  (make-box v-size v-arrange v-get-hglue v-get-vglue kids))

;; user-level accessor
(define (box-children box)
  (cond ((box%? box)
	 (box%.kids box))
	((arraybox%? box)
	 (arraybox%.kids-lists box))
	(else (error "not a box -- BOX-CHILDREN" box))))

;; Vertical sizer

(define (v-size kids)
  (make-size
   (apply max (cons 0
		    (map (lambda (kid)
			   (Size.Width (get-desired-size kid)))
			 kids)))
   (apply + (map (lambda (kid)
		   (Size.Height (get-desired-size kid)))
		 kids))))

;; Horizontal sizer

(define (h-size kids)
  (make-size
   (apply + (map (lambda (kid)
		     (Size.Width (get-desired-size kid)))
		 kids))
   (apply max (cons 0
		    (map (lambda (kid)
			   (Size.Height (get-desired-size kid)))
			 kids)))))

;; Vertical arranger

(define (v-arrange kids my-screen-area)
  (let* ((my-height (UITKRectangle.Height my-screen-area))
	 (full-width (UITKRectangle.Width my-screen-area))
	 (my-offset (UITKRectangle.Offset my-screen-area))
	 (Y (point.Y my-offset))
	 (vglues (map %vglue kids)))
    (conquer-space
     my-height
     vglues
     (lambda (positions-vector)
       (let loop ((n 0) (rest kids))
	 (if (null? rest)
	     #F
	     (let* ((kid (car rest))
		    (kid-y-offset (vector-ref positions-vector n))
		    (height (- (vector-ref positions-vector (+ n 1)) 
			       kid-y-offset))
		    (desired-size (get-desired-size kid))
		    (desired-width (Size.Width Desired-Size))
		    (hglue (%hglue kid))
		    (width (cond ((or (fil-glue? hglue)
				      (fill-glue? hglue))
				  full-width)
				 ((rigid-glue? hglue) desired-width)
				 ;;((percent-glue? hglue)
				 ;;(max desired-width
				 ;;(inexact->exact
				 ;;(ceiling (* .01 (glue.value hglue) height)))))
				 (else (error "Unknown glue class"
					      (glue.class hglue)))))
		    (X (+ (Point.X my-offset)
			  (ceiling
			   (/ (- (UITKRectangle.Width my-screen-area) width)
			      2)))))
	       (assign-screen-area!
		kid
		(make-UITKRectangle (make-point X (+ Y kid-y-offset))
				    (make-size width height)))
	       (loop (+ n 1) (cdr rest)))))))))


;; Horizontal arranger

(define (h-arrange kids my-screen-area)
  (let* ((my-width (UITKRectangle.Width my-screen-area))
	 (full-height (UITKRectangle.Height my-screen-area))
	 (my-offset (UITKRectangle.Offset my-screen-area))
	 (X (point.X my-offset))
	 (hglues (map %hglue kids)))
    (conquer-space
     my-width
     hglues
     (lambda (positions-vector)
       (let loop ((n 0) (rest kids))
	 (if (null? rest)
	     #F
	     (let* ((kid (car rest))
		    (kid-x-offset (vector-ref positions-vector n))
		    (width (- (vector-ref positions-vector (+ n 1)) 
			      kid-x-offset))
		    (desired-size (get-desired-size kid))
		    (desired-height (Size.Height Desired-Size))
		    (vglue (%vglue kid))
		    (height (cond ((or (fil-glue? vglue)
				       (fill-glue? vglue))
				   full-height)
				  ((rigid-glue? vglue) desired-height)
				  ;;((percent-glue? vglue)
				  ;;(max desired-height 
				  ;;(inexact->exact
				  ;;(ceiling (* .01 (glue.value vglue) width)))))
				  (else (error "Unknown glue class"
					       (glue.class vglue)))))
		    (Y (+ (Point.Y my-offset)
			  (ceiling
			   (/ (- (UITKRectangle.Height my-screen-area) height)
			      2)))))
	       (assign-screen-area!
		kid
		(make-UITKRectangle (make-point (+ X kid-x-offset) Y)
				    (make-size width height)))
	       (loop (+ n 1) (cdr rest)))))))))


;;; Calculate hglue and vglue for hboxes...

(define (h-get-hglue kids)
  (series-compose-glues (map %hglue kids)))

(define (h-get-vglue kids)
  (parallel-compose-glues (map %vglue kids)))

;;; ... and vboxes.

(define (v-get-hglue kids)
  (parallel-compose-glues (map %hglue kids)))

(define (v-get-vglue kids)
  (series-compose-glues (map %vglue kids)))


;; Generic arranger

(define (retract-area objects)
  (for-each (lambda (obj) (assign-screen-area! obj #F))
	    objects))

(define (box-add-child! me kid)
  (if (not (valid-child? kid))
      (error "BOX-ADD-CHILD!: Bad UIObj" kid))
  (one-parent-only! kid me)
  (set-Box%.kids! me (append (Box%.kids me) (list kid)))
  (on-geometry-change!
   kid 'BOX
   (lambda (old-screen-area new-screen-area)
     old-screen-area			; Not used
     (if (eq? new-screen-area #T)	; Instigated by child, not manager
	 (box:rearrange me))))
  (on-death! kid 'BOX (lambda () (box-remove-child! me kid)))
  (assign-drawing-surface! kid (drawing-surface me))
  (box:rearrange me))

(define (box-remove-child! me kid)
  (if (not (valid-child? kid))
      (error "BOX-REMOVE-CHILD!: Bad UIObj" kid))
  (set-Box%.kids! me (delq! kid (Box%.kids me)))
  (forget! kid 'BOX)
  (assign-drawing-surface! kid 'RETRACTED)
  (box:rearrange me))

(define (box-assign-drawing-surface! me surface)
  (check-drawing-surface! me surface)
  (for-each (lambda (kid)
	      (if (eq? surface 'RETRACTED)
		  (forget! kid 'BOX))
	      (assign-drawing-surface! kid surface))
	    (Box%.kids me))
  (if (DrawingSurface? surface)
      (set-%desired-size! me ((Box%.sizer me) (Box%.kids me))))
  (if (eq? Surface 'RETRACTED)
      (death! me)
      (geometry-change! me #F #F))
  'OK)

(define (box-assign-screen-area! me screen-area)
  (cond ((vector? screen-area)
	 (set-assigned-screen-area! me screen-area)
	 (let ((old (used-screen-area me)))
	   (if (not (screen-area= old screen-area))
	       (begin
		 (set-used-screen-area! me screen-area)
		 (box:rearrange me)
		 (geometry-change! me old screen-area))))
	 screen-area)
	((not screen-area)
	 (set-assigned-screen-area! me screen-area)
	 (let ((old (used-screen-area me)))
	   (if (not (screen-area= old screen-area))
	       (begin
		 (set-used-screen-area! me screen-area)
		 (retract-area (Box%.kids me))
		 (geometry-change! me old screen-area))))
	 screen-area)
	(else
	 (error "BOX-ASSIGN-SCREEN-AREA!: Bad screen-area" screen-area))))

(define (box-assign-glue! me)
  (let ((kids (Box%.kids me)))
    (for-each assign-glue! kids)
    (set-%hglue! me ((Box%.get-hglue me) kids)) 
    (set-%vglue! me ((Box%.get-vglue me) kids))))

;; Box Maker
(define (box-maker size-proc screen-area-proc get-hglue get-vglue)
  (make-Box%
   (make-UIObjInternals box-add-child!
			'invalid
			UIObj-set-context!
			box-assign-screen-area!
			box-assign-drawing-surface!
			UIObj-point-within?
			UIObj-rectangle-overlaps?
			UIObj-handle-event
			UIObj-get-desired-size
			UIObj-assigned-screen-area
			UIObj-used-screen-area
			UIObj-set-assigned-screen-area!
			UIObj-set-used-screen-area!
			box-assign-glue!)
   size-proc
   screen-area-proc
   get-hglue
   get-vglue))

(define (box:rearrange me)
  (let ((screen-area (used-screen-area me))
	(arrange (Box%.arranger me))
	(size (Box%.sizer me))
	(kids (Box%.kids me)))
    (if screen-area
	(let ((new-size (size kids)))
	  (set-%desired-size! me new-size)
	  (if (size= new-size (UITKRectangle.Size screen-area))
	      (begin (assign-glue! me)
		     (arrange kids screen-area))
	      (begin
		(set-%desired-size! me new-size)
		(geometry-change! me screen-area #T)
		(if (eq? screen-area (used-screen-area me))
		    (begin (assign-glue! me)
			   (arrange kids screen-area)))))))))
		    
(define (box:event-propagator box)
  (lambda (event)
    (for-each (lambda (kid)
		(if (event-within? kid event)
		    (handle-event kid event)))
	      (Box%.kids box))))
  

(define (make-box size-proc screen-area-proc get-hglue get-vglue children)
  (let ((me (box-maker size-proc screen-area-proc get-hglue get-vglue)))
    (on-event! me 'BOX
	       (box:event-propagator me))
    (for-each (lambda (kid) (add-child! me kid)) children)
    me))

;;; Glue Mechanism snarfed from Halstead

;;; Glue abstraction, captures a minimum size (horizontal or vertical,
;;; depending on usage) below which the object really ought not to shrink.
;;; Also specifies a stretchability value (glue-value) and a stretchability
;;; class (glue-class).  Space is divided between two series-composed
;;; glues as follows:
;;;
;;; 1. If the total is less than the sum of the glues' minimum sizes
;;;    then divide the space in proportion to the minimum sizes (everybody
;;;    has to give up the same percentage of their minimum size).
;;;
;;; 2. Else, if both glues have the same glue-class, then divide the excess
;;;    of available space (over the sum of their minimum sizes)
;;;    in proportion to their glue-values.
;;;
;;; 3. If the glue-classes differ, then the glue with the smaller glue-class
;;;    gets its minimum size, and the glue with the larger glue-class gets
;;;    all the rest (thus glue of a given glue-class is "infinitely" more
;;;    stretchable than any glue from a lower glue-class -- this is useful
;;;    for filling out to a boundary without stretching the item before the
;;;    fill).

;;; Conventional glue classes:

(define *rigid-glue-class* -1)	; for things that really don't want to stretch
(define *percent-glue-class* 0)	; for proportionally allocating space
(define *fill-glue-class* 1)	; for things intended to be infinitely stretchable
(define *fil-glue-class* 2)	; even stretchier!

(define (make-rigid-glue minsize value)
  (make-glue minsize *rigid-glue-class* value))

(define (make-percent-glue minsize percent)
  (make-glue minsize *percent-glue-class* percent))

(define (make-fill-glue minsize value)
  (make-glue minsize *fill-glue-class* value))

(define (make-fil-glue minsize value)
  (make-glue minsize *fil-glue-class* value))

(define (rigid-glue? glue)
  (= (glue.class glue) *rigid-glue-class*))

(define (percent-glue? glue)
  (= (glue.class glue) *percent-glue-class*))

(define (fill-glue? glue)
  (= (glue.class glue) *fill-glue-class*))

(define (fil-glue? glue)
  (= (glue.class glue) *fil-glue-class*))

(define *fil-glue* (make-fil-glue 0 1))
(define *rigid-glue* (make-rigid-glue 0 1))
   

;;; Compose two glues laid end-to-end -- sum their minimum sizes
;;;   and their glue values (which implies that if the glue-classes
;;;   differ, then the resulting glue-class and glue-value are those
;;;   of the input glue with the larger glue-class).

(define (series-compose-glue g1 g2)
  (let ((c1 (glue.class g1))
 	(c2 (glue.class g2)))
    (if (< c2 c1)
 	(series-compose-glue g2 g1)
	(make-glue (+ (glue.minsize g1) (glue.minsize g2))
		   c2
		   (if (= c1 c2)
		       (+ (glue.value g1) (glue.value g2))
		       (glue.value g2))))))

;;; Compose two glues laid in parallel -- use the max of their
;;;   minimum sizes and the min of their stretchabilities (which
;;;   implies using the stretchability of the glue with the smaller
;;;   glue-class, or the smaller glue-value if the glue-classes are
;;;   equal).

(define (parallel-compose-glue g1 g2)
  (let ((c1 (glue.class g1))
	(c2 (glue.class g2)))
    (if (< c2 c1)
 	(parallel-compose-glue g2 g1)
	(make-glue (max (glue.minsize g1) (glue.minsize g2))
		   c1
		   (if (= c1 c2)
		       (min (glue.value g1) (glue.value g2))
		       (glue.value g1))))))

;;; Support > 2 glues as arguments

(define (compose-glues fcn list-of-glues)
  ;; If there's no glue at all, make it be fil glue.
  (if (null? list-of-glues)
      *fil-glue*
      (let loop ((cumulative-glue (car list-of-glues))
		 (rest (cdr list-of-glues)))
	(if (null? rest)
	    cumulative-glue
	    (let ((next-glue (car rest)))
	      (loop (fcn cumulative-glue next-glue)
		    (cdr rest)))))))

(define (series-compose-glues list-of-glues)
  (compose-glues series-compose-glue list-of-glues))

(define (parallel-compose-glues list-of-glues)
  (compose-glues parallel-compose-glue list-of-glues))


;;; Choose the less restrictive (in terms of minimum size) of two
;;;   glues.  This procedure is used for implementing the "orbox" combiner:

(define (choose-minimum-glue list-of-glues)
  (define (min-glue g1 g2)
    (let ((min1 (glue.minsize g1))
	  (min2 (glue.minsize g2)))
      (cond ((< min1 min2) g1)
	    ((> min1 min2) g2)
	    (else g1))))	; arbitrary choice
  (let ((g1 (car list-of-glues)))
    (let loop ((list-of-glues list-of-glues) (g g1))
      (if (null? (cdr list-of-glues))
	  (min-glue g (car list-of-glues))
	  (let* ((next-glue (car list-of-glues)))
	    (loop (cdr list-of-glues) (min-glue g next-glue)))))))

(define (choose-maximum-glue list-of-glues)
  (define (max-glue g1 g2)
    (let ((max1 (glue.minsize g1))
	  (max2 (glue.minsize g2)))
      (cond ((< max1 max2) g2)
	    ((> max1 max2) g1)
	    (else g1))))	; arbitrary choice
  (let ((g1 (car list-of-glues)))
    (let loop ((list-of-glues list-of-glues) (g g1))
      (if (null? list-of-glues)
	  g
	  (let* ((next-glue (car list-of-glues)))
	    (loop (cdr list-of-glues) (max-glue g next-glue)))))))

#|
;;; Magnify the minsize and stretchability of a glue by a factor:

(define (magnify-glue g factor)
  (make-glue (* factor (glue.minsize g))
	     (glue.class g)
	     (* factor (glue.value g))))

;;; Decide whether the given glue fits happily into the given space:

(define (glue-fits-space? g space)
  (<= (glue.minsize g) space))
|#

;;; Divide a given amount of space between two glues, according to the
;;;   rules given above.  Returns the amounts of space allocated to the
;;;   two glues to the continuation k.

(define (divide-space space g1 g2 k)
  (let ((m1 (glue.minsize g1))
	(m2 (glue.minsize g2)))
    (let ((msum (+ m1 m2)))
      (if (and (<= space msum) (> msum 0))
	  (let ((x1 (inexact->exact
		     (floor
		      (quotient (+ (* 2 m1 space) msum)
				(* 2 msum)))))) ; round off space allocation
	    (k x1 (- space x1)))
	  (let ((c1 (glue.class g1))
 		(c2 (glue.class g2)))
	    (cond ((< c1 c2) (k m1 (- space m1)))
		  ((> c1 c2) (k (- space m2) m2))
		  (else (let ((v1 (glue.value g1))
			      (v2 (glue.value g2)))
			  (let ((vsum (+ v1 v2)))
			    (let ((x1 (+ m1
					 (inexact->exact
					  (floor
					   (quotient
					    (+ (* 2 v1 (- space msum))
					       vsum)
					    (* 2 vsum)))))))
			      (k x1 (- space x1))))))))))))


;;; Given a space (width or height), a list of glues (assuming the
;;; order of glues provided is left to right), and a receiver, divides
;;; the space between the glues according to their properties.
;;; Receiver is applied to the resulting vector of positions which are
;;; offsets into the space. 

(define (conquer-space space list-of-glues receiver)
  (let* ((num-glues (length list-of-glues))
	 (glues (list->vector list-of-glues))
	 (cum-glues (compute-cumulative-glues list-of-glues))
	 (positions-vector (make-vector (+ num-glues 1))))
    (let loop ((s space) (n (- num-glues 1)))
      (vector-set! positions-vector (+ n 1) s)
      (if (> n 0)
	  (divide-space
	   s (vector-ref cum-glues (- n 1)) (vector-ref glues n)
	   (lambda (s1 s2)
	     s2				; ignore
	     (loop s1 (- n 1))))))
    (vector-set! positions-vector 0 0)
    (receiver positions-vector)))
	 

;;; Given a list of glues, returns a vector of cumulative glues --
;;; glues obtained by series composition of g1, g1&g2, (g1&g2)&g3, and
;;; so on.  For example,
;;;
;;;  (compute-cumulative-glues (list g1 g2 g3)) is equivalent to:
;;; 
;;;      (let* ((g12 (series-compose-glue g1 g2))
;;;             (g123 (series-compose-glue g12 g3)))	 
;;; 	   `#(,g1 ,g12 ,g123))

(define (compute-cumulative-glues list-of-glues)
  ;; If there's no glue at all, make it be fil glue.
  (if (null? list-of-glues)
      *fil-glue*
      (let* ((num-glues (length list-of-glues))
	     (cum-glues (make-vector num-glues))
	     (g1 (car list-of-glues)))
	(vector-set! cum-glues 0 g1)
	(let loop ((n 1) (old-glue g1) (glues (cdr list-of-glues)))
	  (if (= n num-glues)
	      cum-glues
	      (let* ((g (car glues))
		     (new-glue (series-compose-glue old-glue g)))
		(vector-set! cum-glues n new-glue)
		(loop (+ n 1) new-glue (cdr glues))))))))


;;; A space is basically a "piece of glue." It is of class fil, so it
;;; is very stretchable (more so than anything else). It can be used
;;; to fill in spaces between widgets in a box.
;;; This would probably be better off if implemented as a shape
;;; instead of a canvas, but for now (till shapes are working
;;; right)... 

(define (make-space . options)
  (let* ((configure-options (if options (car options) '()))
	 (space (make-canvas `(-width 0 -height 0 ,@configure-options))))
    (set-%hglue! space *fil-glue*)
    (set-%vglue! space *fil-glue*)
    space))



;;; Build a tabular array of boxes.  Each argument is a list of kids that
;;;   are to be arranged left-to-right, in hbox fashion.  These rows of boxes
;;;   are in turn stacked vertically, in vbox fashion; however, the sizes of
;;;   the boxes in different rows interact so that columns, as well as rows,
;;;   of boxes are kept aligned.  Thus (array-box '(A B C) '(D E F) '(G H J))
;;;   will generate the following arrangement of kids A-J:
;;;
;;;		A   B   C
;;;
;;;		D   E   F
;;;
;;;		G   H   J
;;;
;;;   regardless of the individual sizes of the component boxes.  Instead of
;;;   boxes, the following symbols may also appear as elements of an argument:
;;;
;;;      skip -- indicates the corresponding cell is to be left empty.
;;;      left -- indicates the box to the left spans into this cell as well.
;;;      up -- indicates the box above spans into this cell as well.
;;;
;;;   If the argument lists are not all of the same length, they are considered
;;;   to be padded out at the end with as many occurrences of the symbol "left"
;;;   as needed to make their lengths all equal.

(define (kids-lists->complete-kids-lists kids-lists)
  (let ((num-cols (apply max (map length kids-lists))))
    (define (kids-list->complete-kids-list kids-list)
      (let loop ((col 0) (complete-kids-list '()) (rest-kids kids-list))
	(if (= col num-cols)
	    complete-kids-list
	    (let* ((next-kid
		    (if (null? rest-kids)
			'left
			(car rest-kids)))
		   (rest-kids
		    (if (null? rest-kids)
			'()
			(cdr rest-kids)))
		   (next-complete-list
		    (append complete-kids-list (list next-kid))))
	      (loop (+ col 1) next-complete-list rest-kids)))))
    
    (let loop ((complete-kids-lists '()) (rest-kids-lists kids-lists))
      (if (null? rest-kids-lists)
	  complete-kids-lists
	  (let ((next-list (car rest-kids-lists)))
	    (loop (append complete-kids-lists
			  (list (kids-list->complete-kids-list next-list)))
		  (cdr rest-kids-lists)))))))


(define (row-lists->col-lists kids-lists)
  (let ((kids-lists (kids-lists->complete-kids-lists kids-lists)))
    (let loop ((col 0) (col-lists '()))
      (if (= col (apply max (map length kids-lists)))
	  col-lists
	  (let ((col-list
		 (let loop ((row 0) (col-list '()))
		   (if (= row (length kids-lists))
		       col-list
		       (loop (+ row 1)
			     (cons (list-ref (list-ref kids-lists row) col)
				   col-list))))))
	    (loop (+ col 1) (cons col-list col-lists)))))))

(define (array-size kids-lists)
  (let ((col-lists (row-lists->col-lists kids-lists)))
    (make-size
     (apply +
	    (map (lambda (col-list)
		   (apply max
			  (map (lambda (kid)
				 (if (symbol? kid)
				     0
				     (size.width (get-desired-size kid))))
			       col-list)))
		 col-lists))
     (apply +
	    (map (lambda (row-list)
		   (apply max
			  (map (lambda (kid)
				 (if (symbol? kid)
				     0
				     (size.height (get-desired-size kid))))
			       row-list)))
		 kids-lists)))))

(define (array-arrange kids-lists my-screen-area)
  (let* ((my-width (UITKRectangle.Width my-screen-area))
	 (my-height (UITKRectangle.Height my-screen-area))
	 (my-offset (UITKRectangle.Offset my-screen-area))
	 (X (point.X my-offset))
	 (Y (point.Y my-offset))
	 (kids-lists (kids-lists->complete-kids-lists kids-lists))
	 )

    (define (kids-lists->kids-array kids-lists)
      (let loop ((kids-lists kids-lists) (kids-array-list '()))
	(if (null? kids-lists)
	    (list->vector kids-array-list)
	    (loop (cdr kids-lists)
		  (append kids-array-list
			  (list (list->vector (car kids-lists))))))))

    (let* ((kids-array (kids-lists->kids-array kids-lists))
	   (num-rows (vector-length kids-array))
	   (num-cols (vector-length (vector-ref kids-array 0))))

      (define (aref array row col)
	(vector-ref (vector-ref array row) col))

      (define (aset! array row col value)
	(vector-set! (vector-ref array row) col value))

      (define (kids-column-hglue col)
	(define (get-hglue kid)
	  (if (symbol? kid)
	      *fil-glue*
	      (%hglue kid)))
	(let* ((kid1 (aref kids-array 0 col))
	       (g1 (get-hglue kid1)))
	  (let loop ((row 1) (g g1))
	    (if (< row num-rows)
		(let* ((next-kid (aref kids-array row col))
		       (next-glue (get-hglue next-kid)))
		  (loop (+ row 1) (parallel-compose-glue g next-glue)))
		g))))

      (define (kids-row-vglue row)
	(define (get-vglue kid)
	  (if (symbol? kid)
	      *fil-glue*
	      (%vglue kid)))
	(let* ((kid1 (aref kids-array row 0))
	       (g1 (get-vglue kid1)))
	  (let loop ((col 1) (g g1))
	    (if (< col num-cols)
		(let* ((next-kid (aref kids-array row col))
		       (next-glue (get-vglue next-kid)))
		  (loop (+ col 1) (parallel-compose-glue g next-glue)))
		g))))

      (define (enumerate-interval from to)
	(if (> from to)
	    '()
	    (cons from (enumerate-interval (+ from 1) to))))

      (define (instantiate-kids h-positions-vector v-positions-vector)
	(let loop-rows ((row 0))
	  (if (= row num-rows)
	      'done
	      (let loop-cols ((col 0))
		(if (= col num-cols)
		    (loop-rows (+ row 1))
		    (let ((kid (aref kids-array row col)))
		      (if (symbol? kid)
			  (cond ((eq? kid 'skip)
				 (loop-cols (+ col 1)))
				((or (eq? kid 'left) (eq? kid 'up))
				 ;; wasn't to the right or below a
				 ;; valid child, so it's either been
				 ;; taken care of already, or needs to
				 ;; be 'skip.
				 (aset! kids-array row col 'skip)
				 (loop-cols (+ col 1)))
				(else
				 (error
				  "Illegal symbol in array box:"
				  "Must be 'skip, 'left, or 'up." kid)))
			  (let* ((kid-x-offset
				  (vector-ref h-positions-vector col))
				 (kid-y-offset
				  (vector-ref v-positions-vector row))
				 (width (- (vector-ref h-positions-vector
						       (+ col 1))
					   kid-x-offset))
				 (height (- (vector-ref v-positions-vector
							(+ row 1))
					    kid-y-offset)))

			    (let expand-h-loop ((col+ 1) (wid width))
			      (let ((new-col (+ col+ col)))
				(if (= new-col num-cols)
				    (set! width wid)
				    (let ((next-h-kid (aref kids-array row new-col)))
				      (if (symbol? next-h-kid)
					  (cond
					   ((eq? next-h-kid 'left)
					    (let* ((x-offset
						    (vector-ref h-positions-vector
								new-col))
						   (new-wid
						    (+ wid
						       (- (vector-ref
							  	 h-positions-vector
								 (+ new-col 1))
							  x-offset))))
					      (aset! kids-array row new-col 'skip)
					      (expand-h-loop (+ col+ 1) new-wid)))
					   ((eq? next-h-kid 'skip)
					    (set! width wid))
					   ((eq? next-h-kid 'up)
					    (set! width wid))
					   (else
					    (error "Illegal symbol in array box:"
						   "Must be 'skip, 'left, or 'up."
						   next-h-kid)))
					  (set! width wid))))))
		      
			    (let expand-v-loop ((row+ 1) (ht height))
			      (let ((new-row (+ row+ row)))
				(if (= new-row num-rows)
				    (set! height ht)
				    (let ((next-v-kid (aref kids-array new-row col)))
				      (if (symbol? next-v-kid)
					  (cond
					   ((eq? next-v-kid 'up)
					    (let* ((y-offset
						    (vector-ref v-positions-vector
								new-row))
						   (new-ht
						    (+ ht (- (vector-ref
							      	v-positions-vector
								(+ new-row 1))
							     y-offset))))
					      (aset! kids-array new-row col 'skip)
					      (expand-v-loop (+ row+ 1) new-ht)))
					   ((eq? next-v-kid 'skip)
					    (set! height ht))
					   ((eq? next-v-kid 'left)
					    (set! height ht))
					   (else
					    (error "Illegal symbol in array box:"
						   "Must be 'skip, 'left, or 'up."
						   next-v-kid)))
					  (set! height ht))))))
		      
			    (assign-screen-area!
			     kid
			     (make-UITKRectangle (make-point (+ X kid-x-offset)
							     (+ Y kid-y-offset))
						 (make-size width height)))
			    (loop-cols (+ col 1))))))))))

      (let ((cols-hglues (map kids-column-hglue (enumerate-interval 0 (- num-cols 1))))
	    (rows-vglues (map kids-row-vglue (enumerate-interval 0 (- num-rows 1)))))
	(conquer-space
	 my-width
	 cols-hglues
	 (lambda (h-positions-vector)
	   (conquer-space
	    my-height
	    rows-vglues
	    (lambda (v-positions-vector)
	      (instantiate-kids h-positions-vector v-positions-vector))))))
      )))

(define (array-get-hglue kids-lists)
  ;; or minimum?
  (choose-maximum-glue
   (map (lambda (kids-list)
	  (series-compose-glues
	   (map (lambda (kid)
		  (if (symbol? kid)
		      *rigid-glue*
		      (%hglue kid)))
		kids-list)))
	kids-lists)))

(define (array-get-vglue kids-lists)
  (choose-maximum-glue
   (map (lambda (kids-list)
	  (series-compose-glues
	   (map (lambda (kid)
		  (if (symbol? kid)
		      *rigid-glue*
		      (%vglue kid)))
		kids-list)))
	(row-lists->col-lists kids-lists))))


(define (find-real-array-box-children kids-lists)
  (let loop-lists ((kids-lists kids-lists)
		   (valid-kids-list '()))
    (if (null? kids-lists)
	valid-kids-list
	(let loop-list ((kids-list (car kids-lists))
			(valid-kids '()))
	  (if (null? kids-list)
	      (loop-lists (cdr kids-lists)
			  (append valid-kids-list valid-kids))
	      (let ((kid (car kids-list)))
		(if (symbol? kid)
		    (loop-list (cdr kids-list) valid-kids)
		    (loop-list (cdr kids-list)
			       (append valid-kids (list kid))))))))))

(define (array:rearrange me)
  (let ((screen-area (used-screen-area me))
	(kids-lists (ArrayBox%.kids-lists me)))
    (if screen-area
	(let ((new-size (array-size kids-lists)))
	  (set-%desired-size! me new-size)
	  (if (size= new-size (UITKRectangle.Size screen-area))
	      (begin (assign-glue! me)
		     (array-arrange kids-lists screen-area))
	      (begin
		(set-%desired-size! me new-size)
		(geometry-change! me screen-area #T)
		(if (eq? screen-area (used-screen-area me))
		    (begin (assign-glue! me)
			   (array-arrange kids-lists screen-area)))))))))

(define (array-box-add-child! me kid)
  (if (not (valid-child? kid))
      (error "ARRAY-BOX-ADD-CHILD!: Bad UIObj" kid))
  (one-parent-only! kid me)
  (set-ArrayBox%.kids! me (append (ArrayBox%.kids me) (list kid)))
  (on-geometry-change!
   kid 'ARRAY-BOX
   (lambda (old-screen-area new-screen-area)
     old-screen-area			; Not used
     (if (eq? new-screen-area #T)	; Instigated by child, not manager
	 (array:rearrange me))))
  (on-death! kid 'ARRAY-BOX		; Die horribly ....
	     (lambda ()
	       (assign-drawing-surface! me 'RETRACTED)))
  (assign-drawing-surface! kid (drawing-surface me))
  (array:rearrange me))

(define (array-box-assign-drawing-surface! me surface)
  (check-drawing-surface! me surface)
  (for-each (lambda (kid)
	      (if (eq? surface 'RETRACTED)
		  (forget! kid 'ARRAY-BOX))
	      (assign-drawing-surface! kid surface))
	    (ArrayBox%.kids me))
  (if (DrawingSurface? surface)
      (set-%desired-size! me (array-size (ArrayBox%.kids-lists me))))
  (if (eq? surface 'RETRACTED)
      (death! me)
      (geometry-change! me #F #F))
  'OK)

(define (array-box-assign-screen-area! me screen-area)
  (cond ((vector? screen-area)
	 (set-assigned-screen-area! me screen-area)
	 (let ((old (used-screen-area me)))
	   (if (not (screen-area= old screen-area))
	       (begin
		 (set-used-screen-area! me screen-area)
		 (array:rearrange me)
		 (geometry-change! me old screen-area))))
	 screen-area)
	((not screen-area)
	 (set-assigned-screen-area! me screen-area)
	 (let ((old (used-screen-area me)))
	   (if (not (screen-area= old screen-area))
	       (begin
		 (set-used-screen-area! me screen-area)
		 (retract-area (ArrayBox%.kids me))
		 (geometry-change! me old screen-area))))
	 screen-area)
	(else
	 (error "ARRAY-BOX-ASSIGN-SCREEN-AREA!: Bad screen-area" screen-area))))

(define (array-box-assign-glue! me)
  (let ((kids-lists (ArrayBox%.kids-lists me)))
    (for-each assign-glue! (ArrayBox%.kids me))
    (set-%hglue! me (array-get-hglue kids-lists)) 
    (set-%vglue! me (array-get-vglue kids-lists))))

;; Box Maker
(define (array-box-maker kids-lists)
  (make-ArrayBox%
   (make-UIObjInternals 'invalid-arraybox-1 ; array-box-add-child!
			'invalid-arraybox-2 ; array-box-remove-child!
			UIObj-set-context!
			array-box-assign-screen-area!
			array-box-assign-drawing-surface!
			UIObj-point-within?
			UIObj-rectangle-overlaps?
			UIObj-handle-event
			UIObj-get-desired-size
			UIObj-assigned-screen-area
			UIObj-used-screen-area
			UIObj-set-assigned-screen-area!
			UIObj-set-used-screen-area!
			array-box-assign-glue!)
   kids-lists))

(define (array-box-propagator box)
  (lambda (event)
    (for-each (lambda (kid)
		(if (event-within? kid event)
		    (handle-event kid event)))
	      (array-box%.kids box))))
	      
(define (make-array-box . kids-lists)
  (let ((kids (find-real-array-box-children kids-lists)))
    (let ((me (array-box-maker kids-lists)))
      (on-event! me 'ARRAY-BOX
		 (array-box-propagator me))
      (for-each (lambda (kid) (array-box-add-child! me kid))
		kids)
      me)))
