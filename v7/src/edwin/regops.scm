;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; Operations on Groups

(declare (usual-integrations)
	 (integrate-external "edb:struct.bin.0"))

;;;; Region/Mark Operations

;;; These operations are high level, easy to use, but slow compared to
;;; the direct group operations below.  They also cons marks, which
;;; may be a consideration under certain circumstances.

(define (string->region string)
  (group-region (make-group (string-copy string))))

(define (substring->region string start end)
  (group-region (make-group (substring string start end))))

(define (region-insert! mark region)
  (let ((string (region->string region))
	(group (mark-group mark))
	(start (mark-index mark)))
    (let ((n (string-length string)))
      (group-insert-substring! group start string 0 n)
      (%make-region (%make-temporary-mark group start #!FALSE)
		    (%make-temporary-mark group (+ start n) #!TRUE)))))

(define (region-insert-string! mark string)
  (group-insert-substring! (mark-group mark) (mark-index mark)
			   string 0 (string-length string)))

(define (region-insert-substring! mark string start end)
  (group-insert-substring! (mark-group mark) (mark-index mark)
			   string start end))

(define (region-insert-newline! mark)
  (group-insert-char! (mark-group mark) (mark-index mark) char:newline))

(define (region-insert-char! mark char)
  (group-insert-char! (mark-group mark) (mark-index mark) char))

(define (region->string region)
  (group-extract-string (region-group region)
			(region-start-index region)
			(region-end-index region)))

(define (region-delete! region)
  (group-delete! (region-group region)
		 (region-start-index region)
		 (region-end-index region)))

(define (region-extract! region)
  (let ((group (region-group region))
	(start (region-start-index region))
	(end (region-end-index region)))
    (let ((string (group-extract-string group start end)))
      (group-delete! group start end)
      (group-region (make-group string)))))

(define (region-copy region)
  (string->region (region->string region)))

(define (mark-left-char mark)
  (if (group-start? mark)
      (error "No left char: MARK-LEFT-CHAR" mark)
      (group-left-char (mark-group mark) (mark-index mark))))

(define (mark-right-char mark)
  (if (group-end? mark)
      (error "No right char: MARK-RIGHT-CHAR" mark)
      (group-right-char (mark-group mark) (mark-index mark))))

(define (mark-delete-left-char! mark)
  (if (group-start? mark)
      (error "No left char: MARK-DELETE-LEFT-CHAR!" mark)
      (group-delete-left-char! (mark-group mark) (mark-index mark))))

(define (mark-delete-right-char! mark)
  (if (group-end? mark)
      (error "No right char: MARK-DELETE-RIGHT-CHAR!" mark)
      (group-delete-right-char! (mark-group mark) (mark-index mark))))

;;; **** This is not a great thing to do.  It will screw up any marks
;;; that are within the region, pushing them to either side.
;;; Conceptually we just want the characters to be altered.

(define (region-transform! region operation)
  (let ((m (mark-permanent! (region-start region))))
    (let ((string (operation (region->string region))))
      (region-delete! region)
      (region-insert-string! m string))))

;;;; Clipping

(define (region-clip! region)
  (let ((group (region-group region))
	(start (mark-right-inserting (region-start region)))
	(end (mark-left-inserting (region-end region))))
    (record-clipping! group (mark-index start) (mark-index end))
    (vector-set! group group-index:start-mark start)
    (vector-set! group group-index:end-mark end)
    (vector-set! group group-index:display-start start)
    (vector-set! group group-index:display-end end)))

(define (group-un-clip! group)
  (let ((start (%make-permanent-mark group 0 #!FALSE))
	(end (%make-permanent-mark group (group-length group) #!TRUE)))
    (record-clipping! group 0 (group-length group))
    (vector-set! group group-index:start-mark start)
    (vector-set! group group-index:end-mark end)
    (vector-set! group group-index:display-start start)
    (vector-set! group group-index:display-end end)))

(define (with-region-clipped! new-region thunk)
  (let ((group (region-group new-region))
	(old-region))
    (dynamic-wind (lambda ()
		    (set! old-region (group-region group))
		    (region-clip! (set! new-region)))
		  thunk
		  (lambda ()
		    (set! new-region (group-region group))
		    (region-clip! (set! old-region))))))

(define (without-group-clipped! group thunk)
  (define old-region)
  (dynamic-wind (lambda ()
		  (set! old-region (group-region group))
		  (group-un-clip! group))
		thunk
		(lambda ()
		  (region-clip! (set! old-region)))))

(define (group-clipped? group)
  (not (and (zero? (group-start-index group))
	    (= (group-end-index group) (group-length group)))))

(define (group-unclipped-region group)
  (make-region (make-mark group 0)
	       (make-mark group (group-length group))))

;;;; Group Operations

;;; These high-performance ops deal directly with groups and indices
;;; for speed and the least consing.  Since indices are not in general
;;; valid across modifications to the group, they can only be used in
;;; limited ways.  To save an index across a modification, it must be
;;; consed into a permanent mark.

(define (group-extract-string group start end)
  (let ((text (group-text group))
	(gap-start (group-gap-start group))
	(length (group-gap-length group)))
    (cond ((<= end gap-start)
	   (substring text start end))
	  ((>= start gap-start)
	   (substring text (+ start length) (+ end length)))
	  (else
	   (let ((string (string-allocate (- end start))))
	     (substring-move-right! text start gap-start string 0)
	     (substring-move-right! text (group-gap-end group) (+ end length)
				    string (- gap-start start))
	     string)))))

(define (group-insert-string! group index string)
  (group-insert-substring! group index string 0 (string-length string)))

(define (group-left-char group index)
  (string-ref (group-text group)
	      (-1+ (group-index->position group index #!FALSE))))

(define (group-right-char group index)
  (string-ref (group-text group)
	      (group-index->position group index #!TRUE)))

(define (group-delete-left-char! group index)
  (group-delete! group (-1+ index) index))

(define (group-delete-right-char! group index)
  (group-delete! group index (1+ index)))

;;; This parameter controls how much extra space (in characters) is
;;; allocated when the gap is too small to contain a given insertion.
(define gap-allocation-extra 2000)

(define group-insert-char!)
(define %group-insert-char!)
(define group-insert-substring!)
(define %group-insert-substring!)
(define group-delete!)
(define group-operations-package)
(let ()

(set! group-operations-package
      (the-environment))

(set! group-insert-char!
(named-lambda (group-insert-char! group index char)
  (without-interrupts
   (lambda ()
     (group-insert-char-kernel group index char)
     (record-insertion! group index (group-gap-start group))))))

(set! %group-insert-char!
(named-lambda (%group-insert-char! group index char)
  (without-interrupts
   (lambda ()
     (group-insert-char-kernel group index char)))))

(set! group-insert-substring!
(named-lambda (group-insert-substring! group index string start end)
  (without-interrupts
   (lambda ()
     (group-insert-substring-kernel group index string start end)
     (record-insertion! group index (group-gap-start group))))))

(set! %group-insert-substring!
(named-lambda (%group-insert-substring! group index string start end)
  (without-interrupts
   (lambda ()
     (group-insert-substring-kernel group index string start end)))))

(declare (integrate group-insert-char-kernel group-insert-substring-kernel))

(define (group-insert-char-kernel group index char)
  (declare (integrate group index char))
  (barf-if-read-only group)
  (move-gap-to! group index)
  (guarantee-gap-length! group 1)
  (string-set! (group-text group) index char)
  (vector-set! group group-index:gap-length (-1+ (group-gap-length group)))
  (let ((gap-start* (1+ index)))
    (vector-set! group group-index:gap-start gap-start*)
    (undo-record-insertion! group index gap-start*)))

(define (group-insert-substring-kernel group index string start end)
  (declare (integrate group index string start end))
  (barf-if-read-only group)
  (move-gap-to! group index)
  (let ((n (- end start)))
    (guarantee-gap-length! group n)
    (substring-move-right! string start end (group-text group) index)
    (vector-set! group group-index:gap-length (- (group-gap-length group) n))
    (let ((gap-start* (+ index n)))
      (vector-set! group group-index:gap-start gap-start*)
      (undo-record-insertion! group index gap-start*))))

(set! group-delete!
(named-lambda (group-delete! group start end)
  (without-interrupts
   (lambda ()
     (if (not (= start end))
	 (begin (barf-if-read-only group)
		(let ((gap-start (group-gap-start group))
		      (new-end (+ end (group-gap-length group))))
		  ;; Guarantee that the gap is between START and END.
		  (cond ((< gap-start start)
			 (move-gap-to-right! group start))
			((> gap-start end)
			 (move-gap-to-left! group end)))
		  (undo-record-deletion! group start end)
		  (record-deletion! group start end)
		  ;; Clear out any marks.
		  (for-each-mark group
		    (lambda (mark)
		      (let ((position (mark-position mark)))
			(if (and (<= start position)
				 (<= position new-end))
			    (%set-mark-position!
			     mark
			     (if (mark-left-inserting? mark)
				 new-end
				 start))))))
		  ;; Widen the gap to the new boundaries.
		  (vector-set! group group-index:gap-start start)
		  (vector-set! group group-index:gap-end new-end)
		  (vector-set! group group-index:gap-length
			       (- new-end start)))))))))

(declare (integrate barf-if-read-only))
(define (barf-if-read-only group)
  (declare (integrate group))
  (if (group-read-only? group)
      (editor-error "Trying to modify read only text.")))

;;;; The Gap

(define (move-gap-to! group index)
  (let ((gap-start (group-gap-start group)))
    (cond ((< index gap-start)
	   (move-gap-to-left! group index))
	  ((> index gap-start)
	   (move-gap-to-right! group index)))))

(define (move-gap-to-left! group new-start)
  (let ((start (group-gap-start group))
	(length (group-gap-length group))
	(text (group-text group)))
    (let ((new-end (+ new-start length)))
      (for-each-mark group
	(lambda (mark)
	  (let ((position (mark-position mark)))
	    (cond ((and (< new-start position)
			(<= position start))
		   (%set-mark-position! mark (+ position length)))
		  ((and (mark-left-inserting? mark)
			(= new-start position))
		   (%set-mark-position! mark new-end))))))
      (substring-move-right! text new-start start text new-end)
      (vector-set! group group-index:gap-start new-start)
      (vector-set! group group-index:gap-end new-end))))

(define (move-gap-to-right! group new-start)
  (let ((start (group-gap-start group))
	(end (group-gap-end group))
	(length (group-gap-length group))
	(text (group-text group)))
    (let ((new-end (+ new-start length)))
      (for-each-mark group
	(lambda (mark)
	  (let ((position (mark-position mark)))
	    (cond ((and (> new-end position)
			(>= position end))
		   (%set-mark-position! mark (- position length)))
		  ((and (not (mark-left-inserting? mark))
			(= new-end position))
		   (%set-mark-position! mark new-start))))))
      (substring-move-left! text end new-end text start)
      (vector-set! group group-index:gap-start new-start)
      (vector-set! group group-index:gap-end new-end))))

(define (guarantee-gap-length! group n)
  (if (< (group-gap-length group) n)
      (let ((n (+ n gap-allocation-extra))
	    (text (group-text group))
	    (start (group-gap-start group))
	    (end (group-gap-end group))
	    (length (group-gap-length group)))
	(let ((end* (string-length text)))
	  (let ((text* (string-allocate (+ end* n)))
		(new-end (+ end n)))
	    (substring-move-right! text 0 start text* 0)
	    (substring-move-right! text end end* text* new-end)
	    (vector-set! group group-index:text text*)
	    (vector-set! group group-index:gap-end new-end)
	    (if (zero? length)
		(for-each-mark group
		  (lambda (mark)
		    (let ((position (mark-position mark)))
		      (cond ((> position end)
			     (%set-mark-position! mark (+ position n)))
			    ((= position end)
			     (%set-mark-position!
			      mark
			      (if (mark-left-inserting? mark)
				  new-end start)))))))
		(for-each-mark group
		  (lambda (mark)
		    (let ((position (mark-position mark)))
		      (if (>= position end)
			  (%set-mark-position! mark (+ position n)))))))))
	(vector-set! group group-index:gap-length (+ length n)))))

)

;;; Edwin Variables:
;;; Scheme Environment: edwin-package
;;; End:
