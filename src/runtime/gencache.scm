#| -*-Scheme-*-

$Id: gencache.scm,v 1.7 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Method Caches for Generic Dispatch

;;; From "Efficient Method Dispatch in PCL", Gregor Kiczales and Luis
;;; Rodriguez, Proceedings of the 1990 ACM Conference on Lisp and
;;; Functional Programming.  Parts of this code are based on the
;;; September 16, 1992 PCL implementation.

(declare (usual-integrations)
	 (integrate-external "gentag"))

(define-structure (cache (constructor %make-cache))
  (tag-index 0)
  (mask 0 read-only #t)
  (limit 0 read-only #t)
  (n-tags 0 read-only #t)
  (tags '#() read-only #t)
  (values '#() read-only #t)
  (overflow '()))

(define (new-cache n-tags)
  (make-cache dispatch-tag-index-start n-tags 4))

(define (make-cache tag-index n-tags length)
  ;; LENGTH is assumed to be a power of two.
  (%make-cache tag-index
	       (fix:- length 1)
	       (cond ((fix:<= length 4) 1)
		     ((fix:<= length 16) 4)
		     (else 6))
	       n-tags
	       (make-vector length (make-list n-tags #f))
	       (make-vector length #f)
	       '()))

(define-integrable (cache-length cache)
  (vector-length (cache-tags cache)))

(define-integrable (cache-line-tags cache line)
  (vector-ref (cache-tags cache) line))

(define-integrable (set-cache-line-tags! cache line tags)
  (vector-set! (cache-tags cache) line tags))

(define-integrable (cache-line-value cache line)
  (vector-ref (cache-values cache) line))

(define-integrable (set-cache-line-value! cache line value)
  (vector-set! (cache-values cache) line value))

(define-integrable (cache-next-line cache line)
  (if (fix:= (fix:+ line 1) (cache-length cache))
      0
      (fix:+ line 1)))

(define-integrable (cache-line-separation cache line line*)
  (let ((n (fix:- line* line)))
    (if (fix:< n 0)
	(fix:+ n (cache-length cache))
	n)))

(define (probe-cache cache tags)
  (let ((line (compute-primary-cache-line cache tags)))
    (and line
	 (let ((limit (cache-limit cache)))
	   (letrec
	       ((search-lines
		 (lambda (line i)
		   (cond ((match (cache-line-tags cache line))
			  (cache-line-value cache line))
			 ((fix:= i limit)
			  (search-overflow (cache-overflow cache)))
			 (else
			  (search-lines (cache-next-line cache line)
					(fix:+ i 1))))))
		(search-overflow
		 (lambda (overflow)
		   (and (not (null? overflow))
			(if (match (caar overflow))
			    (cdar overflow)
			    (search-overflow (cdr overflow))))))
		(match
		 (lambda (tags*)
		   (let loop ((w1 tags*) (w2 tags))
		     (and (eq? (system-pair-car w1) (system-pair-car w2))
			  (or (null? (system-pair-cdr w1))
			      (loop (system-pair-cdr w1)
				    (system-pair-cdr w2))))))))
	     (search-lines line 0))))))

(define (compute-primary-cache-line cache tags)
  (let ((index (cache-tag-index cache))
	(mask (cache-mask cache)))
    (let loop ((tags tags) (line 0))
      (cond ((null? tags)
	     line)
	    ((not (system-pair-car tags))
	     #f)
	    (else
	     (loop (system-pair-cdr tags)
		   (fix:and (fix:+ line
				   (dispatch-tag-ref (system-pair-car tags)
						     index))
			    mask)))))))

(define (cache-entry-reusable? tags tags*)
  ;; True iff TAGS is (1) empty, (2) contains a tag that is invalid,
  ;; or (3) has the same tags as TAGS*.
  (or (not tags)
      (let loop ((tags tags) (tags* tags*))
	(or (null? tags)
	    (not (system-pair-car tags))
	    (and (eq? (system-pair-car tags) (system-pair-car tags*))
		 (loop (system-pair-cdr tags) (system-pair-cdr tags*)))))))

(define (cache-count cache)
  (let ((length (cache-length cache)))
    (do ((line 0 (fix:+ line 1))
	 (count 0
		(if (let ((tags (cache-line-tags cache line)))
		      (and tags
			   (let loop ((tags tags))
			     (or (null? tags)
				 (and (system-pair-car tags)
				      (loop (system-pair-cdr tags)))))))
		    (fix:+ count 1)
		    count)))
	((fix:= line length) count))))

(declare (integrate-operator probe-cache-1))
(define (probe-cache-1 cache w1)
  (let ((line
	 (fix:and (dispatch-tag-ref w1 (cache-tag-index cache))
		  (cache-mask cache)))
	(match
	 (lambda (tags)
	   (declare (integrate tags))
	   (eq? w1 (system-pair-car tags)))))
    (declare (integrate line))
    (declare (integrate-operator match))
    (if (match (cache-line-tags cache line))
	(cache-line-value cache line)
	(let ((limit (cache-limit cache)))
	  (let search-lines ((line (cache-next-line cache line)) (i 0))
	    (cond ((fix:= i limit)
		   (let search-overflow ((entries (cache-overflow cache)))
		     (and (not (null? entries))
			  (if (match (caar entries))
			      (cdar entries)
			      (search-overflow (cdr entries))))))
		  ((and (cache-line-tags cache line)
			(match (cache-line-tags cache line)))
		   (cache-line-value cache line))
		  (else
		   (search-lines (cache-next-line cache line)
				 (fix:+ i 1)))))))))

(declare (integrate-operator probe-cache-2))
(define (probe-cache-2 cache w1 w2)
  (let ((line
	 (fix:and (fix:+ (dispatch-tag-ref w1 (cache-tag-index cache))
			 (dispatch-tag-ref w2 (cache-tag-index cache)))
		  (cache-mask cache)))
	(match
	 (lambda (tags)
	   (declare (integrate tags))
	   (and (eq? w1 (system-pair-car tags))
		(eq? w2 (system-pair-car (system-pair-cdr tags)))))))
    (declare (integrate line))
    (declare (integrate-operator match))
    (if (and (cache-line-tags cache line)
	     (match (cache-line-tags cache line)))
	(cache-line-value cache line)
	(let ((limit (cache-limit cache)))
	  (let search-lines ((line (cache-next-line cache line)) (i 0))
	    (cond ((fix:= i limit)
		   (let search-overflow ((entries (cache-overflow cache)))
		     (and (not (null? entries))
			  (if (match (caar entries))
			      (cdar entries)
			      (search-overflow (cdr entries))))))
		  ((and (cache-line-tags cache line)
			(match (cache-line-tags cache line)))
		   (cache-line-value cache line))
		  (else
		   (search-lines (cache-next-line cache line)
				 (fix:+ i 1)))))))))

(declare (integrate-operator probe-cache-3))
(define (probe-cache-3 cache w1 w2 w3)
  (let ((line
	 (fix:and
	  (fix:+ (dispatch-tag-ref w1 (cache-tag-index cache))
		 (fix:+ (dispatch-tag-ref w2 (cache-tag-index cache))
			(dispatch-tag-ref w3 (cache-tag-index cache))))
	  (cache-mask cache)))
	(match
	 (lambda (tags)
	   (declare (integrate tags))
	   (and (eq? w1 (system-pair-car tags))
		(eq? w2 (system-pair-car (system-pair-cdr tags)))
		(eq? w3 (system-pair-car
			 (system-pair-cdr (system-pair-cdr tags))))))))
    (declare (integrate line))
    (declare (integrate-operator match))
    (if (match (cache-line-tags cache line))
	(cache-line-value cache line)
	(let ((limit (cache-limit cache)))
	  (let search-lines ((line (cache-next-line cache line)) (i 0))
	    (cond ((fix:= i limit)
		   (let search-overflow ((entries (cache-overflow cache)))
		     (and (not (null? entries))
			  (if (match (caar entries))
			      (cdar entries)
			      (search-overflow (cdr entries))))))
		  ((and (cache-line-tags cache line)
			(match (cache-line-tags cache line)))
		   (cache-line-value cache line))
		  (else
		   (search-lines (cache-next-line cache line)
				 (fix:+ i 1)))))))))

(declare (integrate-operator probe-cache-4))
(define (probe-cache-4 cache w1 w2 w3 w4)
  (let ((line
	 (fix:and
	  (fix:+ (fix:+ (dispatch-tag-ref w1 (cache-tag-index cache))
			(dispatch-tag-ref w2 (cache-tag-index cache)))
		 (fix:+ (dispatch-tag-ref w3 (cache-tag-index cache))
			(dispatch-tag-ref w4 (cache-tag-index cache))))
	  (cache-mask cache)))
	(match
	 (lambda (tags)
	   (declare (integrate tags))
	   (and (eq? w1 (system-pair-car tags))
		(eq? w2 (system-pair-car (system-pair-cdr tags)))
		(eq? w3 (system-pair-car
			 (system-pair-cdr (system-pair-cdr tags))))
		(eq? w4 (system-pair-car
			 (system-pair-cdr
			  (system-pair-cdr (system-pair-cdr tags)))))))))
    (declare (integrate line))
    (declare (integrate-operator match))
    (if (match (cache-line-tags cache line))
	(cache-line-value cache line)
	(let ((limit (cache-limit cache)))
	  (let search-lines ((line (cache-next-line cache line)) (i 0))
	    (cond ((fix:= i limit)
		   (let search-overflow ((entries (cache-overflow cache)))
		     (and (not (null? entries))
			  (if (match (caar entries))
			      (cdar entries)
			      (search-overflow (cdr entries))))))
		  ((and (cache-line-tags cache line)
			(match (cache-line-tags cache line)))
		   (cache-line-value cache line))
		  (else
		   (search-lines (cache-next-line cache line)
				 (fix:+ i 1)))))))))

(define (fill-cache cache tags value)
  ;; TAGS must be converted to a weak list since it will be stored in
  ;; the cache, and we don't want the cache to prevent the tags from
  ;; being GCed.
  (let ((tags (list->weak-list tags)))
    (or (fill-cache-if-possible cache tags value)
	(and (< (cache-count cache) (* (cache-length cache) .8))
	     (adjust-cache cache tags value))
	(expand-cache cache tags value))))

(define (fill-cache-if-possible cache tags value)
  (let ((primary (compute-primary-cache-line cache tags)))
    (if primary
	(let ((free (find-free-cache-line cache primary tags)))
	  (and free
	       (begin
		 (set-cache-line-tags! cache free tags)
		 (set-cache-line-value! cache free value)
		 cache)))
	;; TAGS contains an invalid tag.  Do nothing and return CACHE
	;; because the fill is no longer needed.  While other logic
	;; tries to eliminate this case, it can still happen when one
	;; of the tags is GCed during complex cache operations.
	cache)))

(define (adjust-cache cache tags value)
  ;; Try to rehash the cache.  If that fails, try rehashing with
  ;; different tag indexes.  Fail only when all of the tag indexes
  ;; have been tried and none has worked.
  (let ((length (cache-length cache)))
    (let ((new-cache
	   (make-cache (cache-tag-index cache)
		       (cache-n-tags cache)
		       length)))
      (letrec
	  ((fill-lines
	    (lambda (line)
	      (cond ((fix:= line length)
		     (fill-overflow (cache-overflow cache)))
		    ((try-entry (cache-line-tags cache line)
				(cache-line-value cache line))
		     (fill-lines (fix:+ line 1)))
		    (else
		     (try-next-tag-index)))))
	   (fill-overflow
	    (lambda (entries)
	      (cond ((null? entries)
		     (or (fill-cache-if-possible new-cache tags value)
			 (try-next-tag-index)))
		    ((try-entry (caar entries) (cdar entries))
		     (fill-overflow (cdr entries)))
		    (else
		     (try-next-tag-index)))))
	   (try-entry
	    (lambda (tags* value)
	      (or (cache-entry-reusable? tags* tags)
		  (fill-cache-if-possible new-cache tags* value))))
	   (try-next-tag-index
	    (lambda ()
	      (let ((index
		     (next-dispatch-tag-index (cache-tag-index new-cache))))
		(and index
		     (begin
		       (set-cache-tag-index! new-cache index)
		       (fill-lines 0)))))))
	(fill-lines 0)))))

(define (expand-cache cache tags value)
  ;; Create a new cache that is twice the length of CACHE, rehash the
  ;; contents of CACHE into the new cache, and make the new entry.
  ;; Permits overflows to occur in the new cache.
  (let ((length (cache-length cache)))
    (letrec
	((fill-lines
	  (lambda (new-cache line)
	    (if (fix:= line length)
		(fill-overflow new-cache (cache-overflow cache))
		(fill-lines (maybe-do-fill new-cache
					   (cache-line-tags cache line)
					   (cache-line-value cache line))
			    (fix:+ line 1)))))
	 (fill-overflow
	  (lambda (new-cache overflow)
	    (if (null? overflow)
		(do-fill new-cache tags value)
		(fill-overflow (maybe-do-fill new-cache
					      (caar overflow)
					      (cdar overflow))
			       (cdr overflow)))))
	 (maybe-do-fill
	  (lambda (cache tags* value)
	    (if (cache-entry-reusable? tags* tags)
		cache
		(do-fill cache tags* value))))
	 (do-fill
	  (lambda (cache tags value)
	    (let ((primary (compute-primary-cache-line cache tags)))
	      (if primary
		  (let ((free (find-free-cache-line cache primary tags)))
		    (if free
			(begin
			  (set-cache-line-tags! cache free tags)
			  (set-cache-line-value! cache free value)
			  cache)
			(or (adjust-cache cache tags value)
			    (begin
			      (set-cache-overflow!
			       cache
			       (cons (cons (cache-line-tags cache primary)
					   (cache-line-value cache primary))
				     (cache-overflow cache)))
			      (set-cache-line-tags! cache primary tags)
			      (set-cache-line-value! cache primary value)
			      cache))))
		  cache)))))
      (fill-lines (make-cache (cache-tag-index cache)
			      (cache-n-tags cache)
			      (fix:+ length length))
		  0))))

(define (find-free-cache-line cache primary tags)
  ;; This procedure searches CACHE for a free line to hold an entry
  ;; with the given PRIMARY cache number and TAGS.  Since the entry
  ;; can only be stored within (CACHE-LIMIT CACHE) lines of PRIMARY,
  ;; we either have to find a free line within that limit, or we have
  ;; to find a line with a larger primary which can be displaced to
  ;; another free line within *its* limit.
  (if (cache-entry-reusable? (cache-line-tags cache primary) tags)
      primary
      (let ((limit (cache-limit cache)))
	;; Find a line for an entry whose primary cache number is P.
	;; LINES is the sequence of entries that is waiting to be
	;; displaced into the line if we find it.
	(let pri-loop
	    ((line (cache-next-line cache primary))
	     (p primary)
	     (tags tags)
	     (lines '()))
	  (let sec-loop
	      ((line line)
	       (nsep (cache-line-separation cache p line)))
	    (cond ((fix:= line primary)
		   ;; We've scanned through the entire cache without
		   ;; finding a usable line.
		   #f)
		  ((let ((tags* (cache-line-tags cache line)))
		     (and (not (cache-entry-reusable? tags* tags))
			  (compute-primary-cache-line cache tags*)))
		   =>
		   (lambda (lp)
		     (let ((osep (cache-line-separation cache lp line)))
		       (cond ((fix:>= osep limit)
			      ;; This line contains an entry that is
			      ;; displaced to the limit.  [**** For
			      ;; some reason I don't understand, this
			      ;; terminates the search.]
			      #f)
			     ((or (fix:> nsep osep)
				  (and (fix:= nsep osep)
				       (= 0 (random 2))))
			      ;; The entry we're trying to place is
			      ;; further from its primary than the
			      ;; entry currently stored in this line.
			      ;; So now let's look for somewhere to
			      ;; displace the entry in this line.
			      (pri-loop (cache-next-line cache line)
					lp
					(cache-line-tags cache line)
					(cons line lines)))
			     (else
			      (sec-loop (cache-next-line cache line)
					(fix:+ nsep 1)))))))
		  (else
		   ;; Found a free line.  First perform all of the
		   ;; entry displacements, then return the subsequent
		   ;; free line.
		   (without-interrupts
		    (lambda ()
		      (let loop ((free-line line) (lines lines))
			(if (null? lines)
			    (begin
			      (set-cache-line-tags! cache free-line #f)
			      (set-cache-line-value! cache free-line #f)
			      free-line)
			    (let ((line (car lines)))
			      (set-cache-line-tags!
			       cache
			       free-line
			       (cache-line-tags cache line))
			      (set-cache-line-value!
			       cache
			       free-line
			       (cache-line-value cache line))
			      (loop line (cdr lines))))))))))))))

(define (purge-cache-entries cache predicate)
  (if (there-exists-a-cache-entry? cache predicate)
      ;; Must rebuild cache since deletions are near-impossible.
      (let loop
	  ((cache (new-cache (cache-n-tags cache)))
	   (alist (cache->alist cache)))
	(if (null? alist)
	    cache
	    (loop (if (predicate (caar alist))
		      cache
		      (fill-cache cache (caar alist) (cdar alist)))
		  (cdr alist))))
      cache))

(define (there-exists-a-cache-entry? cache predicate)
  (let ((length (cache-length cache)))
    (let loop ((line 0))
      (and (not (fix:= line length))
	   (let ((tags (cache-line-tags cache line)))
	     (if (or (not tags)
		     (not (system-pair-car tags)))
		 (loop (fix:+ line 1))
		 (or (predicate (weak-list->list tags))
		     (loop (fix:+ line 1)))))))))

(define (cache->alist cache)
  (let ((length (cache-length cache)))
    (do ((line 0 (fix:+ line 1))
	 (alist '()
		(let ((tags (cache-line-tags cache line)))
		  (if (or (not tags)
			  (not (system-pair-car tags)))
		      alist
		      (cons (cons (weak-list->list tags)
				  (cache-line-value cache line))
			    alist)))))
	((fix:= line length) alist))))