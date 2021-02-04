#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

(declare (usual-integrations))

(define-integrable dispatch-tag-ref %record-ref)
(define-integrable dispatch-tag-index-start 1)
(define-integrable dispatch-tag-index-end 9)

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

(define (resize-cache cache new-length)
  (make-cache (cache-tag-index cache)
	      (cache-n-tags cache)
	      new-length))

(define (make-cache tag-index n-tags length)
  ;; LENGTH is assumed to be a power of two.
  (%make-cache tag-index
	       (fix:- length 1)
	       (cond ((fix:<= length 4) 1)
		     ((fix:<= length 16) 4)
		     (else 6))
	       n-tags
	       (make-vector length (make-weak-list n-tags #f))
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
  (fix:and (fix:+ line 1) (cache-mask cache)))

(define-integrable (cache-line-separation cache line line*)
  (let ((n (fix:- line* line)))
    (if (fix:< n 0)
	(fix:+ n (cache-length cache))
	n)))

(define (probe-cache cache tags)

  (define (match tags*)
    (let loop ((w1 tags*) (w2 tags))
      (or (not (weak-pair? w1))
	  (and (eq? (weak-car w1) (car w2))
	       (loop (weak-cdr w1) (cdr w2))))))

  (%probe-cache cache match
    (let ((index (cache-tag-index cache)))
      (let loop ((tags (cdr tags)) (sum (dispatch-tag-ref (car tags) index)))
	(if (pair? tags)
	    (loop (cdr tags)
		  (fix:+ sum (dispatch-tag-ref (car tags) index)))
	    (fix:and sum (cache-mask cache)))))))

(declare (integrate-operator %probe-cache))
(define (%probe-cache cache match line)
  (declare (integrate cache match))
  (if (match (cache-line-tags cache line))
      (cache-line-value cache line)
      (let ((limit (cache-limit cache)))
	(let loop ((line (cache-next-line cache line)) (i 0))
	  (if (fix:< i limit)
	      (if (match (cache-line-tags cache line))
		  (cache-line-value cache line)
		  (loop (cache-next-line cache line) (fix:+ i 1)))
	      (let ov-loop ((overflow (cache-overflow cache)))
		(and (pair? overflow)
		     (if (match (caar overflow))
			 (cdar overflow)
			 (ov-loop (cdr overflow))))))))))

(define (probe-cache-1 cache w1)

  (define-integrable (match tags)
    (eq? w1 (weak-car tags)))

  (%probe-cache cache match
    (fix:and (dispatch-tag-ref w1 (cache-tag-index cache))
	     (cache-mask cache))))

(define (probe-cache-2 cache w1 w2)

  (define-integrable (match tags)
    (and (eq? w1 (weak-car tags))
	 (eq? w2 (weak-car (weak-cdr tags)))))

  (%probe-cache cache match
    (fix:and (fix:+ (dispatch-tag-ref w1 (cache-tag-index cache))
		    (dispatch-tag-ref w2 (cache-tag-index cache)))
	     (cache-mask cache))))

(define (probe-cache-3 cache w1 w2 w3)

  (define-integrable (match tags)
    (and (eq? w1 (weak-car tags))
	 (eq? w2 (weak-car (weak-cdr tags)))
	 (eq? w3 (weak-car (weak-cdr (weak-cdr tags))))))

  (%probe-cache cache match
    (fix:and (fix:+ (dispatch-tag-ref w1 (cache-tag-index cache))
		    (fix:+ (dispatch-tag-ref w2 (cache-tag-index cache))
			   (dispatch-tag-ref w3 (cache-tag-index cache))))
	     (cache-mask cache))))

(define (probe-cache-4 cache w1 w2 w3 w4)

  (define-integrable (match tags)
    (and (eq? w1 (weak-car tags))
	 (eq? w2 (weak-car (weak-cdr tags)))
	 (eq? w3 (weak-car (weak-cdr (weak-cdr tags))))
	 (eq? w4 (weak-car (weak-cdr (weak-cdr (weak-cdr tags)))))))

  (%probe-cache cache match
    (fix:and (fix:+ (fix:+ (dispatch-tag-ref w1 (cache-tag-index cache))
			   (dispatch-tag-ref w2 (cache-tag-index cache)))
		    (fix:+ (dispatch-tag-ref w3 (cache-tag-index cache))
			   (dispatch-tag-ref w4 (cache-tag-index cache))))
	     (cache-mask cache))))

(define (fill-cache cache tags value)
  ;; TAGS must be converted to a weak list since it will be stored in
  ;; the cache, and we don't want the cache to prevent the tags from
  ;; being GCed.
  (let ((tags (list->weak-list tags)))
    (or (fill-cache-if-possible cache tags value)
	;; If the cache isn't too full, try to rehash without expanding.
	(and (< (cache-count cache) (* (cache-length cache) .8))
	     (rehash-cache cache tags value))
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

(define (compute-primary-cache-line cache tags)
  (let ((index (cache-tag-index cache)))
    (let ((tag (weak-car tags)))
      (if (gc-reclaimed-object? tag)
	  #f
	  (let loop ((tags (weak-cdr tags)) (sum (dispatch-tag-ref tag index)))
	    (if (weak-pair? tags)
		(let ((tag (weak-car tags)))
		  (if (gc-reclaimed-object? tag)
		      #f
		      (loop (weak-cdr tags)
			    (fix:+ sum (dispatch-tag-ref tag index)))))
		(fix:and sum (cache-mask cache))))))))

(define (rehash-cache cache tags value)
  ;; Try to rehash the cache.  If that fails, try rehashing with
  ;; different tag indexes.  Fail only when all of the tag indexes
  ;; have been tried and none has worked.
  (let ((length (cache-length cache)))
    (let ((new-cache (resize-cache cache length)))

      (define (loop line)
	(if (fix:< line length)
	    (and (try-copy (cache-line-tags cache line)
			   (cache-line-value cache line))
		 (loop (fix:+ line 1)))
	    (let ov-loop ((entries (cache-overflow cache)))
	      (if (pair? entries)
		  (and (try-copy (caar entries) (cdar entries))
		       (ov-loop (cdr entries)))
		  (fill-cache-if-possible new-cache tags value)))))

      (define (try-copy tags* value*)
	(or (cache-entry-reusable? tags* tags)
	    (fill-cache-if-possible new-cache tags* value*)))

      (let index-loop ()
	(or (loop 0)
	    (let ((index (fix:+ (cache-tag-index new-cache) 1)))
	      (and (fix:< index dispatch-tag-index-end)
		   (begin
		     (set-cache-tag-index! new-cache index)
		     (index-loop)))))))))

(define (expand-cache cache tags value)
  ;; Create a new cache that is twice the length of CACHE, rehash the
  ;; contents of CACHE into the new cache, and make the new entry.
  ;; Permits overflows to occur in the new cache.
  (fill-cache-with-overflow
   (cache-fold (lambda (tags* value* new-cache)
		      (if (cache-entry-reusable? tags* tags)
			  new-cache
			  (fill-cache-with-overflow new-cache tags* value*)))
		    (resize-cache cache (fix:lsh (cache-length cache) 1))
		    cache)
   tags value))

(define (fill-cache-with-overflow cache tags value)
  (or (fill-cache-if-possible cache tags value)
      (rehash-cache cache tags value)
      (let ((primary (compute-primary-cache-line cache tags)))
	(set-cache-overflow!
	 cache
	 (cons (cons (cache-line-tags cache primary)
		     (cache-line-value cache primary))
	       (cache-overflow cache)))
	(set-cache-line-tags! cache primary tags)
	(set-cache-line-value! cache primary value)
	cache)))

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
		   (without-interruption
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

(define (cache-any predicate cache)
  (let ((length (cache-length cache)))
    (let loop ((line 0))
      (if (fix:< line length)
	  (or (predicate (cache-line-tags cache line)
			 (cache-line-value cache line))
	      (loop (fix:+ line 1)))
	  (let ov-loop ((overflow (cache-overflow cache)))
	    (and (pair? overflow)
		 (or (predicate (caar overflow) (cdar overflow))
		     (ov-loop (cdr overflow)))))))))

(define (cache-fold kons knil cache)
  (let ((length (cache-length cache)))
    (let loop ((line 0) (acc knil))
      (if (fix:< line length)
	  (loop (fix:+ line 1)
		(kons (cache-line-tags cache line)
		      (cache-line-value cache line)
		      acc))
	  (let ov-loop ((overflow (cache-overflow cache)) (acc acc))
	    (if (pair? overflow)
		(ov-loop (cdr overflow)
			 (kons (caar overflow)
			       (cdar overflow)
			       acc))
		acc))))))

(define (cache-count cache)
  (cache-fold (lambda (tags value count)
		(declare (ignore value))
		(if (or (tags-empty? tags)
			(tags-reclaimed? tags))
		    count
		    (fix:+ count 1)))
	      0
	      cache))

;; Used only by SOS.
(define (purge-cache-entries cache predicate)
  (if (cache-any (lambda (tags value)
		   (declare (ignore value))
		   (let ((tags* (copy-tags tags)))
		     (and tags*
			  (predicate tags*))))
		 cache)
      ;; Must rebuild cache since deletions are near-impossible.
      (fold (lambda (p cache)
	      (fill-cache cache (car p) (cdr p)))
	    (new-cache (cache-n-tags cache))
	    (cache-fold (lambda (tags value alist)
			  (let ((tags* (copy-tags tags)))
			    (if (and tags* (not (predicate tags*)))
				(cons (cons tags* value) alist)
				alist)))
			'()
			cache))
      cache))

(define (copy-tags tags)
  (if (tags-empty? tags)
      #f
      (let loop ((tags tags) (copy '()))
	(if (weak-pair? tags)
	    (let ((tag (weak-car tags)))
	      (if (gc-reclaimed-object? tag)
		  #f
		  (loop (weak-cdr tags) (cons tag copy))))
	    (reverse copy)))))

(define (tags-reclaimed? tags)
  (and (weak-pair? tags)
       (or (gc-reclaimed-object? (weak-car tags))
	   (tags-reclaimed? (weak-cdr tags)))))

(define-integrable (tags-empty? tags)
  (not (weak-car tags)))

(define (cache-entry-reusable? tags tags*)
  ;; True iff TAGS is: (1) empty; (2) contains a tag that has been reclaimed; or
  ;; (3) has the same tags as TAGS*.
  (or (tags-empty? tags)
      (let loop ((tags tags) (tags* tags*))
	(or (not (weak-pair? tags))
	    (let ((tag (weak-car tags)))
	      (or (gc-reclaimed-object? tag)
		  (and (eq? tag (weak-car tags*))
		       (loop (weak-cdr tags) (weak-cdr tags*)))))))))