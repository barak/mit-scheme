#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Trie amap type
;;; package: (runtime amap trie)

(declare (usual-integrations))

(add-boot-deps! '(runtime trie)
		'(runtime amap impl))

(add-boot-init!
 (lambda ()
   (define-amap-impl 'trie
     '((mutability mutable)
       (kv-types (strong strong)))
     uniform-list-comparator?
     `((->alist ,trie->alist)
       (clean! ,trie-clean!)
       (clear! ,trie-clear!)
       (contains? ,trie:contains?)
       (delete-1! ,trie:delete-1!)
       (empty-copy ,trie:empty-copy)
       (find ,trie:find)
       (fold ,trie-fold)
       (for-each ,trie-for-each)
       (intern! ,trie:intern!)
       (keys ,trie-paths)
       (map! ,trie:map!)
       (new-state ,trie:new-state)
       (prune! ,trie:prune!)
       (ref ,trie:ref)
       (ref/default ,trie:ref/default)
       (set-1! ,trie-set!)
       (size ,trie:size)
       (update! ,trie:update!)
       (update!/default ,trie:update!/default)
       (values ,trie-values)))))

(define (trie:new-state comparator args)
  (declare (ignore args))
  (make-trie
   (comparator-equality-predicate
    (uniform-list-comparator-elt comparator))))

(define (trie:contains? trie path)
  (let ((trie* (find-subtrie trie path)))
    (and trie*
	 (trie-has-value? trie*))))

(define (trie:delete-1! trie path)
  (let ((trie* (find-subtrie trie path)))
    (and trie*
	 (trie-has-value? trie*)
	 (begin
	   (delete-trie-value! trie*)
	   #t))))

(define (trie:empty-copy trie)
  (make-trie (trie-=? trie)))

(define (trie:ref trie path fail succeed)
  (%subtrie-ref (find-subtrie trie path) path fail succeed 'amap-ref))

(define (%subtrie-ref trie path fail succeed caller)
  (if (and trie (trie-has-value? trie))
      (if (default-object? succeed)
	  (trie-value trie)
	  (succeed (trie-value trie)))
      (begin
	(if (default-object? fail)
	    (error:bad-range-argument path caller))
	(fail))))

(define (trie:ref/default trie path default)
  (%subtrie-ref/default (find-subtrie trie path) default))

(define (%subtrie-ref/default trie default)
  (if (and trie (trie-has-value? trie))
      (trie-value trie)
      default))

(define (trie:update! trie path proc fail succeed)
  (let ((trie* (intern-subtrie! trie path)))
    (set-trie-value! trie*
      (proc (%subtrie-ref trie* path fail succeed 'amap-update!)))))

(define (trie:update!/default trie path proc default)
  (let ((trie* (intern-subtrie! trie path)))
    (set-trie-value! trie*
      (proc (%subtrie-ref/default trie* default)))))

(define (trie:intern! trie path fail)
  (let ((trie* (intern-subtrie! trie path)))
    (if (trie-has-value? trie*)
	(trie-value trie*)
	(let ((value (fail)))
	  (set-trie-value! trie* value)
	  value))))

(define (trie:find predicate trie fail)
  (or (let loop ((path '()) (trie trie))
	(or (and (trie-has-value? trie)
		 (predicate (reverse path) (trie-value trie)))
	    (trie-edge-find (lambda (key trie*)
			      (loop (cons key path) trie*))
			    trie)))
      (fail)))

(define (trie:map! procedure trie)
  (let loop ((path '()) (trie trie))
    (if (trie-has-value? trie)
	(set-trie-value! trie
			 (procedure (reverse path) (trie-value trie))))
    (trie-edge-for-each (lambda (key trie*)
			  (loop (cons key path) trie*))
			trie)))

(define (trie:prune! predicate trie)
  (let loop ((path '()) (trie trie))
    (if (and (trie-has-value? trie)
	     (predicate (reverse path) (trie-value trie)))
	(delete-trie-value! trie))
    (trie-edge-for-each (lambda (key trie*)
			  (loop (cons key path) trie*))
			trie)))

(define (trie:size trie)
  (let loop ((trie trie) (size 0))
    (trie-edge-fold (lambda (key trie* size)
		      (declare (ignore key))
		      (loop trie* size))
		    (if (trie-has-value? trie) (+ size 1) size)
		    trie)))