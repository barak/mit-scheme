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

;;;; Simple trie implementation
;;; package: (runtime trie)

(declare (usual-integrations))

(define-record-type <trie>
    (%make-trie =? value edge-alist)
    trie?
  (=? trie-=?)
  (value %trie-value set-trie-value!)
  (edge-alist %trie-edge-alist %set-trie-edge-alist!))

(define (make-trie #!optional =?)
  (%make-trie (if (default-object? =?) equal? =?)
              the-unset-value
              '()))

(define (alist->trie alist #!optional =?)
  (guarantee alist? alist 'alist->trie)
  (let ((trie (make-trie =?)))
    (for-each (lambda (p)
                (trie-set! trie (car p) (cdr p)))
              alist)
    trie))

(define (trie-has-value? trie)
  (not (eq? the-unset-value (%trie-value trie))))

(define (trie-value trie)
  (let ((value (%trie-value trie)))
    (if (eq? the-unset-value value)
        (error:bad-range-argument trie 'trie-value))
    value))

(define (delete-trie-value! trie)
  (set-trie-value! trie the-unset-value))

(define the-unset-value
  (list 'the-unset-value))

(define-print-method trie?
  (standard-print-method 'trie
    (lambda (trie)
      (if (trie-has-value? trie)
          (list (trie-value trie))
          '()))))

(define (%get-child trie key succeed fail)
  (let ((p (assoc key (%trie-edge-alist trie) (trie-=? trie))))
    (if p
	(succeed (cdr p))
	(fail))))

(define (%add-edge! trie key trie*)
  (%set-trie-edge-alist! trie
			 (cons (cons key trie*)
			       (%trie-edge-alist trie))))

(define (trie-edge-find predicate trie)
  (let loop ((this (%trie-edge-alist trie)))
    (and (pair? this)
	 (or (predicate (caar this) (cdar this))
	     (loop (cdr this))))))

(define (trie-edge-fold kons knil trie)
  (fold (lambda (p acc)
	  (kons (car p) (cdr p) acc))
	knil
	(%trie-edge-alist trie)))

(define (trie-edge-for-each procedure trie)
  (for-each (lambda (p)
	      (procedure (car p) (cdr p)))
	    (%trie-edge-alist trie)))

(define (trie-edge-prune! predicate trie)
  (%set-trie-edge-alist! trie
			 (remove! (lambda (p)
				    (predicate (car p) (cdr p)))
				  (%trie-edge-alist trie))))

(define (find-subtrie trie path)
  (let loop ((path path) (trie trie))
    (if (null-list? path 'find-subtrie)
        trie
	(%get-child trie (car path)
	  (lambda (trie*)
	    (loop (cdr path) trie*))
	  (lambda () #f)))))

(define (intern-subtrie! trie path)
  (let loop ((path path) (trie trie))
    (if (null-list? path 'intern-subtrie!)
        trie
	(loop (cdr path)
	      (let ((key (car path)))
		(%get-child trie key
		  (lambda (trie*)
		    trie*)
		  (lambda ()
		    (let ((trie* (make-trie (trie-=? trie))))
		      (%add-edge! trie key trie*)
		      trie*))))))))

(define (trie-ref trie path #!optional fail)
  (let ((trie* (find-subtrie trie path)))
    (if (and trie* (trie-has-value? trie*))
	(trie-value trie*)
	(begin
	  (if (default-object? fail)
	      (error:bad-range-argument path 'trie-ref))
	  (fail)))))

(define (trie-set! trie path value)
  (set-trie-value! (intern-subtrie! trie path) value))

(define (trie-fold kons knil trie)
  (let loop ((path '()) (trie trie) (acc knil))
    (trie-edge-fold (lambda (key trie* acc)
		      (loop (cons key path) trie* acc))
		    (if (trie-has-value? trie)
			(kons (reverse path) (trie-value trie) acc)
			acc)
		    trie)))

(define (trie-for-each procedure trie)
  (let loop ((path '()) (trie trie))
    (if (trie-has-value? trie)
	(procedure (reverse path) (trie-value trie)))
    (trie-edge-for-each (lambda (key trie*)
			  (loop (cons key path) trie*))
			trie)))

(define (trie-clean! trie)
  (let loop ((path '()) (trie trie))
    (trie-edge-prune! (lambda (key trie*)
			(loop (cons key path) trie*)
			(and (not (trie-has-value? trie*))
			     (null? (%trie-edge-alist trie*))))
		      trie)))

(define (trie-clear! trie)
  (delete-trie-value! trie)
  (%set-trie-edge-alist! trie '()))

(define (trie-paths trie)
  (trie-fold (lambda (path value paths)
	       (declare (ignore value))
	       (cons path paths))
	     '()
	     trie))

(define (trie-values trie)
  ;; Not using trie-fold to avoid wasted calls to reverse.
  (let loop ((path '()) (trie trie) (vs '()))
    (trie-edge-fold (lambda (key trie* vs)
		      (loop (cons key path) trie* vs))
		    (if (trie-has-value? trie)
			(cons (trie-value trie) vs)
			vs)
		    trie)))

(define (trie->alist trie)
  (trie-fold alist-cons '() trie))