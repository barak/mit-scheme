#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; LAP Generator: Merge Register Maps

(declare (usual-integrations))

(define (merge-register-maps maps weights)
  ;; This plays merry hell with the map entry order.  An attempt has
  ;; been made to preserve the order in simple cases, but in general
  ;; there isn't enough information to do a really good job.
  (let ((entries
	 (reduce add-weighted-entries
		 '()
		 (if (not weights)
		     (map (lambda (map) (map->weighted-entries map 1)) maps)
		     (map map->weighted-entries maps weights)))))
    (for-each eliminate-unlikely-aliases! entries)
    (eliminate-conflicting-aliases! entries)
    (weighted-entries->map entries)))

(define (eliminate-unlikely-aliases! entry)
  (let ((home-weight (vector-ref entry 1))
	(alias-weights (vector-ref entry 2)))
    (let ((maximum (max home-weight (apply max (map cdr alias-weights)))))
      (if (not (= home-weight maximum))
	  (vector-set! entry 1 0))
      ;; Keep only the aliases with the maximum weights.  Furthermore,
      ;; keep only one alias of a given type.
      (vector-set! entry 2
		   (list-transform-positive alias-weights
		     (let ((types '()))
		       (lambda (alias-weight)
			 (and (= (cdr alias-weight) maximum)
			      (let ((type (register-type (car alias-weight))))
				(and (not (memq type types))
				     (begin (set! types (cons type types))
					    true)))))))))))

(define (eliminate-conflicting-aliases! entries)
  (for-each (lambda (conflicting-alias)
	      (let ((homes (cdr conflicting-alias)))
		(let ((maximum (apply max (map cdr homes))))
		  (let ((winner
			 (list-search-positive homes
			   (lambda (home)
			     (= (cdr home) maximum)))))
		    (for-each
		     (lambda (home)
		       (if (not (eq? home winner))
			   (let ((entry
				  (find-weighted-entry (car home) entries)))
			     (vector-set! entry 2
					  (del-assv! (car conflicting-alias)
						     (vector-ref entry 2))))))
		     homes)))))
	    (conflicting-aliases entries)))

(define (conflicting-aliases entries)
  (let ((alist '()))
    (for-each
     (lambda (entry)
       (let ((home (vector-ref entry 0)))
	 (for-each
	  (lambda (alias-weight)
	    (let ((alist-entry (assv (car alias-weight) alist))
		  (element (cons home (cdr alias-weight))))
	      (if alist-entry
		  (set-cdr! alist-entry (cons element (cdr alist-entry)))
		  (set! alist
			(cons (list (car alias-weight) element) alist)))))
	  (vector-ref entry 2))))
     entries)
    (list-transform-negative alist
      (lambda (alist-entry)
	(null? (cddr alist-entry))))))

(define (map->weighted-entries register-map weight)
  (map (lambda (entry)
	 (vector (map-entry-home entry)
		 (if (map-entry-saved-into-home? entry) weight 0)
		 (map (lambda (alias) (cons alias weight))
		      (map-entry-aliases entry))
		 (map-entry-label entry)))
       (map-entries register-map)))

(define (add-weighted-entries x-entries y-entries)
  (merge-entries x-entries y-entries
    (lambda (entry entries)
      (list-search-positive entries
	(let ((home (vector-ref entry 0)))
	  (lambda (entry)
	    (eqv? home (vector-ref entry 0))))))
    (lambda (x-entry y-entry)
      (vector (vector-ref x-entry 0)
	      (min (vector-ref x-entry 1) (vector-ref y-entry 1))
	      (merge-entries (vector-ref x-entry 2) (vector-ref y-entry 2)
		(lambda (entry entries)
		  (assq (car entry) entries))
		(lambda (x-entry y-entry)
		  (cons (car x-entry) (+ (cdr x-entry) (cdr y-entry)))))
	      ;; If the labels don't match, or only one entry has a
	      ;; label, then the result shouldn't have a label.
	      (and (eqv? (vector-ref x-entry 3) (vector-ref y-entry 3))
		   (vector-ref x-entry 3))))))

(define (merge-entries x-entries y-entries find-entry merge-entry)
  (let loop
      ((x-entries x-entries)
       (y-entries (list-copy y-entries))
       (result '()))
    (if (null? x-entries)
	;; This (feebly) attempts to preserve the entry order.
	(append! (reverse! result) y-entries)
	(let ((x-entry (car x-entries))
	      (x-entries (cdr x-entries)))
	  (let ((y-entry (find-entry x-entry y-entries)))
	    (if y-entry
		(loop x-entries
		      (delq! y-entry y-entries)
		      (cons (merge-entry x-entry y-entry) result))
		(loop x-entries
		      y-entries
		      (cons x-entry result))))))))

(define find-weighted-entry
  (association-procedure eqv? (lambda (entry) (vector-ref entry 0))))

(define (weighted-entries->map entries)
  (let loop
      ((entries entries)
       (map-entries '())
       (map-registers available-machine-registers))
    (if (null? entries)
	(make-register-map (reverse! map-entries)
			   (sort-machine-registers map-registers))
	(let ((aliases (map car (vector-ref (car entries) 2))))
	  (if (null? aliases)
	      (loop (cdr entries) map-entries map-registers)
	      (loop (cdr entries)
		    (cons (make-map-entry
			   (vector-ref (car entries) 0)
			   (positive? (vector-ref (car entries) 1))
			   aliases
			   (vector-ref (car entries) 3))
			  map-entries)
		    (eqv-set-difference map-registers aliases)))))))