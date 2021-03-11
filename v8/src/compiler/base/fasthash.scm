#| -*-Scheme-*-

Copyright (c) 1995-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Fast implementaion of hash tables.
;;; Package: (compiler)

(declare (usual-integrations))

;; This implementation is not thread-safe.  Do not share these
;; hash-tables between concurrent threads.  These tables are strong in
;; the sense that they prevent their keys from being garbage
;; collected.  Tables are re-hashed on demand.
;;
;;   (make-monotonic-strong-eq-hash-table)
;;   (monotonic-strong-eq-hash-table/put! table key value)
;;     Returns #T if the key is new in TABLE, #F is pre-existing
;;   (monotonic-strong-eq-hash-table/get table key default)
;;   (monotonic-strong-eq-hash-table/for-every table procedure)
;;   (monotonic-strong-eq-hash-table/copy table)

(define-structure (table
		   (conc-name table/))
  ;; either #F, #T (rehash because of GC), or the old vector (rehash
  ;; because of growth (or growth and GC)).
  rehash?
  vector
  count)

;; States:
;;  rehash?  vector     Notes
;;    #F       v1       Valid. v1 may be updated
;;    #T       v1       Invalid. v1 must not be updated.
;;    v1       v2       Invalid. v1 must be rehashed into v2.
;;                      v1 must not be updated. v2 contains only empty slots

(define tables)

(define-integrable empty-slot #F)

(define-integrable (eq-hash-mask key mask)
  (let ((key key))
    (fix:and
     (fix:* #b101101011
	    (fix:+ (fix:lsh (object-datum key) -9)
		   (fix:and (object-datum key) mask)))
     mask)))

(define (table/grow! table)
  (let* ((old      (table/vector table))
	 (old-len  (vector-length old))
	 (new-len  (fix:* 2 old-len))
	 (new      (make-vector new-len empty-slot)))
    (set-table/rehash?! table old)
    (set-table/vector! table new)))

(define (make-monotonic-strong-eq-hash-table)
  (let ((hash-table
	 (make-table #F
		     (let ((e empty-slot))
		       (vector e e e e e e e e))
		     0)))
    (set-cdr! tables (weak-cons hash-table (cdr tables)))
    ;; Table is valid even if a GC happened during its allocation.
    hash-table))

(define (monotonic-strong-eq-hash-table/copy table)
  ;;(if (table/rehash? table)		; rehash first rather than
  ;;    (table/rehash! table))		; rehashing both copies later.
  (let ((vector* (vector-copy (table/vector table))))
    (let ((rehash?  (table/rehash? table))
	  (count    (table/count table)))
      (let ((table* (make-table rehash? vector* count)))
	(set-cdr! tables (weak-cons table* (cdr tables)))
	;; Now we may have GC-ed between accessing REHASH? and this line, in
        ;; which case we require a rehash.
	(if (and (table/rehash? table)
		 (not (table/rehash? table*)))
	    (set-table/rehash?! table* #T))
	table*))))

(define (monotonic-strong-eq-hash-table/put! table key datum)

  (define (retry)
    (table/rehash! table)
    (monotonic-strong-eq-hash-table/put! table key datum))

  (let* ((v      (table/vector table))
	 (len    (vector-length v))
	 (mask   (fix:- len 2))		;#b00...0011...110
	 (start  (eq-hash-mask key mask)))
    (let search ((i  start))
      (cond  ((eq? (vector-ref v i) key)
	      ;; Assumption: There will be no interrupt checks between the above
              ;; vector-ref and the following vector-set!.  If the table needs
	      ;; rehashing then we were *very* lucky to find the element, but
	      ;; that is OK.
	      (vector-set! v (fix:+ i 1) datum)
	      #F)
	     ((eq? (vector-ref v i) empty-slot)
	      ;; Assumption: There will be no interrupt checks between the
	      ;; above vector-ref and the following vector-set!s
	      (if (table/rehash? table)
		  (retry)		; KEY might be somewhere else
		  (begin
		    (vector-set! v i key)
		    (vector-set! v (fix:+ i 1) datum)
		    (set-table/count! table (fix:+ (table/count table) 1))
		    ;; We must ensure that the table is NEVER full
		    (if (fix:> (fix:* 3 (table/count table)) len)
			(table/grow! table))
		    #T)))
	     (else
	      (search (fix:and mask (fix:+ i 2))))))))


(define (monotonic-strong-eq-hash-table/get table key default)

  (define-integrable (retry)
    (table/rehash! table)
    (monotonic-strong-eq-hash-table/get table key default))
    
  (let* ((v      (table/vector table))
	 (len    (vector-length v))
	 (mask   (fix:- len 2))		; #b00...0011...110
	 (start  (eq-hash-mask key mask)))
    (let search ((i  start))
      (cond  ((eq? (vector-ref v i) key)
	      (vector-ref v (fix:+ i 1)))
	     ((eq? (vector-ref v i) empty-slot)
	      (if (table/rehash? table)
		  (retry)		; KEY might be somewhere else
		  default))
	     (else
	      (search (fix:and mask (fix:+ i 2))))))))

(define (monotonic-strong-eq-hash-table/for-every table procedure)
  ;; Do not touch the table in any way (put or get) during this operation.
  (let ((v  (if (vector? (table/rehash? table))
		(table/rehash? table)
		(table/vector table))))
    (let loop ((i (- (vector-length v) 2)))
      (cond ((fix:< i 0)
	     unspecific)
	    ((eq? (vector-ref v i) empty-slot)
	     (loop (fix:- i 2)))
	    (else
	     (procedure (vector-ref v i) (vector-ref v (+ i 1)))
	     (loop (fix:- i 2)))))))


(define (monotonic-strong-eq-hash-table->alist table)
  (let ((alist '()))
    (monotonic-strong-eq-hash-table/for-every
     table
     (lambda (key value)
       (set! alist (cons (cons key value) alist))
       unspecific))
    alist))


(define (table/rehash! table)

  (define (rehash-copy old old-len new new-len)
    (let ((mask (fix:- new-len 2)))
      (let loop ((old-i (fix:- old-len 2)))
	(if (fix:>= old-i 0)
	    (let ((key  (vector-ref old old-i)))
	      (let search ((new-i  (eq-hash-mask key mask)))
		(cond ((eq? (vector-ref new new-i) empty-slot)
		       (vector-set! new new-i key)
		       (vector-set! new (fix:+ new-i 1)
				    (vector-ref old (fix:+ old-i 1)))
		       (loop (fix:- old-i 2)))
		      (else
		       (search (fix:and mask (fix:+ new-i 2)))))))))))

  (if (vector? (table/rehash? table))
      (let ((old (table/rehash? table))
	    (new (table/vector table)))
	(set-table/rehash?! table false)
	(rehash-copy old (vector-length old) new (vector-length new)))
      (let* ((vec  (table/vector table))
	     (len  (vector-length vec))
	     (new  (make-vector len empty-slot)))
	(set-table/rehash?! table #F)
	(set-table/vector! table new)
	(rehash-copy vec len new len))))

(define (mark-tables!)
  (let loop ((tables tables))
    (let ((wp (system-pair-cdr tables)))
      (cond ((null? wp)
	     unspecific)
	    ((system-pair-car wp)
	     => (lambda (table)
		  (if (not (table/rehash? table))
		      (set-table/rehash?! table #T))
		  (loop wp)))
	    (else
	     ;; discard weak pair
	     (system-pair-set-cdr! tables (system-pair-cdr wp))
	     (loop tables))))))

(define (initialize-package!)
  (set! tables (cons 'HEAD '()))
  ;;((access add-primitive-gc-daemon! (->environment '(runtime gc-daemons)))
  ;; mark-tables!)
  (add-gc-daemon! mark-tables!)
)

(initialize-package!)

