#| -*-Scheme-*-

$Id: stats.scm,v 1.5 1995/09/05 18:01:24 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Compiler Statistics
;; package: (compiler)

(declare (usual-integrations))

;; Statistics gathering machinery
;;
;; (compiler:reset-statistics!)
;; (compiler:display-statistics)
;; (sample/1 '(name type [parameters]) datum)
;; (sample/2 '(name type [parameters]) datum1 datum2)


;; sample/1 and sample/2 are defined like this in utils.scm so as to be
;; integrated
;;
;;(define-integrable (sample/1 statistic datum)
;;  (if compiler:enable-statistics?
;;      (sample/1/really statistic datum)
;;      unspecific))
;;
;;(define-integrable (sample/2 statistic datum1 datum2)
;;  (if compiler:enable-statistics?
;;      (sample/2/really statistic datum1 datum2)
;;      unspecific))


(define *compiler-statistics* (make-eq-hash-table))

(define (compiler:reset-statistics!)
  (set! *compiler-statistics* (make-eq-hash-table))
  unspecific)

(define-structure
    (compiler-statistic
     (conc-name compiler-statistic/))
  sample
  display)

(define compiler-statistics-flonum-unparser-cutoff '(RELATIVE 5))

(define (compiler:display-statistics)
  (newline)
  (display "Compiler statistics currently ")
  (display (if compiler:enable-statistics? "enabled" "disabled"))
  (display "  (switch is compiler:enable-statistics?)")
  (fluid-let ((flonum-unparser-cutoff
	       compiler-statistics-flonum-unparser-cutoff))
    (for-each (lambda (name.statistic)
		(newline) (newline)
		((compiler-statistic/display (cdr name.statistic))))
	      (sort
	       (hash-table->alist *compiler-statistics*)
	       (lambda (pair1 pair2)
		 (symbol<? (car pair1) (car pair2)))))))

(define (sample statistic . data)
  (if compiler:enable-statistics?
      (cond ((find-statistic statistic)
	     => (lambda (stat)
		  (apply (compiler-statistic/sample stat) data)
		  data))
	    (else
	     (define-compiler-statistic (length data) statistic)
	     (warn "SAMPLE should be replaced with call to SAMPLE/1 or SAMPLE/2"
		   `(sample ,statistic ...))
	     (apply sample/1/really statistic data)))))

(define (sample/1/really statistic datum)
  (cond ((find-statistic statistic)
	 => (lambda (stat)
	      ((compiler-statistic/sample stat) (dethunk datum))))
	(else
	 (define-compiler-statistic 1 statistic)
	 (sample/1/really statistic datum))))

(define (sample/2/really statistic datum1 datum2)
  (cond ((find-statistic statistic)
	 => (lambda (stat)
	      ((compiler-statistic/sample stat)
	       (dethunk datum1)
	       (dethunk datum2))))
	(else
	 (define-compiler-statistic 2 statistic)
	 (sample/2/really statistic datum1 datum2))))


(define (dethunk possible-thunk)
  (let ((possible-thunk possible-thunk))
    (if (promise? possible-thunk)
	(force possible-thunk)
	possible-thunk)))

(define-integrable (find-statistic specification)
  (if (pair? specification)
      (hash-table/get *compiler-statistics* (car specification) #F)
      #F))

(define (define-compiler-statistic arity specification)
  arity					; ignored
  (if (not (and (pair? specification)
		(pair? (cdr specification))))
      (error "Illegal compiler-statistic specification:" specification))
  (let ((name (first specification))
	(type (second specification)))
    (cond ((hash-table/get *compiler-statistic-types* type #F)
	   => (lambda (maker)
		(let ((statistic  (apply maker specification)))
		  (hash-table/put! *compiler-statistics* name statistic))))
	  (else
	   (error "Unknown compiler-statistic type:" type specification)))))

(define *compiler-statistic-types* (make-eq-hash-table))

(define (define-statistic-type name maker)
  (hash-table/put! *compiler-statistic-types* name maker))


(define-statistic-type 'COUNT
  (lambda (name type)
    (let ((count 0))
      (define (sample datum)
	(set! count (+ count datum))
	unspecific)
      (define (print)
	(define (say . stuff) (for-each display stuff))
	(say name  " " type "   "  count))
      (make-compiler-statistic sample print))))


(define-statistic-type 'AVERAGE
  (lambda (name type)
    (let ((n   0)
	  (nmax #F) (nmin #F)
	  (sum 0))
      (define (sample datum)
	(set! n (+ n 1))
	(set! sum (+ sum datum))
	(if nmax
	    (begin
	      (set! nmax (max nmax datum))
	      (set! nmin (min nmin datum)))
	    (begin
	      (set! nmax datum)
	      (set! nmin datum)))
	unspecific)
      (define (print)
	(define (say . stuff) (for-each display stuff))
	(say name  " " type "   ")
	(say " n "  n  "   sum "  sum)
	(say "   mean " (/ sum n))
	(say "   min " nmin "   max " nmax))
      (make-compiler-statistic sample print))))

(define-statistic-type 'HISTOGRAM
  (lambda (name type #!optional method)

    (define (key<? u v)
      (cond ((and (number? u) (number? v))
	     (< u v))
	    ((number? u) #T)
	    ((number? v) #F)
	    ((< (object-type u) (object-type v))
	     #T)
	    ((> (object-type u) (object-type v))
	     #F)
	    ((and (symbol? u) (symbol? v))
	     (symbol<? u v))
	    ((and (string? u) (string? v))
	     (string<? u v))
	    ((and (primitive-procedure? u) (primitive-procedure? v))
	     (key<? (primitive-procedure-name u) (primitive-procedure-name v)))
	    (else '(DONT KNOW))))

    (define (pp-alist alist)
      (define (say . stuff) (for-each display stuff))
      (say name  " "  type  "   total: "  (reduce + 0 (map cdr alist))
	   "   (datum . count):")
      (newline)
      (pp alist (current-output-port) #F 4))

    (define (make-hash-table-histogram)
      (let ((samples  (make-eqv-hash-table)))
	(define (sample datum)
	  (hash-table/put! samples datum
			   (+ (hash-table/get samples datum 0) 1)))
	(define (print)
	  (pp-alist (sort (hash-table->alist samples)
			  (lambda (u v) (key<? (car u) (car v))))))
	(make-compiler-statistic sample print)))
    
    (define (make-vector-histogram)
      (let ((samples  (vector)))
	(define (sample datum)
	  (if (>= datum (vector-length samples))
	      (set! samples (vector-grow samples (+ 1 datum))))
	  (vector-set! samples datum
		       (+ 1 (or (vector-ref samples datum) 0))))
	(define (print)
	  (pp-alist (list-transform-positive
			(vector->list
			 (make-initialized-vector (vector-length samples)
			   (lambda (i) (cons i (vector-ref samples i)))))
		      cdr)))
	(make-compiler-statistic sample print)))
    
    (let ((method (if (default-object? method) 'HASH-TABLE method)))
      (case method
	((HASH-TABLE HASH)  (make-hash-table-histogram))
	((VECTOR)           (make-vector-histogram))
	(else
	 (error "Unknown histogram method:" method `(,name ,type ,method)))))))
