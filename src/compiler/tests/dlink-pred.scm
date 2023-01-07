; Test for predicate continuations with dynamic links.

(declare (usual-integrations))

(let ()
  (declare (no-type-checks))
  (define any
    (identity
     (let ()
       (define (any pred list)
	 (let lp ((list list))
	   (or (null? list)
	       (pred (car list))
	       (lp (cdr list)))))
       any)))
  (define (rexists pred thing)
    (let tlp ((thing thing))
      (cond ((pred thing) #t)
	    ((vector? thing)
	     (let ((n (vector-length thing)))
	       (let lp ((i 0))
		 (cond ((fix:= i n) #f)
		       ((tlp (vector-ref thing i)) #t) ;(*)
		       (else (lp (fix:+ i 1)))))))
	    ((pair? thing)
	     (any tlp thing))
	    (else #f))))
  ((ucode-primitive exit-with-value 1)
   (if ((identity rexists) (lambda (x) (symbol? x)) (vector 1 2))
       123
       0)))