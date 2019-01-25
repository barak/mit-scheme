; Test for INVOCATION-PREFIX:DYNAMIC-LINK with a large frame.

(declare (usual-integrations))

(let ()
  (define (filter-map-if p f l on-tail)
    (let loop
	((l l) (a 0) (b 1) (c 2) (u 3) (v 4) (w 5) (n 6))
      (if (pair? l)
	  (let ((x (car l)))
	    (if (p x)
		(let ((y (f x)))
		  (if y
		      (cons y (loop (cdr l) b c u v w n a))
		      (loop (cdr l) b c u v w n a)))
		(loop (cdr l) b c u v w n a)))
	  (on-tail '() a b c u v w n))))
  (let ((result
	 ((identity filter-map-if)
	  (lambda (x)
	    (fix:zero? (fix:remainder x 2)))
	  (lambda (x)
	    (and (fix:zero? (fix:remainder x 3))
		 (fix:quotient x 6)))
	  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
	  (lambda (tail a b c u v w n)
	    a b c u v w n
	    tail))))
    ((ucode-primitive exit-with-value 1)
     (fix:or (fix:- (car result) 1)
	     (fix:- (car (cdr result)) 2)))))
