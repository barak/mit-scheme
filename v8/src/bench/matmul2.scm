;;
;;  Matrix multiply using matrices represented as vectors of vectors
;;  matmul1 - float matrix

(declare (usual-integrations))

(define (make-initialized-vector length initialization)
  (let ((vector (make-vector length)))
    (let loop ((index 0))
      (if (< index length)
	  (begin
	    (vector-set! vector index (initialization index))
	    (loop (1+ index)))))
    vector))

(define (make-identity-matrix n)
  (make-initialized-vector n
    (lambda (i)
      (make-initialized-vector n
	(lambda (j) (if (= i j) 1.0 0.0))))))

(define (matmul-1 m1 m2)
  (let ((p  (vector-length  m1))
	(q1 (vector-length  (vector-ref m1 0)))
	(q2 (vector-length  m2))
	(r  (vector-length  (vector-ref m2 0))))
    (if (not (= q1 q2))
	(error "size mismatch" p q1 q2 r))

    (make-initialized-vector p
      (lambda (i)
	(make-initialized-vector r
	  (lambda (k)
	    (let loop ((sum 0) (j 0))
	      (if (< j q2)
		  (loop (+ (* (vector-ref (vector-ref m1 i) j)
			      (vector-ref (vector-ref m2 j) k))
			   sum)
			(+ j 1))
		  sum))))))))

(define (test1 n)
  (let ((id  (make-identity-matrix n)))
    (matmul-1 id id)))

(lambda () (test1 100) #T)