(declare (usual-integrations))

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(lambda () (fib 30))
