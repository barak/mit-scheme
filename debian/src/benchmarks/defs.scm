(declare (usual-integrations))

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (tak x y z)
  (if (not (< y x)) z
      (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))

(define (nqueens n)
  (define (ok? row dist placed)
    (or (null? placed)
        (and (not (= (car placed) (+ row dist)))
             (not (= (car placed) (- row dist)))
             (ok? row (+ dist 1) (cdr placed)))))
  (define (try x y z)
    (if (null? x)
        (if (null? y) 1 0)
        (+ (if (ok? (car x) 1 z) (try (append (cdr x) y) '() (cons (car x) z)) 0)
           (try (cdr x) (cons (car x) y) z))))
  (try (iota n 1) '() '()))

(define (bignum-work n)
  (let loop ((i 1) (a 1)) (if (> i n) (remainder a 1000000007) (loop (+ i 1) (* a i)))))

(define (flonum-work n)
  (let loop ((i 0) (a 0.)) (if (= i n) a (loop (+ i 1) (+ a (* 1.0000001 (exact->inexact i)))))))

(define (string-work n)
  (let loop ((i 0) (acc 0))
    (if (= i n) acc
        (loop (+ i 1) (+ acc (string-length (string-upcase (number->string i 16))))))))

(define (list-work n)
  (length (sort (map (lambda (x) (remainder (* x 7919) 10007)) (iota n)) <)))

(define (vector-work n)
  (let ((v (make-vector n 0)))
    (do ((i 0 (+ i 1))) ((= i n)) (vector-set! v i (* i i)))
    (do ((i 0 (+ i 1)) (s 0 (+ s (vector-ref v i)))) ((= i n) s))))

(define (alloc-work n)
  (let loop ((i 0) (acc 0))
    (if (= i n) acc
        (loop (+ i 1) (+ acc (car (list i i i i)))))))

(define (repeat n thunk)
  (let loop ((i 0) (v #f)) (if (= i n) v (loop (+ i 1) (thunk)))))
