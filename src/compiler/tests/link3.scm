(declare (usual-integrations))

(prelude)

(define x 0)

(define (f) x)

(define (g) x)

((ucode-primitive exit-with-value 1)
 (fix:or (f) (g)))
