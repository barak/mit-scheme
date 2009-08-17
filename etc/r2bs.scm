;; $Id: cf092b89d448e21b68361263372c0141820919aa $

(define (real->bit-string x)
  ;; Allocate a 64-bit result to hold the double-precision number.
  (let ((result (bit-string-allocate 64)))
    (read-bits!
     ;; Guarantee that the number is floating-point.
     (exact->inexact x)
     ;; Skip over the non-marked vector header (32 bits).
     32
     result)
    result))

(define (bit-string->flonum bs)
  ;; Allocate a flonum that we can clobber.
  ;; The call to `random' prevents the compiler from constant folding.
  (let ((flonum (exact->inexact (random 1))))
    (write-bits! flonum 32 bs)
    flonum))
