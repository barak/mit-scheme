#| -*-Scheme-*-

Copyright (C) 2013 Taylor R Campbell

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

;;;; IEEE 754 Format

(define (decompose-ieee754-double x)
  (decompose-ieee754-binary x 11 53))

(define (decompose-ieee754-binary x exponent-bits precision)
  (receive (base emin emax bias exp-subnormal exp-inf/nan)
           (ieee754-binary-parameters exponent-bits precision)
    (decompose-ieee754 x base emax precision
      (lambda (sign)                    ;if-zero
        (values sign 0 0))
      (lambda (sign scaled-significand) ;if-subnormal
        (assert (= 0 (shift-right scaled-significand precision)))
        (values sign exp-subnormal scaled-significand))
      (lambda (sign exponent scaled-significand) ;if-normal
        (assert (<= emin exponent emax))
        ;; The integer part is always 1.  Strip it for the binary
        ;; interchange format.
        (assert (= 1 (shift-right scaled-significand (- precision 1))))
        (values sign
                (+ exponent bias)
                (extract-bit-field (- precision 1) 0 scaled-significand)))
      (lambda (sign)                    ;if-infinite
        (values sign exp-inf/nan 0))
      (lambda (sign quiet payload)      ;if-nan
        (assert (not (and (zero? quiet) (zero? payload))))
        (assert (zero? (extract-bit-field (- precision 1) 1 payload)))
        (values sign
                exp-inf/nan
                (replace-bit-field (- precision 1) 1 payload quiet))))))

(define (ieee754-sign x)
  (cond ((< 0 x) 0)
        ((< x 0) 1)
        ;; Zero -- can't use < directly to detect sign.  Elicit a
        ;; computational difference.
        ((negative? (atan x -1)) 1)
        (else 0)))

(define (decompose-ieee754 x base emax precision
          if-zero if-subnormal if-normal if-infinite if-nan)
  (cond ((not (= x x))
         ;; There are, of course, b^p different NaNs.  There is no
         ;; obvious way to computationally detect the sign of a NaN,
         ;; and no portable way to get at the quiet bit or the payload
         ;; bits, so we'll just assume every NaN is a trivial positive
         ;; signalling NaN and hope the caller has a good story...
         (if-nan 0 0 1))
        ((and (< 1. (abs x)) (= x (/ x 2)))
         (if-infinite (if (< 0. x) 0 1)))
        (else
         (let ((sign (ieee754-sign x)) (x (abs x)) (emin (- 1 emax)))
           (define (significand x)
             (truncate->exact (* x (expt base (- precision 1)))))
           (cond ((<= 1 x)              ;Nonnegative exponent (normal)
                  (let loop ((exponent 0) (x x))
                    (cond ((< emax exponent) (if-infinite sign))
                          ((< base x) (loop (+ exponent 1) (/ x base)))
                          (else (if-normal sign exponent (significand x))))))
                 ((< (expt base emin) x) ;Negative exponent, normal
                  (let loop ((exponent 0) (x x))
                    (assert (<= emin exponent))
                    (if (<= 1 x)
                        (if-normal sign exponent (significand x))
                        (loop (- exponent 1) (* x base)))))
                 ((< 0 x)               ;Negative exponent, subnormal
                  (if (<= x (- (expt base emin) (expt base (- 0 precision))))
                      (if-zero sign)
                      (if-subnormal
                       sign
                       (significand (/ x (expt base emin))))))
                 (else
                  (if-zero sign)))))))

(define (compose-ieee754-double sign biased-exponent trailing-significand)
  (compose-ieee754-binary sign biased-exponent trailing-significand 11 53))

(define (compose-ieee754-binary sign biased-exponent trailing-significand
                                exponent-bits precision)
  (receive (base emin emax bias exp-subnormal exp-inf/nan)
           (ieee754-binary-parameters exponent-bits precision)
    (let ((exponent (- biased-exponent bias)))
      (cond ((= exponent exp-subnormal)
             (if (zero? trailing-significand)
                 (compose-ieee754-zero sign base emax precision)
                 (compose-ieee754-subnormal sign trailing-significand
                                            base emax precision)))
            ((= exponent exp-inf/nan)
             (if (zero? trailing-significand)
                 (compose-ieee754-infinity sign base emax precision)
                 (let ((p-1 (- precision 1))
                       (T trailing-significand))
                   (let ((quiet   (extract-bit-field 1 p-1 T))
                         (payload (extract-bit-field p-1 0 T)))
                     (compose-ieee754-nan sign quiet payload
                                          base emax precision)))))
            (else
             (assert (<= emin exponent emax))
             (let ((scaled-significand
                    ;; Add the implied integer part of 1.
                    (replace-bit-field 1 (- precision 1) trailing-significand
                                       1)))
               (compose-ieee754-normal sign exponent scaled-significand
                                       base emax precision)))))))

(define (compose-ieee754-zero sign base emax precision)
  base emax precision                   ;ignore
  (* (expt -1 sign) 0))

(define (compose-ieee754-subnormal sign significand base emax precision)
  (* (expt -1 sign)
     (* significand (expt base (- precision emax)))))

(define (compose-ieee754-normal sign exponent significand base emax precision)
  (assert (<= (- 1 emax) exponent emax))
  (* (expt -1 sign)
     (expt base exponent)
     (/ significand (expt base (- precision 1)))))

(define (compose-ieee754-infinity sign)
  (error "Can't compose an IEEE754 infinity!" sign))

(define (compose-ieee754-nan sign quiet payload)
  (error "Can't compose an IEEE754 NaN!" sign quiet payload))

(define (ieee754-binary-parameters exponent-bits precision)
  (assert (zero? (modulo (+ exponent-bits precision) 32)))
  (let* ((base 2)
         (emax (- (expt base (- exponent-bits 1)) 1)))
    (let ((bias emax)
          (emin (- 1 emax)))
      (let ((exp-subnormal (- emin 1))
            (exp-inf/nan (+ emax 1)))
        (values base emin emax bias exp-subnormal exp-inf/nan)))))

(define (ieee754-double-recomposable? x)
  (= x
     (receive (sign biased-exponent trailing-significand)
              (decompose-ieee754-double x)
       (compose-ieee754-double sign biased-exponent trailing-significand))))

(define (ieee754-binary-hex-string x exponent-bits precision)
  (receive (base emin emax bias exp-subnormal exp-inf/nan)
           (ieee754-binary-parameters exponent-bits precision)
    (define (symbolic sign name extra)
      (assert (or (= sign 0) (= sign 1)))
      (assert (<= 0 extra))
      (let ((extra (number->string extra #x10)))
        (string-append (if (zero? sign) "+" "-") name "." extra)))
    (define (numeric sign integer fractional exponent)
      (assert (or (= sign 0) (= sign 1)))
      (assert (or (= integer 0) (= integer 1)))
      (assert (<= 0 fractional))
      (let ((sign (if (zero? sign) "" "-"))
            (integer (if (zero? integer) "0" "1"))
            (fractional
             (if (zero? fractional) "" (number->string fractional #x10)))
            (exponent (number->string exponent #d10)))
        (string-append sign "0x" integer "." fractional "p" exponent)))
    (decompose-ieee754 x base emax precision
      (lambda (sign)                    ;if-zero
        (numeric sign 0 0 0))
      (lambda (sign scaled-significand) ;if-subnormal
        (assert (< 0 scaled-significand))
        (let ((start (first-set-bit scaled-significand))
              (end (integer-length scaled-significand)))
          (let ((bits (- (- end 1) start)))
            (let ((exponent (- emin (- precision end)))
                  ;; Strip the integer part (1) and the trailing zeros.
                  (fractional
                   (extract-bit-field bits start scaled-significand)))
              ;; Align to a multiple of four bits (hex digits).
              (let* ((alignment (modulo (- 4 (modulo bits 4)) 4))
                     (aligned-fractional
                      (replace-bit-field bits alignment 0 fractional)))
                (numeric sign 1 aligned-fractional exponent))))))
      (lambda (sign exponent scaled-significand)
        (assert (< 0 scaled-significand))
        (assert (= 1 (shift-right scaled-significand (- precision 1))))
        (let ((useless-zeros (round-down (first-set-bit scaled-significand) 4))
              (fractional
               (extract-bit-field (- precision 1) 0 scaled-significand)))
          (numeric sign 1 (shift-right fractional useless-zeros) exponent)))
      (lambda (sign)
        (symbolic sign "inf" 0))
      (lambda (sign quiet payload)
        (symbolic sign (if (zero? quiet) "sNaN" "qNaN") payload)))))

(define (round-up x n)
  (* n (quotient (+ x (- n 1)) n)))

(define (round-down x n)
  (* n (quotient x n)))

(define (round-up2 x n)
  (+ 1 (bitwise-ior (- x 1) (- n 1))))

(define (round-down2 x n)
  (bitwise-andc2 x (- n 1)))
