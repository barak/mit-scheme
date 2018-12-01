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

(declare (usual-integrations))

;;; Nomenclature:
;;;
;;; base, b
;;; exponent width, w: number of bits in exponent field
;;; precision, p: number of bits in significand before and after radix point
;;; significand: integer in [b^{p - 1}, b^b)
;;; trailing significand: integer in [0, b^{p - 1}) = [0, b^t)
;;; trailing significand width: number of digits in trailing significand

(define (decompose-ieee754-binary32 x)
  (decompose-ieee754-binary x 8 24))

(define (decompose-ieee754-binary64 x)
  (decompose-ieee754-binary x 11 53))

(define (decompose-ieee754-binary128 x)
  (decompose-ieee754-binary x 15 113))

(define (decompose-ieee754-binary x exponent-bits precision)
  (receive (base emin emax bias exp-subnormal exp-inf/nan)
           (ieee754-binary-parameters exponent-bits precision)
    (let ((t (- precision 1)))
      (decompose-ieee754 x base emax precision
        (lambda (sign)                  ;if-zero
          (values sign 0 0))
        (lambda (sign significand)      ;if-subnormal
          (assert (= 0 (shift-right significand t)))
          (values sign (+ exp-subnormal bias) significand))
        (lambda (sign exponent significand) ;if-normal
          (assert (<= emin exponent emax))
          ;; The integer part is always 1.  Strip it for the binary
          ;; interchange format.
          (assert (= 1 (shift-right significand t)))
          (values sign (+ exponent bias) (extract-bit-field t 0 significand)))
        (lambda (sign)                  ;if-infinite
          (values sign (+ exp-inf/nan bias) 0))
        (lambda (sign quiet payload)    ;if-nan
          (assert (not (and (zero? quiet) (zero? payload))))
          (assert (zero? (extract-bit-field 1 (- t 1) payload)))
          (values sign
                  (+ exp-inf/nan bias)
                  (replace-bit-field 1 (- t 1) payload quiet)))))))

(define (ieee754-sign x)
  (cond ((< 0 x) 0)
        ((< x 0) 1)
        (else
         ;; Zero -- can't use < directly to detect sign.  Elicit a
         ;; computational difference.
         (if (negative? (atan x -1))
             1
             0))))

(define (decompose-ieee754 x base emax precision
          if-zero if-subnormal if-normal if-infinite if-nan)
  (cond ((not (= x x))
         (if-nan (if (flo:sign-negative? x) 1 0)
                 (if (flo:nan-quiet? x) 1 0)
                 (flo:nan-payload x)))
        ((and (< 1 (abs x)) (= x (/ x 2)))
         (if-infinite (if (< 0. x) 0 1)))
        (else
         (let ((sign (ieee754-sign x))
               (x (abs x))
               (emin (- 1 emax)))
           (define (significand x)
             (round->exact (* x (expt base (- precision 1)))))
           (cond ((<= 1 x)              ;Nonnegative exponent (normal)
                  (let loop ((exponent 0) (x x))
                    (cond ((< emax exponent) (if-infinite sign))
                          ((<= base x) (loop (+ exponent 1) (/ x base)))
                          (else (if-normal sign exponent (significand x))))))
                 ((<= (expt base emin) x) ;Negative exponent, normal
                  (let loop ((exponent 0) (x x))
                    (assert (<= emin exponent))
                    (if (<= 1 x)
                        (if-normal sign exponent (significand x))
                        (loop (- exponent 1) (* x base)))))
                 ((< 0 x)               ;Negative exponent, subnormal
                  (if (<= x (/ (expt base (- emin (+ precision 1))) 2))
                      (if-zero sign)
                      (if-subnormal sign
                                    (significand (/ x (expt base emin))))))
                 (else
                  (if-zero sign)))))))

(define (compose-ieee754-binary32 sign biased-exponent trailing-significand)
  (compose-ieee754-binary sign biased-exponent trailing-significand 8 24))

(define (compose-ieee754-binary64 sign biased-exponent trailing-significand)
  (compose-ieee754-binary sign biased-exponent trailing-significand 11 53))

(define (compose-ieee754-binary128 sign biased-exponent trailing-significand)
  (compose-ieee754-binary sign biased-exponent trailing-significand 15 113))

(define (compose-ieee754-binary sign biased-exponent trailing-significand
                                exponent-bits precision)
  (receive (base emin emax bias exp-subnormal exp-inf/nan)
           (ieee754-binary-parameters exponent-bits precision)
    (let ((exponent (- biased-exponent bias))
          (t (- precision 1)))
      (cond ((= exponent exp-subnormal)
             (if (zero? trailing-significand)
                 (compose-ieee754-zero sign)
                 (compose-ieee754-subnormal sign trailing-significand
                                            base emin precision)))
            ((= exponent exp-inf/nan)
             (if (zero? trailing-significand)
                 (compose-ieee754-infinity sign)
                 (let ((q (extract-bit-field 1 (- t 1) trailing-significand))
                       (p (extract-bit-field (- t 1) 0 trailing-significand)))
                   (compose-ieee754-nan sign q p))))
            (else
             (assert (<= emin exponent emax))
             (let ((significand
                    ;; Add the implied integer part of 1.
                    (replace-bit-field 1 t trailing-significand 1)))
               (compose-ieee754-normal sign exponent significand
                                       base precision)))))))

(define (compose-ieee754-zero sign)
  (* (expt -1 sign) 0.))

(define (compose-ieee754-subnormal sign significand base emin precision)
  (* (expt -1 sign)
     (* significand (expt base (- emin (- precision 1))))))

(define (compose-ieee754-normal sign exponent significand base precision)
  (* (expt -1 sign)
     (* significand (expt base (- exponent (- precision 1))))))

(define (compose-ieee754-infinity sign)
  (* (expt -1 sign)
     (flo:+inf.0)))

(define (compose-ieee754-nan sign quiet payload)
  ;; XXX Using the native microcode's idea of NaN is a little hokey:
  ;; if, for example, we wanted to use this for cross-compilation of
  ;; a Scheme with binary128 floating-point using a Scheme with
  ;; binary64 floating-point, many NaNs that could appear in the
  ;; source code would be unrepresentable in the host.
  (flo:make-nan (= sign 1) (= quiet 1) payload))

(define (ieee754-binary-parameters exponent-bits precision)
  (assert (zero? (modulo (+ exponent-bits precision) 32)))
  (let* ((base 2)
         (emax (- (expt base (- exponent-bits 1)) 1)))
    (let ((bias emax)
          (emin (- 1 emax)))
      (let ((exp-subnormal (- emin 1))
            (exp-inf/nan (+ emax 1)))
        (values base emin emax bias exp-subnormal exp-inf/nan)))))

(define (ieee754-binary32-exact? x)
  (= x
     (receive (sign biased-exponent trailing-significand)
              (decompose-ieee754-binary32 x)
       (compose-ieee754-binary32 sign biased-exponent trailing-significand))))

(define (ieee754-binary64-exact? x)
  (= x
     (receive (sign biased-exponent trailing-significand)
              (decompose-ieee754-binary64 x)
       (compose-ieee754-binary64 sign biased-exponent trailing-significand))))

(define (ieee754-binary128-exact? x)
  (= x
     (receive (sign biased-exponent trailing-significand)
              (decompose-ieee754-binary64 x)
       (compose-ieee754-binary128 sign biased-exponent trailing-significand))))

(define (ieee754-binary-hex-string x exponent-bits precision #!optional mark)
  (receive (base emin emax bias exp-subnormal exp-inf/nan)
           (ieee754-binary-parameters exponent-bits precision)
    bias exp-subnormal exp-inf/nan
    (define (symbolic sign name extra)
      (assert (or (= sign 0) (= sign 1)))
      (string-append (if (zero? sign) "+" "-") name extra))
    (define (numeric sign integer width fractional exponent)
      (assert (or (= sign 0) (= sign 1)))
      (assert (or (= integer 0) (= integer 1)))
      (assert (<= 0 fractional))
      (let ((sign (if (zero? sign) "" "-"))
            (integer (if (zero? integer) "0" "1"))
            (dot (if (zero? fractional) "" "."))
            (frac (if (zero? fractional) "" (format-frac width fractional)))
            (expsign (if (< exponent 0) "-" "+"))
            (exponent (number->string (abs exponent) #d10))
            (mark (if (default-object? mark) "0x" mark)))
        (string-append sign mark integer dot frac "p" expsign exponent)))
    (define (format-frac width fractional)
      (assert (not (zero? fractional)))
      (assert (<= (integer-length fractional) width))
      (receive (width fractional)
               (let ((misalign (remainder width 4)))
                 (if (zero? misalign)
                     (values width fractional)
                     (let ((s (- 4 misalign)))
                       (values (+ width s) (shift-left fractional s)))))
        (assert (<= (integer-length fractional) width))
        (receive (width fractional)
                 (let* ((lsb (first-set-bit fractional))
                        (lo-zeros (quotient lsb 4))
                        (s (* 4 lo-zeros)))
                   (values (- width s) (shift-right fractional s)))
          (assert (<= (integer-length fractional) width))
          (let ((hi-zeros (quotient (- width (integer-length fractional)) 4)))
            (string-append (make-string hi-zeros #\0)
                           (number->string fractional #x10))))))
    (decompose-ieee754 x base emax precision
      (lambda (sign)                    ;if-zero
        (numeric sign 0 0 0 0))
      (lambda (sign significand)        ;if-subnormal
        (assert (< 0 significand))
        (assert (= 0 (shift-right significand (- precision 1))))
        ;; Find the position of the 1 bit.
        (let* ((msb (integer-length significand))
               (width (- msb 1)))
          ;; Extract bits below that, and subtract the from the exponent.
          (let ((fractional (extract-bit-field width 0 significand))
                (exponent (- emin (- precision msb))))
            (numeric sign 1 width fractional exponent))))
      (lambda (sign exponent significand)
        (assert (< 0 significand))
        (assert (= 1 (shift-right significand (- precision 1))))
        ;; We know where the 1 bit is.
        (let* ((width (- precision 1))
               (fractional (extract-bit-field width 0 significand)))
          (numeric sign 1 width fractional exponent)))
      (lambda (sign)
        (symbolic sign "inf" ""))
      (lambda (sign quiet payload)
        payload                         ;XXX Use this.
        (symbolic sign (if (zero? quiet) "sNaN" "NaN") "")))))

(define (ieee754-binary32-hex-string x #!optional mark)
  (ieee754-binary-hex-string x 8 24 mark))

(define (ieee754-binary64-hex-string x #!optional mark)
  (ieee754-binary-hex-string x 11 53 mark))

(define (ieee754-binary128-hex-string x #!optional mark)
  (ieee754-binary-hex-string x 15 113 mark))

(define (round-up x n)
  (* n (quotient (+ x (- n 1)) n)))

(define (round-down x n)
  (* n (quotient x n)))

(define (round-up2 x n)
  (+ 1 (bitwise-ior (- x 1) (- n 1))))

(define (round-down2 x n)
  (bitwise-andc2 x (- n 1)))
