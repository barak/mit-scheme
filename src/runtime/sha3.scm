#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; SHA-3: FIPS-202, Permutation-Based Hash and Extendable-Output Functions
;;; package: (runtime sha3)

(declare (usual-integrations))

(define-primitives
  (bytevector-keccak-f1600 1))

(define-structure sha3
  (state #f read-only #t)
  nb)

(define-integrable (sha3-rate-bytes db)
  (- 200 (* 2 db)))

(define-integrable (sha3-init rb)
  (make-sha3 (make-bytevector 200 0) rb))

(declare (integrate-operator sha3-update))
(define (sha3-update sha3 bv start end rb)
  (assert (< 0 (sha3-nb sha3)))
  (let ((i (- rb (sha3-nb sha3)))
        (ni (min (- end start) (sha3-nb sha3))))
    ;; If there's a partial buffer, try to fill it.
    (assert (<= 0 i))
    (assert (< i rb))
    (assert (<= 0 ni))
    (assert (< ni rb))
    (bytevector-xor! (sha3-state sha3) i bv start ni)
    ;; If we couldn't fill the buffer, we're done.
    (if (< ni (sha3-nb sha3))
        (set-sha3-nb! sha3 (- (sha3-nb sha3) ni))
        ;; Otherwise, permute and go on.
        (begin
          (bytevector-keccak-f1600 (sha3-state sha3))
          ;; If we filled a buffer, permute now.
          (let loop ((start (+ start ni)))
            (if (<= start (- end rb))
                ;; Xor a buffer's worth of input and permute.
                (begin
                  (bytevector-xor! (sha3-state sha3) 0 bv start rb)
                  (bytevector-keccak-f1600 (sha3-state sha3))
                  (loop (+ start rb)))
                ;; Partially fill the buffer with as many bytes as we can.
                (let ((nf (- end start)))
                  (assert (< nf rb))
                  (bytevector-xor! (sha3-state sha3) 0 bv start nf)
                  (set-sha3-nb! sha3 (- rb nf)))))))))

(define-integrable (sha3-final sha3 h hstart db rb)
  (assert (<= db (* 8 25)))
  (assert (< 0 (sha3-nb sha3)))
  ;; Append 01, pad with 10*1 up to buffer boundary, LSB first.
  (bytevector-u8-xor! (sha3-state sha3) (- rb (sha3-nb sha3)) #x06)
  (bytevector-u8-xor! (sha3-state sha3) (- rb 1) #x80)
  (bytevector-keccak-f1600 (sha3-state sha3))
  ;; Reveal the first db bytes of states.
  (bytevector-copy! h hstart (sha3-state sha3) 0 db)
  ;; Forget the rest.  XXX Prevent optimizing away.
  (bytevector-fill! (sha3-state sha3) 0)
  (set-sha3-nb! sha3 0))

(define-integrable (sha3256-rate-bytes) (sha3-rate-bytes 32))

(define (sha3256 bv)
  (let ((s (sha3256-init))
        (h (make-bytevector 32)))
    (sha3256-update s bv 0 (bytevector-length bv))
    (sha3256-final s h 0 32)
    h))

(define (sha3256-init)
  (sha3-init (sha3256-rate-bytes)))

(define (sha3256-update sha3256 bv start end)
  (sha3-update sha3256 bv start end (sha3256-rate-bytes)))

(define (sha3256-final sha3256 bv start end)
  (sha3-final sha3256 bv start (- end start) (sha3256-rate-bytes)))

(define (bytevector-u8-xor! bv i x)
  ;;(declare (no-type-checks) (no-range-checks))
  (guarantee bytevector? bv 'bytevector-u8-xor!)
  (guarantee index-fixnum? i 'bytevector-u8-xor!)
  (if (not (fix:< i (bytevector-length bv)))
      (error:bad-range-argument i 'bytevector-u8-xor!))
  (guarantee u8? x 'bytevector-u8-xor)
  (bytevector-u8-set! bv i (fix:xor x (bytevector-u8-ref bv i))))

(define (bytevector-xor! t ts f fs n)
  ;;(declare (no-type-checks) (no-range-checks))
  (guarantee bytevector? f 'bytevector-xor!)
  (guarantee bytevector? t 'bytevector-xor!)
  (guarantee index-fixnum? ts 'bytevector-xor!)
  (guarantee index-fixnum? fs 'bytevector-xor!)
  (if (not (fix:<= n (fix:- (bytevector-length t) ts)))
      (error:bad-range-argument ts 'bytevector-xor!))
  (if (not (fix:<= n (fix:- (bytevector-length f) fs)))
      (error:bad-range-argument fs 'bytevector-xor!))
  (do ((i 0 (fix:+ i 1))) ((fix:>= i n))
    (let ((ti (bytevector-u8-ref t (fix:+ ts i)))
          (fi (bytevector-u8-ref f (fix:+ fs i))))
      (declare (integrate ti fi))
      (bytevector-u8-set! t (fix:+ ts i) (fix:xor ti fi)))))
