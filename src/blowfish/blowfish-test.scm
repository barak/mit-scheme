#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Tests of Blowfish

(define (define-bf-test name procedure)
  (if (blowfish-available?)
      (define-test name procedure)))

(define (define-variable-key-test i c)
  (define k24
    #u8(#xF0 #xE1 #xD2 #xC3 #xB4 #xA5 #x96 #x87
        #x78 #x69 #x5A #x4B #x3C #x2D #x1E #x0F
        #x00 #x11 #x22 #x33 #x44 #x55 #x66 #x77))
  (define p24 #u8(#xFE #xDC #xBA #x98 #x76 #x54 #x32 #x10))
  (let ((k (bytevector-copy k24 0 i))
        (p p24))
    (define-bf-test (symbol 'variable-key ': i ': 'encrypt)
      (lambda ()
        (let ((bf (blowfish-set-key k))
              (buf (make-bytevector 8)))
          (blowfish-ecb p buf bf #t)
          (assert-equal buf c))))
    (define-bf-test (symbol 'variable-key ': i ': 'decrypt)
      (lambda ()
        (let ((bf (blowfish-set-key k))
              (buf (make-bytevector 8)))
          (blowfish-ecb c buf bf #f)
          (assert-equal buf p))))))

(define-variable-key-test 1 #u8(#xF9 #xAD #x59 #x7C #x49 #xDB #x00 #x5E))
(define-variable-key-test 2 #u8(#xE9 #x1D #x21 #xC1 #xD9 #x61 #xA6 #xD6))
(define-variable-key-test 3 #u8(#xE9 #xC2 #xB7 #x0A #x1B #xC6 #x5C #xF3))
(define-variable-key-test 4 #u8(#xBE #x1E #x63 #x94 #x08 #x64 #x0F #x05))
(define-variable-key-test 5 #u8(#xB3 #x9E #x44 #x48 #x1B #xDB #x1E #x6E))
(define-variable-key-test 6 #u8(#x94 #x57 #xAA #x83 #xB1 #x92 #x8C #x0D))
(define-variable-key-test 7 #u8(#x8B #xB7 #x70 #x32 #xF9 #x60 #x62 #x9D))
(define-variable-key-test 8 #u8(#xE8 #x7A #x24 #x4E #x2C #xC8 #x5E #x82))
(define-variable-key-test 9 #u8(#x15 #x75 #x0E #x7A #x4F #x4E #xC5 #x77))
(define-variable-key-test 10 #u8(#x12 #x2B #xA7 #x0B #x3A #xB6 #x4A #xE0))
(define-variable-key-test 11 #u8(#x3A #x83 #x3C #x9A #xFF #xC5 #x37 #xF6))
(define-variable-key-test 12 #u8(#x94 #x09 #xDA #x87 #xA9 #x0F #x6B #xF2))
(define-variable-key-test 13 #u8(#x88 #x4F #x80 #x62 #x50 #x60 #xB8 #xB4))
(define-variable-key-test 14 #u8(#x1F #x85 #x03 #x1C #x19 #xE1 #x19 #x68))
(define-variable-key-test 15 #u8(#x79 #xD9 #x37 #x3A #x71 #x4C #xA3 #x4F))
(define-variable-key-test 16 #u8(#x93 #x14 #x28 #x87 #xEE #x3B #xE1 #x5C))
(define-variable-key-test 17 #u8(#x03 #x42 #x9E #x83 #x8C #xE2 #xD1 #x4B))
(define-variable-key-test 18 #u8(#xA4 #x29 #x9E #x27 #x46 #x9F #xF6 #x7B))
(define-variable-key-test 19 #u8(#xAF #xD5 #xAE #xD1 #xC1 #xBC #x96 #xA8))
(define-variable-key-test 20 #u8(#x10 #x85 #x1C #x0E #x38 #x58 #xDA #x9F))
(define-variable-key-test 21 #u8(#xE6 #xF5 #x1E #xD7 #x9B #x9D #xB2 #x1F))
(define-variable-key-test 22 #u8(#x64 #xA6 #xE1 #x4A #xFD #x36 #xB4 #x6F))
(define-variable-key-test 23 #u8(#x80 #xC7 #xD7 #xD4 #x5A #x54 #x79 #xAD))
(define-variable-key-test 24 #u8(#x05 #x04 #x4B #x62 #xFA #x52 #xD0 #x80))

((lambda (ks ps cs)
   ((lambda (doit) (for-each doit (iota (length ks)) ks ps cs))
    (lambda (i k p c)
      (define-bf-test (symbol 'encrypt ': i)
        (lambda ()
          (let ((bf (blowfish-set-key k))
                (buf (make-bytevector 8)))
            (blowfish-ecb p buf bf #t)
            (assert-equal buf c))))
      (define-bf-test (symbol 'decrypt ': i)
        (lambda ()
          (let ((bf (blowfish-set-key k))
                (buf (make-bytevector 8)))
            (blowfish-ecb c buf bf #f)
            (assert-equal buf p)))))))
 '(#u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
   #u8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)
   #u8(#x30 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
   #u8(#x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11)
   #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF)
   #u8(#x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11)
   #u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
   #u8(#xFE #xDC #xBA #x98 #x76 #x54 #x32 #x10)
   #u8(#x7C #xA1 #x10 #x45 #x4A #x1A #x6E #x57)
   #u8(#x01 #x31 #xD9 #x61 #x9D #xC1 #x37 #x6E)
   #u8(#x07 #xA1 #x13 #x3E #x4A #x0B #x26 #x86)
   #u8(#x38 #x49 #x67 #x4C #x26 #x02 #x31 #x9E)
   #u8(#x04 #xB9 #x15 #xBA #x43 #xFE #xB5 #xB6)
   #u8(#x01 #x13 #xB9 #x70 #xFD #x34 #xF2 #xCE)
   #u8(#x01 #x70 #xF1 #x75 #x46 #x8F #xB5 #xE6)
   #u8(#x43 #x29 #x7F #xAD #x38 #xE3 #x73 #xFE)
   #u8(#x07 #xA7 #x13 #x70 #x45 #xDA #x2A #x16)
   #u8(#x04 #x68 #x91 #x04 #xC2 #xFD #x3B #x2F)
   #u8(#x37 #xD0 #x6B #xB5 #x16 #xCB #x75 #x46)
   #u8(#x1F #x08 #x26 #x0D #x1A #xC2 #x46 #x5E)
   #u8(#x58 #x40 #x23 #x64 #x1A #xBA #x61 #x76)
   #u8(#x02 #x58 #x16 #x16 #x46 #x29 #xB0 #x07)
   #u8(#x49 #x79 #x3E #xBC #x79 #xB3 #x25 #x8F)
   #u8(#x4F #xB0 #x5E #x15 #x15 #xAB #x73 #xA7)
   #u8(#x49 #xE9 #x5D #x6D #x4C #xA2 #x29 #xBF)
   #u8(#x01 #x83 #x10 #xDC #x40 #x9B #x26 #xD6)
   #u8(#x1C #x58 #x7F #x1C #x13 #x92 #x4F #xEF)
   #u8(#x01 #x01 #x01 #x01 #x01 #x01 #x01 #x01)
   #u8(#x1F #x1F #x1F #x1F #x0E #x0E #x0E #x0E)
   #u8(#xE0 #xFE #xE0 #xFE #xF1 #xFE #xF1 #xFE)
   #u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
   #u8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)
   #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF)
   #u8(#xFE #xDC #xBA #x98 #x76 #x54 #x32 #x10))
 '(#u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
   #u8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)
   #u8(#x10 #x00 #x00 #x00 #x00 #x00 #x00 #x01)
   #u8(#x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11)
   #u8(#x11 #x11 #x11 #x11 #x11 #x11 #x11 #x11)
   #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF)
   #u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
   #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF)
   #u8(#x01 #xA1 #xD6 #xD0 #x39 #x77 #x67 #x42)
   #u8(#x5C #xD5 #x4C #xA8 #x3D #xEF #x57 #xDA)
   #u8(#x02 #x48 #xD4 #x38 #x06 #xF6 #x71 #x72)
   #u8(#x51 #x45 #x4B #x58 #x2D #xDF #x44 #x0A)
   #u8(#x42 #xFD #x44 #x30 #x59 #x57 #x7F #xA2)
   #u8(#x05 #x9B #x5E #x08 #x51 #xCF #x14 #x3A)
   #u8(#x07 #x56 #xD8 #xE0 #x77 #x47 #x61 #xD2)
   #u8(#x76 #x25 #x14 #xB8 #x29 #xBF #x48 #x6A)
   #u8(#x3B #xDD #x11 #x90 #x49 #x37 #x28 #x02)
   #u8(#x26 #x95 #x5F #x68 #x35 #xAF #x60 #x9A)
   #u8(#x16 #x4D #x5E #x40 #x4F #x27 #x52 #x32)
   #u8(#x6B #x05 #x6E #x18 #x75 #x9F #x5C #xCA)
   #u8(#x00 #x4B #xD6 #xEF #x09 #x17 #x60 #x62)
   #u8(#x48 #x0D #x39 #x00 #x6E #xE7 #x62 #xF2)
   #u8(#x43 #x75 #x40 #xC8 #x69 #x8F #x3C #xFA)
   #u8(#x07 #x2D #x43 #xA0 #x77 #x07 #x52 #x92)
   #u8(#x02 #xFE #x55 #x77 #x81 #x17 #xF1 #x2A)
   #u8(#x1D #x9D #x5C #x50 #x18 #xF7 #x28 #xC2)
   #u8(#x30 #x55 #x32 #x28 #x6D #x6F #x29 #x5A)
   #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF)
   #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF)
   #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF)
   #u8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)
   #u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
   #u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
   #u8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))
 '(#u8(#x4E #xF9 #x97 #x45 #x61 #x98 #xDD #x78)
   #u8(#x51 #x86 #x6F #xD5 #xB8 #x5E #xCB #x8A)
   #u8(#x7D #x85 #x6F #x9A #x61 #x30 #x63 #xF2)
   #u8(#x24 #x66 #xDD #x87 #x8B #x96 #x3C #x9D)
   #u8(#x61 #xF9 #xC3 #x80 #x22 #x81 #xB0 #x96)
   #u8(#x7D #x0C #xC6 #x30 #xAF #xDA #x1E #xC7)
   #u8(#x4E #xF9 #x97 #x45 #x61 #x98 #xDD #x78)
   #u8(#x0A #xCE #xAB #x0F #xC6 #xA0 #xA2 #x8D)
   #u8(#x59 #xC6 #x82 #x45 #xEB #x05 #x28 #x2B)
   #u8(#xB1 #xB8 #xCC #x0B #x25 #x0F #x09 #xA0)
   #u8(#x17 #x30 #xE5 #x77 #x8B #xEA #x1D #xA4)
   #u8(#xA2 #x5E #x78 #x56 #xCF #x26 #x51 #xEB)
   #u8(#x35 #x38 #x82 #xB1 #x09 #xCE #x8F #x1A)
   #u8(#x48 #xF4 #xD0 #x88 #x4C #x37 #x99 #x18)
   #u8(#x43 #x21 #x93 #xB7 #x89 #x51 #xFC #x98)
   #u8(#x13 #xF0 #x41 #x54 #xD6 #x9D #x1A #xE5)
   #u8(#x2E #xED #xDA #x93 #xFF #xD3 #x9C #x79)
   #u8(#xD8 #x87 #xE0 #x39 #x3C #x2D #xA6 #xE3)
   #u8(#x5F #x99 #xD0 #x4F #x5B #x16 #x39 #x69)
   #u8(#x4A #x05 #x7A #x3B #x24 #xD3 #x97 #x7B)
   #u8(#x45 #x20 #x31 #xC1 #xE4 #xFA #xDA #x8E)
   #u8(#x75 #x55 #xAE #x39 #xF5 #x9B #x87 #xBD)
   #u8(#x53 #xC5 #x5F #x9C #xB4 #x9F #xC0 #x19)
   #u8(#x7A #x8E #x7B #xFA #x93 #x7E #x89 #xA3)
   #u8(#xCF #x9C #x5D #x7A #x49 #x86 #xAD #xB5)
   #u8(#xD1 #xAB #xB2 #x90 #x65 #x8B #xC7 #x78)
   #u8(#x55 #xCB #x37 #x74 #xD1 #x3E #xF2 #x01)
   #u8(#xFA #x34 #xEC #x48 #x47 #xB2 #x68 #xB2)
   #u8(#xA7 #x90 #x79 #x51 #x08 #xEA #x3C #xAE)
   #u8(#xC3 #x9E #x07 #x2D #x9F #xAC #x63 #x1D)
   #u8(#x01 #x49 #x33 #xE0 #xCD #xAF #xF6 #xE4)
   #u8(#xF2 #x1E #x9A #x77 #xB7 #x1C #x49 #xBC)
   #u8(#x24 #x59 #x46 #x88 #x57 #x54 #x36 #x9A)
   #u8(#x6B #x5C #x5A #x9C #x5D #x9E #x0A #x5A)))

(define (define-cbc-test i c)
  (define k #u8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
  (define iv #u8(7 6 5 4 3 2 1 0))
  (define p #u8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))
  (let ((p (bytevector-copy p 0 i)))
    (define-bf-test (symbol 'cbc:encrypt ': i)
      (lambda ()
        (let ((bf (blowfish-set-key k))
              (iv (bytevector-copy iv))
              (buf (make-bytevector i)))
          (blowfish-cbc p buf bf iv #t)
          (assert-equal buf c)
          (assert-equal iv (bytevector-copy buf (- i 8) i)))))
    (define-bf-test (symbol 'cbc:decrypt ': i)
      (lambda ()
        (let ((bf (blowfish-set-key k))
              (iv (bytevector-copy iv))
              (buf (make-bytevector i)))
          (blowfish-cbc c buf bf iv #f)
          (assert-equal buf p)
          (assert-equal iv (bytevector-copy c (- i 8) i)))))))

(define-cbc-test 8
  #u8(#x90 #x6a #xb9 #x17 #xb0 #x9f #xcd #x3a))

(define-cbc-test 16
  #u8(#x90 #x6a #xb9 #x17 #xb0 #x9f #xcd #x3a
      #x2b #x41 #x4d #x69 #xbb #xa0 #xc0 #xdf))

(define-cbc-test 24
  #u8(#x90 #x6a #xb9 #x17 #xb0 #x9f #xcd #x3a
      #x2b #x41 #x4d #x69 #xbb #xa0 #xc0 #xdf
      #xed #x69 #x9e #xca #x55 #x13 #xc2 #x7e))
