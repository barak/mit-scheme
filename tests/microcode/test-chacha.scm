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

;;;; Tests of ChaCha

(define (define-chacha-core-test name primitive expected)
  (define-test name
    (lambda ()
      (let ((output (make-bytevector 64))
	    (input (make-bytevector 16 0))
	    (key (make-bytevector 32 0))
	    (constant (string->utf8 "expand 32-byte k")))
	(primitive output 0 input key constant)
	(assert-equal output expected)))))

(define-chacha-core-test 'chacha8-core
  (make-primitive-procedure 'chacha8-core 5)
  '#u8(#x3e #x00 #xef #x2f #x89 #x5f #x40 #xd6
       #x7f #x5b #xb8 #xe8 #x1f #x09 #xa5 #xa1
       #x2c #x84 #x0e #xc3 #xce #x9a #x7f #x3b
       #x18 #x1b #xe1 #x88 #xef #x71 #x1a #x1e
       #x98 #x4c #xe1 #x72 #xb9 #x21 #x6f #x41
       #x9f #x44 #x53 #x67 #x45 #x6d #x56 #x19
       #x31 #x4a #x42 #xa3 #xda #x86 #xb0 #x01
       #x38 #x7b #xfd #xb8 #x0e #x0c #xfe #x42))

(define-chacha-core-test 'chacha12-core
  (make-primitive-procedure 'chacha12-core 5)
  '#u8(#x9b #xf4 #x9a #x6a #x07 #x55 #xf9 #x53
       #x81 #x1f #xce #x12 #x5f #x26 #x83 #xd5
       #x04 #x29 #xc3 #xbb #x49 #xe0 #x74 #x14
       #x7e #x00 #x89 #xa5 #x2e #xae #x15 #x5f
       #x05 #x64 #xf8 #x79 #xd2 #x7a #xe3 #xc0
       #x2c #xe8 #x28 #x34 #xac #xfa #x8c #x79
       #x3a #x62 #x9f #x2c #xa0 #xde #x69 #x19
       #x61 #x0b #xe8 #x2f #x41 #x13 #x26 #xbe))

(define-chacha-core-test 'chacha20-core
  (make-primitive-procedure 'chacha20-core 5)
  '#u8(#x76 #xb8 #xe0 #xad #xa0 #xf1 #x3d #x90
       #x40 #x5d #x6a #xe5 #x53 #x86 #xbd #x28
       #xbd #xd2 #x19 #xb8 #xa0 #x8d #xed #x1a
       #xa8 #x36 #xef #xcc #x8b #x77 #x0d #xc7
       #xda #x41 #x59 #x7c #x51 #x57 #x48 #x8d
       #x77 #x24 #xe0 #x3f #xb8 #xd8 #x4a #x37
       #x6a #x43 #xb8 #xf4 #x15 #x18 #xa1 #x1c
       #xc3 #x87 #xb6 #x69 #xb2 #xee #x65 #x86))