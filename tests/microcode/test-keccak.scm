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

;;;; Tests of Keccak primitives

(define-test 'SHA3-256-EMPTY
  (lambda ()
    (let ((s (make-bytevector 200 0)))
      (bytevector-u8-set! s 0 #x06)
      (bytevector-u8-set! s #x87 #x80)
      ((make-primitive-procedure 'BYTEVECTOR-KECCAK-F1600) s)
      (assert-equal (bytevector-copy s 0 32)
                    #u8(                ;SHA3-256("")
                        #xa7 #xff #xc6 #xf8 #xbf #x1e #xd7 #x66
                        #x51 #xc1 #x47 #x56 #xa0 #x61 #xd6 #x62
                        #xf5 #x80 #xff #x4d #xe4 #x3b #x49 #xfa
                        #x82 #xd8 #x0a #x4b #x80 #xf8 #x43 #x4a
                        )))))