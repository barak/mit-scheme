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

;;;; Tests of MD5

(define-test 'MD5
  (lambda ()
    (let ((bv (make-bytevector 256)))
      (do ((i 0 (+ i 1))) ((>= i 256))
        (bytevector-u8-set! bv i i))
      (let ((h (make-bytevector (* 16 256))))
        (let loop ((i 0))
          (if (< i 256)
              (let ((hi (md5-bytevector bv 0 i)))
                (bytevector-copy! h (* 16 i) hi)
                (loop (+ i 1)))
              (assert-equal (md5-bytevector h)
                            #u8(
                                #xbf #xd5 #x08 #x30 #xba #x3e #xbc #x1d
                                #xf2 #x78 #xc0 #x26 #x97 #x79 #xa0 #x3e
                                ))))))))
