#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

(import (scheme base)
	(srfi 143))

(fixnum? 32767) 'expect-true
(fixnum? 1.1) 'expect-false

(fx=? 1 1 1) 'expect-true
(fx=? 1 2 2) 'expect-false
(fx=? 1 1 2) 'expect-false
(fx=? 1 2 3) 'expect-false

(fx<? 1 2 3) 'expect-true
(fx<? 1 1 2) 'expect-false
(fx>? 3 2 1) 'expect-true
(fx>? 2 1 1) 'expect-false
(fx<=? 1 1 2) 'expect-true
(fx<=? 1 2 1) 'expect-false
(fx>=? 2 1 1) 'expect-true
(fx>=? 1 2 1) 'expect-false
(list (fx<=? 1 1 2) (fx<=? 2 1 3)) '(expect equal? '(#t #f))

(fxzero? 0) 'expect-true
(fxzero? 1) 'expect-false

(fxpositive? 0) 'expect-false
(fxpositive? 1) 'expect-true
(fxpositive? -1) 'expect-false

(fxnegative? 0) 'expect-false
(fxnegative? 1) 'expect-false
(fxnegative? -1) 'expect-true

(fxodd? 0) 'expect-false
(fxodd? 1) 'expect-true
(fxodd? -1) 'expect-true
(fxodd? 102) 'expect-false

(fxeven? 0) 'expect-true
(fxeven? 1) 'expect-false
(fxeven? -2) 'expect-true
(fxeven? 102) 'expect-true

(fxmax 3 4) '(expect = 4)
(fxmax 3 5 4) '(expect = 5)
(fxmin 3 4) '(expect = 3)
(fxmin 3 5 4) '(expect = 3)

(fx+ 3 4) '(expect = 7)
(fx* 4 3) '(expect = 12)

(fx- 3 4) '(expect = -1)
(fxneg 3) '(expect = -3)

(fxabs -7) '(expect = 7)
(fxabs 7) '(expect = 7)

(fxsquare 42) '(expect = 1764)
(fxsquare 2) '(expect = 4)

(fxquotient 5 2) '(expect = 2)
(fxquotient -5 2) '(expect = -2)
(fxquotient 5 -2) '(expect = -2)
(fxquotient -5 -2) '(expect = 2)

(fxremainder 13 4) '(expect = 1)
(fxremainder -13 4) '(expect = -1)
(fxremainder 13 -4) '(expect = 1)
(fxremainder -13 -4) '(expect = -1)

(let*-values (((root rem) (fxsqrt 32)))
  (* root rem))
'(expect = 35)

(fxnot 0) '(expect = -1)
(fxand #b0 #b1) '(expect = 0)
(fxand 14 6) '(expect = 6)
(fxior 10 12) '(expect = 14)
(fxxor 10 12) '(expect = 6)
(fxnot -1) '(expect = 0)
(fxif 3 1 8) '(expect = 9)
(fxif 3 8 1) '(expect = 0)
(fxbit-count 12) '(expect = 2)
(fxlength 0) '(expect = 0)
(fxlength 128) '(expect = 8)
(fxlength 255) '(expect = 8)
(fxlength 256) '(expect = 9)
(fxfirst-set-bit 0) '(expect = -1)
(fxfirst-set-bit 1) '(expect = 0)
(fxfirst-set-bit 3) '(expect = 0)
(fxfirst-set-bit 4) '(expect = 2)
(fxfirst-set-bit 6) '(expect = 1)
(fxfirst-set-bit -1) '(expect = 0)
(fxfirst-set-bit -2) '(expect = 1)
(fxfirst-set-bit -3) '(expect = 0)
(fxfirst-set-bit -4) '(expect = 2)
(fxbit-set? 0 1) 'expect-true
(fxbit-set? 1 1) 'expect-false
(fxbit-set? 1 8) 'expect-false
(fxbit-set? fx-width 0) 'expect-error
(fxbit-set? (- fx-width 1) 0) 'expect-false
(fxbit-set? (- fx-width 2) 0) 'expect-false
(fxbit-set? fx-width -1) 'expect-error
(fxbit-set? (- fx-width 1) -1) 'expect-true
(fxbit-set? (- fx-width 2) -1) 'expect-true
(fxbit-set? 10000 -1) 'expect-error
(fxbit-set? 1000 -1) 'expect-error
(fxcopy-bit 0 0 #f) '(expect = 0)
(fxcopy-bit 0 -1 #t) '(expect = -1)
(fxcopy-bit 0 0 #t) '(expect = 1)
(fxcopy-bit 8 6 #t) '(expect = #x106)
(fxcopy-bit 8 6 #f) '(expect = 6)
(fxcopy-bit 0 -1 #f) '(expect = -2)
(fxbit-field 6 0 1) '(expect = 0)
(fxbit-field 6 1 3) '(expect = 3)
(fxarithmetic-shift 1 1) '(expect = 2)
(fxarithmetic-shift 1 -1) '(expect = 0)
(fxbit-field-rotate #b110 1 1 2) '(expect = #b110)
(fxbit-field-rotate #b110 1 2 4) '(expect = #b1010)
(fxbit-field-rotate #b0111 -1 1 4) '(expect = #b1011)
(fxbit-field-rotate #b110 0 0 10) '(expect = #b110)
(fxbit-field-reverse 6 1 3) '(expect = 6)
(fxbit-field-reverse 6 1 4) '(expect = 12)
(fxnot 10) '(expect = -11)
(fxnot -37) '(expect = 36)
(fxior 3  10) '(expect = 11)
(fxand 11 26) '(expect = 10)
(fxxor 3 10) '(expect = 9)
(fxand 37 12) '(expect = 4)
(fxarithmetic-shift 8 2) '(expect = 32)
(fxarithmetic-shift 4 0) '(expect = 4)
(fxarithmetic-shift 8 -1) '(expect = 4)
(fxlength  0) '(expect = 0)
(fxlength  1) '(expect = 1)
(fxlength -1) '(expect = 0)
(fxlength  7) '(expect = 3)
(fxlength -7) '(expect = 3)
(fxlength  8) '(expect = 4)
(fxlength -8) '(expect = 3)
(fxbit-set? 3 10) 'expect-true
(fxbit-set? 2 6) 'expect-true
(fxbit-set? 0 6) 'expect-false
(fxcopy-bit 2 0 #t) '(expect = #b100)
(fxcopy-bit 2 #b1111 #f) #b1011
(fxfirst-set-bit 2) '(expect = 1)
(fxfirst-set-bit 40) '(expect = 3)
(fxfirst-set-bit -28) '(expect = 2)
(fxand #b1 #b1) '(expect = 1)
(fxand #b1 #b10) '(expect = 0)
(fxand #b11 #b10) '(expect = #b10)
(fxand #b101 #b111) '(expect = #b101)
(fxand -1 #b111) '(expect = #b111)
(fxand -2 #b111) '(expect = #b110)
(fxarithmetic-shift 1 0) '(expect = 1)
(fxarithmetic-shift 1 2) '(expect = 4)
(fxarithmetic-shift 1 3) '(expect = 8)
(fxarithmetic-shift 1 4) '(expect = 16)
(fxarithmetic-shift -1 0) '(expect = -1)
(fxarithmetic-shift -1 1) '(expect = -2)
(fxarithmetic-shift -1 2) '(expect = -4)
(fxarithmetic-shift -1 3) '(expect = -8)
(fxarithmetic-shift -1 4) '(expect = -16)
(fxbit-field #b1101101010 0 4) '(expect = #b1010)
(fxbit-field #b1101101010 3 9) '(expect = #b101101)
(fxbit-field #b1101101010 4 9) '(expect = #b10110)
(fxbit-field #b1101101010 4 10) '(expect = #b110110)
(fxif 1 1 2) '(expect = 3)
(fxif #b00111100 #b11110000 #b00001111) '(expect = #b00110011)
(fxcopy-bit 0 0 #t) '(expect = #b1)
