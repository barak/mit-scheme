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

;;;; Test of Dragon4

(declare (usual-integrations))

(define-test 'dragon4
  (lambda ()
    (define (try n settings . expecteds)
      (let ((got
	     (parameterize ((param:flonum-printer-cutoff settings))
	       (number->string (exact->inexact n)))))
	(assert-member got expecteds)))

    ;; From the MIT/GNU Scheme Reference Manual:
    (try (* 4 (atan 1 1))     '(relative 5)              "3.1416")
    (try (* 4000 (atan 1 1))  '(relative 5)              "3141.6")
    (try (* 4000 (atan 1 1))  '(relative 5 scientific)   "3.1416e3")
    (try (* 40000 (atan 1 1)) '(relative 5 scientific)   "3.1416e4")
    (try (* 40000 (atan 1 1)) '(relative 5 engineering)  "31.416e3")
    (try (* 4 (atan 1 1))     '(absolute 5)              "3.14159")
    (try (* 4000 (atan 1 1))  '(absolute 5)              "3141.59265")
    (try (* 4e10 (atan 1 1))  '(absolute -4)             "31415930000.")
    (try (* 4e10 (atan 1 1))  '(absolute -4 scientific)  "3.141593e10")
    (try (* 4e10 (atan 1 1))  '(absolute -4 engineering) "31.41593e9")
    (try (* 4e10 (atan 1 1))  '(absolute -5)             "31415900000.")

    ;; Harder tests:
    (try 0.          'normal  "0.")
    (try 0.0123456   'normal  ".0123456")
    (try 0.000123456 'normal  ".000123456")

    (try 1/3       '(relative 4) ".3333")
    (try 2/3       '(relative 4) ".6667")

    (try 12345.67   '(absolute  1 normal) "12345.7")
    (try 12345.67   '(absolute -4 normal) "10000.")
    (try 4999.      '(absolute -4 normal) "0.")
    (try 5001.      '(absolute -4 normal) "10000.")

    (try 12345.67   '(absolute  1 scientific) "1.23457e4")
    (try 12345.67   '(absolute -4 scientific) "1e4")
    (try 4999.      '(absolute -4 scientific) "0." "0e4" "0e3")
    (try 5001.      '(absolute -4 scientific) "1e4")

    (try 12345.67   '(absolute  1 engineering) "12.3457e3")
    (try 12345.67   '(absolute -4 engineering) "10e3")
    (try 4999.      '(absolute -4 engineering) "0." "0e3")
    (try 5001.      '(absolute -4 engineering) "10e3")
    (try 5001.      '(absolute -5 engineering) "0." "0e3")
    (try 5001.      '(absolute -6 engineering) "0." "0e3")
    (try -5001.     '(absolute -6 engineering) "0." "-0e3")

    (try 0.00499   '(absolute  2 normal) "0." ".00") ; "0." would be prefereable

    (try 0.00500   '(absolute  2 normal) ".01") ; (rounds up in binary)
    (try 0.00501   '(absolute  2 normal) ".01")
    (try 0.00499   '(absolute -3 normal) "0.")
    ))