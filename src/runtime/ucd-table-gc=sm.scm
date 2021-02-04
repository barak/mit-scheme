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

;;;; UCD property: gc=Sm

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-gc=symbol:math? char)
  (char-in-set? char char-set:gc=symbol:math))

(define-deferred char-set:gc=symbol:math
  (char-set* '(43 (60 . 63) 124 126 172 177 215 247 1014 (1542 . 1545) 8260 8274 (8314 . 8317) (8330 . 8333) 8472 (8512 . 8517) 8523 (8592 . 8597) (8602 . 8604) 8608 8611 8614 8622 (8654 . 8656) 8658 8660 (8692 . 8960) (8992 . 8994) 9084 (9115 . 9140) (9180 . 9186) 9655 9665 (9720 . 9728) 9839 (10176 . 10181) (10183 . 10214) (10224 . 10240) (10496 . 10627) (10649 . 10712) (10716 . 10748) (10750 . 11008) (11056 . 11077) (11079 . 11085) 64297 65122 (65124 . 65127) 65291 (65308 . 65311) 65372 65374 65506 (65513 . 65517) 120513 120539 120571 120597 120629 120655 120687 120713 120745 120771 (126704 . 126706))))
