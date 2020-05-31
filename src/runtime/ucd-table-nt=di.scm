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

;;;; UCD property: nt=Di

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-nt=digit? char)
  (char-in-set? char char-set:nt=digit))

(define-deferred char-set:nt=digit
  (char-set* '((178 . 180) 185 (4969 . 4978) 6618 8304 (8308 . 8314) (8320 . 8330) (9312 . 9321) (9332 . 9341) (9352 . 9361) 9450 (9461 . 9470) 9471 (10102 . 10111) (10112 . 10121) (10122 . 10131) (68160 . 68164) (69216 . 69225) (69714 . 69723) (127232 . 127243))))
