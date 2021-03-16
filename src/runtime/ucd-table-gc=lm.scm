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

;;;; UCD property: gc=Lm

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define (char-gc=letter:modifier? char)
  (char-in-set? char char-set:gc=letter:modifier))

(define-deferred char-set:gc=letter:modifier
  (char-set* '((688 . 706) (710 . 722) (736 . 741) 748 750 884 890 1369 1600 (1765 . 1767) (2036 . 2038) 2042 2074 2084 2088 2417 3654 3782 4348 6103 6211 6823 (7288 . 7294) (7468 . 7531) 7544 (7579 . 7616) 8305 8319 (8336 . 8349) (11388 . 11390) 11631 11823 12293 (12337 . 12342) 12347 (12445 . 12447) (12540 . 12543) 40981 (42232 . 42238) 42508 42623 (42652 . 42654) (42775 . 42784) 42864 42888 (43000 . 43002) 43471 43494 43632 43741 (43763 . 43765) (43868 . 43872) 43881 65392 (65438 . 65440) (92992 . 92996) (94099 . 94112) (94176 . 94178) 94179 (123191 . 123198) 125259)))
