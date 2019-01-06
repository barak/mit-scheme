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

;;;; UCD property: gc=Ps

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-gc=punctuation:open? char)
  (char-in-set? char char-set:gc=punctuation:open))

(define-deferred char-set:gc=punctuation:open
  (char-set* '(40 91 123 3898 3900 5787 8218 8222 8261 8317 8333 8968 8970 9001 10088 10090 10092 10094 10096 10098 10100 10181 10214 10216 10218 10220 10222 10627 10629 10631 10633 10635 10637 10639 10641 10643 10645 10647 10712 10714 10748 11810 11812 11814 11816 11842 12296 12298 12300 12302 12304 12308 12310 12312 12314 12317 64831 65047 65077 65079 65081 65083 65085 65087 65089 65091 65095 65113 65115 65117 65288 65339 65371 65375 65378)))
