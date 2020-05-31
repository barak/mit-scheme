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

;;;; UCD property: gc=Sk

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-gc=symbol:modifier? char)
  (char-in-set? char char-set:gc=symbol:modifier))

(define-deferred char-set:gc=symbol:modifier
  (char-set* '(94 96 168 175 180 184 (706 . 710) (722 . 736) (741 . 748) 749 (751 . 768) 885 (900 . 902) 8125 (8127 . 8130) (8141 . 8144) (8157 . 8160) (8173 . 8176) (8189 . 8191) (12443 . 12445) (42752 . 42775) (42784 . 42786) (42889 . 42891) 43867 (64434 . 64450) 65342 65344 65507 (127995 . 128000))))
