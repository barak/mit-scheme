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

;;;; UCD property: nt=De

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define (char-nt=decimal? char)
  (char-in-set? char char-set:nt=decimal))

(define-deferred char-set:nt=decimal
  (char-set* '((48 . 58) (1632 . 1642) (1776 . 1786) (1984 . 1994) (2406 . 2416) (2534 . 2544) (2662 . 2672) (2790 . 2800) (2918 . 2928) (3046 . 3056) (3174 . 3184) (3302 . 3312) (3430 . 3440) (3558 . 3568) (3664 . 3674) (3792 . 3802) (3872 . 3882) (4160 . 4170) (4240 . 4250) (6112 . 6122) (6160 . 6170) (6470 . 6480) (6608 . 6618) (6784 . 6794) (6800 . 6810) (6992 . 7002) (7088 . 7098) (7232 . 7242) (7248 . 7258) (42528 . 42538) (43216 . 43226) (43264 . 43274) (43472 . 43482) (43504 . 43514) (43600 . 43610) (44016 . 44026) (65296 . 65306) (66720 . 66730) (68912 . 68922) (69734 . 69744) (69872 . 69882) (69942 . 69952) (70096 . 70106) (70384 . 70394) (70736 . 70746) (70864 . 70874) (71248 . 71258) (71360 . 71370) (71472 . 71482) (71904 . 71914) (72016 . 72026) (72784 . 72794) (73040 . 73050) (73120 . 73130) (92768 . 92778) (93008 . 93018) (120782 . 120832) (123200 . 123210) (123632 . 123642) (125264 . 125274) (130032 . 130042))))
