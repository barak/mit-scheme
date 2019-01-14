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

;;;; UCD property: Comp_Ex (full-composition-exclusion)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-full-composition-exclusion? char)
  (char-in-set? char char-set:full-composition-exclusion))

(define-deferred char-set:full-composition-exclusion
  (char-set* '((832 . 834) (835 . 837) 884 894 903 (2392 . 2400) (2524 . 2526) 2527 2611 2614 (2649 . 2652) 2654 (2908 . 2910) 3907 3917 3922 3927 3932 3945 3955 (3957 . 3959) 3960 3969 3987 3997 4002 4007 4012 4025 8049 8051 8053 8055 8057 8059 8061 8123 8126 8137 8139 8147 8155 8163 8171 (8174 . 8176) 8185 8187 8189 (8192 . 8194) 8486 (8490 . 8492) (9001 . 9003) 10972 (63744 . 64014) 64016 64018 (64021 . 64031) 64032 64034 (64037 . 64039) (64042 . 64110) (64112 . 64218) 64285 64287 (64298 . 64311) (64312 . 64317) 64318 (64320 . 64322) (64323 . 64325) (64326 . 64335) (119134 . 119141) (119227 . 119233) (194560 . 195102))))
