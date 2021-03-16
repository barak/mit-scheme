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

;;;; UCD property: gc=No

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define (char-gc=number:other? char)
  (char-in-set? char char-set:gc=number:other))

(define-deferred char-set:gc=number:other
  (char-set*
   '((178 . 180)
     185
     (188 . 191)
     (2548 . 2554)
     (2930 . 2936)
     (3056 . 3059)
     (3192 . 3199)
     (3416 . 3423)
     (3440 . 3449)
     (3882 . 3892)
     (4969 . 4989)
     (6128 . 6138)
     6618
     8304
     (8308 . 8314)
     (8320 . 8330)
     (8528 . 8544)
     8585
     (9312 . 9372)
     (9450 . 9472)
     (10102 . 10132)
     11517
     (12690 . 12694)
     (12832 . 12842)
     (12872 . 12880)
     (12881 . 12896)
     (12928 . 12938)
     (12977 . 12992)
     (43056 . 43062)
     (65799 . 65844)
     (65909 . 65913)
     (65930 . 65932)
     (66273 . 66300)
     (66336 . 66340)
     (67672 . 67680)
     (67705 . 67712)
     (67751 . 67760)
     (67835 . 67840)
     (67862 . 67868)
     (68028 . 68030)
     (68032 . 68048)
     (68050 . 68096)
     (68160 . 68169)
     (68221 . 68223)
     (68253 . 68256)
     (68331 . 68336)
     (68440 . 68448)
     (68472 . 68480)
     (68521 . 68528)
     (68858 . 68864)
     (69216 . 69247)
     (69405 . 69415)
     (69457 . 69461)
     (69573 . 69580)
     (69714 . 69734)
     (70113 . 70133)
     (71482 . 71484)
     (71914 . 71923)
     (72794 . 72813)
     (73664 . 73685)
     (93019 . 93026)
     (93824 . 93847)
     (119520 . 119540)
     (119648 . 119673)
     (125127 . 125136)
     (126065 . 126124)
     (126125 . 126128)
     (126129 . 126133)
     (126209 . 126254)
     (126255 . 126270)
     (127232 . 127245))))
