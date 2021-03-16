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

;;;; UCD property: nt=Nu

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define (char-nt=numeric? char)
  (char-in-set? char char-set:nt=numeric))

(define-deferred char-set:nt=numeric
  (char-set*
   '((188 . 191)
     (2548 . 2554)
     (2930 . 2936)
     (3056 . 3059)
     (3192 . 3199)
     (3416 . 3423)
     (3440 . 3449)
     (3882 . 3892)
     (4978 . 4989)
     (5870 . 5873)
     (6128 . 6138)
     (8528 . 8579)
     (8581 . 8586)
     (9321 . 9332)
     (9341 . 9352)
     (9361 . 9372)
     (9451 . 9461)
     9470
     10111
     10121
     10131
     11517
     12295
     (12321 . 12330)
     (12344 . 12347)
     (12690 . 12694)
     (12832 . 12842)
     (12872 . 12880)
     (12881 . 12896)
     (12928 . 12938)
     (12977 . 12992)
     13317
     13443
     14378
     15181
     19968
     19971
     19975
     19977
     20061
     20108
     20116
     20118
     (20159 . 20161)
     20191
     20200
     20237
     20336
     20740
     20806
     20841
     20843
     20845
     21313
     (21315 . 21318)
     21324
     (21441 . 21445)
     22235
     22769
     22777
     24186
     (24318 . 24320)
     (24332 . 24335)
     24336
     25342
     25420
     26578
     28422
     29590
     30334
     32902
     33836
     36014
     36019
     36144
     38433
     38470
     38476
     38520
     38646
     (42726 . 42736)
     (43056 . 43062)
     63851
     63859
     63864
     63922
     63953
     63955
     63997
     (65799 . 65844)
     (65856 . 65913)
     (65930 . 65932)
     (66273 . 66300)
     (66336 . 66340)
     66369
     66378
     (66513 . 66518)
     (67672 . 67680)
     (67705 . 67712)
     (67751 . 67760)
     (67835 . 67840)
     (67862 . 67868)
     (68028 . 68030)
     (68032 . 68048)
     (68050 . 68096)
     (68164 . 68169)
     (68221 . 68223)
     (68253 . 68256)
     (68331 . 68336)
     (68440 . 68448)
     (68472 . 68480)
     (68521 . 68528)
     (68858 . 68864)
     (69225 . 69247)
     (69405 . 69415)
     (69457 . 69461)
     (69573 . 69580)
     (69723 . 69734)
     (70113 . 70133)
     (71482 . 71484)
     (71914 . 71923)
     (72794 . 72813)
     (73664 . 73685)
     (74752 . 74863)
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
     (127243 . 127245)
     131073
     131172
     131298
     131361
     133418
     133507
     133516
     133532
     133866
     133885
     133913
     140176
     141720
     146203
     156269
     194704)))
