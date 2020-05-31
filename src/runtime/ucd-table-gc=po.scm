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

;;;; UCD property: gc=Po

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-gc=punctuation:other? char)
  (char-in-set? char char-set:gc=punctuation:other))

(define-deferred char-set:gc=punctuation:other
  (char-set*
   '((33 . 36)
     (37 . 40)
     42
     44
     (46 . 48)
     (58 . 60)
     (63 . 65)
     92
     161
     167
     (182 . 184)
     191
     894
     903
     (1370 . 1376)
     1417
     1472
     1475
     1478
     (1523 . 1525)
     (1545 . 1547)
     (1548 . 1550)
     1563
     (1566 . 1568)
     (1642 . 1646)
     1748
     (1792 . 1806)
     (2039 . 2042)
     (2096 . 2111)
     2142
     (2404 . 2406)
     2416
     2800
     3572
     3663
     (3674 . 3676)
     (3844 . 3859)
     3860
     3973
     (4048 . 4053)
     (4057 . 4059)
     (4170 . 4176)
     4347
     (4960 . 4969)
     (5741 . 5743)
     (5867 . 5870)
     (5941 . 5943)
     (6100 . 6103)
     (6104 . 6107)
     (6144 . 6150)
     (6151 . 6155)
     (6468 . 6470)
     (6686 . 6688)
     (6816 . 6823)
     (6824 . 6830)
     (7002 . 7009)
     (7164 . 7168)
     (7227 . 7232)
     (7294 . 7296)
     (7360 . 7368)
     7379
     (8214 . 8216)
     (8224 . 8232)
     (8240 . 8249)
     (8251 . 8255)
     (8257 . 8260)
     (8263 . 8274)
     8275
     (8277 . 8287)
     (11513 . 11517)
     (11518 . 11520)
     11632
     (11776 . 11778)
     (11782 . 11785)
     11787
     (11790 . 11799)
     (11800 . 11802)
     11803
     (11806 . 11808)
     (11818 . 11823)
     (11824 . 11834)
     (11836 . 11840)
     11841
     (11843 . 11845)
     (12289 . 12292)
     12349
     12539
     (42238 . 42240)
     (42509 . 42512)
     42611
     42622
     (42738 . 42744)
     (43124 . 43128)
     (43214 . 43216)
     (43256 . 43259)
     43260
     (43310 . 43312)
     43359
     (43457 . 43470)
     (43486 . 43488)
     (43612 . 43616)
     (43742 . 43744)
     (43760 . 43762)
     44011
     (65040 . 65047)
     65049
     65072
     (65093 . 65095)
     (65097 . 65101)
     (65104 . 65107)
     (65108 . 65112)
     (65119 . 65122)
     65128
     (65130 . 65132)
     (65281 . 65284)
     (65285 . 65288)
     65290
     65292
     (65294 . 65296)
     (65306 . 65308)
     (65311 . 65313)
     65340
     65377
     (65380 . 65382)
     (65792 . 65795)
     66463
     66512
     66927
     67671
     67871
     67903
     (68176 . 68185)
     68223
     (68336 . 68343)
     (68409 . 68416)
     (68505 . 68509)
     (69703 . 69710)
     (69819 . 69821)
     (69822 . 69826)
     (69952 . 69956)
     (70004 . 70006)
     (70085 . 70090)
     70093
     70107
     (70109 . 70112)
     (70200 . 70206)
     70313
     (70731 . 70736)
     70747
     70749
     70854
     (71105 . 71128)
     (71233 . 71236)
     (71264 . 71277)
     (71484 . 71487)
     (72769 . 72774)
     (72816 . 72818)
     (74864 . 74869)
     (92782 . 92784)
     92917
     (92983 . 92988)
     92996
     113823
     (121479 . 121484)
     (125278 . 125280))))
