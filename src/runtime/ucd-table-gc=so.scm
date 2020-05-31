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

;;;; UCD property: gc=So

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-gc=symbol:other? char)
  (char-in-set? char char-set:gc=symbol:other))

(define-deferred char-set:gc=symbol:other
  (char-set*
   '(166
     169
     174
     176
     1154
     (1421 . 1423)
     (1550 . 1552)
     1758
     1769
     (1789 . 1791)
     2038
     2554
     2928
     (3059 . 3065)
     3066
     3199
     3407
     3449
     (3841 . 3844)
     3859
     (3861 . 3864)
     (3866 . 3872)
     3892
     3894
     3896
     (4030 . 4038)
     (4039 . 4045)
     (4046 . 4048)
     (4053 . 4057)
     (4254 . 4256)
     (5008 . 5018)
     6464
     (6622 . 6656)
     (7009 . 7019)
     (7028 . 7037)
     (8448 . 8450)
     (8451 . 8455)
     (8456 . 8458)
     8468
     (8470 . 8472)
     (8478 . 8484)
     8485
     8487
     8489
     8494
     (8506 . 8508)
     8522
     (8524 . 8526)
     8527
     (8586 . 8588)
     (8597 . 8602)
     (8604 . 8608)
     (8609 . 8611)
     (8612 . 8614)
     (8615 . 8622)
     (8623 . 8654)
     (8656 . 8658)
     8659
     (8661 . 8692)
     (8960 . 8968)
     (8972 . 8992)
     (8994 . 9001)
     (9003 . 9084)
     (9085 . 9115)
     (9140 . 9180)
     (9186 . 9215)
     (9216 . 9255)
     (9280 . 9291)
     (9372 . 9450)
     (9472 . 9655)
     (9656 . 9665)
     (9666 . 9720)
     (9728 . 9839)
     (9840 . 10088)
     (10132 . 10176)
     (10240 . 10496)
     (11008 . 11056)
     (11077 . 11079)
     (11085 . 11124)
     (11126 . 11158)
     (11160 . 11194)
     (11197 . 11209)
     (11210 . 11218)
     (11244 . 11248)
     (11493 . 11499)
     (11904 . 11930)
     (11931 . 12020)
     (12032 . 12246)
     (12272 . 12284)
     12292
     (12306 . 12308)
     12320
     (12342 . 12344)
     (12350 . 12352)
     (12688 . 12690)
     (12694 . 12704)
     (12736 . 12772)
     (12800 . 12831)
     (12842 . 12872)
     12880
     (12896 . 12928)
     (12938 . 12977)
     (12992 . 13055)
     (13056 . 13312)
     (19904 . 19968)
     (42128 . 42183)
     (43048 . 43052)
     (43062 . 43064)
     43065
     (43639 . 43642)
     65021
     65508
     65512
     (65517 . 65519)
     (65532 . 65534)
     (65847 . 65856)
     (65913 . 65930)
     (65932 . 65935)
     (65936 . 65948)
     65952
     (66000 . 66045)
     (67703 . 67705)
     68296
     71487
     (92988 . 92992)
     92997
     113820
     (118784 . 119030)
     (119040 . 119079)
     (119081 . 119141)
     (119146 . 119149)
     (119171 . 119173)
     (119180 . 119210)
     (119214 . 119273)
     (119296 . 119362)
     119365
     (119552 . 119639)
     (120832 . 121344)
     (121399 . 121403)
     (121453 . 121461)
     (121462 . 121476)
     (121477 . 121479)
     (126976 . 127020)
     (127024 . 127124)
     (127136 . 127151)
     (127153 . 127168)
     (127169 . 127184)
     (127185 . 127222)
     (127248 . 127279)
     (127280 . 127340)
     (127344 . 127405)
     (127462 . 127491)
     (127504 . 127548)
     (127552 . 127561)
     (127568 . 127570)
     (127744 . 127995)
     (128000 . 128723)
     (128736 . 128749)
     (128752 . 128759)
     (128768 . 128884)
     (128896 . 128981)
     (129024 . 129036)
     (129040 . 129096)
     (129104 . 129114)
     (129120 . 129160)
     (129168 . 129198)
     (129296 . 129311)
     (129312 . 129320)
     129328
     (129331 . 129343)
     (129344 . 129356)
     (129360 . 129375)
     (129408 . 129426)
     129472)))
