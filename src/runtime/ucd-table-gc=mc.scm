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

;;;; UCD property: gc=Mc

;;; Generated from Unicode 10.0.0

(declare (usual-integrations))

(define (char-gc=mark:spacing-combining? char)
  (char-in-set? char char-set:gc=mark:spacing-combining))

(define-deferred char-set:gc=mark:spacing-combining
  (char-set*
   '(2307
     2363
     (2366 . 2369)
     (2377 . 2381)
     (2382 . 2384)
     (2434 . 2436)
     (2494 . 2497)
     (2503 . 2505)
     (2507 . 2509)
     2519
     2563
     (2622 . 2625)
     2691
     (2750 . 2753)
     2761
     (2763 . 2765)
     (2818 . 2820)
     2878
     2880
     (2887 . 2889)
     (2891 . 2893)
     2903
     (3006 . 3008)
     (3009 . 3011)
     (3014 . 3017)
     (3018 . 3021)
     3031
     (3073 . 3076)
     (3137 . 3141)
     (3202 . 3204)
     3262
     (3264 . 3269)
     (3271 . 3273)
     (3274 . 3276)
     (3285 . 3287)
     (3330 . 3332)
     (3390 . 3393)
     (3398 . 3401)
     (3402 . 3405)
     3415
     (3458 . 3460)
     (3535 . 3538)
     (3544 . 3552)
     (3570 . 3572)
     (3902 . 3904)
     3967
     (4139 . 4141)
     4145
     4152
     (4155 . 4157)
     (4182 . 4184)
     (4194 . 4197)
     (4199 . 4206)
     (4227 . 4229)
     (4231 . 4237)
     4239
     (4250 . 4253)
     6070
     (6078 . 6086)
     (6087 . 6089)
     (6435 . 6439)
     (6441 . 6444)
     (6448 . 6450)
     (6451 . 6457)
     (6681 . 6683)
     6741
     6743
     6753
     (6755 . 6757)
     (6765 . 6771)
     6916
     6965
     6971
     (6973 . 6978)
     (6979 . 6981)
     7042
     7073
     (7078 . 7080)
     7082
     7143
     (7146 . 7149)
     7150
     (7154 . 7156)
     (7204 . 7212)
     (7220 . 7222)
     7393
     (7410 . 7412)
     7415
     (12334 . 12336)
     (43043 . 43045)
     43047
     (43136 . 43138)
     (43188 . 43204)
     (43346 . 43348)
     43395
     (43444 . 43446)
     (43450 . 43452)
     (43453 . 43457)
     (43567 . 43569)
     (43571 . 43573)
     43597
     43643
     43645
     43755
     (43758 . 43760)
     43765
     (44003 . 44005)
     (44006 . 44008)
     (44009 . 44011)
     44012
     69632
     69634
     69762
     (69808 . 69811)
     (69815 . 69817)
     69932
     70018
     (70067 . 70070)
     (70079 . 70081)
     (70188 . 70191)
     (70194 . 70196)
     70197
     (70368 . 70371)
     (70402 . 70404)
     (70462 . 70464)
     (70465 . 70469)
     (70471 . 70473)
     (70475 . 70478)
     70487
     (70498 . 70500)
     (70709 . 70712)
     (70720 . 70722)
     70725
     (70832 . 70835)
     70841
     (70843 . 70847)
     70849
     (71087 . 71090)
     (71096 . 71100)
     71102
     (71216 . 71219)
     (71227 . 71229)
     71230
     71340
     (71342 . 71344)
     71350
     (71456 . 71458)
     71462
     (72199 . 72201)
     72249
     (72279 . 72281)
     72343
     72751
     72766
     72873
     72881
     72884
     (94033 . 94079)
     (119141 . 119143)
     (119149 . 119155))))
