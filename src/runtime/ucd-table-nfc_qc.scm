#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; UCD property: NFC_QC (nfc-quick-check)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-nfc-quick-check? char)
  (char-in-set? char char-set:nfc-quick-check))

(define-deferred char-set:nfc-quick-check
  (char-set*
   '((0 . 768)     773           (781 . 783)   784  786  (789 . 795) (796 . 803) (809 . 813) 815  (818 . 824)   (825 . 832)   (838 . 884)   (885 . 894) (895 . 903)   (904 . 1619)  (1622 . 2364) (2365 . 2392) (2400 . 2494) (2495 . 2519) (2520 . 2524) 2526 (2528 . 2611) (2612 . 2614) (2615 . 2649) (2652 . 2654) (2655 . 2878)  (2879 . 2902)   (2904 . 2908)   (2910 . 3006)   (3007 . 3031) (3032 . 3158)   (3159 . 3266) (3267 . 3285) (3287 . 3390)   (3391 . 3415)   (3416 . 3530)   (3531 . 3535)   (3536 . 3551) (3552 . 3907)   (3908 . 3917) (3918 . 3922) (3923 . 3927) (3928 . 3932) (3933 . 3945) (3946 . 3955)   3956            3959            (3961 . 3969)   (3970 . 3987)   (3988 . 3997)   (3998 . 4002)   (4003 . 4007)   (4008 . 4012)    (4013 . 4025)     (4026 . 4142)     (4143 . 4449)
     (4470 . 4520) (4547 . 6965) (6966 . 8049) 8050 8052 8054        8056        8058        8060 (8062 . 8123) (8124 . 8126) (8127 . 8137) 8138        (8140 . 8147) (8148 . 8155) (8156 . 8163) (8164 . 8171) (8172 . 8174) (8176 . 8185) 8186          8188 (8190 . 8192) (8194 . 8486) (8487 . 8490) (8492 . 9001) (9003 . 10972) (10973 . 12441) (12443 . 63744) (64014 . 64016) 64017         (64019 . 64021) 64031         64033         (64035 . 64037) (64039 . 64042) (64110 . 64112) (64218 . 64285) 64286         (64288 . 64298) 64311         64317         64319         64322         64325         (64335 . 69818) (69819 . 69927) (69928 . 70462) (70463 . 70487) (70488 . 70832) (70833 . 70842) (70843 . 70845) (70846 . 71087) (71088 . 119134) (119141 . 119227) (119233 . 194560) (195102 . 1114112))))
