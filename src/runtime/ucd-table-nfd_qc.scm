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

;;;; UCD property: NFD_QC (nfd-quick-check)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-nfd-quick-check? char)
  (char-in-set? char char-set:nfd-quick-check))

(define-deferred char-set:nfd-quick-check
  (char-set*
   '((0 . 192)       198             208             (215 . 217)     (222 . 224)   230             240           (247 . 249)   254             (272 . 274)     (294 . 296)     (305 . 308)     312           (319 . 323)     (329 . 332)   (338 . 340)   (358 . 360)   (383 . 416)   (418 . 431)   (433 . 461)     477           (484 . 486)     (497 . 500)     (502 . 504)     (540 . 542)     (544 . 550)   (564 . 832)     834              (837 . 884)       (885 . 894)       (895 . 901)        907           909   (913 . 938) (945 . 970) (975 . 979)     (981 . 1024)  1026          (1028 . 1031)   (1032 . 1036) (1039 . 1049) (1050 . 1081) (1082 . 1104) 1106            (1108 . 1111)   (1112 . 1116)   (1119 . 1142) (1144 . 1217) (1219 . 1232) (1236 . 1238) (1240 . 1242) (1248 . 1250) (1256 . 1258) (1270 . 1272) (1274 . 1570) (1575 . 1728) 1729          (1731 . 1747)   (1748 . 2345) (2346 . 2353) (2354 . 2356)   (2357 . 2392) (2400 . 2507) (2509 . 2524) 2526          (2528 . 2611)   (2612 . 2614)
     (2615 . 2649)   (2652 . 2654)   (2655 . 2888)   (2889 . 2891)   (2893 . 2908) (2910 . 2964)   (2965 . 3018) (3021 . 3144) (3145 . 3264)   (3265 . 3271)   3273            (3276 . 3402)   (3405 . 3546) 3547            (3551 . 3907) (3908 . 3917) (3918 . 3922) (3923 . 3927) (3928 . 3932) (3933 . 3945)   (3946 . 3955) 3956            3959            (3961 . 3969)   (3970 . 3987)   (3988 . 3997) (3998 . 4002)   (4003 . 4007)    (4008 . 4012)     (4013 . 4025)     (4026 . 4134)      (4135 . 6918) 6919  6921        6923        6925            (6927 . 6930) (6931 . 6971) 6972            (6974 . 6976) 6978          (6980 . 7680) 7834          (7836 . 7840)   (7930 . 7936)   (7958 . 7960)   (7966 . 7968) (8006 . 8008) (8014 . 8016) 8024          8026          8028          8030          (8062 . 8064) 8117          8125          (8127 . 8129) 8133            (8148 . 8150) 8156          (8176 . 8178)   8181          (8190 . 8192) (8194 . 8486) (8487 . 8490) (8492 . 8602)   (8604 . 8622)
     (8623 . 8653)   (8656 . 8708)   (8709 . 8713)   (8714 . 8716)   (8717 . 8740) 8741            (8743 . 8769) (8770 . 8772) (8773 . 8775)   8776            (8778 . 8800)   8801            (8803 . 8813) (8818 . 8820)   (8822 . 8824) (8826 . 8832) (8834 . 8836) (8838 . 8840) (8842 . 8876) (8880 . 8928)   (8932 . 8938) (8942 . 9001)   (9003 . 10972)  (10973 . 12364) 12365           12367         12369           12371            12373             12375             12377              12379         12381 12383       12385       (12387 . 12389) 12390         12392         (12394 . 12400) 12402         12405         12408         12411         (12414 . 12436) (12437 . 12446) (12447 . 12460) 12461         12463         12465         12467         12469         12471         12473         12475         12477         12479         12481         (12483 . 12485) 12486         12488         (12490 . 12496) 12498         12501         12504         12507         (12510 . 12532) (12533 . 12535)
     (12539 . 12542) (12543 . 44032) (55204 . 63744) (64014 . 64016) 64017         (64019 . 64021) 64031         64033         (64035 . 64037) (64039 . 64042) (64110 . 64112) (64218 . 64285) 64286         (64288 . 64298) 64311         64317         64319         64322         64325         (64335 . 69786) 69787         (69789 . 69803) (69804 . 69934) (69936 . 70475) (70477 . 70843) 70845         (70847 . 71098) (71100 . 119134) (119141 . 119227) (119233 . 194560) (195102 . 1114112))))