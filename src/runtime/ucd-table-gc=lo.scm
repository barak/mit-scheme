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

;;;; UCD property: gc=Lo

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-gc=letter:other? char)
  (char-in-set? char char-set:gc=letter:other))

(define-deferred char-set:gc=letter:other
  (char-set*
   '(170
     186
     443
     (448 . 452)
     660
     (1488 . 1515)
     (1520 . 1523)
     (1568 . 1600)
     (1601 . 1611)
     (1646 . 1648)
     (1649 . 1748)
     1749
     (1774 . 1776)
     (1786 . 1789)
     1791
     1808
     (1810 . 1840)
     (1869 . 1958)
     1969
     (1994 . 2027)
     (2048 . 2070)
     (2112 . 2137)
     (2208 . 2229)
     (2230 . 2238)
     (2308 . 2362)
     2365
     2384
     (2392 . 2402)
     (2418 . 2433)
     (2437 . 2445)
     (2447 . 2449)
     (2451 . 2473)
     (2474 . 2481)
     2482
     (2486 . 2490)
     2493
     2510
     (2524 . 2526)
     (2527 . 2530)
     (2544 . 2546)
     (2565 . 2571)
     (2575 . 2577)
     (2579 . 2601)
     (2602 . 2609)
     (2610 . 2612)
     (2613 . 2615)
     (2616 . 2618)
     (2649 . 2653)
     2654
     (2674 . 2677)
     (2693 . 2702)
     (2703 . 2706)
     (2707 . 2729)
     (2730 . 2737)
     (2738 . 2740)
     (2741 . 2746)
     2749
     2768
     (2784 . 2786)
     2809
     (2821 . 2829)
     (2831 . 2833)
     (2835 . 2857)
     (2858 . 2865)
     (2866 . 2868)
     (2869 . 2874)
     2877
     (2908 . 2910)
     (2911 . 2914)
     2929
     2947
     (2949 . 2955)
     (2958 . 2961)
     (2962 . 2966)
     (2969 . 2971)
     2972
     (2974 . 2976)
     (2979 . 2981)
     (2984 . 2987)
     (2990 . 3002)
     3024
     (3077 . 3085)
     (3086 . 3089)
     (3090 . 3113)
     (3114 . 3130)
     3133
     (3160 . 3163)
     (3168 . 3170)
     3200
     (3205 . 3213)
     (3214 . 3217)
     (3218 . 3241)
     (3242 . 3252)
     (3253 . 3258)
     3261
     3294
     (3296 . 3298)
     (3313 . 3315)
     (3333 . 3341)
     (3342 . 3345)
     (3346 . 3387)
     3389
     3406
     (3412 . 3415)
     (3423 . 3426)
     (3450 . 3456)
     (3461 . 3479)
     (3482 . 3506)
     (3507 . 3516)
     3517
     (3520 . 3527)
     (3585 . 3633)
     (3634 . 3636)
     (3648 . 3654)
     (3713 . 3715)
     3716
     (3719 . 3721)
     3722
     3725
     (3732 . 3736)
     (3737 . 3744)
     (3745 . 3748)
     3749
     3751
     (3754 . 3756)
     (3757 . 3761)
     (3762 . 3764)
     3773
     (3776 . 3781)
     (3804 . 3808)
     3840
     (3904 . 3912)
     (3913 . 3949)
     (3976 . 3981)
     (4096 . 4139)
     4159
     (4176 . 4182)
     (4186 . 4190)
     4193
     (4197 . 4199)
     (4206 . 4209)
     (4213 . 4226)
     4238
     (4304 . 4347)
     (4349 . 4681)
     (4682 . 4686)
     (4688 . 4695)
     4696
     (4698 . 4702)
     (4704 . 4745)
     (4746 . 4750)
     (4752 . 4785)
     (4786 . 4790)
     (4792 . 4799)
     4800
     (4802 . 4806)
     (4808 . 4823)
     (4824 . 4881)
     (4882 . 4886)
     (4888 . 4955)
     (4992 . 5008)
     (5121 . 5741)
     (5743 . 5760)
     (5761 . 5787)
     (5792 . 5867)
     (5873 . 5881)
     (5888 . 5901)
     (5902 . 5906)
     (5920 . 5938)
     (5952 . 5970)
     (5984 . 5997)
     (5998 . 6001)
     (6016 . 6068)
     6108
     (6176 . 6211)
     (6212 . 6264)
     (6272 . 6277)
     (6279 . 6313)
     6314
     (6320 . 6390)
     (6400 . 6431)
     (6480 . 6510)
     (6512 . 6517)
     (6528 . 6572)
     (6576 . 6602)
     (6656 . 6679)
     (6688 . 6741)
     (6917 . 6964)
     (6981 . 6988)
     (7043 . 7073)
     (7086 . 7088)
     (7098 . 7142)
     (7168 . 7204)
     (7245 . 7248)
     (7258 . 7288)
     (7401 . 7405)
     (7406 . 7410)
     (7413 . 7415)
     (8501 . 8505)
     (11568 . 11624)
     (11648 . 11671)
     (11680 . 11687)
     (11688 . 11695)
     (11696 . 11703)
     (11704 . 11711)
     (11712 . 11719)
     (11720 . 11727)
     (11728 . 11735)
     (11736 . 11743)
     12294
     12348
     (12353 . 12439)
     12447
     (12449 . 12539)
     12543
     (12549 . 12590)
     (12593 . 12687)
     (12704 . 12731)
     (12784 . 12800)
     (13312 . 19894)
     (19968 . 40918)
     (40960 . 40981)
     (40982 . 42125)
     (42192 . 42232)
     (42240 . 42508)
     (42512 . 42528)
     (42538 . 42540)
     42606
     (42656 . 42726)
     42895
     42999
     (43003 . 43010)
     (43011 . 43014)
     (43015 . 43019)
     (43020 . 43043)
     (43072 . 43124)
     (43138 . 43188)
     (43250 . 43256)
     43259
     43261
     (43274 . 43302)
     (43312 . 43335)
     (43360 . 43389)
     (43396 . 43443)
     (43488 . 43493)
     (43495 . 43504)
     (43514 . 43519)
     (43520 . 43561)
     (43584 . 43587)
     (43588 . 43596)
     (43616 . 43632)
     (43633 . 43639)
     43642
     (43646 . 43696)
     43697
     (43701 . 43703)
     (43705 . 43710)
     43712
     43714
     (43739 . 43741)
     (43744 . 43755)
     43762
     (43777 . 43783)
     (43785 . 43791)
     (43793 . 43799)
     (43808 . 43815)
     (43816 . 43823)
     (43968 . 44003)
     (44032 . 55204)
     (55216 . 55239)
     (55243 . 55292)
     (63744 . 64110)
     (64112 . 64218)
     64285
     (64287 . 64297)
     (64298 . 64311)
     (64312 . 64317)
     64318
     (64320 . 64322)
     (64323 . 64325)
     (64326 . 64434)
     (64467 . 64830)
     (64848 . 64912)
     (64914 . 64968)
     (65008 . 65020)
     (65136 . 65141)
     (65142 . 65277)
     (65382 . 65392)
     (65393 . 65438)
     (65440 . 65471)
     (65474 . 65480)
     (65482 . 65488)
     (65490 . 65496)
     (65498 . 65501)
     (65536 . 65548)
     (65549 . 65575)
     (65576 . 65595)
     (65596 . 65598)
     (65599 . 65614)
     (65616 . 65630)
     (65664 . 65787)
     (66176 . 66205)
     (66208 . 66257)
     (66304 . 66336)
     (66352 . 66369)
     (66370 . 66378)
     (66384 . 66422)
     (66432 . 66462)
     (66464 . 66500)
     (66504 . 66512)
     (66640 . 66718)
     (66816 . 66856)
     (66864 . 66916)
     (67072 . 67383)
     (67392 . 67414)
     (67424 . 67432)
     (67584 . 67590)
     67592
     (67594 . 67638)
     (67639 . 67641)
     67644
     (67647 . 67670)
     (67680 . 67703)
     (67712 . 67743)
     (67808 . 67827)
     (67828 . 67830)
     (67840 . 67862)
     (67872 . 67898)
     (67968 . 68024)
     (68030 . 68032)
     68096
     (68112 . 68116)
     (68117 . 68120)
     (68121 . 68148)
     (68192 . 68221)
     (68224 . 68253)
     (68288 . 68296)
     (68297 . 68325)
     (68352 . 68406)
     (68416 . 68438)
     (68448 . 68467)
     (68480 . 68498)
     (68608 . 68681)
     (69635 . 69688)
     (69763 . 69808)
     (69840 . 69865)
     (69891 . 69927)
     (69968 . 70003)
     70006
     (70019 . 70067)
     (70081 . 70085)
     70106
     70108
     (70144 . 70162)
     (70163 . 70188)
     (70272 . 70279)
     70280
     (70282 . 70286)
     (70287 . 70302)
     (70303 . 70313)
     (70320 . 70367)
     (70405 . 70413)
     (70415 . 70417)
     (70419 . 70441)
     (70442 . 70449)
     (70450 . 70452)
     (70453 . 70458)
     70461
     70480
     (70493 . 70498)
     (70656 . 70709)
     (70727 . 70731)
     (70784 . 70832)
     (70852 . 70854)
     70855
     (71040 . 71087)
     (71128 . 71132)
     (71168 . 71216)
     71236
     (71296 . 71339)
     (71424 . 71450)
     71935
     (72384 . 72441)
     (72704 . 72713)
     (72714 . 72751)
     72768
     (72818 . 72848)
     (73728 . 74650)
     (74880 . 75076)
     (77824 . 78895)
     (82944 . 83527)
     (92160 . 92729)
     (92736 . 92767)
     (92880 . 92910)
     (92928 . 92976)
     (93027 . 93048)
     (93053 . 93072)
     (93952 . 94021)
     94032
     (94208 . 100333)
     (100352 . 101107)
     (110592 . 110594)
     (113664 . 113771)
     (113776 . 113789)
     (113792 . 113801)
     (113808 . 113818)
     (124928 . 125125)
     (126464 . 126468)
     (126469 . 126496)
     (126497 . 126499)
     126500
     126503
     (126505 . 126515)
     (126516 . 126520)
     126521
     126523
     126530
     126535
     126537
     126539
     (126541 . 126544)
     (126545 . 126547)
     126548
     126551
     126553
     126555
     126557
     126559
     (126561 . 126563)
     126564
     (126567 . 126571)
     (126572 . 126579)
     (126580 . 126584)
     (126585 . 126589)
     126590
     (126592 . 126602)
     (126603 . 126620)
     (126625 . 126628)
     (126629 . 126634)
     (126635 . 126652)
     (131072 . 173783)
     (173824 . 177973)
     (177984 . 178206)
     (178208 . 183970)
     (194560 . 195102))))
