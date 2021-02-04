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

;;;; UCD property: gc=Cn

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-gc=other:not-assigned? char)
  (char-in-set? char char-set:gc=other:not-assigned))

(define-deferred char-set:gc=other:not-assigned
  (char-set*
   '((888 . 890)
     (896 . 900)
     907
     909
     930
     1328
     (1367 . 1369)
     1376
     1416
     (1419 . 1421)
     1424
     (1480 . 1488)
     (1515 . 1520)
     (1525 . 1536)
     1565
     1806
     (1867 . 1869)
     (1970 . 1984)
     (2043 . 2048)
     (2094 . 2096)
     2111
     (2140 . 2142)
     (2143 . 2208)
     2229
     (2238 . 2260)
     2436
     (2445 . 2447)
     (2449 . 2451)
     2473
     2481
     (2483 . 2486)
     (2490 . 2492)
     (2501 . 2503)
     (2505 . 2507)
     (2511 . 2519)
     (2520 . 2524)
     2526
     (2532 . 2534)
     (2556 . 2561)
     2564
     (2571 . 2575)
     (2577 . 2579)
     2601
     2609
     2612
     2615
     (2618 . 2620)
     2621
     (2627 . 2631)
     (2633 . 2635)
     (2638 . 2641)
     (2642 . 2649)
     2653
     (2655 . 2662)
     (2678 . 2689)
     2692
     2702
     2706
     2729
     2737
     2740
     (2746 . 2748)
     2758
     2762
     (2766 . 2768)
     (2769 . 2784)
     (2788 . 2790)
     (2802 . 2809)
     (2810 . 2817)
     2820
     (2829 . 2831)
     (2833 . 2835)
     2857
     2865
     2868
     (2874 . 2876)
     (2885 . 2887)
     (2889 . 2891)
     (2894 . 2902)
     (2904 . 2908)
     2910
     (2916 . 2918)
     (2936 . 2946)
     2948
     (2955 . 2958)
     2961
     (2966 . 2969)
     2971
     2973
     (2976 . 2979)
     (2981 . 2984)
     (2987 . 2990)
     (3002 . 3006)
     (3011 . 3014)
     3017
     (3022 . 3024)
     (3025 . 3031)
     (3032 . 3046)
     (3067 . 3072)
     3076
     3085
     3089
     3113
     (3130 . 3133)
     3141
     3145
     (3150 . 3157)
     3159
     (3163 . 3168)
     (3172 . 3174)
     (3184 . 3192)
     3204
     3213
     3217
     3241
     3252
     (3258 . 3260)
     3269
     3273
     (3278 . 3285)
     (3287 . 3294)
     3295
     (3300 . 3302)
     3312
     (3315 . 3329)
     3332
     3341
     3345
     (3387 . 3389)
     3397
     3401
     (3408 . 3412)
     (3428 . 3430)
     (3456 . 3458)
     3460
     (3479 . 3482)
     3506
     3516
     (3518 . 3520)
     (3527 . 3530)
     (3531 . 3535)
     3541
     3543
     (3552 . 3558)
     (3568 . 3570)
     (3573 . 3585)
     (3643 . 3647)
     (3676 . 3713)
     3715
     (3717 . 3719)
     3721
     (3723 . 3725)
     (3726 . 3732)
     3736
     3744
     3748
     3750
     (3752 . 3754)
     3756
     3770
     (3774 . 3776)
     3781
     3783
     (3790 . 3792)
     (3802 . 3804)
     (3808 . 3840)
     3912
     (3949 . 3953)
     3992
     4029
     4045
     (4059 . 4096)
     4294
     (4296 . 4301)
     (4302 . 4304)
     4681
     (4686 . 4688)
     4695
     4697
     (4702 . 4704)
     4745
     (4750 . 4752)
     4785
     (4790 . 4792)
     4799
     4801
     (4806 . 4808)
     4823
     4881
     (4886 . 4888)
     (4955 . 4957)
     (4989 . 4992)
     (5018 . 5024)
     (5110 . 5112)
     (5118 . 5120)
     (5789 . 5792)
     (5881 . 5888)
     5901
     (5909 . 5920)
     (5943 . 5952)
     (5972 . 5984)
     5997
     6001
     (6004 . 6016)
     (6110 . 6112)
     (6122 . 6128)
     (6138 . 6144)
     6159
     (6170 . 6176)
     (6264 . 6272)
     (6315 . 6320)
     (6390 . 6400)
     6431
     (6444 . 6448)
     (6460 . 6464)
     (6465 . 6468)
     (6510 . 6512)
     (6517 . 6528)
     (6572 . 6576)
     (6602 . 6608)
     (6619 . 6622)
     (6684 . 6686)
     6751
     (6781 . 6783)
     (6794 . 6800)
     (6810 . 6816)
     (6830 . 6832)
     (6847 . 6912)
     (6988 . 6992)
     (7037 . 7040)
     (7156 . 7164)
     (7224 . 7227)
     (7242 . 7245)
     (7305 . 7360)
     (7368 . 7376)
     7415
     (7418 . 7424)
     (7670 . 7675)
     (7958 . 7960)
     (7966 . 7968)
     (8006 . 8008)
     (8014 . 8016)
     8024
     8026
     8028
     8030
     (8062 . 8064)
     8117
     8133
     (8148 . 8150)
     8156
     (8176 . 8178)
     8181
     8191
     8293
     (8306 . 8308)
     8335
     (8349 . 8352)
     (8383 . 8400)
     (8433 . 8448)
     (8588 . 8592)
     9215
     (9255 . 9280)
     (9291 . 9312)
     (11124 . 11126)
     (11158 . 11160)
     (11194 . 11197)
     11209
     (11218 . 11244)
     (11248 . 11264)
     11311
     11359
     (11508 . 11513)
     11558
     (11560 . 11565)
     (11566 . 11568)
     (11624 . 11631)
     (11633 . 11647)
     (11671 . 11680)
     11687
     11695
     11703
     11711
     11719
     11727
     11735
     11743
     (11845 . 11904)
     11930
     (12020 . 12032)
     (12246 . 12272)
     (12284 . 12288)
     12352
     (12439 . 12441)
     (12544 . 12549)
     (12590 . 12593)
     12687
     (12731 . 12736)
     (12772 . 12784)
     12831
     13055
     (19894 . 19904)
     (40918 . 40960)
     (42125 . 42128)
     (42183 . 42192)
     (42540 . 42560)
     (42744 . 42752)
     42927
     (42936 . 42999)
     (43052 . 43056)
     (43066 . 43072)
     (43128 . 43136)
     (43206 . 43214)
     (43226 . 43232)
     (43262 . 43264)
     (43348 . 43359)
     (43389 . 43392)
     43470
     (43482 . 43486)
     43519
     (43575 . 43584)
     (43598 . 43600)
     (43610 . 43612)
     (43715 . 43739)
     (43767 . 43777)
     (43783 . 43785)
     (43791 . 43793)
     (43799 . 43808)
     43815
     43823
     (43878 . 43888)
     (44014 . 44016)
     (44026 . 44032)
     (55204 . 55216)
     (55239 . 55243)
     (55292 . 55296)
     (64110 . 64112)
     (64218 . 64256)
     (64263 . 64275)
     (64280 . 64285)
     64311
     64317
     64319
     64322
     64325
     (64450 . 64467)
     (64832 . 64848)
     (64912 . 64914)
     (64968 . 65008)
     (65022 . 65024)
     (65050 . 65056)
     65107
     65127
     (65132 . 65136)
     65141
     (65277 . 65279)
     65280
     (65471 . 65474)
     (65480 . 65482)
     (65488 . 65490)
     (65496 . 65498)
     (65501 . 65504)
     65511
     (65519 . 65529)
     (65534 . 65536)
     65548
     65575
     65595
     65598
     (65614 . 65616)
     (65630 . 65664)
     (65787 . 65792)
     (65795 . 65799)
     (65844 . 65847)
     65935
     (65948 . 65952)
     (65953 . 66000)
     (66046 . 66176)
     (66205 . 66208)
     (66257 . 66272)
     (66300 . 66304)
     (66340 . 66352)
     (66379 . 66384)
     (66427 . 66432)
     66462
     (66500 . 66504)
     (66518 . 66560)
     (66718 . 66720)
     (66730 . 66736)
     (66772 . 66776)
     (66812 . 66816)
     (66856 . 66864)
     (66916 . 66927)
     (66928 . 67072)
     (67383 . 67392)
     (67414 . 67424)
     (67432 . 67584)
     (67590 . 67592)
     67593
     67638
     (67641 . 67644)
     (67645 . 67647)
     67670
     (67743 . 67751)
     (67760 . 67808)
     67827
     (67830 . 67835)
     (67868 . 67871)
     (67898 . 67903)
     (67904 . 67968)
     (68024 . 68028)
     (68048 . 68050)
     68100
     (68103 . 68108)
     68116
     68120
     (68148 . 68152)
     (68155 . 68159)
     (68168 . 68176)
     (68185 . 68192)
     (68256 . 68288)
     (68327 . 68331)
     (68343 . 68352)
     (68406 . 68409)
     (68438 . 68440)
     (68467 . 68472)
     (68498 . 68505)
     (68509 . 68521)
     (68528 . 68608)
     (68681 . 68736)
     (68787 . 68800)
     (68851 . 68858)
     (68864 . 69216)
     (69247 . 69632)
     (69710 . 69714)
     (69744 . 69759)
     (69826 . 69840)
     (69865 . 69872)
     (69882 . 69888)
     69941
     (69956 . 69968)
     (70007 . 70016)
     (70094 . 70096)
     70112
     (70133 . 70144)
     70162
     (70207 . 70272)
     70279
     70281
     70286
     70302
     (70314 . 70320)
     (70379 . 70384)
     (70394 . 70400)
     70404
     (70413 . 70415)
     (70417 . 70419)
     70441
     70449
     70452
     (70458 . 70460)
     (70469 . 70471)
     (70473 . 70475)
     (70478 . 70480)
     (70481 . 70487)
     (70488 . 70493)
     (70500 . 70502)
     (70509 . 70512)
     (70517 . 70656)
     70746
     70748
     (70750 . 70784)
     (70856 . 70864)
     (70874 . 71040)
     (71094 . 71096)
     (71134 . 71168)
     (71237 . 71248)
     (71258 . 71264)
     (71277 . 71296)
     (71352 . 71360)
     (71370 . 71424)
     (71450 . 71453)
     (71468 . 71472)
     (71488 . 71840)
     (71923 . 71935)
     (71936 . 72384)
     (72441 . 72704)
     72713
     72759
     (72774 . 72784)
     (72813 . 72816)
     (72848 . 72850)
     72872
     (72887 . 73728)
     (74650 . 74752)
     74863
     (74869 . 74880)
     (75076 . 77824)
     (78895 . 82944)
     (83527 . 92160)
     (92729 . 92736)
     92767
     (92778 . 92782)
     (92784 . 92880)
     (92910 . 92912)
     (92918 . 92928)
     (92998 . 93008)
     93018
     93026
     (93048 . 93053)
     (93072 . 93952)
     (94021 . 94032)
     (94079 . 94095)
     (94112 . 94176)
     (94177 . 94208)
     (100333 . 100352)
     (101107 . 110592)
     (110594 . 113664)
     (113771 . 113776)
     (113789 . 113792)
     (113801 . 113808)
     (113818 . 113820)
     (113828 . 118784)
     (119030 . 119040)
     (119079 . 119081)
     (119273 . 119296)
     (119366 . 119552)
     (119639 . 119648)
     (119666 . 119808)
     119893
     119965
     (119968 . 119970)
     (119971 . 119973)
     (119975 . 119977)
     119981
     119994
     119996
     120004
     120070
     (120075 . 120077)
     120085
     120093
     120122
     120127
     120133
     (120135 . 120138)
     120145
     (120486 . 120488)
     (120780 . 120782)
     (121484 . 121499)
     121504
     (121520 . 122880)
     122887
     (122905 . 122907)
     122914
     122917
     (122923 . 124928)
     (125125 . 125127)
     (125143 . 125184)
     (125259 . 125264)
     (125274 . 125278)
     (125280 . 126464)
     126468
     126496
     126499
     (126501 . 126503)
     126504
     126515
     126520
     126522
     (126524 . 126530)
     (126531 . 126535)
     126536
     126538
     126540
     126544
     126547
     (126549 . 126551)
     126552
     126554
     126556
     126558
     126560
     126563
     (126565 . 126567)
     126571
     126579
     126584
     126589
     126591
     126602
     (126620 . 126625)
     126628
     126634
     (126652 . 126704)
     (126706 . 126976)
     (127020 . 127024)
     (127124 . 127136)
     (127151 . 127153)
     127168
     127184
     (127222 . 127232)
     (127245 . 127248)
     127279
     (127340 . 127344)
     (127405 . 127462)
     (127491 . 127504)
     (127548 . 127552)
     (127561 . 127568)
     (127570 . 127744)
     (128723 . 128736)
     (128749 . 128752)
     (128759 . 128768)
     (128884 . 128896)
     (128981 . 129024)
     (129036 . 129040)
     (129096 . 129104)
     (129114 . 129120)
     (129160 . 129168)
     (129198 . 129296)
     129311
     (129320 . 129328)
     (129329 . 129331)
     129343
     (129356 . 129360)
     (129375 . 129408)
     (129426 . 129472)
     (129473 . 131072)
     (173783 . 173824)
     (177973 . 177984)
     (178206 . 178208)
     (183970 . 194560)
     (195102 . 917505)
     (917506 . 917536)
     (917632 . 917760)
     (918000 . 983040)
     (1048574 . 1048576)
     (1114110 . 1114112))))
