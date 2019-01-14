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

;;;; UCD property: Cased (cased)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-cased? char)
  (char-in-set? char char-set:cased))

(define-deferred char-set:cased
  (char-set*
   '((65 . 91)         (97 . 123)        170               181               186               (192 . 215)       (216 . 247)       (248 . 443)       (444 . 448)       (452 . 660)       (661 . 697)       (704 . 706)       (736 . 741)       837               (880 . 884)       (886 . 888)       (890 . 894)       895             902             (904 . 907)     908   (910 . 930) (931 . 1014)    (1015 . 1154)   (1162 . 1328)   (1329 . 1367)   (1377 . 1416)   (4256 . 4294)   4295            4301            (5024 . 5110)   (5112 . 5118)   (7296 . 7305)   (7424 . 7616)   (7680 . 7958)   (7960 . 7966)   (7968 . 8006)   (8008 . 8014)   (8016 . 8024)   8025            8027            8029            (8031 . 8062)     (8064 . 8117)     (8118 . 8125)     8126   (8130 . 8133)     (8134 . 8141)     (8144 . 8148)     (8150 . 8156) (8160 . 8173)     (8178 . 8181)     (8182 . 8189)     8305              8319              (8336 . 8349)     8450              8455              (8458 . 8468)
     8469              (8473 . 8478)     8484              8486              8488              (8490 . 8494)     (8495 . 8501)     8505              (8508 . 8512)     (8517 . 8522)     8526              (8544 . 8576)     (8579 . 8581)     (9398 . 9450)     (11264 . 11311)   (11312 . 11359)   (11360 . 11493)   (11499 . 11503) (11506 . 11508) (11520 . 11558) 11559 11565       (42560 . 42606) (42624 . 42654) (42786 . 42888) (42891 . 42895) (42896 . 42927) (42928 . 42936) (43000 . 43003) (43824 . 43867) (43868 . 43878) (43888 . 43968) (64256 . 64263) (64275 . 64280) (65313 . 65339) (65345 . 65371) (66560 . 66640) (66736 . 66772) (66776 . 66812) (68736 . 68787) (68800 . 68851) (71840 . 71904) (119808 . 119893) (119894 . 119965) (119966 . 119968) 119970 (119973 . 119975) (119977 . 119981) (119982 . 119994) 119995        (119997 . 120004) (120005 . 120070) (120071 . 120075) (120077 . 120085) (120086 . 120093) (120094 . 120122) (120123 . 120127) (120128 . 120133) 120134
     (120138 . 120145) (120146 . 120486) (120488 . 120513) (120514 . 120539) (120540 . 120571) (120572 . 120597) (120598 . 120629) (120630 . 120655) (120656 . 120687) (120688 . 120713) (120714 . 120745) (120746 . 120771) (120772 . 120780) (125184 . 125252) (127280 . 127306) (127312 . 127338) (127344 . 127370))))
