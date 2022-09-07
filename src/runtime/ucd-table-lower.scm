#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; UCD property: Lower (lower-case)

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define (char-lower-case? char)
  (char-in-set? char char-set:lower-case))

(define-deferred char-set:lower-case
  (char-set*
   '((97 . 123)        170               181               186               (223 . 247)       (248 . 256)       257               259               261               263               265               267               269               271               273               275               277               279               281               283               285    287               289   291   293   295   297             299             301   303             305           307   309   (311 . 313)   314           316   318         320   322   324   326   (328 . 330) 331   333   335   337   339   341   343   345   347             349             351             353             355             357             359             361             363             365             367             369             371             373               375               378               380               (382 . 385)       387           389
     392               (396 . 398)       402               405               (409 . 412)       414               417               419               421               424               (426 . 428)       429               432               436               438               (441 . 443)       (445 . 448)       454               457               460               462    464               466   468   470   472   474             (476 . 478)     479   481             483           485   487   489           491           493   (495 . 497) 499   501   505   507   509         511   513   515   517   519   521   523   525   527             529             531             533             535             537             539             541             543             545             547             549             551             553               555               557               559               561               (563 . 570)   572
     (575 . 577)       578               583               585               587               589               (591 . 660)       (661 . 697)       (704 . 706)       (736 . 741)       837               881               883               887               (890 . 894)       912               (940 . 975)       (976 . 978)       (981 . 984)       985               987    989               991   993   995   997   999             1001            1003  1005            (1007 . 1012) 1013  1016  (1019 . 1021) (1072 . 1120) 1121  1123        1125  1127  1129  1131  1133        1135  1137  1139  1141  1143  1145  1147  1149  1151            1153            1163            1165            1167            1169            1171            1173            1175            1177            1179            1181            1183            1185              1187              1189              1191              1193              1195          1197
     1199              1201              1203              1205              1207              1209              1211              1213              1215              1218              1220              1222              1224              1226              1228              (1230 . 1232)     1233              1235              1237              1239              1241   1243              1245  1247  1249  1251  1253            1255            1257  1259            1261          1263  1265  1267          1269          1271  1273        1275  1277  1279  1281  1283        1285  1287  1289  1291  1293  1295  1297  1299  1301            1303            1305            1307            1309            1311            1313            1315            1317            1319            1321            1323            1325            1327              (1376 . 1417)     (4304 . 4347)     (4349 . 4352)     (5112 . 5118)     (7296 . 7305) (7424 . 7616)
     7681              7683              7685              7687              7689              7691              7693              7695              7697              7699              7701              7703              7705              7707              7709              7711              7713              7715              7717              7719              7721   7723              7725  7727  7729  7731  7733            7735            7737  7739            7741          7743  7745  7747          7749          7751  7753        7755  7757  7759  7761  7763        7765  7767  7769  7771  7773  7775  7777  7779  7781            7783            7785            7787            7789            7791            7793            7795            7797            7799            7801            7803            7805            7807              7809              7811              7813              7815              7817          7819
     7821              7823              7825              7827              (7829 . 7838)     7839              7841              7843              7845              7847              7849              7851              7853              7855              7857              7859              7861              7863              7865              7867              7869   7871              7873  7875  7877  7879  7881            7883            7885  7887            7889          7891  7893  7895          7897          7899  7901        7903  7905  7907  7909  7911        7913  7915  7917  7919  7921  7923  7925  7927  7929            7931            7933            (7935 . 7944)   (7952 . 7958)   (7968 . 7976)   (7984 . 7992)   (8000 . 8006)   (8016 . 8024)   (8032 . 8040)   (8048 . 8062)   (8064 . 8072)   (8080 . 8088)   (8096 . 8104)     (8112 . 8117)     (8118 . 8120)     8126              (8130 . 8133)     (8134 . 8136) (8144 . 8148)
     (8150 . 8152)     (8160 . 8168)     (8178 . 8181)     (8182 . 8184)     8305              8319              (8336 . 8349)     8458              (8462 . 8464)     8467              8495              8500              8505              (8508 . 8510)     (8518 . 8522)     8526              (8560 . 8576)     8580              (9424 . 9450)     (11312 . 11359)   11361  (11365 . 11367)   11368 11370 11372 11377 (11379 . 11381) (11382 . 11390) 11393 11395           11397         11399 11401 11403         11405         11407 11409       11411 11413 11415 11417 11419       11421 11423 11425 11427 11429 11431 11433 11435 11437           11439           11441           11443           11445           11447           11449           11451           11453           11455           11457           11459           11461           11463             11465             11467             11469             11471             11473         11475
     11477             11479             11481             11483             11485             11487             11489             (11491 . 11493)   11500             11502             11507             (11520 . 11558)   11559             11565             42561             42563             42565             42567             42569             42571             42573  42575             42577 42579 42581 42583 42585           42587           42589 42591           42593         42595 42597 42599         42601         42603 42605       42625 42627 42629 42631 42633       42635 42637 42639 42641 42643 42645 42647 42649 (42651 . 42654) 42787           42789           42791           42793           42795           42797           (42799 . 42802) 42803           42805           42807           42809           42811           42813             42815             42817             42819             42821             42823         42825
     42827             42829             42831             42833             42835             42837             42839             42841             42843             42845             42847             42849             42851             42853             42855             42857             42859             42861             (42863 . 42873)   42874             42876  42879             42881 42883 42885 42887 42892           42894           42897 (42899 . 42902) 42903         42905 42907 42909         42911         42913 42915       42917 42919 42921 42927 42933       42935 42937 42939 42941 42943 42947 42952 42954 42998           (43000 . 43003) (43824 . 43867) (43868 . 43881) (43888 . 43968) (64256 . 64263) (64275 . 64280) (65345 . 65371) (66600 . 66640) (66776 . 66812) (68800 . 68851) (71872 . 71904) (93792 . 93824) (119834 . 119860) (119886 . 119893) (119894 . 119912) (119938 . 119964) (119990 . 119994) 119995        (119997 . 120004)
     (120005 . 120016) (120042 . 120068) (120094 . 120120) (120146 . 120172) (120198 . 120224) (120250 . 120276) (120302 . 120328) (120354 . 120380) (120406 . 120432) (120458 . 120486) (120514 . 120539) (120540 . 120546) (120572 . 120597) (120598 . 120604) (120630 . 120655) (120656 . 120662) (120688 . 120713) (120714 . 120720) (120746 . 120771) (120772 . 120778) 120779 (125218 . 125252))))
