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

;;;; UCD property: CWL (changes-when-lower-cased)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (char-changes-when-lower-cased? char)
  (char-in-set? char char-set:changes-when-lower-cased))

(define-deferred char-set:changes-when-lower-cased
  (char-set*
   '((65 . 91) (192 . 215) (216 . 223) 256   258             260   262   264   266   268   270   272         274   276         278   280   282   284   286   288   290   292   294   296             298             300   302             304             306             308             310             313               315   317   319   321   323           325           327           330           332           334           336           338           340           342   344           346   348           350   352           354             356   358             360   362   364         366             368         370         372             374   (376 . 378) 379   381   (385 . 387) 388   (390 . 392) (393 . 396) (398 . 402) (403 . 405) (406 . 409) (412 . 414) (415 . 417) 418   420   (422 . 424)   425           428   (430 . 432) (433 . 436) 437   (439 . 441) 444   (452 . 454)   (455 . 457)   (458 . 460) 461   463           465   467   469   471
     473       475         478         480   482             484   486   488   490   492   494   (497 . 499) 500   (502 . 505) 506   508   510   512   514   516   518   520   522   524             526             528   530             532             534             536             538             540               542   544   546   548   550           552           554           556           558           560           562           (570 . 572)   (573 . 575)   577   (579 . 583)   584   586           588   590           880             882   886             895   902   (904 . 907) 908             (910 . 912) (913 . 930) (931 . 940)     975   984         986   988   990         992   994         996         998         1000        1002        1004        1006        1012  1015  (1017 . 1019) (1021 . 1072) 1120  1122        1124        1126  1128        1130  1132          1134          1136        1138  1140          1142  1144  1146  1148
     1150      1152        1162        1164  1166            1168  1170  1172  1174  1176  1178  1180        1182  1184        1186  1188  1190  1192  1194  1196  1198  1200  1202  1204            1206            1208  1210            1212            1214            (1216 . 1218)   1219            1221              1223  1225  1227  1229  1232          1234          1236          1238          1240          1242          1244          1246          1248          1250  1252          1254  1256          1258  1260          1262            1264  1266            1268  1270  1272        1274            1276        1278        1280            1282  1284        1286  1288  1290        1292  1294        1296        1298        1300        1302        1304        1306        1308  1310  1312          1314          1316  1318        1320        1322  1324        1326  (1329 . 1367) (4256 . 4294) 4295        4301  (5024 . 5110) 7680  7682  7684  7686
     7688      7690        7692        7694  7696            7698  7700  7702  7704  7706  7708  7710        7712  7714        7716  7718  7720  7722  7724  7726  7728  7730  7732  7734            7736            7738  7740            7742            7744            7746            7748            7750              7752  7754  7756  7758  7760          7762          7764          7766          7768          7770          7772          7774          7776          7778  7780          7782  7784          7786  7788          7790            7792  7794            7796  7798  7800        7802            7804        7806        7808            7810  7812        7814  7816  7818        7820  7822        7824        7826        7828        7838        7840        7842        7844  7846  7848          7850          7852  7854        7856        7858  7860        7862  7864          7866          7868        7870  7872          7874  7876  7878  7880
     7882      7884        7886        7888  7890            7892  7894  7896  7898  7900  7902  7904        7906  7908        7910  7912  7914  7916  7918  7920  7922  7924  7926  7928            7930            7932  7934            (7944 . 7952)   (7960 . 7966)   (7976 . 7984)   (7992 . 8000)   (8008 . 8014)     8025  8027  8029  8031  (8040 . 8048) (8072 . 8080) (8088 . 8096) (8104 . 8112) (8120 . 8125) (8136 . 8141) (8152 . 8156) (8168 . 8173) (8184 . 8189) 8486  (8490 . 8492) 8498  (8544 . 8560) 8579  (9398 . 9424) (11264 . 11311) 11360 (11362 . 11365) 11367 11369 11371       (11373 . 11377) 11378       11381       (11390 . 11393) 11394 11396       11398 11400 11402       11404 11406       11408       11410       11412       11414       11416       11418       11420 11422 11424         11426         11428 11430       11432       11434 11436       11438 11440         11442         11444       11446 11448         11450 11452 11454 11456
     11458     11460       11462       11464 11466           11468 11470 11472 11474 11476 11478 11480       11482 11484       11486 11488 11490 11499 11501 11506 42560 42562 42564 42566           42568           42570 42572           42574           42576           42578           42580           42582             42584 42586 42588 42590 42592         42594         42596         42598         42600         42602         42604         42624         42626         42628 42630         42632 42634         42636 42638         42640           42642 42644           42646 42648 42650       42786           42788       42790       42792           42794 42796       42798 42802 42804       42806 42808       42810       42812       42814       42816       42818       42820       42822 42824 42826         42828         42830 42832       42834       42836 42838       42840 42842         42844         42846       42848 42850         42852 42854 42856 42858
     42860     42862       42873       42875 (42877 . 42879) 42880 42882 42884 42886 42891 42893 42896       42898 42902       42904 42906 42908 42910 42912 42914 42916 42918 42920 (42922 . 42927) (42928 . 42933) 42934 (65313 . 65339) (66560 . 66600) (66736 . 66772) (68736 . 68787) (71840 . 71872) (125184 . 125218))))
