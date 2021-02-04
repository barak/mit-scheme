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

;;;; Tests for regular expression compiler

(declare (usual-integrations))

(load-option 'regular-expression)

(define-test 'test-compiler
  (lambda ()
    (let ((compiled (re-compile-pattern regexp #f)))
      (assert-equal (compiled-regexp/byte-stream compiled)
                    (hexadecimal->bytevector compiled-regexp))
      (assert-equal (call-with-output-string
                      (lambda (port)
                        (re-disassemble-pattern compiled port)))
                    disassembled-output))))

(define regexp
  ".*\\([0-9][BEGKMPTY-Zk]?\\|)\\) +\\(\\(['A-Za-z\200-\377]['A-Za-z\200-\377]+\\.? +[ 0-3][0-9]\\|[ 0-3][0-9]\\.? ['A-Za-z\200-\377]['A-Za-z\200-\377]+\\.?\\) +\\([ 0-2][0-9][.:][0-5][0-9]\\|[0-9][0-9][0-9][0-9]\\)\\|['A-Za-z\200-\377]['A-Za-z\200-\377]+\\.? +[ 0-3][0-9], +[0-9][0-9][0-9][0-9]\\|\\([ 0-1]?[0-9][A-Za-z\200-\377]? [ 0-3][0-9][A-Za-z\200-\377]? +\\|[ 0-3][0-9] [ 0-1]?[0-9] +\\)\\([ 0-2][0-9][.:][0-5][0-9]\\|[0-9][0-9][0-9][0-9][A-Za-z\200-\377]?\\)\\|\\([0-9][0-9][0-9][0-9]-\\)?[0-1][0-9]-[0-3][0-9][ T][ 0-2][0-9][.:][0-5][0-9]\\(:[0-6][0-9]\\([,.][0-9]+\\)?\\( ?[-+][0-2][0-9][0-5][0-9]\\)?\\)?\\|[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]\\) +")

(define compiled-regexp
  "0604000a08f9ff0d010620000b08000000000000ff030610000b0e0000000000000000a428110600080503000201290e01090300060500012008f8ff0d020657010d030673000b200000000080000000feffff07feffff07ffffffffffffffffffffffffffffffff0903000625000b200000000080000000feffff07feffff07ffffffffffffffffffffffffffffffff08d8ff060200012e090300060500012008f8ff0b070000000001000f0b08000000000000ff03056d000b070000000001000f0b08000000000000ff03060200012e0201200b200000000080000000feffff07feffff07ffffffffffffffffffffffffffffffff0903000625000b200000000080000000feffff07feffff07ffffffffffffffffffffffffffffffff08d8ff060200012e0e03090300060500012008f8ff0d040633000b07000000000100070b08000000000000ff030b0800000000004000040b070000000000003f0b08000000000000ff030528000b08000000000000ff030b08000000000000ff030b08000000000000ff030b08000000000000ff030e0405a90006a9000b200000000080000000feffff07feffff07ffffffffffffffffffffffffffffffff0903000625000b200000000080000000feffff07feffff07ffffffffffffffffffffffffffffffff08d8ff060200012e090300060500012008f8ff0b070000000001000f0b08000000000000ff0302012c090300060500012008f8ff0b08000000000000ff030b08000000000000ff030b08000000000000ff030b08000000000000ff03054c01064c010d050684000609000b07000000000100030b08000000000000ff030622000b200000000000000000feffff07feffff07ffffffffffffffffffffffffffffffff0201200b070000000001000f0b08000000000000ff030622000b200000000000000000feffff07feffff07ffffffffffffffffffffffffffffffff090300060500012008f8ff0537000b070000000001000f0b08000000000000ff030201200609000b07000000000100030b08000000000000ff03090300060500012008f8ff0e050d060633000b07000000000100070b08000000000000ff030b0800000000004000040b070000000000003f0b08000000000000ff03054d000b08000000000000ff030b08000000000000ff030b08000000000000ff030b08000000000000ff030622000b200000000000000000feffff07feffff07ffffffffffffffffffffffffffffffff0e060510010610010d07062d000b08000000000000ff030b08000000000000ff030b08000000000000ff030b08000000000000ff0302012d0e070b07000000000000030b08000000000000ff0302012d0b070000000000000f0b08000000000000ff030b0b00000000010000000000100b07000000000100070b08000000000000ff030b0800000000004000040b070000000000003f0b08000000000000ff030d0806700002013a0b070000000000007f0b08000000000000ff030d09061d000b06000000000050090300060d000b08000000000000ff0308f0ff0e0906330006020001200b060000000000280b07000000000000070b08000000000000ff030b070000000000003f0b08000000000000ff030e080554000b08000000000000ff030b08000000000000ff030b08000000000000ff030b08000000000000ff0302012d0b07000000000000030b08000000000000ff0302012d0b070000000000000f0b08000000000000ff030e02090300060500012008f8ff")

(define disassembled-output
  "0 (on-failure-jump 7)
3 (any-char)
4 (maybe-finalize-jump 0)
7 (start-memory 1)
9 (on-failure-jump 44)
12 (char-set 00 00 00 00 00 00 ff 03)
22 (on-failure-jump 41)
25 (char-set 00 00 00 00 00 00 00 00 a4 28 11 06 00 08)
41 (jump 47)
44 (exact-n \")\")
47 (stop-memory 1)
49 (dummy-failure-jump 55)
52 (on-failure-jump 60)
55 (exact-1 \" \")
57 (maybe-finalize-jump 52)
60 (start-memory 2)
62 (on-failure-jump 408)
65 (start-memory 3)
67 (on-failure-jump 185)
70 (char-set 00 00 00 00 80 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
104 (dummy-failure-jump 110)
107 (on-failure-jump 147)
110 (char-set 00 00 00 00 80 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
144 (maybe-finalize-jump 107)
147 (on-failure-jump 152)
150 (exact-1 \".\")
152 (dummy-failure-jump 158)
155 (on-failure-jump 163)
158 (exact-1 \" \")
160 (maybe-finalize-jump 155)
163 (char-set 00 00 00 00 01 00 0f)
172 (char-set 00 00 00 00 00 00 ff 03)
182 (jump 294)
185 (char-set 00 00 00 00 01 00 0f)
194 (char-set 00 00 00 00 00 00 ff 03)
204 (on-failure-jump 209)
207 (exact-1 \".\")
209 (exact-n \" \")
212 (char-set 00 00 00 00 80 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
246 (dummy-failure-jump 252)
249 (on-failure-jump 289)
252 (char-set 00 00 00 00 80 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
286 (maybe-finalize-jump 249)
289 (on-failure-jump 294)
292 (exact-1 \".\")
294 (stop-memory 3)
296 (dummy-failure-jump 302)
299 (on-failure-jump 307)
302 (exact-1 \" \")
304 (maybe-finalize-jump 299)
307 (start-memory 4)
309 (on-failure-jump 363)
312 (char-set 00 00 00 00 01 00 07)
321 (char-set 00 00 00 00 00 00 ff 03)
331 (char-set 00 00 00 00 00 40 00 04)
341 (char-set 00 00 00 00 00 00 3f)
350 (char-set 00 00 00 00 00 00 ff 03)
360 (jump 403)
363 (char-set 00 00 00 00 00 00 ff 03)
373 (char-set 00 00 00 00 00 00 ff 03)
383 (char-set 00 00 00 00 00 00 ff 03)
393 (char-set 00 00 00 00 00 00 ff 03)
403 (stop-memory 4)
405 (jump 577)
408 (on-failure-jump 580)
411 (char-set 00 00 00 00 80 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
445 (dummy-failure-jump 451)
448 (on-failure-jump 488)
451 (char-set 00 00 00 00 80 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
485 (maybe-finalize-jump 448)
488 (on-failure-jump 493)
491 (exact-1 \".\")
493 (dummy-failure-jump 499)
496 (on-failure-jump 504)
499 (exact-1 \" \")
501 (maybe-finalize-jump 496)
504 (char-set 00 00 00 00 01 00 0f)
513 (char-set 00 00 00 00 00 00 ff 03)
523 (exact-n \",\")
526 (dummy-failure-jump 532)
529 (on-failure-jump 537)
532 (exact-1 \" \")
534 (maybe-finalize-jump 529)
537 (char-set 00 00 00 00 00 00 ff 03)
547 (char-set 00 00 00 00 00 00 ff 03)
557 (char-set 00 00 00 00 00 00 ff 03)
567 (char-set 00 00 00 00 00 00 ff 03)
577 (jump 912)
580 (on-failure-jump 915)
583 (start-memory 5)
585 (on-failure-jump 720)
588 (on-failure-jump 600)
591 (char-set 00 00 00 00 01 00 03)
600 (char-set 00 00 00 00 00 00 ff 03)
610 (on-failure-jump 647)
613 (char-set 00 00 00 00 00 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
647 (exact-n \" \")
650 (char-set 00 00 00 00 01 00 0f)
659 (char-set 00 00 00 00 00 00 ff 03)
669 (on-failure-jump 706)
672 (char-set 00 00 00 00 00 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
706 (dummy-failure-jump 712)
709 (on-failure-jump 717)
712 (exact-1 \" \")
714 (maybe-finalize-jump 709)
717 (jump 775)
720 (char-set 00 00 00 00 01 00 0f)
729 (char-set 00 00 00 00 00 00 ff 03)
739 (exact-n \" \")
742 (on-failure-jump 754)
745 (char-set 00 00 00 00 01 00 03)
754 (char-set 00 00 00 00 00 00 ff 03)
764 (dummy-failure-jump 770)
767 (on-failure-jump 775)
770 (exact-1 \" \")
772 (maybe-finalize-jump 767)
775 (stop-memory 5)
777 (start-memory 6)
779 (on-failure-jump 833)
782 (char-set 00 00 00 00 01 00 07)
791 (char-set 00 00 00 00 00 00 ff 03)
801 (char-set 00 00 00 00 00 40 00 04)
811 (char-set 00 00 00 00 00 00 3f)
820 (char-set 00 00 00 00 00 00 ff 03)
830 (jump 910)
833 (char-set 00 00 00 00 00 00 ff 03)
843 (char-set 00 00 00 00 00 00 ff 03)
853 (char-set 00 00 00 00 00 00 ff 03)
863 (char-set 00 00 00 00 00 00 ff 03)
873 (on-failure-jump 910)
876 (char-set 00 00 00 00 00 00 00 00 fe ff ff 07 fe ff ff 07 ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff)
910 (stop-memory 6)
912 (jump 1187)
915 (on-failure-jump 1190)
918 (start-memory 7)
920 (on-failure-jump 968)
923 (char-set 00 00 00 00 00 00 ff 03)
933 (char-set 00 00 00 00 00 00 ff 03)
943 (char-set 00 00 00 00 00 00 ff 03)
953 (char-set 00 00 00 00 00 00 ff 03)
963 (exact-n \"-\")
966 (stop-memory 7)
968 (char-set 00 00 00 00 00 00 03)
977 (char-set 00 00 00 00 00 00 ff 03)
987 (exact-n \"-\")
990 (char-set 00 00 00 00 00 00 0f)
999 (char-set 00 00 00 00 00 00 ff 03)
1009 (char-set 00 00 00 00 01 00 00 00 00 00 10)
1022 (char-set 00 00 00 00 01 00 07)
1031 (char-set 00 00 00 00 00 00 ff 03)
1041 (char-set 00 00 00 00 00 40 00 04)
1051 (char-set 00 00 00 00 00 00 3f)
1060 (char-set 00 00 00 00 00 00 ff 03)
1070 (start-memory 8)
1072 (on-failure-jump 1187)
1075 (exact-n \":\")
1078 (char-set 00 00 00 00 00 00 7f)
1087 (char-set 00 00 00 00 00 00 ff 03)
1097 (start-memory 9)
1099 (on-failure-jump 1131)
1102 (char-set 00 00 00 00 00 50)
1110 (dummy-failure-jump 1116)
1113 (on-failure-jump 1129)
1116 (char-set 00 00 00 00 00 00 ff 03)
1126 (maybe-finalize-jump 1113)
1129 (stop-memory 9)
1131 (on-failure-jump 1185)
1134 (on-failure-jump 1139)
1137 (exact-1 \" \")
1139 (char-set 00 00 00 00 00 28)
1147 (char-set 00 00 00 00 00 00 07)
1156 (char-set 00 00 00 00 00 00 ff 03)
1166 (char-set 00 00 00 00 00 00 3f)
1175 (char-set 00 00 00 00 00 00 ff 03)
1185 (stop-memory 8)
1187 (jump 1274)
1190 (char-set 00 00 00 00 00 00 ff 03)
1200 (char-set 00 00 00 00 00 00 ff 03)
1210 (char-set 00 00 00 00 00 00 ff 03)
1220 (char-set 00 00 00 00 00 00 ff 03)
1230 (exact-n \"-\")
1233 (char-set 00 00 00 00 00 00 03)
1242 (char-set 00 00 00 00 00 00 ff 03)
1252 (exact-n \"-\")
1255 (char-set 00 00 00 00 00 00 0f)
1264 (char-set 00 00 00 00 00 00 ff 03)
1274 (stop-memory 2)
1276 (dummy-failure-jump 1282)
1279 (on-failure-jump 1287)
1282 (exact-1 \" \")
1284 (maybe-finalize-jump 1279)
1287 (end)
")