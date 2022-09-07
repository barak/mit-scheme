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

;;;; UCD property: canonical-cm (canonical-composition-mapping)

;;; Generated from Unicode 13.0.0

(declare (usual-integrations))

(define (ucd-canonical-cm-value char)
  (let ((sv (char->integer char)))
    (vector-ref ucd-canonical-cm-table-5 (bytevector-u16be-ref ucd-canonical-cm-table-4 (fix:lsh (fix:or (fix:lsh (bytevector-u8-ref ucd-canonical-cm-table-3 (fix:or (fix:lsh (bytevector-u8-ref ucd-canonical-cm-table-2 (fix:or (fix:lsh (bytevector-u8-ref ucd-canonical-cm-table-1 (fix:or (fix:lsh (bytevector-u8-ref ucd-canonical-cm-table-0 (fix:lsh sv -16)) 4) (fix:and 15 (fix:lsh sv -12)))) 4) (fix:and 15 (fix:lsh sv -8)))) 4) (fix:and 15 (fix:lsh sv -4)))) 4) (fix:and 15 sv)) 1)))))

(define ucd-canonical-cm-table-0
  '#u8(0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))

(define ucd-canonical-cm-table-1
  '#u8(0 1 2 3 4 4 4 4 4 4 4 4 4 4 4 4 4 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4))

(define ucd-canonical-cm-table-2
  '#u8(0 1 2 3 4 5 6 5 5 7 5 8 9 10 5 5 11 5 5 5 5 5 5 5 5 5 5 12 5 5 13 14 5 15 16 5 5 5 5 5 5 5 5 5 5 5 5 5 17 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 18 19 5 20 21 22 5 5 5 23 5 5 5 5 5 5))

(define ucd-canonical-cm-table-3
  '#u8(0 0 0 1 2 3 4 5 0 0 6 0 7 8 9 10 11 12 0 0 13 14 15 16 0 0 17 18 0 0 19 0 0 0 20 0 0 0 0 0 0 21 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 22 23 24 25 26 0 0 27 28 29 30 31 32 0 33 0 0 0 0 0 34 35 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 36 0 37 0 0 0 0 0 0 0 38 39 0 0 0 0 40 41 0 0 0 0 0 0 0 0 42 0 0 0 0 0 0 0 43 0 0 0 0 44 0 0 45 0 0 0 0 0 0 0 46 0 0 0 0 0 0 47 48 0 0 0 0 0 0 0 49 0 0 0 0 0 0 0 0 50 0 0 0 0 51 0 0 0 0 0 0 0 0 0 0 0 0 0 52 53 0 54 55 0 0 0 0 0 0 0 0 0 0 0 0 0 0 56 0 57 58 0 0 0 59 60 61 0 0 0 62 63 64 65 66 67 68 69 0 0 0 70 71 0 0 72 0 0 0 0 0 0 0 0 0 73 0 0 0 74 0 0 75 0 76 77 78 0 79 80 81 82 83 84 0 0 0 0 0 0 0 0 85 86 87 88 0 89 90 91 92 93 94 95 0 0 0 0 0 0 0 0 0 96 97 0 0 0 0 0 0 0 0 98 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 99 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 101 0 0 0 0 0 0 0 102 0 0 0 0 0 0 0 0 0 0 0 0))

(define ucd-canonical-cm-table-4
  '#u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 2 0 3 0 0 0 0 0 4 0 5 0 6 0 7 0 8 0 9 0 10 0 11 0 12 0 13 0 14 0 15 0 16 0 17 0 18 0 19 0 0 0 20 0 21 0 22 0 23 0 24 0 25 0 26 0 27 0 28 0 0 0 0 0 0 0 0 0 0 0 0 0 29 0 30 0 31 0 32 0 33 0 34 0 35 0 36 0 37 0 38 0 39 0 40 0 41 0 42 0 43 0 44 0 0 0 45 0 46 0 47 0 48 0 49 0 50 0 51 0 52 0 53 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 54 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 55 0 0 0 56 0 57 0 58 0 59 0 0 0 0 0 60 0 0 0 0 0 0 0 0 0 61 0 0 0 0 0 0 0 0 0 62 0 63 0 64 0 0 0 65 0 0 0 0 0 0 0 66 0 0 0 0 0 0 0 0 0 0 0 67 0 0 0 68 0 69 0 70 0 71 0 0 0 0 0 72 0 0 0 0 0 0 0 0 0 73 0 0 0 0 0 0 0 0 0 74 0 75 0 76 0 0 0 77 0 0 0 0 0 0 0 78 0 0 0 0 0 0 0 0 0 0 0 79 0 80 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 81 0 82 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 83 0 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 85 0 86 0 0 0 0 0 0 0 0 0 87 0 88 0 0 0 0 0 0 0 0 0 0 0 0 0 89 0 90 0 91 0 92 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 93 0 94 0 95 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 96 0 97 0 0 0 0 0 0 0 0 0 0 0 0 0 98 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 99 0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 101 0 102 0 103 0 104 0 0 0 0 0 0 0 0 0 105 0 106 0 0 0 0 0 107 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 108 0 0 0 0 0 0 0 109 0 0 0 110 0 0 0 111 0 0 0 0 0 0 0 0 0 0 0 112 0 0 0 113 0 0 0 0 0 0 0 114 0 0 0 0 0 0 0 115 0 0 0 0 0 116 0 0 0 117 0 0 0 0 0 118 0 0 0 0 0 0 0 119 0 0 0 120 0 0 0 121 0 0 0 0 0 0 0 0 0 0 0 122 0 0 0 123 0 0 0 0 0 0 0 124 0 0 0 0 0 0 0 125 0 126 0 127 0 0 0 0 0 128 0 0 0 0 0 0 0 129 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 130 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 131 0 0 0 0 0 132 0 0 0 133 0 134 0 135 0 136 0 0 0 137 0 0 0 0 0 0 0 138 0 0 0 0 0 0 0 0 0 139 0 0 0 0 0 0 0 140 0 0 0 0 0 0 0 141 0 0 0 142 0 0 0 0 0 143 0 0 0 0 0 144 0 0 0 145 0 146 0 147 0 148 0 0 0 149 0 0 0 0 0 0 0 150 0 0 0 0 0 0 0 0 0 151 0 0 0 0 0 0 0 152 0 0 0 0 0 0 0 153 0 0 0 154 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 155 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 156 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 158 0 159 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 160 0 161 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 162 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 163 0 0 0 164 0 0 0 0 0 0 0 0 0 0 0 0 0 165 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 166 0 0 0 0 0 167 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 168 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 169 0 0 0 0 0 170 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 172 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 173 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 174 0 175 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 177 0 0 0 0 0 0 0 0 0 0 0 0 0 178 0 0 0 0 0 0 0 179 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 180 0 181 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 182 0 0 0 0 0 183 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 184 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 185 0 0 0 186 0 0 0 187 0 0 0 188 0 0 0 189 0 0 0 0 0 0 0 190 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 191 0 0 0 192 0 0 0 193 0 194 0 0 0 0 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 196 0 197 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 198 0 199 0 0 0 0 0 0 0 0 0 0 0 0 0 200 0 201 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 202 0 203 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 204 0 205 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 206 0 207 0 0 0 0 0 208 0 209 0 210 0 211 0 212 0 213 0 214 0 215 0 216 0 217 0 218 0 219 0 220 0 221 0 222 0 223 0 224 0 225 0 0 0 0 0 0 0 0 0 0 0 0 0 226 0 227 0 0 0 0 0 0 0 0 0 0 0 0 0 228 0 229 0 230 0 231 0 232 0 233 0 234 0 235 0 236 0 237 0 238 0 239 0 240 0 241 0 242 0 243 0 244 0 245 0 0 0 0 0 0 0 0 0 0 0 0 0 246 0 247 0 0 0 0 0 0 0 0 0 0 0 0 0 248 0 249 0 0 0 0 0 0 0 0 0 0 0 0 0 250 0 251 0 0 0 0 0 0 0 0 0 0 0 0 0 252 0 253 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 254 0 0 0 0 0 0 0 0 0 0 0 0 0 255 1 0 1 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1 10 1 11 1 12 1 13 1 14 1 15 0 0 0 0 0 0 1 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 19 0 0 0 0 0 0 0 0 0 0 0 0 1 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 21 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 22 0 0 1 23 0 0 1 24 0 0 1 25 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 26 0 0 1 27 0 0 1 28 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 29 0 0 0 0 0 0 0 0 1 30 0 0 0 0 1 31 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 32 0 0 1 33 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 34 0 0 0 0 0 0 0 0 0 0 0 0 1 35 0 0 1 36 0 0 0 0 1 37 0 0 0 0 0 0 0 0 1 38 0 0 0 0 0 0 1 39 0 0 0 0 1 40 1 41 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 42 1 43 0 0 0 0 1 44 1 45 0 0 0 0 1 46 1 47 1 48 1 49 0 0 0 0 0 0 0 0 1 50 1 51 0 0 0 0 1 52 1 53 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 54 1 55 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 56 0 0 0 0 0 0 0 0 0 0 1 57 1 58 0 0 1 59 0 0 0 0 0 0 0 0 0 0 0 0 1 60 1 61 1 62 1 63 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 64 0 0 0 0 0 0 0 0 1 65 0 0 1 66 0 0 1 67 0 0 1 68 0 0 1 69 0 0 1 70 0 0 1 71 0 0 1 72 0 0 1 73 0 0 1 74 0 0 1 75 0 0 1 76 0 0 0 0 1 77 0 0 1 78 0 0 1 79 0 0 0 0 0 0 0 0 0 0 0 0 1 80 0 0 0 0 1 81 0 0 0 0 1 82 0 0 0 0 1 83 0 0 0 0 1 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 85 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 86 0 0 0 0 0 0 0 0 1 87 0 0 1 88 0 0 1 89 0 0 1 90 0 0 1 91 0 0 1 92 0 0 1 93 0 0 1 94 0 0 1 95 0 0 1 96 0 0 1 97 0 0 1 98 0 0 0 0 1 99 0 0 1 100 0 0 1 101 0 0 0 0 0 0 0 0 0 0 0 0 1 102 0 0 0 0 1 103 0 0 0 0 1 104 0 0 0 0 1 105 0 0 0 0 1 106 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 107 1 108 1 109 1 110 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 111 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 112 0 0 1 113 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 114 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 115 1 116 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 117 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 118 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 119 1 120 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 121 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define ucd-canonical-cm-table-5
  '#(#f  0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99  100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187
     188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376))
