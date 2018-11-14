#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; UCD property: WB (word-break)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (ucd-wb-value char)
  (let ((sv (char->integer char)))
    (vector-ref |ucd-WB-table-5| (bytevector-u8-ref |ucd-WB-table-4| (fix:or (fix:lsh (bytevector-u16be-ref |ucd-WB-table-3| (fix:lsh (fix:or (fix:lsh (bytevector-u8-ref |ucd-WB-table-2| (fix:or (fix:lsh (bytevector-u8-ref |ucd-WB-table-1| (fix:or (fix:lsh (bytevector-u8-ref |ucd-WB-table-0| (fix:lsh sv -16)) 4) (fix:and 15 (fix:lsh sv -12)))) 4) (fix:and 15 (fix:lsh sv -8)))) 4) (fix:and 15 (fix:lsh sv -4))) 1)) 4) (fix:and 15 sv))))))

(define |ucd-WB-table-0|
  '#u8(0 1 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2))

(define |ucd-WB-table-1|
  '#u8(0 1 2 3 4 4 4 4 4 4 5 6 6 7 4 8 9 10 11 12 13 4 14 4 4 4 4 15 4 16 17 18 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 19 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4))

(define |ucd-WB-table-2|
  '#u8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 1 17 18 19 1 20 21 22 23 24 25 26 27 1 28 29 30 31 31 32 31 33 34 31 31 31 31 35 36 37 31 38 39 40 41 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 1 1 1 1 42 1 43 44 45 46 47 48 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 49 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 50 1 51 52 53 54 55 56 57 58 59 1 60 61 62 63 64 65 31 31 31 66 67 68 69 70 71 72 73 74 31 75 31 76 31 31 31 1 1 1 77 78 79 31 31 31 31 31 31 31 31 31 31 1 1 1 1 80 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 1 1 81 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 1 1 82 83 31 31 31 84 85 31 31 31 31 31 31 31 31 31 31 31 86 31 31 31 31 87 88 31 89 90 91 92 31 31 93 31 31 31 31 31 94 31 31 31 31 31 31 31 95 96 31 31 31 31 97 31 31 98 31 99 100 101 102 31 31 103 31 31 31 31 31 31 104 105 31 31 31 31 31 31 31 31 31 31 31 31 31 31))

(define |ucd-WB-table-3|
  '#u8(0 0 0 1 0 2 0 3 0 4 0 5 0 4 0 6 0 7 0 1 0 8 0 9 0 10 0 11 0 10 0 11 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 12 0 13 0 14 0 1 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 16 0 17 0 10 0 18 0 10 0 10 0 10 0 10 0 19 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 20 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 4 0 10 0 21 0 4 0 10 0 22 0 23 0 15 0 24 0 25 0 26 0 27 0 28 0 29 0 30 0 10 0 10 0 31 0 15 0 32 0 33 0 10 0 10 0 10 0 10 0 10 0 34 0 35 0 36 0 37 0 38 0 10 0 15 0 39 0 10 0 10 0 10 0 10 0 10 0 40 0 41 0 42 0 10 0 31 0 43 0 10 0 44 0 45 0 1 0 10 0 46 0 1 0 1 0 1 0 1 0 10 0 47 0 1 0 48 0 49 0 15 0 50 0 10 0 10 0 51 0 15 0 52 0 53 0 4 0 54 0 55 0 56 0 57 0 58 0 59 0 53 0 60 0 61 0 55 0 56 0 62 0 63 0 64 0 65 0 66 0 67 0 18 0 56 0 68 0 69 0 70 0 53 0 71 0 72 0 55 0 56 0 68 0 73 0 74 0 53 0 75 0 76 0 77 0 78 0 79 0 80 0 81 0 65 0 1 0 82 0 83 0 56 0 84 0 85 0 86 0 53 0 1 0 87 0 83 0 56 0 88 0 85 0 89 0 53 0 90 0 91 0 83 0 10 0 92 0 93 0 94 0 53 0 95 0 96 0 97 0 10 0 98 0 99 0 100 0 65 0 101 0 1 0 1 0 1 0 102 0 103 0 104 0 1 0 1 0 1 0 1 0 1 0 105 0 106 0 104 0 1 0 1 0 70 0 107 0 104 0 108 0 109 0 10 0 110 0 23 0 111 0 112 0 15 0 113 0 114 0 1 0 1 0 1 0 1 0 1 0 115 0 116 0 104 0 117 0 118 0 119 0 120 0 121 0 10 0 10 0 122 0 10 0 10 0 123 0 10 0 10 0 10 0 10 0 124 0 125 0 10 0 10 0 124 0 10 0 10 0 126 0 127 0 11 0 10 0 10 0 10 0 127 0 10 0 10 0 10 0 128 0 1 0 1 0 10 0 1 0 10 0 10 0 10 0 10 0 10 0 129 0 4 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 130 0 10 0 4 0 6 0 10 0 10 0 10 0 10 0 131 0 132 0 133 0 134 0 10 0 134 0 10 0 135 0 133 0 136 0 1 0 1 0 1 0 48 0 15 0 137 0 104 0 1 0 138 0 104 0 10 0 10 0 10 0 10 0 10 0 139 0 140 0 10 0 141 0 10 0 10 0 10 0 10 0 142 0 10 0 143 0 144 0 144 0 65 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 104 0 1 0 1 0 10 0 145 0 1 0 1 0 1 0 146 0 15 0 147 0 104 0 104 0 1 0 116 0 1 0 1 0 1 0 1 0 148 0 10 0 10 0 149 0 150 0 104 0 115 0 151 0 152 0 10 0 153 0 42 0 10 0 10 0 40 0 151 0 10 0 10 0 149 0 154 0 155 0 42 0 10 0 156 0 132 0 1 0 1 0 1 0 1 0 157 0 158 0 159 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 15 0 15 0 15 0 160 0 10 0 129 0 10 0 10 0 129 0 161 0 10 0 156 0 10 0 10 0 10 0 162 0 163 0 164 0 110 0 163 0 165 0 166 0 167 0 168 0 169 0 170 0 171 0 172 0 1 0 110 0 1 0 1 0 1 0 15 0 15 0 173 0 174 0 175 0 176 0 177 0 178 0 1 0 10 0 10 0 132 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 179 0 10 0 10 0 180 0 1 0 1 0 181 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 182 0 183 0 1 0 1 0 1 0 1 0 1 0 184 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 143 0 10 0 10 0 143 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 185 0 186 0 10 0 10 0 122 0 10 0 10 0 10 0 187 0 188 0 10 0 189 0 190 0 190 0 190 0 190 0 15 0 15 0 1 0 1 0 191 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 192 0 1 0 193 0 194 0 1 0 1 0 1 0 1 0 1 0 195 0 196 0 196 0 196 0 196 0 196 0 197 0 198 0 10 0 156 0 4 0 10 0 10 0 10 0 10 0 143 0 1 0 10 0 6 0 1 0 1 0 1 0 196 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 196 0 196 0 199 0 196 0 196 0 196 0 196 0 196 0 200 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 110 0 1 0 1 0 1 0 1 0 10 0 10 0 156 0 110 0 10 0 201 0 1 0 10 0 10 0 202 0 203 0 10 0 204 0 10 0 10 0 10 0 10 0 10 0 205 0 1 0 206 0 207 0 10 0 10 0 10 0 10 0 10 0 208 0 10 0 143 0 139 0 1 0 1 0 1 0 206 0 209 0 10 0 210 0 1 0 10 0 10 0 10 0 211 0 212 0 10 0 10 0 149 0 213 0 104 0 15 0 214 0 42 0 10 0 215 0 10 0 216 0 151 0 10 0 110 0 50 0 10 0 10 0 217 0 218 0 104 0 219 0 104 0 10 0 10 0 220 0 221 0 222 0 104 0 1 0 223 0 1 0 1 0 1 0 224 0 225 0 1 0 31 0 226 0 227 0 228 0 190 0 10 0 10 0 123 0 142 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 229 0 104 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 211 0 10 0 230 0 10 0 10 0 231 0 189 0 232 0 233 0 234 0 235 0 10 0 10 0 10 0 10 0 10 0 10 0 60 0 1 0 236 0 10 0 10 0 10 0 10 0 10 0 156 0 1 0 10 0 10 0 10 0 10 0 207 0 10 0 10 0 139 0 1 0 1 0 231 0 15 0 237 0 15 0 238 0 239 0 240 0 1 0 241 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 242 0 243 0 244 0 4 0 5 0 4 0 6 0 245 0 196 0 196 0 246 0 10 0 143 0 247 0 248 0 1 0 249 0 250 0 10 0 11 0 251 0 156 0 156 0 1 0 1 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 6 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 252 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 253 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 110 0 10 0 10 0 10 0 70 0 173 0 1 0 10 0 10 0 1 0 10 0 6 0 10 0 10 0 254 0 10 0 156 0 10 0 10 0 255 1 0 0 1 0 1 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 156 0 104 0 10 0 10 0 255 0 10 0 231 0 10 0 10 0 139 0 10 0 10 0 10 0 211 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 189 0 10 0 142 0 139 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 10 0 10 1 2 0 10 0 142 0 10 0 189 0 10 0 143 0 1 0 1 0 1 0 1 0 10 1 3 0 10 0 142 0 10 0 180 0 1 0 1 0 1 0 1 0 10 0 10 0 10 1 4 0 1 0 1 0 1 0 1 1 5 1 6 0 10 1 7 0 1 0 1 0 10 0 110 0 10 0 110 0 1 0 1 0 109 0 10 1 8 0 1 0 10 0 10 0 10 0 142 0 10 0 142 0 10 1 9 0 10 0 60 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 132 0 1 0 1 0 1 0 10 0 10 0 10 1 9 0 10 0 10 0 10 1 9 0 152 0 10 0 10 1 10 0 221 0 1 0 65 0 188 0 152 0 10 0 10 1 11 0 1 0 10 0 132 0 104 0 152 0 10 0 216 1 12 0 1 0 10 0 10 1 13 0 152 0 10 0 10 0 217 1 14 1 15 0 1 0 1 0 10 0 18 1 16 1 17 0 1 0 1 0 1 0 1 1 18 1 19 0 132 0 10 0 10 0 202 1 20 0 104 1 21 0 55 0 56 0 68 0 73 1 22 1 23 1 24 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 1 25 1 26 0 104 0 1 0 1 0 10 0 10 0 10 0 15 1 27 0 104 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 202 1 28 0 173 1 29 0 1 0 1 0 10 0 10 0 10 0 15 1 30 0 104 0 1 0 1 0 10 0 10 0 31 0 154 0 104 0 1 0 1 0 1 0 1 1 31 0 144 0 104 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 104 0 191 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 132 0 56 0 10 0 202 1 32 0 70 0 104 0 1 0 207 0 10 1 33 0 112 0 221 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 180 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 10 0 10 0 143 0 1 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 211 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 143 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 189 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 132 0 10 0 143 0 104 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 156 1 24 0 10 0 10 0 10 0 221 0 211 0 104 0 236 1 34 0 10 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 252 1 35 0 15 0 116 0 188 0 152 0 1 0 1 0 1 0 1 0 70 0 1 1 36 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 10 0 10 0 6 0 110 0 132 1 37 1 38 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 39 1 40 1 41 0 1 1 42 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 43 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 10 0 241 0 10 0 10 0 10 0 133 1 44 1 45 1 46 0 10 0 10 0 10 1 47 1 48 0 10 1 49 1 50 0 83 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 1 51 0 10 0 83 0 123 0 10 0 123 0 10 0 241 0 10 0 241 0 143 0 10 0 143 0 10 0 56 0 10 0 56 0 10 1 52 1 53 1 53 1 53 0 15 0 15 0 15 1 54 0 15 0 15 0 113 0 219 1 55 0 115 0 23 0 1 0 1 0 1 0 1 0 1 1 32 1 56 1 57 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0 252 0 221 0 1 0 1 0 10 0 10 0 10 0 10 1 58 0 104 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 46 0 10 1 59 1 60 1 61 1 62 1 63 1 64 1 65 0 231 1 66 0 231 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 10 0 180 0 10 0 180 0 10 0 180 0 1 0 1 0 1 0 1 0 1 1 67 1 68 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 69 0 1 0 1 0 1 1 70 0 1 0 1 1 71 0 1 0 1 0 1 0 1 1 72 1 73 1 74 1 75 1 76 0 1 1 77 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 78 0 1 1 79 0 1 0 1 0 1 0 1 1 80 0 1 0 1 0 1 0 1 0 1 1 81 0 1 0 1 0 1 0 1 0 1 1 82 1 83 1 73 0 1 0 1 0 1 0 1 1 84 1 85 1 86 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 87 0 1 0 15 0 15 0 15 0 15 0 15 0 15 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 15 0 1))

(define |ucd-WB-table-4|
  '#u8(0 0 0 0 0 0 0 0 0 0 1 2 2 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 5 0 0 0 0 6 0 7 0 8 8 8 8 8 8 8 8 8 8 9 6 0 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 0 11 10 10 10 10 10 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 0 0 12 0 0 0 0 0 0 0 10 0 9 0 0 10 0 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 0 0 9 0 0 0 0 0 0 0 0 10 10 10 10 10 0 0 0 0 0 0 0 10 0 10 0 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 10 10 10 10 10 0 10 10 0 0 10 10 10 10 6 10 0 0 0 0 0 0 10 9 10 10 10 0 10 0 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 0 13 13 13 13 13 13 13 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 10 0 0 0 0 0 0 10 10 10 10 10 10 10 10 0 6 0 0 0 0 0 0 0 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 0 13 0 13 13 0 13 13 0 13 0 0 0 0 0 0 0 0 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 0 0 0 0 0 14 14 14 10 9 0 0 0 0 0 0 0 0 0 0 0 12 12 12 12 12 12 0 0 0 0 0 0 6 6 0 0 13 13 13 13 13 13 13 13 13 13 13 0 12 0 0 0 10 10 10 10 10 10 10 10 10 10 10 13 13 13 13 13 8 8 8 8 8 8 8 8 8 8 0 8 6 0 10 10 13 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 13 13 13 13 13 13 13 12 0 13 13 13 13 13 13 10 10 13 13 0 13 13 13 13 10 10 8 8 8 8 8 8 8 8 8 8 10 10 10 0 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 12 10 13 10 10 10 10 10 10 10 10 10 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 0 0 10 10 10 10 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 8 8 8 8 8 8 8 8 8 10 10 10 10 10 10 13 13 13 13 10 10 0 0 6 0 10 0 0 0 0 0 10 10 10 10 10 10 13 13 13 13 10 13 13 13 13 13 13 13 13 13 10 13 13 13 10 13 13 13 13 13 0 0 10 10 10 10 10 10 10 10 10 13 13 13 0 0 0 0 10 10 10 10 10 0 10 10 10 10 10 10 10 10 0 0 0 0 0 0 13 13 13 13 13 13 13 13 13 13 13 13 13 13 12 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 13 13 13 10 13 13 10 13 13 13 13 13 13 13 10 10 10 10 10 10 10 10 10 10 13 13 0 0 8 8 8 8 8 8 8 8 8 8 10 13 13 13 0 10 10 10 10 10 10 10 10 0 0 10 10 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 0 10 0 0 0 10 10 10 10 0 0 13 10 13 13 13 13 13 13 13 0 0 13 13 0 0 13 13 13 10 0 0 0 0 0 0 0 0 13 0 0 0 0 10 10 0 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 0 10 10 10 10 10 10 0 0 0 0 10 10 0 10 10 0 10 10 0 10 10 0 0 13 0 13 13 13 13 13 0 0 0 0 13 13 0 0 13 13 13 0 0 0 13 0 0 0 0 0 0 0 10 10 10 10 0 10 0 0 0 0 0 0 0 8 8 8 8 8 8 8 8 8 8 13 13 10 10 10 13 0 0 0 0 0 0 0 0 0 0 0 13 13 13 0 10 10 10 10 10 10 10 10 10 0 10 10 0 10 10 0 10 10 10 10 10 0 0 13 10 13 13 13 13 13 13 13 13 0 13 13 13 0 13 13 13 0 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 13 13 13 0 10 10 10 10 10 10 10 10 0 0 10 13 13 13 13 13 0 0 13 13 0 0 13 13 13 0 0 0 0 0 0 0 0 13 13 0 0 0 0 10 10 0 10 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 10 0 10 10 10 10 10 10 0 0 0 10 10 10 0 10 10 10 10 0 0 0 10 10 0 10 0 10 10 0 0 0 10 10 0 0 0 10 10 10 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 0 13 13 13 13 13 0 0 0 13 13 13 0 13 13 13 13 0 0 10 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 13 13 13 13 0 10 10 10 10 10 10 10 10 0 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 10 13 13 13 13 13 13 13 0 13 13 13 0 13 13 13 13 0 0 0 0 0 0 0 13 13 0 10 10 10 0 0 0 0 0 10 13 13 13 0 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 0 10 10 10 10 10 0 0 13 10 13 13 0 0 0 0 0 13 13 0 0 0 0 0 0 0 10 0 0 10 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 0 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 10 13 13 13 13 13 13 13 0 13 13 13 0 13 13 13 13 10 0 0 0 0 0 10 10 10 13 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 0 0 13 13 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 0 10 0 0 10 10 10 10 10 10 10 0 0 0 13 0 0 0 0 13 13 13 13 13 13 0 13 0 13 13 13 13 13 13 13 13 0 0 13 13 0 0 0 0 0 0 0 0 0 0 0 0 0 13 0 0 13 13 13 13 13 13 13 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 13 13 13 13 0 8 8 8 8 8 8 8 8 8 8 0 0 0 0 0 0 0 13 0 0 13 13 13 13 13 13 0 13 13 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 13 13 0 0 0 0 0 0 0 0 0 0 13 13 0 0 0 0 0 0 0 0 0 0 0 13 0 13 0 13 0 0 0 0 13 13 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 13 13 13 13 13 0 13 13 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 0 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 0 0 0 0 0 0 0 13 13 13 13 0 0 0 0 13 13 13 0 13 13 13 0 0 13 13 13 13 13 13 13 0 0 0 13 13 13 13 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 13 13 13 13 13 13 13 13 0 13 8 8 8 8 8 8 8 8 8 8 13 13 13 13 0 0 10 10 10 10 10 10 0 10 0 0 0 0 0 10 0 0 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 0 0 10 10 10 10 10 10 10 0 10 0 10 10 10 10 0 0 10 0 10 10 10 10 0 0 10 10 10 10 10 10 10 0 10 0 10 10 10 10 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 13 13 13 10 10 10 10 10 10 0 0 10 10 10 10 10 10 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 10 10 10 10 10 10 10 10 10 10 10 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 13 13 13 0 0 0 0 0 0 0 0 0 0 0 10 10 13 13 0 0 0 0 0 0 0 0 0 0 0 0 10 0 13 13 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 12 0 10 10 10 10 10 10 10 10 0 0 0 0 0 0 0 0 10 10 10 10 10 13 13 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 13 10 0 0 0 0 0 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 13 13 13 13 13 13 13 13 13 13 13 13 0 0 0 0 10 10 10 10 10 10 10 13 13 13 13 13 0 0 0 0 0 0 0 0 0 13 13 13 13 13 13 13 13 13 13 0 13 13 13 13 13 13 13 13 13 13 13 13 13 0 0 13 13 13 13 13 13 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 10 10 10 10 10 10 10 0 0 0 0 13 13 13 13 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 10 10 10 10 10 10 10 10 10 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 13 13 10 10 13 13 13 13 13 13 13 13 0 0 0 0 0 0 0 0 8 8 8 8 8 8 8 8 8 8 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 13 13 13 0 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 10 10 10 10 13 10 10 10 10 13 13 13 10 10 0 13 13 0 0 0 0 0 0 13 13 13 13 13 13 0 0 0 0 0 13 13 13 13 13 10 10 10 10 10 10 10 10 0 10 0 10 0 10 0 10 10 10 10 10 10 0 10 10 10 10 10 10 10 0 10 0 0 0 10 10 10 0 10 10 10 10 10 10 10 0 0 0 10 10 10 10 0 0 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 15 12 12 0 0 0 0 0 0 0 0 7 7 0 0 0 0 0 0 0 0 0 0 7 0 0 9 2 2 12 12 12 12 12 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 11 11 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 11 0 0 0 0 0 0 0 0 0 0 0 12 12 12 12 12 0 12 12 12 12 12 12 12 12 12 12 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 10 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 0 0 0 0 10 0 0 10 10 10 10 10 10 10 10 10 10 0 10 0 0 0 10 10 10 10 10 0 0 0 0 0 0 10 0 10 0 10 0 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 0 0 10 10 10 10 0 0 0 0 0 10 10 10 10 10 0 0 0 0 10 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16 16 16 16 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 0 0 0 0 0 0 10 10 10 10 13 13 13 10 10 0 0 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 10 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 0 0 0 0 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 13 13 0 18 18 18 18 18 0 0 0 0 0 10 10 0 0 0 0 0 0 0 0 0 0 0 0 13 13 18 18 0 0 0 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 0 18 18 18 18 0 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 0 18 18 18 18 18 18 18 18 0 0 0 0 0 0 0 0 8 8 8 8 8 8 8 8 8 8 10 10 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 13 13 13 13 0 13 13 13 13 13 13 13 13 13 13 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 13 13 13 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 10 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 10 10 10 10 10 10 10 13 10 10 10 13 10 10 10 10 13 10 10 10 10 10 10 10 13 13 13 13 13 0 0 0 0 0 0 0 0 10 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 13 13 10 10 10 10 10 10 10 10 10 10 10 10 10 10 13 13 13 13 13 13 0 0 0 0 0 0 0 0 0 0 13 13 10 10 10 10 10 10 0 0 0 10 0 10 0 0 10 10 10 10 10 10 13 13 13 13 13 13 13 13 0 0 10 10 10 10 10 10 10 13 13 13 13 13 13 13 13 13 10 10 10 13 13 13 13 13 13 13 13 13 13 13 13 13 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 13 13 13 0 0 0 0 0 0 0 0 0 10 10 10 13 10 10 10 10 10 10 10 10 13 13 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 0 0 13 0 13 13 13 0 0 13 13 0 0 0 0 0 13 13 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 10 10 13 13 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 0 0 10 10 10 10 10 10 0 0 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 10 10 10 13 13 13 13 13 13 13 13 0 13 13 0 0 10 10 10 10 10 10 10 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 0 0 0 0 0 10 10 10 10 10 0 0 0 0 0 14 13 14 14 14 14 14 14 14 14 14 14 0 14 14 14 14 14 14 14 14 14 14 14 14 14 0 14 14 14 14 14 0 14 0 14 14 0 14 14 0 14 14 14 14 14 14 14 14 14 14 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 10 6 0 0 9 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 11 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 11 11 11 6 0 7 0 6 9 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 0 12 0 0 0 0 0 0 0 7 0 0 0 0 6 0 7 0 0 0 0 0 0 0 0 0 0 0 9 6 0 0 0 0 0 0 0 0 0 0 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 13 13 0 0 10 10 10 10 10 10 0 0 10 10 10 10 10 10 0 0 10 10 10 10 10 10 0 0 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 12 12 12 0 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 0 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 0 0 10 10 10 10 10 10 13 13 13 13 13 0 0 0 0 0 10 10 10 10 0 0 0 0 10 10 10 10 10 10 10 10 0 10 10 10 10 10 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 0 0 10 0 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 0 0 0 10 0 0 10 10 10 10 0 10 10 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 0 0 0 0 0 0 10 10 10 13 13 13 0 13 13 0 0 0 0 0 13 13 13 13 10 10 10 10 0 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 0 0 0 0 13 13 13 0 0 0 0 13 10 10 10 10 10 13 13 0 0 0 0 0 0 0 0 0 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 0 0 12 0 0 13 13 13 13 13 0 8 8 8 8 8 8 8 8 8 8 10 10 10 13 0 0 10 0 0 0 0 0 0 0 0 0 13 10 10 10 10 0 0 0 0 0 13 13 13 0 0 0 8 8 8 8 8 8 8 8 8 8 10 0 10 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 13 0 0 0 0 0 0 13 0 10 10 10 10 10 10 10 0 10 0 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 13 13 13 13 13 13 13 13 13 13 13 0 0 0 0 0 13 13 13 13 0 10 10 10 10 10 10 10 10 0 0 10 10 0 0 0 0 0 0 13 0 0 0 0 0 10 10 10 10 10 13 13 0 0 13 13 13 13 13 13 13 0 0 0 13 13 13 13 13 0 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 10 10 10 10 0 0 0 0 0 13 13 13 13 10 10 0 10 0 0 0 0 0 0 0 0 13 13 13 13 13 13 0 0 13 13 13 13 13 13 13 13 0 0 0 0 0 0 0 0 10 10 10 10 13 13 0 0 13 0 0 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 13 13 13 13 13 13 0 13 13 13 13 13 13 13 13 0 0 13 13 13 13 13 13 13 13 13 13 13 13 13 13 10 10 10 10 10 10 10 10 0 0 0 0 0 10 10 10 10 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 10 10 0 0 0 13 13 0 12 12 12 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 13 0 0 0 13 13 13 13 13 13 12 12 12 12 12 12 12 12 13 13 13 13 13 13 13 13 0 0 13 13 13 13 13 13 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 0 0 0 0 13 13 13 0 0 0 0 0 0 0 0 0 0 0 0 0 10 0 0 10 10 0 0 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 0 10 0 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 0 0 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 0 10 10 10 10 10 0 10 0 0 0 10 10 10 10 10 10 10 10 10 10 10 10 0 0 10 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 10 10 10 10 0 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 13 13 13 13 13 13 13 0 0 0 0 13 13 13 13 13 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 13 13 13 13 13 13 13 13 13 0 0 13 13 13 13 13 13 13 0 13 13 0 13 13 13 13 13 0 0 0 0 0 10 10 10 10 13 13 13 13 13 13 13 0 0 0 0 0 0 10 10 0 10 0 0 10 0 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 0 10 0 10 0 0 0 0 0 0 10 0 0 0 0 10 0 10 0 10 0 10 10 10 0 10 10 0 10 0 0 10 0 10 0 10 0 10 0 10 0 10 10 0 10 0 0 10 10 10 10 0 10 10 10 10 10 10 10 0 10 10 10 10 0 10 10 10 10 0 10 0 10 10 10 10 10 10 10 10 10 10 0 10 10 10 10 10 0 10 10 10 0 10 10 10 10 10 0 10 10 10 10 10 0 0 0 0 0 0 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 16 16 0 0 0 0 0 16 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 20 20 20 20 20 0 0 16 16 0 0 16 16 16 16 16 16 16 16 16 16 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 21 21 21 21 0 0 0 0 16 0 16 16 16 16 16 16 16 16 16 0 0 0 16 0 0 0 0 16 16 16 0 16 16 16 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 16 0 0 0 0 16 0 0 0 0 0 16 0 0 0 0 16 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 16 16 16 0 0 0 16 16 16 16 16 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16 16 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16 16 16 16 16 16 16 0 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 16 0 0 16 16 16 16 16 16 16 0 0 16 16 16 0 0 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define |ucd-WB-table-5|
  '#(20 12 16 0 1 19 15 13 17 14 11 5 7 6 9 21 2 8 10 18 4 3))
