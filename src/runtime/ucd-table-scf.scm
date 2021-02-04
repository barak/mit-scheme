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

;;;; UCD property: scf (simple-case-folding)

;;; Generated from Unicode 9.0.0

(declare (usual-integrations))

(define (ucd-scf-value char)
  (or (let ((sv (char->integer char)))
        (vector-ref ucd-scf-table-5 (bytevector-u16be-ref ucd-scf-table-4 (fix:lsh (fix:or (fix:lsh (bytevector-u8-ref ucd-scf-table-3 (fix:or (fix:lsh (bytevector-u8-ref ucd-scf-table-2 (fix:or (fix:lsh (bytevector-u8-ref ucd-scf-table-1 (fix:or (fix:lsh (bytevector-u8-ref ucd-scf-table-0 (fix:lsh sv -16)) 4) (fix:and 15 (fix:lsh sv -12)))) 4) (fix:and 15 (fix:lsh sv -8)))) 4) (fix:and 15 (fix:lsh sv -4)))) 4) (fix:and 15 sv)) 1))))
      char))

(define ucd-scf-table-0
  '#u8(0 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))

(define ucd-scf-table-1
  '#u8(0 1 2 3 3 3 3 3 3 3 4 3 3 3 3 5 6 7 3 3 3 3 3 3 3 3 3 3 3 3 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))

(define ucd-scf-table-2
  '#u8(0 1 2 3 4 5 6 6 6 6 6 6 6 6 6 6 7 6 6 8 6 6 6 6 6 6 6 6 9 6 10 11 6 12 6 6 13 6 6 6 6 6 6 6 14 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 15 16 6 6 6 17 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 18 6 6 6 6 19 6 6 6 6 6 6 6 20 6 6 6 6 6 6 6 6 6 6 6 21 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 22 6 6 6 6 6 6))

(define ucd-scf-table-3
  '#u8(0 0 0 0 1 2 0 0 0 0 0 3 4 5 0 0 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 27 0 0 28 29 30 31 0 32 33 34 35 36 37 38 0 0 0 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 55 56 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 58 0 0 0 0 0 0 0 0 59 0 0 0 0 0 0 0 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 0 83 84 85 86 87 88 89 90 0 0 91 92 0 0 93 0 94 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 95 96 0 0 0 97 98 99 0 0 0 100 101 102 103 104 105 106 107 108 109 0 0 0 0 110 111 112 0 113 114 0 0 0 0 0 0 0 0 115 116 117 118 119 120 121 122 123 124 0 0 0 0 0 0 0 0 0 0 0 125 126 127 128 129 0 0 0 0 0 0 130 131 0 0 0 0 0 0 0 0 0 0 0 0 132 133 134 0 0 0 0 0 0 0 0 135 136 137 0 0 0 0 0 0 0 0 0 0 138 139 140 141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 142 143 0 0 0 0 144 145 146 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define ucd-scf-table-4
  '#u8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0 10 0 11 0 12 0 13 0 14 0 15 0 16 0 17 0 18 0 19 0 20 0 21 0 22 0 23 0 24 0 25 0 26 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 27 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 28 0 29 0 30 0 31 0 32 0 33 0 34 0 35 0 36 0 37 0 38 0 39 0 40 0 41 0 42 0 43 0 44 0 45 0 46 0 47 0 48 0 49 0 50 0 0 0 51 0 52 0 53 0 54 0 55 0 56 0 57 0 0 0 58 0 0 0 59 0 0 0 60 0 0 0 61 0 0 0 62 0 0 0 63 0 0 0 64 0 0 0 65 0 0 0 66 0 0 0 67 0 0 0 68 0 0 0 69 0 0 0 70 0 0 0 71 0 0 0 72 0 0 0 73 0 0 0 74 0 0 0 75 0 0 0 76 0 0 0 77 0 0 0 78 0 0 0 79 0 0 0 80 0 0 0 81 0 0 0 0 0 0 0 82 0 0 0 83 0 0 0 84 0 0 0 0 0 85 0 0 0 86 0 0 0 87 0 0 0 88 0 0 0 89 0 0 0 90 0 0 0 91 0 0 0 92 0 0 0 0 0 93 0 0 0 94 0 0 0 95 0 0 0 96 0 0 0 97 0 0 0 98 0 0 0 99 0 0 0 100 0 0 0 101 0 0 0 102 0 0 0 103 0 0 0 104 0 0 0 105 0 0 0 106 0 0 0 107 0 0 0 108 0 0 0 109 0 0 0 110 0 0 0 111 0 0 0 112 0 0 0 113 0 0 0 114 0 0 0 115 0 0 0 116 0 117 0 0 0 118 0 0 0 119 0 0 0 19 0 0 0 120 0 121 0 0 0 122 0 0 0 123 0 124 0 0 0 125 0 126 0 127 0 0 0 0 0 128 0 129 0 130 0 131 0 0 0 132 0 133 0 0 0 134 0 135 0 136 0 0 0 0 0 0 0 137 0 138 0 0 0 139 0 140 0 0 0 141 0 0 0 142 0 0 0 143 0 144 0 0 0 145 0 0 0 0 0 146 0 0 0 147 0 148 0 0 0 149 0 150 0 151 0 0 0 152 0 0 0 153 0 154 0 0 0 0 0 0 0 155 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 156 0 156 0 0 0 157 0 157 0 0 0 158 0 158 0 0 0 159 0 0 0 160 0 0 0 161 0 0 0 162 0 0 0 163 0 0 0 164 0 0 0 165 0 0 0 166 0 0 0 0 0 167 0 0 0 168 0 0 0 169 0 0 0 170 0 0 0 171 0 0 0 172 0 0 0 173 0 0 0 174 0 0 0 175 0 0 0 0 0 176 0 176 0 0 0 177 0 0 0 178 0 179 0 180 0 0 0 181 0 0 0 182 0 0 0 183 0 0 0 184 0 0 0 185 0 0 0 186 0 0 0 187 0 0 0 188 0 0 0 189 0 0 0 190 0 0 0 191 0 0 0 192 0 0 0 193 0 0 0 194 0 0 0 195 0 0 0 196 0 0 0 197 0 0 0 198 0 0 0 199 0 0 0 200 0 0 0 201 0 0 0 202 0 0 0 203 0 0 0 204 0 0 0 205 0 0 0 206 0 0 0 207 0 0 0 208 0 0 0 209 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 210 0 211 0 0 0 212 0 213 0 0 0 0 0 214 0 0 0 215 0 216 0 217 0 218 0 0 0 219 0 0 0 220 0 0 0 221 0 0 0 222 0 0 0 0 0 0 0 0 0 0 0 0 0 223 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 224 0 0 0 225 0 0 0 0 0 0 0 226 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 227 0 0 0 0 0 0 0 0 0 0 0 0 0 228 0 0 0 229 0 230 0 231 0 0 0 232 0 0 0 233 0 234 0 0 0 235 0 236 0 237 0 238 0 239 0 240 0 241 0 242 0 223 0 243 0 244 0 27 0 245 0 246 0 247 0 248 0 249 0 0 0 250 0 251 0 252 0 253 0 254 0 255 1 0 1 1 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 250 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 0 236 0 242 0 0 0 0 0 0 0 253 0 248 0 0 1 4 0 0 1 5 0 0 1 6 0 0 1 7 0 0 1 8 0 0 1 9 0 0 1 10 0 0 1 11 0 0 1 12 0 0 1 13 0 0 1 14 0 0 1 15 0 0 0 243 0 249 0 0 0 0 0 242 0 239 0 0 1 16 0 0 1 17 1 18 0 0 0 0 1 19 1 20 1 21 1 22 1 23 1 24 1 25 1 26 1 27 1 28 1 29 1 30 1 31 1 32 1 33 1 34 1 35 1 36 1 37 1 38 1 39 1 40 1 41 1 42 1 43 1 44 1 45 1 46 1 47 1 48 1 49 1 50 1 51 1 52 1 53 1 54 1 55 1 56 1 57 1 58 1 59 1 60 1 61 1 62 1 63 1 64 1 65 1 66 1 67 1 68 1 69 1 70 0 0 1 71 0 0 1 72 0 0 1 73 0 0 1 74 0 0 1 75 0 0 1 76 0 0 1 77 0 0 1 78 0 0 1 79 0 0 1 80 0 0 1 81 0 0 1 82 0 0 1 83 0 0 1 84 0 0 1 85 0 0 1 86 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 87 0 0 1 88 0 0 1 89 0 0 1 90 0 0 1 91 0 0 1 92 0 0 1 93 0 0 1 94 0 0 1 95 0 0 1 96 0 0 1 97 0 0 1 98 0 0 1 99 0 0 1 100 0 0 1 101 0 0 1 102 0 0 1 103 0 0 1 104 0 0 1 105 0 0 1 106 0 0 1 107 0 0 1 108 0 0 1 109 0 0 1 110 0 0 1 111 0 0 1 112 0 0 1 113 0 0 1 114 1 115 0 0 1 116 0 0 1 117 0 0 1 118 0 0 1 119 0 0 1 120 0 0 1 121 0 0 0 0 1 122 0 0 1 123 0 0 1 124 0 0 1 125 0 0 1 126 0 0 1 127 0 0 1 128 0 0 1 129 0 0 1 130 0 0 1 131 0 0 1 132 0 0 1 133 0 0 1 134 0 0 1 135 0 0 1 136 0 0 1 137 0 0 1 138 0 0 1 139 0 0 1 140 0 0 1 141 0 0 1 142 0 0 1 143 0 0 1 144 0 0 1 145 0 0 1 146 0 0 1 147 0 0 1 148 0 0 1 149 0 0 1 150 0 0 1 151 0 0 1 152 0 0 1 153 0 0 1 154 0 0 1 155 0 0 1 156 0 0 1 157 0 0 1 158 0 0 1 159 0 0 1 160 0 0 1 161 0 0 1 162 0 0 1 163 0 0 1 164 0 0 1 165 0 0 1 166 0 0 1 167 0 0 1 168 0 0 1 169 0 0 0 0 1 170 1 171 1 172 1 173 1 174 1 175 1 176 1 177 1 178 1 179 1 180 1 181 1 182 1 183 1 184 1 185 1 186 1 187 1 188 1 189 1 190 1 191 1 192 1 193 1 194 1 195 1 196 1 197 1 198 1 199 1 200 1 201 1 202 1 203 1 204 1 205 1 206 1 207 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 208 1 209 1 210 1 211 1 212 1 213 1 214 1 215 1 216 1 217 1 218 1 219 1 220 1 221 1 222 1 223 1 224 1 225 1 226 1 227 1 228 1 229 1 230 1 231 1 232 1 233 1 234 1 235 1 236 1 237 1 238 1 239 1 240 1 241 1 242 1 243 1 244 1 245 0 0 1 246 0 0 0 0 0 0 0 0 0 0 1 247 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 248 1 249 1 250 1 251 1 252 1 253 0 0 0 0 1 40 1 42 1 52 1 55 1 56 1 56 1 64 1 71 1 254 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 255 0 0 2 0 0 0 2 1 0 0 2 2 0 0 2 3 0 0 2 4 0 0 2 5 0 0 2 6 0 0 2 7 0 0 2 8 0 0 2 9 0 0 2 10 0 0 2 11 0 0 2 12 0 0 2 13 0 0 2 14 0 0 2 15 0 0 2 16 0 0 2 17 0 0 2 18 0 0 2 19 0 0 2 20 0 0 2 21 0 0 2 22 0 0 2 23 0 0 2 24 0 0 2 25 0 0 2 26 0 0 2 27 0 0 2 28 0 0 2 29 0 0 2 30 0 0 2 31 0 0 2 32 0 0 2 33 0 0 2 34 0 0 2 35 0 0 2 36 0 0 2 37 0 0 2 38 0 0 2 39 0 0 2 40 0 0 2 41 0 0 2 42 0 0 2 43 0 0 2 44 0 0 2 45 0 0 2 46 0 0 2 47 0 0 2 48 0 0 2 49 0 0 2 50 0 0 2 51 0 0 2 52 0 0 2 53 0 0 2 54 0 0 2 55 0 0 2 56 0 0 2 57 0 0 2 58 0 0 2 59 0 0 2 60 0 0 2 61 0 0 2 62 0 0 2 63 0 0 2 64 0 0 2 65 0 0 2 66 0 0 2 67 0 0 2 68 0 0 2 69 0 0 2 70 0 0 2 71 0 0 2 72 0 0 2 73 0 0 0 0 0 0 0 0 0 0 0 0 2 47 0 0 0 0 2 74 0 0 2 75 0 0 2 76 0 0 2 77 0 0 2 78 0 0 2 79 0 0 2 80 0 0 2 81 0 0 2 82 0 0 2 83 0 0 2 84 0 0 2 85 0 0 2 86 0 0 2 87 0 0 2 88 0 0 2 89 0 0 2 90 0 0 2 91 0 0 2 92 0 0 2 93 0 0 2 94 0 0 2 95 0 0 2 96 0 0 2 97 0 0 2 98 0 0 2 99 0 0 2 100 0 0 2 101 0 0 2 102 0 0 2 103 0 0 2 104 0 0 2 105 0 0 2 106 0 0 2 107 0 0 2 108 0 0 2 109 0 0 2 110 0 0 2 111 0 0 2 112 0 0 2 113 0 0 2 114 0 0 2 115 0 0 2 116 0 0 2 117 0 0 2 118 0 0 2 119 0 0 2 120 0 0 2 121 0 0 2 122 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 123 2 124 2 125 2 126 2 127 2 128 2 129 2 130 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 131 2 132 2 133 2 134 2 135 2 136 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 137 2 138 2 139 2 140 2 141 2 142 2 143 2 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 145 2 146 2 147 2 148 2 149 2 150 2 151 2 152 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 153 2 154 2 155 2 156 2 157 2 158 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 159 0 0 2 160 0 0 2 161 0 0 2 162 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 163 2 164 2 165 2 166 2 167 2 168 2 169 2 170 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 171 2 172 2 173 2 174 2 175 2 176 2 177 2 178 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 179 2 180 2 181 2 182 2 183 2 184 2 185 2 186 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 187 2 188 2 189 2 190 2 191 2 192 2 193 2 194 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 195 2 196 2 197 2 198 2 199 0 0 0 223 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 200 2 201 2 202 2 203 2 204 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 205 2 206 2 207 2 208 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 209 2 210 2 211 2 212 2 213 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 214 2 215 2 216 2 217 2 218 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 11 0 33 0 0 0 0 0 0 0 0 0 0 0 0 2 219 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 220 2 221 2 222 2 223 2 224 2 225 2 226 2 227 2 228 2 229 2 230 2 231 2 232 2 233 2 234 2 235 0 0 0 0 0 0 2 236 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 237 2 238 2 239 2 240 2 241 2 242 2 243 2 244 2 245 2 246 2 247 2 248 2 249 2 250 2 251 2 252 2 253 2 254 2 255 3 0 3 1 3 2 3 3 3 4 3 5 3 6 3 7 3 8 3 9 3 10 3 11 3 12 3 13 3 14 3 15 3 16 3 17 3 18 3 19 3 20 3 21 3 22 3 23 3 24 3 25 3 26 3 27 3 28 3 29 3 30 3 31 3 32 3 33 3 34 3 35 3 36 3 37 3 38 3 39 3 40 3 41 3 42 3 43 3 44 3 45 3 46 3 47 3 48 3 49 3 50 3 51 3 52 3 53 0 0 3 54 0 0 3 55 3 56 3 57 0 0 0 0 3 58 0 0 3 59 0 0 3 60 0 0 3 61 3 62 3 63 3 64 0 0 3 65 0 0 0 0 3 66 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 67 3 68 3 69 0 0 3 70 0 0 3 71 0 0 3 72 0 0 3 73 0 0 3 74 0 0 3 75 0 0 3 76 0 0 3 77 0 0 3 78 0 0 3 79 0 0 3 80 0 0 3 81 0 0 3 82 0 0 3 83 0 0 3 84 0 0 3 85 0 0 3 86 0 0 3 87 0 0 3 88 0 0 3 89 0 0 3 90 0 0 3 91 0 0 3 92 0 0 3 93 0 0 3 94 0 0 3 95 0 0 3 96 0 0 3 97 0 0 3 98 0 0 3 99 0 0 3 100 0 0 3 101 0 0 3 102 0 0 3 103 0 0 3 104 0 0 3 105 0 0 3 106 0 0 3 107 0 0 3 108 0 0 3 109 0 0 3 110 0 0 3 111 0 0 3 112 0 0 3 113 0 0 3 114 0 0 3 115 0 0 3 116 0 0 3 117 0 0 3 118 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 119 0 0 3 120 0 0 0 0 0 0 0 0 3 121 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 122 0 0 3 123 0 0 3 124 0 0 3 125 0 0 3 126 0 0 1 254 0 0 3 127 0 0 3 128 0 0 3 129 0 0 3 130 0 0 3 131 0 0 3 132 0 0 3 133 0 0 3 134 0 0 3 135 0 0 3 136 0 0 3 137 0 0 3 138 0 0 3 139 0 0 3 140 0 0 3 141 0 0 3 142 0 0 3 143 0 0 0 0 0 0 3 144 0 0 3 145 0 0 3 146 0 0 3 147 0 0 3 148 0 0 3 149 0 0 3 150 0 0 3 151 0 0 3 152 0 0 3 153 0 0 3 154 0 0 3 155 0 0 3 156 0 0 3 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 158 0 0 3 159 0 0 3 160 0 0 3 161 0 0 3 162 0 0 3 163 0 0 3 164 0 0 0 0 0 0 3 165 0 0 3 166 0 0 3 167 0 0 3 168 0 0 3 169 0 0 3 170 0 0 3 171 0 0 3 172 0 0 3 173 0 0 3 174 0 0 3 175 0 0 3 176 0 0 3 177 0 0 3 178 0 0 3 179 0 0 3 180 0 0 3 181 0 0 3 182 0 0 3 183 0 0 3 184 0 0 3 185 0 0 3 186 0 0 3 187 0 0 3 188 0 0 3 189 0 0 3 190 0 0 3 191 0 0 3 192 0 0 3 193 0 0 3 194 0 0 3 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 196 0 0 3 197 0 0 3 198 3 199 0 0 3 200 0 0 3 201 0 0 3 202 0 0 3 203 0 0 0 0 0 0 0 0 3 204 0 0 3 205 0 0 0 0 3 206 0 0 3 207 0 0 0 0 0 0 3 208 0 0 3 209 0 0 3 210 0 0 3 211 0 0 3 212 0 0 3 213 0 0 3 214 0 0 3 215 0 0 3 216 0 0 3 217 0 0 3 218 3 219 3 220 3 221 3 222 0 0 3 223 3 224 3 225 3 226 3 227 0 0 3 228 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 229 3 230 3 231 3 232 3 233 3 234 3 235 3 236 3 237 3 238 3 239 3 240 3 241 3 242 3 243 3 244 3 245 3 246 3 247 3 248 3 249 3 250 3 251 3 252 3 253 3 254 3 255 4 0 4 1 4 2 4 3 4 4 4 5 4 6 4 7 4 8 4 9 4 10 4 11 4 12 4 13 4 14 4 15 4 16 4 17 4 18 4 19 4 20 4 21 4 22 4 23 4 24 4 25 4 26 4 27 4 28 4 29 4 30 4 31 4 32 4 33 4 34 4 35 4 36 4 37 4 38 4 39 4 40 4 41 4 42 4 43 4 44 4 45 4 46 4 47 4 48 4 49 4 50 4 51 4 52 0 0 4 53 4 54 4 55 4 56 4 57 4 58 4 59 4 60 4 61 4 62 4 63 4 64 4 65 4 66 4 67 4 68 4 69 4 70 4 71 4 72 4 73 4 74 4 75 4 76 4 77 4 78 0 0 0 0 0 0 0 0 0 0 4 79 4 80 4 81 4 82 4 83 4 84 4 85 4 86 4 87 4 88 4 89 4 90 4 91 4 92 4 93 4 94 4 95 4 96 4 97 4 98 4 99 4 100 4 101 4 102 4 103 4 104 4 105 4 106 4 107 4 108 4 109 4 110 4 111 4 112 4 113 4 114 4 115 4 116 4 117 4 118 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 119 4 120 4 121 4 122 4 123 4 124 4 125 4 126 4 127 4 128 4 129 4 130 4 131 4 132 4 133 4 134 4 135 4 136 4 137 4 138 4 139 4 140 4 141 4 142 4 143 4 144 4 145 4 146 4 147 4 148 4 149 4 150 4 151 4 152 4 153 4 154 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 155 4 156 4 157 4 158 4 159 4 160 4 161 4 162 4 163 4 164 4 165 4 166 4 167 4 168 4 169 4 170 4 171 4 172 4 173 4 174 4 175 4 176 4 177 4 178 4 179 4 180 4 181 4 182 4 183 4 184 4 185 4 186 4 187 4 188 4 189 4 190 4 191 4 192 4 193 4 194 4 195 4 196 4 197 4 198 4 199 4 200 4 201 4 202 4 203 4 204 4 205 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 206 4 207 4 208 4 209 4 210 4 211 4 212 4 213 4 214 4 215 4 216 4 217 4 218 4 219 4 220 4 221 4 222 4 223 4 224 4 225 4 226 4 227 4 228 4 229 4 230 4 231 4 232 4 233 4 234 4 235 4 236 4 237 4 238 4 239 4 240 4 241 4 242 4 243 4 244 4 245 4 246 4 247 4 248 4 249 4 250 4 251 4 252 4 253 4 254 4 255 5 0 5 1 5 2 5 3 5 4 5 5 5 6 5 7 5 8 5 9 5 10 5 11 5 12 5 13 5 14 5 15 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define ucd-scf-table-5
  '#(#f       #\a      #\b      #\c      #\d      #\e      #\f      #\g      #\h      #\i      #\j      #\k      #\l      #\m      #\n      #\o      #\p      #\q      #\r      #\s      #\t      #\u      #\v      #\w      #\x      #\y      #\z      #\x3bc   #\à      #\á      #\â      #\ã      #\ä      #\å      #\æ      #\ç      #\è      #\é      #\ê      #\ë      #\ì      #\í      #\î      #\ï      #\ð      #\ñ      #\ò      #\ó      #\ô      #\õ      #\ö      #\ø      #\ù      #\ú      #\û      #\ü      #\ý      #\þ      #\x101   #\x103   #\x105   #\x107   #\x109   #\x10b   #\x10d   #\x10f   #\x111   #\x113   #\x115   #\x117   #\x119   #\x11b   #\x11d   #\x11f   #\x121   #\x123   #\x125   #\x127   #\x129   #\x12b   #\x12d   #\x12f   #\x133   #\x135   #\x137   #\x13a   #\x13c   #\x13e   #\x140   #\x142   #\x144   #\x146   #\x148   #\x14b   #\x14d   #\x14f   #\x151   #\x153   #\x155   #\x157   #\x159   #\x15b   #\x15d   #\x15f   #\x161   #\x163   #\x165   #\x167   #\x169
     #\x16b   #\x16d   #\x16f   #\x171   #\x173   #\x175   #\x177   #\ÿ      #\x17a   #\x17c   #\x17e   #\x253   #\x183   #\x185   #\x254   #\x188   #\x256   #\x257   #\x18c   #\x1dd   #\x259   #\x25b   #\x192   #\x260   #\x263   #\x269   #\x268   #\x199   #\x26f   #\x272   #\x275   #\x1a1   #\x1a3   #\x1a5   #\x280   #\x1a8   #\x283   #\x1ad   #\x288   #\x1b0   #\x28a   #\x28b   #\x1b4   #\x1b6   #\x292   #\x1b9   #\x1bd   #\x1c6   #\x1c9   #\x1cc   #\x1ce   #\x1d0   #\x1d2   #\x1d4   #\x1d6   #\x1d8   #\x1da   #\x1dc   #\x1df   #\x1e1   #\x1e3   #\x1e5   #\x1e7   #\x1e9   #\x1eb   #\x1ed   #\x1ef   #\x1f3   #\x1f5   #\x195   #\x1bf   #\x1f9   #\x1fb   #\x1fd   #\x1ff   #\x201   #\x203   #\x205   #\x207   #\x209   #\x20b   #\x20d   #\x20f   #\x211   #\x213   #\x215   #\x217   #\x219   #\x21b   #\x21d   #\x21f   #\x19e   #\x223   #\x225   #\x227   #\x229   #\x22b   #\x22d   #\x22f   #\x231   #\x233   #\x2c65  #\x23c   #\x19a   #\x2c66  #\x242   #\x180   #\x289   #\x28c
     #\x247   #\x249   #\x24b   #\x24d   #\x24f   #\x3b9   #\x371   #\x373   #\x377   #\x3f3   #\x3ac   #\x3ad   #\x3ae   #\x3af   #\x3cc   #\x3cd   #\x3ce   #\x3b1   #\x3b2   #\x3b3   #\x3b4   #\x3b5   #\x3b6   #\x3b7   #\x3b8   #\x3ba   #\x3bb   #\x3bd   #\x3be   #\x3bf   #\x3c0   #\x3c1   #\x3c3   #\x3c4   #\x3c5   #\x3c6   #\x3c7   #\x3c8   #\x3c9   #\x3ca   #\x3cb   #\x3d7   #\x3d9   #\x3db   #\x3dd   #\x3df   #\x3e1   #\x3e3   #\x3e5   #\x3e7   #\x3e9   #\x3eb   #\x3ed   #\x3ef   #\x3f8   #\x3f2   #\x3fb   #\x37b   #\x37c   #\x37d   #\x450   #\x451   #\x452   #\x453   #\x454   #\x455   #\x456   #\x457   #\x458   #\x459   #\x45a   #\x45b   #\x45c   #\x45d   #\x45e   #\x45f   #\x430   #\x431   #\x432   #\x433   #\x434   #\x435   #\x436   #\x437   #\x438   #\x439   #\x43a   #\x43b   #\x43c   #\x43d   #\x43e   #\x43f   #\x440   #\x441   #\x442   #\x443   #\x444   #\x445   #\x446   #\x447   #\x448   #\x449   #\x44a   #\x44b   #\x44c   #\x44d   #\x44e   #\x44f   #\x461
     #\x463   #\x465   #\x467   #\x469   #\x46b   #\x46d   #\x46f   #\x471   #\x473   #\x475   #\x477   #\x479   #\x47b   #\x47d   #\x47f   #\x481   #\x48b   #\x48d   #\x48f   #\x491   #\x493   #\x495   #\x497   #\x499   #\x49b   #\x49d   #\x49f   #\x4a1   #\x4a3   #\x4a5   #\x4a7   #\x4a9   #\x4ab   #\x4ad   #\x4af   #\x4b1   #\x4b3   #\x4b5   #\x4b7   #\x4b9   #\x4bb   #\x4bd   #\x4bf   #\x4cf   #\x4c2   #\x4c4   #\x4c6   #\x4c8   #\x4ca   #\x4cc   #\x4ce   #\x4d1   #\x4d3   #\x4d5   #\x4d7   #\x4d9   #\x4db   #\x4dd   #\x4df   #\x4e1   #\x4e3   #\x4e5   #\x4e7   #\x4e9   #\x4eb   #\x4ed   #\x4ef   #\x4f1   #\x4f3   #\x4f5   #\x4f7   #\x4f9   #\x4fb   #\x4fd   #\x4ff   #\x501   #\x503   #\x505   #\x507   #\x509   #\x50b   #\x50d   #\x50f   #\x511   #\x513   #\x515   #\x517   #\x519   #\x51b   #\x51d   #\x51f   #\x521   #\x523   #\x525   #\x527   #\x529   #\x52b   #\x52d   #\x52f   #\x561   #\x562   #\x563   #\x564   #\x565   #\x566   #\x567   #\x568   #\x569   #\x56a
     #\x56b   #\x56c   #\x56d   #\x56e   #\x56f   #\x570   #\x571   #\x572   #\x573   #\x574   #\x575   #\x576   #\x577   #\x578   #\x579   #\x57a   #\x57b   #\x57c   #\x57d   #\x57e   #\x57f   #\x580   #\x581   #\x582   #\x583   #\x584   #\x585   #\x586   #\x2d00  #\x2d01  #\x2d02  #\x2d03  #\x2d04  #\x2d05  #\x2d06  #\x2d07  #\x2d08  #\x2d09  #\x2d0a  #\x2d0b  #\x2d0c  #\x2d0d  #\x2d0e  #\x2d0f  #\x2d10  #\x2d11  #\x2d12  #\x2d13  #\x2d14  #\x2d15  #\x2d16  #\x2d17  #\x2d18  #\x2d19  #\x2d1a  #\x2d1b  #\x2d1c  #\x2d1d  #\x2d1e  #\x2d1f  #\x2d20  #\x2d21  #\x2d22  #\x2d23  #\x2d24  #\x2d25  #\x2d27  #\x2d2d  #\x13f0  #\x13f1  #\x13f2  #\x13f3  #\x13f4  #\x13f5  #\xa64b  #\x1e01  #\x1e03  #\x1e05  #\x1e07  #\x1e09  #\x1e0b  #\x1e0d  #\x1e0f  #\x1e11  #\x1e13  #\x1e15  #\x1e17  #\x1e19  #\x1e1b  #\x1e1d  #\x1e1f  #\x1e21  #\x1e23  #\x1e25  #\x1e27  #\x1e29  #\x1e2b  #\x1e2d  #\x1e2f  #\x1e31  #\x1e33  #\x1e35  #\x1e37  #\x1e39  #\x1e3b  #\x1e3d  #\x1e3f  #\x1e41  #\x1e43
     #\x1e45  #\x1e47  #\x1e49  #\x1e4b  #\x1e4d  #\x1e4f  #\x1e51  #\x1e53  #\x1e55  #\x1e57  #\x1e59  #\x1e5b  #\x1e5d  #\x1e5f  #\x1e61  #\x1e63  #\x1e65  #\x1e67  #\x1e69  #\x1e6b  #\x1e6d  #\x1e6f  #\x1e71  #\x1e73  #\x1e75  #\x1e77  #\x1e79  #\x1e7b  #\x1e7d  #\x1e7f  #\x1e81  #\x1e83  #\x1e85  #\x1e87  #\x1e89  #\x1e8b  #\x1e8d  #\x1e8f  #\x1e91  #\x1e93  #\x1e95  #\ß      #\x1ea1  #\x1ea3  #\x1ea5  #\x1ea7  #\x1ea9  #\x1eab  #\x1ead  #\x1eaf  #\x1eb1  #\x1eb3  #\x1eb5  #\x1eb7  #\x1eb9  #\x1ebb  #\x1ebd  #\x1ebf  #\x1ec1  #\x1ec3  #\x1ec5  #\x1ec7  #\x1ec9  #\x1ecb  #\x1ecd  #\x1ecf  #\x1ed1  #\x1ed3  #\x1ed5  #\x1ed7  #\x1ed9  #\x1edb  #\x1edd  #\x1edf  #\x1ee1  #\x1ee3  #\x1ee5  #\x1ee7  #\x1ee9  #\x1eeb  #\x1eed  #\x1eef  #\x1ef1  #\x1ef3  #\x1ef5  #\x1ef7  #\x1ef9  #\x1efb  #\x1efd  #\x1eff  #\x1f00  #\x1f01  #\x1f02  #\x1f03  #\x1f04  #\x1f05  #\x1f06  #\x1f07  #\x1f10  #\x1f11  #\x1f12  #\x1f13  #\x1f14  #\x1f15  #\x1f20  #\x1f21  #\x1f22  #\x1f23  #\x1f24
     #\x1f25  #\x1f26  #\x1f27  #\x1f30  #\x1f31  #\x1f32  #\x1f33  #\x1f34  #\x1f35  #\x1f36  #\x1f37  #\x1f40  #\x1f41  #\x1f42  #\x1f43  #\x1f44  #\x1f45  #\x1f51  #\x1f53  #\x1f55  #\x1f57  #\x1f60  #\x1f61  #\x1f62  #\x1f63  #\x1f64  #\x1f65  #\x1f66  #\x1f67  #\x1f80  #\x1f81  #\x1f82  #\x1f83  #\x1f84  #\x1f85  #\x1f86  #\x1f87  #\x1f90  #\x1f91  #\x1f92  #\x1f93  #\x1f94  #\x1f95  #\x1f96  #\x1f97  #\x1fa0  #\x1fa1  #\x1fa2  #\x1fa3  #\x1fa4  #\x1fa5  #\x1fa6  #\x1fa7  #\x1fb0  #\x1fb1  #\x1f70  #\x1f71  #\x1fb3  #\x1f72  #\x1f73  #\x1f74  #\x1f75  #\x1fc3  #\x1fd0  #\x1fd1  #\x1f76  #\x1f77  #\x1fe0  #\x1fe1  #\x1f7a  #\x1f7b  #\x1fe5  #\x1f78  #\x1f79  #\x1f7c  #\x1f7d  #\x1ff3  #\x214e  #\x2170  #\x2171  #\x2172  #\x2173  #\x2174  #\x2175  #\x2176  #\x2177  #\x2178  #\x2179  #\x217a  #\x217b  #\x217c  #\x217d  #\x217e  #\x217f  #\x2184  #\x24d0  #\x24d1  #\x24d2  #\x24d3  #\x24d4  #\x24d5  #\x24d6  #\x24d7  #\x24d8  #\x24d9  #\x24da  #\x24db  #\x24dc  #\x24dd
     #\x24de  #\x24df  #\x24e0  #\x24e1  #\x24e2  #\x24e3  #\x24e4  #\x24e5  #\x24e6  #\x24e7  #\x24e8  #\x24e9  #\x2c30  #\x2c31  #\x2c32  #\x2c33  #\x2c34  #\x2c35  #\x2c36  #\x2c37  #\x2c38  #\x2c39  #\x2c3a  #\x2c3b  #\x2c3c  #\x2c3d  #\x2c3e  #\x2c3f  #\x2c40  #\x2c41  #\x2c42  #\x2c43  #\x2c44  #\x2c45  #\x2c46  #\x2c47  #\x2c48  #\x2c49  #\x2c4a  #\x2c4b  #\x2c4c  #\x2c4d  #\x2c4e  #\x2c4f  #\x2c50  #\x2c51  #\x2c52  #\x2c53  #\x2c54  #\x2c55  #\x2c56  #\x2c57  #\x2c58  #\x2c59  #\x2c5a  #\x2c5b  #\x2c5c  #\x2c5d  #\x2c5e  #\x2c61  #\x26b   #\x1d7d  #\x27d   #\x2c68  #\x2c6a  #\x2c6c  #\x251   #\x271   #\x250   #\x252   #\x2c73  #\x2c76  #\x23f   #\x240   #\x2c81  #\x2c83  #\x2c85  #\x2c87  #\x2c89  #\x2c8b  #\x2c8d  #\x2c8f  #\x2c91  #\x2c93  #\x2c95  #\x2c97  #\x2c99  #\x2c9b  #\x2c9d  #\x2c9f  #\x2ca1  #\x2ca3  #\x2ca5  #\x2ca7  #\x2ca9  #\x2cab  #\x2cad  #\x2caf  #\x2cb1  #\x2cb3  #\x2cb5  #\x2cb7  #\x2cb9  #\x2cbb  #\x2cbd  #\x2cbf  #\x2cc1  #\x2cc3  #\x2cc5
     #\x2cc7  #\x2cc9  #\x2ccb  #\x2ccd  #\x2ccf  #\x2cd1  #\x2cd3  #\x2cd5  #\x2cd7  #\x2cd9  #\x2cdb  #\x2cdd  #\x2cdf  #\x2ce1  #\x2ce3  #\x2cec  #\x2cee  #\x2cf3  #\xa641  #\xa643  #\xa645  #\xa647  #\xa649  #\xa64d  #\xa64f  #\xa651  #\xa653  #\xa655  #\xa657  #\xa659  #\xa65b  #\xa65d  #\xa65f  #\xa661  #\xa663  #\xa665  #\xa667  #\xa669  #\xa66b  #\xa66d  #\xa681  #\xa683  #\xa685  #\xa687  #\xa689  #\xa68b  #\xa68d  #\xa68f  #\xa691  #\xa693  #\xa695  #\xa697  #\xa699  #\xa69b  #\xa723  #\xa725  #\xa727  #\xa729  #\xa72b  #\xa72d  #\xa72f  #\xa733  #\xa735  #\xa737  #\xa739  #\xa73b  #\xa73d  #\xa73f  #\xa741  #\xa743  #\xa745  #\xa747  #\xa749  #\xa74b  #\xa74d  #\xa74f  #\xa751  #\xa753  #\xa755  #\xa757  #\xa759  #\xa75b  #\xa75d  #\xa75f  #\xa761  #\xa763  #\xa765  #\xa767  #\xa769  #\xa76b  #\xa76d  #\xa76f  #\xa77a  #\xa77c  #\x1d79  #\xa77f  #\xa781  #\xa783  #\xa785  #\xa787  #\xa78c  #\x265   #\xa791  #\xa793  #\xa797  #\xa799  #\xa79b  #\xa79d  #\xa79f
     #\xa7a1  #\xa7a3  #\xa7a5  #\xa7a7  #\xa7a9  #\x266   #\x25c   #\x261   #\x26c   #\x26a   #\x29e   #\x287   #\x29d   #\xab53  #\xa7b5  #\xa7b7  #\x13a0  #\x13a1  #\x13a2  #\x13a3  #\x13a4  #\x13a5  #\x13a6  #\x13a7  #\x13a8  #\x13a9  #\x13aa  #\x13ab  #\x13ac  #\x13ad  #\x13ae  #\x13af  #\x13b0  #\x13b1  #\x13b2  #\x13b3  #\x13b4  #\x13b5  #\x13b6  #\x13b7  #\x13b8  #\x13b9  #\x13ba  #\x13bb  #\x13bc  #\x13bd  #\x13be  #\x13bf  #\x13c0  #\x13c1  #\x13c2  #\x13c3  #\x13c4  #\x13c5  #\x13c6  #\x13c7  #\x13c8  #\x13c9  #\x13ca  #\x13cb  #\x13cc  #\x13cd  #\x13ce  #\x13cf  #\x13d0  #\x13d1  #\x13d2  #\x13d3  #\x13d4  #\x13d5  #\x13d6  #\x13d7  #\x13d8  #\x13d9  #\x13da  #\x13db  #\x13dc  #\x13dd  #\x13de  #\x13df  #\x13e0  #\x13e1  #\x13e2  #\x13e3  #\x13e4  #\x13e5  #\x13e6  #\x13e7  #\x13e8  #\x13e9  #\x13ea  #\x13eb  #\x13ec  #\x13ed  #\x13ee  #\x13ef  #\xff41  #\xff42  #\xff43  #\xff44  #\xff45  #\xff46  #\xff47  #\xff48  #\xff49  #\xff4a  #\xff4b  #\xff4c  #\xff4d
     #\xff4e  #\xff4f  #\xff50  #\xff51  #\xff52  #\xff53  #\xff54  #\xff55  #\xff56  #\xff57  #\xff58  #\xff59  #\xff5a  #\x10428 #\x10429 #\x1042a #\x1042b #\x1042c #\x1042d #\x1042e #\x1042f #\x10430 #\x10431 #\x10432 #\x10433 #\x10434 #\x10435 #\x10436 #\x10437 #\x10438 #\x10439 #\x1043a #\x1043b #\x1043c #\x1043d #\x1043e #\x1043f #\x10440 #\x10441 #\x10442 #\x10443 #\x10444 #\x10445 #\x10446 #\x10447 #\x10448 #\x10449 #\x1044a #\x1044b #\x1044c #\x1044d #\x1044e #\x1044f #\x104d8 #\x104d9 #\x104da #\x104db #\x104dc #\x104dd #\x104de #\x104df #\x104e0 #\x104e1 #\x104e2 #\x104e3 #\x104e4 #\x104e5 #\x104e6 #\x104e7 #\x104e8 #\x104e9 #\x104ea #\x104eb #\x104ec #\x104ed #\x104ee #\x104ef #\x104f0 #\x104f1 #\x104f2 #\x104f3 #\x104f4 #\x104f5 #\x104f6 #\x104f7 #\x104f8 #\x104f9 #\x104fa #\x104fb #\x10cc0 #\x10cc1 #\x10cc2 #\x10cc3 #\x10cc4 #\x10cc5 #\x10cc6 #\x10cc7 #\x10cc8 #\x10cc9 #\x10cca #\x10ccb #\x10ccc #\x10ccd #\x10cce #\x10ccf #\x10cd0 #\x10cd1 #\x10cd2 #\x10cd3
     #\x10cd4 #\x10cd5 #\x10cd6 #\x10cd7 #\x10cd8 #\x10cd9 #\x10cda #\x10cdb #\x10cdc #\x10cdd #\x10cde #\x10cdf #\x10ce0 #\x10ce1 #\x10ce2 #\x10ce3 #\x10ce4 #\x10ce5 #\x10ce6 #\x10ce7 #\x10ce8 #\x10ce9 #\x10cea #\x10ceb #\x10cec #\x10ced #\x10cee #\x10cef #\x10cf0 #\x10cf1 #\x10cf2 #\x118c0 #\x118c1 #\x118c2 #\x118c3 #\x118c4 #\x118c5 #\x118c6 #\x118c7 #\x118c8 #\x118c9 #\x118ca #\x118cb #\x118cc #\x118cd #\x118ce #\x118cf #\x118d0 #\x118d1 #\x118d2 #\x118d3 #\x118d4 #\x118d5 #\x118d6 #\x118d7 #\x118d8 #\x118d9 #\x118da #\x118db #\x118dc #\x118dd #\x118de #\x118df #\x1e922 #\x1e923 #\x1e924 #\x1e925 #\x1e926 #\x1e927 #\x1e928 #\x1e929 #\x1e92a #\x1e92b #\x1e92c #\x1e92d #\x1e92e #\x1e92f #\x1e930 #\x1e931 #\x1e932 #\x1e933 #\x1e934 #\x1e935 #\x1e936 #\x1e937 #\x1e938 #\x1e939 #\x1e93a #\x1e93b #\x1e93c #\x1e93d #\x1e93e #\x1e93f #\x1e940 #\x1e941 #\x1e942 #\x1e943))
