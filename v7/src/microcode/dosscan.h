/* -*-C-*-

$Id: dosscan.h,v 1.4 1992/09/06 16:24:26 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* Scan code conversion table for DOS */

#ifndef SCM_DOSSCAN_INCLUDE
#define SCM_DOSSCAN_INCLUDE

#define METAFY(c)	((unsigned char) (((unsigned char) (c)) + 128))
/* This had better get uppercase characters */ 
#define CONTROLIFY(c)	((unsigned char) (((unsigned char) (c)) - 64))

#define NO_CONVERSION	((unsigned char *) ((void *) 0))

unsigned char CTRL_AT[] = { '\0' };
unsigned char HARD_ATTN[] = "HA";
unsigned char SOFT_ATTN[] = "SA";
unsigned char META_a[] = {METAFY('a'), '\0'};
unsigned char META_b[] = {METAFY('b'), '\0'};
unsigned char META_c[] = {METAFY('c'), '\0'};
unsigned char META_d[] = {METAFY('d'), '\0'};
unsigned char META_e[] = {METAFY('e'), '\0'};
unsigned char META_f[] = {METAFY('f'), '\0'};
unsigned char META_g[] = {METAFY('g'), '\0'};
unsigned char META_h[] = {METAFY('h'), '\0'};
unsigned char META_i[] = {METAFY('i'), '\0'};
unsigned char META_j[] = {METAFY('j'), '\0'};
unsigned char META_k[] = {METAFY('k'), '\0'};
unsigned char META_l[] = {METAFY('l'), '\0'};
unsigned char META_m[] = {METAFY('m'), '\0'};
unsigned char META_n[] = {METAFY('n'), '\0'};
unsigned char META_o[] = {METAFY('o'), '\0'};
unsigned char META_p[] = {METAFY('p'), '\0'};
unsigned char META_q[] = {METAFY('q'), '\0'};
unsigned char META_r[] = {METAFY('r'), '\0'};
unsigned char META_s[] = {METAFY('s'), '\0'};
unsigned char META_t[] = {METAFY('t'), '\0'};
unsigned char META_u[] = {METAFY('u'), '\0'};
unsigned char META_v[] = {METAFY('v'), '\0'};
unsigned char META_w[] = {METAFY('w'), '\0'};
unsigned char META_x[] = {METAFY('x'), '\0'};
unsigned char META_y[] = {METAFY('y'), '\0'};
unsigned char META_z[] = {METAFY('z'), '\0'};
unsigned char META_1[] = {METAFY('1'), '\0'};
unsigned char META_2[] = {METAFY('2'), '\0'};
unsigned char META_3[] = {METAFY('3'), '\0'};
unsigned char META_4[] = {METAFY('4'), '\0'};
unsigned char META_5[] = {METAFY('5'), '\0'};
unsigned char META_6[] = {METAFY('6'), '\0'};
unsigned char META_7[] = {METAFY('7'), '\0'};
unsigned char META_8[] = {METAFY('8'), '\0'};
unsigned char META_9[] = {METAFY('9'), '\0'};
unsigned char META_0[] = {METAFY('0'), '\0'};
unsigned char META_DASH[] = {METAFY('-'), '\0'};
unsigned char META_EQUAL[] = {METAFY('='), '\0'};
unsigned char META_RET[] = {METAFY('\r'), '\0'};
unsigned char META_TAB[] = {METAFY('\t'), '\0'};
unsigned char META_LBROK[] = {METAFY('['), '\0'};
unsigned char META_RBROK[] = {METAFY(']'), '\0'};
unsigned char META_BACK[] = {METAFY('\\'), '\0'};
unsigned char META_SEMI[] = {METAFY(';'), '\0'};
unsigned char META_RQUOTE[] = {METAFY('\''), '\0'};
unsigned char META_COMMA[] = {METAFY(','), '\0'};
unsigned char META_DOT[] = {METAFY('.'), '\0'};
unsigned char META_SLASH[] = {METAFY('/'), '\0'};
unsigned char META_LQUOTE[] = {METAFY('`'), '\0'};
unsigned char META_PLUS[] = {METAFY('+'), '\0'};

#define DEFAULT_SCANCODE_CONVERSIONS				\
{								\
/* 0 */		NO_CONVERSION,					\
/* 1 */		NO_CONVERSION,					\
/* 2 */		NO_CONVERSION,					\
/* 3 */		CTRL_AT,					\
/* 4 */		NO_CONVERSION,					\
/* 5 */		NO_CONVERSION,					\
/* 6 */		NO_CONVERSION,					\
/* 7 */		NO_CONVERSION,					\
/* 8 */		NO_CONVERSION,					\
/* 9 */		NO_CONVERSION,					\
/* 10 */	NO_CONVERSION,					\
/* 11 */	NO_CONVERSION,					\
/* 12 */	NO_CONVERSION,					\
/* 13 */	NO_CONVERSION,					\
/* 14 */	NO_CONVERSION,					\
/* 15 */	NO_CONVERSION,					\
/* 16 */	META_q,						\
/* 17 */	META_w,						\
/* 18 */	META_e,						\
/* 19 */	META_r,						\
/* 20 */	META_t,						\
/* 21 */	META_y,						\
/* 22 */	META_u,						\
/* 23 */	META_i,						\
/* 24 */	META_o,						\
/* 25 */	META_p,						\
/* 26 */	META_LBROK,					\
/* 27 */	META_RBROK,					\
/* 28 */	META_RET,					\
/* 29 */	NO_CONVERSION,					\
/* 30 */	META_a,						\
/* 31 */	META_s,						\
/* 32 */	META_d,						\
/* 33 */	META_f,						\
/* 34 */	META_g,						\
/* 35 */	META_h,						\
/* 36 */	META_j,						\
/* 37 */	META_k,						\
/* 38 */	META_l,						\
/* 39 */	META_SEMI,					\
/* 40 */	META_RQUOTE,					\
/* 41 */	META_LQUOTE,					\
/* 42 */	NO_CONVERSION,					\
/* 43 */	META_BACK,					\
/* 44 */	META_z,						\
/* 45 */	META_x,						\
/* 46 */	META_c,						\
/* 47 */	META_v,						\
/* 48 */	META_b,						\
/* 49 */	META_n,						\
/* 50 */	META_m,						\
/* 51 */	META_COMMA,					\
/* 52 */	META_DOT,					\
/* 53 */	META_SLASH,					\
/* 54 */	NO_CONVERSION,					\
/* 55 */	META_PLUS,					\
/* 56 */	NO_CONVERSION,					\
/* 57 */	NO_CONVERSION,					\
/* 58 */	NO_CONVERSION,					\
/* 59 */	"(proceed)\r",					\
/* 60 */	NO_CONVERSION,					\
/* 61 */	NO_CONVERSION,					\
/* 62 */	NO_CONVERSION,					\
/* 63 */	NO_CONVERSION,					\
/* 64 */	NO_CONVERSION,					\
/* 65 */	NO_CONVERSION,					\
/* 66 */	NO_CONVERSION,					\
/* 67 */	NO_CONVERSION,					\
/* 68 */	NO_CONVERSION,					\
/* 69 */	NO_CONVERSION,					\
/* 70 */	NO_CONVERSION,					\
/* 71 */	NO_CONVERSION,					\
/* 72 */	NO_CONVERSION,					\
/* 73 */	NO_CONVERSION,					\
/* 74 */	META_SLASH,					\
/* 75 */	NO_CONVERSION,					\
/* 76 */	NO_CONVERSION,					\
/* 77 */	NO_CONVERSION,					\
/* 78 */	NO_CONVERSION,					\
/* 79 */	NO_CONVERSION,					\
/* 80 */	NO_CONVERSION,					\
/* 81 */	NO_CONVERSION,					\
/* 82 */	NO_CONVERSION,					\
/* 83 */	NO_CONVERSION,					\
/* 84 */	NO_CONVERSION,					\
/* 85 */	NO_CONVERSION,					\
/* 86 */	NO_CONVERSION,					\
/* 87 */	NO_CONVERSION,					\
/* 88 */	NO_CONVERSION,					\
/* 89 */	NO_CONVERSION,					\
/* 90 */	NO_CONVERSION,					\
/* 91 */	NO_CONVERSION,					\
/* 92 */	NO_CONVERSION,					\
/* 93 */	NO_CONVERSION,					\
/* 94 */	NO_CONVERSION,					\
/* 95 */	NO_CONVERSION,					\
/* 96 */	NO_CONVERSION,					\
/* 97 */	NO_CONVERSION,					\
/* 98 */	NO_CONVERSION,					\
/* 99 */	NO_CONVERSION,					\
/* 100 */	NO_CONVERSION,					\
/* 101 */	NO_CONVERSION,					\
/* 102 */	NO_CONVERSION,					\
/* 103 */	HARD_ATTN,		/* Ctrl-F10 */		\
/* 104 */	NO_CONVERSION,					\
/* 105 */	NO_CONVERSION,					\
/* 106 */	NO_CONVERSION,					\
/* 107 */	NO_CONVERSION,					\
/* 108 */	NO_CONVERSION,					\
/* 109 */	NO_CONVERSION,					\
/* 110 */	NO_CONVERSION,					\
/* 111 */	NO_CONVERSION,					\
/* 112 */	NO_CONVERSION,					\
/* 113 */	SOFT_ATTN,		/* Alt-F10 */		\
/* 114 */	NO_CONVERSION,					\
/* 115 */	NO_CONVERSION,					\
/* 116 */	NO_CONVERSION,					\
/* 117 */	NO_CONVERSION,					\
/* 118 */	NO_CONVERSION,					\
/* 119 */	NO_CONVERSION,					\
/* 120 */	META_1,						\
/* 121 */	META_2,						\
/* 122 */	META_3,						\
/* 123 */	META_4,						\
/* 124 */	META_5,						\
/* 125 */	META_6,						\
/* 126 */	META_7,						\
/* 127 */	META_8,						\
/* 128 */	META_9,						\
/* 129 */	META_0,						\
/* 130 */	META_DASH,					\
/* 131 */	META_EQUAL,					\
/* 132 */	NO_CONVERSION,					\
/* 133 */	NO_CONVERSION,					\
/* 134 */	NO_CONVERSION,					\
/* 135 */	NO_CONVERSION,					\
/* 136 */	NO_CONVERSION,					\
/* 137 */	NO_CONVERSION,					\
/* 138 */	NO_CONVERSION,					\
/* 139 */	NO_CONVERSION,					\
/* 140 */	NO_CONVERSION,					\
/* 141 */	NO_CONVERSION,					\
/* 142 */	NO_CONVERSION,					\
/* 143 */	NO_CONVERSION,					\
/* 144 */	NO_CONVERSION,					\
/* 145 */	NO_CONVERSION,					\
/* 146 */	NO_CONVERSION,					\
/* 147 */	NO_CONVERSION,					\
/* 148 */	NO_CONVERSION,					\
/* 149 */	NO_CONVERSION,					\
/* 150 */	NO_CONVERSION,					\
/* 151 */	NO_CONVERSION,					\
/* 152 */	NO_CONVERSION,					\
/* 153 */	NO_CONVERSION,					\
/* 154 */	NO_CONVERSION,					\
/* 155 */	NO_CONVERSION,					\
/* 156 */	NO_CONVERSION,					\
/* 157 */	NO_CONVERSION,					\
/* 158 */	NO_CONVERSION,					\
/* 159 */	NO_CONVERSION,					\
/* 160 */	NO_CONVERSION,					\
/* 161 */	NO_CONVERSION,					\
/* 162 */	NO_CONVERSION,					\
/* 163 */	NO_CONVERSION,					\
/* 164 */	META_DASH,					\
/* 165 */	META_TAB,					\
/* 166 */	META_RET,					\
/* 167 */	NO_CONVERSION,					\
/* 168 */	NO_CONVERSION,					\
/* 169 */	NO_CONVERSION,					\
/* 170 */	NO_CONVERSION,					\
/* 171 */	NO_CONVERSION,					\
/* 172 */	NO_CONVERSION,					\
/* 173 */	NO_CONVERSION,					\
/* 174 */	NO_CONVERSION,					\
/* 175 */	NO_CONVERSION,					\
/* 176 */	NO_CONVERSION,					\
/* 177 */	NO_CONVERSION,					\
/* 178 */	NO_CONVERSION,					\
/* 179 */	NO_CONVERSION,					\
/* 180 */	NO_CONVERSION,					\
/* 181 */	NO_CONVERSION,					\
/* 182 */	NO_CONVERSION,					\
/* 183 */	NO_CONVERSION,					\
/* 184 */	NO_CONVERSION,					\
/* 185 */	NO_CONVERSION,					\
/* 186 */	NO_CONVERSION,					\
/* 187 */	NO_CONVERSION,					\
/* 188 */	NO_CONVERSION,					\
/* 189 */	NO_CONVERSION,					\
/* 190 */	NO_CONVERSION,					\
/* 191 */	NO_CONVERSION,					\
/* 192 */	NO_CONVERSION,					\
/* 193 */	NO_CONVERSION,					\
/* 194 */	NO_CONVERSION,					\
/* 195 */	NO_CONVERSION,					\
/* 196 */	NO_CONVERSION,					\
/* 197 */	NO_CONVERSION,					\
/* 198 */	NO_CONVERSION,					\
/* 199 */	NO_CONVERSION,					\
/* 200 */	NO_CONVERSION,					\
/* 201 */	NO_CONVERSION,					\
/* 202 */	NO_CONVERSION,					\
/* 203 */	NO_CONVERSION,					\
/* 204 */	NO_CONVERSION,					\
/* 205 */	NO_CONVERSION,					\
/* 206 */	NO_CONVERSION,					\
/* 207 */	NO_CONVERSION,					\
/* 208 */	NO_CONVERSION,					\
/* 209 */	NO_CONVERSION,					\
/* 210 */	NO_CONVERSION,					\
/* 211 */	NO_CONVERSION,					\
/* 212 */	NO_CONVERSION,					\
/* 213 */	NO_CONVERSION,					\
/* 214 */	NO_CONVERSION,					\
/* 215 */	NO_CONVERSION,					\
/* 216 */	NO_CONVERSION,					\
/* 217 */	NO_CONVERSION,					\
/* 218 */	NO_CONVERSION,					\
/* 219 */	NO_CONVERSION,					\
/* 220 */	NO_CONVERSION,					\
/* 221 */	NO_CONVERSION,					\
/* 222 */	NO_CONVERSION,					\
/* 223 */	NO_CONVERSION,					\
/* 224 */	NO_CONVERSION,					\
/* 225 */	NO_CONVERSION,					\
/* 226 */	NO_CONVERSION,					\
/* 227 */	NO_CONVERSION,					\
/* 228 */	NO_CONVERSION,					\
/* 229 */	NO_CONVERSION,					\
/* 230 */	NO_CONVERSION,					\
/* 231 */	NO_CONVERSION,					\
/* 232 */	NO_CONVERSION,					\
/* 233 */	NO_CONVERSION,					\
/* 234 */	NO_CONVERSION,					\
/* 235 */	NO_CONVERSION,					\
/* 236 */	NO_CONVERSION,					\
/* 237 */	NO_CONVERSION,					\
/* 238 */	NO_CONVERSION,					\
/* 239 */	NO_CONVERSION,					\
/* 240 */	NO_CONVERSION,					\
/* 241 */	NO_CONVERSION,					\
/* 242 */	NO_CONVERSION,					\
/* 243 */	NO_CONVERSION,					\
/* 244 */	NO_CONVERSION,					\
/* 245 */	NO_CONVERSION,					\
/* 246 */	NO_CONVERSION,					\
/* 247 */	NO_CONVERSION,					\
/* 248 */	NO_CONVERSION,					\
/* 249 */	NO_CONVERSION,					\
/* 250 */	NO_CONVERSION,					\
/* 251 */	NO_CONVERSION,					\
/* 252 */	NO_CONVERSION,					\
/* 253 */	NO_CONVERSION,					\
/* 254 */	NO_CONVERSION,					\
/* 255 */	NO_CONVERSION					\
}

#define KEYBOARD_SCANCODE_TABLE_SIZE	(256)
#endif
