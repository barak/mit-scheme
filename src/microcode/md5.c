/*-
 * Copyright (c) 2017 Taylor R. Campbell
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#define	_POSIX_C_SOURCE	200809L

#include <assert.h>
#include <stdint.h>
#include <string.h>

#include "md5.h"

#define	secret	/* can't use in variable-time operations, should zero */

void *(*volatile md5_explicit_memset_impl)(void *, int, size_t) = &memset;

static void *
explicit_memset(void *p, int c, size_t n)
{

	return (*md5_explicit_memset_impl)(p, c, n);
}

static inline uint32_t
le32dec(const void *buf)
{
	const uint8_t *p = buf;
	uint32_t v = 0;

	v |= (uint32_t)p[0];
	v |= (uint32_t)p[1] << 8;
	v |= (uint32_t)p[2] << 16;
	v |= (uint32_t)p[3] << 24;

	return v;
}

static inline void
le64enc(void *buf, uint64_t v)
{
	uint8_t *p = buf;

	p[0] = (v >>  0) & 0xff;
	p[1] = (v >>  8) & 0xff;
	p[2] = (v >> 16) & 0xff;
	p[3] = (v >> 24) & 0xff;
	p[4] = (v >> 32) & 0xff;
	p[5] = (v >> 40) & 0xff;
	p[6] = (v >> 48) & 0xff;
	p[7] = (v >> 56) & 0xff;
}

static inline uint32_t
rol32(secret uint32_t x, unsigned c)
{

	return ((x << c) | (x >> (32 - c)));
}

#define	F(X,Y,Z)	(((X) & (Y)) | (~(X) & (Z)))
#define	G(X,Y,Z)	(((X) & (Z)) | ((Y) & ~(Z)))
#define	H(X,Y,Z)	((X) ^ (Y) ^ (Z))
#define	I(X,Y,Z)	((Y) ^ ((X) | ~(Z)))
#define	OP(f, a,b,c,d, k,s,v) \
  (a) = (b) + rol32((a) + f(b,c,d) + le32dec(&M[4*(k)]) + UINT32_C(v), (s))

static void
md5_compress(secret uint32_t S[4], const secret uint8_t M[16*4])
{
	uint32_t A = S[0];
	uint32_t B = S[1];
	uint32_t C = S[2];
	uint32_t D = S[3];

	/* Magic constant i is integer part of 2^32 abs(sin(i)).  */

	/* Round 1 */
	OP(F, A,B,C,D,  0,  7, 0xd76aa478); /* 1 */
	OP(F, D,A,B,C,  1, 12, 0xe8c7b756); /* 2 */
	OP(F, C,D,A,B,  2, 17, 0x242070db); /* 3 */
	OP(F, B,C,D,A,  3, 22, 0xc1bdceee); /* 4 */

	OP(F, A,B,C,D,  4,  7, 0xf57c0faf); /* 5 */
	OP(F, D,A,B,C,  5, 12, 0x4787c62a); /* 6 */
	OP(F, C,D,A,B,  6, 17, 0xa8304613); /* 7 */
	OP(F, B,C,D,A,  7, 22, 0xfd469501); /* 8 */

	OP(F, A,B,C,D,  8,  7, 0x698098d8); /* 9 */
	OP(F, D,A,B,C,  9, 12, 0x8b44f7af); /* 10 */
	OP(F, C,D,A,B, 10, 17, 0xffff5bb1); /* 11 */
	OP(F, B,C,D,A, 11, 22, 0x895cd7be); /* 12 */

	OP(F, A,B,C,D, 12,  7, 0x6b901122); /* 13 */
	OP(F, D,A,B,C, 13, 12, 0xfd987193); /* 14 */
	OP(F, C,D,A,B, 14, 17, 0xa679438e); /* 15 */
	OP(F, B,C,D,A, 15, 22, 0x49b40821); /* 16 */

	/* Round 2 */
	OP(G, A,B,C,D,  1,  5, 0xf61e2562); /* 17 */
	OP(G, D,A,B,C,  6,  9, 0xc040b340); /* 18 */
	OP(G, C,D,A,B, 11, 14, 0x265e5a51); /* 19 */
	OP(G, B,C,D,A,  0, 20, 0xe9b6c7aa); /* 20 */

	OP(G, A,B,C,D,  5,  5, 0xd62f105d); /* 21 */
	OP(G, D,A,B,C, 10,  9, 0x02441453); /* 22 */
	OP(G, C,D,A,B, 15, 14, 0xd8a1e681); /* 23 */
	OP(G, B,C,D,A,  4, 20, 0xe7d3fbc8); /* 24 */

	OP(G, A,B,C,D,  9,  5, 0x21e1cde6); /* 25 */
	OP(G, D,A,B,C, 14,  9, 0xc33707d6); /* 26 */
	OP(G, C,D,A,B,  3, 14, 0xf4d50d87); /* 27 */
	OP(G, B,C,D,A,  8, 20, 0x455a14ed); /* 28 */

	OP(G, A,B,C,D, 13,  5, 0xa9e3e905); /* 29 */
	OP(G, D,A,B,C,  2,  9, 0xfcefa3f8); /* 30 */
	OP(G, C,D,A,B,  7, 14, 0x676f02d9); /* 31 */
	OP(G, B,C,D,A, 12, 20, 0x8d2a4c8a); /* 32 */

	/* Round 3 */
	OP(H, A,B,C,D,  5,  4, 0xfffa3942); /* 33 */
	OP(H, D,A,B,C,  8, 11, 0x8771f681); /* 34 */
	OP(H, C,D,A,B, 11, 16, 0x6d9d6122); /* 35 */
	OP(H, B,C,D,A, 14, 23, 0xfde5380c); /* 36 */

	OP(H, A,B,C,D,  1,  4, 0xa4beea44); /* 37 */
	OP(H, D,A,B,C,  4, 11, 0x4bdecfa9); /* 38 */
	OP(H, C,D,A,B,  7, 16, 0xf6bb4b60); /* 39 */
	OP(H, B,C,D,A, 10, 23, 0xbebfbc70); /* 40 */

	OP(H, A,B,C,D, 13,  4, 0x289b7ec6); /* 41 */
	OP(H, D,A,B,C,  0, 11, 0xeaa127fa); /* 42 */
	OP(H, C,D,A,B,  3, 16, 0xd4ef3085); /* 43 */
	OP(H, B,C,D,A,  6, 23, 0x04881d05); /* 44 */

	OP(H, A,B,C,D,  9,  4, 0xd9d4d039); /* 45 */
	OP(H, D,A,B,C, 12, 11, 0xe6db99e5); /* 46 */
	OP(H, C,D,A,B, 15, 16, 0x1fa27cf8); /* 47 */
	OP(H, B,C,D,A,  2, 23, 0xc4ac5665); /* 48 */

	/* Round 4 */
	OP(I, A,B,C,D,  0,  6, 0xf4292244); /* 49 */
	OP(I, D,A,B,C,  7, 10, 0x432aff97); /* 50 */
	OP(I, C,D,A,B, 14, 15, 0xab9423a7); /* 51 */
	OP(I, B,C,D,A,  5, 21, 0xfc93a039); /* 52 */

	OP(I, A,B,C,D, 12,  6, 0x655b59c3); /* 53 */
	OP(I, D,A,B,C,  3, 10, 0x8f0ccc92); /* 54 */
	OP(I, C,D,A,B, 10, 15, 0xffeff47d); /* 55 */
	OP(I, B,C,D,A,  1, 21, 0x85845dd1); /* 56 */

	OP(I, A,B,C,D,  8,  6, 0x6fa87e4f); /* 57 */
	OP(I, D,A,B,C, 15, 10, 0xfe2ce6e0); /* 58 */
	OP(I, C,D,A,B,  6, 15, 0xa3014314); /* 59 */
	OP(I, B,C,D,A, 13, 21, 0x4e0811a1); /* 60 */

	OP(I, A,B,C,D,  4,  6, 0xf7537e82); /* 61 */
	OP(I, D,A,B,C, 11, 10, 0xbd3af235); /* 62 */
	OP(I, C,D,A,B,  2, 15, 0x2ad7d2bb); /* 63 */
	OP(I, B,C,D,A,  9, 21, 0xeb86d391); /* 64 */

	S[0] += A;
	S[1] += B;
	S[2] += C;
	S[3] += D;
}

static const uint32_t md5_iv[4] = {
	UINT32_C(0x67452301),
	UINT32_C(0xefcdab89),
	UINT32_C(0x98badcfe),
	UINT32_C(0x10325476),
};

void
md5_init(struct md5 *M)
{

	M->state[0] = md5_iv[0];
	M->state[1] = md5_iv[1];
	M->state[2] = md5_iv[2];
	M->state[3] = md5_iv[3];
	M->b = 0;
	M->i = 0;
}

void
md5_update(struct md5 *M, const void *buf, size_t len)
{
	const uint8_t *p = buf;
	size_t n = len;

	assert(M->i < sizeof(M->block));
	assert(M->i == M->b % 64);

	if (n < sizeof(M->block) - M->i) {
		/* Can only partially fill the buffer.  */
		(void)memcpy(&M->block[M->i], p, n);
		M->b += n;
		M->i += n;
		assert(M->i < sizeof(M->block));
		assert(M->i == M->b % 64);
		return;
	} else if (0 < M->i) {
		/* Can fill the partial buffer, compress, and go on.  */
		(void)memcpy(&M->block[M->i], p, sizeof(M->block) - M->i);
		M->b += sizeof(M->block) - M->i;
		md5_compress(M->state, M->block);
		p += sizeof(M->block) - M->i;
		n -= sizeof(M->block) - M->i;
	}

	/* At a block boundary.  Compress straight from the input.  */
	while (sizeof(M->block) <= n) {
		M->b += sizeof(M->block);
		md5_compress(M->state, p);
		p += sizeof(M->block);
		n -= sizeof(M->block);
	}

	/* Put whatever's left in the buffer.  */
	(void)memcpy(M->block, p, n);
	M->b += n;
	M->i = n;
	assert(M->i < sizeof(M->block));
	assert(M->i == M->b % 64);
}

void
md5_final(struct md5 *M, uint8_t h[16])
{

	assert(M->i < sizeof(M->block));
	assert(M->i == M->b % 64);

	/* Append 1 bit.  */
	M->block[M->i++] = 0x80;

	/* If there's less than 8 bytes left, zero and go to new block.  */
	if (sizeof(M->block) - 8 < M->i) {
		while (M->i < sizeof(M->block)) {
			M->block[M->i++] = 0;
		}
		md5_compress(M->state, M->block);
		M->i = 0;
	}

	/* Zero up to the last 64 bits.  */
	if (M->i < sizeof(M->block) - 8) {
		do {
			M->block[M->i++] = 0;
		} while (M->i < sizeof(M->block) - 8);
	}

	/* Append length in bits.  */
	assert(M->i == sizeof(M->block) - 8);
	le64enc(&M->block[sizeof(M->block) - 8], 8*M->b);

	/* Compress one last time.  */
	md5_compress(M->state, M->block);

	/* Reveal the complete state.  (Sorry, length-extension!)  */
	(void)memcpy(h, M->state, sizeof(M->state));

	/* Zero it all.  */
	(void)explicit_memset(M, 0, sizeof M);
}

int
md5_selftest(void)
{
	static const uint8_t expected[16] = {
		0xbf, 0xd5, 0x08, 0x30, 0xba, 0x3e, 0xbc, 0x1d,
		0xf2, 0x78, 0xc0, 0x26, 0x97, 0x79, 0xa0, 0x3e,
	};
	struct md5 m;
	uint8_t s[256], actual[16];
	unsigned i;

	for (i = 0; i < 256; i++)
		s[i] = i;

	md5_init(&m);
	for (i = 0; i < 256; i++) {
		struct md5 m0;
		uint8_t h[16];

		md5_init(&m0);
		md5_update(&m0, s, i/2);
		md5_update(&m0, s + i/2, i);
		md5_final(&m0, h);
		md5_update(&m, h, 16);
	}
	md5_final(&m, actual);
	if (memcmp(expected, actual, 16) != 0)
		return -1;

	return 0;
}
