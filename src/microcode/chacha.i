/*-
 * Copyright (c) 2014 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Taylor R. Campbell.
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
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#define	_POSIX_C_SOURCE	200809L

#include "chacha.h"

#include <stdint.h>

static inline uint32_t
chacha_le32dec(const void *buf)
{
	const uint8_t *p = buf;
	uint32_t v = 0;

	v |= (uint32_t)*p++ << 0;
	v |= (uint32_t)*p++ << 8;
	v |= (uint32_t)*p++ << 16;
	v |= (uint32_t)*p++ << 24;

	return v;
}

static inline void
chacha_le32enc(void *buf, uint32_t v)
{
	uint8_t *p = buf;

	*p++ = v & 0xff; v >>= 8;
	*p++ = v & 0xff; v >>= 8;
	*p++ = v & 0xff; v >>= 8;
	*p++ = v & 0xff; v >>= 8;
}

static uint32_t
rol32(uint32_t u, unsigned c)
{

	return (u << c) | (u >> (32 - c));
}

#define	QUARTERROUND(a, b, c, d) do {					      \
	(a) += (b); (d) ^= (a); (d) = rol32((d), 16);			      \
	(c) += (d); (b) ^= (c); (b) = rol32((b), 12);			      \
	(a) += (b); (d) ^= (a); (d) = rol32((d),  8);			      \
	(c) += (d); (b) ^= (c); (b) = rol32((b),  7);			      \
} while (/*CONSTCOND*/0)

static const uint8_t chacha_core_constant32[16] = "expand 32-byte k";

void
chacha_core(uint8_t *out, const uint8_t *in, const uint8_t *k,
    const uint8_t *c)
{
	uint32_t x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15;
	uint32_t j0,j1,j2,j3,j4,j5,j6,j7,j8,j9,j10,j11,j12,j13,j14,j15;
	int i;

	j0 = x0 = chacha_le32dec(c + 0);
	j1 = x1 = chacha_le32dec(c + 4);
	j2 = x2 = chacha_le32dec(c + 8);
	j3 = x3 = chacha_le32dec(c + 12);
	j4 = x4 = chacha_le32dec(k + 0);
	j5 = x5 = chacha_le32dec(k + 4);
	j6 = x6 = chacha_le32dec(k + 8);
	j7 = x7 = chacha_le32dec(k + 12);
	j8 = x8 = chacha_le32dec(k + 16);
	j9 = x9 = chacha_le32dec(k + 20);
	j10 = x10 = chacha_le32dec(k + 24);
	j11 = x11 = chacha_le32dec(k + 28);
	j12 = x12 = chacha_le32dec(in + 0);
	j13 = x13 = chacha_le32dec(in + 4);
	j14 = x14 = chacha_le32dec(in + 8);
	j15 = x15 = chacha_le32dec(in + 12);

	for (i = chacha_core_ROUNDS; i > 0; i -= 2) {
		QUARTERROUND( x0, x4, x8,x12);
		QUARTERROUND( x1, x5, x9,x13);
		QUARTERROUND( x2, x6,x10,x14);
		QUARTERROUND( x3, x7,x11,x15);
		QUARTERROUND( x0, x5,x10,x15);
		QUARTERROUND( x1, x6,x11,x12);
		QUARTERROUND( x2, x7, x8,x13);
		QUARTERROUND( x3, x4, x9,x14);
	}

	chacha_le32enc(out + 0, x0 + j0);
	chacha_le32enc(out + 4, x1 + j1);
	chacha_le32enc(out + 8, x2 + j2);
	chacha_le32enc(out + 12, x3 + j3);
	chacha_le32enc(out + 16, x4 + j4);
	chacha_le32enc(out + 20, x5 + j5);
	chacha_le32enc(out + 24, x6 + j6);
	chacha_le32enc(out + 28, x7 + j7);
	chacha_le32enc(out + 32, x8 + j8);
	chacha_le32enc(out + 36, x9 + j9);
	chacha_le32enc(out + 40, x10 + j10);
	chacha_le32enc(out + 44, x11 + j11);
	chacha_le32enc(out + 48, x12 + j12);
	chacha_le32enc(out + 52, x13 + j13);
	chacha_le32enc(out + 56, x14 + j14);
	chacha_le32enc(out + 60, x15 + j15);
}

int
chacha_core_selftest(void)
{
	const uint8_t nonce[chacha_core_INPUTBYTES] = {0};
	const uint8_t key[chacha_core_KEYBYTES] = {0};
	uint8_t block[64];
	unsigned i;

	chacha_core(block, nonce, key, chacha_core_constant32);
	for (i = 0; i < 64; i++) {
		if (block[i] != chacha_core_selftest_vector[i])
			return -1;
	}

	return 0;
}
