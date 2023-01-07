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

#ifndef	BLOWFISH_H
#define	BLOWFISH_H

#include <stddef.h>
#include <stdint.h>

#define	BLOWFISH_MIN_KEYLEN	4
#define	BLOWFISH_MAX_KEYLEN	56
#define	BLOWFISH_BLOCKLEN	8

struct blowfish {
	uint32_t	P[18];
	uint32_t	S[4][256];
};

void	blowfish_init(struct blowfish *, const void *, size_t);
void	blowfish_clear(struct blowfish *);

void	blowfish_encrypt(const struct blowfish *,
	    const uint8_t[BLOWFISH_BLOCKLEN],
	    uint8_t[static BLOWFISH_BLOCKLEN]);
void	blowfish_decrypt(const struct blowfish *,
	    const uint8_t[static BLOWFISH_BLOCKLEN],
	    uint8_t[static BLOWFISH_BLOCKLEN]);

void	blowfish_encrypt_cbc(const struct blowfish *,
	    uint8_t[static BLOWFISH_BLOCKLEN], const uint8_t *, uint8_t *,
	    size_t);
void	blowfish_decrypt_cbc(const struct blowfish *,
	    uint8_t[static BLOWFISH_BLOCKLEN], const uint8_t *, uint8_t *,
	    size_t);
unsigned
	blowfish_encrypt_cfb64(const struct blowfish *,
	    uint8_t[static BLOWFISH_BLOCKLEN], unsigned, const uint8_t *,
	    uint8_t *, size_t);
unsigned
	blowfish_decrypt_cfb64(const struct blowfish *,
	    uint8_t[static BLOWFISH_BLOCKLEN], unsigned, const uint8_t *,
	    uint8_t *, size_t);
unsigned
	blowfish_encrypt_ofb64(const struct blowfish *,
	    uint8_t[static BLOWFISH_BLOCKLEN], unsigned, const uint8_t *,
	    uint8_t *, size_t);
unsigned
	blowfish_decrypt_ofb64(const struct blowfish *,
	    uint8_t[static BLOWFISH_BLOCKLEN], unsigned, const uint8_t *,
	    uint8_t *, size_t);

int	blowfish_selftest(void);

extern void *(*volatile blowfish_explicit_memset_impl)(void *, int, size_t);

#endif	/* BLOWFISH_H */
