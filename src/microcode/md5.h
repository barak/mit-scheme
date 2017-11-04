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

#ifndef	MD5_H
#define	MD5_H

#include <stdint.h>

#define	MD5_HASHLEN	16
#define	MD5_BLOCKLEN	64

struct md5 {
	uint32_t	state[4];	/* MD5 state, {A,B,C,D} */
	uint8_t		block[16*4];	/* partial message block */
	uint64_t	b;		/* number of bytes hashed */
	uint_fast8_t	i;		/* index of next byte in block */
};

void	md5_init(struct md5 *);
void	md5_update(struct md5 *, const void *, size_t);
void	md5_final(struct md5 *, uint8_t[16]);
int	md5_selftest(void);

extern void *(*volatile md5_explicit_memset_impl)(void *, int, size_t);

#endif	/* MD5_H */
