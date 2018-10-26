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

#include <stdint.h>

static const uint8_t chacha12_core_selftest_vector[64] = {
	0x9b,0xf4,0x9a,0x6a,0x07,0x55,0xf9,0x53,
	0x81,0x1f,0xce,0x12,0x5f,0x26,0x83,0xd5,
	0x04,0x29,0xc3,0xbb,0x49,0xe0,0x74,0x14,
	0x7e,0x00,0x89,0xa5,0x2e,0xae,0x15,0x5f,
	0x05,0x64,0xf8,0x79,0xd2,0x7a,0xe3,0xc0,
	0x2c,0xe8,0x28,0x34,0xac,0xfa,0x8c,0x79,
	0x3a,0x62,0x9f,0x2c,0xa0,0xde,0x69,0x19,
	0x61,0x0b,0xe8,0x2f,0x41,0x13,0x26,0xbe,
};

#define chacha_core_ROUNDS		12

#define	chacha_core			chacha12_core
#define	chacha_core_selftest		chacha12_core_selftest
#define	chacha_core_selftest_vector	chacha12_core_selftest_vector

#include "chacha.i"
