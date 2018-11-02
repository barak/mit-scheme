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

static const uint8_t chacha8_core_selftest_vector[64] = {
	0x3e,0x00,0xef,0x2f,0x89,0x5f,0x40,0xd6,
	0x7f,0x5b,0xb8,0xe8,0x1f,0x09,0xa5,0xa1,
	0x2c,0x84,0x0e,0xc3,0xce,0x9a,0x7f,0x3b,
	0x18,0x1b,0xe1,0x88,0xef,0x71,0x1a,0x1e,
	0x98,0x4c,0xe1,0x72,0xb9,0x21,0x6f,0x41,
	0x9f,0x44,0x53,0x67,0x45,0x6d,0x56,0x19,
	0x31,0x4a,0x42,0xa3,0xda,0x86,0xb0,0x01,
	0x38,0x7b,0xfd,0xb8,0x0e,0x0c,0xfe,0x42,
};

#define chacha_core_ROUNDS		8

#define	chacha_core			chacha8_core
#define	chacha_core_selftest		chacha8_core_selftest
#define	chacha_core_selftest_vector	chacha8_core_selftest_vector

#include "chacha.i"
