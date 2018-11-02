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

static const uint8_t chacha20_core_selftest_vector[64] = {
	0x76,0xb8,0xe0,0xad,0xa0,0xf1,0x3d,0x90,
	0x40,0x5d,0x6a,0xe5,0x53,0x86,0xbd,0x28,
	0xbd,0xd2,0x19,0xb8,0xa0,0x8d,0xed,0x1a,
	0xa8,0x36,0xef,0xcc,0x8b,0x77,0x0d,0xc7,
	0xda,0x41,0x59,0x7c,0x51,0x57,0x48,0x8d,
	0x77,0x24,0xe0,0x3f,0xb8,0xd8,0x4a,0x37,
	0x6a,0x43,0xb8,0xf4,0x15,0x18,0xa1,0x1c,
	0xc3,0x87,0xb6,0x69,0xb2,0xee,0x65,0x86,
};

#define chacha_core_ROUNDS		20

#define	chacha_core			chacha20_core
#define	chacha_core_selftest		chacha20_core_selftest
#define	chacha_core_selftest_vector	chacha20_core_selftest_vector

#include "chacha.i"
