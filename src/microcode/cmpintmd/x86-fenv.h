/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

*/

/* C99 <fenv.h> emulation for x86 (shared between i386 and amd64) */

#define HAVE_FENV_T
#define HAVE_FEXCEPT_T

typedef int fexcept_t;

typedef struct
{
  int sse_mxcsr;
} sse_fenv_t;

/* FIXME: This structure needs to be packed.  */

struct x87_environment
{
  unsigned short x87_control_word;
  unsigned short x87_unused1;
  unsigned short x87_status_word;
  unsigned short x87_unused2;
  unsigned short x87_tag_word;
  unsigned short x87_unused3;
  unsigned int x87_instruction_offset;
  unsigned short x87_instruction_cs_selector;
  unsigned short x87_instruction_opcode : 11;
  unsigned short x87_unused4 : 5;
  unsigned int x87_data_offset;
  unsigned short x87_data_ds_selector;
  unsigned short x87_unused5;
};

typedef union
{
  struct x87_environment environment;
  unsigned char environment_bytes [sizeof (struct x87_environment)];
} x87_fenv_t;

typedef struct
{
  sse_fenv_t fenv_sse;
  x87_fenv_t fenv_x87;
} fenv_t;

#define FE_TONEAREST 0
#define FE_DOWNWARD 1
#define FE_UPWARD 2
#define FE_TOWARDZERO 3

#define HAVE_FEGETROUND
#define HAVE_FESETROUND

extern int fegetround (void);
extern int fesetround (int);

#define FE_INVALID (1 << 0)
#define FE_DENORMAL (1 << 1)	/* Non-standard */
#define FE_DIVBYZERO (1 << 2)
#define FE_OVERFLOW (1 << 3)
#define FE_UNDERFLOW (1 << 4)
#define FE_INEXACT (1 << 5)

#define FE_ALL_EXCEPT							\
  (FE_DENORMAL|FE_DIVBYZERO|FE_INEXACT|FE_INVALID|FE_OVERFLOW|FE_UNDERFLOW)

#if FE_ALL_EXCEPT != 0x3f
#  error Floating-point exception set is wrong.
#endif

#define HAVE_FECLEAREXCEPT
#define HAVE_FEGETEXCEPTFLAG
#define HAVE_FERAISEEXCEPT
#define HAVE_FESETEXCEPTFLAG
#define HAVE_FETESTEXCEPT

extern int fetestexcept (int);
extern int feclearexcept (int);
extern int feraiseexcept (int);
extern int fegetexceptflag (fexcept_t *, int);
extern int fesetexceptflag (const fexcept_t *, int);

/* The next three are glibc extensions.  */

#define HAVE_FEDISABLEEXCEPT
#define HAVE_FEENABLEEXCEPT
#define HAVE_FEGETEXCEPT

extern int fegetexcept (void);
extern int feenableexcept (int);
extern int fedisableexcept (int);

#define HAVE_FEGETENV
#define HAVE_FESETENV
#define HAVE_FEHOLDEXCEPT
#define HAVE_FEUPDATEENV

extern int fegetenv (fenv_t *);
extern int fesetenv (const fenv_t *);
extern int feholdexcept (fenv_t *);
extern int feupdateenv (const fenv_t *);
