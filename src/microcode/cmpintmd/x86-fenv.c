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

extern unsigned int sse_read_mxcsr (void);
extern void sse_write_mxcsr (unsigned int);
extern void x87_clear_exceptions (void);
extern void x87_trap_exceptions (void);
extern unsigned short x87_read_control_word (void);
extern void x87_write_control_word (unsigned short);
extern unsigned short x87_read_status_word (void);
/* extern void x87_write_status_word (unsigned short);	No f(n)stsw. */
extern void x87_read_environment (unsigned char *);
extern void x87_write_environment (const unsigned char *);

#ifndef x87_p
#  ifdef HAVE_X87
static const bool x87_p = true;
#  else
static const bool x87_p = false;
#  endif
#endif

#ifndef sse_p
#  ifdef HAVE_SSE
static const bool sse_p = true;
#  else
static const bool sse_p = false;
#  endif
#endif

int
fegetround (void)
{
  int sse_mode = (sse_p ? (3 & ((sse_read_mxcsr ()) >> 13)) : 0);
  int x87_mode = (x87_p ? (3 & ((x87_read_control_word ()) >> 10)) : 0);
  if (x87_p && sse_p)
    return ((sse_mode == x87_mode) ? sse_mode : (-1));
  else
    return (x87_p? x87_mode : sse_p? sse_mode : (-1));
}

int
fesetround (int mode)
{
  switch (mode)
    {
    case 0: case 1: case 2: case 3: break;
    default: return (1);
    }
  if (sse_p)
    sse_write_mxcsr ((mode << 13) | (0xffff9fff & (sse_read_mxcsr ())));
  if (x87_p)
    x87_write_control_word
      ((mode << 10) | (0xf3ff & (x87_read_control_word ())));
  return (! (sse_p || x87_p));
}

int
fetestexcept (int excepts)
{
  excepts &= FE_ALL_EXCEPT;
  return
    ((sse_p ? ((sse_read_mxcsr ()) & excepts) : 0)
     | (x87_p ? ((x87_read_status_word ()) & excepts) : 0));
}

int
feclearexcept (int excepts)
{
  if (excepts &~ FE_ALL_EXCEPT)
    return (-1);
  if (sse_p)
    sse_write_mxcsr ((sse_read_mxcsr ()) &~ excepts);
  if (x87_p)
    {
      if (excepts == FE_ALL_EXCEPT)
	/* This is supposed to be much faster than fetching and storing
	   the environment.  */
	x87_clear_exceptions ();
      else
	{
	  x87_fenv_t fenv;
	  x87_read_environment (fenv.environment_bytes);
	  (fenv.environment.x87_status_word) &=~ excepts;
	  x87_write_environment (fenv.environment_bytes);
	}
    }
  return (! (sse_p || x87_p));
}

int
feraiseexcept (int excepts)
{
  if (excepts &~ FE_ALL_EXCEPT)
    return (-1);
  if (sse_p)
    sse_write_mxcsr ((sse_read_mxcsr ()) | excepts);
  if (x87_p)
    {
      x87_fenv_t fenv;
      x87_read_environment (fenv.environment_bytes);
      (fenv.environment.x87_status_word) |= excepts;
      x87_write_environment (fenv.environment_bytes);
      x87_trap_exceptions ();
    }
  /* There seems to be no good way to request a trap in SSE.
     Fortunately, I don't think there are any systems with SSE but not
     x87.
  if (sse_p && (!x87_p) && (fetestexcept (excepts)))
    raise (SIGFPE); */
  return (! (sse_p || x87_p));
}

int
fegetexceptflag (fexcept_t *flagp, int excepts)
{
  if (excepts &~ FE_ALL_EXCEPT)
    return (-1);
  (*flagp) = (fetestexcept (excepts));
  return (! (sse_p || x87_p));
}

int
fesetexceptflag (const fexcept_t *flagp, int excepts)
{
  if (((*flagp) | excepts) &~ FE_ALL_EXCEPT)
    return (-1);
  if (sse_p)
    sse_write_mxcsr ((sse_read_mxcsr ()) | ((*flagp) & excepts));
  if (x87_p)
    {
      x87_fenv_t fenv;
      x87_read_environment (fenv.environment_bytes);
      (fenv.environment.x87_status_word) |= ((*flagp) & excepts);
      x87_write_environment (fenv.environment_bytes);
    }
  return (! (sse_p || x87_p));
}

/* The following are glibc extensions.  */

int
fegetexcept (void)
{
  int sse_mask = (sse_p ? ((sse_read_mxcsr ()) >> 7) : (~0));
  int x87_mask = (x87_p ? (x87_read_control_word ()) : (~0));
  return (FE_ALL_EXCEPT &~ (sse_mask & x87_mask));
}

int
feenableexcept (int excepts)
{
  int old_excepts = 0;
  if (excepts &~ FE_ALL_EXCEPT)
    return (-1);
  if (sse_p)
    {
      unsigned int mxcsr = (sse_read_mxcsr ());
      old_excepts |= (FE_ALL_EXCEPT &~ (mxcsr >> 7));
      sse_write_mxcsr (mxcsr &~ (excepts << 7));
    }
  if (x87_p)
    {
      unsigned short control_word = (x87_read_control_word ());
      old_excepts |= (FE_ALL_EXCEPT &~ control_word);
      x87_write_control_word (control_word &~ excepts);
    }
  return ((sse_p || x87_p) ? old_excepts : (-1));
}

int
fedisableexcept (int excepts)
{
  int old_excepts = 0;
  if (excepts &~ FE_ALL_EXCEPT)
    return (-1);
  if (sse_p)
    {
      unsigned int mxcsr = (sse_read_mxcsr ());
      old_excepts |= (FE_ALL_EXCEPT &~ (mxcsr >> 7));
      sse_write_mxcsr (mxcsr | (excepts << 7));
    }
  if (x87_p)
    {
      unsigned short control_word = (x87_read_control_word ());
      old_excepts |= (FE_ALL_EXCEPT &~ control_word);
      x87_write_control_word (control_word | excepts);
    }
  return ((sse_p || x87_p) ? old_excepts : (-1));
}

int
fegetenv (fenv_t *envp)
{
  if (sse_p)
    (envp->fenv_sse.sse_mxcsr) = (sse_read_mxcsr ());
  if (x87_p)
    x87_read_environment (envp->fenv_x87.environment_bytes);
  return (! (sse_p || x87_p));
}

int
fesetenv (const fenv_t *envp)
{
  if (sse_p)
    sse_write_mxcsr (envp->fenv_sse.sse_mxcsr);
  if (x87_p)
    x87_write_environment (envp->fenv_x87.environment_bytes);
  return (! (sse_p || x87_p));
}

int
feholdexcept (fenv_t *envp)
{
  int status = (fegetenv (envp));
  if (status != 0)
    return (status);
  if (sse_p)
    sse_write_mxcsr
      (((envp->fenv_sse.sse_mxcsr)
	| (FE_ALL_EXCEPT << 7))	/* Set all mask bits.  */
       &~ FE_ALL_EXCEPT);	/* Clear all flag bits.  */
  if (x87_p)
    {
      x87_clear_exceptions ();
      x87_write_control_word
	((envp->fenv_x87.environment.x87_control_word) | FE_ALL_EXCEPT);
    }
  return (0);
}

int
feupdateenv (const fenv_t *envp)
{
  int excepts = (fetestexcept (FE_ALL_EXCEPT));
  int status = (fesetenv (envp));
  if (status != 0)
    return (status);
  return (feraiseexcept (excepts));
}
