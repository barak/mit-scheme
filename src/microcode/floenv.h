/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

/* Floating Point Environment */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "cmpintmd.h"

#if (defined (HAVE_FENV_H))
#  include <fenv.h>
#  ifdef HAVE_FENV_T
#    define scheme_fenv_t fenv_t
#  endif
#  ifdef HAVE_FEXCEPT_T
#    define scheme_fexcept_t fexcept_t
#  endif
#  ifdef __APPLE__
#    undef HAVE_FEGETEXCEPT
#  endif
#elif ((!defined (CMPINTMD_EMULATES_FENV)) && (defined (HAVE_IEEEFP_H)))

/* Assumption: If we have <ieeefp.h>, then we don't need to test for
   individual definitions in it.  If you come across a different
   <ieeefp.h> from what one finds on BSD systems, you'll have to fix
   this code.  */

#  include <ieeefp.h>

#  ifndef FE_TONEAREST
#    define FE_TONEAREST FP_RN
#  endif

#  ifndef FE_DOWNWARD
#    define FE_DOWNWARD FP_RM
#  endif

#  ifndef FE_UPWARD
#    define FE_UPWARD FP_RP
#  endif

#  ifndef FE_TOWARDZERO
#    define FE_TOWARDZERO FP_RZ
#  endif

#  ifndef HAVE_FEGETROUND
#    define HAVE_FEGETROUND
#    define fegetround fpgetround
#  endif

#  ifndef HAVE_FESETROUND
#    define HAVE_FESETROUND
#    define fesetround(rm) ((fpsetround (rm)), 0)
#  endif

#  ifndef FE_INVALID
#    define FE_INVALID FP_X_INV
#  endif

#  ifndef FE_DIVBYZERO
#    define FE_DIVBYZERO FP_X_DZ
#  endif

#  ifndef FE_OVERFLOW
#    define FE_OVERFLOW FP_X_OFL
#  endif

#  ifndef FE_UNDERFLOW
#    define FE_UNDERFLOW FP_X_UFL
#  endif

#  ifndef FE_INEXACT
#    define FE_INEXACT FP_X_IMP
#  endif

#  ifndef FE_ALL_EXCEPT
#    define FE_ALL_EXCEPT			\
  (FE_DIVBYZERO | FE_INEXACT | FE_INVALID | FE_OVERFLOW | FE_UNDERFLOW)
#  endif

/* FP_X_IOV?  */

#  ifndef HAVE_FEXCEPT_T
#    define HAVE_FEXCEPT_T
typedef fp_except fexcept_t;
#  endif

#  ifndef HAVE_FETESTEXCEPT
#    define HAVE_FETESTEXCEPT
#    define fetestexcept(excepts) ((excepts) & (fpgetsticky ()))
#  endif

#  ifndef HAVE_FECLEAREXCEPT
#    define HAVE_FECLEAREXCEPT
#    define feclearexcept(excepts)		\
  ((fpsetsticky ((fpgetsticky ()) &~ (FE_ALL_EXCEPT & (excepts)))), 0)
#  endif

#  ifndef HAVE_FERAISEEXCEPT
#    define HAVE_FERAISEEXCEPT
static inline int
feraiseexcept (int excepts)
{
  (void) fpsetsticky ((fpgetsticky ()) | (FE_ALL_EXCEPT & (excepts)));
  /* Force a floating-point operation to happen, which ideally should
     trap if there are unmasked exceptions pending, presumably because
     of the above fpsetsticky.  */
  volatile double x = 0;
  volatile double y = 0;
  volatile double z = (x + y);
  (void) z;			/* ignored */
  return (0);
}
#  endif

#  ifndef HAVE_FEGETEXCEPTFLAG
#    define HAVE_FEGETEXCEPTFLAG
#    define fegetexceptflag(flagp, excepts)	\
  (((* ((0 ? ((int *) 0) : flagp))) = ((excepts) & (fpgetsticky ()))), 0)
#  endif

#  ifndef HAVE_FESETEXCEPTFLAG
#    define HAVE_FESETEXCEPTFLAG
#    define fesetexceptflag(flagp, excepts)				\
  ((fpsetsticky								\
    (FE_ALL_EXCEPT & (excepts) & (* ((0 ? ((const int *) 0) : flagp))))), \
   0)
#  endif

#  ifndef HAVE_FEGETEXCEPT
#    define HAVE_FEGETEXCEPT
#    define fegetexcept fpgetmask
#  endif

#  ifndef HAVE_FEENABLEEXCEPT
#    define HAVE_FEENABLEEXCEPT
#    define feenableexcept(excepts)		\
  (fpsetmask ((fpgetmask ()) | (FE_ALL_EXCEPT & (excepts))))
#  endif

#  ifndef HAVE_FEDISABLEEXCEPT
#    define HAVE_FEDISABLEEXCEPT
#    define fedisableexcept(excepts)		\
  (fpsetmask ((fpgetmask ()) &~ (FE_ALL_EXCEPT & (excepts))))
#  endif

/* Kludge for NetBSD<6, which only halfway supports C99 fenv cruft.  */
#  ifdef fenv_t
#    undef fenv_t
#  endif
#  define fenv_t scheme_fenv_t
#  ifndef HAVE_FENV_T
#    define HAVE_FENV_T
#  endif

typedef struct
{
  fp_except fe_enabled_exceptions;
  fp_except fe_sticky_exceptions;
  fp_rnd fe_rounding_mode;
} fenv_t;

#  define HAVE_FEGETENV
#  define HAVE_FESETENV
#  define HAVE_FEHOLDEXCEPT
#  define HAVE_FEUPDATEENV

static inline int
fegetenv (fenv_t *fe)
{
  (fe->fe_enabled_exceptions) = (fpgetmask ());
  (fe->fe_sticky_exceptions) = (fpgetsticky ());
  (fe->fe_rounding_mode) = (fpgetround ());
  return (0);
}

static inline int
fesetenv (const fenv_t *fe)
{
  (void) fpsetmask (fe->fe_enabled_exceptions);
  (void) fpsetsticky (fe->fe_sticky_exceptions);
  (void) fpsetround (fe->fe_rounding_mode);
  return (0);
}

static inline int
feholdexcept (fenv_t *fe)
{
  (void) fegetenv (fe);
  fpsetmask (0);
  return (0);
}

static inline int
feupdateenv (const fenv_t *fe)
{
  fp_except exceptions = (fpgetsticky ());
  (void) fesetenv (fe);
  (void) feraiseexcept (exceptions);
  return (0);
}
#endif
