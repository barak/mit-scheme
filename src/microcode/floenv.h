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
#elif ((!defined (CMPINTMD_EMULATES_FENV)) && (defined (HAVE_IEEEFP_H)))

/* Assumption: If we have <ieeefp.h>, then we don't need to test for
   individual definitions in it.  If you come across a different
   <ieeefp.h> from what one finds on BSD systems, you'll have to fix
   this code.  */

#  include <ieeefp.h>

#  define FE_TONEAREST FP_RN
#  define FE_DOWNWARD FP_RM
#  define FE_UPWARD FP_RP
#  define FE_TOWARDZERO FP_RZ

#  define fegetround fpgetround
#  define fesetround(rm) ((fpsetround (rm)), 0)

#  define FE_INVALID FP_X_INV
#  define FE_DIVBYZERO FP_X_DZ
#  define FE_OVERFLOW FP_X_OFL
#  define FE_UNDERFLOW FP_X_UFL
#  define FE_INEXACT FP_X_IMP
/* FP_X_IOV?  */

#  define fexcept_t fp_except

#  define fetestexcepts(excepts) ((excepts) & (fpgetsticky ()))
#  define fecleareexcept(excepts)		\
  (fpsetsticky ((fpgetsticky ()) &~ (FE_ALL_EXCEPT & (excepts))))

/* This isn't right -- it doesn't necessarily actually raise the
   exception until some floating-point operation is performed.  */
#  define feraiseexcept(excepts)		\
  (fpsetsticky ((fpgetsticky ()) || (FE_ALL_EXCEPT & (excepts)))

#  define fegetexceptflag(flagp, excepts)	\
  (((* ((0 ? ((int *) 0) : flagp))) = ((excepts) & (fpgetsticky ()))), 0)
#  define fesetexceptflag(flagp, excepts)				\
  (fpsetsticky								\
   (FE_ALL_EXCEPT & (excepts) & (* ((0 ? ((const int *) 0) : flagp)))))

#  define fegetexcept fpgetmask
#  define feenableexcept(excepts)		\
  (fpsetmask ((fpgetmask ()) || (FE_ALL_EXCEPT & (excepts))))
#  define fedisableexcept(excepts)		\
  (fpsetmask ((fpgetmask ()) &~ (FE_ALL_EXCEPT & (excepts))))

typedef struct
{
  fp_except fe_enabled_exceptions;
  fp_except fe_sticky_exceptions;
  fp_rnd fe_rounding_mode;
} fenv_t;

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
  /* Unfortunately, this doesn't actually do anything, because of the
     useless definition of feraiseexcept above.  */
  (void) feraiseexcept (exceptions);
}
#endif
