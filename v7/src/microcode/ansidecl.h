/* Copyright (C) 1990 Free Software Foundation, Inc.
This file is part of the GNU C Library.

$Id: ansidecl.h,v 1.7 2000/12/05 21:23:42 cph Exp $

The GNU C Library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU C Library; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* ANSI and traditional C compatibility macros

   ANSI C is assumed if STDC_HEADERS is #defined.

	Macros
		PTR		- Generic pointer type
		LONG_DOUBLE	- `long double' type
		CONST		- `const' keyword
		VOLATILE	- `volatile' keyword
		SIGNED		- `signed' keyword
		PTRCONST	- Generic const pointer (void *const)

	EXFUN(name, prototype)		- declare external function NAME
					  with prototype PROTOTYPE
	DEFUN(name, arglist, args)	- define function NAME with
					  args ARGLIST of types in ARGS
	DEFUN_VOID(name)		- define function NAME with no args
	AND				- argument separator for ARGS
	NOARGS				- null arglist
	DOTS				- `...' in args

    For example:
	extern int EXFUN(printf, (CONST char *format DOTS));
	int DEFUN(fprintf, (stream, format),
		  FILE *stream AND CONST char *format DOTS) { ... }
	void DEFUN_VOID(abort) { ... }
*/

#ifndef	_ANSIDECL_H
#define	_ANSIDECL_H	1


/* Every source file includes this file,
   so they will all get the switch for lint.  */
/* LINTLIBRARY */

#if defined(__STDC__) || defined(STDC_HEADERS)
#define HAVE_STDC

#define	PTR		void *
#define	PTRCONST	void *CONST
#define	LONG_DOUBLE	long double

#define	AND		,
#define	NOARGS		void
#define	VOLATILE	volatile
#define	SIGNED		signed
#define	DOTS		, ...

/* Some systems don't declare their libraries correctly, making CONST
   impossible to have. */
#ifdef CONST
#undef CONST
#endif

#ifdef NO_CONST
#define CONST
#else
#define	CONST		const
#endif

#define	EXFUN(name, proto)		name proto
#define	DEFUN(name, arglist, args)	name(args)
#define	DEFUN_VOID(name)		name(NOARGS)

#else /* not (__STDC__ || STDC_HEADERS) */

#define	PTR		char *
#define	PTRCONST	PTR
#define	LONG_DOUBLE	double

#define	AND		;
#define	NOARGS
#define	CONST
#define	VOLATILE
#define	SIGNED
#define	DOTS

#define	EXFUN(name, proto)		name()
#define	DEFUN(name, arglist, args)	name arglist args;
#define	DEFUN_VOID(name)		name()

#endif /* not (__STDC__ || STDC_HEADERS)  */

#endif	/* _ANSIDECL_H */
