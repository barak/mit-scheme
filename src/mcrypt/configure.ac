dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme mcrypt interface],
        [0.1],
        [bug-mit-scheme@gnu.org],
        [mit-scheme-mcrypt])
AC_CONFIG_SRCDIR([mcrypt.pkg])
AC_CONFIG_HEADERS([config.h])

AC_COPYRIGHT(
[Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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
])

AH_TOP([/*

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

*/])

AC_ARG_ENABLE([smp],
    AS_HELP_STRING([---enable-smp],
	[Support multi-processing if available [[yes]]]))
: ${with_smp='no'}

AC_CHECK_HEADER([mcrypt.h],[],[
    AC_MSG_ERROR([Header file <mcrypt.h> not found.])])

dnl Check for SMP support (pthreads... ?).
if test "${enable_smp}" != no; then
    AC_CHECK_HEADER([pthread.h],
	[
	AC_DEFINE([ENABLE_SMP], [1],
	    [Define to 1 for Symmetric MultiProcessing support.])
	],
	[AC_MSG_ERROR([Header file <pthread.h> not found.])])
fi

MCRYPT_CFLAGS=`libmcrypt-config --cflags`
MCRYPT_LIBS=`libmcrypt-config --libs`
CFLAGS="${MCRYPT_CFLAGS} ${CFLAGS}"
LIBS="${LIBS} ${MCRYPT_LIBS}"

AC_SUBST([CFLAGS])
AC_SUBST([LIBS])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
