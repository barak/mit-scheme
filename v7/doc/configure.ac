dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT Scheme documentation],
	[7.7.2pre],
	[bug-cscheme@zurich.ai.mit.edu],
	[mit-scheme-doc])
AC_REVISION([$Id: configure.ac,v 1.1 2002/11/27 03:15:21 cph Exp $])
AC_CONFIG_SRCDIR([ref-manual/scheme.texinfo])

AC_COPYRIGHT(
[Copyright (c) 2000, 2001, 2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
])

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
if test "$(${INSTALL} --help 2> /dev/null | fgrep -e --preserve-timestamps)"
then
    INSTALL="${INSTALL} --preserve-timestamps"
fi

INST_TARGETS=

AC_ARG_ENABLE([html],
	[AC_HELP_STRING([--enable-html], [generate HTML documentation])])
if test "${enable_html:-yes}" = "yes"; then
    htmldir="${libdir}/mit-scheme/doc"
elif test "${enable_html}" = "no"; then
    htmldir=
else
    htmldir="${enable_html}"
fi
test "${htmldir}" = "" || INST_TARGETS="${INST_TARGETS} install-html"
AC_SUBST([htmldir])

AC_ARG_ENABLE([pdf],
	[AC_HELP_STRING([--enable-pdf], [generate PDF documentation])])
if test "${enable_pdf:-yes}" = "yes"; then
    pdfdir="${libdir}/mit-scheme/doc"
elif test "${enable_pdf}" = "no"; then
    pdfdir=
else
    pdfdir="${enable_pdf}"
fi
test "${pdfdir}" = "" || INST_TARGETS="${INST_TARGETS} install-pdf"
AC_SUBST([pdfdir])

AC_ARG_ENABLE([ps],
	[AC_HELP_STRING([--enable-ps], [generate Postscript documentation])])
if test "${enable_ps:-yes}" = "yes"; then
    psdir="${libdir}/mit-scheme/doc"
elif test "${enable_ps}" = "no"; then
    psdir=
else
    psdir="${enable_ps}"
fi
test "${psdir}" = "" || INST_TARGETS="${INST_TARGETS} install-ps"
AC_SUBST([psdir])

AC_SUBST([INST_TARGETS])

for D in ref-manual sos user-manual; do
    test -L "${D}/gfdl.texinfo" \
      || (cd "${D}"; ${LN_S} ../imail/gfdl.texinfo .)
done

AC_CONFIG_FILES([
	Makefile
	imail/Makefile
	ref-manual/Makefile
	sos/Makefile
	user-manual/Makefile])
AC_OUTPUT
