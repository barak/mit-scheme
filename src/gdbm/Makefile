# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014
#     Massachusetts Institute of Technology
#
# This file is part of MIT/GNU Scheme.
#
# MIT/GNU Scheme is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# MIT/GNU Scheme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MIT/GNU Scheme; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301, USA.

MIT_SCHEME_EXE = mit-scheme
exe = '$(MIT_SCHEME_EXE)' --batch-mode

all: gdbm-shim.so gdbm-types.bin gdbm-const.bin
	echo '(load "compile")' | $(exe)

check:
	echo '(load "check")' | $(exe)

install:
	( echo '(begin'; \
	  echo '  (install-shim "$(DESTDIR)" "gdbm")'; \
	  echo '  (install-load-option "$(DESTDIR)" "gdbm2" "gdbm"))' ) \
	| $(exe) -- *.com *.bci *.pkd make.scm

clean distclean maintainer-clean:
	rm -f gdbm-const.scm gdbm-const gdbm-const.c gdbm-shim.c
	rm -f gdbm-*.crf gdbm-*.fre gdbm-*.pkd
	rm -f *.o *.so *.bin *.ext *.com *.bci *.moc *.fni
	rm -f gdbm-check.db

gdbm-shim.so: gdbm-shim.o gdbm-adapter.o
	echo "(link-shim)" | $(exe) -- -o $@ $^ -lgdbm

gdbm-adapter.o: gdbm-adapter.c gdbm-shim.h
	echo '(compile-shim)' | $(exe) -- -c $<

gdbm-shim.o: gdbm-shim.c gdbm-shim.h
	echo '(compile-shim)' | $(exe) -- -c $<

gdbm-shim.c gdbm-const.c gdbm-types.bin: gdbm.cdecl gdbm-shim.h
	echo '(generate-shim "gdbm" "#include \"gdbm-shim.h\"")' | $(exe)

gdbm-const.bin: gdbm-const.scm
	echo '(sf "gdbm-const")' | $(exe)

gdbm-const.scm: gdbm-const
	./gdbm-const

gdbm-const: gdbm-const.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

gdbm-const.o: gdbm-const.c gdbm-shim.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

.PHONY: all check install clean distclean maintainer-clean
