
# $Id: Makefile,v 1.2 2000/07/10 22:04:23 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

all:	user.ps user.info html/user_toc.html

user.ps: user.dvi
	dvips -o $@ $^

user.dvi: user.texinfo
	texi2dvi $^

user.pdf: user.texinfo
	texi2pdf $^

user.info: user.texinfo
	makeinfo $^
	rm -f /usr/local/lib/mit-scheme/edwin/info/user.info*
	cpx user.info* /usr/local/lib/mit-scheme/edwin/info/.
	gzip -v /usr/local/lib/mit-scheme/edwin/info/user.info*

html/user_toc.html: user.texinfo
	(cd html;make)
