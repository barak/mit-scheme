
# $Id: Makefile,v 1.1 2000/07/09 01:19:11 cph Exp $
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

all:	imail.ps imail.info html/imail_toc.html

imail.ps: imail.dvi
	dvips -o $@ $^

imail.dvi: imail.texinfo
	texi2dvi $^

imail.info: imail.texinfo
	makeinfo $^
#	rm -f /usr/local/lib/mit-scheme/edwin/info/imail.info*
#	cpx imail.info* /usr/local/lib/mit-scheme/edwin/info/.
#	gzip -v /usr/local/lib/mit-scheme/edwin/info/imail.info*

html/imail_toc.html: imail.texinfo
	(cd html;make)
