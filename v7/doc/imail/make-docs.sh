#!/bin/sh
#
# $Id: make-docs.sh,v 1.2 2000/07/07 21:15:52 cph Exp $
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

rm -rf doc-files
mkdir doc-files
cd doc-files
makeinfo ../imail.texinfo
texi2dvi ../imail.texinfo
dvips -o imail.ps imail.dvi
texi2html -split_chapter ../imail.texinfo
