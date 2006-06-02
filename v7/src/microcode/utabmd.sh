#!/bin/sh
#
# $Id: utabmd.sh,v 1.6 2006/06/02 06:50:11 ihtfisp Exp $
#
# Copyright 2002,2003 Massachusetts Institute of Technology
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
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

if [ -z "${SCHEME_COMPILER}" ]; then
    SCHEME_COMPILER="scheme --compiler"
fi
echo '(sf "utabmd")' | ${SCHEME_COMPILER}
