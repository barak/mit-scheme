#!/bin/sh
#
# $Id: Tags.sh,v 1.4 2003/02/14 18:48:11 cph Exp $
#
# Copyright 2000 Massachusetts Institute of Technology
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

# Utility to make TAGS file for the MIT/GNU Scheme compiler directory.
# The working directory must be the compiler directory.

etags back/*.scm base/*.scm fggen/*.scm fgopt/*.scm machine/*.scm \
      rtlbase/*.scm rtlgen/*.scm rtlopt/*.scm
