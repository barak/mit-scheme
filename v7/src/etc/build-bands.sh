#!/bin/sh
#
# $Id: build-bands.sh,v 1.1 2000/12/07 21:58:57 cph Exp $
#
# Copyright (c) 2000 Massachusetts Institute of Technology
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(
    cd runtime
    ../microcode/scheme -library ../lib -fasl make.com <<EOF
(disk-save "../lib/runtime.com")
EOF
)

microcode/scheme -library lib -large <<EOF
(load-option 'SF)
(load-option 'COMPILER)
(disk-save "lib/compiler.com")
EOF

microcode/scheme -library lib -compiler <<EOF
(load-option 'EDWIN)
(disk-save "lib/all.com")
EOF

microcode/scheme -library lib -large <<EOF
(load-option 'EDWIN)
(disk-save "lib/edwin.com")
EOF

(
    cd runtime-check
    ../microcode/scheme -library ../lib -fasl make.com <<EOF
(load-option 'EDWIN)
(load-option 'STUDENT)
(disk-save "../lib/6001.com")
EOF
)
