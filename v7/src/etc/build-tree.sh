#!/bin/sh
#
# $Id: build-tree.sh,v 1.2 2000/10/16 18:02:42 cph Exp $
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

# Program to finish setting up the Scheme source tree after it is
# checked out.  Adds required links, builds TAGS files, etc.

if [ ! -d 6001 ]
then
  echo "This must be run from the top-level Scheme source directory."
  exit 1
fi
for directory in 6001 cref edwin imail rcs runtime sf sos win32
do
  (cd $directory; ln -s ../Makefile.std Makefile)
done
for directory in 6001 compiler cref edwin imail rcs runtime sf sos swat win32
do
  (cd $directory; make TAGS)
done
for directory in edwin imail runtime sos
do
  (cd $directory; ln -s ed-ffi.scm .edwin-ffi)
done
(cd microcode; scheme -load os2pm.scm < /dev/null)
(cd microcode; etags -r '/^DEF[A-Za-z_ \t(]+"\([^"]+\)"/' *.[ch])
(cd microcode/cmpauxmd; make all)
(cd pcsample; etags *.scm *.c)
(cd compiler/machines/vax;
  for n in 1 2 3
  do
    ln -s instr${n}.scm dinstr${n}.scm
  done)
