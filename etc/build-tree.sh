#!/bin/sh
#
# $Id: build-tree.sh,v 1.5 2000/08/18 19:33:24 cph Exp $
#
# Program to finish setting up the Scheme source tree after it is
# checked out.  Adds required links, builds TAGS files, etc.
#
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
(cd microcode; etags -r '/^DEF[A-Za-z_ \t(]+"\([^"]+\)"/' *.[ch])
(cd microcode; scheme -load os2pm.scm < /dev/null)
(cd microcode/cmpauxmd; make all)
(cd pcsample; etags *.scm *.c)
(cd compiler/machines/vax;
  for n in 1 2 3
  do
    ln -s instr${n}.scm dinstr${n}.scm
  done)
