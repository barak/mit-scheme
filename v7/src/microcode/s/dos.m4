#!/bin/csh -f
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/s/Attic/dos.m4,v 1.1 1992/03/05 20:30:55 jinx Exp $
# Postprocessing to get valid dos assembly language from cmpaux-i386.m4

set tmpfil = "m4.tmp"
set seen_input = 0
rm -f "$tmpfil"

echo "define(DOS,1)" >> "$tmpfil"
while ($#argv != 0)
  if ("$argv[1]" == "-P") then
    echo "$argv[2]" >> "$tmpfil"
    shift
  else
    set seen_input = 1
    sed -e '/^#/D' < "$argv[1]" >> "$tmpfil"
  endif
  shift
end
if ($seen_input == 0) then
  sed -e '/^#/D' >> "$tmpfil"
endif
m4 < "$tmpfil" | sed -e 's/#/;/g' -e 's/^$//'
rm -f "$tmpfil"
