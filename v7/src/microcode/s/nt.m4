#!/bin/csh -f
###
### $Id: nt.m4,v 1.2 1995/10/24 06:29:51 cph Exp $
###
### Copyright (c) 1993-95 Massachusetts Institute of Technology
###
### Processing to get Win32 assembly language from "i386.m4".

set tmpfil = "m4.tmp"
set seen_input = 0
rm -f "$tmpfil"

echo changecom\(\`\;\'\) >> "$tmpfil"
echo "define(WIN32,1)" >> "$tmpfil"
while ($#argv != 0)
  if ("$argv[1]" == "-P") then
    echo "$argv[2]" >> "$tmpfil"
    shift
  else
    set seen_input = 1
    sed -e '/#/;/g' < "$argv[1]" >> "$tmpfil"
  endif
  shift
end
if ($seen_input == 0) then
  sed -e 's/#/;/g' >> "$tmpfil"
endif
m4 < "$tmpfil" | sed -e 's/^$//' | sed -n -e '/^..*/p'
rm -f "$tmpfil"
