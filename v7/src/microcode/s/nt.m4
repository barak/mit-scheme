#!/bin/csh -f
###
###	$Id: nt.m4,v 1.1 1993/06/24 08:04:38 gjr Exp $
###
###	Copyright (c) 1993 Massachusetts Institute of Technology
###
####	Postprocessing to get valid nt assembly language from cmpauxmd/i386.m4

set tmpfil = "m4.tmp"
set seen_input = 0
rm -f "$tmpfil"

echo changecom\(\`\;\'\) >> "$tmpfil"
echo "define(DOS,1)" >> "$tmpfil"
echo "define(WINNT,1)" >> "$tmpfil"
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
