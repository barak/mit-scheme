#!/bin/csh -f
###	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/s/Attic/dos.m4,v 1.3 1992/08/24 15:18:30 jinx Exp $
###	Copyright (c) 1992 Massachusetts Institute of Technology

####	Postprocessing to get valid dos assembly language from cmpaux-i386.m4

set tmpfil = "m4.tmp"
set seen_input = 0
rm -f "$tmpfil"

echo changecom\(\`\;\'\) >> "$tmpfil"
echo "define(DOS,1)" >> "$tmpfil"
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
