#!/bin/sh
###
###	$Id: ultrix.m4,v 1.7 1995/06/28 22:05:35 cph Exp $
###
###	Copyright (c) 1989-1995 Massachusetts Institute of Technology
###
####	Postprocessing to make m4 work correctly under Ultrix & BSD.

if [ $# = 0 ]
then
  sed -e '/^#/D' | m4 | sed -e 's/@/$/g' -e 's/^$//'
else
  tmpfil="m4.tmp"
  seen_input=0
  rm -f "$tmpfil"

  while [ $# != 0 ]
  do
    if [ "$1" = "-P" ]
    then
      echo "$2" >> "$tmpfil"
      shift
    else
      seen_input=1
      sed -e '/^#/D' < "$1" >> "$tmpfil"
    fi
    shift
  done
  if [ $seen_input = 0 ]
  then
    sed -e '/^#/D' >> "$tmpfil"
  fi
  m4 < "$tmpfil" | sed -e 's/@/$/g' -e 's/^$//'
  rm -f "$tmpfil"
fi
