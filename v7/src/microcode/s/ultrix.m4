#!/bin/csh -f
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/s/Attic/ultrix.m4,v 1.3 1990/08/14 18:20:07 cph Rel $
# Postprocessing to make m4 work correctly under Ultrix.
cc -E $* | sed -e '/^#/D' | m4 | sed -e 's/@/$/g' -e 's/^$//'
