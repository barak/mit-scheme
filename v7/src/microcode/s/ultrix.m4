#!/bin/csh -f
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/s/Attic/ultrix.m4,v 1.2 1989/08/02 00:53:42 cph Exp $
# Postprocessing to make m4 work correctly under Ultrix.
m4 $* | sed -e 's/@/$/g' -e 's/^$//'
