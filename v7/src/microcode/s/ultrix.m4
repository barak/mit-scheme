#!/bin/csh -f
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/s/Attic/ultrix.m4,v 1.1 1989/07/27 08:18:04 cph Exp $
# Postprocessing to make m4 work correctly under Ultrix.
m4 $* | sed -e 's/@/$$/g' -e 's/^$$//'
