#
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/Attic/Makefile,v 1.2 1988/08/31 06:09:11 cph Exp $
#
# Makefile for Scheme compiler
# This makefile supports some utilities for the compiler.
# The compiler provides its own tools for rebuilding itself,
# written in Scheme.
#

SOURCES = back/foo base/foo bobcat/foo fggen/foo fgopt/foo\
	rtlbase/foo rtlgen/foo rtlopt/foo

SHELL = /bin/sh

all:
	echo "No ALL action"

tags:
	etags $(SOURCES:foo=*.scm)

index:
	rm -f INDEX
	/zu/cph/bin/schndx $(SOURCES:foo=*.scm) > INDEX

listing:
	prlist comp.pkg $(SOURCES:foo=*.scm)

locked:
	rlog -L -R RCS/*,v $(SOURCES:foo=RCS/*,v)

stage1:
	cd back ; mkdir STAGE1 ; mv *.com *.binf STAGE1
	cd base ; mkdir STAGE1 ; mv *.com *.binf STAGE1
	cd bobcat ; mkdir STAGE1 ; mv *.com *.binf STAGE1
	cd fggen ; mkdir STAGE1 ; mv *.com *.binf STAGE1
	cd fgopt ; mkdir STAGE1 ; mv *.com *.binf STAGE1
	cd rtlbase ; mkdir STAGE1 ; mv *.com *.binf STAGE1
	cd rtlgen ; mkdir STAGE1 ; mv *.com *.binf STAGE1
	cd rtlopt ; mkdir STAGE1 ; mv *.com *.binf STAGE1

unstage1:
	cd back ; mv STAGE1/* . ; rmdir STAGE1
	cd base ; mv STAGE1/* . ; rmdir STAGE1
	cd bobcat ; mv STAGE1/* . ; rmdir STAGE1
	cd fggen ; mv STAGE1/* . ; rmdir STAGE1
	cd fgopt ; mv STAGE1/* . ; rmdir STAGE1
	cd rtlbase ; mv STAGE1/* . ; rmdir STAGE1
	cd rtlgen ; mv STAGE1/* . ; rmdir STAGE1
	cd rtlopt ; mv STAGE1/* . ; rmdir STAGE1

stage2:
	cd back ; mkdir STAGE2 ; mv *.com *.binf STAGE2
	cd base ; mkdir STAGE2 ; mv *.com *.binf STAGE2
	cd bobcat ; mkdir STAGE2 ; mv *.com *.binf STAGE2
	cd fggen ; mkdir STAGE2 ; mv *.com *.binf STAGE2
	cd fgopt ; mkdir STAGE2 ; mv *.com *.binf STAGE2
	cd rtlbase ; mkdir STAGE2 ; mv *.com *.binf STAGE2
	cd rtlgen ; mkdir STAGE2 ; mv *.com *.binf STAGE2
	cd rtlopt ; mkdir STAGE2 ; mv *.com *.binf STAGE2

unstage2:
	cd back ; mv STAGE2/* . ; rmdir STAGE2
	cd base ; mv STAGE2/* . ; rmdir STAGE2
	cd bobcat ; mv STAGE2/* . ; rmdir STAGE2
	cd fggen ; mv STAGE2/* . ; rmdir STAGE2
	cd fgopt ; mv STAGE2/* . ; rmdir STAGE2
	cd rtlbase ; mv STAGE2/* . ; rmdir STAGE2
	cd rtlgen ; mv STAGE2/* . ; rmdir STAGE2
	cd rtlopt ; mv STAGE2/* . ; rmdir STAGE2
