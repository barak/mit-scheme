#
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/Attic/Makefile,v 1.4 1988/10/29 05:14:08 jinx Exp $
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
	cd back ; mkdir STAGE1 ; mv *.com *.binf STAGE1/.
	cd base ; mkdir STAGE1 ; mv *.com *.binf STAGE1/.
	cd bobcat ; mkdir STAGE1 ; mv *.com *.binf STAGE1/.
	cd fggen ; mkdir STAGE1 ; mv *.com *.binf STAGE1/.
	cd fgopt ; mkdir STAGE1 ; mv *.com *.binf STAGE1/.
	cd rtlbase ; mkdir STAGE1 ; mv *.com *.binf STAGE1/.
	cd rtlgen ; mkdir STAGE1 ; mv *.com *.binf STAGE1/.
	cd rtlopt ; mkdir STAGE1 ; mv *.com *.binf STAGE1/.

unstage1:
	cd back ; mv STAGE1/* . ; rmdir STAGE1
	cd base ; mv STAGE1/* . ; rmdir STAGE1
	cd bobcat ; mv STAGE1/* . ; rmdir STAGE1
	cd fggen ; mv STAGE1/* . ; rmdir STAGE1
	cd fgopt ; mv STAGE1/* . ; rmdir STAGE1
	cd rtlbase ; mv STAGE1/* . ; rmdir STAGE1
	cd rtlgen ; mv STAGE1/* . ; rmdir STAGE1
	cd rtlopt ; mv STAGE1/* . ; rmdir STAGE1

cpstage1:
	cd back ; cp STAGE1/* . 
	cd base ; cp STAGE1/* . 
	cd bobcat ; cp STAGE1/* . 
	cd fggen ; cp STAGE1/* . 
	cd fgopt ; cp STAGE1/* . 
	cd rtlbase ; cp STAGE1/* . 
	cd rtlgen ; cp STAGE1/* . 
	cd rtlopt ; cp STAGE1/* . 

lnstage1:
	cd back ; ln STAGE1/* . 
	cd base ; ln STAGE1/* . 
	cd bobcat ; ln STAGE1/* . 
	cd fggen ; ln STAGE1/* . 
	cd fgopt ; ln STAGE1/* . 
	cd rtlbase ; ln STAGE1/* . 
	cd rtlgen ; ln STAGE1/* . 
	cd rtlopt ; ln STAGE1/* . 

stage2:
	cd back ; mkdir STAGE2 ; mv *.com *.binf STAGE2/.
	cd base ; mkdir STAGE2 ; mv *.com *.binf STAGE2/.
	cd bobcat ; mkdir STAGE2 ; mv *.com *.binf STAGE2/.
	cd fggen ; mkdir STAGE2 ; mv *.com *.binf STAGE2/.
	cd fgopt ; mkdir STAGE2 ; mv *.com *.binf STAGE2/.
	cd rtlbase ; mkdir STAGE2 ; mv *.com *.binf STAGE2/.
	cd rtlgen ; mkdir STAGE2 ; mv *.com *.binf STAGE2/.
	cd rtlopt ; mkdir STAGE2 ; mv *.com *.binf STAGE2/.

unstage2:
	cd back ; mv STAGE2/* . ; rmdir STAGE2
	cd base ; mv STAGE2/* . ; rmdir STAGE2
	cd bobcat ; mv STAGE2/* . ; rmdir STAGE2
	cd fggen ; mv STAGE2/* . ; rmdir STAGE2
	cd fgopt ; mv STAGE2/* . ; rmdir STAGE2
	cd rtlbase ; mv STAGE2/* . ; rmdir STAGE2
	cd rtlgen ; mv STAGE2/* . ; rmdir STAGE2
	cd rtlopt ; mv STAGE2/* . ; rmdir STAGE2

cpstage2:
	cd back ; cp STAGE2/* . 
	cd base ; cp STAGE2/* . 
	cd bobcat ; cp STAGE2/* . 
	cd fggen ; cp STAGE2/* . 
	cd fgopt ; cp STAGE2/* . 
	cd rtlbase ; cp STAGE2/* . 
	cd rtlgen ; cp STAGE2/* . 
	cd rtlopt ; cp STAGE2/* . 

lnstage2:
	cd back ; ln STAGE2/* . 
	cd base ; ln STAGE2/* . 
	cd bobcat ; ln STAGE2/* . 
	cd fggen ; ln STAGE2/* . 
	cd fgopt ; ln STAGE2/* . 
	cd rtlbase ; ln STAGE2/* . 
	cd rtlgen ; ln STAGE2/* . 
	cd rtlopt ; ln STAGE2/* . 

stage3:
	cd back ; mkdir STAGE3 ; mv *.com *.binf STAGE3/.
	cd base ; mkdir STAGE3 ; mv *.com *.binf STAGE3/.
	cd bobcat ; mkdir STAGE3 ; mv *.com *.binf STAGE3/.
	cd fggen ; mkdir STAGE3 ; mv *.com *.binf STAGE3/.
	cd fgopt ; mkdir STAGE3 ; mv *.com *.binf STAGE3/.
	cd rtlbase ; mkdir STAGE3 ; mv *.com *.binf STAGE3/.
	cd rtlgen ; mkdir STAGE3 ; mv *.com *.binf STAGE3/.
	cd rtlopt ; mkdir STAGE3 ; mv *.com *.binf STAGE3/.

unstage3:
	cd back ; mv STAGE3/* . ; rmdir STAGE3
	cd base ; mv STAGE3/* . ; rmdir STAGE3
	cd bobcat ; mv STAGE3/* . ; rmdir STAGE3
	cd fggen ; mv STAGE3/* . ; rmdir STAGE3
	cd fgopt ; mv STAGE3/* . ; rmdir STAGE3
	cd rtlbase ; mv STAGE3/* . ; rmdir STAGE3
	cd rtlgen ; mv STAGE3/* . ; rmdir STAGE3
	cd rtlopt ; mv STAGE3/* . ; rmdir STAGE3

cpstage3:
	cd back ; cp STAGE3/* . 
	cd base ; cp STAGE3/* . 
	cd bobcat ; cp STAGE3/* . 
	cd fggen ; cp STAGE3/* . 
	cd fgopt ; cp STAGE3/* . 
	cd rtlbase ; cp STAGE3/* . 
	cd rtlgen ; cp STAGE3/* . 
	cd rtlopt ; cp STAGE3/* . 

lnstage3:
	cd back ; ln STAGE3/* . 
	cd base ; ln STAGE3/* . 
	cd bobcat ; ln STAGE3/* . 
	cd fggen ; ln STAGE3/* . 
	cd fgopt ; ln STAGE3/* . 
	cd rtlbase ; ln STAGE3/* . 
	cd rtlgen ; ln STAGE3/* . 
	cd rtlopt ; ln STAGE3/* . 
