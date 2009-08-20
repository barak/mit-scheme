#
# $Id$
#
# Makefile for Scheme compiler
# This makefile supports some utilities for the compiler.
# The compiler provides its own tools for rebuilding itself,
# written in Scheme.
#

SOURCES = back/foo base/foo machine/foo midend/foo rtlbase/foo rtlopt/foo
SUBDIRS = $(SOURCES:foo=)

SHELL = /bin/sh

all:
	echo "No ALL action"

tags:
	etags back/*.scm base/*.scm \
	machines/*/*.scm midend/*.scm rtlbase/*.scm rtlopt/*.scm

index:
	rm -f INDEX
	/zu/cph/bin/schndx $(SOURCES:foo=*.scm) > INDEX

listing:
	prlist comp.pkg $(SOURCES:foo=*.scm)

locked:
	for subdir in . back base midend rtlbase rtlopt machines/*; \
	do \
	rlog -L -R $${subdir}/RCS/*,v ; \
	done

stage1:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	mkdir STAGE1 ; mv *.com *.bci STAGE1/. ) \
	done

unstage1:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	mv STAGE1/* . ; rmdir STAGE1 ) \
	done

rmstage1:
	rm -rf $(SOURCES:foo=STAGE1)

cpstage1:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	cpx STAGE1/* . ) \
	done

lnstage1:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	ln STAGE1/* . ) \
	done

stage2:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	mkdir STAGE2 ; mv *.com *.bci STAGE2/. ) \
	done

unstage2:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	mv STAGE2/* . ; rmdir STAGE2 ) \
	done

rmstage2:
	rm -rf $(SOURCES:foo=STAGE2)

cpstage2:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	cpx STAGE2/* . ) \
	done

lnstage2:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	ln STAGE2/* . ) \
	done

stage3:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	mkdir STAGE3 ; mv *.com *.bci STAGE3/. ) \
	done

unstage3:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	mv STAGE3/* . ; rmdir STAGE3 ) \
	done

rmstage3:
	rm -rf $(SOURCES:foo=STAGE3)

cpstage3:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	cpx STAGE3/* . ) \
	done

lnstage3:
	for subdir in $(SUBDIRS); \
	do \
	( cd $$subdir ; echo "making $$subdir" ; \
	ln STAGE3/* . ) \
	done
