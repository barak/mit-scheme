#
#  This makefile handles these tasks:
#    . production of pcsdld.sl
#    . installation in scheme library
#
#  It does not handle the compilation of scheme files.

#SCHEME_ROOT=..
SCHEME_ROOT=/scheme/8.0/700
INSTALL_DIRECTORY=$(SCHEME_ROOT)/lib/pcsample

SCHEME_OBJECTS = load.com      pcsboot.com  pcsdisp.com   pcsiproc.com \
                 pcsample.com  pcscobl.com  pcsintrp.com  pribinut.com

pcsdld.sl: pcsdld.o
	ld -b -o pcsdld.sl pcsdld.o

pcsdld.o: pcsdld.c pcsample.c pcscobl.c pcsiproc.c
	cc -c -O -Ae +z -I$(SCHEME_ROOT) -DMIT_SCHEME -D_HPUX pcsdld.c

install: pcsdld.sl $(SCHEME_OBJECTS) pcs.bco pcs.bld
	-mkdir $(INSTALL_DIRECTORY)
	cp -p pcsdld.sl $(INSTALL_DIRECTORY)
	cp -p pcs.bco pcs.bld *.com *.bci $(INSTALL_DIRECTORY)

tags: TAGS
TAGS:
	etags *.scm *.c

.PHONY: tags TAGS install
