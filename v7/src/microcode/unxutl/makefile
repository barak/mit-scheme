#
# Makefile for MIT CScheme microcode.
#
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/unxutl/Attic/makefile,v 1.14 1991/10/29 22:40:06 jinx Exp $
#
CPP = cc -E
MAKE = make  # BSD doesn't have it as a default.

all: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} all
.PHONY: all

scheme: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} scheme
.PHONY: scheme

xscheme: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} xscheme
.PHONY: xscheme

bchscheme: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} bchscheme
.PHONY: bchscheme

bchdrn: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} bchdrn
.PHONY: bchdrn

Psbtobin: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Psbtobin
.PHONY: Psbtobin

Bintopsb: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Bintopsb
.PHONY: Bintopsb

Ppband: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Ppband
.PHONY: Ppband

install: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} install
.PHONY: install

world: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} world
.PHONY: world

# If you have a problem with cc -E here, changing
# the definition of CPP above may fix it.
xmakefile: ymakefile ymake.script ymake.local cf.h s.h m.h
	rm -f xmakefile junk.c
	cp ymakefile junk.c
	$(CPP) junk.c | sed -n -f ymake.script > xmakefile
	rm -f junk.c

clean remove:
	rm -f *.o *.touch xmakefile
	rm -f scheme xscheme bchscheme
	rm -f Findprim Bintopsb Psbtobin Breakup Wsize Ppband hard-params
	rm -f usrdef.c limits.h float.h
	rm -f lint.out

tags:
	etags *.[ch] ../runtime/*.scm

locked:
	rlog -L -R RCS/*,v

getpagesize.h : pagesize.h
	rm -f getpagesize.h
	cp pagesize.h getpagesize.h
