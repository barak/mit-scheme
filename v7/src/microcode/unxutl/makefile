#
# Makefile for MIT CScheme microcode.
#
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/unxutl/Attic/makefile,v 1.18 1992/08/24 13:31:00 jinx Exp $
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

install.world: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} install.world
.PHONY: install.world

# If you have a problem with cc -E here, changing
# the definition of CPP above may fix it.
xmakefile: ymakefile ymake.script ymake.local cf.h s.h m.h
	rm -f xmakefile junk.c
	cp ymakefile junk.c
	$(CPP) junk.c | sed -n -f ymake.script > xmakefile
	rm -f junk.c

clean remove:
	rm -f *.o *.s *.touch xmakefile
	rm -f scheme xscheme bchscheme bchdrn scm6003 kitscheme scmLYN
	rm -f Findprim Bintopsb Psbtobin Breakup Wsize Ppband
	rm -f hard-params limits.h float.h
	rm -f usrdef.c bchdef.c usr6003.c usrkit.c usrLYN.c
	rm -f lint.out

unconfig:
	rm -f m.h s.h cmpint2.h config.status

tags:
	etags *.[ch] ../runtime/*.scm

locked:
	rlog -L -R RCS/*,v

getpagesize.h : pagesize.h
	rm -f getpagesize.h
	cp pagesize.h getpagesize.h
