#
# Makefile for MIT CScheme microcode.
#
# $Id: makefile,v 1.20 1993/02/22 20:31:31 nick Exp $
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

everything: xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} everything
.PHONY: everything

# If you have a problem with cc -E here, changing
# the definition of CPP above may fix it.
xmakefile: ymakefile ymake.script ymake.local cf.h s.h m.h
	rm -f xmakefile junk.c
	cp ymakefile junk.c
	$(CPP) junk.c | sed -n -f ymake.script > xmakefile
	rm -f junk.c

clean remove:
# the - at the beginning of the following line is what causes make to continue 
# even if the clean.world target doesn't exist
	- $(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} clean.world
	rm -f *.o *.s *.touch xmakefile
	rm -f scheme xscheme bchscheme bchdrn
	rm -f Findprim Bintopsb Psbtobin Breakup Wsize Ppband
	rm -f hard-params limits.h float.h
	rm -f usrdef.c bchdef.c
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
