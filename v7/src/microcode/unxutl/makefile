#
# Makefile for MIT CScheme microcode.
#
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/unxutl/Attic/makefile,v 1.9 1990/06/12 17:32:29 jinx Exp $
#
CPP = cc -E
MAKE = make  # BSD doesn't have it as a default.

all: xmakefile doall

doall:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} all

scheme: xmakefile doscheme

doscheme:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} scheme

xscheme: xmakefile doxscheme

doxscheme:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} xscheme

bchscheme: xmakefile dobchscheme

dobchscheme:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} bchscheme

Psbtobin: xmakefile dopsbtobin

dopsbtobin:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Psbtobin

Bintopsb: xmakefile dobintopsb

dobintopsb:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Bintopsb

Ppband: xmakefile doppband

doppband:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Ppband

install: xmakefile doinstall

doinstall:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} install

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

.DEFAULT : xmakefile
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} $@
