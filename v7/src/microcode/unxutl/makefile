#
# Makefile for MIT CScheme microcode.
#
# $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/unxutl/Attic/makefile,v 1.2 1989/07/26 03:55:44 cph Exp $
#
CPP = cc -E
MAKE = make  # BSD doesn't have it as a default.

all: xmakefile doall

doall:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} all

scheme: xmakefile doscheme

doscheme:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} scheme

bchscheme: xmakefile dobchscheme

dobchscheme:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} bchscheme

Psbtobin: xmakefile dopsbtobin

dopsbtobin:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Psbtobin

Bintopsb: xmakefile dobintopsb

dobintopsb:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Bintopsb

Breakup: xmakefile dobreakup

dobreakup:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Breakup

Ppband: xmakefile doppband

doppband:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} Ppband

# If you have a problem with cc -E here, changing
# the definition of CPP above may fix it.
xmakefile: ymakefile cf.h
	rm -f xmakefile
	cp ymakefile junk.c
	$(CPP) junk.c | sed -n -f ymake.script > xmakefile
	rm -f junk.c

clean remove:
	rm -f *.o *.touch *.s xmakefile
	rm -f scheme xscheme bchscheme
	rm -f Findprim Bintopsb Psbtobin Breakup Wsize Ppband
	rm -f usrdef.c
	rm -f lint.out

tags :
	etags *.[ch] ../runtime/*.scm

locked:
	rlog -L -R RCS/*,v

getpagesize.h : pagesize.h
	rm -f getpagesize.h
	cp pagesize.h getpagesize.h
