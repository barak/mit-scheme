#
# Makefile for MIT CScheme microcode.
#
# $Id: makefile,v 1.27 1993/06/29 23:12:54 cph Exp $
#
CPP = cc -E
MAKE = make  # BSD doesn't have it as a default.

all: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} all
.PHONY: all

scheme: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} scheme
.PHONY: scheme

xscheme: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} xscheme
.PHONY: xscheme

bchscheme: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} bchscheme
.PHONY: bchscheme

bchdrn: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} bchdrn
.PHONY: bchdrn

psbtobin: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} psbtobin
.PHONY: psbtobin

bintopsb: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} bintopsb
.PHONY: bintopsb

ppband: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} ppband
.PHONY: ppband

hppacach: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} hppacach
.PHONY: hppacach

install: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} install
.PHONY: install

world: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} world
.PHONY: world

install.world: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} install.world
.PHONY: install.world

everything: xmkfile
	$(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} everything
.PHONY: everything

# If you have a problem with cc -E here, changing
# the definition of CPP above may fix it.
xmkfile: ymkfile ymake.sed ymake.lcl ymake.ccl cf.h s.h m.h
	rm -f xmkfile junk.c
	cp ymkfile junk.c
	$(CPP) junk.c | sed -n -f ymake.sed > xmkfile
	rm -f junk.c

ymake.lcl:
	touch ymake.lcl

ymake.ccl:
	echo "COMPILED_SOURCES =" > ymake.ccl

clean remove:
# the - at the beginning of the following line is what causes make to continue 
# even if the clean.world target doesn't exist
	- $(MAKE) $(MAKEOVERRIDES) -f xmkfile ${MFLAGS} clean.world
	rm -f *.o *.s *.tch xmkfile
	rm -f scheme xscheme bchscheme bchdrn
	rm -f findprim bintopsb psbtobin breakup wsize ppband
	rm -f hard-par limits.h float.h
	rm -f usrdef.c bchdef.c compinit.h
	rm -f lint.out

unconfig:
	rm -f m.h s.h cmpintmd.h cmpauxmd.* config.out
	rm -f ymkfile ymake.sed makefile
	- rm -f cf-save.h ymake-save.lcl
	- mv -f cf.h cf-save.h
	- mv -f ymake.lcl ymake-save.lcl

tags:
	etags *.[ch] ../runtime/*.scm

locked:
	rlog -L -R RCS/*,v
	rlog -L -R m/RCS/*,v
	rlog -L -R s/RCS/*,v
	rlog -L -R dosutl/RCS/*,v
	rlog -L -R ntutl/RCS/*,v
	rlog -L -R unxutl/RCS/*,v

getpgsz.h : pagesize.h
	rm -f getpgsz.h
	cp pagesize.h getpgsz.h
