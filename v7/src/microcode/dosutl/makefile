### -*- Fundamental -*-
###
###	$Id: makefile,v 1.11 1994/03/25 01:12:07 gjr Exp $
###
###	Copyright (c) 1992-1993 Massachusetts Institute of Technology
###
###	This material was developed by the Scheme project at the
###	Massachusetts Institute of Technology, Department of
###	Electrical Engineering and Computer Science.  Permission to
###	copy this software, to redistribute it, and to use it for any
###	purpose is granted, subject to the following restrictions and
###	understandings.
###
###	1. Any copy made of this software must include this copyright
###	notice in full.
###
###	2. Users of this software agree to make their best efforts (a)
###	to return to the MIT Scheme project any improvements or
###	extensions that they make, so that these may be included in
###	future releases; and (b) to inform MIT of noteworthy uses of
###	this software.
###
###	3. All materials developed as a consequence of the use of this
###	software shall duly acknowledge such use, in accordance with
###	the usual standards of acknowledging credit in academic
###	research.
###
###	4. MIT has made no warrantee or representation that the
###	operation of this software will be error-free, and MIT is
###	under no obligation to provide any services, by way of
###	maintenance, update, or otherwise.
###
###	5. In conjunction with products arising from the use of this
###	material, there shall be no use of the name of the
###	Massachusetts Institute of Technology nor of any adaptation
###	thereof in any advertising, promotional, or sales literature
###	without prior written consent from MIT in each case.
###

####	 Makefile for Scheme on DOS

TERMCAP_OBJECTS = dostterm.obj tparam.obj
TERMCAP_SOURCES = dostterm.c tparam.c
TERMCAP_LIBS =
GRAPHICS_SOURCES = # dosfg.c
GRAPHICS_OBJECTS = # dosfg.obj
GRAPHICS_LIBS = # fgp.lib
MACHINE_SWITCHES = -3 -mx -DNO_CONST -Di386
MACHINE_SOURCES = cmpint.c cmpauxmd.asm
MACHINE_OBJECTS = cmpint.obj cmpauxmd.obj
GC_HEAD_FILES = gccode.h cmpgc.h cmpintmd.h
USER_PRIM_SOURCES = dosint10.c dosi10.asm dosx32.c
USER_PRIM_OBJECTS = dosint10.obj dosi10.obj dosx32.obj
USER_LIBS =
CC = ztc
M4 = # m4
AS = mlx
LDFLAGS =
CFLAGS = -DMIT_SCHEME $(MACHINE_SWITCHES) -D__STDC__ -Jm -o

all: bintopsb.exe psbtobin.exe scheme.exe bchschem.exe # mswload.exe

.c.obj:
	$(CC) $(CFLAGS) -c $*.c
.c.s:
	$(CC) $(CFLAGS) -S $*.c
.m4.asm:
	$(M4)  -DTYPE_CODE_LENGTH=6 $*.m4 > $*.asm
.asm.obj:
	$(AS) /Zm /Cp /c $*.asm

SCHEME_SOURCES = $(TERMCAP_SOURCES) $(GRAPHICS_SOURCES) $(USER_PRIM_SOURCES) missing.c
SCHEME_OBJECTS = $(TERMCAP_OBJECTS) $(GRAPHICS_OBJECTS) $(USER_PRIM_OBJECTS) missing.obj
SCHEME_LIB = $(USER_LIBS) $(GRAPHICS_LIBS) $(TERMCAP_LIBS) -lm
CORE_SOURCES = \
$(MACHINE_SOURCES) \
artutl.c \
avltree.c \
bignum.c \
bigprm.c \
bitstr.c \
boot.c \
char.c \
comutl.c \
daemon.c \
debug.c \
dfloat.c \
error.c \
extern.c \
fasload.c \
fixnum.c \
flonum.c \
generic.c \
hooks.c \
hunk.c \
intern.c \
interp.c \
intprm.c \
list.c \
lookprm.c \
lookup.c \
obstack.c \
option.c \
osscheme.c \
ostty.c \
outf.c \
prim.c \
primutl.c \
prmcon.c \
ptrvec.c \
purutl.c \
regex.c \
rgxprim.c \
step.c \
storage.c \
string.c \
syntax.c \
sysprim.c \
term.c \
transact.c \
utils.c \
vector.c \
wind.c

STD_GC_SOURCES = \
fasdump.c \
gcloop.c \
memmag.c \
purify.c \
wabbit.c

BCH_GC_SOURCES = \
bchdmp.c \
bchgcl.c \
bchmmg.c \
bchpur.c \
bchutl.c

# DOS
DOS_SOURCES = \
intext.c \
dosfs.c \
dosenv.c \
dosfile.c \
dosio.c \
dosconio.c \
dostty.c \
dostop.c \
dosutil.c \
dossig.c \
dostrap.c \
prdosfs.c \
dossys.c \
doskbd.c \
dosexcp.c \
doskbutl.asm \
dosxcutl.asm \
dosasutl.asm

DOS_OBJECTS = \
intext.obj \
dosfs.obj \
dosenv.obj \
dosfile.obj \
dosio.obj \
dosconio.obj \
dostty.obj \
dostop.obj \
dosutil.obj \
dossig.obj \
dostrap.obj \
dossys.obj \
doskbd.obj \
dosexcp.obj \
doskbutl.obj \
dosxcutl.obj \
dosasutl.obj

OS_PRIM_SOURCES = \
prosfile.c \
prosfs.c \
prosio.c \
prosterm.c \
prostty.c \
prosenv.c \
prdosfs.c \
prdosenv.c
# prosproc.c \
# pruxsock.c

HEAD_FILES = scheme.tch prims.h zones.h locks.h bignum.h \
	$(GC_HEAD_FILES) trap.h lookup.h history.h cmpint.h

CORE_OBJECTS = \
$(MACHINE_OBJECTS) \
artutl.obj \
avltree.obj \
bignum.obj \
bigprm.obj \
bitstr.obj \
boot.obj \
char.obj \
comutl.obj \
daemon.obj \
debug.obj \
dfloat.obj \
error.obj \
extern.obj \
fasload.obj \
fixnum.obj \
flonum.obj \
generic.obj \
hooks.obj \
hunk.obj \
intern.obj \
interp.obj \
intprm.obj \
list.obj \
lookprm.obj \
lookup.obj \
obstack.obj \
option.obj \
osscheme.obj \
ostty.obj \
outf.obj \
prim.obj \
primutl.obj \
prmcon.obj \
ptrvec.obj \
purutl.obj \
regex.obj \
rgxprim.obj \
step.obj \
storage.obj \
string.obj \
syntax.obj \
sysprim.obj \
term.obj \
transact.obj \
utils.obj \
vector.obj \
wind.obj

OS_PRIM_OBJECTS = \
prosfile.obj \
prosfs.obj \
prosio.obj \
prosterm.obj \
prosenv.obj \
prostty.obj \
prdosenv.obj \
prdosfs.obj
# prosproc.obj \
# pruxsock.obj

STD_GC_OBJECTS = \
fasdump.obj \
gcloop.obj \
memmag.obj \
purify.obj \
wabbit.obj

BCH_GC_OBJECTS = \
bchdmp.obj \
bchgcl.obj \
bchmmg.obj \
bchpur.obj \
bchutl.obj

SOURCES = $(CORE_SOURCES) $(STD_GC_SOURCES)
OBJECTS = $(CORE_OBJECTS) $(STD_GC_OBJECTS) $(DOS_OBJECTS) $(OS_PRIM_OBJECTS) usrdef.obj

BCHSOURCES = $(CORE_SOURCES) $(BCH_GC_SOURCES)
BCHOBJECTS = $(CORE_OBJECTS) $(BCH_GC_OBJECTS) $(DOS_OBJECTS) $(OS_PRIM_OBJECTS) bchdef.obj

XTNDRLIB = x32v.lib

scheme.exe : $(OBJECTS) $(SCHEME_OBJECTS) scm-ztc.lst
	$(CC) @scm-ztc.lst $(XTNDRLIB)
bchschem.exe : $(BCHOBJECTS) $(SCHEME_OBJECTS) bch-ztc.lst
	$(CC) @bch-ztc.lst $(XTNDRLIB)
findprim.exe : findprim.obj
	$(CC) findprim.obj $(MACHINE_SWITCHES) $(XTNDRLIB)
bintopsb.exe : bintopsb.obj missing.obj
	$(CC) bintopsb.obj missing.obj $(MACHINE_SWITCHES) $(LDFLAGS) $(XTNDRLIB)
psbtobin.exe : psbtobin.obj missing.obj
	$(CC) psbtobin.obj missing.obj $(MACHINE_SWITCHES) $(LDFLAGS) $(XTNDRLIB)
breakup.exe : breakup.obj
	$(CC) breakup.obj $(MACHINE_SWITCHES) $(LDFLAG)
wsize.exe : wsize.obj
	$(CC) wsize.obj $(MACHINE_SWITCHES) $(LDFLAGS)
ppband.exe : ppband.obj
	$(CC) ppband.obj $(MACHINE_SWITCHES) $(LDFLAGS) $(XTNDRLIB)

usrdef.c : $(SCHEME_SOURCES) $(SOURCES) $(OS_PRIM_SOURCES) dosconio.c usrdef.tch findprim.exe scm-prm.lst
	rm -f usrdef.c
	./findprim -o usrdef.c -l .\scm-prm.lst

bchdef.c : $(SCHEME_SOURCES) $(BCHSOURCES) $(OS_PRIM_SOURCES) usrdef.tch findprim.exe bch-prm.lst
	rm -f bchdef.c
	./findprim -o bchdef.c -l .\bch-prm.lst

primitive_tables :
	rm -f usrdef.c usrdef.obj bchdef.c bchdef.obj

COMMON_OBJECTS = $(CORE_OBJECTS) $(STD_GC_OBJECTS) $(DOS_OBJECTS) $(OS_PRIM_OBJECTS)

scheme.tch : scheme.h oscond.h ansidecl.h dstack.h obstack.h config.h \
	bkpt.h object.h scode.h sdata.h gc.h interp.h stack.h futures.h \
	types.h errors.h returns.h const.h fixobj.h default.h extern.h \
	prim.h intrpt.h critsec.h float.h
	rm -f scheme.tch
	touch scheme.tch

psbmap.tch : config.h object.h bignum.h bignmint.h bitstr.h types.h \
	sdata.h const.h psbmap.h $(GC_HEAD_FILES) comlin.h comlin.c
	rm -f psbmap.tch
	touch psbmap.tch

usrdef.tch : usrdef.h config.h object.h prim.h
	rm -f usrdef.tch
	touch usrdef.tch

# Zortech MAKE seems not to allow redirection
# limits.h : hard-par.exe
#	-./hard-par -l > limits.h

# float.h : hard-par.exe
#	-./hard-par -f > float.h

hard-par.exe : hard-par.c
	$(CC) hard-par.c $(MACHINE_SWITCHES) -DNO_SIG -DNO_SC $(LDFLAGS)

foo $(USER_PRIM_OBJECTS) : $(HEAD_FILES)
interp.obj : scheme.tch locks.h trap.h lookup.h history.h cmpint.h zones.h prmcon.h
hooks.obj : scheme.tch prims.h winder.h history.h
utils.obj : scheme.tch prims.h winder.h history.h cmpint.h syscall.h
primutl.obj : scheme.tch os.h prims.h usrdef.h prename.h syscall.h \
	avltree.h $(GC_HEAD_FILES)
hunk.obj list.obj step.obj vector.obj sysprim.obj daemon.obj prim.obj extern.obj : \
	scheme.tch prims.h
lookup.obj debug.obj intern.obj : scheme.tch prims.h lookup.h trap.h locks.h
fasload.obj : scheme.tch prims.h osscheme.h osfile.h osio.h $(GC_HEAD_FILES) \
	trap.h option.h prmcon.h load.c fasl.h
fasdump.obj : scheme.tch prims.h osio.h osfile.h osfs.h $(GC_HEAD_FILES) \
	trap.h lookup.h fasl.h dump.c
memmag.obj : scheme.tch prims.h $(GC_HEAD_FILES) memmag.h
gcloop.obj : scheme.tch $(GC_HEAD_FILES)
purify.obj : scheme.tch prims.h $(GC_HEAD_FILES) zones.h
wabbit.obj : scheme.tch $(GC_HEAD_FILES) zones.h
purutl.obj : scheme.tch prims.h $(GC_HEAD_FILES) zones.h
comutl.obj : scheme.tch prims.h
artutl.obj : scheme.tch
avltree.obj : ansidecl.h avltree.h
bignum.obj : scheme.tch bignmint.h limits.h
bigprm.obj flonum.obj intprm.obj : scheme.tch prims.h zones.h
generic.obj : scheme.tch prims.h
fixnum.obj : scheme.tch prims.h mul.c
storage.obj : scheme.tch gctype.c
char.obj string.obj dfloat.obj : scheme.tch prims.h
dostterm.obj : scheme.tch prims.h osterm.h
boot.obj : scheme.tch prims.h version.h option.h ostop.h
option.obj : ansidecl.h option.h
term.obj : scheme.tch
missing.obj : config.h
BCHGCC_H = bchgcc.h oscond.h $(GC_HEAD_FILES)
bchdmp.obj : scheme.tch prims.h dosio.h osio.h osfile.h trap.h lookup.h \
	$(BCHGCC_H) fasl.h dump.c
bchdrn.obj : ansidecl.h bchdrn.h
bchmmg.obj : scheme.tch prims.h msdos.h $(BCHGCC_H) option.h bchdrn.h memmag.h
bchgcl.obj : scheme.tch $(BCHGCC_H)
bchpur.obj : scheme.tch prims.h $(BCHGCC_H) zones.h
bchutl.obj : ansidecl.h
syntax.obj : scheme.tch prims.h edwin.h syntax.h
bitstr.obj : scheme.tch prims.h bitstr.h
regex.obj : scheme.tch syntax.h regex.h
rgxprim.obj : scheme.tch prims.h edwin.h syntax.h regex.h
bintopsb.obj : psbmap.tch trap.h limits.h fasl.h load.c bltdef.h
psbtobin.obj : psbmap.tch float.h fasl.h dump.c
ppband.obj : ansidecl.h config.h errors.h types.h const.h object.h \
	       $(GC_HEAD_FILES) sdata.h load.c fasl.h

fft.obj : scheme.tch prims.h zones.h array.h image.h
array.obj image.obj : scheme.tch prims.h array.h
cmpint.obj : scheme.tch prim.h $(GC_HEAD_FILES)
osscheme.obj : scheme.tch posixtyp.h os.h osscheme.h
ostty.obj : ansidecl.h oscond.h posixtyp.h os.h ostty.h osscheme.h
error.obj ptrvec.obj transact.obj : ansidecl.h dstack.h
wind.obj : ansidecl.h dstack.h obstack.h
obstack.obj : obstack.h

OS_PRIM_DEPENDENCIES = scheme.tch prims.h posixtyp.h os.h
prosenv.obj : osenv.h ostop.h $(OS_PRIM_DEPENDENCIES)
prosfile.obj : osfile.h $(OS_PRIM_DEPENDENCIES)
prosfs.obj : osfs.h $(OS_PRIM_DEPENDENCIES)
prosio.obj : osio.h $(OS_PRIM_DEPENDENCIES)
prosproc.obj : osproc.h $(OS_PRIM_DEPENDENCIES)
prosterm.obj : osterm.h osio.h $(OS_PRIM_DEPENDENCIES)
prostty.obj : ostty.h osctty.h ossig.h osfile.h osio.h $(OS_PRIM_DEPENDENCIES)
prmcon.obj : scheme.tch prims.h prmcon.h $(OS_PRIM_DEPENDENCIES)

DOS_DEPENDENCIES = oscond.h ansidecl.h posixtyp.h intext.h \
		   dstack.h os.h osscheme.h msdos.h dossys.h syscall.h
dosenv.obj : osenv.h $(DOS_DEPENDENCIES)
dosfile.obj : osfile.h osio.h dosio.h $(DOS_DEPENDENCIES)
dosfs.obj : osfs.h $(DOS_DEPENDENCIES)
dosio.obj : osio.h dosio.h $(DOS_DEPENDENCIES) 
dosconio.obj : scheme.tch prims.h dosscan.h osio.h dosio.h $(DOS_DEPENDENCIES)
dostop.obj : ostop.h dostop.h osctty.h dosutil.h errors.h option.h $(DOS_DEPENDENCIES)
dostty.obj : ostty.h osenv.h osio.h dosio.h osterm.h dosterm.h $(DOS_DEPENDENCIES)
dosutil.obj : dosutil.h $(DOS_DEPENDENCIES)
dossig.obj : ossig.h osctty.h ostty.h critsec.h dosexcp.h doskbd.h $(DOS_DEPENDENCIES)
dostrap.obj: dostrap.h dosexcp.h $(DOS_DEPENDENCIES)
dossys.obj: dossys.h
dosexcp.obj : dosexcp.h dossys.h dosinsn.h
doskbd.obj : doskbd.h dossys.h dosinsn.h
dosasutl.obj : dosasutl.asm
doskbutl.obj : doskbutl.asm
dosxcutl.obj : dosxcutl.asm
dosint10.obj: dosint10.c dosint10.h
dosi10.obj: dosi10.asm
dosx32.obj: dosx32.c
prdosenv.obj : $(DOS_DEPENDENCIES)
prdosfs.obj : $(DOS_DEPENDENCIES) scheme.h prims.h osfs.h
cmpauxmd.obj : cmpauxmd.asm

clean:
  del *.tch
  del *.obj
  del *.exe

unconfig:
  del cmpintmd.h
  del cmpauxmd.asm
  del float.h
  del limits.h
  del makefile
  del *.lst


