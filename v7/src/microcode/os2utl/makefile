### -*- Fundamental -*-
###
###	$Id: makefile,v 1.2 1994/12/02 20:44:01 cph Exp $
###
###	Copyright (c) 1994 Massachusetts Institute of Technology
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

####	 Makefile for Scheme under OS/2

# This makefile is configured for use with GNU make, the IBM C Set++/2
# compiler, and the GNU assembler included with the gcc/2 package.
# Unfortunately, the output from the GNU assembler requires two .lib
# files from gcc/2 as well; these files are referenced here using
# absolute pathnames.

# The makefile can also be configured to compile Scheme using gcc/2.
# The changes required are to comment out the first block of
# definitions starting with "CC = icc", and to uncomment the
# definitions starting with "CC = gcc ...".  Then, change all the
# instances of "/Fe" to "-o ".  At the present time, this program
# doesn't work when compiled with gcc/2, but that is only because it
# depends on two 16-bit API calls that gcc/2 is unable to generate
# calling sequences for.  This can be fixed with a tiny bit of
# assembly language, or even a simple interface library precompiled
# with icc.

OBJ = obj # obj for IBM C and GCC/2

MACHINE_SOURCES = cmpint.c cmpauxmd.asm
MACHINE_OBJECTS = cmpint.$(OBJ) cmpauxmd.$(OBJ)
GC_HEAD_FILES = gccode.h cmpgc.h cmpintmd.h
USER_PRIM_SOURCES =
USER_PRIM_OBJECTS =
USER_LIBS =

CC = icc
ICC_FLAGS = /Gd+ /Gm+ /Q+
DEBUG_CFLAGS = /Ti+ /O- /Oi- /Debug
OPTIMIZE_CFLAGS = /O
CFLAGS = -DMIT_SCHEME -D__STDC__ $(DEBUG_CFLAGS) $(ICC_FLAGS)
DEBUG_LDFLAGS = $(DEBUG_CFLAGS)
OPTIMIZE_LDFLAGS =
LDFLAGS = $(DEBUG_LDFLAGS) $(ICC_FLAGS) /B"/PM:pm"

.SUFFIXES:
.SUFFIXES: .c .obj
.c.obj:
	$(CC) $(CFLAGS) /C $<

#CC = gcc -Uunix -U__unix__ -U__unix -D__GCC2__
#CFLAGS = -g -DMIT_SCHEME
#LDFLAGS = -g
#
#.SUFFIXES:
#.SUFFIXES: .c .obj
#.c.obj:
#	$(CC) $(CFLAGS) -c $<

AS = as
ASFLAGS = -I

cmpauxmd.$(OBJ) : cmpauxmd.asm
	$(AS) $(ASFLAGS) -o cmpauxmd.$(OBJ) cmpauxmd.asm

SCHEME_SOURCES = $(USER_PRIM_SOURCES) missing.c
SCHEME_OBJECTS = $(USER_PRIM_OBJECTS) missing.$(OBJ)
SCHEME_LIB = $(USER_LIBS) d:\local\prog\gcc2\lib\libcrt.lib d:\local\prog\gcc2\lib\gcc.lib # -lpmio -lstdwin -Xlinker -pm

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

CORE_OBJECTS = \
$(MACHINE_OBJECTS) \
artutl.$(OBJ) \
avltree.$(OBJ) \
bignum.$(OBJ) \
bigprm.$(OBJ) \
bitstr.$(OBJ) \
boot.$(OBJ) \
char.$(OBJ) \
comutl.$(OBJ) \
daemon.$(OBJ) \
debug.$(OBJ) \
dfloat.$(OBJ) \
error.$(OBJ) \
extern.$(OBJ) \
fasload.$(OBJ) \
fixnum.$(OBJ) \
flonum.$(OBJ) \
generic.$(OBJ) \
hooks.$(OBJ) \
hunk.$(OBJ) \
intern.$(OBJ) \
interp.$(OBJ) \
intprm.$(OBJ) \
list.$(OBJ) \
lookprm.$(OBJ) \
lookup.$(OBJ) \
obstack.$(OBJ) \
option.$(OBJ) \
osscheme.$(OBJ) \
ostty.$(OBJ) \
outf.$(OBJ) \
prim.$(OBJ) \
primutl.$(OBJ) \
prmcon.$(OBJ) \
ptrvec.$(OBJ) \
purutl.$(OBJ) \
regex.$(OBJ) \
rgxprim.$(OBJ) \
step.$(OBJ) \
storage.$(OBJ) \
string.$(OBJ) \
syntax.$(OBJ) \
sysprim.$(OBJ) \
term.$(OBJ) \
transact.$(OBJ) \
utils.$(OBJ) \
vector.$(OBJ) \
wind.$(OBJ)

STD_GC_SOURCES = \
fasdump.c \
gcloop.c \
memmag.c \
purify.c \
wabbit.c

STD_GC_OBJECTS = \
fasdump.$(OBJ) \
gcloop.$(OBJ) \
memmag.$(OBJ) \
purify.$(OBJ) \
wabbit.$(OBJ)

BCH_GC_SOURCES = \
bchdmp.c \
bchgcl.c \
bchmmg.c \
bchpur.c \
bchutl.c

BCH_GC_OBJECTS = \
bchdmp.$(OBJ) \
bchgcl.$(OBJ) \
bchmmg.$(OBJ) \
bchpur.$(OBJ) \
bchutl.$(OBJ)

OS_PRIM_SOURCES = \
prosenv.c \
prosfile.c \
prosfs.c \
prosio.c \
prosterm.c \
prostty.c \
pros2fs.c
# prosproc.c
# prospty.c

OS_PRIM_OBJECTS = \
prosenv.$(OBJ) \
prosfile.$(OBJ) \
prosfs.$(OBJ) \
prosio.$(OBJ) \
prosterm.$(OBJ) \
prostty.$(OBJ) \
pros2fs.$(OBJ)
#prosproc.$(OBJ)
#prospty.$(OBJ)

OS2_SOURCES = \
os2.c \
os2api.c \
os2conio.c \
os2cthrd.c \
os2ctty.c \
os2env.c \
os2file.c \
os2fs.c \
os2io.c \
os2msg.c \
os2pipe.c \
os2pm.c \
os2pmcon.c \
os2term.c \
os2thrd.c \
os2top.c \
os2tty.c

OS2_OBJECTS = \
os2.$(OBJ) \
os2api.$(OBJ) \
os2conio.$(OBJ) \
os2cthrd.$(OBJ) \
os2ctty.$(OBJ) \
os2env.$(OBJ) \
os2file.$(OBJ) \
os2fs.$(OBJ) \
os2io.$(OBJ) \
os2msg.$(OBJ) \
os2pipe.$(OBJ) \
os2pm.$(OBJ) \
os2pmcon.$(OBJ) \
os2term.$(OBJ) \
os2thrd.$(OBJ) \
os2top.$(OBJ) \
os2tty.$(OBJ)

HEAD_FILES = scheme.tch prims.h zones.h locks.h bignum.h \
	$(GC_HEAD_FILES) trap.h lookup.h history.h cmpint.h

SOURCES = $(CORE_SOURCES) $(STD_GC_SOURCES)
OBJECTS = $(CORE_OBJECTS) $(STD_GC_OBJECTS) $(OS2_OBJECTS) \
	$(OS_PRIM_OBJECTS) usrdef.$(OBJ)

BCHSOURCES = $(CORE_SOURCES) $(BCH_GC_SOURCES)
BCHOBJECTS = $(CORE_OBJECTS) $(BCH_GC_OBJECTS) $(OS2_OBJECTS) \
	$(OS_PRIM_OBJECTS) bchdef.$(OBJ)

all: bintopsb.exe psbtobin.exe scheme.exe bchschem.exe

clean:
	rm -f *.$(OBJ) *.exe *.tch usrdef.c bchdef.c

scheme.exe xscheme.exe : $(OBJECTS) $(SCHEME_OBJECTS)
	$(CC) $(LDFLAGS) /Fe$@ $^ $(SCHEME_LIB)

bchschem.exe : $(BCHOBJECTS) $(SCHEME_OBJECTS)
	$(CC) $(LDFLAGS) /Fe$@ $^ $(SCHEME_LIB)

findprim.exe : findprim.$(OBJ)
	$(CC) $(LDFLAGS) /Fe$@ $^

bintopsb.exe : bintopsb.$(OBJ) missing.$(OBJ)
	$(CC) $(LDFLAGS) /Fe$@ $^

psbtobin.exe : psbtobin.$(OBJ) missing.$(OBJ)
	$(CC) $(LDFLAGS) /Fe$@ $^

breakup.exe : breakup.$(OBJ)
	$(CC) $(LDFLAGS) /Fe$@ $^

wsize.exe : wsize.$(OBJ)
	$(CC) $(LDFLAGS) /Fe$@ $^

ppband.exe : ppband.$(OBJ)
	$(CC) $(LDFLAGS) /Fe$@ $^

usrdef.c : $(SCHEME_SOURCES) $(SOURCES) $(OS_PRIM_SOURCES) usrdef.tch \
	findprim.exe
	.\findprim $(SCHEME_SOURCES) $(SOURCES) $(OS_PRIM_SOURCES) > usrdef.c

bchdef.c : $(SCHEME_SOURCES) $(BCHSOURCES) $(OS_PRIM_SOURCES) usrdef.tch \
	findprim xmkfile
	.\findprim $(SCHEME_SOURCES) $(BCHSOURCES) $(OS_PRIM_SOURCES) \
	> bchdef.c

COMMON_OBJECTS = $(CORE_OBJECTS) $(STD_GC_OBJECTS) $(OS2_OBJECTS) \
	$(OS_PRIM_OBJECTS)

scheme.tch : scheme.h oscond.h ansidecl.h dstack.h obstack.h config.h \
	bkpt.h object.h scode.h sdata.h gc.h interp.h stack.h futures.h \
	types.h errors.h returns.h const.h fixobj.h default.h extern.h \
	prim.h intrpt.h critsec.h outf.h
	rm -f $@
	touch $@

psbmap.tch : config.h object.h bignum.h bignmint.h bitstr.h types.h \
	sdata.h const.h psbmap.h $(GC_HEAD_FILES) comlin.h comlin.c
	rm -f $@
	touch $@

usrdef.tch : usrdef.h config.h object.h prim.h
	rm -f $@
	touch $@

foo $(USER_PRIM_OBJECTS) : $(HEAD_FILES)

interp.$(OBJ) : scheme.tch locks.h trap.h lookup.h history.h cmpint.h zones.h \
	prmcon.h

hooks.$(OBJ) : scheme.tch prims.h winder.h history.h

utils.$(OBJ) : scheme.tch prims.h winder.h history.h cmpint.h syscall.h

primutl.$(OBJ) : scheme.tch os.h prims.h usrdef.h prename.h syscall.h \
	avltree.h $(GC_HEAD_FILES)

hunk.$(OBJ) list.$(OBJ) step.$(OBJ) vector.$(OBJ) : scheme.tch prims.h
sysprim.$(OBJ) daemon.$(OBJ) prim.$(OBJ) extern.$(OBJ) : scheme.tch prims.h

lookup.$(OBJ) lookprm.$(OBJ) debug.$(OBJ) intern.$(OBJ) : scheme.tch prims.h \
	lookup.h trap.h locks.h
fasload.$(OBJ) : scheme.tch prims.h osscheme.h osfile.h osio.h \
	$(GC_HEAD_FILES) trap.h option.h prmcon.h load.c fasl.h
fasdump.$(OBJ) : scheme.tch prims.h osio.h osfile.h osfs.h $(GC_HEAD_FILES) \
	trap.h lookup.h fasl.h dump.c
memmag.$(OBJ) : scheme.tch prims.h memmag.h $(GC_HEAD_FILES) memmag.h
gcloop.$(OBJ) : scheme.tch $(GC_HEAD_FILES)
purify.$(OBJ) : scheme.tch prims.h $(GC_HEAD_FILES) zones.h
wabbit.$(OBJ) : scheme.tch $(GC_HEAD_FILES)
purutl.$(OBJ) : scheme.tch prims.h $(GC_HEAD_FILES) zones.h
comutl.$(OBJ) : scheme.tch prims.h
gctype.$(OBJ) : config.h
artutl.$(OBJ) : scheme.tch
avltree.$(OBJ) : ansidecl.h avltree.h
bignum.$(OBJ) : scheme.tch bignmint.h
bigprm.$(OBJ) flonum.$(OBJ) intprm.$(OBJ) : scheme.tch prims.h zones.h
generic.$(OBJ) : scheme.tch prims.h
fixnum.$(OBJ) : scheme.tch prims.h mul.c
storage.$(OBJ) : scheme.tch gctype.c
char.$(OBJ) string.$(OBJ) dfloat.$(OBJ) : scheme.tch prims.h
tterm.$(OBJ) : scheme.tch prims.h osterm.h
boot.$(OBJ) : scheme.tch prims.h version.h option.h ostop.h os.h
option.o : ansidecl.h obstack.h config.h osenv.h
term.$(OBJ) : scheme.tch
missing.$(OBJ) : config.h
BCHGCC_H = bchgcc.h oscond.h $(GC_HEAD_FILES)
bchdmp.$(OBJ) : scheme.tch prims.h os2.h osio.h osfile.h trap.h lookup.h \
	$(BCHGCC_H) fasl.h dump.c
bchdrn.$(OBJ) : ansidecl.h bchdrn.h
bchmmg.$(OBJ) : scheme.tch prims.h os2.h $(BCHGCC_H) option.h bchdrn.h memmag.h
bchgcl.$(OBJ) : scheme.tch $(BCHGCC_H)
bchpur.$(OBJ) : scheme.tch prims.h $(BCHGCC_H) zones.h
bchutl.$(OBJ) : ansidecl.h
syntax.$(OBJ) : scheme.tch prims.h edwin.h syntax.h
bitstr.$(OBJ) : scheme.tch prims.h bitstr.h
regex.$(OBJ) : scheme.tch syntax.h regex.h
rgxprim.$(OBJ) : scheme.tch prims.h edwin.h syntax.h regex.h
bintopsb.$(OBJ) : psbmap.tch trap.h fasl.h load.c bltdef.h
psbtobin.$(OBJ) : psbmap.tch fasl.h dump.c
ppband.$(OBJ) : ansidecl.h config.h errors.h types.h const.h object.h \
	$(GC_HEAD_FILES) sdata.h load.c fasl.h
wsize.$(OBJ) : config.h
cmpint.$(OBJ) : scheme.tch prim.h $(GC_HEAD_FILES)
osscheme.$(OBJ) : scheme.tch posixtyp.h os.h osscheme.h
ostty.$(OBJ) : ansidecl.h oscond.h posixtyp.h os.h ostty.h osscheme.h
error.$(OBJ) ptrvec.$(OBJ) transact.$(OBJ) : ansidecl.h dstack.h outf.h
wind.$(OBJ) : ansidecl.h dstack.h obstack.h
obstack.$(OBJ) : obstack.h
$(OS_PRIM_OBJECTS) : scheme.tch prims.h posixtyp.h os.h
prosenv.$(OBJ) : osenv.h ostop.h
prosfile.$(OBJ) : osfile.h
prosfs.$(OBJ) : osfs.h
prosio.$(OBJ) : osio.h
#prosproc.$(OBJ) : osproc.h
#prospty.$(OBJ) : osterm.h osio.h ospty.h
prosterm.$(OBJ) : osterm.h osio.h
prostty.$(OBJ) : ostty.h osctty.h osfile.h osio.h
prmcon.$(OBJ) : scheme.tch prims.h prmcon.h

$(OS2_OBJECTS) : dstack.h osscheme.h outf.h os.h ansidecl.h oscond.h \
	posixtyp.h syscall.h osio.h os2.h os2api.h os2cthrd.h os2ctty.h \
	os2io.h os2msg.h os2pm.h os2thrd.h
os2ctty.$(OBJ) : osctty.h ossig.h
os2env.$(OBJ) : osenv.h scheme.tch
os2file.$(OBJ) : osfile.h
os2fs.$(OBJ) : osfs.h
os2top.$(OBJ) : ostop.h option.h
os2tty.$(OBJ) : ostty.h
