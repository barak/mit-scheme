### -*- Fundamental -*-
###
###     $Id: makefile,v 1.24 1999/05/11 03:52:25 cph Exp $
###
###     Copyright (c) 1992-1999 Massachusetts Institute of Technology
###
###     This program is free software; you can redistribute it and/or
###     modify it under the terms of the GNU General Public License as
###     published by the Free Software Foundation; either version 2 of
###     the License, or (at your option) any later version.
###
###     This program is distributed in the hope that it will be
###     useful, but WITHOUT ANY WARRANTY; without even the implied
###     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
###     PURPOSE.  See the GNU General Public License for more details.
###
###     You should have received a copy of the GNU General Public
###     License along with this program; if not, write to the Free
###     Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
###     USA.
###

####     Makefile for Scheme on NT
!include <win32.mak>

USER_PRIM_SOURCES =
USER_PRIM_OBJECTS =
USER_LIBS =

# **** Microsoft supplies their assembler as a separate product, and
# **** we don't currently have a copy, so use the Watcom assembler.
# Assembler options.
#
# /bt=nt	build NT binary (default when host is NT)
# /d1		generate debug info
# /fpi		generate in-line 387 insns, emulate if none
# /fp3		generate in-line 387 insns
# /mf		use flat memory model
# /zq		operate quietly
ASFLAGS = /bt=nt /d1 /fpi /fp3 /mf /zq
AS = wasm

!IFDEF NODEBUG
cflags = $(cflags) -Ox
!ELSE
cflags = $(cflags) -Zi -Od
!ENDIF
cflags = $(cflags) $(cvarsmt) /DMIT_SCHEME /DGUI=1 /I.

all: scheme.exe bchschem.exe bintopsb.exe psbtobin.exe

.c.obj:
	$(cc) $(cflags) -c $*.c

.asm.obj:
	$(AS) $(ASFLAGS) $*.asm

CORE_SOURCES = \
artutl.c \
avltree.c \
bignum.c \
bigprm.c \
bitstr.c \
boot.c \
char.c \
cmpauxmd.asm \
cmpint.c \
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
tparam.c \
transact.c \
utils.c \
vector.c \
wind.c

OS_PRIM_SOURCES = \
prbfish.c \
prgdbm.c \
prmd5.c \
prosfile.c \
prosfs.c \
prosio.c \
prosproc.c \
prosterm.c \
prostty.c \
prosenv.c \
prntenv.c \
prntfs.c \
prntio.c \
pruxsock.c

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

NT_SOURCES = \
intext.c \
ntenv.c \
ntfile.c \
ntfs.c \
ntgui.c \
ntio.c \
ntproc.c \
ntscreen.c \
ntsig.c \
ntsock.c \
ntsys.c \
nttop.c \
nttrap.c \
nttterm.c \
nttty.c \
ntasutl.asm

HEAD_FILES = scheme.tch prims.h zones.h locks.h bignum.h \
	$(GC_HEAD_FILES) trap.h lookup.h history.h cmpint.h

GC_HEAD_FILES = gccode.h cmpgc.h ntscmlib.h cmpintmd.h 

CORE_OBJECTS = \
artutl.obj \
avltree.obj \
bignum.obj \
bigprm.obj \
bitstr.obj \
boot.obj \
char.obj \
cmpauxmd.obj \
cmpint.obj \
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
tparam.obj \
transact.obj \
utils.obj \
vector.obj \
wind.obj

OS_PRIM_OBJECTS = \
prbfish.obj \
prgdbm.obj \
prmd5.obj \
prosfile.obj \
prosfs.obj \
prosio.obj \
prosproc.obj \
prosterm.obj \
prosenv.obj \
prostty.obj \
prntenv.obj \
prntfs.obj \
prntio.obj \
pruxsock.obj

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

NT_OBJECTS = \
intext.obj \
ntenv.obj \
ntfile.obj \
ntfs.obj \
ntgui.obj \
ntio.obj \
ntproc.obj \
ntscreen.obj \
ntsig.obj \
ntsock.obj \
ntsys.obj  \
nttop.obj \
nttrap.obj \
nttterm.obj \
nttty.obj \
ntasutl.obj

SOURCES = $(CORE_SOURCES) $(STD_GC_SOURCES)
OBJECTS = $(CORE_OBJECTS) $(STD_GC_OBJECTS) $(NT_OBJECTS) \
	$(OS_PRIM_OBJECTS) usrdef.obj

BCHSOURCES = $(CORE_SOURCES) $(BCH_GC_SOURCES)
BCHOBJECTS = $(CORE_OBJECTS) $(BCH_GC_OBJECTS) $(NT_OBJECTS) \
	$(OS_PRIM_OBJECTS) bchdef.obj

SCHEME_SOURCES = $(USER_PRIM_SOURCES) missing.c
SCHEME_OBJECTS = $(USER_PRIM_OBJECTS) missing.obj
SCHEME_LIB = $(USER_LIBS) md5.lib blowfish.lib gdbm.lib

scheme: scheme.exe

scheme.exe : $(OBJECTS) $(SCHEME_OBJECTS) scheme32.obj ntgui.res
	$(link) $(linkdebug) $(guiflags) /base:0x4000000 /fixed /map \
	-out:scheme.exe $(OBJECTS) $(SCHEME_OBJECTS) scheme32.obj \
	$(guilibsmt) $(SCHEME_LIB) ntgui.res

bchschem.exe : $(BCHOBJECTS) $(SCHEME_OBJECTS) scheme32.obj
	$(link) $(linkdebug) $(guiflags) /base:0x4000000 /fixed /map \
	-out:bchschem.exe $(BCHOBJECTS) $(SCHEME_OBJECTS) scheme32.obj \
	$(guilibsmt) $(SCHEME_LIB) ntgui.res

scheme32.obj : scheme32.c ntscmlib.h

bintopsb.exe : bintopsb.obj missing.obj
	$(link) $(linkdebug) $(conflags) -out:bintopsb.exe \
	bintopsb.obj missing.obj $(conlibs)

psbtobin.exe : psbtobin.obj missing.obj
	$(link) $(linkdebug) $(conflags) -out:psbtobin.exe \
	psbtobin.obj missing.obj $(conlibs)

findprim.exe : findprim.obj
	$(link) $(linkdebug) $(conflags) -out:findprim.exe \
	findprim.obj $(conlibs)

usrdef.c : $(SCHEME_SOURCES) $(SOURCES) $(OS_PRIM_SOURCES) ntgui.c usrdef.tch \
	findprim.exe
	.\findprim $(SCHEME_SOURCES) $(SOURCES) $(OS_PRIM_SOURCES) ntgui.c \
	> usrdef.c

bchdef.c : $(SCHEME_SOURCES) $(BCHSOURCES) $(OS_PRIM_SOURCES) ntgui.c \
	usrdef.tch findprim.exe
	.\findprim $(SCHEME_SOURCES) $(BCHSOURCES) $(OS_PRIM_SOURCES) ntgui.c \
	> bchdef.c

# Update the resource if necessary
ntgui.res: ntgui.rc ntgui.h ntdialog.dlg ntdialog.h
    rc -r -fo $*.res $(cvars) $*.rc

scheme.tch : scheme.h oscond.h ansidecl.h dstack.h obstack.h config.h \
	bkpt.h object.h scode.h sdata.h gc.h interp.h stack.h futures.h \
	types.h errors.h returns.h const.h fixobj.h default.h extern.h \
	prim.h intrpt.h critsec.h float.h outf.h
	wtouch /q /r scheme.tch

psbmap.tch : config.h object.h bignum.h bignmint.h bitstr.h types.h \
	sdata.h const.h psbmap.h $(GC_HEAD_FILES) comlin.h comlin.c
	wtouch /q /r psbmap.tch

usrdef.tch : usrdef.h config.h object.h prim.h
	wtouch /q /r usrdef.tch

foo $(USER_PRIM_OBJECTS) : $(HEAD_FILES)

### files compiled with optimization
interp.obj : scheme.tch locks.h trap.h lookup.h history.h cmpint.h zones.h \
	prmcon.h

ntscreen.obj : ntscreen.c ntgui.h ntscreen.h

gcloop.obj : scheme.tch $(GC_HEAD_FILES)

fasload.obj : scheme.tch prims.h osscheme.h osfile.h osio.h $(GC_HEAD_FILES) \
	trap.h option.h prmcon.h load.c fasl.h

hooks.obj : scheme.tch prims.h winder.h history.h
utils.obj : scheme.tch prims.h winder.h history.h cmpint.h syscall.h ntapi.h
primutl.obj : scheme.tch os.h prims.h usrdef.h prename.h syscall.h ntapi.h \
	avltree.h $(GC_HEAD_FILES)
hunk.obj list.obj step.obj vector.obj : scheme.tch prims.h
sysprim.obj daemon.obj prim.obj extern.obj : scheme.tch prims.h
lookup.obj debug.obj intern.obj : scheme.tch prims.h lookup.h trap.h locks.h
fasdump.obj : scheme.tch prims.h osio.h osfile.h osfs.h $(GC_HEAD_FILES) \
	trap.h lookup.h fasl.h dump.c
memmag.obj : scheme.tch prims.h $(GC_HEAD_FILES) memmag.h
purify.obj : scheme.tch prims.h $(GC_HEAD_FILES) zones.h
wabbit.obj : scheme.tch $(GC_HEAD_FILES)
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
nttterm.obj : scheme.tch prims.h osterm.h
boot.obj : scheme.tch prims.h version.h option.h ostop.h
option.obj : scheme.tch fasl.h osenv.h osfs.h
term.obj : scheme.tch
missing.obj : config.h
BCHGCC_H = bchgcc.h oscond.h $(GC_HEAD_FILES)
bchdmp.obj : bchdmp.c scheme.tch prims.h ntio.h osio.h osfile.h trap.h \
	lookup.h $(BCHGCC_H) fasl.h dump.c
bchdrn.obj : bchdrn.c ansidecl.h bchdrn.h
bchmmg.obj : bchmmg.c scheme.tch prims.h nt.h $(BCHGCC_H) option.h bchdrn.h \
	memmag.h
bchgcl.obj : bchgcl.c scheme.tch $(BCHGCC_H)
bchpur.obj : bchpur.c scheme.tch prims.h $(BCHGCC_H) zones.h
bchutl.obj : bchutl.c ansidecl.h
syntax.obj : syntax.c scheme.tch prims.h edwin.h syntax.h
bitstr.obj : bitstr.c scheme.tch prims.h bitstr.h
regex.obj : regex.c scheme.tch syntax.h regex.h
rgxprim.obj : rgxprim.c scheme.tch prims.h edwin.h syntax.h regex.h
bintopsb.obj : bintopsb.c psbmap.tch trap.h limits.h fasl.h load.c bltdef.h
psbtobin.obj : psbtobin.c psbmap.tch float.h fasl.h dump.c
ppband.obj : ppband.c ansidecl.h config.h errors.h types.h const.h object.h \
	       $(GC_HEAD_FILES) sdata.h load.c fasl.h
outf.obj : outf.c scheme.tch ntscreen.h

fft.obj : fft.c scheme.tch prims.h zones.h array.h image.h
array.obj image.obj : scheme.tch prims.h array.h
cmpint.obj : cmpint.c scheme.tch prim.h $(GC_HEAD_FILES)
osscheme.obj : osscheme.c scheme.tch posixtyp.h os.h osscheme.h
ostty.obj : ostty.c ansidecl.h oscond.h posixtyp.h os.h ostty.h osscheme.h
error.obj ptrvec.obj transact.obj : ansidecl.h dstack.h outf.h
wind.obj : wind.c ansidecl.h dstack.h obstack.h
obstack.obj : obstack.c obstack.h

OS_PRIM_DEPENDENCIES = scheme.tch prims.h posixtyp.h os.h
prbfish.obj : prbfish.c $(OS_PRIM_DEPENDENCIES)
prgdbm.obj : prgdbm.c $(OS_PRIM_DEPENDENCIES)
prmd5.obj : prmd5.c $(OS_PRIM_DEPENDENCIES)
prosenv.obj : prosenv.c osenv.h ostop.h $(OS_PRIM_DEPENDENCIES)
prosfile.obj : prosfile.c osfile.h $(OS_PRIM_DEPENDENCIES)
prosfs.obj : prosfs.c osfs.h $(OS_PRIM_DEPENDENCIES)
prosio.obj : prosio.c osio.h $(OS_PRIM_DEPENDENCIES)
prosproc.obj : prosproc.c osproc.h $(OS_PRIM_DEPENDENCIES)
prosterm.obj : prosterm.c osterm.h osio.h $(OS_PRIM_DEPENDENCIES)
prostty.obj : prostty.c ostty.h osctty.h ossig.h osfile.h osio.h \
	$(OS_PRIM_DEPENDENCIES)
pruxsock.obj : $(OS_PRIM_DEPENDENCIES)
prmcon.obj : prmcon.c scheme.tch prims.h prmcon.h $(OS_PRIM_DEPENDENCIES)

NT_DEPENDENCIES = oscond.h ansidecl.h posixtyp.h intext.h \
		   dstack.h os.h osscheme.h nt.h ntapi.h ntsys.h syscall.h
ntenv.obj : ntenv.c scheme.tch osenv.h ntscreen.h $(NT_DEPENDENCIES)
ntfile.obj : ntfile.c osfile.h osio.h ntio.h $(NT_DEPENDENCIES)
ntfs.obj : ntfs.c ntfs.h osfs.h $(NT_DEPENDENCIES)
ntio.obj : ntio.c osio.h ntio.h ntscreen.h $(NT_DEPENDENCIES)
ntproc.obj : ntproc.c $(NT_DEPENDENCIES) osproc.h ntproc.h osio.h ntio.h \
	ntscreen.h ntgui.h
nttop.obj : nttop.c ostop.h nttop.h osctty.h errors.h option.h \
	$(NT_DEPENDENCIES)
nttty.obj : nttty.c ostty.h osenv.h osio.h ntio.h osterm.h ntterm.h \
	ntscreen.h $(NT_DEPENDENCIES)
ntsig.obj : ntsig.c ossig.h osctty.h ostty.h critsec.h \
	$(NT_DEPENDENCIES) ntgui.h ntio.h ntscmlib.h ntscreen.h
ntsock.obj : ntsock.c ntio.h osio.h uxsock.h $(NT_DEPENDENCIES)
nttrap.obj: nttrap.c nttrap.h ntscmlib.h $(GC_HEAD_FILES) $(NT_DEPENDENCIES)
ntsys.obj: ntsys.c ntsys.h
ntgui.obj : ntgui.c ntdialog.h ntgui.h ntscreen.h $(NT_DEPENDENCIES) scheme.tch
ntasutl.obj : ntasutl.asm
prntenv.obj : prntenv.c $(NT_DEPENDENCIES)
prntfs.obj : prntfs.c ntfs.h $(NT_DEPENDENCIES) scheme.tch prims.h osfs.h
prntio.obj : prntio.c $(NT_DEPENDENCIES) scheme.tch prims.h ntio.h osio.h \
	syscall.h ntscreen.h ntgui.h

cmpauxmd.obj : cmpauxmd.asm

clean:
  -del *.tch
  -del *.obj
  -del *.exe
  -del *.lib
  -del *.dll
  -del *.exp
  -del *.rbj
  -del *.res

unconfig:
  -del cmpauxmd.asm
  -del cmpintmd.h
  -del limits.h
  -del float.h
  -del ntdialog.h
  -del ntw16lib.c
  -del ntw32lib.c
  -del makefile
  -del *.lst
  -del *.bat
  -del *.mak
  -del *.def
  -del *.dlg
  -del *.ico
  -del *.rc
  -del *.cur
