### -*- Fundamental -*-
###
###	$Id: makefile,v 1.7 1993/08/21 02:31:23 gjr Exp $
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

####	 Makefile for Scheme on NT
!include <ntwin32.mak>
#cflags = $(cflags) -DDEVL=1 -D_X86_=1

TERMCAP_OBJECTS = nttterm.obj tparam.obj
TERMCAP_SOURCES = nttterm.c tparam.c
TERMCAP_LIBS =
GRAPHICS_SOURCES =
GRAPHICS_OBJECTS =
GRAPHICS_LIBS =
#MACHINE_SWITCHES = -3 -mx -DNO_CONST -Di386
MACHINE_SWITCHES = -DNO_CONST -Di386
MACHINE_SOURCES = cmpint.c cmpauxmd.asm
MACHINE_OBJECTS = cmpint.obj cmpauxmd.obj
GC_HEAD_FILES = gccode.h cmpgc.h ntscmlib.h cmpintmd.h 
USER_PRIM_SOURCES =
USER_PRIM_OBJECTS =
USER_LIBS =
BINDIR = \scheme\nt\microcode
AS = mlx /Zm /Cp /c # masm386 /z # 
LDFLAGS =
cflags = $(cflags) -DMIT_SCHEME -DWINNT -DCL386 $(MACHINE_SWITCHES) -DGUI=1

# NT compilation system for scheme (NOT utilities)
#sysflags = $(conflags)
sysflags = $(guiflags)
syslibs = $(guilibs)
cvobj = cvtomf

all: ntscmlib.dll scheme.exe # bchschem.exe bintopsb.exe psbtobin.exe

.c.obj:
	$(cc) $(cflags) $(cdebug) -c $*.c
#	$(cc) $(cflags) -Zi -Ox -c $*.c
#	$(cvobj) $*.obj

#.c.s:
#	$(CC) $(CFLAGS) -S $*.c
# .m4.asm:
# 	$(M4)  -DTYPE_CODE_LENGTH=6 $*.m4 > $*.asm
#	$(AS) $*.asm, $*.obj, nul.lst, nul.crf
.asm.obj:
	$(AS) $*.asm

SCHEME_SOURCES = $(TERMCAP_SOURCES) $(GRAPHICS_SOURCES) $(USER_PRIM_SOURCES) missing.c
SCHEME_OBJECTS = $(TERMCAP_OBJECTS) $(GRAPHICS_OBJECTS) $(USER_PRIM_OBJECTS) missing.obj
SCHEME_LIB = $(USER_LIBS) $(GRAPHICS_LIBS) $(TERMCAP_LIBS) -lm
CORE_SOURCES = \
$(MACHINE_SOURCES) \
artutl.c \
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
purify.c
BCH_GC_SOURCES = \
bchdmp.c \
bchgcl.c \
bchmmg.c \
bchpur.c \
bchutl.c
# nt
NT_SOURCES = \
intext.c \
ntfs.c \
ntenv.c \
ntfile.c \
ntgui.c \
ntio.c \
nttty.c \
nttop.c \
ntutil.c \
ntsig.c \
nttrap.c \
ntsys.c \
ntscreen.c \
ntasutl.asm

NT_OBJECTS = \
intext.obj \
ntfs.obj \
ntenv.obj \
ntfile.obj \
ntgui.obj \
ntgui.rbj \
ntio.obj \
nttty.obj \
nttop.obj \
ntutil.obj \
ntsig.obj \
nttrap.obj \
ntsys.obj  \
ntscreen.obj \
ntasutl.obj

OS_PRIM_SOURCES = \
prosfile.c \
prosfs.c \
prosio.c \
prosterm.c \
prostty.c \
prosenv.c \
prntfs.c \
prntenv.c

HEAD_FILES = scheme.tch prims.h zones.h locks.h bignum.h \
	$(GC_HEAD_FILES) trap.h lookup.h history.h cmpint.h

CORE_OBJECTS = \
$(MACHINE_OBJECTS) \
artutl.obj \
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
prntenv.obj \
prntfs.obj

STD_GC_OBJECTS = \
fasdump.obj \
gcloop.obj \
memmag.obj \
purify.obj
BCH_GC_OBJECTS = \
bchdmp.obj \
bchgcl.obj \
bchmmg.obj \
bchpur.obj \
bchutl.obj

SOURCES = $(CORE_SOURCES) $(STD_GC_SOURCES)
OBJECTS = $(CORE_OBJECTS) $(STD_GC_OBJECTS) $(NT_OBJECTS) $(OS_PRIM_OBJECTS) usrdef.obj

BCHSOURCES = $(CORE_SOURCES) $(BCH_GC_SOURCES)
BCHOBJECTS = $(CORE_OBJECTS) $(BCH_GC_OBJECTS) $(NT_OBJECTS) $(OS_PRIM_OBJECTS) bchdef.obj

# Update the resource if necessary
ntgui.rbj: ntgui.rc ntgui.h ntdialog.dlg ntdialog.h
    rc -r -fo $*.res $(cvars) $*.rc
!IFDEF CPUTYPE
    cvtres -$(CPU) $*.res -o $*.rbj
!ENDIF

scheme: scheme.exe
scheme.exe : $(OBJECTS) $(SCHEME_OBJECTS) scm-cl3.lst
	$(link) $(linkdebug) $(sysflags) -out:scheme.exe @scm-cl3.lst $(syslibs) ntscmlib.lib crtdll.lib
bchschem.exe : $(BCHOBJECTS) $(SCHEME_OBJECTS) bch-cl3.lst
	$(link) $(linkdebug) $(sysflags) -out:bchschem.exe @bch-cl3.lst $(syslibs) ntscmlib.lib crtdll.lib
findprim.exe : findprim.obj
	$(link) $(linkdebug) $(conflags) -out:findprim.exe findprim.obj $(conlibs)
bintopsb.exe : bintopsb.obj missing.obj
	$(link) $(linkdebug) $(conflags) -out:bintopsb.exe bintopsb.obj missing.obj $(conlibs)
psbtobin.exe : psbtobin.obj missing.obj
	$(link) $(linkdebug) $(conflags) -out:psbtobin.exe psbtobin.obj missing.obj $(conlibs)
breakup.exe : breakup.obj
	$(link) $(linkdebug) $(conflags) -out:breakup.exe breakup $(conlibs)
wsize.exe : wsize.obj
	$(link) $(linkdebug) $(conflags) -out:wsize.exe wsize.obj $(conlibs)
ppband.exe : ppband.obj
	$(link) $(linkdebug) $(conflags) -out:ppband.exe ppband.obj $(conlibs)

usrdef.c : $(SCHEME_SOURCES) $(SOURCES) $(OS_PRIM_SOURCES) ntgui.c usrdef.tch findprim.exe scm-p-nt.lst
	rm -f usrdef.c
	.\findprim -o usrdef.c -l .\scm-p-nt.lst

bchdef.c : $(SCHEME_SOURCES) $(BCHSOURCES) $(OS_PRIM_SOURCES) usrdef.tch findprim.exe bch-p-nt.lst
	rm -f bchdef.c
	.\findprim -o bchdef.c -l .\bch-p-nt.lst

primitive_tables :
	rm -f usrdef.c usrdef.obj bchdef.c bchdef.obj

COMMON_OBJECTS = $(CORE_OBJECTS) $(STD_GC_OBJECTS) $(NT_OBJECTS) $(OS_PRIM_OBJECTS)
6003_SOURCES = $(X_SOURCES) $(TERMCAP_OBJECTS) $(GRAPHICS_SOURCES)\
	  Sgraph_ar.c fft.c array.c\
	      image.c
6003_OBJECTS = $(X_OBJECTS) $(TERMCAP_OBJECTS) $(GRAPHICS_OBJECTS)\
	  Sgraph_ar.obj fft.obj array.obj\
	      image.obj usr6003.obj
6003_LIB =  $(USER_LIBS) $(GRAPHICS_LIBS) $(X_LIB) $(TERMCAP_LIBS)\
	-lm

scheme.tch : scheme.h oscond.h ansidecl.h dstack.h obstack.h config.h \
	bkpt.h object.h scode.h sdata.h gc.h interp.h stack.h futures.h \
	types.h errors.h returns.h const.h fixobj.h default.h extern.h \
	prim.h intrpt.h critsec.h float.h outf.h
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
	$(cc) $(cflags) $(cdebug) -Ox -c $*.c
#	$(cvobj) $*.obj

hooks.obj : scheme.tch prims.h winder.h history.h
utils.obj : scheme.tch prims.h winder.h history.h cmpint.h syscall.h
primutl.obj : scheme.tch prims.h prename.h syscall.h
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
purutl.obj : scheme.tch prims.h $(GC_HEAD_FILES) zones.h
comutl.obj : scheme.tch prims.h
artutl.obj : scheme.tch
bignum.obj : scheme.tch bignmint.h limits.h
bigprm.obj flonum.obj intprm.obj : scheme.tch prims.h zones.h
generic.obj : scheme.tch prims.h
fixnum.obj : scheme.tch prims.h mul.c
storage.obj : scheme.tch gctype.c
char.obj string.obj dfloat.obj : scheme.tch prims.h
nttterm.obj : scheme.tch prims.h osterm.h
boot.obj : scheme.tch prims.h version.h option.h ostop.h
option.obj : ansidecl.h option.h
term.obj : scheme.tch
missing.obj : config.h
BCHGCC_H = bchgcc.h oscond.h $(GC_HEAD_FILES)
bchdmp.obj : scheme.tch prims.h ntio.h osio.h osfile.h trap.h lookup.h \
	$(BCHGCC_H) fasl.h dump.c
bchdrn.obj : ansidecl.h bchdrn.h
bchmmg.obj : scheme.tch prims.h nt.h $(BCHGCC_H) option.h bchdrn.h memmag.h
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
error.obj ptrvec.obj transact.obj : ansidecl.h dstack.h outf.h
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

NT_DEPENDENCIES = oscond.h ansidecl.h posixtyp.h intext.h \
		   dstack.h os.h osscheme.h nt.h ntsys.h syscall.h
ntenv.obj : osenv.h $(NT_DEPENDENCIES)
ntfile.obj : osfile.h osio.h ntio.h $(NT_DEPENDENCIES)
ntfs.obj : osfs.h $(NT_DEPENDENCIES)
ntio.obj : osio.h ntio.h $(NT_DEPENDENCIES)
nttop.obj : ostop.h nttop.h osctty.h ntutil.h errors.h option.h $(NT_DEPENDENCIES)
nttty.obj : ostty.h osenv.h osio.h ntio.h osterm.h ntterm.h $(NT_DEPENDENCIES)
ntutil.obj : ntutil.h $(NT_DEPENDENCIES)
ntsig.obj : ossig.h osctty.h ostty.h critsec.h $(NT_DEPENDENCIES) ntscmlib.h
nttrap.obj: nttrap.h ntscmlib.h $(GC_HEAD_FILES) $(NT_DEPENDENCIES)
ntsys.obj: ntsys.h
ntgui.obj : ntgui.c ntdialog.h ntscreen.h $(NT_DEPENDENCIES) scheme.tch
ntasutl.obj : ntasutl.asm
ntkbutl.obj : ntkbutl.asm
ntscreen.obj : ntscreen.c ntscreen.h
prntenv.obj : $(NT_DEPENDENCIES)
prntfs.obj : $(NT_DEPENDENCIES) scheme.h prims.h osfs.h
cmpauxmd.obj : cmpauxmd.asm

ntscmlib.dll: ntwntlib.dll ntw32lib.dll
	copy ntwntlib.lib ntscmlib.lib
	copy ntwntlib.dll ntscmlib.dll

ntwntlib.obj: ntwntlib.c ntscmlib.h makefile

ntwntlib.exp: ntwntlib.obj ntscmlib.def
	$(implib) -machine:$(CPU) -def:ntscmlib.def $*.obj -out:$*.lib
	
ntwntlib.dll: ntwntlib.obj ntwntlib.exp
	$(link) $(linkdebug) -dll -out:$*.dll \
		-entry:_CRT_INIT$(DLLENTRY) \
		$** $(syslibs) winmm.lib crtdll.lib

ntw32lib.obj : ntw32lib.c ntscmlib.h makefile

ntw32lib.exp: ntw32lib.obj ntscmlib.def
	$(implib) -machine:$(CPU) -def:ntscmlib.def $*.obj -out:$*.lib
	
ntw32lib.dll: ntw32lib.obj ntw32lib.exp
	$(link) $(linkdebug) -dll -out:$*.dll \
		-entry:ntw32lib_dllinit$(DLLENTRY) \
		$** $(syslibs) w32sut32.lib

clean:
  del *.tch
  del *.obj
  del *.exe
  del *.lib
  del *.dll
  del *.exp
  del *.rbj
  del *.res

unconfig:
  del cmpintmd.h
  del cmpauxmd.asm
  del float.h
  del limits.h
  del makefile
  del *.lst
  del setenv.bat
  del win31.bat
  del winnt.bat
  del *.mak
  del *.def
  del *.dlg
  del *.ico


