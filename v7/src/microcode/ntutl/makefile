### -*- Fundamental -*-
###
###     $Id: makefile,v 1.25 2001/07/18 05:25:00 cph Exp $
###
###     Copyright (c) 1992-2001 Massachusetts Institute of Technology
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
###     Software Foundation, Inc., 59 Temple Place - Suite 330,
###     Boston, MA 02111-1307, USA.

#### Makefile for Scheme under Win32 compiled by Microsoft Visual C++.
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

# Update the resource if necessary
ntgui.res: ntgui.rc ntgui.h ntdialog.dlg ntdialog.h
    rc -r -fo $*.res $(cvars) $*.rc

usrdef.c : $(SCHEME_SOURCES) $(SOURCES) $(OS_PRIM_SOURCES) ntgui.c usrdef.tch \
	findprim.exe
	.\findprim $(SCHEME_SOURCES) $(SOURCES) $(OS_PRIM_SOURCES) ntgui.c \
	> usrdef.c

bchdef.c : $(SCHEME_SOURCES) $(BCHSOURCES) $(OS_PRIM_SOURCES) ntgui.c \
	usrdef.tch findprim.exe
	.\findprim $(SCHEME_SOURCES) $(BCHSOURCES) $(OS_PRIM_SOURCES) ntgui.c \
	> bchdef.c

#
# Dependencies.  (This was a lot of work!)
#
# This first section defines the dependencies of the include files.
#
AVLTREE_H = avltree.h $(CONFIG_H)
BCHDRN_H = bchdrn.h $(CONFIG_H)
BCHGCC_H = bchgcc.h $(CONFIG_H) $(GCCODE_H)
BIGNMINT_H = bignmint.h $(PRIMS_H)
BIGNUM_H = bignum.h ansidecl.h
BITSTR_H = bitstr.h
BKPT_H = bkpt.h
CMPGC_H = cmpgc.h $(CMPINTMD_H)
CMPINTMD_H = cmpintmd.h $(CMPTYPE_H)
CMPINT_H = cmpint.h
CMPTYPE_H = cmptype.h
COMLIN_H = comlin.h ansidecl.h
CONFIG_H = config.h confshared.h ansidecl.h
CONST_H = const.h
CRITSEC_H = critsec.h
DEFAULT_H = default.h
DSTACK_H = dstack.h ansidecl.h
DUMP_C = dump.c
EDWIN_H = edwin.h
ERRORS_H = errors.h
EXTERN_H = extern.h
FASL_H = fasl.h
FIXOBJ_H = fixobj.h
FLOAT_H = float.h
FUTURES_H = futures.h
GCCODE_H = gccode.h $(CMPGC_H)
GCTYPE_C = gctype.c $(CONFIG_H)
GC_H = gc.h
HISTORY_H = history.h
INTERP_H = interp.h
INTEXT_H = intext.h ansidecl.h $(DSTACK_H)
INTRPT_H = intrpt.h
LIMITS_H = limits.h
LOAD_C = load.c $(FASL_H)
LOCKS_H = locks.h
LOOKUP_H = lookup.h
MEMMAG_H = memmag.h $(NTSCMLIB_H)
MUL_C = mul.c $(CONFIG_H)
NTAPI_H = ntapi.h
NTDIALOG_H = ntdialog.h
NTGUI_H = ntgui.h
NTIO_H = ntio.h $(OSIO_H)
NTSCMLIB_H = ntscmlib.h
NTSCREEN_H = ntscreen.h
NTSYS_H = ntsys.h
NTTERM_H = ntterm.h $(OSTERM_H)
NTTOP_H = nttop.h $(OSTOP_H)
NTTRAP_H = nttrap.h
OBJECT_H = object.h
OBSTACK_H = obstack.h $(CONFIG_H)
OPTION_H = option.h ansidecl.h
OSCTTY_H = osctty.h $(OS_H)
OSENV_H = osenv.h $(OS_H)
OSFILE_H = osfile.h $(OS_H)
OSFS_H = osfs.h $(OS_H)
OSIO_H = osio.h $(OS_H)
OSSCHEME_H = osscheme.h $(OUTF_H) $(OS_H)
OSSIG_H = ossig.h $(OS_H)
OSTERM_H = osterm.h $(OS_H)
OSTOP_H = ostop.h $(OS_H)
OSTTY_H = ostty.h $(OS_H)
OS_H = os.h $(CONFIG_H)
OUTF_H = outf.h $(CONFIG_H)
PRENAME_H = prename.h
PRIMS_H = prims.h ansidecl.h
PRIM_H = prim.h
PRMCON_H = prmcon.h
REGEX_H = regex.h
RETURNS_H = returns.h
SCODE_H = scode.h
SDATA_H = sdata.h
STACK_H = stack.h
SYNTAX_H = syntax.h
SYSCALL_H = syscall.h $(CONFIG_H) $(NTAPI_H)
TRAP_H = trap.h
TYPES_H = types.h
USRDEF_H = usrdef.h $(SCHEME_H) $(PRIMS_H)
UXSOCK_H = uxsock.h $(OSIO_H)
VERSION_H = version.h
WINDER_H = winder.h
ZONES_H = zones.h

PSBMAP_H = psbmap.h $(CONFIG_H) $(TYPES_H) $(OBJECT_H) $(BIGNUM_H) \
	$(BIGNMINT_H) $(SDATA_H) $(CONST_H) $(GCCODE_H) $(CMPTYPE_H) \
	$(COMLIN_H)

NT_H = nt.h $(CONFIG_H) $(INTEXT_H) $(DSTACK_H) $(OSSCHEME_H) $(NTSYS_H) \
	$(SYSCALL_H) $(NTAPI_H)

SCHEME_H = scheme.h $(CONFIG_H) $(DSTACK_H) $(OBSTACK_H) $(TYPES_H) \
	$(CONST_H) $(OBJECT_H) $(INTRPT_H) $(CRITSEC_H) $(GC_H) $(SCODE_H) \
	$(SDATA_H) $(FUTURES_H) $(ERRORS_H) $(RETURNS_H) $(FIXOBJ_H) \
	$(STACK_H) $(INTERP_H) $(OUTF_H) $(BKPT_H) $(DEFAULT_H) $(EXTERN_H) \
	$(BIGNUM_H) $(PRIM_H) $(FLOAT_H)

#
# This second section is the dependencies of the object files.
#
artutl.obj: artutl.c $(SCHEME_H) $(LIMITS_H)
avltree.obj: avltree.c $(AVLTREE_H)
bignum.obj: bignum.c $(SCHEME_H) $(BIGNMINT_H) $(LIMITS_H)
bigprm.obj: bigprm.c $(SCHEME_H) $(PRIMS_H) $(ZONES_H)
bitstr.obj: bitstr.c $(SCHEME_H) $(PRIMS_H) $(BITSTR_H)
boot.obj: boot.c $(SCHEME_H) $(PRIMS_H) $(VERSION_H) $(OPTION_H) $(OSTOP_H) \
	$(OSTTY_H)
char.obj: char.c $(SCHEME_H) $(PRIMS_H)
cmpauxmd.obj: cmpauxmd.asm
cmpint.obj: cmpint.c $(CONFIG_H) $(DSTACK_H) $(OUTF_H) $(TYPES_H) $(CONST_H) \
	$(OBJECT_H) $(INTRPT_H) $(GC_H) $(SDATA_H) $(ERRORS_H) $(RETURNS_H) \
	$(FIXOBJ_H) $(STACK_H) $(INTERP_H) $(DEFAULT_H) $(EXTERN_H) $(TRAP_H) \
	$(PRIMS_H) $(PRIM_H) $(CMPGC_H) $(NTSCMLIB_H)
comutl.obj: comutl.c $(SCHEME_H) $(PRIMS_H)
daemon.obj: daemon.c $(SCHEME_H) $(PRIMS_H) $(OSIO_H)
debug.obj: debug.c $(SCHEME_H) $(PRIMS_H) $(TRAP_H) $(LOOKUP_H)
dfloat.obj: dfloat.c $(SCHEME_H) $(PRIMS_H)
error.obj: error.c $(OUTF_H) $(DSTACK_H)
extern.obj: extern.c $(SCHEME_H) $(PRIMS_H)
fasload.obj: fasload.c $(SCHEME_H) $(PRIMS_H) $(OSSCHEME_H) $(OSFILE_H) \
	$(OSIO_H) $(GCCODE_H) $(TRAP_H) $(OPTION_H) $(PRMCON_H)
fixnum.obj: fixnum.c $(SCHEME_H) $(PRIMS_H) $(MUL_C)
flonum.obj: flonum.c $(SCHEME_H) $(PRIMS_H) $(ZONES_H)
generic.obj: generic.c $(SCHEME_H) $(PRIMS_H)
hooks.obj: hooks.c $(SCHEME_H) $(PRIMS_H) $(WINDER_H) $(HISTORY_H)
hunk.obj: hunk.c $(SCHEME_H) $(PRIMS_H)
intern.obj: intern.c $(SCHEME_H) $(PRIMS_H) $(TRAP_H)
interp.obj: interp.c $(SCHEME_H) $(LOCKS_H) $(TRAP_H) $(LOOKUP_H) $(WINDER_H) \
	$(HISTORY_H) $(CMPINT_H) $(ZONES_H) $(PRMCON_H)
intprm.obj: intprm.c $(SCHEME_H) $(PRIMS_H) $(ZONES_H)
list.obj: list.c $(SCHEME_H) $(PRIMS_H)
lookprm.obj: lookprm.c $(SCHEME_H) $(PRIMS_H) $(LOCKS_H) $(TRAP_H) $(LOOKUP_H)
lookup.obj: lookup.c $(SCHEME_H) $(LOCKS_H) $(TRAP_H) $(LOOKUP_H)
obstack.obj: obstack.c $(OBSTACK_H)
option.obj: option.c $(SCHEME_H) $(FASL_H) $(OSENV_H) $(OSFS_H) $(NT_H) \
	$(NTIO_H)
osscheme.obj: osscheme.c $(SCHEME_H) $(PRIMS_H) $(OSSCHEME_H)
ostty.obj: ostty.c $(OSTTY_H) $(OSSCHEME_H)
outf.obj: outf.c $(SCHEME_H) $(NTSCREEN_H)
prim.obj: prim.c $(SCHEME_H) $(PRIMS_H)
primutl.obj: primutl.c $(SCHEME_H) $(PRIMS_H) $(OS_H) $(USRDEF_H) \
	$(PRENAME_H) $(SYSCALL_H) $(AVLTREE_H) $(CMPGC_H)
prmcon.obj: prmcon.c $(SCHEME_H) $(PRIMS_H) $(PRMCON_H)
ptrvec.obj: ptrvec.c $(OUTF_H) $(DSTACK_H)
purutl.obj: purutl.c $(SCHEME_H) $(PRIMS_H) $(GCCODE_H) $(ZONES_H) \
	$(CMPINT_H)
regex.obj: regex.c $(SCHEME_H) $(SYNTAX_H) $(REGEX_H)
rgxprim.obj: rgxprim.c $(SCHEME_H) $(PRIMS_H) $(EDWIN_H) $(SYNTAX_H) $(REGEX_H)
step.obj: step.c $(SCHEME_H) $(PRIMS_H)
storage.obj: storage.c $(SCHEME_H) $(GCTYPE_H)
string.obj: string.c $(SCHEME_H) $(PRIMS_H)
syntax.obj: syntax.c $(SCHEME_H) $(PRIMS_H) $(EDWIN_H) $(SYNTAX_H)
sysprim.obj: sysprim.c $(SCHEME_H) $(PRIMS_H) $(OSTTY_H) $(OSTOP_H)
term.obj: term.c $(SCHEME_H) $(OSTOP_H) $(OSIO_H) $(OSFS_H) $(OSFILE_H) \
	$(EDWIN_H)
tparam.obj: tparam.c ansidecl.h
transact.obj: transact.c ansidecl.h $(OUTF_H) $(DSTACK_H)
utils.obj: utils.c $(SCHEME_H) $(PRIMS_H) $(WINDER_H) $(HISTORY_H) \
	$(CMPINT_H) $(SYSCALL_H)
vector.obj: vector.c $(SCHEME_H) $(PRIMS_H)
wind.obj: wind.c $(OBSTACK_H) $(DSTACK_H) $(OUTF_H)

prbfish.obj: prbfish.c $(SCHEME_H) $(PRIMS_H)
prgdbm.obj: prgdbm.c $(SCHEME_H) $(PRIMS_H) $(OS_H)
prmd5.obj: prmd5.c $(SCHEME_H) $(PRIMS_H)
prosenv.obj: prosenv.c $(SCHEME_H) $(PRIMS_H) $(OSENV_H) $(OSTOP_H) $(LIMITS_H)
prosfile.obj: prosfile.c $(SCHEME_H) $(PRIMS_H) $(OSFILE_H)
prosfs.obj: prosfs.c $(SCHEME_H) $(PRIMS_H) $(OSFILE_H) $(OSFS_H) $(OSIO_H)
prosio.obj: prosio.c $(SCHEME_H) $(PRIMS_H) $(OSIO_H)
prosproc.obj: prosproc.c $(SCHEME_H) $(PRIMS_H) $(OSPROC_H) $(OSIO_H)
prosterm.obj: prosterm.c $(SCHEME_H) $(PRIMS_H) $(OSTERM_H) $(OSIO_H)
prostty.obj: prostty.c $(SCHEME_H) $(PRIMS_H) $(OSTTY_H) $(OSCTTY_H) \
	$(OSFILE_H) $(OSIO_H)
pruxsock.obj: pruxsock.c $(SCHEME_H) $(PRIMS_H) $(UXSOCK_H)
prntenv.obj: prntenv.c $(SCHEME_H) $(PRIMS_H) $(NT_H) $(NTIO_H)
prntfs.obj: prntfs.c $(SCHEME_H) $(PRIMS_H) $(NT_H) $(NTFS_H)
prntio.obj: prntio.c $(SCHEME_H) $(PRIMS_H) $(NTIO_H) $(NT_H) $(NTSCREEN_H) \
	$(NTGUI_H) $(SYSCALL_H) $(NTPROC_H) $(OSTTY_H)

fasdump.obj: fasdump.c $(SCHEME_H) $(PRIMS_H) $(OSSCHEME_H) $(OSIO_H) \
	$(OSFILE_H) $(OSFS_H) $(GCCODE_H) $(TRAP_H) $(LOOKUP_H) $(FASL_H) \
	$(DUMP_C)
gcloop.obj: gcloop.c $(SCHEME_H) $(GCCODE_H)
memmag.obj: memmag.c $(SCHEME_H) $(PRIMS_H) $(MEMMAG_H) $(GCCODE_H)
purify.obj: purify.c $(SCHEME_H) $(PRIMS_H) $(GCCODE_H) $(ZONES_H)
wabbit.obj: wabbit.c $(SCHEME_H) $(GCCODE_H)

bchdmp.obj: bchdmp.c $(SCHEME_H) $(PRIMS_H) $(OSFILE_H) $(OSFS_H) $(TRAP_H) \
	$(LOOKUP_H) $(FASL_H) $(NT_H) $(NTIO_H) $(BCHGCC_H) $(DUMP_C)
bchgcl.obj: bchgcl.c $(SCHEME_H) $(BCHGCC_H)
bchmmg.obj: bchmmg.c $(SCHEME_H) $(PRIMS_H) $(MEMMAG_H) $(OPTION_H) \
	$(OSENV_H) $(OSENV_H) $(NT_H) $(BCHGCC_H) $(BCHDRN_H)
bchpur.obj: bchpur.c $(SCHEME_H) $(PRIMS_H) $(BCHGCC_H) $(ZONES_H)
bchutl.obj: bchutl.c $(CONFIG_H)

intext.obj: intext.c ansidecl.h $(DSTACK_H) $(INTEXT_H)
ntenv.obj: ntenv.c $(SCHEME_H) $(NT_H) $(OSENV_H) $(NTSCREEN_H)
ntfile.obj: ntfile.c $(NT_H) $(OSFILE_H) $(NTIO_H)
ntfs.obj: ntfs.c $(NT_H) $(NTFS_H) $(OUTF_H)
ntgui.obj: ntgui.c $(SCHEME_H) $(PRIMS_H) $(OS_H) $(NT_H) $(NTDIALOG_H) \
	$(NTGUI_H) $(NTSCREEN_H)
ntio.obj: ntio.c $(SCHEME_H) $(PRIMS_H) $(NT_H) $(NTIO_H) $(OSTERM_H) \
	$(OSFILE_H) $(OUTF_H) $(OSSIG_H) $(INTRPT_H) $(NTSCREEN_H)
ntproc.obj: ntproc.c $(NT_H) $(NTPROC_H) $(NTIO_H) $(NTSCREEN_H) $(NTGUI_H)
ntscreen.obj: ntscreen.c $(NT_H) $(NTSCREEN_H) $(NTGUI_H)
ntsig.obj: ntsig.c $(SCHEME_H) $(CRITSEC_H) $(OSSIG_H) $(OSCTTY_H) $(OSTTY_H) \
	$(NT_H) $(NTGUI_H) $(NTIO_H) $(NTSCMLIB_H) $(NTSCREEN_H) $(NTSYS_H)
ntsock.obj: ntsock.c $(SCHEME_H) $(PRIMS_H) $(NT_H) $(NTIO_H) $(UXSOCK_H)
ntsys.obj: ntsys.c $(NT_H) $(NTSYS_H)
nttop.obj: nttop.c $(NT_H) $(NTTOP_H) $(OSCTTY_H) $(PRIMS_H) $(ERRORS_H) \
	$(OPTION_H) $(OUTF_H) $(NTSCMLIB_H)
nttrap.obj: nttrap.c $(SCHEME_H) $(OS_H) $(NT_H) $(NTTRAP_H) $(GCCODE_H) \
	$(NTSCMLIB_H)
nttterm.obj: $(NTTTERM_H) $(SCHEME_H) $(PRIMS_H) $(OSTERM_H)
nttty.obj: nttty.c $(NT_H) $(OSTTY_H) $(OSENV_H) $(NTIO_H) $(NTTERM_H) \
	$(NTSCREEN_H)
ntasutl.obj: ntasutl.asm

missing.obj: missing.c $(CONFIG_H)

findprim.$(OBJ): findprim.c $(CONFIG_H)

bintopsb.obj: bintopsb.c $(PSBMAP_H) $(LIMITS_H) $(LOAD_C) $(BLTDEF_H) \
	$(TRAP_H)
psbtobin.obj: psbtobin.c $(PSBMAP_H) $(FLOAT_H) $(LIMITS_H) $(FASL_H) $(DUMP_C)
