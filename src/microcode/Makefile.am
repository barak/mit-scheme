## Process this file with automake to produce Makefile.in
##
## The microcode is the C part of MIT/GNU Scheme: the interpreter, the
## garbage collector and the primitives.

MIT_SCHEME_ARCH = @MIT_SCHEME_ARCH@
MIT_SCHEME_VERSION = @MIT_SCHEME_VERSION@
AUXDIR_NAME = @AUXDIR_NAME@
EXE_NAME = mit-scheme-$(MIT_SCHEME_ARCH)-$(MIT_SCHEME_VERSION)

## Where the interpreter looks for its band and its shared objects.
auxdir = $(libdir)/$(AUXDIR_NAME)

AM_CPPFLAGS = -I. -I$(srcdir) @SCHEME_DEFS@ \
	-DDEFAULT_LIBRARY_PATH=\"$(auxdir)\"

## **** The interpreter ****
##
## Installed by hand below, under a name that carries the back end and the
## version, so it is noinst_ rather than bin_.

noinst_PROGRAMS = scheme

scheme_SOURCES = artutl.c avltree.c bkpt.c bignum.c bigprm.c bitstr.c boot.c \
	bytevector.c chacha12.c chacha20.c chacha8.c char.c daemon.c \
	debug.c dfloat.c error.c extern.c fasdump.c fasl.c fasload.c \
	fixnum.c floenv.c flonum.c fma.c gcloop.c generic.c hooks.c hunk.c \
	intern.c interp.c intprm.c keccak.c list.c lookprm.c lookup.c \
	md5.c memmag.c missing.c obstack.c option.c osscheme.c ostty.c \
	outf.c prchacha.c prentropy.c prim.c primutl.c prkeccak.c prmd5.c \
	ptrvec.c purify.c purutl.c regex.c rgxprim.c step.c storage.c \
	string.c syntax.c sysprim.c term.c transact.c utabmd.c utils.c \
	vector.c wind.c prosenv.c prosfile.c prosfs.c prosio.c prosproc.c \
	prospty.c prosterm.c prostty.c pruxsock.c intext.c pruxenv.c \
	pruxfs.c pruxio.c ux.c uxctty.c uxentropy.c uxenv.c uxfile.c \
	uxfs.c uxio.c uxproc.c uxsig.c uxsock.c uxterm.c uxtop.c uxtrap.c \
	uxtty.c uxutil.c

## Generated: the primitive table, built by findprim from the sources above.
nodist_scheme_SOURCES = usrdef.c

## Which of these configure picks depends on the back end and on what the
## terminal library turned out to be; it reports the objects it settled on
## in OPTIONAL_OBJECTS.  Listing them here lets automake emit rules for
## them without committing to building any.
EXTRA_scheme_SOURCES = cmpauxmd.c compinit.c pruxdld.c pruxffi.c \
	svm1-interp.c termcap.c terminfo.c tparam.c tterm.c unstackify.c

scheme_LDADD = @OPTIONAL_OBJECTS@ $(LIARC_OBJECTS)
scheme_DEPENDENCIES = @OPTIONAL_OBJECTS@ $(LIARC_OBJECTS)
scheme_LDFLAGS = @SCHEME_LDFLAGS@

## **** Rules for C back end (part 1, variables) ****

@LIARC_VARS@

## **** Auxiliary programs ****
##
## configure names the ones this back end needs in AUX_PROGRAMS; automake
## needs to have heard of all of them regardless.

EXTRA_PROGRAMS = gen-nonce extract-liarc-decls macosx-starter

aux_PROGRAMS = @AUX_PROGRAMS@
aux_DATA = @AUX_DATA@

gen_nonce_SOURCES = gen-nonce.c
extract_liarc_decls_SOURCES = extract-liarc-decls.c
macosx_starter_SOURCES = macosx-starter.c

## **** Generated sources ****
##
## findprim scans the sources for primitive definitions.  It runs during the
## build, so it is compiled with the build machine's compiler, which is why
## it is not an automake program.

usrdef.c: $(scheme_SOURCES) @OPTIONAL_SOURCES@ findprim Makefile
	$(AM_V_GEN)rm -f $@.tmp
	$(AM_V_at)./findprim $(scheme_SOURCES) @OPTIONAL_SOURCES@ > $@.tmp && mv -f $@.tmp $@

findprim: findprim.c config.h
	$(AM_V_CC)@HOST_CC@ $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) -o $@ $(srcdir)/findprim.c

## cmpauxmd is the back end's assembly glue, expanded from m4.

SUFFIXES = .m4 .s

.m4.s:
	$(AM_V_GEN)rm -f $@.tmp
	$(AM_V_at)$(srcdir)/makegen/m4.sh @M4@ @M4_FLAGS@ $< > $@.tmp \
	  && mv -f $@.tmp $@

.s.o:
	$(AM_V_CC)@AS@ @AS_FLAGS@ -o $@ $<

## The hand-written link rule marked the binary for PaX; configure leaves
## PAXCTL as ':' everywhere except NetBSD.
all-local: scheme$(EXEEXT)
	@PAXCTL@ +m scheme$(EXEEXT)

## **** Rules for C back end (part 2, rules) ****

@LIARC_RULES@

## **** Installation ****
##
## The interpreter goes in under its versioned name, with the unversioned
## names made as symlinks beside it.

install-exec-local: scheme$(EXEEXT)
	$(MKDIR_P) $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) scheme$(EXEEXT) $(DESTDIR)$(bindir)/$(EXE_NAME)
	$(srcdir)/../etc/install-bin-symlinks.sh \
	  $(DESTDIR)$(bindir) $(EXE_NAME) $(MIT_SCHEME_ARCH)

install-data-local: @INSTALL_INCLUDE@
	$(MKDIR_P) $(DESTDIR)$(auxdir)
	$(INSTALL_DATA) $(srcdir)/pruxffi.h $(DESTDIR)$(auxdir)/mit-scheme.h

## The C back end additionally installs the headers and the two wrapper
## scripts needed to compile and link generated C.
install-include:
	$(MKDIR_P) $(DESTDIR)$(auxdir)
	$(INSTALL_SCRIPT) liarc-cc $(DESTDIR)$(auxdir)/.
	sed -e "s&^SCHEME_EXE=.*&SCHEME_EXE=$(bindir)/$(EXE_NAME)&" \
	  < liarc-ld > liarc-ld.install
	$(INSTALL_SCRIPT) liarc-ld.install $(DESTDIR)$(auxdir)/liarc-ld
	rm -f liarc-ld.install
	$(MKDIR_P) $(DESTDIR)$(auxdir)/include
	$(INSTALL_DATA) *.h $(DESTDIR)$(auxdir)/include/.

.PHONY: install-include

## **** Cleaning ****

MOSTLYCLEANFILES = usrdef.c compinit.c compinit.h cmpauxmd.s \
	$(LIARC_OBJECTS)
CLEANFILES = findprim $(EXTRA_PROGRAMS)
DISTCLEANFILES = cmpauxmd.m4 cmpauxmd.c cmpintmd.h makegen-cc \
	cmpintmd-config.h cmpintmd.c liarc-cc liarc-ld

EXTRA_DIST = findprim.c Tags.sh
