## Process this file with automake to produce Makefile.in
##
## Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
##     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
##     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
##     2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Massachusetts
##     Institute of Technology
##
## This file is part of MIT/GNU Scheme.
##
## MIT/GNU Scheme is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or (at
## your option) any later version.
##
## MIT/GNU Scheme is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MIT/GNU Scheme; if not, write to the Free Software
## Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
## USA.

MIT_SCHEME_EXE = @MIT_SCHEME_TOOLCHAIN_EXE@
scmlibdir = @MIT_SCHEME_LIBDIR@
scmlib_subdir = $(scmlibdir)edwin
scmdocdir = $(datarootdir)/doc/@MIT_SCHEME_PROJECT@

scmlib_sub_DATA = $(sources)
scmlib_sub_DATA += $(binaries)
scmlib_sub_DATA += loadef.scm edwin.bld
scmlib_sub_DATA += make.scm edwin.pkg @MIT_SCHEME_PKD@
scmlib_sub_DATA += TUTORIAL

#info_TEXINFOS = mit-scheme-edwin.texi
#AM_MAKEINFOHTMLFLAGS = --no-split

sources = \
	abbrev.scm adapters.scm ansi.scm argred.scm artdebug.scm autold.scm \
	autosv.scm basic.scm bios.scm bufcom.scm buffer.scm buffrm.scm \
	bufinp.scm bufmnu.scm bufout.scm bufset.scm bufwfs.scm bufwin.scm \
	bufwiu.scm bufwmc.scm c-mode.scm calias.scm cinden.scm class.scm \
	clscon.scm clsmac.scm comatch.scm comhst.scm comint.scm comman.scm \
	compile.scm comred.scm comtab.scm comwin.scm curren.scm dabbrev.scm \
	debian-changelog.scm debug.scm debuge.scm diff.scm dired.scm \
	dirunx.scm dirw32.scm display.scm docstr.scm dos.scm doscom.scm \
	dosfile.scm dosproc.scm dosshell.scm ed-ffi.scm editor.scm edtfrm.scm \
	edtstr.scm evlcom.scm eystep.scm filcom.scm fileio.scm fill.scm \
	grpops.scm hlpcom.scm htmlmode.scm image.scm info.scm input.scm \
	intmod.scm iserch.scm javamode.scm key-w32.scm keymap.scm keyparse.scm \
	kilcom.scm kmacro.scm lincom.scm linden.scm loadef.scm lspcom.scm \
	macros.scm make.scm malias.scm manual.scm midas.scm modefs.scm \
	modes.scm modlin.scm modwin.scm motcom.scm motion.scm mousecom.scm \
	nntp.scm notify.scm nvector.scm occur.scm outline.scm paredit.scm \
	pasmod.scm paths.scm print.scm process.scm prompt.scm pwedit.scm \
	pwparse.scm rcsparse.scm reccom.scm regcom.scm regexp.scm regops.scm \
	replaz.scm rfc822.scm ring.scm rmail.scm rmailsrt.scm rmailsum.scm \
	schmod.scm scrcom.scm screen.scm search.scm sendmail.scm sercom.scm \
	shell.scm simple.scm snr.scm sort.scm string.scm strpad.scm strtab.scm \
	struct.scm syntax.scm tagutl.scm techinfo.scm telnet.scm termcap.scm \
	texcom.scm things.scm tparse.scm tterm.scm tximod.scm txtprp.scm \
	undo.scm unix.scm utils.scm utlwin.scm vc.scm vc-bzr.scm vc-cvs.scm \
	vc-git.scm vc-rcs.scm vc-svn.scm verilog.scm vhdl.scm webster.scm \
	win32.scm win32com.scm wincom.scm window.scm winout.scm \
	world-monitor.scm xform.scm xterm.scm
binaries = $(sources:.scm=.bci) $(sources:.scm=.com)
$(binaries): stamp-scheme
edwin.bld: stamp-scheme
@MIT_SCHEME_PKD@: stamp-scheme
stamp-scheme: $(sources) edwin.ldr edwin.pkg
	MIT_SCHEME_EXE=$(MIT_SCHEME_EXE) ./compile.sh
	echo "done" > $@

CLEANFILES = *.bin *.ext *.com *.bci *.moc *.fni *.crf *.fre *.pkd *.bld
CLEANFILES += stamp-scheme

#TESTS = edwin-check.sh

TAGS_FILES = $(sources) edwin.ldr

EXTRA_DIST = $(sources) edwin.ldr
EXTRA_DIST += TUTORIAL
EXTRA_DIST += compile.sh decls.scm
EXTRA_DIST += edwin.sf edwin.cbf edwin.pkg
