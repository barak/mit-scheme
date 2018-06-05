Structure and construction of the MIT/GNU Scheme source tree

This README delves into the details of building MIT/GNU Scheme from
source on GNU/Linux.  If you found this README in a binary
distribution, you probably want the installation instructions in
../doc/user-manual/mit-scheme-user.info node "Installation", also
available online at http://www.gnu.org/software/mit-scheme/.

The rest of this file assumes you were able to successfully complete
an installation.  Mit-scheme is used to build mit-scheme, so a binary
distribution must be installed first.  The only alternative is to
cross-compile on a host that has a binary distribution installed.

If you have the "Portable C" distribution, you have the result of LIAR
cross-compiling itself to C.  The resulting .c files can be compiled
almost anywhere, e.g. on a 64bit target withOUT an mit-scheme already
installed.  To build mit-scheme from this distribution, see node
"Portable C Installation" in ../doc/user-manual/mit-scheme-user.info,
also available online at http://www.gnu.org/software/mit-scheme/.  To
build this distribution from sources, use src/etc/make-liarc-dist.sh.

Directories
===========

MIT/GNU Scheme is a large program consisting of many subdirectories.
These subdirectories can be further grouped together into rough
functional subsystems.

The core subsystem consists of these directories:

* "microcode" contains the C code that is used to build the executable
  program "scheme".

* "runtime" contains the bulk of the run-time library, including
  almost everything documented in the reference manual.

* "sos" contains the SOS object-oriented programming extension.

* "star-parser" contains the pattern-matching parser language
  extension.

* "win32" contains extra parts of the run-time library that are
  specific to the Microsoft Windows platform.

* "xml" contains support for XML and XHTML I/O.

* "ffi" provides syntax for calling foreign (C) functions and
  manipulating alien (C) data.

The compiler subsystem consists of these three directories:

* "sf" contains a program that translates Scheme source code to an
  internal binary format called SCode.  SCode is the internal
  representation used by the MIT/GNU Scheme interpreter.  The "sf"
  program also performs a handful of optimizations, such as
  user-directed beta substitution and early binding of known variables
  such as CAR.

* "compiler" contains the native-code compiler.  This program
  translates SCode to machine-language instructions.

* "cref" is a cross-reference program that also implements a
  rudimentary module system.

The editor subsystem consists of two directories:

* "edwin" contains our Emacs-like editor written in Scheme.

* "imail" contains an email-reading program for Edwin.

There are a few C/Unix FFI plugins:

* "gdbm" wraps libgdbm, the GNU dbm database routines, and provides a
  drop-in replacement for the microcode module based package (runtime
  gdbm).

* "blowfish" wraps libssl or libcrypto blowfish functions, and
  provides a drop-in replacement for the microcode module based
  package (runtime blowfish).

* "mcrypt" wraps libmcrypt and provides replacements for the mcrypt-*
  procedures implemented in (runtime crypto).

* "pgsql" wraps libpq and provides a drop-in replacement for the
  microcode module based package (runtime postgresql).

* "x11" wraps libX11 and provides a drop-in replacement for the
  microcode module based package (runtime x-graphics).

These are miscellaneous extras:

* "6001" is extra code used here at MIT for teaching 6.001, our
  introductory computer-science course based on "Structure and
  Interpretation of Computer Programs".

* "etc" contains miscellaneous files for building the program.

* "ssp" is an implementation of "Scheme Server Pages" that supports
  server-side web programming.  It works in conjunction with Apache
  and mod-lisp.

* "xdoc" is a web-programming document language, used at MIT for an
  experimental electronics circuit course during spring term 2004.
  This language is no longer in active use and will not be supported.
  But it is a good example of "ssp" usage.

Building from source on unix systems
====================================

Building MIT/GNU Scheme from the sources in the git repository is a
multi-stage process designed around a number of "build states" and
specific commands that move the build tree from one state to another.
These are the build states, ordered from least to most "built".

* The `fresh' state is the initial state of the tree when it is
  freshly checked out of the git repository.

* The `distribution' state is what we distribute to the world.  In
  this state, all of the target-system independent configuration has
  been done.

* In the `configured' state, the tree is customized for a particular
  target system, but it is not yet compiled.

* In the `compiled' state, the tree is fully compiled.

The following table shows the commands used to transition the build
tree from one build state to another.  All of the commands must be run
in the "src" directory.

	From		To		Command
	------------	------------	---------------------
	fresh		distribution	./Setup.sh
	distribution	configured	./configure
	configured	compiled	make
	compiled	configured	make clean
	compiled	distribution	make distclean
	compiled	fresh		make maintainer-clean
	configured	distribution	make distclean
	configured	fresh		make maintainer-clean

Thus the following sequence of commands can be used to build and
install MIT/GNU Scheme, assuming you have already installed a
compatible binary release.

	./Setup.sh
	./configure
	make
	make install

Note that the "./Setup.sh" command requires a compiler that supports
the "-M" option for generating dependencies.  Normally this step is
executed on a GNU/Linux system.

All of these commands require a working mit-scheme command from a
compatible binary release.  This "host scheme" is usually any recent
release, but the most recent is most likely to have all of the runtime
primitives and macros and whatnot required by the latest sources.  If
you have the latest release installed and working, yet cannot compile
the latest sources, please feel free to report this as a bug, via the
bug tracking system mentioned on the project homepage:

	http://www.gnu.org/software/mit-scheme/

If you have installed your host scheme somewhere other than the usual
system-wide location(s), you may want to set the MIT_SCHEME_EXE
environment variable.  The Makefiles expect it to be the host scheme's
command name.  For information about installing MIT/GNU Scheme in
unusual locations, please see the Unix Installation instructions.

Building an incompatible compiler
=================================

If the basic compiler data structures have changed, it may not be
possible to directly build the compiler by invoking make.  (This is a
known bug.)

However, it is possible to build the compiler from the Scheme sources
if you have a working installation with a runtime band.  Here is how:

    1.  Put the source tree into the `configured' state as per the
        above instructions.

    2.  Make the "src/compiler/" directory be your working directory.

    3.  `Syntax' the compiler with these steps:

        a.  Start scheme with the runtime band:
            scheme --band runtime.com

        b.  ]=> (load-option 'sf)

        c.  ]=> (load "compiler.sf")

        d.  ]=> (exit)

    4.  Compile the compiler with these steps:

        a.  Start scheme with the runtime band:
            scheme --band runtime.com

        b.  ]=> (load-option 'sf)

        c.  ]=> (load "make")

        d.  ]=> (load "compiler.cbf")

        e.  ]=> (exit)

    5.  Build a new compiler band with these steps:

        a.  Start scheme with the runtime band:
            scheme --band runtime.com

        b.  ]=> (load-option 'cref)

        c.  ]=> (load-option 'sf)

        d.  ]=> (load "make")

        e.  ]=> (disk-save "compiler-band.com")

The resulting band, compiler-band.com, should be suitable for
compiling the compiler.

