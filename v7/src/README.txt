Structure and construction of the MIT Scheme source tree

$Id: README.txt,v 1.2 2001/02/23 16:16:38 cph Exp $

Directories
===========

MIT Scheme is a large program consisting of many subdirectories.
These subdirectories can be further grouped together into rough
functional subsystems.

The core subsystem consists of these directories:

* "microcode" contains the C code that is used to build the executable
  programs "scheme" and "bchscheme".

* "runtime" contains the bulk of the run-time library, including
  almost everything documented in the reference manual.

* "runtime-check" is a build directory used to make alternate run-time
  library binaries that are safer than the standard binaries.  The
  standard binaries are compiled with type-checking disabled for many
  common operations; the alternate binaries have type-checking
  enabled.

* "sos" contains the code for the SOS object-oriented programming
  extension.

* "win32" contains extra parts of the run-time library that are
  specific to the Microsoft Windows platform.

The compiler subsystem consists of these three directories:

* "sf" contains a program that translates Scheme source code to an
  internal binary format called SCode.  SCode is the internal
  representation used by the MIT Scheme interpreter.  The "sf" program
  also performs a handful of optimizations, such as user-directed beta
  substitution and early binding of known variables such as CAR.

* "compiler" contains the native-code compiler.  This program
  translates SCode to machine-language instructions.

* "cref" is a cross-reference program that also implements a
  rudimentary module system.

The editor subsystem consists of two directories:

* "edwin" contains our Emacs-like editor written in Scheme.

* "imail" contains an email-reading program for Edwin.

These are miscellaneous extras:

* "6001" is extra code used here at MIT for teaching 6.001, our
  introductory computer-science course based on "Structure and
  Interpretation of Computer Programs".  "sicp" contains an older
  version of this code that is no longer in use (and probably no
  longer works).

* "etc" contains miscellaneous files for building the system.

* "rcs" is a parser for RCS files.  It also contains a program for
  generating merged log files, in RCS or ChangeLog format, for
  directory trees under RCS or CVS control.

These directories are no longer actively in use and the code they
contain may not work:

* "pcsample" contains a profiling extension.

* "swat" contains an extension that interfaces MIT Scheme to the Tk
  graphical toolkit.

* "wabbit" contains program for finding all of the objects that
  contain pointers to a given object.

Building from source on unix systems
====================================

Building MIT Scheme from the CVS sources is a multi-stage process.
The system has been designed around a number of "build states" and
specific commands that move the system from one build state to
another.  These are the build states, ordered from least to most
"built".

* The `CVS' state is the initial state of the tree when it is freshly
  checked out from CVS.

* The `distribution' state is what we distribute to the world.  In
  this state, all of the target-system independent configuration has
  been done.

* In the `configured' state, the tree is customized for a particular
  target system, but it is not yet compiled.

* In the `compiled' state, the tree is fully compiled.

The following table shows the commands are used to transition the
system from one build state to another.  All of the commands must be
run while the working directory is the top-level directory of the
source tree.

	From		To		Command
	------------	------------	---------------------
	CVS		distribution	./Setup.sh
	distribution	configured	./configure
	configured	compiled	make
	compiled	configured	make clean
	compiled	distribution	make distclean
	compiled	CVS		make maintainer-clean
	configured	distribution	make distclean
	configured	CVS		make maintainer-clean

The following sequence of commands can be used to rebuild MIT
Scheme from the CVS sources and install it:

	./Setup.sh
	./configure
	make
	make install

Note that the "./Setup.sh" command requires a compiler that supports
the "-M" option for generating dependencies.  Normally this step is
executed on a GNU/Linux system.
