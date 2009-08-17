Structure and construction of the MIT/GNU Scheme source tree

$Id: 1373f4ada45c04f7ea739c705c881ec8ef4b4b83 $

Directories
===========

MIT/GNU Scheme is a large program consisting of many subdirectories.
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

* "sos" contains the SOS object-oriented programming extension.

* "star-parser" contains the pattern-matching parser language
  extension.

* "win32" contains extra parts of the run-time library that are
  specific to the Microsoft Windows platform.

* "xml" contains support for XML and XHTML I/O.

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

* "ssp" is an implementation of "Scheme Server Pages" that supports
  server-side web programming.  It works in conjunction with Apache
  and mod-lisp.

* "xdoc" is a web-programming document language, used at MIT for an
  experimental electronics circuit course during spring term 2004.
  This language is no longer in active use and will not be supported.
  But it is a good example of "ssp" usage.

These directories are no longer actively in use and the code they
contain may not work:

* "pcsample" contains a profiling extension.

* "swat" contains an extension that interfaces MIT/GNU Scheme to the
  Tk graphical toolkit.

* "wabbit" contains program for finding all of the objects that
  contain pointers to a given object.

Building from source on unix systems - ``The Easy Way''
====================================

Building MIT/GNU Scheme from the CVS sources is a multi-stage process.
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

The following sequence of commands can be used to rebuild MIT/GNU
Scheme from the CVS sources and install it, assuming that some binary
release corresponding to the CVS sources has already been installed:

	./Setup.sh
	./configure
	make
	make install

Note that the "./Setup.sh" command requires a compiler that supports
the "-M" option for generating dependencies.  Normally this step is
executed on a GNU/Linux system.

The above should suffice for building from the CVS sources when some
binary release corresponding to the CVS sources has been successfully
obtained, unpacked and confirmed to run without errors.  When not,
some modest manual configuration is called for.  The next few sections
address this eventuality, why it might arise and how to overcome it.

Building from source on unix systems - ``The Hard Way''
====================================

These more detailed build instructions cover the case where no binary
release corresponding to the CVS sources runs successfully on your
system or none can be installed in the usual system-wide location(s).

If you don't care to understand the underlying issues, skip ahead to
the next section, which is a step-by-step ground-up methodology for
installing MIT/GNU Scheme from scratch from the CVS sources.  If
anything goes wrong in that effort, however, you may need to return
here to understand what went wrong and what might be tried to fix it.

The Problem
-----------

So why does MIT/GNU Scheme fail to install without the corresponding
binary release first being installed?  (And how do you overcome that?)

In short, to transition a pristine checked out CVS source tree
installation from the ``maintainer-clean'' CVS build state into the
``distribution'' build state, the "./Setup.sh" uses MIT/GNU Scheme
itself to grovel over the Scheme sources to build up Scheme system
file dependencies and other distribution-specific configuration data.

This means that in order to invoke "./Setup.sh" successfully, you must
first already have some runnable version of MIT/GNU Scheme available.

Why
---

So why might a binary release distribution fail to run on your system?

This may occur if your system's dynamic library modules (like `glibc')
do not match those against which the binary release was built or if
you simply do not have permission to install software packages in the
usual place(s) on your system.

It also may arise if the installed version of MIT/GNU Scheme on your
system does not match the version you are trying to build from the CVS
sources but you cannot (or prefer not to) uninstall it and replace it
with the matching version (as when release 7.6 is installed but you're
trying to build 7.7 sources while you or others on your system still
depend on the older version being installed for other projects, etc.).

How
---

A few simple environment variable settings are normally sufficient to
work around these difficulties but on rare occasion some more serious
manual preparations will be required.  Specifically, if you cannot
successfully run _any_ MIT/GNU Scheme binaries then you will need to
skip to the next section first:  ``MIT/GNU Scheme Tabula Rasa''.

Before delving into details, some high-level explanation is in order.

What
----

The CVS sources ship in `maintainer-clean' configuration.  This means
that no generated "Makefile" nor "configure" files are present nor are
any C or Scheme compiled files.  Additionally, various infrastructure
symbolic links used to identify the operating system and/or family of
microprocessor are not yet in place in the CVS tree build directory.

The combination of invoking "./Setup.sh" followed by "configure"
addresses these, as described in the build transition table in the
previous section.

Alas, going from the "CVS" build state to the "distribution" build
state requires running a general tool over various file configuration
tables to set up various file dependency lists and system-dependent
Makefile directives and so forth.  As you may have already guessed,
the general tool used to do this in the MIT/GNU Scheme CVS tree is
MIT/GNU Scheme itself.

Naturally, this results in a bit of a chicken-and-egg puzzle.

The Rub
-------

In a nutshell, both the "./Setup.sh" command and the "make" invocation
prescribed earlier require the availability of:  1) _some_ executable
MIT/GNU Scheme ``microcode'' file (usually named "scheme"), as well as
2) the base companion MIT/GNU Scheme microcode table ("utabmd.bin")
and run-time band for that microcode release. (named "runtime.com").

Moreover, in the case of the "make" invocation, we also require an
MIT/GNU Scheme run-time compiler band ("compiler.com" or "all.com")
containing the MIT/GNU Scheme native compiler so it can compile the
run-time system.  Otherwise, the entire system run-time would be
running interpreter-only (including the compiler itself).  Although
that may be a necessary evil on a platform for which the compiler has
not yet been ported (like on an EDSAC or an FTL nano-processor based
system), the default "make" directive assumes you want to compile the
run-time library and tools (including the MIT Scheme compiler itself).

Friction
--------

Due to various release (in)compatibility issues--- such as re-named
internal system procedures, the hygienic macro system re-write, or
re-factored interfaces on internal system modules---  it is not
normally possible to build a given release (say, 7.7) from sources by
using an incompatible installed release (say, 7.6).

It is, however, often possible to build MIT/GNU Scheme from the ground
up from the sources even when the corresponding binaries are not fully
installed.

Naturally, this process is a bit delicate.  It does require that
_some_ binary release has at least been obtained and unpacked, even if
the binary executable cannot be run (e.g., due to dynamic library
incompatibilities and the like, such as `glibc' versions, &c.) and
even if you do not have file system permissions to install software in
the usual system-wide locations (like `/usr/local/' or `/usr/share/').

The details of this modestly complicated process follow.

Building from source on unix systems -  Not using an installed Scheme
====================================

There are two classes of complication that may occur when trying to
build MIT/GNU Scheme from scratch:

  If you can obtain, unpack and successfully run some binary release
      of MIT/GNU Scheme but you cannot install it in the usual place
      then you may be able to do a ``warm'' build.

  If, however, no obtainable binary MIT/GNU Scheme release package
      successfully runs on your system--- e.g., due to their having
      been built with dynamic libraries (like `glibc') that do not
      match your particular system installation--- then a relatively
      simple microcode ``cold'' build is required first.

Since a ``warm'' build is probably more common, we describe it first.
We're optimistic that way.


``Warm'' build using matching binary and source releases
--------------------------------------------------------

[0]. Obtain the appropriate binary release for the source you have.

     The first step is to obtain the base "runtime.com" and "all.com"
     run-time bands, the "scheme" executable (``microcode'') that
     corresponds to the release we wish to build from the CVS sources,
     and the microcode table ("utabmd.bin") and sundry other MIT/GNU
     Scheme library support files (in `lib/mit-scheme/').

     For instance, if the CVS sources are for the Scheme release 7.7.1
     branch, then you require `mit-scheme-7.7.1-<cpu>-<ostype>.tar.gz'
     for your system.

     If the "scheme" executable (``microcode'') from these obtained
     binaries cannot run successfully on your system, you must resort
     to the ``cold'' build procedure detailed below.

     The next step diagnoses if that is the case for your system.

[1]. Verify that the binary release runs on your system.

     Binary releases contain:  1) a `bin/' directory containing the
     executable ``microcode'' (usually named "scheme"), and:  2) a
     `lib/mit-scheme/' directory containing the run-time bands for
     that microcode (along with some other goodies).  You should
     verify that these microcode and run-time binaries run on your
     system before going any further.

     You do that as follows...

[1a]._____
     First, assume the unpacked `bin' and `lib' directories are both
     immediate subdirectories of a directory whose absolute pathname
     is defined by the environment variable MITSCHEME_BOOTSTRAP_DIR.

     This is mainly for expository convenience and purity of essence.

     For example, type something like one of the following into your
     interactive shell, depending on which shell you use and where you
     placed the untar'd binaries:

  (ba)sh:
          export MITSCHEME_BOOTSTRAP_DIR="$HOME/testing/scheme-7.7.1"

          export MITSCHEME_MICROCODE_DIR = \
              "${MITSCHEME_BOOTSTRAP_DIR}/bin"

          export MITSCHEME_EXECUTABLE = \
              "${MITSCHEME_MICROCODE_DIR}/scheme"

  (t)csh:
          setenv MITSCHEME_BOOTSTRAP_DIR "$HOME/testing/scheme-7.7.1"

          setenv MITSCHEME_MICROCODE_DIR \
              "${MITSCHEME_BOOTSTRAP_DIR}/bin"

          setenv MITSCHEME_EXECUTABLE \
              "${MITSCHEME_MICROCODE_DIR}/scheme"

     Please note that none of these bindings end in a `/' character.

     These are just convenience variables we define to help clarify
     the following presentation.  You may name them whatever you like
     or simply type in full absolute file pathnames in place of their
     appearances in what follows.  Season to taste.

[1b].____
     Next, you must define the standard internally-referenced Scheme
     environment variable MITSCHEME_LIBRARY_PATH to point to the
     `lib/mit-scheme/' subdirectory of the binary release directory,
     for example, by typing something like one of the following:

  (ba)sh:
          export MITSCHEME_LIBRARY_PATH = \
              "${MITSCHEME_BOOTSTRAP_DIR}/lib/mit-scheme"
  (t)csh:
          setenv MITSCHEME_LIBRARY_PATH \
              "${MITSCHEME_BOOTSTRAP_DIR}/lib/mit-scheme"

     Please note that these bindings do not end in a `/' character.

     This environment variable is _not_ merely for convenience:  it
     is a built-in environment variable that Scheme probes on entry.
     It is how we tell the microcode where to find its companion
     run-time bands and support files when they are not yet installed.

[1c].____
     Last, you are now prepared to verify that this binary release
     runs correctly on your system by typing the following at your
     interactive shell prompt:

               ${MITSCHEME_EXECUTABLE} -no-init-file

     The `-no-init-file' is to avoid incompatibility distractions in
     case you have a "$HOME/.scheme.init" file for some other release.

     [Footnote:  Future releases may eventually require two leading
     dashes (as `--no-init-file') but, for backward compatibility, one
     dash is maximally compatible for now.  Similarly throughout the
     rest of this discussion, we will always show one-dash argument
     ``switches''.  Use two-dash switches if this causes any obvious
     problems here in the future.]

     If you encounter an error message complaining about a library not
     being found, you cannot use this binary ``microcode'' on your
     system.  You may try again with a different binary release or you
     can embrace fate and resort to a ``cold'' build (see below).

     Once loaded, try evaluating something trivial (like:  0 or #t
     followed by carriage [RETURN] or [ENTER]) to verify that it has
     loaded successfully and can evaluate constants.

     Then try something like:  (* 5 4 3 2 1)  then maybe:  (pp pp)
     for laughs, and maybe:  (gc-flip 0) for kicks.  Finally, quit by
     typing:  (exit)

     These simple tests verify, respectively, that the microcode can
     load, that the microcode and the run-time band are compatible,
     that the library path points to compiler-compatible debugging
     info files--- in case anything goes wrong down stream, it helps
     to see what--- and, finally, that the band's heap is stable.

     If this generates an error about an incompatibility between the
     microcode and runtime then double check your work above since the
     microcode and runtime directories shipped in a binary release are
     always compatible.  One telltale of this problem is when Scheme
     crashes with a segmentation fault (SIGSEGV) within the critical
     section of the garbage collection daemon.

     Assuming the above has all succeeded, you may now begin the warm
     build process proper, which is only a slight generalization of
     that described in the multi-stage build transition table earlier.


This concludes Step [1] (``Verify that the binary release runs.'').

Once it has been verified that the microcode runs and that the
run-time library is consistent with it, you are prepared to start the
CVS build's bootstrapping process.  The adventure continues!

[2]. Run "./Setup.sh" from the CVS `src' dir using bootstrap binaries.

     BUT FIRST...

     First, you must define a new shell environment variable used to
     bootstrap the build process, since MIT/GNU Scheme uses a few
     Scheme scripts and shell scripts during the `Setup.sh' and `make'
     processes to lift itself up by its bootstraps.  Specifically, you
     must define:

  (ba)sh:
          export    SCHEME_LARGE = \
              "${MITSCHEME_EXECUTABLE} -large -no-init-file"
  (t)csh:
          setenv    SCHEME_LARGE \
              "${MITSCHEME_EXECUTABLE} -large -no-init-file"

     This tells the build scripts where the "scheme" executable file
     lives and how to invoke it with a large heap size.  If it is not
     defined, the build scripts will use the first "scheme" found in
     your shell execution path (shell variable PATH) instead.

     Setting the SCHEME_LARGE environment variable gives us control
     over which "scheme" to use without having to perturb the normal
     PATH setting or, worse, squirreling things away early in your
     normal PATH search sequence.

     This new environment variable is used only by the build scripts:
     please do not confuse it with the normal MIT/GNU Scheme built-in
     environment variables named MITSCHEME_LARGE_(CONSTANT|HEAP|STACK)
     which are run-time, not build-time, configuration parameters.

     With this defined appropriately, invoking "./Setup.sh" should
     succeed.  Look carefully at the output it generates to verify
     that it succeeded in generating the microcode Makefiles.  I.e.,
     the call to `generate-makefile' should not have yielded an error.

     If this fails then the binaries you're using are not compatible
     with the CVS sources you are trying to build.  If you are using
     the current stable binary release with the current CVS tree then
     please send a bug report via the Project GNU bug tracking system
     for MIT/GNU Scheme [ http://www.gnu.org/software/mit-scheme/ ] as
     this might reflect an incompatibility in the development branch.

[3]. Run "./configure" from the CVS `src' directory.

     This does not require a Scheme executable so it should always
     succeed.  If not, please send a bug report as described in the
     preceding step.

[4]. Invoke "make" from the CVS `src' dir using newly built microcode.

     BUT FIRST...

     Now you must define a small set of shell environment variables
     used to bootstrap the Scheme run-time compilation process.
     Specifically, you must define (while positioned within the CVS
     `src' directory):

  (ba)sh:
          export         MITSCHEME_LIBRARY_PATH = \
                      "${MITSCHEME_BOOTSTRAP_DIR}/lib/mit-scheme"

          export            SCHEME_LARGE = \
          "${PWD}/microcode/scheme -large    -heap 4000 -no-init-file"

          export            SCHEME_COMPILER = \
          "${PWD}/microcode/scheme -compiler -heap 4000 -no-init-file"

  (t)csh:
          setenv         MITSCHEME_LIBRARY_PATH \
                      "${MITSCHEME_BOOTSTRAP_DIR}/lib/mit-scheme"

          setenv            SCHEME_LARGE \
          "${PWD}/microcode/scheme -large    -heap 4000 -no-init-file"

          setenv            SCHEME_COMPILER \
          "${PWD}/microcode/scheme -compiler -heap 4000 -no-init-file"

     Please note that this uses the bootstrap binary run-time files
     (as before) but with the newly generated microcode produced by
     compiling the CVS sources in the `src/microcode/' directory _and_
     with a Scheme heap size large enough to handle the entire build.

     This works because the run-time bands do not depend on dynamic
     system libraries (like `glibc') or other low-level bits and
     pieces.  Specifically, run-time bands must be compatible with
     the underlying microcode versions atop which they run but the
     run-time bands do not link directly into the microcode file (nor
     vice versa) so they are functionally coupled but not bit-wise
     coupled nor directly linked at the object code level.  This is
     the purpose of the "utabmd.bin" file in `lib/'.

     Together, these tell the build scripts where the new CVS "scheme"
     microcode file will be made, how to invoke it with a very large
     heap size and/or with the native code compiler loaded (both with
     sufficient heap space to compile the entire CVS `src/runtime/'
     contents), and where to find compatible run-time and compiler
     bands to use while compiling this new CVS run-time and again
     while loading then dumping various pre-defined collections of
     these newly compiled run-time files constituting the various
     run-time ``bands'' built in the CVS `lib/' subdirectory.

     Finally, please note that "make" will compile the entire run-time
     library, compiler sources, and other auxiliary Scheme modules.

     This takes a fairly long time and produces a copious amount of
     output chatter.  It may be wise, therefore, to copy its output
     and error streams into a files that can be inspected afterward to
     look for any non-fatal warnings or non-show-stopping errors that
     may nonetheless be symptomatic of problems that might bite later.
     Using your shell's output redirection compatibilities or piping
     the output(s) through the unix `tee' utility should prove useful.

[5]. Cleaning up loose ends... and maybe installing your new build.

[5a].__________
     To install...

     If you would now like to install this newly built MIT/GNU Scheme
     as the system-wide default, type the following:

         make install

     Afterward, before proceeding, take care to clean up the various
     shell environment variables used during the directed build.

     Depending on which interactive shell you use, do one of these
     (but _only_ if you typed `make install' above):

  (ba)sh:
         unset    MITSCHEME_BOOTSTRAP_DIR
         unset    MITSCHEME_MICROCODE_DIR
         unset    MITSCHEME_EXECUTABLE
         unset    MITSCHEME_LIBRARY_PATH
         unset       SCHEME_LARGE
         unset       SCHEME_COMPILER

  (t)csh:
         unsetenv MITSCHEME_BOOTSTRAP_DIR
         unsetenv MITSCHEME_MICROCODE_DIR
         unsetenv MITSCHEME_EXECUTABLE
         unsetenv MITSCHEME_LIBRARY_PATH
         unsetenv    SCHEME_LARGE
         unsetenv    SCHEME_COMPILER

[5b]._________________
     Or not to install...

     If, rather than installing the newly built MIT/GNU Scheme, you
     prefer to use it in place (or somewhere else), be sure to adjust
     the various environment variables accordingly.

     In particular, take special care to snap MITSCHEME_LIBRARY_PATH
     to point to your newly compiled library (with no trailing `/'):

  (ba)sh:
          export MITSCHEME_LIBRARY_PATH = "${PWD}/lib"
  (t)csh:
          setenv MITSCHEME_LIBRARY_PATH   "${PWD}/lib"

     Note, however, that in order to invoke your new Scheme microcode,
     your will have to use either its absolute pathname, a relative
     path name relative to what working directory you are in when you
     invoke it, or else invoke it as $SCHEME_LARGE or $SCHEME_COMPILER
     (thereby continuing to use the environment variables established
     to finish the build).  You may also choose to install symbolic
     links in your exec PATH to point it or other such shenanigans.

Good luck and happy hacking!

Building from source on unix systems - ``MIT/GNU Scheme Tabula Rasa''
====================================

 Context:  A ``binary'' release includes only:  1) a `bin/' directory
           containing the MIT/GNU Scheme ``microcode'' executable(s),
           and:  2) the `lib/mit-scheme/' directory containing the
           MIT/GNU Scheme ``runtime'' bands, debugging info, and such
           that correspond to that version of the companion microcode.
           Binary releases do not contain source code files.

    Fact:  Although the ``runtime'' files are system-independent, the
           microcode file(s) are linked against dynamic OS libraries.
           This means that a binary microcode release may fail to run
           on your specific machine if it has missing or incompatible
           dynamic libraries installed (like, in `/usr/include/').

Question:  What can you do when the "scheme" microcode from a binary
           release cannot be run on your system, perhaps due to old
           or missing or incompatible dynamic system libraries, etc.?

  Answer:  Build the (C-based) "scheme" microcode from the separately
           distributed CPU+OS-specific source distribution.  Afterward
           you can use this new locally-built microcode in combination
           with the corresponding binary release's run-time library.

 Insight:  The key trick idea is that any given version of the MIT/GNU
           Scheme runtime library is built to run atop a corresponding
           release of the MIT/GNU Scheme microcode but they are only
           functionally coupled, not low-level bit-wise coupled at the
           link layer or any such nightmare.  This means you can brew
           your own custom executable microcode specific to your CPU
           and OS configuration from the ``ucode'' source distribution
           and it will still work with the pre-compiled binary release
           of the matching runtime library from the binary releases.
           (By the way, this flexible linkage is accomplished courtesy
           of the "utabmd.bin" file in `lib/'.  Thank you, "utabmd"!)

 Details:  Following is a bit more context and detail followed by the
           instructions for building just the MIT/GNU Scheme microcode
           file(s) from sources, step by step.  You may prefer to skip
           directly to the enumerated step-by-step instructions if you
           don't care about the gory details of what you're doing or
           why.  Ignorance = Bliss.

The following describes how to build a compatible microcode for the
binary release corresponding to the CVS sources.  In order to be
explicit, it presumes that the target binary release is Release 7.7.1
on an Intel x86 compatible system running GNU/Linux.  Adjust what
follows appropriately for other releases, CPUs and/or unix variants.

For example, one might reasonably prefer to bootstrap from the most
recent "snapshot" instead of the most recent "stable" release, in
which case one would replace `mit-scheme-7.7.1' below with something
like `mit-scheme-20060414' [http://www.gnu.org/software/mit-scheme/].

This also presumes that your goal is merely to build a given binary
release's microcode file(s) locally, not to install them system wide.
To that end, it does not contain the usual Step #6:  make install
from the normal build state transition ritual.

The idea is to build the microcode corresponding to the target binary
release, doing so manually and separately from both the CVS tree and
the binary release tree.  This task is accomplished by downloading,
unpacking and building the target microcode from (non-CVS) sources
then copying the resulting microcode into your unpacked binary release
directory structure, replacing the non-runnable microcode file(s) with
the manually built ones built specially for your system.

Once done, the resulting patched binary release directory can be used
as in a normal ``warm'' build from CVS sources.  In other words, at
the end of the following action list, go to step [1] (not [0]) of the
``warm'' build instructions and proceed from there as usual.

That being said, ...

``Cold'' build using non-matching binary and CVS source releases
----------------------------------------------------------------

[Adapted from: http://www.gnu.org/software/mit-scheme/other-unix.html]

1. Download the standard binary package for your release, CPU & unix
   (e.g., mit-scheme-7.7.1-ix86-gnu-linux.tar.gz) and the sources
   (e.g., mit-scheme-7.7.1-src-ucode.tar.gz).  The _full_ sources
   (e.g., mit-scheme-7.7.1-src.tar.gz) also works but that's overkill.

2. Unpack the source package and `cd' to it's `src/microcode/' subdir:

   tar xzf mit-scheme-7.7.1-src-ucode.tar.gz

   cd          scheme-7.7.1/src/microcode/

3. Configure the microcode directory (from `src/microcode/'):

   ./configure

   By having downloaded the microcode source distribution for a
   specific MIT/GNU Scheme release, CPU and unix variant, we avoid the
   need to run "src/Setup.sh" to go from the "CVS" build state to the
   "distribution" build state.  (Once fully built, our resulting local
   microcode can then be used to advance the CVS source directory from
   its unpacked "CVS" state to its "distribution" state in order to
   proceed building the full MIT/GNU Scheme system.  This buys us the
   needed bootstrapping leverage to proceed where we might otherwise
   have failed due to lack of a compatible binary microcode release.)

4. Compile the program (from `src/microcode/'):

   make

   Note that, being in the unpacked `src/microcode/' directory, only
   the microcode is built, not any run-time or compiler or other
   files, so not bootstrapping MIT/GNU Scheme is needed (yet).  To
   wit, those parts of the total CVS build that would have required a
   bootstrapping MIT/GNU Scheme are sidestepped here since:  1) the
   microcode table set up is anticipated in the release+CPU+unix-
   specific source distribution, and:  2) the microcode-only "make"
   invocation terminates without going on to try to compile any
   run-time or other Scheme files.  In short, building the microcode
   entails only compiling C code, not mucking about with Scheme code.

5. Unpack the standard binary package (if you hadn't already done so
   in the ``warm'' build step #0 above) and copy your newly built
   microcode executables into it:

   cd ../../..

   tar xzf mit-scheme-7.7.1-ix86-gnu-linux.tar.gz  # If not already

   cp -fp      scheme-7.7.1/src/microcode/scheme    bin/.
   cp -fp      scheme-7.7.1/src/microcode/bchscheme bin/.

   Clobbering/replacing the copies of the binary release microcode
   file(s) shouldn't disturb you since, presumably, the main reason
   you resorted to a ``cold'' build in the first place is that the
   normal binary release microcode wouldn't run anyway (or at least
   wasn't as obsessively tuned to your exact system as you imagine
   you'd like it to be).

Now proceed from step #1 (not #0) of the ``warm'' build instructions
above to finish building from the CVS sources.

And good luck!
