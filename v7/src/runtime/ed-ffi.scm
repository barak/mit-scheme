#| -*- Scheme -*-

$Id: ed-ffi.scm,v 1.10 1995/01/06 00:40:35 cph Exp $

Copyright (c) 1988-95 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Edwin buffer packaging info

(declare (usual-integrations))

(standard-scheme-find-file-initialization
 '#(
    ("advice"	(runtime advice)
		syntax-table/system-internal)
    ("arith"	(runtime number)
		syntax-table/system-internal)
    ("bitstr"	()
		syntax-table/system-internal)
    ("binut"	(runtime pc-sample)
		syntax-table/system-internal)
    ("boole"	()
		syntax-table/system-internal)
    ("boot"	()
		syntax-table/system-internal)
    ("char"	(runtime character)
		syntax-table/system-internal)
    ("chrset"	(runtime character-set)
		syntax-table/system-internal)
    ("codwlk"	(runtime scode-walker)
		syntax-table/system-internal)
    ("conpar"	(runtime continuation-parser)
		syntax-table/system-internal)
    ("contin"	(runtime continuation)
		syntax-table/system-internal)
    ("cpoint"	(runtime control-point)
		syntax-table/system-internal)
    ("datime"	(runtime date/time)
		syntax-table/system-internal)
    ("dbgcmd"	(runtime debugger-command-loop)
		syntax-table/system-internal)
    ("dbgutl"	(runtime debugger-utilities)
		syntax-table/system-internal)
    ("dosdir"	(runtime directory)
		syntax-table/system-internal)
    ("dosprm"	()
		syntax-table/system-internal)
    ("dospth"	(runtime pathname dos)
		syntax-table/system-internal)
    ("debug"	(runtime debugger)
		syntax-table/system-internal)
    ("defstr"	(runtime defstruct)
		syntax-table/system-internal)
    ("dragon4"	(runtime number)
		syntax-table/system-internal)
    ("emacs"	(runtime emacs-interface)
		syntax-table/system-internal)
    ("equals"	()
		syntax-table/system-internal)
    ("error"	(runtime error-handler)
		syntax-table/system-internal)
    ("events"	(runtime event-distributor)
		syntax-table/system-internal)
    ("fileio"	(runtime file-i/o-port)
		syntax-table/system-internal)
    ("fixart"	()
		syntax-table/system-internal)
    ("format"	(runtime format)
		syntax-table/system-internal)
    ("framex"	(runtime debugging-info)
		syntax-table/system-internal)
    ("gc"	(runtime garbage-collector)
		syntax-table/system-internal)
    ("gcdemn"	(runtime gc-daemons)
		syntax-table/system-internal)
    ("gcnote"	(runtime gc-notification)
		syntax-table/system-internal)
    ("gcstat"	(runtime gc-statistics)
		syntax-table/system-internal)
    ("gdatab"	(runtime global-database)
		syntax-table/system-internal)
    ("genio"	(runtime generic-i/o-port)
		syntax-table/system-internal)
    ("gensym"	(runtime gensym)
		syntax-table/system-internal)
    ("global"	()
		syntax-table/system-internal)
    ("graphics"	(runtime graphics)
		syntax-table/system-internal)
    ("hash"	(runtime hash)
		syntax-table/system-internal)
    ("hashtb"	(runtime hash-table)
		syntax-table/system-internal)
    ("histry"	(runtime history)
		syntax-table/system-internal)
    ("infstr"	(runtime compiler-info)
		syntax-table/system-internal)
    ("infutl"	(runtime compiler-info)
		syntax-table/system-internal)
    ("input"	(runtime input-port)
		syntax-table/system-internal)
    ("intrpt"	(runtime interrupt-handler)
		syntax-table/system-internal)
    ("io"	(runtime primitive-io)
		syntax-table/system-internal)
    ("krypt"	(runtime krypt)
		syntax-table/system-internal)
    ("lambda"	(runtime lambda-abstraction)
		syntax-table/system-internal)
    ("lambdx"	()
		syntax-table/system-internal)
    ("list"	(runtime list)
		syntax-table/system-internal)
    ("load"	(runtime load)
		syntax-table/system-internal)
    ("macros"	(runtime macros)
		syntax-table/system-internal)
    ("msort"	()
		syntax-table/system-internal)
    ("numint"	(runtime number interface)
		syntax-table/system-internal)
    ("numpar"	(runtime number-parser)
		syntax-table/system-internal)
    ("option"	(runtime options)
		syntax-table/system-internal)
    ("os2dir"	(runtime directory)
		syntax-table/system-internal)
    ("os2graph"	(runtime os2-graphics)
		syntax-table/system-internal)
    ("os2prm"	()
		syntax-table/system-internal)
    ("os2winp"	(runtime os2-window-primitives)
		syntax-table/system-internal)
    ("output"	(runtime output-port)
		syntax-table/system-internal)
    ("packag"	(package)
		syntax-table/system-internal)
    ("parse"	(runtime parser)
		syntax-table/system-internal)
    ("partab"	(runtime parser-table)
		syntax-table/system-internal)
    ("pathnm"	(runtime pathname)
		syntax-table/system-internal)
    ("pcsample" (runtime pc-sample)
		syntax-table/system-internal)
    ("pcscobl"  (runtime pc-sample  code-blocks)
		syntax-table/system-internal)
    ("pcsdisp"  (runtime pc-sample display)
		syntax-table/system-internal)
    ("pcsiproc" (runtime pc-sample interp-procs)
		syntax-table/system-internal)
    ("poplat"	(runtime population)
		syntax-table/system-internal)
    ("port"	(runtime port)
		syntax-table/system-internal)
    ("pp"	(runtime pretty-printer)
		syntax-table/system-internal)
    ("prgcop"	(runtime program-copier)
		syntax-table/system-internal)
    ("process"	(runtime subprocess)
		syntax-table/system-internal)
    ("prop1d"	(runtime 1d-property)
		syntax-table/system-internal)
    ("prop2d"	(runtime 2D-property)
		syntax-table/system-internal)
    ("qsort"	()
		syntax-table/system-internal)
    ("queue"	()
		syntax-table/system-internal)
    ("random"	(runtime random-number)
		syntax-table/system-internal)
    ("record"	(runtime record)
		syntax-table/system-internal)
    ("rep"	(runtime rep)
		syntax-table/system-internal)
    ("savres"	(runtime save/restore)
		syntax-table/system-internal)
    ("scan"	(runtime scode-scan)
		syntax-table/system-internal)
    ("scode"	(runtime scode)
		syntax-table/system-internal)
    ("scomb"	(runtime scode-combinator)
		syntax-table/system-internal)
    ("sdata"	(runtime scode-data)
		syntax-table/system-internal)
    ("sfile"	()
		syntax-table/system-internal)
    ("socket"	(runtime socket)
		syntax-table/system-internal)
    ("starbase"	(runtime starbase-graphics)
		syntax-table/system-internal)
    ("stream"	(runtime stream)
		syntax-table/system-internal)
    ("string"	()
		syntax-table/system-internal)
    ("strnin"	(runtime string-input)
		syntax-table/system-internal)
    ("strott"	(runtime truncated-string-output)
		syntax-table/system-internal)
    ("strout"	(runtime string-output)
		syntax-table/system-internal)
    ("syntab"	(runtime syntax-table)
		syntax-table/system-internal)
    ("syntax"	(runtime syntaxer)
		syntax-table/system-internal)
    ("sysclk"	(runtime system-clock)
		syntax-table/system-internal)
    ("sysmac"	(runtime system-macros)
		syntax-table/system-internal)
    ("system"	(runtime system)
		syntax-table/system-internal)
    ("thread"	(runtime thread)
		syntax-table/system-internal)
    ("tscript"	(runtime transcript)
		syntax-table/system-internal)
    ("ttyio"	(runtime console-i/o-port)
		syntax-table/system-internal)
    ("udata"	()
		syntax-table/system-internal)
    ("uenvir"	(runtime environment)
		syntax-table/system-internal)
    ("uerror"	(runtime microcode-errors)
		syntax-table/system-internal)
    ("unpars"	(runtime unparser)
		syntax-table/system-internal)
    ("unsyn"	(runtime unsyntaxer)
		syntax-table/system-internal)
    ("unxdir"	(runtime directory)
		syntax-table/system-internal)
    ("unxprm"	()
		syntax-table/system-internal)
    ("unxpth"	(runtime pathname unix)
		syntax-table/system-internal)
    ("uproc"	(runtime procedure)
		syntax-table/system-internal)
    ("urtrap"	(runtime reference-trap)
		syntax-table/system-internal)
    ("usrint"	(runtime user-interface)
		syntax-table/system-internal)
    ("utabs"	(runtime microcode-tables)
		syntax-table/system-internal)
    ("vector"	()
		syntax-table/system-internal)
    ("version"	(runtime)
		syntax-table/system-internal)
    ("where"	(runtime environment-inspector)
		syntax-table/system-internal)
    ("wind"	(runtime state-space)
		syntax-table/system-internal)
    ("wrkdir"	(runtime working-directory)
		syntax-table/system-internal)
    ("x11graph"	(runtime X-graphics)
		syntax-table/system-internal)
    ("xeval"	(runtime extended-scode-eval)
		syntax-table/system-internal)
    ("xeval"	(runtime stepper)
		syntax-table/system-internal)))