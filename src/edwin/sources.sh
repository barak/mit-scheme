#!/bin/sh
# -*-Scheme-*-
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
#     2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
#     Massachusetts Institute of Technology
#
# This file is part of MIT/GNU Scheme.
#
# MIT/GNU Scheme is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
# 
# MIT/GNU Scheme is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with MIT/GNU Scheme; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
# USA.

# Generate a list of all source files.

set -e
: ${MIT_SCHEME_EXE=mit-scheme}
${MIT_SCHEME_EXE} --batch-mode -- $1 <<\EOF
(begin

  (define (filenames)
    '("abbrev" "ansi" "argred" "artdebug" "autold" "autosv" "basic"
      "bios" "bufcom" "buffer" "buffrm" "bufinp" "bufmnu" "bufout"
      "bufset" "bufwfs" "bufwin" "bufwiu" "bufwmc" "c-mode" "calias"
      "cinden" "class" "clscon" "clsmac" "comatch" "comhst" "comint"
      "comman" "compile" "comred" "comtab" "comwin" "curren" "dabbrev"
      "debian-changelog" "debug" "debuge" "diff" "dired" "dirunx"
      "dirw32" "display" "docstr" "dos" "doscom" "dosfile" "dosproc"
      "dosshell" "ed-ffi" "editor" "edtfrm" "edtstr" "evlcom" "eystep"
      "filcom" "fileio" "fill" "grpops" "hlpcom" "htmlmode" "image"
      "info" "input" "intmod" "iserch" "javamode" "key-w32" "keymap"
      "keyparse" "kilcom" "kmacro" "lincom" "linden" "lisppaste"
      "loadef" "lspcom" "macros" "make" "malias" "manual" "midas"
      "modefs" "modes" "modlin" "modwin" "motcom" "motion" "mousecom"
      "nntp" "notify" "nvector" "occur" "outline" "paredit" "pasmod"
      "paths" "print" "process" "prompt" "pwedit" "pwparse" "rcsparse"
      "reccom" "regcom" "regexp" "regops" "replaz" "rfc822"
      "ring" "rmail" "rmailsrt" "rmailsum" "schmod" "scrcom" "screen"
      "search" "sendmail" "sercom" "shell" "simple" "snr" "sort"
      "string" "strpad" "strtab" "struct" "syntax" "tagutl" "techinfo"
      "telnet" "termcap" "texcom" "things" "tparse" "tterm" "tximod"
      "txtprp" "undo" "unix" "utils" "utlwin" "vc" "vc-bzr" "vc-cvs"
      "vc-git" "vc-rcs" "vc-svn" "verilog" "vhdl" "webster" "win32"
      "win32com" "wincom" "window" "winout" "world-monitor"
      "xform" "xterm"))

  (define (my-write . strings)
    (for-each write-string strings))

  (define (my-write-line . strings)
    (apply my-write strings) (newline))

  (let ((command (car (if (environment-bound? system-global-environment
					      'command-line-arguments)
			  (command-line-arguments)
			  (command-line))))
	(files (filenames)))
    (cond ((string=? command "scm")
	   (for-each (lambda (name) (my-write " "name))
		     (sort (map (lambda (pathname)
				  (->namestring (pathname-new-type pathname "scm")))
				files)
			   string<?)))
	  ((string=? command "com")
	   (for-each
	     (lambda (name) (my-write " "name".bci "name".com"))
	     (sort (map ->namestring files) string<?)))
	  ((string=? command "deps")
	   (for-each
	     (lambda (name)
	       (my-write-line name".bci: stamp-scheme")
	       (my-write-line name".com: stamp-scheme"))
	     (sort (map ->namestring files) string<?)))
	  (else
	   (error "Unexpected command:" command))))
  )
EOF
