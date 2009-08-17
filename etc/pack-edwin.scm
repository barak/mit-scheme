#| -*-Scheme-*-

$Id: 905a6b66958832e0ccfae7d0dde44e9538b88c08 $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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

;;;; File to generate a single loadable file for Edwin

(define (pack-edwin output)
  (pack-binaries output
                 '(("/scheme/edwin"
		    "make.com"
		    "/scheme/lib/options/rbtree.com"
		    "edwin.bcon"
		    "edwin.bldr"
		    "edwin.bad"
		    "utils.com"
		    "nvector.com"
		    "ring.com"
		    "strtab.com"
		    "strpad.com"
		    "macros.com"
		    "class.com"
		    "clscon.com"
		    "clsmac.com"
		    "xform.com"
		    "paths.com"
		    "struct.com"
		    "grpops.com"
		    "txtprp.com"
		    "regops.com"
		    "motion.com"
		    "search.com"
		    "image.com"
		    "comman.com"
		    "docstr.com"
		    "comtab.com"
		    "modes.com"
		    "buffer.com"
		    "bufset.com"
		    "undo.com"
		    "display.com"
		    "screen.com"
		    "winren.com"
		    "window.com"
		    "utlwin.com"
		    "bufwin.com"
		    "bufwfs.com"
		    "bufwiu.com"
		    "bufwmc.com"
		    "comwin.com"
		    "modwin.com"
		    "buffrm.com"
		    "edtfrm.com"
		    "calias.com"
		    ;; "xterm.com"
		    ;; "key.com"
		    "termcap.com"
		    "tterm.com"
		    "ansi.com"
		    "bios.com"
		    "edtstr.com"
		    "editor.com"
		    "curren.com"
		    "simple.com"
		    ;; "debuge.com"
		    "modlin.com"
		    "input.com"
		    "prompt.com"
		    "comred.com"
		    "bufinp.com"
		    "bufout.com"
		    "winout.com"
		    "things.com"
		    "tparse.com"
		    "syntax.com"
		    "regexp.com"
		    "rgxcmp.com"
		    "linden.com"
		    ;; "unix.com"
		    "dos.com"
		    "fileio.com"
		    ;; "process.com"
		    "dosproc.com"
		    "argred.com"
		    "autold.com"
		    "autosv.com"
		    "basic.com"
		    "bufcom.com"
		    "bufmnu.com"
		    "c-mode.com"
		    "cinden.com"
		    "comhst.com"
		    ;; "comint.com"
		    ;; "compile.com"
		    ;; "dabbrev.com"
		    ;; "xcom.com"
		    "debug.com"
		    "dired.com"
		    ;; "dirunx.com"
		    "evlcom.com"
		    "filcom.com"
		    "fill.com"
		    "hlpcom.com"
		    ;; "info.com"
		    "intmod.com"
		    "keymap.com"
		    "kilcom.com"
		    "kmacro.com"
		    "lincom.com"
		    "lspcom.com"
		    ;; "malias.com"
		    "motcom.com"
		    ;; "occur.com"
		    ;; "outline.com"
		    ;; "rcs.com"
		    "reccom.com"
		    "regcom.com"
		    "replaz.com"
		    ;; "rmail.com"
		    ;; "rmailsrt.com"
		    "schmod.com"
		    ;; "sendmail.com"
		    "sercom.com"
		    "iserch.com"
		    ;; "shell.com"
		    "tagutl.com"
		    "texcom.com"
		    "wincom.com"
		    "scrcom.com"
		    "modefs.com"
		    ;; "xmodef.com"
		    "rename.com"
		    "loadef.com"
		    ;; "bochser.com"
		    ;; "bochsmod.com"
		    ;; "notify.com"
		    ))))