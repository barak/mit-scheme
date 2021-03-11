#| -*-Scheme-*-

Copyright (c) 1993-96 Massachusetts Institute of Technology

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

;;;; File to generate a single loadable file for SF and Liar (Intel 386)

(if (not (environment-bound? system-global-environment 'PACK-BINARIES))
    (load (merge-pathnames "pack" (directory-pathname (current-load-pathname)))
	  '(RUNTIME LOAD)))

(define (pack-compiler #!optional output)
  (pack-binaries (if (default-object? output) "lib/compdel.com" output)
		 '(("sf"
		    "make.com"
		    "sf.bco"
		    "sf.bld"
		    "lsets.com"
		    "table.com"
		    "pthmap.com"
		    "object.com"
		    "emodel.com"
		    "gconst.com"
		    "usicon.com"
		    "tables.com"
		    "gimprt.com"
		    "toplev.com"
		    "xform.com"
		    "subst.com"
		    "cgen.com"
		    "usiexp.com"
		    "reduct.com"
		    "pardec.com"
		    "copy.com"
		    "free.com"
		    "chtype.com"
		    "butils.com")
		   ("compiler"
		    "machines/i386/make.com"
		    "base/make.com"
		    "compiler.bco"
		    "compiler.bld"
		    "base/switch.com"
		    "base/object.com"
		    "base/enumer.com"
		    "base/sets.com"
		    "base/mvalue.com"
		    "base/scode.com"
		    "machines/i386/machin.com"
		    "back/asutl.com"
		    "base/utils.com"
		    "base/cfg1.com"
		    "base/cfg2.com"
		    "base/cfg3.com"
		    "base/ctypes.com"
		    "base/rvalue.com"
		    "base/lvalue.com"
		    "base/blocks.com"
		    "base/proced.com"
		    "base/contin.com"
		    "base/subprb.com"
		    "rtlbase/rgraph.com"
		    "rtlbase/rtlty1.com"
		    "rtlbase/rtlty2.com"
		    "rtlbase/rtlexp.com"
		    "rtlbase/rtlcon.com"
		    "rtlbase/rtlreg.com"
		    "rtlbase/rtlcfg.com"
		    "rtlbase/rtlobj.com"
		    "rtlbase/regset.com"
		    "rtlbase/valclass.com"
		    "back/insseq.com"
		    "base/refctx.com"
		    "base/macros.com"
		    "machines/i386/decls.com"
		    "base/toplev.com"
		    "base/crstop.com"
		    "base/asstop.com"
		    "base/debug.com"
		    "base/pmlook.com"
		    "base/pmpars.com"
		    "base/pmerly.com"
		    "base/infnew.com"
		    "base/constr.com"
		    "fggen/canon.com"
		    "fggen/fggen.com"
		    "fggen/declar.com"
		    "fgopt/outer.com"
		    "fgopt/sideff.com"
		    "fgopt/folcon.com"
		    "fgopt/operan.com"
		    "fgopt/varind.com"
		    "fgopt/envopt.com"
		    "fgopt/closan.com"
		    "fgopt/contan.com"
		    "fgopt/offset.com"
		    "fgopt/conect.com"
		    "fgopt/delint.com"
		    "fgopt/desenv.com"
		    "fgopt/blktyp.com"
		    "fgopt/simple.com"
		    "fgopt/simapp.com"
		    "fgopt/subfre.com"
		    "fgopt/order.com"
		    "fgopt/reord.com"
		    "fgopt/reuse.com"
		    "fgopt/param.com"
		    "fgopt/reteqv.com"
		    "rtlgen/rtlgen.com"
		    "rtlgen/rgstmt.com"
		    "rtlgen/fndvar.com"
		    "machines/i386/rgspcm.com"
		    "rtlbase/rtline.com"
		    "rtlgen/rgproc.com"
		    "rtlgen/opncod.com"
		    "rtlgen/fndblk.com"
		    "rtlgen/rgrval.com"
		    "rtlgen/rgcomb.com"
		    "rtlgen/rgretn.com"
		    "rtlopt/rcse1.com"
		    "rtlopt/rcse2.com"
		    "rtlopt/rcseep.com"
		    "rtlopt/rcseht.com"
		    "rtlopt/rcserq.com"
		    "rtlopt/rcsesr.com"
		    "rtlopt/rdebug.com"
		    "rtlopt/rinvex.com"
		    "rtlopt/rtlcsm.com"
		    "rtlopt/rdflow.com"
		    "rtlopt/rerite.com"
		    "rtlopt/rlife.com"
		    "rtlopt/rcompr.com"
		    "rtlopt/ralloc.com"
		    "back/lapgn1.com"
		    "back/lapgn2.com"
		    "back/lapgn3.com"
		    "back/regmap.com"
		    "machines/i386/lapgen.com"
		    "machines/i386/rules1.com"
		    "machines/i386/rules2.com"
		    "machines/i386/rules3.com"
		    "machines/i386/rules4.com"
		    "machines/i386/rulfix.com"
		    "machines/i386/rulflo.com"
		    "machines/i386/rulrew.com"
		    "back/syntax.com"
		    "back/syerly.com"
		    "machines/i386/coerce.com"
		    "back/asmmac.com"
		    "machines/i386/insmac.com"
		    "machines/i386/inerly.com"
		    "machines/i386/insutl.com"
		    "machines/i386/instr1.com"
		    "machines/i386/instr2.com"
		    "machines/i386/instrf.com"
		    "back/mermap.com"
		    "back/linear.com"
		    "machines/i386/lapopt.com"
		    "machines/i386/assmd.com"
		    "back/symtab.com"
		    "back/bitutl.com"
		    "back/bittop.com"))))