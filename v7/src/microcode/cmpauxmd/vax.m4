### -*-Midas-*-
###
### $Id: vax.m4,v 1.4 2000/12/05 21:23:50 cph Exp $
###
### Copyright (c) 1991-2000 Massachusetts Institute of Technology
###
### This program is free software; you can redistribute it and/or
### modify it under the terms of the GNU General Public License as
### published by the Free Software Foundation; either version 2 of the
### License, or (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with this program; if not, write to the Free Software
### Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
###

#### Vax assembly language (BSD as Syntax) part of the compiled code
#### interface.  See cmpint.txt, cmpaux.txt, cmpint.c, cmpint-vax.h,
#### and cmpgc.h for more documentation.
####
#### NOTE:
####	Assumptions:
####
####	1) C uses CALLS and RET for linkage.
####
####	2) The C compiler divides registers into three groups:
####	- Linkage registers, used for procedure calls and global
####	references.  On Vax: ap, fp, sp, pc.
####	- Super temporaries, not preserved accross procedure calls and
####	always usable.  On the Vax this depends on the compiler:
####	GCC and BSD PCC use r0-r5 as super temporaries.
####	The (VMS) Vax C compiler uses r0-r1 as super temporaries.
####	- Preserved registers saved by the callee if they are written.
####	On the Vax: all others.
####
####	3) C procedures return long values in r0.
####	Two word structures are returned in different ways:
####	o GCC returns them in r0/r1.  Define flag GCC.
####	o PCC returns the address of the structure (in static storage)
####	in r0.  This is the default.
####	o (VMS) Vax C passes the address of the destination structure
####	as the first argument.  The other arguments are shifted right.
####
#### Compiled Scheme code uses the following register convention:
####	- sp (r14) contains the Scheme stack pointer, not the C stack
####	pointer.
####	- fp (r13) contains the dynamic link when needed.
####	- ap (r12) contains the Scheme free pointer.
####	- r11 contains the Scheme datum mask.
####	- r10 contains a pointer to the Scheme interpreter's
####	"register" block.  This block contains the compiler's copy of
####	MemTop, the interpreter's registers (val, env, exp, etc),
####	temporary locations for compiled code, and the mechanism used
####	to invoke the hooks in this file.
####	- r9 is where Scheme compiled code returns values.
####
####	All other registers are available to the compiler.  A
####	caller-saves convention is used, so the registers need not be
####	preserved by subprocedures.
####
#### MAJOR NOTE: $ signifies immediate values to AS on the Vax.
#### However, M4 also uses $ to signify macro constants,
#### thus we use @ here to signify immediate values and a sed script
#### is run on the output of M4 to change them to $.

####	Utility macros and definitions

ifdef(`VMS', `', `ifdef(`GCC',`',`define(PCC,1)')')

ifdef(`VMS',
      `define(HEX,`^X$1')',
      `define(HEX,`0x$1')')


ifdef(`VMS',
      `define(ASMSET,
	      `
	$1=$2')',

      `define(ASMSET,
	      `
	.set $1,$2')')


ifdef(`VMS',
      `define(extern_c_variable,
	      `$1')',
      `define(extern_c_variable,
	      `_$1')')

define(extern_c_label,
       `extern_c_variable($1)')


ifdef(`VMS',
      `define(reference_c_variable,
	      `
	.save_psect
	.psect $1,pic,usr,ovr,rel,gbl,shr,noexe,rd,wrt,novec
$1:
	.restore_psect')',

      `define(reference_c_variable,
	      `')')


ifdef(`VMS',
      `define(define_c_label,
	      `
$1::')',

      `define(define_c_label,
	      `
	.globl	extern_c_label($1)
extern_c_label($1):')')


ifdef(`VMS',
      `define(define_c_procedure,
	      `
	.align	word
define_c_label($1)
	.word	^M<r2,r3,r4,r5,r6,r7,r8,r9,r10,r11>')', # save r2-r11

      `define(define_c_procedure,
	      `
	.align	1
define_c_label($1)
	.word	0x0fc0')')		# save r6-r11

# This must match the compiler (machines/vax/machin.scm)

define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 6))
define(ADDRESS_MASK, eval((0 - (2 ** (32 - TC_LENGTH))), 10))

define(rval,r9)
define(regs,r10)
define(rmask,r11)
define(rfree,ap)
define(dlink,fp)

ASMSET(regblock_val,8)
ASMSET(address_mask,ADDRESS_MASK)

reference_c_variable(Ext_Stack_Pointer)
reference_c_variable(Free)
reference_c_variable(Registers)
reference_c_variable(utility_table)

###
### Global data
###

ifdef(`VMS',
      `
	.psect	code,nowrt,exe,long
	.psect	data,wrt,noexe,quad

	.psect	data
	.align	long
c_save_stack:
	.long	0

	.psect	code',
      `
	.data
	.align	2
	.comm	c_save_stack,4

	.text')

### Callable by C conventions.  Swaps to Scheme register set and jumps
### to the entry point specified by its only argument.

define_c_procedure(C_to_interface)
	movl	4(ap),r1		# Argument: entry point
	subl2	@8,sp			# Allocate space for return struct.
	pushl	ap			# Save while in Scheme
	pushl	fp
	movl	@address_mask,rmask
	moval	extern_c_variable(Registers),regs

### The data in r1 is the address of an entry point to invoke.

define_c_label(interface_to_scheme)
					# Swap to C registers
	movl	sp,c_save_stack
	movl	extern_c_variable(Ext_Stack_Pointer),sp
	movl	extern_c_variable(Free),rfree
					# Scheme return value
	movl	regblock_val(regs),rval
	bicl3	rmask,rval,dlink
	jmp	(r1)			# invoke entry point

### The data in r1 is a return code to the interpreter

define_c_label(interface_to_C)
	movl	r1,r0			# C return location
	ret

### Called by Scheme through a jump instruction in the register block.
### It expects an index in r0, a return address on the stack, and 3
### longword arguments in r2-r4.
### The return address needs to be bumped over the format longword.

define_c_label(asm_scheme_to_interface_jsb)
	addl3	@4,(sp)+,r1
#	brb	asm_scheme_to_interface

### Transfer procedure from Scheme to C.
### Called by Scheme through a jump instruction in the register block.
### It expects an index in r0, and 4 longword arguments in r1-r4

define_c_label(asm_scheme_to_interface)
					# Swap to C registers
	movl	rval,regblock_val(regs)
	movl	rfree,extern_c_variable(Free)
	movl	sp,extern_c_variable(Ext_Stack_Pointer)
	movl	c_save_stack,sp
	movl	(sp),fp
	movl	4(sp),ap
					# extract the C utility
	moval	extern_c_variable(utility_table),r6
	movl	(r6)[r0],r6
					# push arguments to utility
	pushl	r4
	pushl	r3
	pushl	r2
	pushl	r1
					# call C procedure
	ifdef(`VMS',
	      `pushab	24(sp)
	       calls	@5,(r6)',
	      `calls	@4,(r6)')
					# return struct -> r0/r1
	ifdef(`VMS',
	      `movl	28(sp),r1
	       movl	24(sp),r0')
	ifdef(`PCC',
	      `movl	4(r0),r1
	       movl	(r0),r0')

	jmp	(r0)			# invoke return handler

### Called by Scheme trampolines through a jump instruction in the
### register block.
### It expects an index in r0, and a return address on the stack.
### The return address needs to be bumped over the padding in the
### trampoline.

define_c_label(asm_trampoline_to_interface)
	addl3	@2,(sp)+,r1
	brb	extern_c_label(asm_scheme_to_interface)

ifdef(`VMS',
      `.end',
      `')
