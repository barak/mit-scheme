 ### -*- Midas -*-
 ### 
 ### Copyright (C) 1992 Digital Equipment Corporation (D.E.C.)
 ### Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
 ###     1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 ###     2004, 2005, 2006, 2007, 2008, 2009, 2010 Massachusetts
 ###     Institute of Technology
 ### 
 ### This software was developed at the Digital Equipment Corporation
 ### Cambridge Research Laboratory.  Permission to copy this software, to
 ### redistribute it, and to use it for any purpose is granted, subject to
 ### the following restrictions and understandings.
 ### 
 ### 1. Any copy made of this software must include this copyright notice
 ### in full.
 ### 
 ### 2. Users of this software agree to make their best efforts (a) to
 ### return to both the Digital Equipment Corporation Cambridge Research
 ### Lab (CRL) and the MIT Scheme project any improvements or extensions
 ### that they make, so that these may be included in future releases; and
 ### (b) to inform CRL and MIT of noteworthy uses of this software.
 ### 
 ### 3. All materials developed as a consequence of the use of this
 ### software shall duly acknowledge such use, in accordance with the usual
 ### standards of acknowledging credit in academic research.
 ### 
 ### 4. D.E.C. has made no warrantee or representation that the operation
 ### of this software will be error-free, and D.E.C. is under no obligation
 ### to provide any services, by way of maintenance, update, or otherwise.
 ### 
 ### 5. In conjunction with products arising from the use of this material,
 ### there shall be no use of the name of the Digital Equipment Corporation
 ### nor of any adaptation thereof in any advertising, promotional, or
 ### sales literature without prior written consent from D.E.C. in each
 ### case.

 ### Alpha Architecture assembly language part of the compiled
 ### code interface. See cmpint.txt, cmpint.c, cmpint-alpha.h, and
 ### cmpgc.h for more documentation.
 ###
 ### NOTE:
 ###	Assumptions:
 ###
 ###	1) The C compiler divides registers into three groups:
 ###	- Linkage registers, used for procedure calls and global
 ###	references.  On Alpha: 26 (return address), 27 (procedure
 ###	descriptor), 28 (assembler temp), 29 (global pointer),
 ###	30 (stack pointer), and 31 (always 0).
 ###	- super temporaries, not preserved accross procedure calls and
 ###	always usable. On Alpha: 0-8, 16-21 (argument registers), and
 ###	22-25.  Values are returned in 0.
 ###	- preserved registers saved by the callee if they are written.
 ###	On Alpha: 9-14, 15 (frame base)
 ###
 ###	2) Arguments, if passed on a stack, are popped by the caller.
 ###	Thus most "leaf" procedures need not worry about them. On
 ###	Alpha: The first six arguments are passed in registers and
 ###	have no space allocated on the stack.  Integer scalars are
 ###	returned in register 0; floating point scalars are returned
 ###	in fp0; floating point complex numbers are returned in fp0 and
 ###	fp1; structured values are returned through a pointer passed
 ###	in the first argument register and the remaining arguments are
 ###	shifted over by one register.
 ###
 ###	3) There is a hardware or software maintained stack for
 ###	control.  The procedure calling sequence may leave return
 ###	addresses in registers, but they must be saved somewhere for
 ###	nested calls and recursive procedures.  On Alpha: Passed in a
 ###	register, and no slot stack exists.  The return link is in 26.
 ###	The (C) stack pointer is in 30.
 ###
 ###	4) C procedures return long values in a super temporary
 ###	register.  Alpha: two or more word structures are returned in
 ###	a location specified by the contents of the first argument
 ###	register, and all other arguments are shifted over one
 ###	location (i.e.  apparent argument 1 is passed in the register
 ###	usually used for argument 2, etc.)
 ###
 ###	5) On Alpha we don't know the floating point register save
 ###	convention yet.
 ###
 ### Compiled Scheme code uses the following register convention.
 ### Note that scheme_to_interface, the register block, the closure
 ### hook, link_to_interface, compiled_entry_type_bits, and
 ### closure_free are preserved by C calls, but the others are not,
 ### since they change dynamically.  trampoline_to_interface can be
 ### reached at a fixed offset from scheme_to_interface.
 ###
 ### Register Usage Information
 ### Number  .dis   C                 Scheme
 ### ======  ====   =======           ======
 ### 0       v0     Return Value      Return Value
 ### 1       t0     caller saves      <free, but utility index (not shifted)>
 ### 2       t1     caller saves      Stack-Pointer
 ### 3       t2     caller saves      MemTop
 ### 4       t3     caller saves      Free
 ### 5       t4     caller saves      Dynamic Link
 ### 6       t5     caller saves      <free>
 ### 7       t6     caller saves      <free>
 ### 8       t7     caller saves      <free>
 ### 9       s0     callee saves      Regs-Pointer
 ### 10      s1     callee saves      Scheme-To-Interface
 ### 11      s2     callee saves      Closure Hook (jump ind. for full addr.)
 ### 12      s3     callee saves      Link-To-Interface
 ### 13      s4     callee saves      Compiled-Entry-Type-Bits
 ### 14      s5     callee saves      Closure-Free
 ### 15      fp?    frame base        <free>
 ### 16      a0     argument 1        <free, but for utilities>
 ### 17      a1     argument 2        <free, but for utilities>
 ### 18      a2     argument 3        <free, but for utilities>
 ### 19      a3     argument 4        <free, but for utilities>
 ### 20      a4     argument 5        <free, but for utilities>
 ### 21      a5     argument 6        <free>
 ### 22      t8     caller saves      <free>
 ### 23      t9     caller saves      <free>
 ### 24      t10    caller saves      <free>
 ### 25      t11    caller saves      <free>
 ### 26      ra     return address    <free, but used for closure linkage>
 ### 27      t12    proc. descript.   <free>
 ### 28      at?    volatile scratch  Assembler Temporary (tensioning)
 ### 29      gp     global pointer    <free>
 ### 30      sp     stack pointer     C Stack Pointer (do not use!)
 ### 31      zero   Z E R O           Z E R O

 # The following are derived from cmpint-alpha.h, scaled by 8
#define REGBLOCK_FIRST_EXTRA			128
#define REGBLOCK_ADDRESS_OF_STACK_POINTER	REGBLOCK_FIRST_EXTRA
#define REGBLOCK_ADDRESS_OF_FREE		REGBLOCK_FIRST_EXTRA+8
#define REGBLOCK_ADDRESS_OF_UTILITY_TABLE	REGBLOCK_FIRST_EXTRA+16
#define REGBLOCK_ALLOCATE_CLOSURE		REGBLOCK_FIRST_EXTRA+24

 # The following are derived from const.h, scaled by 8
#define REGBLOCK_MEMTOP			0
#define REGBLOCK_STACKGUARD		8
#define REGBLOCK_VAL			16
#define REGBLOCK_ENV			24
#define REGBLOCK_COMPILER_TEMP		32
#define REGBLOCK_EXPR			40
#define REGBLOCK_RETURN			48
#define REGBLOCK_LEXPR_ACTUALS		56
#define REGBLOCK_PRIMITIVE		64
#define REGBLOCK_CLOSURE_FREE		72
#define REGBLOCK_CLOSURE_SPACE		80

#include "types.h"
#include <machine/pal.h>
	.text	
	.set	noat
	.set	noreorder
	.set	nomacro

 ###	A bunch of .aent pseudo-ops were removed because they generate
 ###	a NOP, and we are counting instructions

 #      .align 16 
 # ^ Apparently the assembler does not like that, but will take the following.
 # Even though the manual says 1-4 is the valid range.

	.align 12
	.ent hook_jump_table 0
	.globl	hook_jump_table
hook_jump_table:
	# All entries in this table must be exactly four
	# instructions long (see lapgen.scm)

 ###	.aent	scheme_closure_hook
	.globl	scheme_closure_hook
scheme_closure_hook:	# Entry 0, Offset 0
	# Compiled code in a closure can be of the form
	#	JMP $26,($11),0
	#	<desired address>
	# when <desired address> is too far away, and $11
	# points here.
	ldq	$22,0($26)
	jmp	$28,($22),0
	nop
	nop

 ###	.aent	asm_allocate_closure
asm_allocate_closure:	# Entry 1, Offset 16
	# This must preserve ALL Scheme allocatable registers
	# $16 has the total number of Scheme objects to allocate
        # $17 has pointer to the first entry point, 16 bytes into the
        #     block we failed to allocate.
	# $28 has the return address
	# Returns an offset 16 bytes into the allocated space in $17
	# It fills the allocated region with 
        #       SUBQ SP,#8,SP//JMP $26,($11),hint
	# and then synchronizes the I- and D-caches for this region of
        # memory.  It also needs to update regnum:closure-free and
        # free (i.e. registers $14 and $4)

	stq	$0,80($sp)
	stq	$1,88($sp)
	ldq	$1,REGBLOCK_ADDRESS_OF_FREE($9)
	br	$31,asm_allocate_continue

asm_allocate_continue:
	stq	$2,96($sp)
	# Register 3 is MemTop
	stq	$4,0($1)	# Store into Free itself
	stq	$5,104($sp)
	stq	$6,112($sp)
	stq	$7,120($sp)
	stq	$8,128($sp)
	# 9 - 15 are callee saves anyway
	# 16 and 17 are the argument registers we are passing through
	stq	$18,136($sp)
	stq	$19,144($sp)
	stq	$20,152($sp)
	stq	$21,160($sp)
	stq	$22,168($sp)
	stq	$23,176($sp)
	stq	$24,184($sp)
	stq	$25,192($sp)
	stq	$26,200($sp)
	stq	$27,208($sp)
	stq	$28,216($sp)
	stq	$29,224($sp)
	# 30 is the stack pointer itself, 31 is ZERO
	ldq	$27,REGBLOCK_ALLOCATE_CLOSURE($9)	
	jsr	$26,($27),allocate_closure
	ldq	$29,REGBLOCK_ADDRESS_OF_FREE($9)
	bis	$0,$0,$17	# Return the value in $17
	ldq	$0,80($sp)
	ldq	$1,88($sp)
	ldq	$2,96($sp)
	ldq	$3,REGBLOCK_MEMTOP($9)
	ldq	$4,0($29)	# Retrieve from Free itself
	ldq	$5,104($sp)
	ldq	$6,112($sp)
	ldq	$7,120($sp)
	ldq	$8,128($sp)
	ldq	$14,REGBLOCK_CLOSURE_FREE($9)
	ldq	$18,136($sp)
	ldq	$19,144($sp)
	ldq	$20,152($sp)
	ldq	$21,160($sp)
	ldq	$22,168($sp)
	ldq	$23,176($sp)
	ldq	$24,184($sp)
	ldq	$25,192($sp)
	ldq	$26,200($sp)
	ldq	$27,208($sp)
	ldq	$28,216($sp)
	ldq	$29,224($sp)
	ret	$28,($28),1
	.end hook_jump_table

	.align 4
	.globl	Flush_I_Cache
	.ent Flush_I_Cache 0
Flush_I_Cache:
	call_pal	PAL_imb
	ret	$28,($26),1
	.end	Flush_I_Cache

 # Argument (in $a0) is a compiled Scheme entry point
 # but save C registers first
 #
 # Frame layout:
#define FRAME_SIZE	232
 #     ....................
 # FS  .                  . <-- Old SP (not our property)
 #     ....................
 #FS-8 . Register save    .
 #     . area used by     .
 #  80 . Allocate_Closure .
 #     ....................
 #  72 . Return str. high .
 #     ....................
 #  64 . Return str. low  .
 #     ....................
 #  56 . Caller's $9 (S0) .
 #     ....................
 #  48 . Caller's $10 (S1).
 #     ....................
 #  40 . Caller's $11 (S2).
 #     ....................
 #  32 . Caller's $12 (S3).
 #     ....................
 #  24 . Caller's $13 (S4).
 #     ....................
 #  16 . Caller's $14 (S5).
 #     ....................
 #  8  . Caller's $15 (FP).
 #     ....................
 #  0  . Our return addr. . <-- New SP
 #     ....................

 # IMPORTANT: If the following sequence is changed,
 # link_to_interface must remain aligned!

	.align	4
	.globl	C_to_interface
	.ent	C_to_interface 1
C_to_interface:
	.set	macro
	ldgp	$gp,0($27)	# Offset 0, 4
	# which expands into (low and high from linker):
	# ldah	$gp,high($t12)
	# lda   $gp,low($gp)
	.set	nomacro
	lda	$sp,-FRAME_SIZE($sp)
				# Offset 8: Allocate frame
	stq	$9,56($sp)	# Offset 12
	stq	$10,48($sp)	# Offset 16
	stq	$11,40($sp)	# Offset 20
	stq	$12,32($sp)	# Offset 24
	stq	$13,24($sp)	# Offset 28
	stq	$14,16($sp)	# Offset 32
	stq	$15,8($sp)	# Offset 36
	stq	$26,0($sp)	# Offset 40
	.mask	0x0400fe00, -FRAME_SIZE
	.frame	$sp,FRAME_SIZE,$26
	br	$12,setup_registers_continue
				# Offset 44

#define LINK_TO_SCHEME 16

 # IMPORTANT: The distance between link_to_interface
 # and scheme_to_interface is fixed at LINK_TO_SCHEME bytes!

 ###	.aent	link_to_interface
link_to_interface:		# Offset 48, SHOULD BE octabyte aligned
	# $1 has utility index
	# $17 (arg 1) has return address from JMP that got you here
	# $18 etc. have other utility arguments if needed
	lda	$17,4($17)	# Skip over format word ...
	br	$28,scheme_to_interface
	nop
	nop	

	.align 4
 ###	.aent 	scheme_to_interface
	.globl 	scheme_to_interface
scheme_to_interface:
	# $1 has utility index (not shifted)
	# $17 (etc.) have utility arguments as needed
	ldq	$24,REGBLOCK_ADDRESS_OF_UTILITY_TABLE($9)	# 0
	stq	$0,REGBLOCK_VAL($9)				# 4
	ldq	$22,REGBLOCK_ADDRESS_OF_STACK_POINTER($9)	# 8
	ldq	$23,REGBLOCK_ADDRESS_OF_FREE($9)		# 12
	stq	$14,REGBLOCK_CLOSURE_FREE($9)
	s8addq	$1,$24,$24	# Address of entry in table	# 16
	stq	$2,0($22)	# Save sp_register		# 20
	ldq	$27,0($24)	# Destination address		# 24
	lda	$16,64($sp)	# Return structure value here	# 28
	stq	$4,0($23)	# Save Free			# 32
	jsr	$26,($27),comutil_operator_arity_trap
	ldq	$22,64($sp)	# Get next procedure address	# 40
	ldq	$16,72($sp)	# Value to pass to next procedure	# 44
	jmp	$28,($22),interface_to_scheme			# 48

 # Argument (in $a0) is a compiled Scheme entry point.  Reload
 # the Scheme registers and go to work...any registers not reloaded
 # here must be callee saves by C.

	.align 4
 ###	.aent	interface_to_scheme
	.globl	interface_to_scheme
interface_to_scheme:
	ldq	$0,REGBLOCK_VAL($9)				# 64
	# Register 1 isn't used
	ldq	$2,REGBLOCK_ADDRESS_OF_STACK_POINTER($9)
	ldq	$3,REGBLOCK_MEMTOP($9)
	ldq	$4,REGBLOCK_ADDRESS_OF_FREE($9)
	ldq	$2,0($2)
	ldq	$4,0($4)
	zap	$0,0x80,$5	# Initialize dynamic link register
	.aent	off_to_scheme_code
	.globl	off_to_scheme_code
off_to_scheme_code:
	jmp	$28,($16),0	# Off to compiled code ...

	.align 4
setup_registers_continue:
	.set	at
	.set	macro
	lda	$9,Registers
 #	lda	$10,scheme_to_interface-link_to_interface($12)
 #	^ The assembler cannot handle the instruction above.
 #	The offset is computed by counting the distance between
 #	both labels.
	lda	$10,LINK_TO_SCHEME($12)
 #	lda	$11,scheme_closure_hook-link_to_interface($12)
 #	^ The assembler cannot handle the instruction above.
 #	use a more expensive 2-instruction sequence.
	lda	$11,scheme_closure_hook
	.set	nomacro
	.set	noat
	# Register 12 already initialized
	bis	$31,TC_COMPILED_ENTRY,$13
	sll	$13,56,$13	# Shift to most significant byte
	ldq	$14,REGBLOCK_CLOSURE_FREE($9)
	br	$28,interface_to_scheme

	.align 4
	.aent	interface_to_C
	.globl	interface_to_C
interface_to_C:
 # Argument 1 (in $16) is the returned value
	bis	$16,$16,$0	# Real return value register
	ldq	$26,0($sp)	# Return address
	ldq	$9,56($sp)
	ldq	$10,48($sp)
	ldq	$11,40($sp)
	ldq	$12,32($sp)
	ldq	$13,24($sp)
	ldq	$14,16($sp)
	ldq	$15,8($sp)
	lda	$sp,FRAME_SIZE($sp)
	ret	$28,($26), 1
	.end	C_to_interface
