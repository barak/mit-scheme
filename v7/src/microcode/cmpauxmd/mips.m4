/* #define DEBUG_INTERFACE */
 ### -*-Midas-*-
 ###
 ###	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/mips.m4,v 1.5 1992/07/30 15:07:46 jinx Exp $
 ###
 ###	Copyright (c) 1989-91 Massachusetts Institute of Technology
 ###
 ###	This material was developed by the Scheme project at the
 ###	Massachusetts Institute of Technology, Department of
 ###	Electrical Engineering and Computer Science.  Permission to
 ###	copy this software, to redistribute it, and to use it for any
 ###	purpose is granted, subject to the following restrictions and
 ###	understandings.
 ###
 ###	1. Any copy made of this software must include this copyright
 ###	notice in full.
 ###
 ###	2. Users of this software agree to make their best efforts (a)
 ###	to return to the MIT Scheme project any improvements or
 ###	extensions that they make, so that these may be included in
 ###	future releases; and (b) to inform MIT of noteworthy uses of
 ###	this software.
 ###
 ###	3. All materials developed as a consequence of the use of this
 ###	software shall duly acknowledge such use, in accordance with
 ###	the usual standards of acknowledging credit in academic
 ###	research.
 ###
 ###	4. MIT has made no warrantee or representation that the
 ###	operation of this software will be error-free, and MIT is
 ###	under no obligation to provide any services, by way of
 ###	maintenance, update, or otherwise.
 ###
 ###	5. In conjunction with products arising from the use of this
 ###	material, there shall be no use of the name of the
 ###	Massachusetts Institute of Technology nor of any adaptation
 ###	thereof in any advertising, promotional, or sales literature
 ###	without prior written consent from MIT in each case.
 ###

 #### MIPS Architecture assembly language part of the compiled
 #### code interface. See cmpint.txt, cmpint.c, cmpint-mips.h, and
 #### cmpgc.h for more documentation.
 ####
 #### NOTE:
 ####	Assumptions:
 ####
 ####	1) All registers (except double floating point registers) and
 ####	stack locations hold a C long object.
 ####
 ####	2) The C compiler divides registers into three groups:
 ####	- Linkage registers, used for procedure calls and global
 ####	references.  On MIPS: 0 (always 0), 31 (return address),
 ####	28 (global data pointer), and 29 (C stack pointer).
 ####	- super temporaries, not preserved accross procedure calls and
 ####	always usable. On MIPS: 2-15. 4-7 are argument registers,
 ####	2 and 3 are return registers.
 ####	- preserved registers saved by the callee if they are written.
 ####	On MIPS: 16-25
 ####
 ####	3) Arguments, if passed on a stack, are popped by the caller
 ####	or by the procedure return instruction (as on the VAX).  Thus
 ####	most "leaf" procedures need not worry about them. On MIPS: All
 ####	arguments have slots in the stack, allocated and popped by the
 ####	caller, but the first four words are actually passed in
 ####	registers 4 through 7, unless they are floating point
 ####	arguments, in which case they are passed in floating point
 ####	registers.
 ####
 ####	4) There is a hardware or software maintained stack for
 ####	control.  The procedure calling sequence may leave return
 ####	addresses in registers, but they must be saved somewhere for
 ####	nested calls and recursive procedures.  On MIPS: Passed in a
 ####	register, but a slot on the stack exists, allocated by the
 ####	caller.  The return link is in 31.  The stack pointer is in
 ####	29.
 ####
 ####	5) C procedures return long values in a super temporary
 ####   register.  MIPS only: two word structures are returned in a
 ####   location specified by the contents of the first argument
 ####   register, and all other arguments are shifted over one
 ####   location (i.e.  apparent argument 1 is passed in the register
 ####   usually used for argument 2, etc.)
 ####
 ####	6) On MIPS the floating point registers fr20-fr31 are
 ####	callee-saves registers, fr12-fr15 are parameter registers, and
 ####	fr4-fr11 and fr16-fr19 are caller-saves registers.  fr0-3 are
 ####	return result registers.  Only the even numbered registers are
 ####	used (odd registers contain second 32 bits of 64 bit values).
 ####
 #### Compiled Scheme code uses the following register convention.
 #### Note that scheme_to_interface and the register block are
 #### preserved by C calls, but the others are not, since they change
 #### dynamically.  scheme_to_interface_linked and
 #### trampoline_to_interface can be reached at fixed offsets from
 #### scheme_to_interface.
 ####   - gr2  is the returned value register
 ####	- gr3  contains the Scheme stack pointer.
 ####   - gr4 - gr7 are used by C for arguments and can't be used
 ####          permanently by Scheme
 ####	- gr8  contains a cached version of MemTop.
 ####	- gr9  contains the Scheme free pointer.
 ####	- gr10 contains the address of scheme_to_interface.
 ####	- gr11 contains the dynamic link when needed.
 ####   <CALLEE SAVES REGISTERS BELOW HERE>
 ####   - gr16 - gr 19 aren't used by Scheme
 ####	- gr20 contains the address mask for machine pointers.
 ####	- gr21 contains a pointer to the Scheme interpreter's
 ####	       "register" block.  This block contains the compiler's
 ####          copy of MemTop, the interpreter's registers (val, env,
 ####          exp, etc), temporary locations for compiled code.
 ####   - gr22 contains the top 6 address bits for heap pointers
 ####   <CALLEE SAVES REGISTERS ABOVE HERE>
 ####   - gr25 is used a an index for dispatch into C.
 ####   - gr26 and 27 are reserved for the OS
 ####   - gr28 contains the pointer to C static variables
 ####   - gr29 contains the C stack pointer
 ####
 ####	All other registers are available to the compiler.  A
 ####	caller-saves convention is used, so the registers need not be
 ####	preserved by subprocedures.
 ####
 ####   Notice that register gr25 is used for the index used to
 ####   dispatch into the trampolines and interface routines.

	.verstamp	1 31
	.text	
	.align	2
	.set	noat
	.set	noreorder

define(value, 2)
define(stack, 3)
define(C_arg1, 4)
define(C_arg2, 5)
define(C_arg3, 6)
define(C_arg4, 7)
define(memtop, 8)
define(free, 9)
define(s_to_i, 10)
define(dynlink, 11)

define(addr_mask, 20)
define(registers, 21)
define(heap_bits, 22)
	
define(tramp_index, 25)

 # Argument (in $C_arg1) is a compiled Scheme entry point
 # but save C registers first
	.globl	C_to_interface
	.ent	C_to_interface
C_to_interface:
	addi	$sp,$sp,-112
	.frame	$sp,112,$0
	.mask	0x80ff0000,0
	sw	$31,108($sp)		# Save return address
	sw	$23,104($sp)
	sw	$22,100($sp)
	sw	$21,96($sp)
	sw	$20,92($sp)
	sw	$19,88($sp)
	sw	$18,84($sp)
	sw	$17,80($sp)
	sw	$16,76($sp)
	.fmask	0x00000fff,0
	s.d	$f30,68($sp)
	s.d	$f28,60($sp)
	s.d	$f26,52($sp)
	s.d	$f24,44($sp)
	s.d	$f22,36($sp)
	s.d	$f20,28($sp)
	# 20 and 24($sp) hold return data structure from C hooks
        # 16 is reserved for 4th argument to hooks, if used.
        # 4, 8, and 12($sp) are space for 1st - 3rd argument.
        # 0($sp) is space for holding return pointer
#ifdef DEBUG_INTERFACE
	la	$registers,Debug_Buffer
	.set	at
	sw	$registers,Debug_Buffer_Pointer
	.set	noat
#endif
	la	$registers,Registers
	lw	$heap_bits,Free
	lui	$addr_mask,0xfc00
	and	$heap_bits,$heap_bits,$addr_mask
	nor	$addr_mask,$0,$addr_mask
 # ... fall through ...
 # Argument (in $C_arg1) is a compiled Scheme entry point.  Reload
 # the Scheme registers and go to work...any registers not reloaded
 # here must be callee saves by C.
	.globl	interface_to_scheme
interface_to_scheme:
	lw	$value,8($registers)
	lw	$memtop,0($registers)
	lw	$stack,Ext_Stack_Pointer
	lw	$free,Free
	and	$dynlink,$addr_mask,$value
	or	$dynlink,$heap_bits,$dynlink
#ifdef DEBUG_INTERFACE
	andi	$at,$free,3
	bne	$at,0,Bad_Free_Pointer
	nop
Continue_Past_Free_Problem:
#endif
	jal	$31,$C_arg1		# Off to compiled code ...
        addi	$s_to_i,$31,100		# Set up scheme_to_interface

	.globl	hook_jump_table
hook_jump_table:
 # This sequence of NOPs is provided to allow for modification of
 # the sequence that appears above without having to recompile the
 # world.  The compiler "knows" the distance between
 # scheme_to_interface_ble and hook_jump_table (100 bytes)
 #
 # $tramp_index has the offset into the table that is desired.
	.globl	link_to_interface
link_to_interface: 	# ...scheme_to_interface-100
	addi	$31,$31,4	# Skip over format word ...

	.globl	trampoline_to_interface
trampoline_to_interface:	# ...scheme_to_interface-96
	j	scheme_to_interface
	add	$C_arg2,$0,$31	# Arg2 <- trampoline data area
	
	j	generate_closure # ...-88
	sw	$25,4($free)	# ...-84

	j	push_closure_entry	# ...-80
	sw	$1,0($free)	# ...-76

	nop	# ...-72
	nop	# ...-68
	nop	# ...-64
	nop	# ...-60
	nop	# ...-56
	nop	# ...-52
	nop	# ...-48
	nop	# ...-44
	nop	# ...-40
	nop	# ...-36
	nop	# ...-32
	nop	# ...-28
	nop	# ...-24
	nop	# ...-20
	nop	# ...-16
	nop	# ...-12
	nop	# ...-8
	nop	# ...-4

 # DO NOT MOVE the following label, it is used above ...
 #  Argument (in $tramp_index) is index into utility_table for the
 # interface procedure to be called.  The Scheme compiler has saved
 # any registers that it may need.  Registers 5 through 7 are loaded
 # with arguments for the C procedure that is being invoked.  The
 # fourth argument (if used) is stored at 16($sp).

	.globl 	scheme_to_interface
scheme_to_interface:
	sw	$value,8($registers)
#ifdef DEBUG_INTERFACE
	lw	$value,Free_Constant
	addi	$0,$0,0			# Load delay
	sltu	$at,$stack,$value
	bne	$at,$0,Stack_Overflow_Detected
	addi	$0,$0,0
	lw	$value,Debug_Buffer_Pointer
	addi	$0,$0,0
	sw	$stack,0($value)        # Stack pointer
        sw	$25,4($value)           # Index
        sw	$C_arg2,8($value)       # 1st arg.
        sw	$C_arg3,12($value)      # 2nd arg.
        sw	$C_arg4,16($value)      # 3rd arg.
	addi	$value,$value,20
	la	$12,Debug_Buffer_End
	bne	$12,$value,Store_Pointer_Back
	nop
	la	$12,Debug_Buffer
	add	$value,$0,$12
Store_Pointer_Back:
	.set	at
	sw	$value,Debug_Buffer_Pointer
	lw	$value,Debug_Call_Count
	lw	$12,Debug_Call_Max
	addi	$value,$value,1
	sw	$value,Debug_Call_Count
	beq	$value,$12,Debug_Tight_Loop
	nop
	.set	noat
#endif
after_overflow:
	la	$24,utility_table	# Find table
	add	$25,$24,$25		# Address of entry
	lw	$25,0($25)		# gr25 <- Entry
	la	$24,Ext_Stack_Pointer
	sw	$stack,0($24)		# Save Scheme stack pointer
	la	$24,Free
	sw	$free,0($24)		# Save Free
	jal	$31,$25			# Off to interface code
	addi	$C_arg1,$sp,20		# Return value on C stack
	lw	$25,20($sp)		# Get dispatch address
	lw	$C_arg1,24($sp)		# Arg1 <- value component
	jal	$31,$25			# Redispatch ...
	addi	$0,$0,0			# Branch delay...

 # Argument 1 (in $C_arg1) is the returned value
	.globl interface_to_C
interface_to_C:
	l.d	$f20,28($sp)
	l.d	$f22,36($sp)
	l.d	$f24,44($sp)
	l.d	$f26,52($sp)
	l.d	$f28,60($sp)
	l.d	$f30,68($sp)
	lw	$16,76($sp)
	lw	$17,80($sp)
	lw	$18,84($sp)
	lw	$19,88($sp)
	lw	$20,92($sp)
	lw	$21,96($sp)
	lw	$22,100($sp)
	lw	$23,104($sp)
	lw	$31,108($sp)
	addi	$sp,$sp,112		# Pop stack back
	j	$31			# Return
	add	$2,$0,$C_arg1		# Return value to C
	.end	C_to_interface

#ifdef DEBUG_INTERFACE
	.globl	Stack_Overflow_Detected
Stack_Overflow_Detected:
	j	after_overflow
	nop

	.globl	Bad_Free_Pointer
Bad_Free_Pointer:
	j	Continue_Past_Free_Problem
	nop
#endif

	.globl	interface_initialize
	.ent	interface_initialize
interface_initialize:
	.frame	$sp,0,$31
	cfc1	$25,$31		# read FPU control register
	nop
	ori	$25,$25,0xf00	# enable V, Z, O, U traps
	ctc1	$25,$31		# write FPU control register
	j	$31		# return
	nop
	.end	interface_initialize

	.globl	generate_closure
	.ent	generate_closure
generate_closure:
	.frame	$sp,0,$0
	# On arrival:
	#   31 is the return address
	#    1 has the size of the closure (longwords)
	#    4 has the offset from return address to destination
	#   25 has the GC offset and format words
	# Generates the closure on the heap, updating free pointer
 #	sw	$25,4($free)	# Store GC and format words on heap
	lui	$25,0x3400
	add	$25,$1,$25
	sw	$25,0($free)	# Store manifest closure header
	add	$25,$31,$4	# 25 <- destination address
	and	$25,$25,$addr_mask
	srl	$25,$25,2	# JAL will unshift at runtime
	lui	$4,0x0C00
	or	$25,$25,$4	# JAL instruction
	sw	$25,8($free)	# Store in closure
	lui	$25,0x23FF
	ori	$25,0xFFF8
	sw	$25,12($free)	# Store ADDI 31,31,-8
	addi	$1,$1,1		# 1 longword header
	sll	$1,$1,2		# longwords -> bytes
	j	$31		# Done!
	add	$free,$free,$1	# Increment Free pointer by size

	.end	generate_closure

	.globl	push_closure_entry
	.ent	push_closure_entry
push_closure_entry:
	.frame	$sp,0,$0
	# On arrival:
	#   31 is the return address
	#    1 has the GC offset and format words
	#    4 has the offset from return address to destination
	# Push a closure entry on the heap, updating free pointer.
	# The header for the group of closure entries has already been
	# generated. 
 #	sw	$1,0($free)	# Store GC and format words on heap
	add	$1,$31,$4	# 1 <- destination address
	and	$1,$1,$addr_mask
	srl	$1,$1,2		# JAL will unshift at runtime
	lui	$4,0x0C00
	or	$1,$1,$4	# JAL instruction
	sw	$1,4($free)	# Store in closure
	lui	$1,0x23FF
	ori	$1,0xFFF8
	sw	$1,8($free)	# Store ADDI 31,31,-8
	j	$31		# Done!
	addi	$free,$free,12	# Increment Free pointer

	.end	push_closure_entry

	.globl	Debug_Tight_Loop
	.ent	Debug_Tight_Loop
Debug_Tight_Loop:
	beq	$12,$value,Debug_Tight_Loop
	nop
	j	after_overflow
	.end	Debug_Tight_Loop

#ifdef DEBUG_INTERFACE
	.data
	.globl	Debug_Buffer_Pointer
Debug_Buffer_Pointer:
	.word	0
	.globl	Debug_Buffer
Debug_Buffer:
	.word	0:30
Debug_Buffer_End:
	.word	0
	.globl	Debug_Call_Count
Debug_Call_Count:
	.word	0
	.globl	Debug_Call_Max
Debug_Call_Max:
	.word	0
#endif
