changecom(`;');;; -*-Midas-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/cmpauxmd/hppa.m4,v 1.8 1990/01/22 22:33:22 jinx Exp $
;;;
;;;	Copyright (c) 1989, 1990 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; HP Precision Architecture assembly language part of the compiled
;;;; code interface. See cmpint.txt, cmpint.c, cmpint-hppa.h, and
;;;; cmpgc.h for more documentation.
;;;;
;;;; NOTE:
;;;;	Assumptions:
;;;;
;;;;	1) All registers (except double floating point registers) and
;;;;	stack locations hold a C long object.
;;;;
;;;;	2) The C compiler divides registers into three groups:
;;;;	- Linkage registers, used for procedure calls and global
;;;;	references.  On HPPA: gr0 (always 0), gr2 (return address),
;;;;	gr27 (global data pointer), and gr30 (stack pointer).
;;;;	- super temporaries, not preserved accross procedure calls and
;;;;	always usable. On HPPA: gr1, gr19-gr26, gr28-29, gr31.
;;;;	gr26-23 are argument registers, gr28-29 are return registers.
;;;;	- preserved registers saved by the callee if they are written.
;;;;	On HPPA: gr3-gr18
;;;;
;;;;	3) Arguments, if passed on a stack, are popped by the caller
;;;;	or by the procedure return instruction (as on the VAX).  Thus
;;;;	most "leaf" procedures need not worry about them. On HPPA: All
;;;;	arguments have slots in the stack, allocated and popped by the
;;;;	caller, but the first four words are actually passed in gr26,
;;;;	gr25, gr24, gr23, unless they are floating point arguments, in
;;;;	which case they are passed in floating point registers.
;;;;
;;;;	4) There is a hardware or software maintained stack for
;;;;	control.  The procedure calling sequence may leave return
;;;;	addresses in registers, but they must be saved somewhere for
;;;;	nested calls and recursive procedures.  On HPPA: Passed in a
;;;;	register, but a slot on the stack exists, allocated by the
;;;;	caller.  The return link is in gr2 and immediately saved in
;;;;	-20(0,30) if the procedure makes further calls.  The stack
;;;;	pointer is in gr30.
;;;;
;;;;	5) C procedures return long values in a super temporary
;;;;    register.  Two word structures are returned in super temporary
;;;;    registers as well.  On HPPA: gr28 is used for long returns,
;;;;	gr28/gr29 are used for two word structure returns.
;;;;
;;;;	6) Floating point registers are not preserved by this
;;;;	interface.  The interface is only called from the Scheme
;;;;	interpreter, which does not use floating point data.  Thus
;;;;	although the calling convention would require us to preserve
;;;;	them, they contain garbage.  On HPPA: fr12-fr15 are
;;;;	callee-saves registers, fr4-fr7 are parameter registers, and
;;;;	fr8-fr11 are caller-saves registers.  fr0-fr3 are status
;;;;	registers.
;;;;
;;;; Compiled Scheme code uses the following register convention.
;;;; Note that scheme_to_interface_ble and the register block are
;;;; preserved by C calls, but the others are not, since they change
;;;; dynamically.  scheme_to_interface and trampoline_to_interface can
;;;; be reached at fixed offsets from scheme_to_interface_ble.
;;;;	- gr22 contains the Scheme stack pointer.
;;;;	- gr21 contains the Scheme free pointer.
;;;;	- gr20 contains a cached version of MemTop.
;;;;	- gr19 contains the dynamic link when needed.
;;;;	- gr5 contains the quad mask for machine pointers.
;;;;	- gr4 contains a pointer to the Scheme interpreter's
;;;;	"register" block.  This block contains the compiler's copy of
;;;;	MemTop, the interpreter's registers (val, env, exp, etc),
;;;;	temporary locations for compiled code.
;;;;	- gr3 contains the address of scheme_to_interface_ble.
;;;;
;;;;	All other registers are available to the compiler.  A
;;;;	caller-saves convention is used, so the registers need not be
;;;;	preserved by subprocedures.
;;;;
;;;; ADB mnemonics:
;;;;	arg3 = gr23; arg2 = gr24; arg1 = gr25; arg0 = gr26
;;;;	dp   = gr27; ret0 = gr28; ret1 = gr29; sp   = gr30; rp   = gr02

define(TC_LENGTH, ifdef(`TYPE_CODE_LENGTH', TYPE_CODE_LENGTH, 8))
define(QUAD_MASK, eval(2 ** (TC_LENGTH - 2)))
define(LOW_TC_BIT, eval(TC_LENGTH - 1))

	.SPACE  $TEXT$
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
C_to_interface
	.PROC
	.CALLINFO CALLER,FRAME=28,SAVE_RP
	.ENTRY
	STW	2,-20(0,30)		; Save return address
	STWM	3,112(30)		; Save first reg, allocate frame
	STW	4,-108(30)		; Save the other regs
	STW	5,-104(30)
	STW	6,-100(30)
	STW	7,-96(30)
	STW	8,-92(30)
	STW	9,-88(30)
	STW	10,-84(30)
	STW	11,-80(30)
	STW	12,-76(30)
	STW	13,-72(30)
	STW	14,-68(30)
	STW	15,-64(30)
	STW	16,-60(30)
	STW	17,-56(30)
	STW	18,-52(30)
	ADDIL	L'Registers-$global$,27
	LDO	R'Registers-$global$(1),4 ; Setup Regs
	LDI	QUAD_MASK,5

interface_to_scheme
	LDW	8(0,4),2		; Move interpreter reg to val
	LDW	0(0,4),20		; Setup memtop
	ADDIL	L'Ext_Stack_Pointer-$global$,27
	LDW	R'Ext_Stack_Pointer-$global$(1),22 ; Setup stack pointer
	ADDIL	L'Free-$global$,27
	LDW	R'Free-$global$(1),21	; Setup free
	COPY	2,19			; Restore dynamic link if any
	DEP	5,LOW_TC_BIT,TC_LENGTH,19
	.CALL	RTNVAL=GR		; out=28
	BLE	0(5,26)			; Invoke entry point
	COPY	31,3			; Setup scheme_to_interface_ble

scheme_to_interface_ble
	ADDI	4,31,31			; Skip over format word ...
trampoline_to_interface
	COPY	31,26
	DEP	0,31,2,26
scheme_to_interface
	STW	2,8(0,4)		; Move val to interpreter reg
	ADDIL	L'utility_table-$global$,27
	LDO	R'utility_table-$global$(1),29
	LDWX,S	28(0,29),29		; Find handler
	ADDIL	L'Ext_Stack_Pointer-$global$,27
	STW	22,R'Ext_Stack_Pointer-$global$(1) ; Update stack pointer
	ADDIL	L'Free-$global$,27
	STW	21,R'Free-$global$(1)	; Update free
	.CALL	ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR,RTNVAL=GR
	BLE	0(4,29)			; Call handler
	COPY	31,2			; Setup return address
	BV	0(28)			; Call receiver
	COPY	29,26			; Setup entry point

;; This sequence of NOPs is provided to allow for modification of
;; the sequence that appears above without having to recompile the
;; world.  The compiler "knows" the distance between
;; scheme_to_interface_ble and hook_jump_table (100 bytes)

	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP

hook_jump_table				; scheme_to_interface + 100
store_closure_code
	B	store_closure_code_real+4
	LDIL	L'0x23400000,20		; LDIL opcode and register

;; On arrival, 31 has a return address and 1 contains the address to
;; which the closure should jump.  The appropriate instructions (LDIL
;; and BLE and SUBI) are pushed on the heap.

store_closure_code_real
	LDIL	L'0x23400000,20		; LDIL opcode and register
	EXTRU	1,0,1,5
	DEP	5,31,1,20
	EXTRU	1,11,11,5
	DEP	5,30,11,20
	EXTRU	1,13,2,5
	DEP	5,17,2,20
	EXTRU	1,18,5,5
	DEP	5,15,5,20
	STWM	20,4(0,21)		; Store LDIL instruction
	LDIL	L'0xe7406000,20		; BLE opcode, register and nullify
	LDO	R'0xe7406000(20),20
	EXTRU	1,19,1,5
	DEP	5,29,1,20
	EXTRU	1,29,10,5
	DEP	5,28,10,20
	STWM	20,4(0,21)		; Store BLE instruction
	LDIL	L'0xb7ff07e9,20
	LDO	R'0xb7ff07e9(20),20
	STWM	20,4(0,21)		; Store ADDI instruction
	LDW	0(0,4),20		; Reload memtop
	BE	0(5,31)			; Return
	LDI	QUAD_MASK,5		; Restore register 5

interface_to_C
	COPY	29,28			; Setup C value
        LDW     -132(0,30),2		; Restore return address
        LDW     -52(0,30),18		; Restore saved registers
        LDW     -56(0,30),17
        LDW     -60(0,30),16
        LDW     -64(0,30),15
        LDW     -68(0,30),14
        LDW     -72(0,30),13
        LDW     -76(0,30),12
        LDW     -80(0,30),11
        LDW     -84(0,30),10
        LDW     -88(0,30),9
        LDW     -92(0,30),8
        LDW     -96(0,30),7
        LDW     -100(0,30),6
        LDW     -104(0,30),5
        LDW     -108(0,30),4
        BV      0(2)			; Return
        .EXIT
        LDWM    -112(0,30),3		; Restore last reg, pop frame
        .PROCEND			;in=26;out=28;

	.SPACE	$TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
;	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.SUBSPA $UNWIND$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$
	.SPACE	$PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
$THISMODULE$
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO
	.IMPORT $global$,DATA
	.IMPORT	Registers,DATA
	.IMPORT	Ext_Stack_Pointer,DATA
	.IMPORT	Free,DATA
	.IMPORT	utility_table,DATA
	.SPACE	$TEXT$
	.SUBSPA $CODE$
	.EXPORT C_to_interface,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
	.EXPORT interface_to_scheme,PRIV_LEV=3
	.EXPORT interface_to_C,PRIV_LEV=3
	.EXPORT scheme_to_interface_ble,PRIV_LEV=3
	.EXPORT trampoline_to_interface,PRIV_LEV=3
	.EXPORT scheme_to_interface,PRIV_LEV=3
	.EXPORT store_closure_code,PRIV_LEV=3
	.END
