changecom(`;');;; -*-Midas-*-
;;;
;;; $Id: hppa.m4,v 1.43 2007/01/05 15:33:08 cph Exp $
;;;
;;; Copyright (c) 1989-2000, 2002 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT/GNU Scheme.
;;;
;;; MIT/GNU Scheme is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; MIT/GNU Scheme is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT/GNU Scheme; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;;; 02110-1301, USA.

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
;;;;	GCC returns two word structures differently: It passes
;;;;	the address of the structure in gr28!
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

changequote(",")
define(HEX, "0x$1")
define(ASM_DEBUG, 0)
define(TC_LENGTH, ifdef("TYPE_CODE_LENGTH", TYPE_CODE_LENGTH, 6))
define(QUAD_MASK, eval(2 ** (TC_LENGTH - 2)))
define(LOW_TC_BIT, eval(TC_LENGTH - 1))
define(DATUM_LENGTH, eval(32 - TC_LENGTH))
define(FIXNUM_LENGTH, DATUM_LENGTH)
define(FIXNUM_POS, eval(FIXNUM_LENGTH - 1))
define(FIXNUM_BIT, eval(TC_LENGTH + 1))
define(TC_START, eval(TC_LENGTH - 1))
define(TC_FLONUM, 0x6)
define(TC_VECTOR, 0xa)
define(TC_FIXNUM, 0x1a)
define(TC_STRING, 0x1e)
define(TC_NMV, 0x27)
define(TC_CCENTRY, 0x28)
define(FLONUM_VECTOR_HEADER, eval((TC_NMV * (2 ** DATUM_LENGTH)) + 2))
define(TC_FALSE, 0)
define(TC_TRUE, 0x8)
define(SHARP_F, eval(TC_FALSE * (2 ** DATUM_LENGTH)))
define(SHARP_T, eval(TC_TRUE * (2 ** DATUM_LENGTH)))
define(C_FRAME_SIZE,
       ifdef("HPC", 112,
	     ifdef("GCC", 120,
	           `Unknown C compiler: bad frame size')))
define(INT_BIT_STACK_OVERFLOW, 31)

	.SPACE  $TEXT$
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
C_to_interface
	.PROC
	.CALLINFO CALLER,FRAME=28,SAVE_RP
	.ENTRY
	STW	2,-20(0,30)			; Save return address
	STWM	3,eval(C_FRAME_SIZE)(30)	; Save first reg, 
	STW	4,-108(30)			;  and allocate frame
	STW	5,-104(30)			; Save the other regs
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
	LDO	R'Registers-$global$(1),4	; Setup Regs
	LDI	QUAD_MASK,5

ep_interface_to_scheme
	LDW	8(0,4),2			; Move interpreter reg to val
	COPY	2,19				; Restore dynamic link if any
	DEP	5,LOW_TC_BIT,TC_LENGTH,19
	ADDIL	L'sp_register-$global$,27
	LDW	R'sp_register-$global$(1),22	; Setup stack pointer

ep_interface_to_scheme_2
	LDW	0(0,4),20			; Setup memtop
	ADDIL	L'Free-$global$,27
	LDW	R'Free-$global$(1),21		; Setup free
	.CALL	RTNVAL=GR			; out=28
	BLE	0(5,26)				; Invoke entry point
	COPY	31,3				; Setup scheme_to_interface_ble

scheme_to_interface_ble
	ADDI	4,31,31				; Skip over format word ...
trampoline_to_interface
	COPY	31,26
	DEP	0,31,2,26
scheme_to_interface
	STW	2,8(0,4)			; Move val to interpreter reg
	ADDIL	L'hppa_utility_table-$global$,27
	LDW	R'hppa_utility_table-$global$(1),29
	ADDIL	L'sp_register-$global$,27
	LDWX,S	28(0,29),29			; Find handler
	STW	22,R'sp_register-$global$(1)	; Update stack pointer
	ADDIL	L'Free-$global$,27
	STW	21,R'Free-$global$(1)		; Update free
	ifelse(ASM_DEBUG,1,"ADDIL	L'interface_counter-$global$,27
	LDW	R'interface_counter-$global$(1),21
	LDO	1(21),21
	STW	21,R'interface_counter-$global$(1)
	ADDIL	L'interface_limit-$global$,27
	LDW	R'interface_limit-$global$(1),22
	COMB,=,N	21,22,interface_break
interface_proceed")
	ifdef("GCC", "LDO	-116(30),28")
	.CALL	ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=GR,RTNVAL=GR
	BLE	0(4,29)				; Call handler
	COPY	31,2				; Setup return address
	ifdef("GCC", "LDW	-116(30),28
		      LDW	-112(30),29")
	BV	0(28)				; Call receiver
	COPY	29,26				; Setup entry point

;; This sequence of NOPs is provided to allow for modification of
;; the sequence that appears above without having to recompile the
;; world.  The compiler "knows" the distance between
;; scheme_to_interface_ble and hook_jump_table (100 bytes)

	ifelse(ASM_DEBUG,1,"","NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP")
	ifdef("GCC","","NOP
	NOP
	NOP")

;; This label is used by the trap handler

ep_scheme_hooks_low
hook_jump_table					; scheme_to_interface + 100
store_closure_code_hook
	B	store_closure_code+4
	LDIL	L'0x23400000,20			; LDIL opcode and register

store_closure_entry_hook
	B	store_closure_entry+4
	DEP	0,31,2,1			; clear PC protection bits

multiply_fixnum_hook
	B	multiply_fixnum+4
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26	; arg1

fixnum_quotient_hook
	B	fixnum_quotient+4
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26	; arg1

fixnum_remainder_hook
	B	fixnum_remainder+4
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26	; arg1

fixnum_lsh_hook
	B	fixnum_lsh+4
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25	; arg2

generic_plus_hook
	B	generic_plus+4
	LDW	0(0,22),6			; arg1

generic_subtract_hook
	B	generic_subtract+4
	LDW	0(0,22),6			; arg1

generic_times_hook
	B	generic_times+4
	LDW	0(0,22),6			; arg1

generic_divide_hook
	B	generic_divide+4
	LDW	0(0,22),6			; arg1

generic_equal_hook
	B	generic_equal+4
	LDW	0(0,22),6			; arg1

generic_less_hook
	B	generic_less+4
	LDW	0(0,22),6			; arg1

generic_greater_hook
	B	generic_greater+4
	LDW	0(0,22),6			; arg1

generic_increment_hook
	B	generic_increment+4
	LDW	0(0,22),6			; arg1

generic_decrement_hook
	B	generic_decrement+4
	LDW	0(0,22),6			; arg1

generic_zero_hook
	B	generic_zero+4
	LDW	0(0,22),6			; arg1

generic_positive_hook
	B	generic_positive+4
	LDW	0(0,22),6			; arg1

generic_negative_hook
	B	generic_negative+4
	LDW	0(0,22),6			; arg1

shortcircuit_apply_hook
	B	shortcircuit_apply+4
	EXTRU	26,5,6,24			; procedure type -> 24

shortcircuit_apply_1_hook
	B	shortcircuit_apply_1+4
	EXTRU	26,5,6,24			; procedure type -> 24

shortcircuit_apply_2_hook
	B	shortcircuit_apply_2+4
	EXTRU	26,5,6,24			; procedure type -> 24

shortcircuit_apply_3_hook
	B	shortcircuit_apply_3+4
	EXTRU	26,5,6,24			; procedure type -> 24

shortcircuit_apply_4_hook
	B	shortcircuit_apply_4+4
	EXTRU	26,5,6,24			; procedure type -> 24

shortcircuit_apply_5_hook
	B	shortcircuit_apply_5+4
	EXTRU	26,5,6,24			; procedure type -> 24

shortcircuit_apply_6_hook
	B	shortcircuit_apply_6+4
	EXTRU	26,5,6,24			; procedure type -> 24

shortcircuit_apply_7_hook
	B	shortcircuit_apply_7+4
	EXTRU	26,5,6,24			; procedure type -> 24

shortcircuit_apply_8_hook
	B	shortcircuit_apply_8+4
	EXTRU	26,5,6,24			; procedure type -> 24

stack_and_interrupt_check_hook
	B	stack_and_interrupt_check+4
	LDW	44(0,4),25			; Stack_Guard -> r25

invoke_primitive_hook
	B	invoke_primitive+4
	DEPI	0,31,2,31			; clear privilege bits

vector_cons_hook
	B	vector_cons+4
	LDW	0(0,22),26			; length as fixnum

string_allocate_hook
	B	string_allocate+4
	LDW	0(0,22),26			; length as fixnum

floating_vector_cons_hook
	B	floating_vector_cons+4
	LDW	0(0,22),26			; length as fixnum

flonum_sin_hook
	B	flonum_sin+4
	COPY	22,18

flonum_cos_hook
	B	flonum_cos+4
	COPY	22,18

flonum_tan_hook
	B	flonum_tan+4
	COPY	22,18

flonum_asin_hook
	B	flonum_asin+4
	COPY	22,18

flonum_acos_hook
	B	flonum_acos+4
	COPY	22,18

flonum_atan_hook
	B	flonum_atan+4
	COPY	22,18

flonum_exp_hook
	B	flonum_exp+4
	COPY	22,18

flonum_log_hook
	B	flonum_log+4
	COPY	22,18

flonum_truncate_hook
	B	flonum_truncate+4
	COPY	22,18

flonum_ceiling_hook
	B	flonum_ceiling+4
	COPY	22,18

flonum_floor_hook
	B	flonum_floor+4
	COPY	22,18

flonum_atan2_hook
	B	flonum_atan2+4
	COPY	22,18

compiled_code_bkpt_hook				; hook 44 (offset 451 + 1)
	B	compiled_code_bkpt+4
	LDO	-8(31),31

compiled_closure_bkpt_hook			; hook 45 (offset 451 + 9)
	B	compiled_closure_bkpt+4
	LDO	-12(31),31

copy_closure_pattern_hook
	B	copy_closure_pattern+4
	LDW	-3(0,31),29			; offset

copy_multiclosure_pattern_hook
	B	copy_multiclosure_pattern+4
	LDW	-3(0,31),29			; offset	

closure_entry_bkpt_hook				; hook 48 (offset 451 + 33)
	B	closure_entry_bkpt+4
	LDO	-8(31),31			; bump back to entry point

;;
;; Provide dummy trapping hooks in case a newer version of compiled
;; code that expects more hooks is run.
;;

no_hook
	BREAK	0,49
	NOP
	BREAK	0,50
	NOP
	BREAK	0,51
	NOP
	BREAK	0,52
	NOP
	BREAK	0,53
	NOP
	BREAK	0,54
	NOP
	BREAK	0,55
	NOP
	BREAK	0,56
	NOP
	BREAK	0,57
	NOP
	BREAK	0,58
	NOP
	BREAK	0,59
	NOP
	BREAK	0,60
	NOP
	BREAK	0,61
	NOP
	BREAK	0,62
	NOP
	BREAK	0,63
	NOP

ifelse(ASM_DEBUG,1,"interface_break
	COMB,=	21,22,interface_break
	NOP
	B,N	interface_proceed")

store_closure_entry
;;
;; On arrival, 31 has a return address and 1 contains the address to
;; which the closure should jump with pc protection bits.
;; 26 contains the format/gc-offset word for this entry.
;;
	DEP	0,31,2,1			; clear PC protection bits
	STWM	26,4(0,21)			; move format long to heap
;; fall through to store_closure_code

store_closure_code
;;
;; On arrival, 31 has a return address and 1 contains the address to
;; which the closure should jump.  The appropriate instructions (LDIL
;; and BLE and SUBI) are pushed on the heap.
;;     Important:
;; 3 words in memory are modified, but only 2 FDC instructions and one FIC
;; instruction are issued.  The PDC_CACHE description in the I/O Architecture
;; manual specifies that each flush will flush a multiple of 16 bytes, thus
;; a flush of the first data word and a flush of the last data word suffice to
;; flush all three.  A single FIC of the first instruction word suffices since
;; the space is newly allocated and the whole I-cache was flushed at
;; exec and relocation(GC) time.
;; The SYNC is assumed to be separated by at least 7 instructions from
;; the first execution of the new instructions.
;;
	LDIL	L'0x23400000,20			; LDIL opcode and register
	EXTRU	1,0,1,5
	DEP	5,31,1,20
	EXTRU	1,11,11,5
	DEP	5,30,11,20
	EXTRU	1,13,2,5
	DEP	5,17,2,20
	EXTRU	1,18,5,5
	DEP	5,15,5,20
	STW	20,0(0,21)			; Store LDIL instruction
	LDIL	L'0xe7406000,20			; BLE opcode, register
	LDO	R'0xe7406000(20),20		;  and nullify
	EXTRU	1,19,1,5
	DEP	5,29,1,20
	EXTRU	1,29,10,5
	DEP	5,28,10,20
	STW	20,4(0,21)			; Store BLE instruction
	LDIL	L'0xb7ff07e9,20
	LDO	R'0xb7ff07e9(20),20
	STW	20,8(0,21)			; Store ADDI instruction
	LDI	12,20
	FDC	0(0,21)				; flush 1st inst. from D-cache
	FDC	20(0,21)			; flush last inst. from D-cache
	SYNC
	FIC,M	20(5,21)			; flush 1st inst. from I-cache
	SYNC
	LDW	0(0,4),20			; Reload memtop
	BE	0(5,31)				; Return
	LDI	QUAD_MASK,5			; Restore register 5

multiply_fixnum
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;;
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26	; arg1
	STW	26,0(0,21)
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25	; arg2
	STW	25,4(0,21)
	ZDEPI	1,TC_LENGTH,1,26		; FIXNUM_LIMIT
	FLDWS	0(0,21),4
	FLDWS	4(0,21),5
	STW	26,8(0,21)			; FIXNUM_LIMIT
        FCNVXF,SGL,DBL  4,4			; arg1
        FCNVXF,SGL,DBL  5,5			; arg2
	FMPY,DBL	4,5,4
	FLDWS	8(0,21),5			; FIXNUM_LIMIT
        FCNVXF,SGL,DBL  5,5			; FIXNUM_LIMIT
	COPY	0,25				; signal no overflow
	FCMP,DBL,!>=	4,5			; result too large?
	FTEST
	B,N	multiply_fixnum_ovflw
	FSUB,DBL	0,5,5
	FCMP,DBL,!<	4,5			; result too small?
	FTEST
	B,N	multiply_fixnum_ovflw
	FCNVFXT,DBL,SGL	4,5
	FSTWS	5,0(0,21)			; result
	LDW	0(0,21),26
	BE	0(5,31)				; return
	ZDEP    26,FIXNUM_POS,FIXNUM_LENGTH,26	; make into fixnum
;;
multiply_fixnum_ovflw
	COPY	0,26
	LDO	1(0),25				; signal overflow
	BE	0(5,31)				; return
	ZDEP    26,FIXNUM_POS,FIXNUM_LENGTH,26	; make into fixnum

fixnum_quotient
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; Note that quotient only overflows when dividing by 0 and when the
;; divisor is -1 and the dividend is the most negative fixnum,
;; producing the most positive fixnum plus 1.
;;
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26	; arg1
	COMB,=	0,25,fixnum_quotient_ovflw
	STW	26,0(0,21)
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25	; arg2
	STW	25,4(0,21)
	ZDEPI	1,TC_LENGTH,1,26		; FIXNUM_LIMIT
	FLDWS	0(0,21),4
	FLDWS	4(0,21),5
        FCNVXF,SGL,DBL  4,4			; arg1
        FCNVXF,SGL,DBL  5,5			; arg2
	FDIV,DBL	4,5,4
	STW	26,0(0,21)			; FIXNUM_LIMIT
	FCNVFXT,DBL,SGL	4,5
	FSTWS	5,4(0,21)			; result
	FLDWS	0(0,21),5			; FIXNUM_LIMIT
	FCNVXF,SGL,DBL	5,5
	FCMP,DBL,!>=	4,5			; result too large?
	LDW	4(0,21),26
	COPY	0,25				; signal no overflow
	FTEST
;;
fixnum_quotient_ovflw
	LDO	1(0),25				; signal overflow
	BE	0(5,31)				; return
	ZDEP    26,FIXNUM_POS,FIXNUM_LENGTH,26	; make into fixnum

;; fixnum_remainder
;;
;; NOTE: The following code is disabled because the FREM instruction
;;	 has been dropped from the architecture and has never been
;;	 implemented in hardware.
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; Note that remainder only overflows when dividing by 0.
;; Note also that the FREM instruction does not compute the same as
;; the Scheme remainder operation.  The sign of the result must
;; sometimes be adjusted.
;;
;;	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26	; arg1
;;	COMB,=,N	0,25,fixnum_remainder_ovflw
;;	STW	26,0(0,21)
;;	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25	; arg2
;;	STW	25,4(0,21)
;;	FLDWS	0(0,21),4
;;	FLDWS	4(0,21),5
;;	FCNVXF,SGL,DBL  4,4			; arg1
;;	FCNVXF,SGL,DBL  5,5			; arg2
;;	FREM,DBL	4,5,4
;;	FCNVFXT,DBL,SGL	4,5
;;	FSTWS	5,4(0,21)			; result
;;	LDW	4(0,21),1
;;	XOR,<	26,1,0				; skip if signs !=
;;	B,N	fixnum_remainder_done
;;	COMB,=,N	0,1,fixnum_remainder_done
;;	XOR,<	26,25,0				; skip if signs !=
;;	ADD,TR	1,25,1				; result += arg2
;;	SUB	1,25,1				; result -= arg2
;;;;
;;fixnum_remainder_done
;;	ZDEP    1,FIXNUM_POS,FIXNUM_LENGTH,26	; make into fixnum
;;	BE	0(5,31)				; return
;;	COPY	0,25				; signal no overflow
;;;;
;;fixnum_remainder_ovflw
;;	BE	0(5,31)				; return
;;	LDO	1(0),25				; signal overflow

fixnum_remainder
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum
;; arguments.
;; Remainder can overflow only if arg2 = 0.
;;
	EXTRS	26,FIXNUM_POS,FIXNUM_LENGTH,26	; arg1
	STWM	29,-4(0,22)			; Preserve gr29
	COMB,=,N	0,25,fixnum_remainder_ovflw
	STWM	31,-4(0,22)			; Preserve ret. add.
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25	; arg2
	STWM	26,-4(0,22)			; Preserve arg1
        .CALL   				;in=25,26;out=29; (MILLICALL)
	BL	$$remI,31
	STWM	25,-4(0,22)			; Preserve arg2
;;
	LDWM	4(0,22),25			; Restore arg2
	LDWM	4(0,22),26			; Restore arg1
	XOR,<	26,29,0				; Skip if signs !=
	B,N	fixnum_remainder_done
	COMB,=,N	0,29,fixnum_remainder_done
	XOR,<	26,25,0
	ADD,TR	29,25,29			; setup result
	SUB	29,25,29
;;
fixnum_remainder_done
	ZDEP	29,FIXNUM_POS,FIXNUM_LENGTH,26	; make into fixnum
	LDWM	4(0,22),31			; Restore ret. add.
	COPY	0,25				; signal no overflow
	BE	0(5,31)				; return
	LDWM	4(0,22),29			; Restore gr29
;;
fixnum_remainder_ovflw
	LDO	1(0),25				; signal overflow
	COPY	0,26				; bogus return value
	BE	0(5,31)				; return
	LDWM	4(0,22),29			; Restore gr29	

fixnum_lsh
;;
;; On arrival, 31 has a return address and 26 and 25 have the fixnum arguments.
;; If arg2 is negative, it is a right shift, otherwise a left shift.
;;
	EXTRS	25,FIXNUM_POS,FIXNUM_LENGTH,25	; arg2
	COMB,<,N	0,25,fixnum_lsh_positive
	SUB	0,25,25				; negate, for right shift
	COMICLR,>	FIXNUM_LENGTH,25,0
	LDI	31,25				; shift right completely
	MTSAR	25
	VSHD	0,26,26				; shift right
	DEP	0,31,TC_LENGTH,26		; normalize fixnum
	BE	0(5,31)				; return
	COPY	0,25				; signal no overflow
;;
fixnum_lsh_positive
	SUBI,>	32,25,25			; shift amount for right shift
	COPY	0,25				; shift left completely
	MTSAR	25
	VSHD	26,0,26				; shift right (32 - arg2)
	BE	0(5,31)				; return
	COPY	0,25				; signal no overflow

;;;; Generic arithmetic utilities.
;;;  On entry the arguments are on the Scheme stack, and the return
;;;  address immediately above them.

define(define_generic_binary,
"generic_$1
	LDW	0(0,22),6			; arg1
	LDW	4(0,22),8			; arg2
	EXTRU	6,TC_START,TC_LENGTH,7		; type of arg1
	EXTRU	8,TC_START,TC_LENGTH,9		; type of arg2
	COMIB,<>,N	TC_FLONUM,7,generic_$1_fail
	COMIB,<>,N	TC_FLONUM,9,generic_$1_fail
	DEP	5,TC_START,TC_LENGTH,6		; data segment quadrant bits
	FLDDS	4(0,6),4			; arg1 -> fr4
	DEP	5,TC_START,TC_LENGTH,8		; data segment quadrant bits
	FLDDS	4(0,8),5			; arg2 -> fr5
	B	binary_flonum_result		; cons flonum and return
	$3,DBL	4,5,4				; operate

generic_$1_fail					; ?? * ??, out of line
	B	scheme_to_interface
	LDI	HEX($2),28			; operation code")

flonum_result
unary_flonum_result
	ADDI,TR	4,22,6				; ret. add. location

binary_flonum_result				; expects data in fr4.
	LDO	8(22),6				; ret. add. location
	DEPI	4,31,3,21			; align free
	COPY	21,2				; result (untagged)
	LDW	0(0,6),8			; return address
	LDIL	L'FLONUM_VECTOR_HEADER,7
	;	LDO	R'FLONUM_VECTOR_HEADER(7),7 ; Assembler bug!
	ADDI	R'FLONUM_VECTOR_HEADER,7,7
	STWM	7,4(0,21)			; vector header
	DEPI	TC_FLONUM,TC_START,TC_LENGTH,2 ; tag flonum
	DEP	5,TC_START,TC_LENGTH,8		; data segment quadrant bits
	FSTDS,MA	4,8(0,21)		; store floating data
	BLE	0(5,8)				; return!
	LDO	4(6),22				; pop frame 

define(define_generic_binary_predicate,
"generic_$1
	LDW	0(0,22),6			; arg1
	LDW	4(0,22),8			; arg2
	EXTRU	6,TC_START,TC_LENGTH,7		; type of arg1
	EXTRU	8,TC_START,TC_LENGTH,9		; type of arg2
	COMIB,<>,N	TC_FLONUM,7,generic_$1_one_unk
	COMIB,<>,N	TC_FLONUM,9,generic_$1_two_unk
	DEP	5,TC_START,TC_LENGTH,6		; data segment quadrant bits
	FLDDS	4(0,6),4			; arg1 -> fr4
	DEP	5,TC_START,TC_LENGTH,8		; data segment quadrant bits
	FLDDS	4(0,8),5			; arg2 -> fr5
	LDO	8(22),22			; pop args from stack
	B	generic_boolean_result		; cons answer and return
	FCMP,DBL,$3	4,5			; compare

generic_$1_one_unk				; ~FLO * ??
	COMIB,<>,N	TC_FLONUM,9,generic_$1_fail
	COMICLR,=	TC_FIXNUM,7,0
	B,N	generic_$1_fail
	EXTRS	6,31,FIXNUM_LENGTH,6		; sign extend arg1
	STW	6,0(0,21)			; through memory into fpcp
	LDO	8(22),22			; pop args from stack
	DEP	5,TC_START,TC_LENGTH,8		; data segment quadrant bits
	FLDWS	0(0,21),4			; single int arg1 -> fr4
	FLDDS	4(0,8),5			; arg2 -> fr5
        FCNVXF,SGL,DBL  4,4			; convert to double float
	B	generic_boolean_result		; cons answer and return
	FCMP,DBL,$3	4,5			; compare

generic_$1_two_unk				; FLO * ~FLO
	COMICLR,=	TC_FIXNUM,9,0
	B,N	generic_$1_fail
	EXTRS	8,31,FIXNUM_LENGTH,8		; sign extend arg2
	STW	8,0(0,21)			; through memory into fpcp
	LDO	8(22),22			; pop args from stack
	DEP	5,TC_START,TC_LENGTH,6		; data segment quadrant bits
	FLDWS	0(0,21),5			; single int arg2 -> fr5
	FLDDS	4(0,6),4			; arg1 -> fr4
        FCNVXF,SGL,DBL  5,5			; convert to double float
	B	generic_boolean_result		; cons answer and return
	FCMP,DBL,$3	4,5			; compare

generic_$1_fail					; ?? * ??, out of line
	B	scheme_to_interface
	LDI	HEX($2),28			; operation code")

generic_boolean_result
	LDWM	4(0,22),8			; return address
	LDIL	L'SHARP_T,2
	FTEST
	LDIL	L'SHARP_F,2
	DEP	5,TC_START,TC_LENGTH,8		; data segment quadrant bits
	BLE,N	0(5,8)				; return!

define(define_generic_unary,
"generic_$1
	LDW	0(0,22),6			; arg
	EXTRU	6,TC_START,TC_LENGTH,7		; type of arg
	COMIB,<>,N	TC_FLONUM,7,generic_$1_fail
	LDI	1,7				; constant 1
	STW	7,0(0,21)			; into memory
	DEP	5,TC_START,TC_LENGTH,6		; data segment quadrant bits
	FLDWS	0(0,21),5			; 1 -> fr5
	FLDDS	4(0,6),4			; arg -> fr4
	FCNVXF,SGL,DBL	5,5			; convert to double float
	B	unary_flonum_result		; cons flonum and return
	$3,DBL	4,5,4				; operate

generic_$1_fail
	B	scheme_to_interface
	LDI	HEX($2),28			; operation code")

define(define_generic_unary_predicate,
"generic_$1
	LDW	0(0,22),6			; arg
	EXTRU	6,TC_START,TC_LENGTH,7		; type of arg
	COMIB,<>,N	TC_FLONUM,7,generic_$1_fail
	DEP	5,TC_START,TC_LENGTH,6		; data segment quadrant bits
	FLDDS	4(0,6),4			; arg -> fr4
	LDO	4(22),22			; pop arg from stack
	B	generic_boolean_result		; cons answer and return
	FCMP,DBL,$3	4,0			; compare

generic_$1_fail
	B	scheme_to_interface
	LDI	HEX($2),28			; operation code")

define_generic_unary(decrement,22,FSUB)
define_generic_binary(divide,23,FDIV)
define_generic_binary_predicate(equal,24,=)
define_generic_binary_predicate(greater,25,>)
define_generic_unary(increment,26,FADD)
define_generic_binary_predicate(less,27,<)
define_generic_binary(subtract,28,FSUB)
define_generic_binary(times,29,FMPY)
define_generic_unary_predicate(negative,2a,<)
define_generic_binary(plus,2b,FADD)
define_generic_unary_predicate(positive,2c,>)
define_generic_unary_predicate(zero,2d,=)

;;;; Optimized procedure application for unknown procedures.
;;;  Procedure in r26, arity (for shortcircuit-apply) in r25.

shortcircuit_apply
	EXTRU	26,5,6,24			; procedure type -> 24
	COMICLR,=	TC_CCENTRY,24,0
	B,N	shortcircuit_apply_lose
	DEP	5,5,6,26			; procedure -> address
	LDB	-3(0,26),23			; procedure's frame-size
	COMB,<>,N	25,23,shortcircuit_apply_lose
	BLE,N	0(5,26)				; invoke procedure

define(define_shortcircuit_fixed,
"shortcircuit_apply_$1
	EXTRU	26,5,6,24			; procedure type -> 24
	COMICLR,=	TC_CCENTRY,24,0
	B	shortcircuit_apply_lose
	LDI	$1,25
	DEP	5,5,6,26			; procedure -> address
	LDB	-3(0,26),23			; procedure's frame-size
	COMB,<>,N	25,23,shortcircuit_apply_lose
	BLE,N	0(5,26)				; invoke procedure")

define_shortcircuit_fixed(1)
define_shortcircuit_fixed(2)
define_shortcircuit_fixed(3)
define_shortcircuit_fixed(4)
define_shortcircuit_fixed(5)
define_shortcircuit_fixed(6)
define_shortcircuit_fixed(7)
define_shortcircuit_fixed(8)

shortcircuit_apply_lose
	DEP	24,5,6,26			; insert type back
	B	scheme_to_interface
	LDI	0x14,28

;;; Return address in r31.  r26 contains the offset from the return
;;; address to the interrupt invocation label.

stack_and_interrupt_check
	LDW	44(0,4),25			; Stack_Guard -> r25
	LDW	0(0,4),20			; MemTop -> r20
;;;
;;; If the Scheme stack pointer is <= Stack_Guard, then the stack has
;;; overflowed -- in which case we must signal a stack-overflow interrupt.
	COMB,<=,N 22,25,stack_and_interrupt_check_stack_overflow
;;;
;;; If (Free >= MemTop), signal an interrupt.
	COMB,>=,N 21,20,stack_and_interrupt_check_signal_interrupt
;;;
;;; Otherwise, return normally -- there's nothing to do.
	BE	0(5,31)
	NOP

stack_and_interrupt_check_stack_overflow
	LDW	48(0,4),25			; IntCode -> r25
	LDW	4(0,4),24			; IntEnb -> r24
;;;
;;; Set the stack-overflow interrupt bit and write the interrupt word
;;; back out to memory.  If the stack-overflow interrupt is disabled,
;;; skip forward to gc test.  Otherwise, set MemTop to -1 and signal
;;; the interrupt.
	DEPI	1,INT_BIT_STACK_OVERFLOW,1,25
	BB,>=	24,INT_BIT_STACK_OVERFLOW,stack_and_interrupt_check_no_overflow
	STW	25,48(0,4)			; r25 -> IntCode
	ADDI	-1,0,20				; -1 -> r20
	STW	20,0(0,4)			; r20 -> MemTop
;;;
;;; If (Free >= MemTop), signal an interrupt.
stack_and_interrupt_check_no_overflow
	SUB,<	21,20,0				; skip next inst.
						;  if (Free < MemTop)
;;;
;;; To signal the interrupt, add the interrupt invocation offset to
;;; the return address, then return normally.
stack_and_interrupt_check_signal_interrupt
	ADD	26,31,31
	BE	0(5,31)				; return
	NOP

;;; invoke_primitive and *cons all have the same interface:
;;; The "return address" in r31 points to a word containing
;;; the distance between itself and the word in memory containing
;;; the primitive object.
;;; All arguments are passed on the stack, ready for the primitive.

invoke_primitive
	DEPI	0,31,2,31			; clear privilege bits
	LDW	0(0,31),26			; get offset
	ADDIL	L'hppa_primitive_table-$global$,27
	LDWX	26(0,31),26			; get primitive
	LDW	R'hppa_primitive_table-$global$(1),25
	EXTRU	26,31,DATUM_LENGTH,24		; get primitive index
	STW	26,32(0,4)			; store primitive
	ADDIL	L'Primitive_Arity_Table-$global$,27
	LDW	R'Primitive_Arity_Table-$global$(1),18
	LDWX,S	24(0,25),25			; find primitive entry point
	ADDIL	L'sp_register-$global$,27
	STW	22,R'sp_register-$global$(1)	; Update stack pointer
	ADDIL	L'Free-$global$,27
	LDWX,S	24(0,18),18			; primitive arity
	STW	21,R'Free-$global$(1)		; Update free	
	.CALL	RTNVAL=GR			; out=28
	BLE	0(4,25)				; Call primitive
	COPY	31,2				; Setup return address

	ADDIL	L'sp_register-$global$,27
	LDW	R'sp_register-$global$(1),22	; Setup stack pointer
	COPY	28,2				; Move result to val
	SH2ADD	18,22,22			; pop frame
	LDWM	4(0,22),26			; return address as object
	STW	0,32(0,4)			; clear primitive
	B	ep_interface_to_scheme_2
	DEP	5,TC_START,TC_LENGTH,26		; return address as address

;;; The BLE in invoke_primitive can jump here.
;;; The primitive index is in gr24

cross_segment_call
	ADDIL	L'Primitive_Procedure_Table-$global$,27
	LDW	R'Primitive_Procedure_Table-$global$(1),22
	LDWX,S	24(0,22),22
	B,N	$$dyncall			; ignore the return address

vector_cons
	LDW	0(0,22),26			; length as fixnum
	COPY	21,2
	ZDEP	26,31,DATUM_LENGTH,26		; length as machine word
	SH2ADD	26,21,25			; end of data (-1)
	COMBF,<	25,20,invoke_primitive		; no space, use primitive
	LDW	4(0,22),24			; fill value
	LDO	4(25),21			; allocate!
	STW	26,0(0,2)			; vector length (0-tagged)
	LDO	4(2),23				; start location

vector_cons_loop
	COMBT,<,N	23,21,vector_cons_loop
	STWM	24,4(0,23)			; initialize

	LDW	8(0,22),25			; return address as object
	DEPI	TC_VECTOR,TC_START,TC_LENGTH,2	; tag result
	DEP	5,TC_START,TC_LENGTH,25		; return address as address
	BLE	0(5,25)				; return!
	LDO	12(22),22			; pop stack frame

string_allocate
	LDW	0(0,22),26			; length as fixnum
	COPY	21,2				; return value
	ZDEP	26,31,DATUM_LENGTH,26		; length as machine word
	ADD	26,21,25			; end of data (-(9+round))
	COMBF,<	25,20,invoke_primitive		; no space, use primitive
	SHD	0,26,2,24			; scale down to word
	STB	0,8(0,25)			; end-of-string #\NUL
	LDO	2(24),24			; total word size (-1)
	STWS,MB	26,4(0,21)			; store string length
	LDI	TC_NMV,1
	SH2ADD	24,21,21			; allocate!
	DEP	1,TC_START,TC_LENGTH,24		; tag header
	LDW	4(0,22),25			; return address as object
	STW	24,0(0,2)			; store nmv header
	LDI	TC_STRING,1
	DEP	5,TC_START,TC_LENGTH,25		; return address as address
	DEP	1,TC_START,TC_LENGTH,2		; tag result
	BLE	0(5,25)				; return!
	LDO	8(22),22			; pop stack frame

floating_vector_cons
	LDW	0(0,22),26			; length as fixnum
	; STW	0,0(0,21)			; make heap parseable
	DEPI	4,31,3,21			; bump free past header
	COPY	21,2				; return value
	ZDEP	26,31,DATUM_LENGTH,26		; length as machine word
	SH3ADD	26,21,25			; end of data (-1)
	COMBF,<	25,20,invoke_primitive		; no space, use primitive
	SHD	26,0,31,26			; scale, harmless in delay slot
	LDO	4(25),21			; allocate!
	LDI	TC_NMV,1
	DEP	1,TC_START,TC_LENGTH,26		; tag header
	LDW	4(0,22),25			; return address as object
	STW	26,0(0,2)			; store nmv header
	DEPI	TC_FLONUM,TC_START,TC_LENGTH,2	; tag result
	DEP	5,TC_START,TC_LENGTH,25		; return address as address
	BLE	0(5,25)				; return!
	LDO	8(22),22			; pop stack frame

define(define_floating_point_util,
"flonum_$1
	STW	2,8(0,4)			; preserve val
	COPY	22,18				; preserve regs
	COPY	21,17
	COPY	19,16
        .CALL   ARGW0=FR,ARGW1=FU,RTNVAL=FU     ;fpin=105;fpout=104;
	BL	$2,2
	COPY	31,15
	COPY	16,19
	COPY	17,21
	COPY	18,22
	LDW	8(0,4),2			; restore val
	BE	0(5,15)
	LDW	0(0,4),20")

define_floating_point_util(sin,sin)
define_floating_point_util(cos,cos)
define_floating_point_util(tan,tan)
define_floating_point_util(asin,asin)
define_floating_point_util(acos,acos)
define_floating_point_util(atan,atan)
define_floating_point_util(exp,exp)
define_floating_point_util(log,log)
define_floating_point_util(truncate,double_truncate)
define_floating_point_util(ceiling,ceil)
define_floating_point_util(floor,floor)

flonum_atan2
	STW	2,8(0,4)			; preserve val
	COPY	22,18				; preserve regs
	COPY	21,17
	COPY	19,16
        .CALL   ARGW0=FR,ARGW1=FU,ARGW2=FR,ARGW3=FU,RTNVAL=FU   ;fpin=105,107;fpout=104;
	BL	atan2,2
	COPY	31,15
	COPY	16,19
	COPY	17,21
	COPY	18,22
	LDW	8(0,4),2			; restore val
	BE	0(5,15)
	LDW	0(0,4),20

compiled_code_bkpt
	LDO	-4(31),31			; bump back to entry point
	COPY	19,25				; Preserve Dynamic link
	B	trampoline_to_interface
	LDI	0x3c,28

compiled_closure_bkpt
	LDO	-12(31),31			; bump back to entry point
	B	trampoline_to_interface
	LDI	0x3d,28

closure_entry_bkpt
	LDO	-4(31),31			; bump back to entry point
	B	trampoline_to_interface
	LDI	0x3c,28

;; On arrival, 31 has a return address.  The word at the return
;; address has the offset between the return address and the
;; closure pattern.
;; Returns the address of the entry point in 25
;; Used: 29, 28, 26, 25, fp11, fp10 [31]

copy_closure_pattern
	LDW	-3(0,31),29			; offset
	DEPI	4,31,3,21			; quad align
	ADD	29,31,29			; addr of pattern
	LDWS,MA	4(0,29),28			; load pattern header
	LDO	8(21),25			; preserve for FDC & FIC
	STWS,MA	28,4(0,21)			; store pattern header
	FLDDS,MA	8(0,29),10		; load entry
	FLDDS,MA	8(0,29),11
	FSTDS,MA	10,8(0,21)		; store entry
	FSTDS,MA	11,8(0,21)
	FDC	0(0,25)
	FDC	0(0,21)
	SYNC
	FIC	0(5,25)
	BE	4(5,31)
	SYNC

;; On arrival, 31 has a return address and 1 contains the number of
;; entries in the closure.  The word at the return address has the
;; offset between the return address and the closure pattern.
;; Returns the address of the entry point in 25
;; Used: 29, 28, 26, 25, fp11, fp10 [31, 1]

copy_multiclosure_pattern
	LDW	-3(0,31),29			; offset
	DEPI	4,31,3,21			; quad align
	ADD	29,31,29			; addr of pattern
	LDWS,MA	4(0,29),28			; load pattern header
	LDO	12(21),25			; preserve for FIC
	STWS,MA	28,4(0,21)			; store pattern header
	LDI	-16,26				; FDC index
	
copy_multiclosure_pattern_loop
	FLDDS,MA	8(0,29),10		; load entry
	FLDDS,MA	8(0,29),11
	FSTDS,MA	10,8(0,21)		; store entry
	FSTDS,MA	11,8(0,21)
	ADDIB,>	-1,1,copy_multiclosure_pattern_loop
	FDC	26(0,21)

	LDWS,MA	4(0,29),28			; load pattern tail
	COPY	21,26
	STWS,MA 28,4(0,21)			; store pattern tail
	FDC	0(0,26)
	SYNC
	FIC	0(5,25)
	BE	4(5,31)				; return
	SYNC

;; This label is used by the trap handler

ep_scheme_hooks_high

;;;; Assembly language entry point used by utilities in cmpint.c
;;;  to return to the interpreter.
;;;  It returns from C_to_interface.

ep_interface_to_C
	COPY	29,28				; Setup C value
        LDW     -eval(C_FRAME_SIZE+20)(0,30),2	; Restore return address
        LDW     -52(0,30),18			; Restore saved registers
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
        BV      0(2)				; Return
        .EXIT
        LDWM    -eval(C_FRAME_SIZE)(0,30),3	; Restore last reg, pop frame
        .PROCEND				;in=26;out=28;

;;;; Procedure to initialize this interface.
;;;
;;; C signature:
;;;
;;; void initialize_interface (void);

interface_initialize
	.PROC
	.CALLINFO CALLER,FRAME=4,SAVE_RP
	.ENTRY
	STW	2,-20(0,30)			; Preserve return address
	LDO	64(30),30			; Allocate stack frame
	STW	3,-64(30)			; Preserve gr3
	FSTWS	0,-4(30)
	LDW	-4(30),22
	LDI	30,21				; enable V, Z, O, U traps
	OR	21,22,22
	STW	22,-4(30)
	FLDWS	-4(30),0
						; Prepare entry points
	BL	known_pc,3			; get pc
	NOP
known_pc

define(store_entry_point,"ADDIL	L'ep_$1-known_pc,3
	LDO	R'ep_$1-known_pc(1),29
	ADDIL	L'$1-$global$,27
	STW	29,R'$1-$global$(1)")

	store_entry_point(interface_to_scheme)
	store_entry_point(interface_to_C)

changequote([,])
define(builtin,[ADDIL	L'$1-known_pc,3
	LDO	R'$1-known_pc(1),26
	ADDIL	L'$1_string-$global$,27
	.CALL	ARGW0=GR
	BL	declare_builtin,2
	LDO	R'$1_string-$global$(1),25 divert(1)
$1_string
	.ALIGN	8
	.STRINGZ "$1" divert(0)])

	builtin(scheme_to_interface_ble)
	builtin(ep_scheme_hooks_low)
	builtin(store_closure_entry)
	builtin(store_closure_code)
	builtin(multiply_fixnum)
	builtin(fixnum_quotient)
	builtin(fixnum_remainder)
	builtin(fixnum_lsh)
	builtin(flonum_result)
	builtin(generic_boolean_result)
	builtin(generic_decrement)
	builtin(generic_divide)
	builtin(generic_equal)
	builtin(generic_greater)
	builtin(generic_increment)
	builtin(generic_less)
	builtin(generic_subtract)
	builtin(generic_times)
	builtin(generic_negative)
	builtin(generic_plus)
	builtin(generic_positive)
	builtin(generic_zero)
	builtin(shortcircuit_apply)
	builtin(shortcircuit_apply_1)
	builtin(shortcircuit_apply_2)
	builtin(shortcircuit_apply_3)
	builtin(shortcircuit_apply_4)
	builtin(shortcircuit_apply_5)
	builtin(shortcircuit_apply_6)
	builtin(shortcircuit_apply_7)
	builtin(shortcircuit_apply_8)
	builtin(stack_and_interrupt_check)
	builtin(invoke_primitive)
	builtin(cross_segment_call)
	builtin(vector_cons)
	builtin(string_allocate)
	builtin(floating_vector_cons)
	builtin(flonum_sin)
	builtin(flonum_cos)
	builtin(flonum_tan)
	builtin(flonum_asin)
	builtin(flonum_acos)
	builtin(flonum_atan)
	builtin(flonum_exp)
	builtin(flonum_log)
	builtin(flonum_truncate)
	builtin(flonum_ceiling)
	builtin(flonum_floor)
	builtin(flonum_atan2)
	builtin(compiled_code_bkpt)
	builtin(compiled_closure_bkpt)
	builtin(copy_closure_pattern)
	builtin(copy_multiclosure_pattern)
	builtin(ep_scheme_hooks_high)
changequote(",")
						; Return
	LDW	-84(30),2			; Restore return address
	LDW	-64(30),3			; Restore gr3
	BV	0(2)
	.EXIT
	LDO	-64(30),30			; De-allocate stack frame
	.PROCEND

;;;; Routine to flush some locations from the processor cache.
;;;
;;; Its C signature is
;;;
;;; void
;;; cache_flush_region (address, count, cache_set)
;;;     void *address;
;;;     long count;		/* in long words */
;;;	unsigned int cache_set;
;;;
;;; cache_set is a bit mask of the flags I_CACHE (1) and D_CACHE (2).
;;; the requested cache (or both) is flushed.
;;;
;;; We only need to flush every 16 bytes, since cache lines are
;;; architecturally required to have cache line sizes that are
;;; multiples of 16 bytes.  This is wasteful on processors with cache
;;; line sizes greater than 16 bytes, but this routine is typically
;;; called to flush very small ranges.
;;; We flush an additional time after flushing every 16 bytes since
;;; the start address may not be aligned with a cache line, and thus
;;; the end address may fall in a different cache line from the
;;; expected one.  The extra flush is harmless when not necessary.

cache_flush_region
	.PROC
        .CALLINFO CALLER,FRAME=0
	.ENTRY
	LDO	3(25),25			; add 3 to round up
	SHD	0,25,2,25			; divide count (in longs) by 4
	COPY	25,28				; save for FIC loop
	COPY	26,29				; save for FIC loop
	LDI	16,1				; increment
	BB,>=,N	24,30,process_i_cache		; if D_CACHE is not set,
						;  skip d-cache
;;;
flush_cache_fdc_loop
	ADDIB,>=	-1,25,flush_cache_fdc_loop
	FDC,M	1(0,26)
	SYNC
;;;
process_i_cache
	BB,>=,N	24,31,L$exit2			; if I_CACHE is not set, return
;;;
flush_cache_fic_loop
	ADDIB,>=	-1,28,flush_cache_fic_loop
	FIC,M	1(5,29)
;;;
L$exit2
	BV	0(2)
	.EXIT
	SYNC
	.PROCEND				;in=25,26;

;;;; Routine to flush the processor cache.
;;;
;;; Its C signature is
;;;
;;; void
;;; cache_flush_all (cache_set, cache_info)
;;;      unsigned int cache_set;
;;;      struct pdc_cache_rtn_block *cache_info;
;;;
;;; cache_set is a bit mask of the flags I_CACHE (1) and D_CACHE (2).
;;; the requested cache (or both) is flushed.
;;;
;;; struct pdc_cache_rtn_block is defined in <machine/pdc_rqsts.h> and
;;; is the structure returned by the PDC_CACHE
;;; processor-dependent-code call, and stored in the kernel variable
;;; (HP-UX) "cache_tlb_parms".  Only the cache parameters (and not the
;;; TLB parameters) are used.

cache_flush_all
	.PROC
	.CALLINFO CALLER,FRAME=24
	.ENTRY

do_d_cache
	BB,>=,N	26,30,do_i_cache		; if D_CACHE is not set,
						;  skip d-cache

	LDW	32(0,25),31			; 31 <- address (init. base)
	LDW	44(0,25),29			; 29 <- loop
	LDW	36(0,25),23			; 23 <- stride
	LDW	40(0,25),19			; 19 <- count

	LDO	-1(19),19			; decrement count
	COMIB,>,N	0,19,d_sync		; if (count < 0), no flush
	COMIB,=,N	1,29,d_direct_l
	COMIB,=,N	2,29,d_assoc2_l
	COMIB,=,N	4,29,d_assoc4_l

d_assoc_l					; set-associative flush-loop
	COPY	29,20				; 20 (lcount) <- loop

d_set_l						; set flush-loop
	LDO	-1(20),20			; decrement lcount
	COMIB,<=,N	0,20,d_set_l		; if (lcount >= 0), set loop
	FDCE	0(0,31)				; flush entry at (address)

	LDO	-1(19),19			; decrement count
	COMIB,<=	0,19,d_assoc_l		; if (count >= 0), loop
	ADD	31,23,31			; address++

	B	do_i_cache			; next
	SYNC					; synchronize after flush

d_assoc4_l					; 4-way set-associative loop
	FDCE	0(0,31)				; flush entry at (*address)
	FDCE	0(0,31)				; flush entry at (*address)
	FDCE	0(0,31)				; flush entry at (*address)
	FDCE,M	23(0,31)			; flush entry at (*address++)
	COMIB,<		0,19,d_assoc4_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

	B	do_i_cache			; next
	SYNC					; synchronize after flush

d_assoc2_l					; 2-way set-associative loop
	FDCE	0(0,31)				; flush entry at (*address)
	FDCE,M	23(0,31)			; flush entry at (*address++)
	COMIB,<		0,19,d_assoc2_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

	B	do_i_cache			; next
	SYNC					; synchronize after flush

d_direct_l					; direct-mapped flush loop
	FDCE,M	23(0,31)			; flush entry at (*address++)
	COMIB,<		0,19,d_direct_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

d_sync
	SYNC					; synchronize after flush

do_i_cache
	BB,>=,N	26,31,L$exit1			; if I_CACHE is not set, return

	LDW	8(0,25),31			; 31 <- address (init. base)
	LDW	20(0,25),29			; 29 <- loop
	LDW	12(0,25),23			; 23 <- stride
	LDW	16(0,25),19			; 19 <- count

	LDO	-1(19),19			; decrement count
	COMIB,>,N	0,19,i_sync		; if (count < 0), no flush
	COMIB,=,N	1,29,i_direct_l
	COMIB,=,N	2,29,i_assoc2_l
	COMIB,=,N	4,29,i_assoc4_l

i_assoc_l					; set-associative flush-loop
	COPY	29,20				; 20 (lcount) <- loop

i_set_l						; set flush-loop
	LDO	-1(20),20			; decrement lcount
	COMIB,<=,N	0,20,i_set_l		; if (lcount >= 0), set loop
	FICE	0(5,31)				; flush entry at (address)

	LDO	-1(19),19			; decrement count
	COMIB,<=	0,19,i_assoc_l		; if (count >= 0), loop
	ADD	31,23,31			; address++

	B	i_skips				; next
	SYNC					; synchronize after flush

i_assoc4_l					; 4-way set-associative loop
	FICE	0(5,31)				; flush entry at (*address)
	FICE	0(5,31)				; flush entry at (*address)
	FICE	0(5,31)				; flush entry at (*address)
	FICE,M	23(5,31)			; flush entry at (*address++)
	COMIB,<		0,19,i_assoc4_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

	B	i_skips				; next
	SYNC					; synchronize after flush

i_assoc2_l					; 2-way set-associative loop
	FICE	0(5,31)				; flush entry at (*address)
	FICE,M	23(5,31)			; flush entry at (*address++)
	COMIB,<		0,19,i_assoc2_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

	B	i_skips				; next
	SYNC					; synchronize after flush

i_direct_l					; direct-mapped flush loop
	FICE,M	23(5,31)			; flush entry at (*address++)
	COMIB,<		0,19,i_direct_l		; if (count > 0), loop
	LDO	-1(19),19			; decrement count

i_sync
	SYNC					; synchronize after flush

i_skips
	NOP					; 7 instructionss as prescribed
	NOP					; by the programming note in
	NOP					; the description for SYNC.
	NOP
	NOP

L$exit1
	BV	0(2)
	.EXIT
	NOP
	.PROCEND ;in=25,26;

bkpt_normal_proceed
	BL	bkpt_normal_cont,1		; Get PC
	DEP	0,31,2,1
bkpt_normal_cont
	LDW	bkpt_normal_ep-bkpt_normal_cont(0,1),1		; entry point
	BV	0(1)				; Invoke
	NOP					; Slot for first instruction
bkpt_normal_ep
	NOP					; Slot for fall through

bkpt_plus_proceed
	COMB,=	1,1,bkpt_plus_t			; Slot for first instruction
	NOP					; Slot for second instruction
	STWM	1,-4(0,22)			; Preserve 1
	BL	bkpt_plus_cont_f,1		; Get PC
	DEP	0,31,2,1
bkpt_plus_cont_f
	LDW	bkpt_plus_ep-bkpt_plus_cont_f(0,1),1		; entry point
	BV	0(1)				; Invoke
	LDWM	4(0,22),1
bkpt_plus_t
	STWM	1,-4(0,22)			; Preserve 1
	BL	bkpt_plus_cont_t,1		; Get PC
	DEP	0,31,2,1
bkpt_plus_cont_t
	LDW	bkpt_plus_bt-bkpt_plus_cont_t(0,1),1		; entry point
	BV	0(1)				; Invoke
	LDWM	4(0,22),1
bkpt_plus_ep
	NOP					; Slot for fall through
bkpt_plus_bt
	NOP					; Slot for branch target

bkpt_minus_proceed_start
bkpt_minus_t
	STWM	1,-4(0,22)			; Preserve 1
	BL	bkpt_minus_cont_t,1		; Get PC
	DEP	0,31,2,1
bkpt_minus_cont_t
	LDW	bkpt_minus_bt-bkpt_minus_cont_t(0,1),1 ; entry point
	BV	0(1)				; Invoke
	LDWM	4(0,22),1
bkpt_minus_proceed
	COMB,=	1,1,bkpt_minus_t		; Slot for first instruction
	NOP					; Slot for second instruction
	STWM	1,-4(0,22)			; Preserve 1
	BL	bkpt_minus_cont_f,1		; Get PC
	DEP	0,31,2,1
bkpt_minus_cont_f
	LDW	bkpt_minus_ep-bkpt_minus_cont_f(0,1),1 ; entry point
	BV	0(1)				; Invoke
	LDWM	4(0,22),1
bkpt_minus_ep
	NOP					; Slot for fall through
bkpt_minus_bt
	NOP					; Slot for branch target

bkpt_closure_proceed
	BL	bkpt_closure_cont,1
	DEP	0,31,2,1
bkpt_closure_cont
	LDW	bkpt_closure_entry-bkpt_closure_cont(0,1),25
	LDW	bkpt_closure_closure-bkpt_closure_cont(0,1),31
	BV	0(25)	
	COPY	31,25
bkpt_closure_closure
	NOP					; Closure object pointer
bkpt_closure_entry
	NOP					; Eventual entry point
bkpt_closure_proceed_end
	NOP	

	.SPACE	$TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
;	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.SUBSPA $UNWIND$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$
	.SPACE	$PRIVATE$
	.SUBSPA $SHORTBSS$
interface_to_scheme .COMM 4
interface_to_C .COMM 4
scheme_hooks_low .COMM 4
scheme_hooks_high .COMM 4
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
$THISMODULE$
ifelse(ASM_DEBUG,1,"interface_counter
	.ALIGN	8
	.WORD	0
interface_limit
	.WORD	0")
undivert(1)
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO
	.IMPORT $global$,DATA
	.IMPORT	Registers,DATA
	.IMPORT	sp_register,DATA
	.IMPORT	Free,DATA
	.IMPORT	hppa_utility_table,DATA
	.IMPORT	hppa_primitive_table,DATA
	.IMPORT	Primitive_Arity_Table,DATA
	.IMPORT	Primitive_Procedure_Table,DATA
	.SPACE	$TEXT$
	.SUBSPA $CODE$
        .IMPORT $$dyncall,MILLICODE
        .IMPORT $$remI,MILLICODE
	.IMPORT declare_builtin,CODE
	.IMPORT	sin,CODE
	.IMPORT	cos,CODE
	.IMPORT	tan,CODE
	.IMPORT	asin,CODE
	.IMPORT	acos,CODE
	.IMPORT	atan,CODE
	.IMPORT	exp,CODE
	.IMPORT	log,CODE
	.IMPORT	double_truncate,CODE
	.IMPORT	ceil,CODE
	.IMPORT	floor,CODE
	.IMPORT	atan2,CODE
	.EXPORT C_to_interface,PRIV_LEV=3,ARGW0=GR,RTNVAL=GR
	.EXPORT ep_interface_to_scheme,PRIV_LEV=3
	.EXPORT scheme_to_interface_ble,PRIV_LEV=3
	.EXPORT trampoline_to_interface,PRIV_LEV=3
	.EXPORT scheme_to_interface,PRIV_LEV=3
	.EXPORT hook_jump_table,PRIV_LEV=3
	.EXPORT cross_segment_call,PRIV_LEV=3
	.EXPORT	flonum_atan2,PRIV_LEV=3
	.EXPORT ep_interface_to_C,PRIV_LEV=3
	.EXPORT interface_initialize,PRIV_LEV=3
	.EXPORT cache_flush_region,PRIV_LEV=3
	.EXPORT cache_flush_all,PRIV_LEV=3
	.EXPORT bkpt_normal_proceed,PRIV_LEV=3
	.EXPORT bkpt_plus_proceed,PRIV_LEV=3
	.EXPORT bkpt_minus_proceed_start,PRIV_LEV=3
	.EXPORT bkpt_minus_proceed,PRIV_LEV=3
	.EXPORT bkpt_closure_proceed,PRIV_LEV=3
	.EXPORT bkpt_closure_proceed_end,PRIV_LEV=3
	.END
