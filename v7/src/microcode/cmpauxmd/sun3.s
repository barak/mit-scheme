regblock_val	=	8
address_mask	=	0x3FFFFFF
	.data
	.globl	c_save_stack
c_save_stack:
	.skip	4
	.text
	.globl	_interface_initialize
_interface_initialize:
	link	a6,#0
	fmovel	#0x7480,fpcr
	unlk	a6
	rts
	.globl	_C_to_interface
_C_to_interface:
	link	a6,#-44
	moveml	d2-d7/a2-a5,a7@(4)
	movl	a6@(8),a0
	bras	interface_to_scheme_internal
	.globl	_asm_scheme_to_interface
_asm_scheme_to_interface:
	.globl	scheme_to_interface
scheme_to_interface:
	movl	a5,_Free
	movl	sp,_Ext_Stack_Pointer
	movl	c_save_stack,sp
	movl	a7@,a6
	movl	d4,a7@-
	movl	d3,a7@-
	movl	d2,a7@-
	movl	d1,a7@-
	lea	_utility_table,a0
	movl	a0@(0,d0:w:4),a0
	jsr	a0@
	lea	a7@(16),sp
	movl	d0,a0
	movl	a0@(4),d1
	movl	a0@(0),a0
	jmp	a0@
	.globl	_interface_to_scheme
_interface_to_scheme:
	movl	d1,a0
	.globl	interface_to_scheme_internal
interface_to_scheme_internal:
	movl	a6,a7@
	movl	sp,c_save_stack
	movl	_Ext_Stack_Pointer,sp
	movl	_Free,a5
	lea	_Registers,a6
	movl	#address_mask,d7
	movl	a6@(regblock_val),d0
	movl	d0,d1
	andl	d7,d1
	movl	d1,a4
	jmp	a0@
	.globl	_interface_to_C
_interface_to_C:
	movl	d1,d0
	moveml	a7@(4),d2-d7/a2-a5
	unlk	a6
	rts
	.globl	_asm_trampoline_to_interface
_asm_trampoline_to_interface:
	.globl	trampoline_to_interface
trampoline_to_interface:
	movl	a7@+,d1
	bra	scheme_to_interface
	.globl	_asm_scheme_to_interface_jsr
_asm_scheme_to_interface_jsr:
	.globl	scheme_to_interface_jsr
scheme_to_interface_jsr:
	movl	a7@+,d1
	addql	#4,d1
	bra	scheme_to_interface
	.globl	_asm_primitive_lexpr_apply
_asm_primitive_lexpr_apply:
	moveq	#0x13,d0
	bra	scheme_to_interface
	.globl	_asm_error
_asm_error:
	moveq	#0x15,d0
	bra	scheme_to_interface
	.globl	_asm_link
_asm_link:
	moveq	#0x17,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_interrupt_closure
_asm_interrupt_closure:
	moveq	#0x18,d0
	bra	scheme_to_interface
	.globl	_asm_interrupt_procedure
_asm_interrupt_procedure:
	moveq	#0x1a,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_interrupt_continuation
_asm_interrupt_continuation:
	moveq	#0x1b,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_assignment_trap
_asm_assignment_trap:
	moveq	#0x1d,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_reference_trap
_asm_reference_trap:
	moveq	#0x1f,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_safe_reference_trap
_asm_safe_reference_trap:
	moveq	#0x20,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_generic_decrement
_asm_generic_decrement:
	moveq	#0x22,d0
	bra	scheme_to_interface
	.globl	_asm_generic_divide
_asm_generic_divide:
	moveq	#0x23,d0
	bra	scheme_to_interface
	.globl	_asm_generic_equal
_asm_generic_equal:
	moveq	#0x24,d0
	bra	scheme_to_interface
	.globl	_asm_generic_greater
_asm_generic_greater:
	moveq	#0x25,d0
	bra	scheme_to_interface
	.globl	_asm_generic_increment
_asm_generic_increment:
	moveq	#0x26,d0
	bra	scheme_to_interface
	.globl	_asm_generic_less
_asm_generic_less:
	moveq	#0x27,d0
	bra	scheme_to_interface
	.globl	_asm_generic_subtract
_asm_generic_subtract:
	moveq	#0x28,d0
	bra	scheme_to_interface
	.globl	_asm_generic_multiply
_asm_generic_multiply:
	moveq	#0x29,d0
	bra	scheme_to_interface
	.globl	_asm_generic_negative
_asm_generic_negative:
	moveq	#0x2a,d0
	bra	scheme_to_interface
	.globl	_asm_generic_add
_asm_generic_add:
	moveq	#0x2b,d0
	bra	scheme_to_interface
	.globl	_asm_generic_positive
_asm_generic_positive:
	moveq	#0x2c,d0
	bra	scheme_to_interface
	.globl	_asm_generic_zero
_asm_generic_zero:
	moveq	#0x2d,d0
	bra	scheme_to_interface
	.globl	_asm_primitive_error
_asm_primitive_error:
	moveq	#0x36,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_interrupt_dlink
_asm_interrupt_dlink:
	movl	a4,d2
	moveq	#0x19,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_primitive_apply
_asm_primitive_apply:
	movl	a5,_Free
	movl	sp,_Ext_Stack_Pointer
	movl	c_save_stack,sp
	movl	a7@,a6
	movl	d1,a7@-
	lea	_utility_table,a0
	movl	a0@(0x12*4),a0
	jsr	a0@
	lea	a7@(4),sp
	movl	d0,a0
	movl	a0@(4),d1
	movl	a0@(0),a0
	jmp	a0@
tc_compiled_entry	=	0x28
offset_apply	=	0x14
	.globl	_asm_shortcircuit_apply
_asm_shortcircuit_apply:
	.globl	shortcircuit_apply
shortcircuit_apply:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_2
	andl	d7,d1
	movl	d1,a0
	movb	a0@(-3),d1
	extw	d1
	cmpw	d1,d2
	bnes	shortcircuit_apply_1
	jmp	a0@
	.globl	shortcircuit_apply_1
shortcircuit_apply_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_2
shortcircuit_apply_2:
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_shortcircuit_apply_size_1
_asm_shortcircuit_apply_size_1:
	.globl	shortcircuit_apply_size_1
shortcircuit_apply_size_1:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_size_1_2
	andl	d7,d1
	movl	d1,a0
	cmpb	#1,a0@(-3)
	bnes	shortcircuit_apply_size_1_1
	jmp	a0@
	.globl	shortcircuit_apply_size_1_1
shortcircuit_apply_size_1_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_size_1_2
shortcircuit_apply_size_1_2:
	moveq	#1,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_shortcircuit_apply_size_2
_asm_shortcircuit_apply_size_2:
	.globl	shortcircuit_apply_size_2
shortcircuit_apply_size_2:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_size_2_2
	andl	d7,d1
	movl	d1,a0
	cmpb	#2,a0@(-3)
	bnes	shortcircuit_apply_size_2_1
	jmp	a0@
	.globl	shortcircuit_apply_size_2_1
shortcircuit_apply_size_2_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_size_2_2
shortcircuit_apply_size_2_2:
	moveq	#2,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_shortcircuit_apply_size_3
_asm_shortcircuit_apply_size_3:
	.globl	shortcircuit_apply_size_3
shortcircuit_apply_size_3:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_size_3_2
	andl	d7,d1
	movl	d1,a0
	cmpb	#3,a0@(-3)
	bnes	shortcircuit_apply_size_3_1
	jmp	a0@
	.globl	shortcircuit_apply_size_3_1
shortcircuit_apply_size_3_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_size_3_2
shortcircuit_apply_size_3_2:
	moveq	#3,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_shortcircuit_apply_size_4
_asm_shortcircuit_apply_size_4:
	.globl	shortcircuit_apply_size_4
shortcircuit_apply_size_4:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_size_4_2
	andl	d7,d1
	movl	d1,a0
	cmpb	#4,a0@(-3)
	bnes	shortcircuit_apply_size_4_1
	jmp	a0@
	.globl	shortcircuit_apply_size_4_1
shortcircuit_apply_size_4_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_size_4_2
shortcircuit_apply_size_4_2:
	moveq	#4,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_shortcircuit_apply_size_5
_asm_shortcircuit_apply_size_5:
	.globl	shortcircuit_apply_size_5
shortcircuit_apply_size_5:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_size_5_2
	andl	d7,d1
	movl	d1,a0
	cmpb	#5,a0@(-3)
	bnes	shortcircuit_apply_size_5_1
	jmp	a0@
	.globl	shortcircuit_apply_size_5_1
shortcircuit_apply_size_5_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_size_5_2
shortcircuit_apply_size_5_2:
	moveq	#5,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_shortcircuit_apply_size_6
_asm_shortcircuit_apply_size_6:
	.globl	shortcircuit_apply_size_6
shortcircuit_apply_size_6:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_size_6_2
	andl	d7,d1
	movl	d1,a0
	cmpb	#6,a0@(-3)
	bnes	shortcircuit_apply_size_6_1
	jmp	a0@
	.globl	shortcircuit_apply_size_6_1
shortcircuit_apply_size_6_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_size_6_2
shortcircuit_apply_size_6_2:
	moveq	#6,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_shortcircuit_apply_size_7
_asm_shortcircuit_apply_size_7:
	.globl	shortcircuit_apply_size_7
shortcircuit_apply_size_7:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_size_7_2
	andl	d7,d1
	movl	d1,a0
	cmpb	#7,a0@(-3)
	bnes	shortcircuit_apply_size_7_1
	jmp	a0@
	.globl	shortcircuit_apply_size_7_1
shortcircuit_apply_size_7_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_size_7_2
shortcircuit_apply_size_7_2:
	moveq	#7,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_shortcircuit_apply_size_8
_asm_shortcircuit_apply_size_8:
	.globl	shortcircuit_apply_size_8
shortcircuit_apply_size_8:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_size_8_2
	andl	d7,d1
	movl	d1,a0
	cmpb	#8,a0@(-3)
	bnes	shortcircuit_apply_size_8_1
	jmp	a0@
	.globl	shortcircuit_apply_size_8_1
shortcircuit_apply_size_8_1:
	movl	a7@(-4),d1
	.globl	shortcircuit_apply_size_8_2
shortcircuit_apply_size_8_2:
	moveq	#8,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
