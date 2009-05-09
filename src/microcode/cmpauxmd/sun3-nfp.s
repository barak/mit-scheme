regblock_memtop	=	0
regblock_int_mask	=	4
regblock_val	=	8
regblock_stack_guard	=	44
regblock_int_code	=	48
address_mask	=	0x3FFFFFF
	.data
	.globl	c_save_stack
c_save_stack:
	.skip	4
	.text
	.globl	_interface_initialize
_interface_initialize:
	link	a6,#0
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
	movl	d6,a6@(regblock_val)
	movl	a5,_Free
	movl	sp,_sp_register
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
	movl	_sp_register,sp
	movl	_Free,a5
	lea	_Registers,a6
	movl	#address_mask,d7
	movl	a6@(regblock_val),d6
	movl	d6,d0
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
	.globl	_asm_primitive_error
_asm_primitive_error:
	moveq	#0x36,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_generic_quotient
_asm_generic_quotient:
	moveq	#0x37,d0
	bra	scheme_to_interface
	.globl	_asm_generic_remainder
_asm_generic_remainder:
	moveq	#0x38,d0
	bra	scheme_to_interface
	.globl	_asm_generic_modulo
_asm_generic_modulo:
	moveq	#0x39,d0
	bra	scheme_to_interface
	.globl	_asm_interrupt_dlink
_asm_interrupt_dlink:
	movl	a4,d2
	moveq	#0x19,d0
	bra	scheme_to_interface_jsr
	.globl	_asm_primitive_apply
_asm_primitive_apply:
	movl	a5,_Free
	movl	sp,_sp_register
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
tc_flonum	=	0x06
tc_fixnum	=	0x1A
tc_manifest_nmv	=	0x27
tc_false	=	0x0
tc_true	=	0x8
offset_apply	=	0x14
	.globl	_asm_shortcircuit_apply
_asm_shortcircuit_apply:
	.globl	shortcircuit_apply
shortcircuit_apply:
	movb	a7@,d0
	andb	#0xFC,d0
	movl	a7@+,d1
	cmpb	#tc_compiled_entry*4,d0
	bnes	shortcircuit_apply_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	movb	a0@(-3),d3
	extw	d3
	cmpw	d3,d2
	bnes	shortcircuit_apply_1
	jmp	a0@
	.globl	shortcircuit_apply_1
shortcircuit_apply_1:
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
	bnes	shortcircuit_apply_size_1_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	cmpb	#1,a0@(-3)
	bnes	shortcircuit_apply_size_1_1
	jmp	a0@
	.globl	shortcircuit_apply_size_1_1
shortcircuit_apply_size_1_1:
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
	bnes	shortcircuit_apply_size_2_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	cmpb	#2,a0@(-3)
	bnes	shortcircuit_apply_size_2_1
	jmp	a0@
	.globl	shortcircuit_apply_size_2_1
shortcircuit_apply_size_2_1:
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
	bnes	shortcircuit_apply_size_3_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	cmpb	#3,a0@(-3)
	bnes	shortcircuit_apply_size_3_1
	jmp	a0@
	.globl	shortcircuit_apply_size_3_1
shortcircuit_apply_size_3_1:
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
	bnes	shortcircuit_apply_size_4_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	cmpb	#4,a0@(-3)
	bnes	shortcircuit_apply_size_4_1
	jmp	a0@
	.globl	shortcircuit_apply_size_4_1
shortcircuit_apply_size_4_1:
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
	bnes	shortcircuit_apply_size_5_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	cmpb	#5,a0@(-3)
	bnes	shortcircuit_apply_size_5_1
	jmp	a0@
	.globl	shortcircuit_apply_size_5_1
shortcircuit_apply_size_5_1:
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
	bnes	shortcircuit_apply_size_6_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	cmpb	#6,a0@(-3)
	bnes	shortcircuit_apply_size_6_1
	jmp	a0@
	.globl	shortcircuit_apply_size_6_1
shortcircuit_apply_size_6_1:
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
	bnes	shortcircuit_apply_size_7_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	cmpb	#7,a0@(-3)
	bnes	shortcircuit_apply_size_7_1
	jmp	a0@
	.globl	shortcircuit_apply_size_7_1
shortcircuit_apply_size_7_1:
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
	bnes	shortcircuit_apply_size_8_1
	movl	d1,d3
	andl	d7,d3
	movl	d3,a0
	cmpb	#8,a0@(-3)
	bnes	shortcircuit_apply_size_8_1
	jmp	a0@
	.globl	shortcircuit_apply_size_8_1
shortcircuit_apply_size_8_1:
	moveq	#8,d2
	moveq	#offset_apply,d0
	bra	scheme_to_interface
	.globl	_asm_allocate_closure
_asm_allocate_closure:
	movl	a5,_Free
	movl	sp,_sp_register
	movl	c_save_stack,sp
	movl	a7@,a6
	movl	a1,a7@-
	movl	d1,a7@-
	movl	d0,a7@-
	jsr	_allocate_closure
	addql	#4,sp
	movl	d0,a0
	movl	a7@+,d1
	movl	a7@+,a1
	movl	a6,a7@
	movl	sp,c_save_stack
	movl	_sp_register,sp
	movl	_Free,a5
	lea	_Registers,a6
	movl	#address_mask,d7
	rts
	.globl	asm_generic_flonum_result
asm_generic_flonum_result:
	movl	a5,d6
	movl	#tc_manifest_nmv*4*0x1000000+2,a5@+
	fmoved	fp0,a5@+
	orl	#tc_flonum*4*0x1000000,d6
	andb	#1*4-1,a7@
	rts
	.globl	asm_true_result
asm_true_result:
	movl	#tc_true*4*0x1000000,d6
	andb	#1*4-1,a7@
	rts
	.globl	asm_false_result
asm_false_result:
	movl	#tc_false*4*0x1000000,d6
	andb	#1*4-1,a7@
	rts
	.globl	_asm_generic_decrement
_asm_generic_decrement:
	movb	a7@,d0
	andb	#0xFC,d0
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_decrement_hook
	movl	a7@+,d0
	andl	d7,d0
	movl	d0,a0
	fmoved	a0@(4),fp0
	fsubb	#1,fp0
	bra	asm_generic_flonum_result
asm_generic_decrement_hook:
	moveq	#0x22,d0
	bra	scheme_to_interface
	.globl	_asm_generic_divide
_asm_generic_divide:
	movb	a7@,d0
	andb	#0xFC,d0
	movb	a7@(4),d1
	andb	#0xFC,d1
	movl	a7@,d2
	movl	a7@(4),d3
	andl	d7,d2
	andl	d7,d3
	movl	d2,a0
	movl	d3,a1
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_divide_fix_flo
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_divide_flo_fix
	fmoved	a0@(4),fp0
	fdivd	a1@(4),fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_divide_fix_flo:
	cmpb	#tc_fixnum*4,d0
	bnes	asm_generic_divide_hook
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_divide_hook
	lsll	#6,d2
	asrl	#6,d2
	fmovel	d2,fp0
	fdivd	a1@(4),fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_divide_flo_fix:
	cmpb	#tc_fixnum*4,d1
	bnes	asm_generic_divide_hook
	lsll	#6,d3
	asrl	#6,d3
	fmoved	a0@(4),fp0
	fdivl	d3,fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_divide_hook:
	moveq	#0x23,d0
	bra	scheme_to_interface
	.globl	_asm_generic_equal
_asm_generic_equal:
	movb	a7@,d0
	andb	#0xFC,d0
	movb	a7@(4),d1
	andb	#0xFC,d1
	movl	a7@,d2
	movl	a7@(4),d3
	andl	d7,d2
	andl	d7,d3
	movl	d2,a0
	movl	d3,a1
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_equal_fix_flo
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_equal_flo_fix
	addql	#8,sp
	fmoved	a0@(4),fp0
	fcmpd	a1@(4),fp0
	fbeq	asm_true_result
	bra	asm_false_result
asm_generic_equal_fix_flo:
	cmpb	#tc_fixnum*4,d0
	bnes	asm_generic_equal_hook
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_equal_hook
	addql	#8,sp
	lsll	#6,d2
	asrl	#6,d2
	fmovel	d2,fp0
	fcmpd	a1@(4),fp0
	fbeq	asm_true_result
	bra	asm_false_result
asm_generic_equal_flo_fix:
	cmpb	#tc_fixnum*4,d1
	bnes	asm_generic_equal_hook
	addql	#8,sp
	lsll	#6,d3
	asrl	#6,d3
	fmoved	a0@(4),fp0
	fcmpl	d3,fp0
	fbeq	asm_true_result
	bra	asm_false_result
asm_generic_equal_hook:
	moveq	#0x24,d0
	bra	scheme_to_interface
	.globl	_asm_generic_greater
_asm_generic_greater:
	movb	a7@,d0
	andb	#0xFC,d0
	movb	a7@(4),d1
	andb	#0xFC,d1
	movl	a7@,d2
	movl	a7@(4),d3
	andl	d7,d2
	andl	d7,d3
	movl	d2,a0
	movl	d3,a1
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_greater_fix_flo
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_greater_flo_fix
	addql	#8,sp
	fmoved	a0@(4),fp0
	fcmpd	a1@(4),fp0
	fbgt	asm_true_result
	bra	asm_false_result
asm_generic_greater_fix_flo:
	cmpb	#tc_fixnum*4,d0
	bnes	asm_generic_greater_hook
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_greater_hook
	addql	#8,sp
	lsll	#6,d2
	asrl	#6,d2
	fmovel	d2,fp0
	fcmpd	a1@(4),fp0
	fbgt	asm_true_result
	bra	asm_false_result
asm_generic_greater_flo_fix:
	cmpb	#tc_fixnum*4,d1
	bnes	asm_generic_greater_hook
	addql	#8,sp
	lsll	#6,d3
	asrl	#6,d3
	fmoved	a0@(4),fp0
	fcmpl	d3,fp0
	fbgt	asm_true_result
	bra	asm_false_result
asm_generic_greater_hook:
	moveq	#0x25,d0
	bra	scheme_to_interface
	.globl	_asm_generic_increment
_asm_generic_increment:
	movb	a7@,d0
	andb	#0xFC,d0
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_increment_hook
	movl	a7@+,d0
	andl	d7,d0
	movl	d0,a0
	fmoved	a0@(4),fp0
	faddb	#1,fp0
	bra	asm_generic_flonum_result
asm_generic_increment_hook:
	moveq	#0x26,d0
	bra	scheme_to_interface
	.globl	_asm_generic_less
_asm_generic_less:
	movb	a7@,d0
	andb	#0xFC,d0
	movb	a7@(4),d1
	andb	#0xFC,d1
	movl	a7@,d2
	movl	a7@(4),d3
	andl	d7,d2
	andl	d7,d3
	movl	d2,a0
	movl	d3,a1
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_less_fix_flo
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_less_flo_fix
	addql	#8,sp
	fmoved	a0@(4),fp0
	fcmpd	a1@(4),fp0
	fblt	asm_true_result
	bra	asm_false_result
asm_generic_less_fix_flo:
	cmpb	#tc_fixnum*4,d0
	bnes	asm_generic_less_hook
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_less_hook
	addql	#8,sp
	lsll	#6,d2
	asrl	#6,d2
	fmovel	d2,fp0
	fcmpd	a1@(4),fp0
	fblt	asm_true_result
	bra	asm_false_result
asm_generic_less_flo_fix:
	cmpb	#tc_fixnum*4,d1
	bnes	asm_generic_less_hook
	addql	#8,sp
	lsll	#6,d3
	asrl	#6,d3
	fmoved	a0@(4),fp0
	fcmpl	d3,fp0
	fblt	asm_true_result
	bra	asm_false_result
asm_generic_less_hook:
	moveq	#0x27,d0
	bra	scheme_to_interface
	.globl	_asm_generic_subtract
_asm_generic_subtract:
	movb	a7@,d0
	andb	#0xFC,d0
	movb	a7@(4),d1
	andb	#0xFC,d1
	movl	a7@,d2
	movl	a7@(4),d3
	andl	d7,d2
	andl	d7,d3
	movl	d2,a0
	movl	d3,a1
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_subtract_fix_flo
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_subtract_flo_fix
	fmoved	a0@(4),fp0
	fsubd	a1@(4),fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_subtract_fix_flo:
	cmpb	#tc_fixnum*4,d0
	bnes	asm_generic_subtract_hook
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_subtract_hook
	lsll	#6,d2
	asrl	#6,d2
	fmovel	d2,fp0
	fsubd	a1@(4),fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_subtract_flo_fix:
	cmpb	#tc_fixnum*4,d1
	bnes	asm_generic_subtract_hook
	lsll	#6,d3
	asrl	#6,d3
	fmoved	a0@(4),fp0
	fsubl	d3,fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_subtract_hook:
	moveq	#0x28,d0
	bra	scheme_to_interface
	.globl	_asm_generic_multiply
_asm_generic_multiply:
	movb	a7@,d0
	andb	#0xFC,d0
	movb	a7@(4),d1
	andb	#0xFC,d1
	movl	a7@,d2
	movl	a7@(4),d3
	andl	d7,d2
	andl	d7,d3
	movl	d2,a0
	movl	d3,a1
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_multiply_fix_flo
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_multiply_flo_fix
	fmoved	a0@(4),fp0
	fmuld	a1@(4),fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_multiply_fix_flo:
	cmpb	#tc_fixnum*4,d0
	bnes	asm_generic_multiply_hook
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_multiply_hook
	lsll	#6,d2
	asrl	#6,d2
	fmovel	d2,fp0
	fmuld	a1@(4),fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_multiply_flo_fix:
	cmpb	#tc_fixnum*4,d1
	bnes	asm_generic_multiply_hook
	lsll	#6,d3
	asrl	#6,d3
	fmoved	a0@(4),fp0
	fmull	d3,fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_multiply_hook:
	moveq	#0x29,d0
	bra	scheme_to_interface
	.globl	_asm_generic_negative
_asm_generic_negative:
	movb	a7@,d0
	andb	#0xFC,d0
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_negative_hook
	movl	a7@+,d0
	andl	d7,d0
	movl	d0,a0
	fmoved	a0@(4),fp0
	fblt	asm_true_result
	bra	asm_false_result
asm_generic_negative_hook:
	moveq	#0x2a,d0
	bra	scheme_to_interface
	.globl	_asm_generic_add
_asm_generic_add:
	movb	a7@,d0
	andb	#0xFC,d0
	movb	a7@(4),d1
	andb	#0xFC,d1
	movl	a7@,d2
	movl	a7@(4),d3
	andl	d7,d2
	andl	d7,d3
	movl	d2,a0
	movl	d3,a1
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_add_fix_flo
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_add_flo_fix
	fmoved	a0@(4),fp0
	faddd	a1@(4),fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_add_fix_flo:
	cmpb	#tc_fixnum*4,d0
	bnes	asm_generic_add_hook
	cmpb	#tc_flonum*4,d1
	bnes	asm_generic_add_hook
	lsll	#6,d2
	asrl	#6,d2
	fmovel	d2,fp0
	faddd	a1@(4),fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_add_flo_fix:
	cmpb	#tc_fixnum*4,d1
	bnes	asm_generic_add_hook
	lsll	#6,d3
	asrl	#6,d3
	fmoved	a0@(4),fp0
	faddl	d3,fp0
	addql	#8,sp
	bra	asm_generic_flonum_result
asm_generic_add_hook:
	moveq	#0x2b,d0
	bra	scheme_to_interface
	.globl	_asm_generic_positive
_asm_generic_positive:
	movb	a7@,d0
	andb	#0xFC,d0
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_positive_hook
	movl	a7@+,d0
	andl	d7,d0
	movl	d0,a0
	fmoved	a0@(4),fp0
	fbgt	asm_true_result
	bra	asm_false_result
asm_generic_positive_hook:
	moveq	#0x2c,d0
	bra	scheme_to_interface
	.globl	_asm_generic_zero
_asm_generic_zero:
	movb	a7@,d0
	andb	#0xFC,d0
	cmpb	#tc_flonum*4,d0
	bnes	asm_generic_zero_hook
	movl	a7@+,d0
	andl	d7,d0
	movl	d0,a0
	fmoved	a0@(4),fp0
	fbeq	asm_true_result
	bra	asm_false_result
asm_generic_zero_hook:
	moveq	#0x2d,d0
	bra	scheme_to_interface
	.globl	_asm_stack_and_interrupt_check_12
_asm_stack_and_interrupt_check_12:
	movl	#-12,a7@-
	bras	stack_and_interrupt_check
	.globl	_asm_stack_and_interrupt_check_14
_asm_stack_and_interrupt_check_14:
	movl	#-14,a7@-
	bras	stack_and_interrupt_check
	.globl	_asm_stack_and_interrupt_check_18
_asm_stack_and_interrupt_check_18:
	movl	#-18,a7@-
	bras	stack_and_interrupt_check
	.globl	_asm_stack_and_interrupt_check_22
_asm_stack_and_interrupt_check_22:
	movl	#-22,a7@-
	bras	stack_and_interrupt_check
	.globl	_asm_stack_and_interrupt_check_24
_asm_stack_and_interrupt_check_24:
	movl	#-24,a7@-
	.globl	stack_and_interrupt_check
stack_and_interrupt_check:
	cmpl	a6@(regblock_stack_guard),sp
	bgts	stack_and_interrupt_check_1
	bset	#0,a6@(regblock_int_code+3)
	btst	#0,a6@(regblock_int_mask+3)
	beqs	stack_and_interrupt_check_1
	movl	#-1,a6@(regblock_memtop)
	bras	stack_and_interrupt_check_2
stack_and_interrupt_check_1:
	cmpl	a6@(regblock_memtop),a5
	bges	stack_and_interrupt_check_2
	addql	#4,sp
	rts
stack_and_interrupt_check_2:
	movl	d0,a7@-
	movl	a7@(4),d0
	addl	d0,a7@(8)
	movl	a7@,d0
	addql	#8,sp
	rts
	.globl	_asm_set_interrupt_enables
_asm_set_interrupt_enables:
	.globl	set_interrupt_enables
set_interrupt_enables:
	movl	a6@(regblock_int_mask),d6
	orl	#tc_fixnum*4*0x1000000,d6
	movl	a7@+,d0
	andl	d7,d0
	movl	d0,a6@(regblock_int_mask)
	moveq	#-1,d1
	movl	a6@(regblock_int_code),d2
	andl	d0,d2
	bnes	set_interrupt_enables_1
	movl	_MemTop,d1
	btst	#2,d0
	bnes	set_interrupt_enables_1
	movl	_Heap_Top,d1
set_interrupt_enables_1:
	movl	d1,a6@(regblock_memtop)
	movl	_Stack_Guard,d1
	btst	#0,d0
	bnes	set_interrupt_enables_2
	movl	_Stack_Bottom,d1
set_interrupt_enables_2:
	movl	d1,a6@(regblock_stack_guard)
	movl	a7@+,d0
	andl	d7,d0
	movl	d0,a0
	jmp	a0@
