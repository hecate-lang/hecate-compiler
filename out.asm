	.text
	.def	@feat.00;
	.scl	3;
	.type	0;
	.endef
	.globl	@feat.00
.set @feat.00, 0
	.file	"sum"
	.def	print_int;
	.scl	2;
	.type	32;
	.endef
	.globl	print_int
	.p2align	4, 0x90
print_int:
.seh_proc print_int
	subq	$40, %rsp
	.seh_stackalloc 40
	.seh_endprologue
	movl	%ecx, %edx
	leaq	.Lfmt_str(%rip), %rcx
	callq	printf
	nop
	addq	$40, %rsp
	retq
	.seh_endproc

	.def	main;
	.scl	2;
	.type	32;
	.endef
	.globl	main
	.p2align	4, 0x90
main:
.seh_proc main
	subq	$40, %rsp
	.seh_stackalloc 40
	.seh_endprologue
	movl	$42, %ecx
	callq	print_int
	nop
	addq	$40, %rsp
	retq
	.seh_endproc

	.section	.rdata,"dr"
.Lfmt_str:
	.asciz	"debug: %d"

