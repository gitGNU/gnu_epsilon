## [one-line description]

	.file	"euclid.c"
# I obtained this by hand-modifying the output of GCC so that
# gcd becomes essentially a transliteration of my SVM code:
	.text
.globl gcd
	.type	gcd, @function
# euclid: cmp %r1 %r2 %c0
#         be  %c0 out
# #        cmp %r1 %r2 %c0         ; not needed, of course
#         blt  %c0 less
#         sub %r1 %r2 %r1
#         b euclid
# less:   sub %r2 %r1 %r2
#         b euclid
# out:    dumpr %r1
#         exit
gcd:
.LFB0:
	.cfi_startproc
	movq	%rdi, %rax      # I want a in %rax
	movq	%rsi, %rbx      # I want b in %rbx
        # Here starts the transliteration
.EUCLID:
	cmpq	%rax, %rbx	
	je	.OUT
#	cmpq	%rax, %rbx	
	jl	.LESS
	subq	%rax, %rbx	
	jmp	.EUCLID
.LESS:
	subq	%rbx, %rax	
	jmp	.EUCLID
.OUT:
	ret
	.cfi_endproc
.LFE0:
	.size	gcd, .-gcd
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%li\n"
	.text
.globl main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	subq	$8, %rsp	#,
	.cfi_def_cfa_offset 16
	movl	$12, %esi	#,
	movl	$3333333333, %edi	#,
	call	gcd	#
	movl	$.LC0, %edi	#,
	movq	%rax, %rsi	# D.3281,
	xorl	%eax, %eax	#
	call	printf	#
	xorl	%eax, %eax	#
	addq	$8, %rsp	#,
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	"GCC: (GNU) 4.5.1"
	.section	.note.GNU-stack,"",@progbits
