	.file	"euclid-c.c"
# GNU C (Debian 4.6.2-6) version 4.6.2 (x86_64-linux-gnu)
#	compiled by GNU C version 4.6.2, GMP version 5.0.2, MPFR version 3.1.0-p3, MPC version 0.9
# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed:  -imultilib . -imultiarch x86_64-linux-gnu euclid-c.c
# -mtune=generic -march=x86-64 -O -Os -fverbose-asm
# options enabled:  -fasynchronous-unwind-tables -fauto-inc-dec
# -fbranch-count-reg -fcaller-saves -fcombine-stack-adjustments -fcommon
# -fcompare-elim -fcprop-registers -fcrossjumping -fcse-follow-jumps
# -fdefer-pop -fdelete-null-pointer-checks -fdevirtualize -fdwarf2-cfi-asm
# -fearly-inlining -feliminate-unused-debug-types -fexpensive-optimizations
# -fforward-propagate -ffunction-cse -fgcse -fgcse-lm
# -fguess-branch-probability -fident -fif-conversion -fif-conversion2
# -findirect-inlining -finline -finline-functions
# -finline-functions-called-once -finline-small-functions -fipa-cp
# -fipa-profile -fipa-pure-const -fipa-reference -fipa-sra
# -fira-share-save-slots -fira-share-spill-slots -fivopts
# -fkeep-static-consts -fleading-underscore -fmath-errno -fmerge-constants
# -fmerge-debug-strings -fmove-loop-invariants -fomit-frame-pointer
# -foptimize-register-move -foptimize-sibling-calls -fpartial-inlining
# -fpeephole -fpeephole2 -fprefetch-loop-arrays -freg-struct-return
# -fregmove -freorder-blocks -freorder-functions -frerun-cse-after-loop
# -fsched-critical-path-heuristic -fsched-dep-count-heuristic
# -fsched-group-heuristic -fsched-interblock -fsched-last-insn-heuristic
# -fsched-rank-heuristic -fsched-spec -fsched-spec-insn-heuristic
# -fsched-stalled-insns-dep -fschedule-insns2 -fshow-column -fsigned-zeros
# -fsplit-ivs-in-unroller -fsplit-wide-types -fstrict-aliasing
# -fstrict-overflow -fstrict-volatile-bitfields -fthread-jumps
# -ftoplevel-reorder -ftrapping-math -ftree-bit-ccp -ftree-builtin-call-dce
# -ftree-ccp -ftree-ch -ftree-copy-prop -ftree-copyrename -ftree-cselim
# -ftree-dce -ftree-dominator-opts -ftree-dse -ftree-forwprop -ftree-fre
# -ftree-loop-if-convert -ftree-loop-im -ftree-loop-ivcanon
# -ftree-loop-optimize -ftree-parallelize-loops= -ftree-phiprop -ftree-pre
# -ftree-pta -ftree-reassoc -ftree-scev-cprop -ftree-sink
# -ftree-slp-vectorize -ftree-sra -ftree-switch-conversion -ftree-ter
# -ftree-vect-loop-version -ftree-vrp -funit-at-a-time -funwind-tables
# -fvect-cost-model -fverbose-asm -fzee -fzero-initialized-in-bss
# -m128bit-long-double -m64 -m80387 -maccumulate-outgoing-args
# -malign-stringops -mfancy-math-387 -mfp-ret-in-387 -mglibc -mieee-fp
# -mmmx -mno-sse4 -mpush-args -mred-zone -msse -msse2 -mtls-direct-seg-refs

# Compiler executable checksum: 744451c0425010ece4a75d0169bfc3a5

	.text
	.globl	euclid
	.type	euclid, @function
euclid:
.LFB0:
	.cfi_startproc
	movl	%edi, %eax	# a, a
.L6:
	cmpl	%esi, %eax	# b, a
	je	.L3	#,
	jbe	.L4	#,
	subl	%esi, %eax	# b, a
	jmp	.L6	#
.L4:
	subl	%eax, %esi	# a, b
	jmp	.L6	#
.L3:
	ret
	.cfi_endproc
.LFE0:
	.size	euclid, .-euclid
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%u\n"
	.section	.text.startup,"ax",@progbits
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	pushq	%rax	#
	.cfi_def_cfa_offset 16
	movl	$12, %esi	#,
	movl	$-961633963, %edi	#,
	call	euclid	#
	movl	$.LC0, %edi	#,
	movl	%eax, %esi	# D.2049,
	xorl	%eax, %eax	#
	call	printf	#
	xorl	%eax, %eax	#
	popq	%rdx	#
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	"GCC: (Debian 4.6.2-6) 4.6.2"
	.section	.note.GNU-stack,"",@progbits
