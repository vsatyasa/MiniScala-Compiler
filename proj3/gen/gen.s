.text
.global putchar, getchar, entry_point

################# FUNCTIONS #####################
#################################################


###################### MAIN #####################
entry_point:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, heap(%rip)
	pushq %rbx
	pushq %r12
	pushq %r13
	pushq %r14
	pushq %r15
	movq heap, %rax
	movq $1, (%rax)
	movq $5, %rdi
	movq heap, %rax
	movq (%rax), %rbx
	addq %rdi, %rbx
	movq heap, %rax
	movq (%rax), %rdi
	movq heap, %rax
	movq %rbx, (%rax)
	movq %rdi, %rsi
	movq $2, %rdx
	movq $6, %rcx
	movq heap, %rax
	addq %rsi, %rdx
	movq %rcx, (%rax, %rdx, 8 )
	movq %rdi, %rsi
	movq $2, %rdx
	movq heap, %rax
	addq %rsi, %rdx
	movq (%rax, %rdx, 8 ), %rbx
	movq %rbx, %rsi
	movq %rsi, %rdi
	movq %rdi, %rax
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %rbx
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret
#################################################


#################### DATA #######################

.data
heap:	.quad 0
#################################################
