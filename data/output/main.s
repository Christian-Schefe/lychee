.globl main
main:
pushq %rbp
movq %rsp, %rbp
movq $5, %rax
pushq %rax
movq $4, %rax
popq %rcx
addq %rcx, %rax
movq $3, %rax
pushq %rax
movq $4, %rax
popq %rcx
addq %rcx, %rax
movq $0, %rax
movq %rbp, %rsp
popq %rbp
ret
