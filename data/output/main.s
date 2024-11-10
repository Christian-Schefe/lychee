.globl main
main:
pushq %rbp
movq %rsp, %rbp
movq $10, %rax
pushq %rax
call fib
addq $8, %rsp
movq %rbp, %rsp
popq %rbp
ret
.globl fib
fib:
pushq %rbp
movq %rsp, %rbp
movq 16(%rbp), %rax
pushq %rax
movq $1, %rax
popq %rcx
cmpq %rax, %rcx
movq $0, %rax
sete %al
cmpq $0, %rax
je .L1_clause0
movq $1, %rax
jmp .L1_end
.L1_clause0:
movq 16(%rbp), %rax
pushq %rax
movq $0, %rax
popq %rcx
cmpq %rax, %rcx
movq $0, %rax
sete %al
cmpq $0, %rax
movq $0, %rax
setne %al
.L1_end:
cmpq $0, %rax
je .L0
movq 16(%rbp), %rax
movq %rbp, %rsp
popq %rbp
ret
.L0:
movq 16(%rbp), %rax
pushq %rax
movq $1, %rax
movq %rax, %rcx
popq %rax
subq %rcx, %rax
pushq %rax
call fib
addq $8, %rsp
pushq %rax
movq 16(%rbp), %rax
pushq %rax
movq $2, %rax
movq %rax, %rcx
popq %rax
subq %rcx, %rax
pushq %rax
call fib
addq $8, %rsp
popq %rcx
addq %rcx, %rax
movq %rbp, %rsp
popq %rbp
ret
