.globl main
main:
movq $5, %rax
push %rax
movq $7, %rax
pop %rcx
imulq %rcx, %rax
push %rax
movq $2, %rax
push %rax
movq $3, %rax
pop %rcx
imulq %rcx, %rax
pop %rcx
addq %rcx, %rax
push %rax
movq $4, %rax
push %rax
movq $5, %rax
push %rax
movq $1, %rax
neg %rax
pop %rcx
imulq %rcx, %rax
pop %rcx
addq %rcx, %rax
pop %rcx
subq %rcx, %rax
neg %rax
ret
