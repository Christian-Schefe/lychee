.globl main
main:
movq $12, %rax
neg %rax
neg %rax
neg %rax
neg %rax
neg %rax
neg %rax
neg %rax
neg %rax
neg %rax
neg %rax
push %rax
movq $1, %rax
push %rax
movq $10, %rax
neg %rax
pop %rcx
imul %rcx, %rax
push %rax
movq $22, %rax
movq %rax, %rcx
pop %rax
subq %rcx, %rax
push %rax
movq $33, %rax
pop %rcx
add %rcx, %rax
push %rax
movq $14, %rax
neg %rax
pop %rcx
imul %rcx, %rax
push %rax
movq $7, %rax
neg %rax
movq %rax, %rcx
pop %rax
cqto
idivq %rcx
push %rax
movq $97, %rax
push %rax
movq $2, %rax
neg %rax
push %rax
movq $5, %rax
neg %rax
pop %rcx
imul %rcx, %rax
movq %rax, %rcx
pop %rax
cqto
idivq %rcx
movq %rdx, %rax
pop %rcx
add %rcx, %rax
movq %rax, %rcx
pop %rax
cqto
idivq %rcx
movq %rdx, %rax
ret
