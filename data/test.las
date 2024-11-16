call _main
exit
_main:
push #64 bp
mov bp sp
set r0 4
sub sp r0
set r0 5
store #32 r0 [bp;8]
load #32 r0 [bp;8]
push #64 r0
set r0 1
mov r1 r0
pop #64 r0
add r0 r1
mov r1 r0
load #32 r0 [bp;8]
add r0 r1
store #32 r0 [bp;8]
load #32 r0 [bp;8]
jmp _main_return
_main_return:
mov sp bp
pop #64 bp
ret