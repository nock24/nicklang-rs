section .data
    str1 db "hello world", 10, "", 0
section .text
global _start
_start:
main:
    call hello_world
    add rsp, 0
    push rax
    push rbx
    pop rdx
    pop rsi
    mov rax, 1
    mov rdi, 1
    syscall
main_ret:
    add rsp, 0
    mov rdi, 0
    mov rax, 60
    syscall
hello_world:
    push str1
    push QWORD 13
    pop rbx
    pop rax
    jmp hello_world_ret
hello_world_ret:
    add rsp, 0
    add rsp, 0
    ret
