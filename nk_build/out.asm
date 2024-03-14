section .data
    str1 db "hello", 10, "", 0
section .text
global _start
_start:
main:
    mov rax, 0
    push rax
    mov rax, 10
    push rax
label1:
    push str1
    push QWORD 7
    pop rdx
    pop rsi
    mov rax, 1
    mov rdi, 1
    syscall
    add rsp, 0
    inc QWORD [rsp + 8]
    mov rax, [rsp + 8]
    mov rbx, [rsp + 0]
    cmp rax, rbx
    jnz label1
main_ret:
    add rsp, 0
    mov rdi, 0
    mov rax, 60
    syscall
