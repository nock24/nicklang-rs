section .data
str1 db "hello", 0
section .text
    global _start
_start:
main:
    push str1
    push QWORD 5
    call f
    add rsp, 16
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
f:
    push QWORD [rsp + 16]
    push QWORD [rsp + 16]
    push QWORD [rsp + 8]
    push QWORD [rsp + 8]
    pop rbx
    pop rax
    jmp f_ret
f_ret:
    add rsp, 0
    add rsp, 16
    ret
