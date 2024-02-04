global _start
_start:
main:
    str1 db "hello", 0
    push str1
    push QWORD [rsp + 0]
    pop rsi
    mov rax, 1
    mov rdi, 1
    mov rdx, 6
    syscall
    mov rax, 0
    push rax
    pop rdi
    mov rax, 60
    syscall
main_ret:
    add rsp, 8
    mov rdi, 0
    mov rax, 60
    syscall
