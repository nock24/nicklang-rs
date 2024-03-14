section .data
    str1 db "first", 10, "", 0
    str2 db "hello", 10, "", 0
section .text
global _start
_start:
main:
    mov rax, 0
    push rax
    mov rax, 10
    push rax
label1:
    push QWORD [rsp + 8]
    push QWORD [rsp + 0]
    pop rax
    test rax, rax
    jz label2
    push str1
    push QWORD 7
    pop rdx
    pop rsi
    mov rax, 1
    mov rdi, 1
    syscall
    add rsp, 0
    jmp label3
label2:
    push str2
    push QWORD 7
    pop rdx
    pop rsi
    mov rax, 1
    mov rdi, 1
    syscall
    add rsp, 0
label3:
    add rsp, 0
    inc QWORD [rsp + 16]
    mov rax, [rsp + 16]
    mov rbx, [rsp + 8]
    cmp rax, rbx
    jnz label1
main_ret:
    add rsp, 0
    mov rdi, 0
    mov rax, 60
    syscall
