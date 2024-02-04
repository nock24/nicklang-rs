section .data
section .text
    global _start
_start:
main:
    mov rax, 2
    push rax
    mov rax, 1
    push rax
    call f
    add rsp, 16
    push rax
    push QWORD [rsp + 0]
    pop rdi
    mov rax, 60
    syscall
main_ret:
    add rsp, 8
    mov rdi, 0
    mov rax, 60
    syscall
f:
    push QWORD [rsp + 8]
    push QWORD [rsp + 24]
    push QWORD [rsp + 8]
    push QWORD [rsp + 8]
    pop rbx
    pop rax
    add rax, rbx
    push rax
    pop rax
    jmp f_ret
f_ret:
    add rsp, 0
    add rsp, 16
    ret
