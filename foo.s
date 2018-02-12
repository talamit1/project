%include "scheme.s"

section .data
start_of_data:

sobNil:
	dq SOB_NIL
sobTrue:
	dq SOB_TRUE
sobFalse:
	dq SOB_FALSE
sobVoid:
	dq SOB_VOID
sobNumber1:
	dq MAKE_LITERAL(T_INTEGER ,1)
sobNumber2:
	dq MAKE_LITERAL(T_INTEGER ,2)
sobNumber3:
	dq MAKE_LITERAL(T_INTEGER ,3)
sobNumber4:
	dq MAKE_LITERAL(T_INTEGER ,4)
sobNumber9:
	dq MAKE_LITERAL(T_INTEGER ,9)

section .bss
extern exit, printf, scanf
section .text

main:
	nop


	;or-start
	mov rax, qword[sobNumber1]
	cmp rax,SOB_FALSE
	jne L_or_end_1
	mov rax, qword[sobNumber2]
	cmp rax,SOB_FALSE
	jne L_or_end_1
	mov rax, qword[sobNumber3]
	cmp rax,SOB_FALSE
	jne L_or_end_1
	mov rax, qword[sobNumber4]
	L_or_end_1:
	;or-end
	push RAX
	call write_sob
	add rsp,8
	mov rdi, newline
	mov rax, 0
	call printf




	mov rax, qword[sobNumber2]
	push RAX
	call write_sob
	add rsp,8
	mov rdi, newline
	mov rax, 0
	call printf




	;if-start
	mov rax, qword[sobNumber1]
	cmp rax,SOB_FALSE
	je L_if_else_1
	mov rax, qword[sobNumber2]
	jmp L_if_end_1
	L_if_else_1:
	mov rax, qword[sobNumber3]
	L_if_end_1:
	;end-if
	push RAX
	call write_sob
	add rsp,8
	mov rdi, newline
	mov rax, 0
	call printf




	mov rax, qword[sobNumber1]
	mov rax, qword[sobNumber9]
	mov rax, qword[sobNumber3]
	push RAX
	call write_sob
	add rsp,8
	mov rdi, newline
	mov rax, 0
	call printf


	ret


section .data
newline:
	db CHAR_NEWLINE, 0
