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
section .bss
extern exit, printf, scanf
section .text
main:
	nop


	;or-start
	MAKE_LITERAL(T_INTEGER, 1)
	cmp rax,SOB_FALSE
	jne L_or_end_1
	MAKE_LITERAL(T_INTEGER, 2)
	cmp rax,SOB_FALSE
	jne L_or_end_1
	MAKE_LITERAL(T_INTEGER, 3)
	cmp rax,SOB_FALSE
	jne L_or_end_1
	MAKE_LITERAL(T_INTEGER, 4)
	L_or_end_1:
	;or-end
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
