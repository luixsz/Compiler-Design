; -- header --
          bits 64
          default rel
          ; -- variables --
          section .bss
          read_number resq 1
          ; -- contants --
          section .data
          read_format db "%id", 0 ; the format string for scanf
          string_literal_0 db "Hello, World", 0
; -- Entry Point --
          section .text
          global main
          extern ExitProcess
          extern printf
          extern scanf

          main:
          	PUSH rbp
          	mov rbp, rsp
          	SUB rsp, 32
          	; -- PUSH ---
	PUSH 10
	; -- PUSH ---
	PUSH 7
	; -- ADD ---
	 POP rax
	ADD qword [rsp], rax
	; -- PRINT ---
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	 ; -- HALT ---
	JMP EXIT_LABEL
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
