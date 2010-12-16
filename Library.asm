section .text

EXIT   equ 1
WRITE  equ 4
STDOUT equ 1
STDIN  equ 2

global input_int
global output_str
global output_int
global _start
extern main

_start:
  call main
  mov ebx,eax
  mov eax, EXIT
  int 80H

input_int:


output_str:
	push eax
	push ebx

	mov eax, WRITE
	mov ebx, STDOUT
	; implemented by Output.hs, setting up ECX to be the address of the string
	; also implemented by Output.hs, setting up EDX to be the number of chars to print

	int 80H ; Call the kernel

	pop ebx
	pop eax

	ret


output_int:
	ret
