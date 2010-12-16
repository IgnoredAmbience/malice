section .bss

buf db 80

section .text

EXIT   equ 1
WRITE  equ 4
READ   equ 3
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
	mov eax,READ     ; fread(stdin,buf,79)
	mov ebx,STDIN
	mov ecx,buf
	mov edx,79
	int 80H

	xor edi,edi
	xor eax,eax 	; result
	mov ebx,10		; imul is silly

	ii_1:
		mov esi,[buf + edi] ; move the next char into esi
		cmp esi,48		; is it less than '0', if so exit
		jl exit
		cmp esi,57		; is it more than '9', if so exit
		jg exit
		imul eax,ebx	; multiply the current result by 10
		sub esi,48		; convert the ascii number to an actual one
		add eax,esi		; add it to the result register
		inc edi			; move to next char
		jmp ii_1

	exit:
		ret


output_str:
	push eax
	push ebx

	mov eax, WRITE 	; fwrite(stdout,buf,len)
	mov ebx, STDOUT
	; implemented by Output.hs, setting up ECX to be the address of the string
	; also implemented by Output.hs, setting up EDX to be the number of chars to print

	int 80H ; Call the kernel

	pop ebx
	pop eax

	ret


output_int:
	xor edi,edi ; set counter to 0
	mov ebx,10 ; set the divisor to 10

	oi_1:
		; input comes into eax
		xor edx,edx
		idiv ebx
		add edx,48
		mov [buf + edi],edx
		inc edi
		cmp eax,0
		jne oi_1

	; flip the buffer
	mov edx,edi	; save the string length
	mov [buf + edx],0 ; terminate the string
	dec edi
	xor esi,esi

	oi_2:
		mov eax,[buf + esi] ; swap the edi'th and esi'th bytes
		mov ebx,[buf + edi]
		mov [buf + edi],eax
		mov [buf + esi],ebx
		inc esi	; inc esi and dec edi
		dec edi
		cmp esi,edi ; continue if esi < edi
		jl oi_2
	
	; move the string to ecx, and the length to edx for printing
	mov ecx,buf
	jmp output_str
