section .data

buf: times 80 db 0
rbuf: times 80 db 0
rbuflen: dd 0
rbufptr: dd 0

err1 db `Oh no! You wanted their `
len1 equ $-err1
err2 db ` piece, but they only had `
len2 equ $-err2
err3 db `.\n`
len3 equ $-err3

section .text

EXIT   equ 1
WRITE  equ 4
READ   equ 3
STDIN  equ 0
STDOUT equ 1

global input_int
global output_str
global output_int
global bounds_check
global _start
extern main

_start:
  call main
  mov ebx,eax
  mov eax, EXIT
  int 80H

input_int:
; README: THIS BREAKS WHEN READING IN FROM A PIPE
; (however, this works fine from the terminal)
  
  mov edi, [rbufptr]
  mov esi, [rbuflen]
  xor ecx,ecx

  ii_seek:
    cmp edi, esi
    jge ii_read           ; read in if out of bounds, or no data

    mov cl,[rbuf + edi]
    cmp ecx,'0'
    jl ii_seek_next
    cmp ecx,'9'
    jg ii_seek_next
    jmp ii_main           ; data found, begin
  ii_seek_next:
    inc edi               ; loop to next char in buf
    jmp ii_seek

  ii_read:
    mov eax,READ     ; read(stdin,rbuf,79)
    mov ebx,STDIN
    mov ecx,rbuf
    mov edx,79
    int 80H

    xor edi,edi
    mov [rbuflen],eax

  ii_main:
  xor eax,eax   ; result
  mov ebx,10    ; imul is silly
  xor ecx,ecx

  mov cl,[rbuf + edi]
  cmp ecx,'-'
  jne ii_1_pos
  inc edi
  jmp ii_1_neg

  ii_1_pos:
    mov cl,[rbuf + edi] ; move the next char into ecx
    cmp ecx,'0'    ; is it less than '0', if so exit
    jl exit
    cmp ecx,'9'    ; is it more than '9', if so exit
    jg exit
    imul eax,ebx   ; multiply the current result by 10
    sub ecx,'0'    ; convert the ascii number to an actual one
    add eax,ecx    ; add it to the result register
    inc edi        ; move to next char
    jmp ii_1_pos

  ii_1_neg:
    mov cl,[rbuf + edi] ; move the next char into ecx
    cmp ecx,'0'    ; is it less than '0', if so exit
    jl exit
    cmp ecx,'9'    ; is it more than '9', if so exit
    jg exit
    imul eax,ebx   ; multiply the current result by 10
    sub ecx,'0'    ; convert the ascii number to an actual one
    sub eax,ecx    ; subtract it from the result register
    inc edi        ; move to next char
    jmp ii_1_neg

  exit:
    mov [rbufptr], edi
    ret


output_str:
  push eax
  push ebx

  mov eax, WRITE   ; write(stdout,buf,len)
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
  xor esi,esi

  test eax,eax
  jns oi_1
  mov byte [buf],'-'
  inc edi
  not eax
  inc eax
  inc esi

  oi_1:
    ; input comes into eax
    xor edx,edx
    idiv ebx
    add edx,48
    mov [buf + edi],dl
    inc edi
    cmp eax,0
    jne oi_1

  ; flip the buffer
  mov edx,edi  ; save the string length
  mov byte [buf + edx],0 ; terminate the string
  dec edi

  oi_2:
    mov al,[buf + esi] ; swap the edi'th and esi'th bytes
    mov bl,[buf + edi]
    mov [buf + edi],al
    mov [buf + esi],bl
    inc esi  ; inc esi and dec edi
    dec edi
    cmp esi,edi ; continue if esi < edi
    jl oi_2
  
  ; move the string to ecx, and the length to edx for printing
  mov ecx,buf
  jmp output_str


bounds_check:
  mov ecx,[ebx]
  cmp eax,ecx
  jg bounds_check_fail
  ret

bounds_check_fail:
  push ecx
  push eax
  mov ecx,err1
  mov edx,len1
  call output_str
  pop eax
  call output_int
  mov ecx,err2
  mov edx,len2
  call output_str
  pop eax
  call output_int
  mov ecx,err3
  mov edx,len3
  call output_str

  mov ebx,1       ; tell the OS how much of a fail the MAlice program was
  mov eax,EXIT
  int 80H
