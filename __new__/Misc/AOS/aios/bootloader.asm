[BITS 16]
[ORG 0x7C00]
xor ax, ax
mov ds, ax

cld

mov ah, 02h
mov al, 0Fh
mov ch, 00h
mov cl, 02h
mov dh, 00h
xor bx, bx
mov es, bx
mov bx, 7E00h
int 13h
jmp 0x0000:0x7E00
times 510-($-$$) db 0
dw 0xAA55

%include 'bootloader2.asm'
times 4096-($-$$) db 0

%include 'file_table.asm'
times 20480-($-$$) db 0