[BITS 16]
[ORG 0xCC00]

jmp start

os_attributes_in_mem equ 0xFE04

start:
mov si, os_attributes_in_mem
call os_disk_load_file_offset
jc .file_found
	.file_not_found:
		mov si, .file_not_found_msg
		call os_string_out
		call os_newline
		jmp 0x0000:0x7E19
		.file_not_found_msg db 'File not found!', 0

	.file_found:
		mov bx, 0xCE00
		mov cl, byte [si]
		call os_disk_read_file
		mov si, 0xCE00
		mov bl, 30h
		call os_string_out
		call os_newline
		jmp 0x0000:0x7E19

os_cursor_move_right:
	pusha
	call os_cursor_get_pos
	inc dl
	call os_cursor_move_pos
	popa
	ret

os_cursor_move_down:
	pusha
	call os_cursor_get_pos
	inc dh
	call os_cursor_move_pos
	popa
	ret

os_cursor_move_start:
	pusha
	call os_cursor_get_pos
	mov dl, 00h
	call os_cursor_move_pos
	popa
	ret

os_cursor_get_pos:
	pusha
	mov ah, 03h
	mov bh, 00h
	int 10h
	mov [.tmp], dx
	popa
	mov dx, [.tmp]
	ret
	.tmp dw 0

os_cursor_move_pos:
	pusha
	mov ah, 02h
	mov bh, 00h
	int 10h
	popa
	ret

os_string_out:
	pusha
	mov ah, 09h
	mov bh, 00h
	mov cx, 01h
	.continue:
		lodsb
		cmp al, 00h
		je .done
		cmp al, 0Dh
		je .enter
		cmp al, 20h
		jl .continue
		cmp al, 7Eh
		jg .continue
		int 10h
		call os_cursor_move_right
		jmp .continue
	.enter:
		call os_newline
		jmp .continue
	.done:
		popa
		ret

os_newline:
	pusha
	call os_cursor_move_down
	call os_cursor_move_start
	popa
	ret

os_string_compare:
	pusha
	.continue:
		mov al, [si]
		mov bl, [di]
		cmp al, bl
		jne .different
		inc si
		inc di
		cmp bl, 00h
		je .same
		jmp .continue
	.different:
		popa
		clc
		ret
	.same:
		popa
		stc
		ret

os_disk_load_file_offset:
	pusha
	mov di, 8C00h
	.continue:
		cmp byte [di], 00h
		je .next
		call os_string_compare
		jc .load_file
	.next:
		add di, 100h
		cmp byte [di], 00h
		jne .continue
		popa
		clc
		ret
	.load_file:
		add di, 0ECh
		pusha
		mov si, di
		lodsw
		mov byte [.file_size], al
		popa
		add di, 04h
		pusha
		mov si, di
		lodsw
		mov word [.file_offset], ax
		popa
		popa
		mov si, .file_offset
		mov al, byte [.file_size]
		stc
		ret
	.file_offset dw 0
	.file_size db 0

os_disk_read_file:
; al -> number of sectors to read
; bx -> memory_offset
; cl -> sector_offset
	pusha
	mov byte [.sector_count], al
	mov word [.memory_offset], bx
	xor ax, ax
	mov ds, ax
	mov ah, 02h
	mov al, byte [.sector_count]
	mov ch, 00h
	mov dh, 00h
	xor bx, bx
	mov es, bx
	mov bx, word [.memory_offset]
	int 13h
	popa
	ret
	.sector_count db 0
	.memory_offset dw 0

times 512-($-$$) db 0