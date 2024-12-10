[BITS 16]
[ORG 0xCC00]

mov bl, 30h
mov si, msg
call os_string_out

jmp 0000h:7E0Dh

msg db 'Hello, This program prints hi.', 0

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

os_clear_screen:
	pusha
	mov dx, 00h
	call os_cursor_move_pos
	mov ah, 06h
	mov al, 00h
	mov cx, 00h
	mov dh, 18h
	mov dl, 4Fh
	int 10h
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

os_cursor_move_right:
	pusha
	call os_cursor_get_pos
	inc dl
	call os_cursor_move_pos
	popa
	ret

os_newline:
	pusha
	call os_cursor_move_down
	call os_cursor_move_start
	popa
	ret

times 512-($-$$) db 0