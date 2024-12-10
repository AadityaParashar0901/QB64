[BITS 16]

jmp 0x0000:0x7C00

os_string_in_mem equ 0xFE00

os_boot_shutdown:
	pusha
	mov ax, 5301h
	xor bx, bx
	int 15h
	mov ax, 5307h
	mov bx, 0001h
	mov cx, 0003h
	int 15h
	popa
	clc
	ret

os_boot_restart:
	pusha
	mov ah, 00h
	int 19h
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

os_cursor_move_right:
	pusha
	call os_cursor_get_pos
	inc dl
	call os_cursor_move_pos
	popa
	ret

os_cursor_move_left:
	pusha
	call os_cursor_get_pos
	dec dl
	call os_cursor_move_pos
	popa
	ret

os_cursor_move_up:
	pusha
	call os_cursor_get_pos
	dec dh
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

os_char_out:
	pusha
	mov ah, 09h
	mov bh, 00h
	mov cx, 01h
	int 10h
	call os_cursor_move_right
	popa
	ret

os_newline:
	pusha
	call os_cursor_move_down
	call os_cursor_move_start
	popa
	ret

os_string_in:
	pusha
	mov di, os_string_in_mem
	.continue:
		call os_char_in
		cmp al, 0Dh
		je .done
		cmp al, 08h
		je .backspace
		cmp al, 4Bh
		je .left
		cmp al, 4Dh
		je .right
		cmp al, 20h
		jl .continue
		cmp al, 7Eh
		jg .continue
		stosb
		call os_char_out
		jmp .continue
	.backspace:
		cmp di, os_string_in_mem
		je .continue
		call os_cursor_move_left
		dec di
		mov al, 00h
		call os_char_out
		call os_cursor_move_left
		jmp .continue
	.left:
		cmp byte [di - 01h], 00h
		je .continue
		call os_cursor_move_left
		dec di
		jmp .continue
	.right:
		cmp byte [di + 01h], 00h
		je .continue
		call os_cursor_move_right
		inc di
		jmp .continue
	.done:
		mov al, 00h
		stosb
		popa
		ret

os_char_in:
	pusha
	mov ah, 00h
	int 16h
	mov word [.tmp], ax
	popa
	mov ax, word [.tmp]
	ret
	.tmp dw 0

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

os_string_copy:
	pusha
	.continue:
		mov al, byte [si]
		mov byte [di], al
		inc si
		inc di
		cmp al, 00h
		jne .continue
	.done:
		stosb
		popa
		ret

os_string_copy_len:
	pusha
	mov dx, 00h
	.continue:
		mov al, byte [si]
		mov byte [di], al
		inc si
		inc di
		inc dx
		cmp dx, cx
		jl .continue
	.done:
		mov al, 00h
		stosb
		popa
		ret

os_string_fill:
	pusha
	mov dx, 00h
	.loop:
		stosb
		inc dx
		cmp dx, cx
		jl .loop
	popa
	ret

os_string_length:
	pusha
	mov cx, 0000h
	.continue:
		lodsb
		cmp al, 00h
		je .done
		inc cx
		jmp .continue
	.done:
		mov word [.string_length], cx
		popa
		mov cx, word [.string_length]
		ret
		.string_length dw 0

os_string_uppercase:
	pusha
	.continue:
		cmp byte [di], 00h
		je .done
		cmp byte [di], 61h
		jb .next
		cmp byte [di], 7Ah
		ja .next
		sub byte [di], 20h
	.next:
		inc di
		jmp .continue
	.done:
		popa
		ret

os_string_lowercase:
	pusha
	.continue:
		cmp byte [di], 00h
		je .done
		cmp byte [di], 41h
		jb .next
		cmp byte [di], 5Ah
		ja .next
		add byte [di], 20h
	.next:
		inc di
		jmp .continue
	.done:
		popa
		ret

os_string_find:
	pusha
	;si - input string
	;di - string to find
	.continue:
		cmp byte[si], 00h
		je .not_found
		call os_string_compare
		jc .found
		inc si
		jmp .continue
	.found:
		mov word [.found_offset], si
		popa
		mov si, word [.found_offset]
		stc
		ret
	.not_found:
		popa
		clc
		ret
.found_offset dw 0

os_char_find:
	pusha
	;si - input string
	;al - char to find
	.continue:
		cmp byte [si], 00h
		je .not_found
		cmp byte [si], al
		je .found
		inc si
		jmp .continue
	.found:
		mov word [.found_offset], si
		popa
		mov si, word [.found_offset]
		stc
		ret
	.not_found:
		popa
		clc
		ret
.found_offset dw 0

os_disk_list_files:
	pusha
	mov si, 8C00h
	.continue:
		cmp byte [si], 00h
		je .next
		call os_string_copy
		call os_string_length
		add di, cx
		mov byte [di], 0Dh
		inc di
	.next:
		add si, 100h
		cmp byte [si], 00h
		jne .continue
	dec di
	mov al, 0Dh
	stosb
	mov al, 00h
	stosb
	popa
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

os_disk_write_file:
	;si - File Name Offset
	;ax - File Sectors
	;dx - File Memory Offset
	pusha
	mov byte [.file_sectors], al
	mov word [.file_memory_offset], dx
	mov di, 8B00h
	.continue:
		add di, 100h
		cmp byte [di], 00h
		je .done
		mov cl, 00h
		add di, 0ECh
		add cl, byte [di]
		add di, 04h
		add cl, byte [di]
		sub di, 0F0h
		jmp .continue
	.done:
		call os_string_copy
		add di, 0ECh
		mov byte [di], al
		add di, 04h
		mov byte [di], cl
	mov byte [.file_sector_offset], cl
	xor ax, ax
	mov ds, ax
	.file_table_write:
		mov ah, 03h
		mov al, 20h
		mov ch, 00h
		mov cl, 09h
		mov dh, 00h
		mov dl, byte [BOOT_DRIVE]
		xor bx, bx
		mov es, bx
		mov bx, 8C00h
		int 13h
	.file_write:
		mov ah, 03h
		mov al, byte [.file_sectors]
		mov ch, 00h
		mov cl, byte [.file_sector_offset]
		mov dh, 00h
		xor bx, bx
		mov es, bx
		mov bx, word [.file_memory_offset]
		int 13h
	popa
	ret
	.file_sectors db 0
	.file_memory_offset dw 0
	.file_sector_offset db 0

os_print_int_to_hex:
	pusha
	xor bx, bx
	mov bl, 30h
	mov byte [.input_int], al
	idiv byte [.tmp_byte]
	mov si, .hex_chars
	xor ah, ah
	add si, ax
	lodsb
	call os_char_out
	imul byte [.tmp_byte]
	sub byte [.input_int], al
	mov si, .hex_chars
	xor ah, ah
	mov al, byte [.input_int]
	add si, ax
	lodsb
	call os_char_out
	popa
	ret
.input_int db 0
.tmp_byte db 10h
.print_memory times 3 db 0
.hex_chars db "0123456789ABCDEF"