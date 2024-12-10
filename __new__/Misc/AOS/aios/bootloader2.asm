start:
mov byte [BOOT_DRIVE], dl
mov bh, 30h
mov bl, 30h
call os_clear_screen
mov si, .boot_start_msg
call os_string_out
jmp cli_continue
.boot_start_msg db 'AIOS', 0

BOOT_DRIVE db 0

cli_continue:
	call os_newline
	mov si, cli_continue_msg
	call os_string_out
	call os_string_in
	call os_newline
	mov di, os_string_in_mem
	call os_string_uppercase

cli_compare_commands:
	mov si, cli_command_shutdown
	call os_string_compare
	jc cli_shutdown

	mov si, cli_command_restart
	call os_string_compare
	jc cli_restart

	mov si, cli_command_list
	call os_string_compare
	jc cli_list_files

	mov si, cli_command_clear
	call os_string_compare
	jc cli_clear_screen

	mov si, cli_command_help
	call os_string_compare
	jc cli_show_help

	mov si, cli_command_write
	call os_string_compare
	jc cli_write

cli_check_files:

	mov si, di
	mov al, 20h
	call os_char_find
	jc .command_attributes

	.only_command:
		mov si, di
		mov di, .command_name
		call os_string_copy
		jmp .continue

	.command_attributes:
		mov cx, si
		sub cx, di
		mov si, os_string_in_mem
		mov di, .command_name
		call os_string_copy_len

	.continue:
		mov si, .command_name
		call os_disk_load_file_offset
		jnc .command_not_found
		mov bx, 0xCC00
		mov cl, byte [si]
		call os_disk_read_file
		mov bx, 3030h
		jmp 0x0000:0xCC00

	.command_not_found:
		mov si, .command_not_found_msg
		call os_string_out
		jmp cli_continue

.command_name times 16 db 0
.command_not_found_msg db 'Command not found!', 0

jmp cli_continue

cli_shutdown:
	mov si, .shutdown_msg
	call os_string_out
	call os_boot_shutdown
	jmp cli_continue
.shutdown_msg db 'Shutting Down...', 0

cli_restart:
	mov si, .restart_msg
	call os_string_out
	call os_boot_restart
	jmp cli_continue
.restart_msg db 'Restarting...', 0

cli_list_files:
	mov di, kernel_memory
	call os_disk_list_files
	mov si, kernel_memory
	call os_string_out
	jmp cli_continue

cli_clear_screen:
	call os_clear_screen
	jmp cli_continue

cli_show_help:
	mov si, .help_msg
	call os_string_out
	jmp cli_continue

.help_msg db 'Commands:', 13, 'SHUTDOWN, RESTART, LS, CLEAR, WRITE, HELP', 0

cli_write:
	pusha
	mov si, .file_name
	mov ax, 01h
	mov dx, .file_contents
	call os_disk_write_file
	popa
	jmp cli_continue

.file_name db 'NEWFILE', 0
.file_contents db 'I am the new file', 0

hlt

cli_command_shutdown db 'SHUTDOWN', 0
cli_command_restart db 'RESTART', 0
cli_command_list db 'LS', 0
cli_command_clear db 'CLEAR', 0
cli_command_help db 'HELP', 0
cli_command_write db 'WRITE', 0
cli_continue_msg db '>>', 0

%include 'kernel_features.asm'

kernel_memory: