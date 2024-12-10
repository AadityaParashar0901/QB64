[BITS 16]
[ORG 0xCC00]



jmp 0000h:7E19h

times 512-($-$$) db 0