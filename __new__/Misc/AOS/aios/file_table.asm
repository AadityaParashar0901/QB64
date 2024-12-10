db 'FILE1.TXT'
times 0xEC-9 db 0
dd 1
db 41, 0, 0, 0
times 12 db 0

db 'FILE2.TXT'
times 0xEC-9 db 0
dd 1
db 42, 0, 0, 0
times 12 db 0

db 'FILE3.TXT'
times 0xEC-9 db 0
dd 1
db 43, 0, 0, 0
times 12 db 0

db 'HI'
times 0xEC-2 db 0
dd 1
db 44, 0, 0, 0
times 12 db 0

db 'CAT'
times 0xEC-3 db 0
dd 1
db 45, 0, 0, 0
times 12 db 0

db 'EDITOR'
times 0xEC-6 db 0
dd 1
db 46, 0, 0, 0
times 12 db 0