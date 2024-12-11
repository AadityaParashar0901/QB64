@echo off
if exist aios.img del aios.img
del bootloader.bin
c:\env\bin\nasm -f bin bootloader.asm -o bootloader.bin
c:\env\bin\nasm -f bin files\hi.asm -o files\hi
c:\env\bin\nasm -f bin files\cat.asm -o files\cat
c:\env\bin\nasm -f bin files\editor.asm -o files\editor
c:\env\bin\nasm -f bin padding.asm -o padding
copy /b bootloader.bin + files\FILE1.TXT + files\FILE2.TXT + files\FILE3.TXT + files\hi + files\cat + files\editor + padding aios.img
del bootloader.bin padding files\hi files\cat files\editor
if not exist aios.img pause
qemu-system-x86_64 aios.img