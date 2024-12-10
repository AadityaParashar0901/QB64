$Console:Only
If _CommandCount <> 3 Then Print "Error": System
If Command$(1) = "-e" Then MODE = 1
If Command$(1) = "-d" Then MODE = 2
If MODE = 0 Then Print "Error": System
Open Command$(2) For Binary As #1
Dim Shared PWD_CRC~&
PWD_CRC~& = CRC32(Command$(3))
A$ = String$(LOF(1), 0)
Get #1, , A$
Close #1
If Len(A$) Mod 16 Then A$ = A$ + String$(16 - (Len(A$) Mod 16), 0)
Select Case MODE
    Case 1:
        For I& = 1 To Len(A$) Step 16
            B$ = Mid$(A$, I&, 16)
            B$ = ENCRYPT$(B$)
            Mid$(A$, I&, 16) = B$
        Next I&
        Open Command$(2) + ".encrypted" For Binary As #1
        Put #1, , A$
    Case 2:
        For I& = 1 To Len(A$) Step 16
            B$ = Mid$(A$, I&, 16)
            B$ = DECRYPT$(B$)
            Mid$(A$, I&, 16) = B$
        Next I&
        Open Command$(2) + ".decrypted" For Binary As #1
        Put #1, , A$
End Select
A$ = ""
Close
System
Function CRC32~& (__IN$)
    Dim As _Unsigned Long __CRC32_POLY, __CRC
    __CRC32_POLY = &HEDB88320
    __CRC = &HFFFFFFFF
    For __I& = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(Mid$(__IN$, __I&, 1))
        For __J = 1 To 8
            If __CRC And 1 Then
                __CRC = (__CRC \ 2) Xor __CRC32_POLY
            Else
                __CRC = __CRC \ 2
            End If
        Next __J
    Next __I&
    CRC32~& = Not __CRC
End Function
Function ENCRYPT$ (__IN$)
    Dim BYTE~%%, __I&, __I$16, __J$16
    __I$16 = __IN$
    For __I& = 1 To 16: BYTE~%% = Asc(__I$16, __I&): BYTE~%% = BYTE~%% + 64: Asc(__J$16, __I&) = BYTE~%%: Next __I& 'Substitute
    __J$16 = Right$(__J$16, 4) + Left$(__J$16, 12) 'Shift Rows
    __J$16 = Mid$(__J$16, 4, 1) + Mid$(__J$16, 1, 3) + Mid$(__J$16, 8, 1) + Mid$(__J$16, 5, 3) + Mid$(__J$16, 12, 1) + Mid$(__J$16, 9, 3) + Mid$(__J$16, 16, 1) + Mid$(__J$16, 13, 3) 'Shift Columns
    For __I& = 1 To 4: Mid$(__J$16, __I& * 4 - 3, 4) = MKL$((2147483648 + CVL(Mid$(__J$16, __I& * 4 - 3))) Xor PWD_CRC~&): Next __I& 'Xor Key and Reverse
    ENCRYPT$ = __J$16
End Function
Function DECRYPT$ (__IN$)
    Dim BYTE~%%, __I&, __I$16, __J$16
    __I$16 = __IN$
    For __I& = 1 To 4: Mid$(__J$16, __I& * 4 - 3, 4) = MKL$((2147483648 + CVL(Mid$(__I$16, __I& * 4 - 3))) Xor PWD_CRC~&): Next __I& 'Reverse and Xor Key
    __J$16 = Mid$(__J$16, 2, 3) + Mid$(__J$16, 1, 1) + Mid$(__J$16, 6, 3) + Mid$(__J$16, 5, 1) + Mid$(__J$16, 10, 3) + Mid$(__J$16, 9, 1) + Mid$(__J$16, 14, 3) + Mid$(__J$16, 13, 1) 'Shift Columns
    __J$16 = Right$(__J$16, 12) + Left$(__J$16, 4) 'Shift Rows
    For __I& = 1 To 16: BYTE~%% = Asc(__J$16, __I&): BYTE~%% = BYTE~%% - 64: Asc(__J$16, __I&) = BYTE~%%: Next __I& 'Substitute
    DECRYPT$ = __J$16
End Function
