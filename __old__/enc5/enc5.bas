$Console:Only
DefLng A-Z
Dim P$64, FILESIZE$8, D$64, O$64, TD$
If _CommandCount = 0 Then System
Open Command$(1) For Binary As #1
Open Command$(1) + ".enc5" For Binary As #2
Open Command$(1) + ".out" For Binary As #3
P$64 = SHA256$64(Command$(2))
PARTS = LOF(1) \ 64 + Sgn(LOF(1) Mod 64)
FILESIZE$8 = _MK$(_Unsigned _Integer64, LOF(1))
Put #2, , FILESIZE$8
For K = 1 To PARTS
    Get #1, , D$64
    O$64 = Encrypt$64(D$64, P$64)
    Put #2, , O$64
    D$64 = Decrypt$64(O$64, P$64)
    If K = PARTS And Sgn(LOF(1) Mod 64) Then
        TD$ = Left$(D$64, LOF(1) Mod 64)
        Put #3, , TD$
    Else
        Put #3, , D$64
    End If
Next K
System
Function Encrypt$64 (__I$64, __P$64)
    Static As Long I, II, IJ, JI, JJ, J
    Static DataOut$64
    Static As _Unsigned _Byte DataMatrix(1 To 8, 1 To 8), PasswordMatrix(1 To 8, 1 To 8), HashMatrix(1 To 8, 1 To 8), ConstantMatrix(1 To 8, 1 To 8), OutMatrix(1 To 8, 1 To 8)
    MemCopy _Offset(__I$64), _Offset(DataMatrix()), 64
    MemCopy _Offset(__P$64), _Offset(PasswordMatrix()), 64
    'Shuffle Data Matrix
    For I = 0 To 63
        II = 1 + I \ 8: IJ = 1 + (I Mod 8)
        JI = 1 + (63 - I) \ 8: JJ = 1 + ((63 - I) Mod 8)
        Swap DataMatrix(II, IJ), DataMatrix(JI, JJ)
    Next I
    'Generate Constant, Hash Matrices & Encrypt
    For I = 1 To 8: For J = 1 To 8
            ConstantMatrix(I, J) = Hash~%%(PasswordMatrix(I, J) + 127)
            HashMatrix(I, J) = Hash~%%(ConstantMatrix(I, J) Xor PasswordMatrix(I, J)) + 127
            OutMatrix(I, J) = (DataMatrix(I, J) Xor HashMatrix(I, J)) + ConstantMatrix(I, J)
    Next J, I
    MemCopy _Offset(OutMatrix()), _Offset(DataOut$64), 64
    Encrypt$64 = DataOut$64
End Function
Function Decrypt$64 (__I$64, __P$64)
    Static As Long I, II, IJ, JI, JJ, J
    Static DataOut$64
    Static As _Unsigned _Byte DataMatrix(1 To 8, 1 To 8), PasswordMatrix(1 To 8, 1 To 8), HashMatrix(1 To 8, 1 To 8), ConstantMatrix(1 To 8, 1 To 8), OutMatrix(1 To 8, 1 To 8)
    MemCopy _Offset(__I$64), _Offset(OutMatrix()), 64
    MemCopy _Offset(__P$64), _Offset(PasswordMatrix()), 64
    'Generate Constant, Hash Matrices & Decrypt
    For I = 1 To 8: For J = 1 To 8
            ConstantMatrix(I, J) = Hash~%%(PasswordMatrix(I, J) + 127)
            HashMatrix(I, J) = Hash~%%(ConstantMatrix(I, J) Xor PasswordMatrix(I, J)) + 127
            DataMatrix(I, J) = (OutMatrix(I, J) - ConstantMatrix(I, J)) Xor HashMatrix(I, J)
    Next J, I
    'UnShuffle Data Matrix
    For I = 0 To 63
        II = 1 + I \ 8: IJ = 1 + (I Mod 8)
        JI = 1 + (63 - I) \ 8: JJ = 1 + ((63 - I) Mod 8)
        Swap DataMatrix(II, IJ), DataMatrix(JI, JJ)
    Next I
    MemCopy _Offset(DataMatrix()), _Offset(DataOut$64), 64
    Decrypt$64 = DataOut$64
End Function
Function Hash~%% (X As _Unsigned _Integer64)
    Hash~%% = _SHL(X And 15, 4) Or _SHR(X And 240, 4)
End Function
Sub MemCopy (__S As _Offset, __D As _Offset, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _Mem(__S, __SIZE): __M2 = _Mem(__D, __SIZE)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
'$Include:'files\hash.bm'
