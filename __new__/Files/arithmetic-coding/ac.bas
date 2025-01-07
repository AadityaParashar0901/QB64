$Console:Only
Dim As _Unsigned Long BLOCKSIZE, I
If _CommandCount = 0 Then System
INFILE$ = Command$(2)
If _FileExists(INFILE$) = 0 Then INFILE$ = _StartDir$ + "\" + INFILE$
If _FileExists(INFILE$) = 0 Then System
OUTFILE$ = INFILE$
Do
    I = I + 1
    If _FileExists(OUTFILE$ + ".ac") Then OUTFILE$ = INFILE$ + " (" + _Trim$(Str$(I)) + ")" Else Exit Do
Loop
OUTFILE$ = OUTFILE$ + ".ac"
If _StriCmp(Command$(1), "-c") = 0 Then MODE = 1
If _StriCmp(Command$(1), "-d") = 0 Then MODE = 2
ST! = Timer(0.001)
Y = CsrLin
Select Case MODE
    Case 1
        Open INFILE$ For Binary As #1
        Open OUTFILE$ For Binary As #2
        BLOCKSIZE = 2 ^ Val(Command$(3))
        If BLOCKSIZE = 1 Then BLOCKSIZE = 2 ^ 3
        Do While LOF(1)
            If Seek(1) + BLOCKSIZE - 1 < LOF(1) Then D$ = String$(BLOCKSIZE, 0) Else D$ = String$(LOF(1) - Seek(1) + 1, 0)
            Get #1, , D$
            C$ = Compress$(_Deflate$(D$))
            L& = Len(C$)
            Put #2, , L&
            Put #2, , C$
            Locate Y, 1: Print "Progress:"; 100 * (Seek(1) - 1) / LOF(1); "%, Ratio:"; 100 * LOF(2) / (Seek(1) - 1)
            If Seek(1) - 1 = LOF(1) Then Exit Do
        Loop
    Case 2
        Open INFILE$ For Binary As #1
        Open OUTFILE$ + ".out" For Binary As #2
        Do While LOF(1)
            Get #1, , L&
            If EOF(1) Then Exit Do
            C$ = String$(L&, 0)
            Get #1, , C$
            D$ = _Inflate$(Decompress$(C$))
            Put #2, , D$
            Locate Y, 1: Print "Progress:"; 100 * (Seek(1) - 1) / LOF(1); "%"
        Loop
End Select
Print "Time:"; Timer(0.001) - ST!; ", Ratio:"; 100 * LOF(2) / LOF(1)
System
Function Compress$ (I$)
    Dim As _Unsigned Long I, J
    Dim As _Unsigned _Bit * 3 MIN, O(1 To 8)
    Dim As _Unsigned _Byte B, LB, D(1 to 8)
    B$ = ""
    For I = 1 To 8
        For J = 1 To 8
            If Asc(I$, 1 + MIN) > Asc(I$, J) Then MIN = J - 1
        Next J
        O(I) = MIN
        D(I) = Asc(I$, 1 + MIN) - LB
        If min_bits_used(D(I)) > MBU Then MBU = min_bits_used(D(I))
        LB = Asc(I$, 1 + MIN)
    Next I
    B$ = ByteToBits$(MBU, 3)
    For I = 1 To 8
        B$ = B$ + ByteToBits$(D(I), MBU)
    Next I
    For I = 1 To 8
        B$ = B$ + ByteToBits$(O(I), 3)
    Next I
    L = Len(B$) \ 8 + Sgn(Len(B$) Mod 8)
    B$ = B$ + "00000000"
    For I = 1 To L
        O$ = O$ + Chr$(Val("&B" + Mid$(B$, I * 8 - 7, 8)))
    Next I
    Compress$ = O$
End Function
Function Decompress$ (I$)

End Function
Function ByteToBits$ (__BYTE As _Unsigned _Byte, __MAX_LEN As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    Dim __O$
    __O$ = String$(__MAX_LEN, 48)
    For __I = 0 To __MAX_LEN - 1
        If __BYTE And 2 ^ __I Then Asc(__O$, __MAX_LEN - __I) = 49
    Next __I
    ByteToBits$ = __O$
End Function
Function min_bits_used~%% (A As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    For __I = 7 To 0 Step -1
        If A And 2 ^ __I Then min_bits_used = __I + 1: Exit Function
    Next __I
    min_bits_used = 0
End Function
