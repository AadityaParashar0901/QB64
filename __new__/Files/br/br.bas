$Console:Only
If _CommandCount = 0 Then System
If Command$(1) = "-c" Then MODE = 1
If Command$(1) = "-d" Then MODE = 2
INFILE$ = Command$(2)
If _FileExists(INFILE$) = 0 Then PATHPREFIX$ = _StartDir$ + "\"
INFILE$ = PATHPREFIX$ + INFILE$
If _FileExists(INFILE$) = 0 Then Print "File "; Command$(2); " does not exists!": System
Y = CsrLin + 1
If MODE = 1 Then
    Print "Compressing to ";: Open INFILE$ For Binary As #1
    OUTFILE$ = INFILE$ + ".br"
    Print OUTFILE$
    Open OUTFILE$ For Output As #2
    Close #2
    Open OUTFILE$ For Binary As #2
    ST! = Timer(0.001)
    BS = 2 ^ Val(Command$(3))
    If BS = 1 Then BS = 2 ^ 20
    Do
        LT! = Timer(0.001)
        If LOF(1) - Seek(1) + 1 >= BS Then I$ = Space$(BS) Else I$ = Space$(LOF(1) - Seek(1) + 1)
        Get #1, , I$
        O$ = Compress$(I$)
        T$ = MKL$(Len(O$)) + O$
        Put #2, , T$
        If LOF(1) <= Seek(1) - 1 Then Exit Do
        Locate Y, 1: Print Round(100 * (Seek(1) - 1) / LOF(1)); "%", Round(100 * LOF(2) / (Seek(1) - 1)); "%", Round(Timer(0.001) - LT!); "s", Round((Timer(0.001) - LT!) / BS * (LOF(1) - Seek(1) + 1)); "s", Round(Timer(0.001) - ST!); "s"
    Loop
    Print "Ratio:"; Round(100 * LOF(2) / LOF(1)); "%"
    Print "Time: "; Timer(0.001) - ST!; "s"
    Close
ElseIf MODE = 2 Then
    Print "Decompressing to ";: Open INFILE$ For Binary As #1
    OUTFILE$ = INFILE$ + ".out"
    Open OUTFILE$ For Output As #2
    Close #2
    Open OUTFILE$ For Binary As #2
    ST! = Timer(0.001)
    Do
        LT! = Timer(0.001)
        Get #1, , L&
        If EOF(1) = -1 Then Exit Do
        I$ = Space$(L&)
        Get #1, , I$
        O$ = DeCompress$(I$)
        Put #2, , O$
        Locate Y, 1: Print Round(100 * (Seek(1) - 1) / LOF(1)); "%", Round(Timer(0.001) - LT!); "s", Round(Timer(0.001) - ST!); "s"
    Loop
    Print "Time: "; Timer(0.001) - ST!; "s"
    Close
End If
System
Function Compress$ (__I$)
    Dim As _Unsigned _Byte __BYTE, __LB
    Dim As _Unsigned Long __I, __J, __OFFSET, __LENA
    __LENA = Len(__I$)
    __O$ = __I$
    For __J = 1 To 16
        For __I = 1 To __LENA
            __BYTE = Asc(__O$, __I)
            Asc(__O$, __I) = __BYTE - __LB
            __LB = __BYTE
        Next __I
    Next __J
    Compress$ = __O$
End Function
Function DeCompress$ (__I$)

End Function
Function ByteToBits$ (__BYTE As _Unsigned _Byte, __MAX_LEN As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    Dim __O$8
    __O$8 = String$(8, 48)
    For __I = 1 To __MAX_LEN
        Asc(__O$8, 9 - __I) = 48 - _ReadBit(__BYTE, __I - 1)
    Next __I
    ByteToBits$ = Right$(__O$8, __MAX_LEN)
End Function
Function min_bits_used~%% (A As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    For __I = 7 To 0 Step -1
        If A And 2 ^ __I Then min_bits_used = __I + 1: Exit Function
    Next __I
    min_bits_used = 0
End Function
Function Round (N As Single)
    Round = ((N * 100) \ 1) / 100
End Function
