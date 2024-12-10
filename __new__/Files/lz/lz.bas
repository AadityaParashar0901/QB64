$Console:Only
'11.6 KB in 3 sec
Const WINDOWSIZE = 256
Const MAXLENGTH = 255
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
    OUTFILE$ = INFILE$ + ".lz"
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
        If Command$(4) = "n" Then O$ = Compress$(I$) Else O$ = _Deflate$(Compress$(I$))
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
        If Command$(4) = "n" Then O$ = DeCompress$(I$) Else O$ = DeCompress$(_Inflate$(I$))
        Put #2, , O$
        Locate Y, 1: Print Round(100 * (Seek(1) - 1) / LOF(1)); "%", Round(Timer(0.001) - LT!); "s", Round(Timer(0.001) - ST!); "s"
    Loop
    Print "Time: "; Timer(0.001) - ST!; "s"
    Close
End If
System
Function Compress$ (A$)
    Dim As _Unsigned Long I, J, __BYTE_BUFFER_OFFSET, __MATCH_BUFFER_OFFSET
    Dim As String __BYTE_BUFFER, __MATCH_BUFFER
    Dim As _Unsigned Integer P
    Dim As _Unsigned _Bit * 3 __MATCH_BUFFER_BIT_OFFSET
    __BYTE_BUFFER = String$(Len(A$) * 2, 0)
    __MATCH_BUFFER = String$(Len(A$) \ 2, 0)
    For I = 1 To Len(A$)
        For J = MAXLENGTH + 5 To 5 Step -1
            If InStr(I - WINDOWSIZE, A$, Chr$(Asc(A$, I))) = 0 Then J = 4: Exit For
            If 1 Then
                P = I - InStr(I - WINDOWSIZE, A$, Mid$(A$, I, J))
            Else
                P = I - INSTRING(I - WINDOWSIZE, A$, I, J)
            End If
            If P > 0 And P < WINDOWSIZE + 1 Then
                Exit For
            End If
        Next J
        If __MATCH_BUFFER_BIT_OFFSET = 0 Then __MATCH_BUFFER_OFFSET = __MATCH_BUFFER_OFFSET + 1
        If J <= 4 Then 'BYTE
            'Asc(__MATCH_BUFFER, __MATCH_BUFFER_OFFSET) = _ResetBit(Asc(__MATCH_BUFFER, __MATCH_BUFFER_OFFSET), __MATCH_BUFFER_BIT_OFFSET)
            __MATCH_BUFFER_BIT_OFFSET = __MATCH_BUFFER_BIT_OFFSET + 1
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = Asc(A$, I)
        Else 'MATCH
            Asc(__MATCH_BUFFER, __MATCH_BUFFER_OFFSET) = _SetBit(Asc(__MATCH_BUFFER, __MATCH_BUFFER_OFFSET), __MATCH_BUFFER_BIT_OFFSET)
            __MATCH_BUFFER_BIT_OFFSET = __MATCH_BUFFER_BIT_OFFSET + 1
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = (J - 5) \ 256
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = (J - 5) Mod 256
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = (P - 1) \ 256
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = (P - 1) Mod 256
            I = I + J - 1
        End If
    Next I
    If __MATCH_BUFFER_BIT_OFFSET > 0 Then __MATCH_BUFFER_OFFSET = __MATCH_BUFFER_OFFSET + 1
    Compress$ = MKL$(__BYTE_BUFFER_OFFSET) + MKL$(__MATCH_BUFFER_OFFSET) + Left$(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) + Left$(__MATCH_BUFFER, __MATCH_BUFFER_OFFSET)
    __BYTE_BUFFER = ""
    __MATCH_BUFFER = ""
End Function
Function DeCompress$ (A$)

End Function
Function INSTRING~&& (__START_POSITION As Long, __STRING As String, __FSTRING_START_POSITION As _Unsigned Long, __FSTRING_LENGTH As _Unsigned Long)
    Dim As _Unsigned Long I, J, F
    Dim As _Unsigned Long __START, __END, __STRING_LENGTH
    Dim As String * 1 __C
    __STRING_LENGTH = Len(__STRING)
    If __START_POSITION < 1 Then __START = 1 Else __START = __START_POSITION
    __END = __FSTRING_START_POSITION - 1
    __A~%% = Asc(__STRING, __FSTRING_START_POSITION)
    __C = Chr$(__A~%%)
    For I = __START To __END
        If Asc(__STRING, I) <> __A~%% Then _Continue
        J = 0
        Do
            J = J + 1
            If __END + J > __STRING_LENGTH Or J > __FSTRING_LENGTH Then F = I: Exit Do
            If Asc(__STRING, I + J - 1) <> Asc(__STRING, __END + J) Then Exit Do
        Loop
        If F Then Exit For
        I = InStr(I + 1, __STRING, __C) - 1
    Next I
    If F = 0 Then F = __FSTRING_START_POSITION
    INSTRING = F
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
