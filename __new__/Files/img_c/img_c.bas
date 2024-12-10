$Console:Only
If _CommandCount = 0 Then System
Dim As _Unsigned Long BS
Dim ST!, LT!, I$, O$, Y%, INFILE$, OUTFILE$, PATHPREFIX$, MODE%%, L&
If Command$(1) = "-c" Or Command$(1) = "--compress" Then MODE%% = 1
If Command$(1) = "-d" Or Command$(1) = "--decompress" Then MODE%% = 2
INFILE$ = Command$(2)
If _FileExists(INFILE$) = 0 Then PATHPREFIX$ = _StartDir$ + "\"
INFILE$ = PATHPREFIX$ + INFILE$
If _FileExists(INFILE$) = 0 Then Print "File "; Command$(2); " does not exists!": System
Y% = CsrLin + 1
Select Case MODE%%
    Case 1: Print "Compressing": IMG& = _LoadImage(INFILE$, 32)
        Open INFILE$ For Binary As #1
        OUTFILE$ = INFILE$ + ".img_c"
        Open OUTFILE$ For Output As #2
        Close #2
        Open OUTFILE$ For Binary As #2
        ST! = Timer(0.001)
        O$ = Compress$(IMG&)
        Put #2, , O$
        Print "Ratio:"; Round(100 * LOF(2) / LOF(1)); "%"
        Print "Time: "; Timer(0.001) - ST!; "s"
        Close
    Case 2: Print "Decompressing": Open INFILE$ For Binary As #1
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
            Locate Y%, 1: Print Round(100 * (Seek(1) - 1) / LOF(1)); "%", Round(Timer(0.001) - LT!); "s", Round(Timer(0.001) - ST!); "s"
        Loop
        Print "Time: "; Timer(0.001) - ST!; "s"
        Close
End Select
System
Function Compress$ (__I&)
    Dim As _Unsigned _Byte __BYTE
    Dim As _Unsigned Integer __W, __H
    Dim As _Unsigned Long __I, __J, __K, __Source, __Colours, __P, __L
    Dim As _Unsigned Long __Pallete(1 To 16777216)
    __Source = _Source: _Source __I&
    __W = _Width(__I&)
    __H = _Height(__I&)
    Print "Preprocessing"
    For __I = 0 To __W - 1
        For __J = 0 To __H - 1
            __P = Point(__I, __J)
            For __K = 1 To __Colours
                If __P = __Pallete(__K) Then
                    Exit For
                End If
            Next __K
            If __Colours = 0 Or __P <> __Pallete(__K) Then
                __Colours = __Colours + 1
                If __Colours > 16777216 Then Print "Error": System
                __Pallete(__Colours) = __P
            End If
    Next __J, __I
    __MBU = min_bits_used~%%(__Colours - 1)
    __B$ = String$(__W * __H * __MBU, 48)
    Print "Encoding"
    For __I = 0 To __W - 1: For __J = 0 To __H - 1
            __P = Point(__I, __J)
            For __K = 1 To __Colours
                If __P = __Pallete(__K) Then
                    Mid$(__B$, (__I * __W + __J) * __MBU + 1, __MBU) = LongToBits$(__K - 1, __MBU)
                End If
            Next __K
    Next __J, __I
    __L = __W * __H * __MBU
    __L = _SHR(__L, 3) + Sgn(__L Mod 8)
    __O$ = String$(__L, 0)
    For __I = 1 To __L
        Asc(__O$, __I) = Val("&B" + Mid$(__B$, __I * 8 - 7, 8))
    Next __I
    __Pallete$ = String$(__Colours * 4, 0)
    MemCopy _Offset(__Pallete()), _Offset(__Pallete$), __Colours * 4
    __O$ = _Deflate$(__O$)
    Compress$ = MKI$(__W) + MKI$(__H) + MKL$(__Colours) + __Pallete$ + MKL$(Len(__O$)) + __O$
    _Source __Source
End Function
Function DeCompress$ (__I$)
End Function
Sub MemCopy (__S As _Offset, __D As _Offset, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _Mem(__S, __SIZE): __M2 = _Mem(__D, __SIZE)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
Sub MemCopyFromImage (__S As Long, __D As _Offset, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _MemImage(__S): __M2 = _Mem(__D, __SIZE)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
Function ByteToBits$ (__BYTE As _Unsigned _Byte, __MAX_LEN As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    Dim __O$8
    __O$8 = String$(8, 48)
    For __I = 1 To __MAX_LEN
        Asc(__O$8, 9 - __I) = 48 - _ReadBit(__BYTE, __I - 1)
    Next __I
    ByteToBits$ = Right$(__O$8, __MAX_LEN)
End Function
Function LongToBits$ (__LONG As _Unsigned Long, __MAX_LEN As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    Dim __O$32
    __O$32 = String$(32, 48)
    For __I = 1 To __MAX_LEN
        Asc(__O$32, 33 - __I) = 48 - _ReadBit(__LONG, __I - 1)
    Next __I
    LongToBits$ = Right$(__O$32, __MAX_LEN)
End Function
Function min_bits_used~%% (A As _Unsigned Long)
    Dim __I As _Unsigned _Byte
    For __I = 31 To 0 Step -1
        If A And 2 ^ __I Then min_bits_used = __I + 1: Exit Function
    Next __I
    min_bits_used = 1
End Function
Function Remain~& (A~&, B~&)
    Remain~& = A~& \ B~& + Sgn(A~& Mod B~&)
End Function
Function Round (__N As Double)
    Round = Int(100 * __N) / 100
End Function
Function PNG32 (__FILENAME As String, __IMAGE As Long, __PNGDATA As String)
    Dim As String __PNGHEADER, __IHDR, __IDAT, __IMAGDATA, __IEND
    Dim As String __EXT
    Dim As Long __WIDTH, __HEIGHT, __F, __SOURCE, __OFFSET, __X, __Y, __P
    Dim As _Byte __SAVEMODE
    If Len(__FILENAME) Then __SAVEMODE = -1 Else __SAVEMODE = 0
    __PNGHEADER$ = Chr$(&H89) + Chr$(&H50) + Chr$(&H4E) + Chr$(&H47) + Chr$(&H0D) + Chr$(&H0A) + Chr$(&H1A) + Chr$(&H0A)
    __WIDTH = _Width(__IMAGE): __HEIGHT = _Height(__IMAGE)
    __F = FreeFile
    If LCase$(Right$(__FILENAME, 4)) <> ".png" Then __EXT = ".png" Else __EXT = ""
    If __SAVEMODE Then Open __FILENAME + __EXT For Output As #__F: Close #__F
    If __SAVEMODE Then Open __FILENAME + __EXT For Binary As #__F
    If __SAVEMODE Then Put #__F, , __PNGHEADER$
    __IHDR = "IHDR" + Reverse$(MKL$(__WIDTH)) + Reverse$(MKL$(__HEIGHT)) + Chr$(&H08) + Chr$(&H06) + String$(3, 0)
    __IHDR = Reverse$(MKL$(&H0D)) + __IHDR + Reverse$(MKL$(crc32(__IHDR)))
    If __SAVEMODE Then Put #__F, , __IHDR
    __SOURCE = _Source
    _Source __IMAGE
    __IMAGDATA = String$(__HEIGHT * __WIDTH * 4 + __HEIGHT, 0)
    __OFFSET = 1
    For __Y = 1 To __HEIGHT Step 1
        __OFFSET = __OFFSET + 1
        For __X = 1 To __WIDTH Step 1
            __P = Point(__X - 1, __Y - 1)
            Asc(__IMAGDATA, __OFFSET) = _Red32(__P)
            Asc(__IMAGDATA, __OFFSET + 1) = _Green32(__P)
            Asc(__IMAGDATA, __OFFSET + 2) = _Blue32(__P)
            Asc(__IMAGDATA, __OFFSET + 3) = _Alpha32(__P)
            __OFFSET = __OFFSET + 4
        Next __X
    Next __Y
    _Source __SOURCE
    __IDAT = _Deflate$(__IMAGDATA)
    __IDAT = Reverse$(MKL$(Len(__IDAT))) + "IDAT" + __IDAT + Reverse$(MKL$(crc32("IDAT" + __IDAT)))
    If __SAVEMODE Then Put #__F, , __IDAT
    __IEND = Reverse$(MKL$(&H00)) + "IEND" + Reverse$(MKL$(&HAE426082))
    If __SAVEMODE Then Put #__F, , __IEND
    If __SAVEMODE Then Close #__F
    __PNGDATA = __PNGHEADER$ + __IHDR + __IDAT + __IEND
    If _FileExists(__FILENAME) Then PNG32 = -1 Else PNG32 = 0
End Function
Function Reverse$ (__IN$)
    IN$ = __IN$
    L~& = Len(IN$)
    For I~& = 1 To _SHR(L~&, 1)
        TMP~%% = Asc(IN$, I~&)
        Asc(IN$, I~&) = Asc(IN$, L~& - I~& + 1)
        Asc(IN$, L~& - I~& + 1) = TMP~%%
    Next I~&
    Reverse$ = IN$
End Function
Function crc32~& (__IN$)
    Dim As _Unsigned Long __CRC32_POLY, __CRC, __I
    Dim As _Unsigned _Byte __J
    __CRC32_POLY = &HEDB88320
    __CRC = &HFFFFFFFF
    For __I = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(__IN$, __I)
        For __J = 1 To 8
            If __CRC And 1 Then __CRC = (__CRC \ 2) Xor __CRC32_POLY Else __CRC = __CRC \ 2
        Next __J
    Next __I
    crc32~& = Not __CRC
End Function
Function adler32~& (__IN$)
    Dim As _Unsigned Long __A, __B
    __A = 1: __B = 0
    For __I = 1 To Len(__IN$)
        __A = (__A + Asc(Mid$(__IN$, __I, 1))) Mod 65521
        __B = (__B + __A) Mod 65521
    Next __I
    adler32~& = __B * 65536 + __A
End Function
