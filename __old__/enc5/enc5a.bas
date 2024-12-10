$Console:Only
DefLng A-Z
If _CommandCount = 0 Then System
ENCRYPTEDFILENAME$ = Command$(1)
If _StriCmp(Right$(ENCRYPTEDFILENAME$, 6), ".enc5a") Then ENCRYPTEDFILENAME$ = ENCRYPTEDFILENAME$ + ".enc5a"
Open ENCRYPTEDFILENAME$ For Binary As #1
Password$ = Command$(2)
If _CommandCount = 2 Or Command$(3) = "l" Then GoTo LISTFILES
If Command$(3) = "a" Then GoTo ADDFILES
If Command$(3) = "e" Then GoTo EXTRACTFILES
If Command$(3) = "t" Then GoTo TESTFILES
HELP:
Print "ENC5 [ARCHIVE NAME] [PASSWORD] [OPTION] [FILES]"
Print "OPTIONS:"
Print "a    Add"
Print "e    Extract"
Print "l    List"
Print "t    Test"
System
ADDFILES:
For I = 4 To _CommandCount
    FS = _InStrRev(Command$(I), "\"): FN$ = Command$(I)
    If FS Then FN$ = Mid$(Command$(I), FS + 1)
    Print "Adding File:"; Chr$(34); FN$; Chr$(34)
    FN$ = FN$ + String$(256 - Len(FN$), 32)
    INFILE$ = Command$(I)
    If _FileExists(INFILE$) = 0 Then If _FileExists(_StartDir$ + "\" + INFILE$) = 0 Then Print "File doesn't exists!" Else INFILE$ = _StartDir$ + "\" + INFILE$
    Open INFILE$ For Binary As #2
    LOF2 = Rem64~&(LOF(2))
    F$ = String$(LOF2, 0): Get #2, , F$
    Seek #1, LOF(1) + 1
    E$ = Chr$(&HED) + ENC$(FN$, Password$) + MKL$(crc32(F$)) + MKL$(LOF(2)) + ENC$(F$, Password$)
    Put #1, , E$
    F$ = ""
    Close #2
Next I
System
LISTFILES:
If LOF(1) = 0 Then Print "No Files": System
Do
    Get #1, , FNL$1: If FNL$1 <> Chr$(&HED) Then Exit Do
    FN$ = String$(256, 0)
    Get #1, , FN$
    FN$ = _Trim$(DEC$(FN$, Password$))
    Print FN$
    Get #1, , FCRC32$4
    Get #1, , FS$4
    Seek #1, Seek(1) + Rem64~&(UCVL(FS$4))
Loop
System
EXTRACTFILES:
If LOF(1) = 0 Then Print "No Files": System
Do
    Get #1, , FNL$1: If FNL$1 <> Chr$(&HED) Then Exit Do
    FN$ = String$(256, 0)
    Get #1, , FN$
    FN$ = _Trim$(DEC$(FN$, Password$))
    If _CommandCount = 3 Then EXTRACT = -1 Else EXTRACT = 0
    For I = 4 To _CommandCount
        If FN$ = Command$(I) Then EXTRACT = -1: Exit For
    Next I
    If EXTRACT Then Print "Extracting:"; FN$;
    Get #1, , FCRC32$4
    Get #1, , FS$4
    F$ = String$(Rem64~&(UCVL(FS$4)), 0)
    Get #1, , F$
    F$ = DEC$(F$, Password$)
    If EXTRACT Then
        If UCVL(FCRC32$4) <> crc32(F$) Then Print ": Corrupted File" Else Print ": Healthy File"
        Do
            If _FileExists(_StartDir$ + "\" + FN$) Then
                FN$ = "_" + FN$
            Else
                Exit Do
            End If
        Loop
        Open _StartDir$ + "\" + FN$ For Binary As #2
        F$ = Left$(F$, UCVL(FS$4))
        Put #2, , F$
        Close #2
    End If
Loop
System
TESTFILES:
If LOF(1) = 0 Then Print "No Files": System
Do
    Get #1, , FNL$1: If FNL$1 <> Chr$(&HED) Then Exit Do
    FN$ = String$(256, 0)
    Get #1, , FN$
    FN$ = _Trim$(DEC$(FN$, Password$))
    Print "Testing:"; FN$;
    Get #1, , FCRC32$4
    Get #1, , FS$4
    F$ = String$(Rem64~&(UCVL(FS$4)), 0)
    Get #1, , F$
    F$ = DEC$(F$, Password$)
    If UCVL(FCRC32$4) <> crc32(F$) Then Print ": Corrupted File" Else Print ": Healthy File"
Loop
System
Function Rem64~& (A~&)
    B~%% = A~& Mod 64
    Rem64~& = A~& + 64 * Sgn(B~%%) - (B~%%)
End Function
Function UCVL~& (A$4)
    UCVL~& = CVL(A$4)
End Function
Function ULNG~& (A&)
    ULNG~& = A& + 2147483648
End Function
Function ENC$ (I$, P$)
    PFC$64 = FC$64(P$)
    PARTS = _SHR(Rem64~&(Len(I$)), 6)
    O$ = I$ + String$(PARTS * 64 - Len(I$), 0)
    For I~& = 1 To PARTS
        Mid$(O$, I~& * 64 - 63, 64) = Encrypt$(Mid$(O$, I~& * 64 - 63, 64), PFC$64)
    Next I~&
    ENC$ = O$
    O$ = ""
End Function
Function DEC$ (I$, P$)
    PFC$64 = FC$64(P$)
    PARTS = Len(I$) \ 64
    O$ = I$
    For I~& = 1 To PARTS
        Mid$(O$, I~& * 64 - 63, 64) = Decrypt$(Mid$(O$, I~& * 64 - 63, 64), PFC$64)
    Next I~&
    DEC$ = O$
    O$ = ""
End Function
Function Encrypt$64 (__I$64, __P$64)
    Static As Long I, II, IJ, JI, JJ, J
    Static DataOut$64
    Static As _Unsigned _Byte DataMatrix(1 To 8, 1 To 8), PasswordMatrix(1 To 8, 1 To 8), HashMatrix(1 To 8, 1 To 8), ConstantMatrix(1 To 8, 1 To 8), OutMatrix(1 To 8, 1 To 8)
    Static FirstRun As _Bit
    MemCopy _Offset(__I$64), _Offset(DataMatrix()), 64
    MemCopy _Offset(__P$64), _Offset(PasswordMatrix()), 64
    If FirstRun = 0 Or __OLDP$64 <> __P$64 Then
        For I = 1 To 8: For J = 1 To 8
                ConstantMatrix(I, J) = Hash~%%(PasswordMatrix(I, J) + 127): HashMatrix(I, J) = Hash~%%(ConstantMatrix(I, J) Xor PasswordMatrix(I, J)) + 127
        Next J, I
        __OLDP$64 = __P$64
        FirstRun = -1
    End If
    'Shuffle Data Matrix
    For I = 0 To 63
        II = 1 + I \ 8: IJ = 1 + (I Mod 8)
        JI = 1 + (63 - I) \ 8: JJ = 1 + ((63 - I) Mod 8)
        Swap DataMatrix(II, IJ), DataMatrix(JI, JJ)
    Next I
    'Generate Constant, Hash Matrices & Encrypt
    For I = 1 To 8: For J = 1 To 8
            OutMatrix(I, J) = (DataMatrix(I, J) Xor HashMatrix(I, J)) + ConstantMatrix(I, J)
    Next J, I
    MemCopy _Offset(OutMatrix()), _Offset(DataOut$64), 64
    Encrypt$64 = DataOut$64
End Function
Function Decrypt$64 (__I$64, __P$64)
    Static As Long I, II, IJ, JI, JJ, J
    Static DataOut$64
    Static As _Unsigned _Byte DataMatrix(1 To 8, 1 To 8), PasswordMatrix(1 To 8, 1 To 8), HashMatrix(1 To 8, 1 To 8), ConstantMatrix(1 To 8, 1 To 8), OutMatrix(1 To 8, 1 To 8)
    Static FirstRun As _Bit
    MemCopy _Offset(__I$64), _Offset(OutMatrix()), 64
    MemCopy _Offset(__P$64), _Offset(PasswordMatrix()), 64
    If FirstRun = 0 Or __OLDP$64 <> __P$64 Then
        For I = 1 To 8: For J = 1 To 8
                ConstantMatrix(I, J) = Hash~%%(PasswordMatrix(I, J) + 127): HashMatrix(I, J) = Hash~%%(ConstantMatrix(I, J) Xor PasswordMatrix(I, J)) + 127
        Next J, I
        __OLDP$64 = __P$64
        FirstRun = -1
    End If
    'Generate Constant, Hash Matrices & Decrypt
    For I = 1 To 8: For J = 1 To 8
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
Function FC$64 (__I$)
    Restore HASHCONSTANTS
    For __I = 1 To 8
        Read __POLY~&
        Mid$(H$64, __I * 4 - 3, 4) = c32$8(__I$, __POLY~&)
    Next __I
    FC$64 = H$64
    Exit Function
    HASHCONSTANTS:
    Data &H6a09e667,&Hbb67ae85,&H3c6ef372,&Ha54ff53a,&H510e527f,&H9b05688c,&H1f83d9ab,&H5be0cd19
End Function
Function c32$8 (__IN$, __CRC32_POLY As _Unsigned Long)
    Dim As _Unsigned Long __CRC, __I
    Dim As _Unsigned _Byte __J
    __CRC = &HFFFFFFFF
    For __I = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(__IN$, __I)
        For __J = 1 To 8
            If __CRC And 1 Then __CRC = (__CRC \ 2) Xor __CRC32_POLY Else __CRC = __CRC \ 2
        Next __J
    Next __I
    c32$ = LongToHex$(Not __CRC)
End Function
Function BytesToBits$ (__IN$)
    Dim __I As _Unsigned Long
    Dim __O$
    __O$ = Space$(Len(__IN$) * 8)
    For __I = 1 To Len(__IN$)
        Mid$(__O$, (__I - 1) * 8 + 1, 8) = ByteToBits$(Asc(Mid$(__IN$, __I, 1)), 8)
    Next __I
    BytesToBits$ = __O$
End Function
Function ByteToBits$ (__BYTE As _Unsigned _Byte, __MAX_LEN As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    Dim __O$
    __O$ = Space$(__MAX_LEN)
    For __I = 0 To __MAX_LEN - 1
        If __BYTE And 2 ^ __I Then Mid$(__O$, __MAX_LEN - __I, 1) = "1" Else Mid$(__O$, __MAX_LEN - __I, 1) = "0"
    Next __I
    ByteToBits$ = __O$
End Function
Function Integer64ToBits$ (__INT64 As _Unsigned _Integer64, __MAX_LEN As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    Dim __O$
    __O$ = Space$(__MAX_LEN)
    For __I = 0 To __MAX_LEN - 1
        If __INT64 And 2 ^ __I Then Mid$(__O$, __MAX_LEN - __I, 1) = "1" Else Mid$(__O$, __MAX_LEN - __I, 1) = "0"
    Next __I
    Integer64ToBits$ = __O$
End Function
Function LongToHex$ (A As _Unsigned Long)
    H$ = Hex$(A)
    LongToHex$ = String$(8 - Len(H$), "0") + H$
End Function
Function RightRotate~& (A As _Unsigned Long, B As _Unsigned _Byte)
    Select Case B
        Case 2: RightRotate~& = _SHR(A, B) Or _SHL(A And 7, 32 - B)
        Case 6: RightRotate~& = _SHR(A, B) Or _SHL(A And 127, 32 - B)
        Case 7: RightRotate~& = _SHR(A, B) Or _SHL(A And 255, 32 - B)
        Case 11: RightRotate~& = _SHR(A, B) Or _SHL(A And 4095, 32 - B)
        Case 13: RightRotate~& = _SHR(A, B) Or _SHL(A And 16383, 32 - B)
        Case 17: RightRotate~& = _SHR(A, B) Or _SHL(A And 262143, 32 - B)
        Case 18: RightRotate~& = _SHR(A, B) Or _SHL(A And 524287, 32 - B)
        Case 19: RightRotate~& = _SHR(A, B) Or _SHL(A And 1048575, 32 - B)
        Case 22: RightRotate~& = _SHR(A, B) Or _SHL(A And 8388607, 32 - B)
        Case 25: RightRotate~& = _SHR(A, B) Or _SHL(A And 67108863, 32 - B)
    End Select
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
