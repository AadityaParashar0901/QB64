$Console:Only
Dim BLOCK_SIZE_VALUE As _Unsigned Integer
If _CommandCount < 2 Or _CommandCount > 3 Then GoTo HELP
INFILE$ = Command$(2)
If _CommandCount = 3 Then BLOCK_SIZE_VALUE = Val(Command$(3)) Else BLOCK_SIZE_VALUE = 20
If _FileExists(INFILE$) = 0 Then
    If _FileExists(_StartDir$ + "\" + INFILE$) = 0 Then Print "File doesn't exists" Else INFILE$ = _StartDir$ + "\" + INFILE$
End If
If _FileExists(INFILE$) = 0 Then System
If Command$(1) = "-c" Then GoTo COMPRESS
If Command$(1) = "-d" Then GoTo DECOMPRESS
HELP:
Print "simple-8b_2"
Print "Usage:"
Print "    " + Command$(0) + " [OPTION] [FILE]"
Print "Options:"
Print "    -c Compress"
Print "    -d Decompress"
System
COMPRESS:
Open INFILE$ For Binary As #1
Open INFILE$ + ".s8b2" For Binary As #2
L~& = LOF(1)
S~& = 0
BLOCK_SIZE~& = 2 ^ BLOCK_SIZE_VALUE
Print "Compressing: " + INFILE$
Y = CsrLin
LF~& = LOF(1)
Put #2, , LF~&
Do
    If S~& + BLOCK_SIZE~& > L~& Then I$ = String$(L~& - S~&, 0) Else I$ = String$(BLOCK_SIZE~&, 0)
    S~& = S~& + Len(I$)
    Get #1, , I$
    O$ = _Deflate$(Compress$(I$))
    LO~& = Len(O$)
    Put #2, , LO~&
    Put #2, , O$
    Locate Y, 1: Print Round!(100 * S~& / L~&); "%", Round(100 * LOF(2) / S~&); "%    "
Loop While EOF(1) = 0 And S~& < L~&
System
DECOMPRESS:
Open INFILE$ For Binary As #1
Open INFILE$ + ".s8b2_out" For Binary As #2
L~& = LOF(1)
S~& = 0
Print "Compressing: " + INFILE$
Y = CsrLin
Get #1, , LF~&
Do
    Get #1, , LO~&
    O$ = String$(LO~&, 0)
    Get #1, , O$
    I$ = Decompress$(O$)
    Put #2, , I$
    Locate Y, 1: Print Round!(100 * LOF(2) / LF~&); "%"
Loop While EOF(1) = 0 And LOF(2) < LF~&
System
Function Round! (N As Single): R& = N * 100: Round! = R& / 100: End Function
Function Compress$ (A$)
    Dim As String BUFFER
    O$ = String$(Len(A$) * 2, 0)
    Const nFactor = 32
    BUFFER = String$(nFactor * 8, 0)
    Dim As _Unsigned Long I, O_OFFSET, BUFFER_OFFSET
    O_OFFSET = 1
    BUFFER_OFFSET = 1
    MAX_MBU~%% = 0
    For I = 1 To Len(A$)
        BYTE~%% = Asc(A$, I)
        MBU~%% = min_bits_used~%%(BYTE~%%)
        If MAX_MBU~%% < MBU~%% Then
            If MBU~%% = 8 Then
                'PUSH_OUT_BUFFER
                Asc(O$, O_OFFSET) = Len(BUFFER) + 8 * Sgn(Len(BUFFER) Mod 8) + MAX_MBU~%%: O_OFFSET = O_OFFSET + 1
                For J = 1 To Len(BUFFER) \ 8 + Sgn(Len(BUFFER) Mod 8)
                    Asc(O$, O_OFFSET) = Val("&B" + Mid$(BUFFER, J * 8 - 7, 8)): O_OFFSET = O_OFFSET + 1
                Next J
                BUFFER_OFFSET = 2
                Asc(BUFFER, 1) = BYTE~%% 'BUFFER = BYTE~%%
                MAX_MBU~%% = 8
            Else
                If MAX_MBU~%% * (BUFFER_OFFSET - 1) + MBU~%% > MBU~%% * (BUFFER_OFFSET) Then MAX_MBU~%% = MBU~%%
            End If
        Else
            Asc(BUFFER, BUFFER_OFFSET) = BYTE~%%: BUFFER_OFFSET = BUFFER_OFFSET + 1 'BUFFER += BYTE~%%
            If BUFFER_OFFSET > (nFactor - 1) * 8 Then
                'PUSH_OUT_BUFFER
                Asc(O$, O_OFFSET) = Len(BUFFER) + 8 * Sgn(Len(BUFFER) Mod 8) + MAX_MBU~%%: O_OFFSET = O_OFFSET + 1
                For J = 1 To Len(BUFFER) \ 8 + Sgn(Len(BUFFER) Mod 8)
                    Asc(O$, O_OFFSET) = Val("&B" + Mid$(BUFFER, J * 8 - 7, 8)): O_OFFSET = O_OFFSET + 1
                Next J
                BUFFER_OFFSET = 1
                MAX_MBU~%% = 0
            End If
        End If
    Next I
    If BUFFER_OFFSET > 1 Then
        Asc(O$, O_OFFSET) = Len(BUFFER) + 8 * Sgn(Len(BUFFER) Mod 8) + MAX_MBU~%%: O_OFFSET = O_OFFSET + 1
        For J = 1 To Len(BUFFER) \ 8 + Sgn(Len(BUFFER) Mod 8)
            Asc(O$, O_OFFSET) = Val("&B" + Mid$(BUFFER, J * 8 - 7, 8)): O_OFFSET = O_OFFSET + 1
        Next J
    End If
    Compress$ = Left$(O$, O_OFFSET - 1)
End Function
Function Decompress$ (A$)
End Function
Function min_bits_used~%% (A~%%)
    Select Case A~%%
        Case 0, 1: min_bits_used~%% = 1
        Case 2, 3: min_bits_used~%% = 2
        Case 4 To 7: min_bits_used~%% = 3
        Case 8 To 15: min_bits_used~%% = 4
        Case 16 To 31: min_bits_used~%% = 5
        Case 32 To 63: min_bits_used~%% = 6
        Case 64 To 127: min_bits_used~%% = 7
        Case 128 To 255: min_bits_used~%% = 8
    End Select
End Function
Function Byte2Bits$ (A~%%, L~%%)
    Static O$8
    Dim I As _Unsigned _Byte
    B~%% = A~%%
    While B~%%
        I = I + 1
        Asc(O$8, I) = 48 + (B~%% And 1)
        B~%% = _SHR(B~%%, 1)
    Wend
    Byte2Bits$ = Right$(O$8, L~%%)
End Function
