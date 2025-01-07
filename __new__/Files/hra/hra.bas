$Console:Only
'Hash Repeating Algorithm
Const HASH_TABLE_SIZE = 256
Dim Shared HASH_TABLE_LENGTH(1 To HASH_TABLE_SIZE) As _Unsigned Long
Dim Shared HASH_TABLE_STRING(1 To HASH_TABLE_SIZE) As String
Dim Shared HASH_TABLE_STATUS As String * HASH_TABLE_SIZE
Dim Shared HASH_TABLE_HASHES As String * HASH_TABLE_SIZE

Dim Shared As _Unsigned _Byte COMPRESSED, UNCOMPRESSED
Dim Shared As _Unsigned Long L_U_B
COMPRESSED = 1
UNCOMPRESSED = 0
Dim As String UNCOMPRESSED_BUFFER

INFILE$ = Command$(1)
If _FileExists(INFILE$) = 0 Then System
Open INFILE$ For Binary As #1
Open INFILE$ + ".hra" For Binary As #2
A$ = String$(LOF(1), 0)
Get #1, , A$
Dim As _Unsigned Long I
Dim As _Unsigned _Byte L
For I = 1 To Len(A$)
    For L = 256 To 2 Step -1
        T$ = Mid$(A$, I, L)
        FH~% = Find_Hash_Table(T$)
        If FH~% Then
            L_U_B = Len(UNCOMPRESSED_BUFFER)
            If L_U_B Then
                Put #2, , UNCOMPRESSED
                Put #2, , L_U_B
                Put #2, , UNCOMPRESSED_BUFFER
                UNCOMPRESSED_BUFFER = ""
            End If
            Put #2, , COMPRESSED
            Put #2, , FH~%
            I = I + L
            Exit For
        Else
            UNCOMPRESSED_BUFFER = UNCOMPRESSED_BUFFER + Chr$(Asc(A$, I))
            If L > 128 Then R = Add_Hash_Table~%%(T$)
        End If
    Next L
Next I
Print 100 * LOF(2) / LOF(1)
System
Function Hash~%% (A$)
    Dim As _Unsigned Long I
    For I = 1 To Len(A$)
        H~%% = H~%% + Asc(A$, I)
    Next I
    Hash~%% = H~%%
End Function
Function Add_Hash_Table~%% (A$)
    If Find_Hash_Table(A$) Then Exit Function
    H~%% = Hash~%%(A$)
    H2~%% = InStr(HASH_TABLE_STATUS, Chr$(0))
    If H2~%% = 0 Then Exit Function
    Asc(HASH_TABLE_HASHES, H2~%%) = H~%%
    HASH_TABLE_STRING(H2~%%) = A$
    Asc(HASH_TABLE_STATUS, H2~%%) = 1
    HASH_TABLE_LENGTH(H2~%%) = Len(A$)
    Add_Hash_Table = -1
End Function
Function Find_Hash_Table~%% (A$)
    Dim As _Unsigned Long L
    H$1 = Chr$(Hash~%%(A$))
    L = Len(A$)
    I = InStr(HASH_TABLE_HASHES, H$1)
    Do While I
        I = InStr(I, HASH_TABLE_HASHES, H$1)
        If Asc(HASH_TABLE_STATUS, I) = 0 Then _Continue
        If HASH_TABLE_LENGTH(I) <> L Then _Continue
        If HASH_TABLE_STRING(I) = A$ Then Find_Hash_Table~%% = I
    Loop
    Find_Hash_Table = 0
End Function
Function Delete_Hash_Table~%% (H~%%)
    If H~%% = 0 Then Exit Function
End Function
