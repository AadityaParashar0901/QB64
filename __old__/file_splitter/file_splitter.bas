$Console:Only
DefLng A-Z
If _CommandCount = 0 Then System
Dim BLOCK As String
Dim BLOCKSIZE As _Unsigned _Integer64
BLOCKSIZE = 1048576
INFILE$ = Command$(1)
If _FileExists(INFILE$) = 0 Then
    If _FileExists(_StartDir$ + "\" + Command$(1)) = 0 Then
        Print "File doesn't exists"
    Else
        INFILE$ = _StartDir$ + "\" + Command$(1)
    End If
End If
If _CommandCount = 1 And Len(INFILE$) - InStr(INFILE$, ".p") <= 6 Then GoTo JOIN
SPLIT:
If _CommandCount = 2 Then
    If Right$(Command$(2), 1) = "K" Then BLOCKSIZE = Val(Command$(2)) * 1024
    If Right$(Command$(2), 1) = "M" Then BLOCKSIZE = Val(Command$(2)) * 1048576
    If Right$(Command$(2), 1) = "G" Then BLOCKSIZE = Val(Command$(2)) * 1073741824
End If
BLOCK = String$(BLOCKSIZE, 0)
Open INFILE$ For Binary As #1
PARTS = Int(LOF(1) / BLOCKSIZE) + Sgn(LOF(1) Mod BLOCKSIZE)
OUTFILE$ = INFILE$ + ".p"
Y = CsrLin
For I = 1 To PARTS
    Locate Y, 1
    Print "Part"; I
    Print "[" + String$(I * 100 / PARTS, "=") + String$(100 - I * 100 / PARTS, " ") + "]"
    Open OUTFILE$ + NUM$(I, PARTS) For Output As #2
    Close #2
    Open OUTFILE$ + NUM$(I, PARTS) For Binary As #2
    If I = PARTS Then
        BLOCK = ""
        BLOCK = String$(LOF(1) - Seek(1) + 1, 0)
    End If
    Get #1, , BLOCK
    Put #2, , BLOCK
    Close #2
Next I
System
JOIN:
PARTS = 10 ^ (Len(INFILE$) - _InStrRev(INFILE$, ".p") - 1) - 1
OUTFILE$ = Left$(INFILE$, _InStrRev(INFILE$, ".p") - 1)
Open OUTFILE$ For Binary As #2
Y = CsrLin
For I = 1 To PARTS
    Locate Y, 1
    If _FileExists(OUTFILE$ + ".p" + NUM$(I, PARTS)) = 0 Then Exit For
    Print "Join"; I
    'Print "[" + String$(I * 100 / PARTS, "=") + String$(100 - I * 100 / PARTS, " ") + "]"
    Open OUTFILE$ + ".p" + NUM$(I, PARTS) For Binary As #1
    BLOCKSIZE = LOF(1)
    BLOCK = ""
    BLOCK = String$(BLOCKSIZE, 0)
    Get #1, , BLOCK
    Put #2, , BLOCK
    Close #1
Next I
System
Function NUM$ (X, P)
    N$ = _Trim$(Str$(X))
    NUM$ = String$(Len(_Trim$(Str$(P))) - Len(N$), "0") + N$
End Function
