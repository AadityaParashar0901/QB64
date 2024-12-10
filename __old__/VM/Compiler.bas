$Console:Only
'$Dynamic
Type Token: Type As String: Value As String: Pos_Start As Integer: Pos_End As Integer: End Type
Type Label: Name As String: Position As Long: End Type
Const True = -1, False = 0
Dim Shared Labels(1) As Label, nLabel As Integer: nLabel = 0
Dim Shared LN As Long
Dim A$(1 To 1024)
Const T_Command = "Command", T_String = "String", T_Number = "Number", T_Hex = "Hex", T_Label = "Label", T_LabelName = "Label_Name", T_List = "List", T_Block = "Block", T_Empty = "Empty", T_NewLine = "NewLine", T_UnderScore = "_", T_BlockStart = "{", T_BlockEnd = "}"
Const Commands = " ra, rb, rc, rd, re, rf, rg, rh, nop, db, times, add, adm, arv, amv, sub, sbm, srv, smv, mul, mum, mrv, mmv, div, dim, drv, dmv, modrr, modrm, modrv, modmv, btr, btm, itr, itm, ltr, ltm, rtr, mtm, rtm, mtr, sfz, sfc, sfe, clz, clc, cfe, hlt, icr, dcr, icm, dcm, gk, wk, smrx, smry, smmx, smmy, crr, crm, cmm, crv, cmv, cvv, jiz, jnz, jic, jnc, je, jne, jl, jle, jg, jge, jmp, call, ret, pusha, popa, pushb, popb, lct, prn, lod, sto, cls, col, ior, iow, spr, spm, gpr, gpm, sys, "
If _CommandCount = 0 Then System
Open Command$(1) For Input As #1
If _InStrRev(Command$(1), ".") Then oFILE$ = Left$(Command$(1), _InStrRev(Command$(1), ".") - 1) Else oFILE$ = Command$(1)
If _FileExists(oFILE$) Then Kill oFILE$
Open oFILE$ For Binary As #2
Position = 1
LN = 0
Print "Positions: {";
Do
    LN = LN + 1
    Line Input #1, L$
    If L$ = "" And EOF(1) Then Exit Do
    If Left$(_Trim$(L$), 1) = ";" Then _Continue
    OL$ = L$
    L$ = Lexer$(L$)
    O$ = ""
    For I = 1 To Val(GetElementFromList$(L$, 0, 2))
        A$(I) = GetElementFromList$(L$, I, 2)
    Next I
    P = 0
    Select Case A$(1)
        Case "nop":
        Case "db":
            P = -1
            For I = 2 To Val(GetElementFromList$(L$, 0, 2))
                If GetElementFromList$(L$, I, 1) = T_Hex Or GetElementFromList$(L$, I, 1) = T_Number Then P = P + 1 Else P = P + Len(GetElementFromList$(L$, I, 2))
            Next I
        Case "times": P = Val(GetElementFromList$(L$, 2, 2)) - 1
        Case "add", "sub", "mul", "div", "modrr", "rtr", "crr": P = 2
        Case "adm", "sbm", "mum", "dim", "modrm", "mtr", "crm": P = 3
        Case "arv", "srv", "mrv", "drv", "modrv", "btr", "crv": P = 2
        Case "amv", "smv", "mmv", "dmv", "modmv", "btm", "cmv": P = 3
        Case "itr": P = 3
        Case "itm": P = 4
        Case "ltr": P = 5
        Case "ltm": P = 6
        Case "mtm": P = 4
        Case "rtm": P = 3
        Case "sfz", "sfc", "sfe", "clz", "clc", "cfe", "hlt", "gk", "wk", "ret", "pusha", "popa", "pushb", "popb", "prn", "lod", "sto", "cls", "sys": P = 0
        Case "icr", "dcr", "smrx", "smry": P = 1
        Case "icm", "dcm", "jiz", "jnz", "jic", "jnc", "je", "jne", "jl", "jle", "jg", "jge", "jmp", "call", "smmx", "smmy": P = 2
        Case "cmm": P = 4
        Case "cvv": P = 2
        Case "col": P = 2
        Case "lct", "ior", "iow": P = 4
        Case "spr", "gpr": P = 5
        Case "spm", "gpm": P = 6
        Case Else:
            If GetElementFromList$(L$, 1, 1) = T_Label Then
                nLabel = nLabel + 1
                ReDim _Preserve Labels(1 To nLabel) As Label
                Labels(nLabel).Name = GetElementFromList$(L$, 2, 2)
                Labels(nLabel).Position = Position
                P = -1
            End If
    End Select
    Print A$(1); ":"; _Trim$(Str$(P + 1));
    If EOF(1) = 0 Then Print ",";
    Position = Position + P + 1
    If EOF(1) Then Exit Do
Loop
Print "}"
Print "Labels: {";
For I = 1 To nLabel
    Print Labels(I).Name; ":"; _Trim$(Str$(Labels(I).Position));
    If I < nLabel Then Print ",";
Next I
Print "}"
Close #1
Open Command$(1) For Input As #1
LN = 0
Do
    LN = LN + 1
    Line Input #1, L$
    If L$ = "" And EOF(1) Then Exit Do
    L$ = Lexer$(L$)
    Print "["; L$; "]";
    O$ = ""
    For I = 1 To Val(GetElementFromList$(L$, 0, 2))
        A$(I) = GetElementFromList$(L$, I, 2)
    Next I
    Select Case A$(1)
        Case "nop":
        Case "db":
            For I = 2 To Val(GetElementFromList$(L$, 0, 2))
                If GetElementFromList$(L$, I, 1) = T_Hex Or GetElementFromList$(L$, I, 1) = T_Number Then O$ = O$ + Value$(GetElementFromList$(L$, I, 2), 1) Else O$ = O$ + GetElementFromList$(L$, I, 2)
            Next I
        Case "times":
            If GetElementFromList$(L$, 3, 1) = T_Hex Or GetElementFromList$(L$, 3, 1) = T_Number Then O$ = String$(Val(GetElementFromList$(L$, 2, 2)), Value$(GetElementFromList$(L$, 3, 2), 1)) Else O$ = String$(Val(GetElementFromList$(L$, 2, 2)), GetElementFromList$(L$, 3, 2))
        Case "add", "sub", "mul", "div", "modrr", "rtr", "crr": O$ = GRID$(A$(2)) + GRID$(A$(3))
        Case "adm", "sbm", "mum", "dim", "modrm", "mtr", "crm": O$ = GRID$(A$(2)) + GMID$(A$(3), GetElementFromList$(L$, 3, 1))
        Case "arv", "srv", "mrv", "drv", "modrv", "btr", "crv": O$ = GRID$(A$(2)) + Value$(A$(3), 1)
        Case "amv", "smv", "mmv", "dmv", "modmv", "btm", "cmv": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + Value$(A$(3), 1)
        Case "itr": O$ = GRID$(A$(2)) + Value$(A$(3), 2)
        Case "itm": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + Value$(A$(3), 2)
        Case "ltr": O$ = GRID$(A$(2)) + Value$(A$(3), 4)
        Case "ltm": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + Value$(A$(3), 4)
        Case "mtm": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + GMID$(A$(3), GetElementFromList$(L$, 3, 1))
        Case "rtm": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + GRID$(A$(3))
        Case "sfz", "sfc", "sfe", "clz", "clc", "cfe", "hlt", "gk", "wk", "ret", "pusha", "popa", "pushb", "popb", "prn", "lod", "sto", "cls", "sys"
        Case "icr", "dcr", "smrx", "smry": O$ = GRID$(A$(2))
        Case "icm", "dcm", "jiz", "jnz", "jic", "jnc", "je", "jne", "jl", "jle", "jg", "jge", "jmp", "call", "smmx", "smmy": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1))
        Case "cmm": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + GMID$(A$(3), GetElementFromList$(L$, 3, 1))
        Case "cvv": O$ = Value$(A$(2), 1) + Value$(A$(3), 1)
        Case "col": O$ = GRID$(A$(2)) + GRID$(A$(3))
        Case "lct": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + GMID$(A$(3), GetElementFromList$(L$, 3, 1))
        Case "ior", "iow": O$ = GRID$(A$(2)) + GMID$(A$(3), GetElementFromList$(L$, 3, 1)) + GRID$(A$(4))
        Case "spr", "gpr": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + GMID$(A$(3), GetElementFromList$(L$, 3, 1)) + GRID$(A$(4))
        Case "spm", "gpm": O$ = GMID$(A$(2), GetElementFromList$(L$, 2, 1)) + GMID$(A$(3), GetElementFromList$(L$, 3, 1)) + GMID$(A$(4), GetElementFromList$(L$, 4, 1))
        Case Else:
    End Select
    O$ = GKC$(A$(1)) + O$
    Print "->[";
    For I = 1 To Len(O$)
        Print Hex$(Asc(O$, I));
        If I < Len(O$) Then Print ";";
    Next I
    Print "]"
    Put #2, , O$
    If EOF(1) Then Exit Do
Loop
Close
System
Function Lexer$ (text$)
    DefInt A-Z
    Dim Tokens(1) As Token, nToken As _Unsigned Integer
    nToken = 0
    For I = 1 To Len(text$)
        c$ = Mid$(text$, I, 1)
        advanceNToken = False
        Select Case c$
            Case ":": T$ = T_Label: advanceNToken = True
            Case "&": T$ = T_Hex
            Case Chr$(34): stringMode = Not stringMode
            Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9": If oldT$ <> T_Hex Then T$ = T_Number
            Case Chr$(32), Chr$(8), Chr$(9): If stringMode = 0 Then T$ = T_Empty
            Case ";": Exit For
            Case Else
                If (Asc(UCase$(c$)) >= 65 And Asc(UCase$(c$)) <= 90) Or c$ = "_" Then
                    If stringMode Then
                        T$ = T_String
                    Else
                        T$ = T_Command
                    End If
                Else
                    If Not stringMode Then
                        InvalidCharacterError text$, I
                    End If
                End If
        End Select

        If T$ <> T_Empty Then
            If T$ <> oldT$ Then
                Tokens(nToken).Pos_End = I - 1: nToken = nToken + 1: ReDim _Preserve Tokens(nToken) As Token: Tokens(nToken).Pos_Start = I
            ElseIf advanceNToken Then
                Tokens(nToken).Pos_End = I - 1: nToken = nToken + 1: ReDim _Preserve Tokens(nToken) As Token: Tokens(nToken).Pos_Start = I
            End If
            oldT$ = T$: Tokens(nToken).Type = T$
            If T$ = T_Number Or T$ = T_Hex Or T$ = T_String Or T$ = T_Command Then If Not c$ = Chr$(34) Then Tokens(nToken).Value = Tokens(nToken).Value + c$
        End If
        If T$ <> T_String And (T$ = T_Empty And (Tokens(nToken).Type = T_Command Or Tokens(nToken).Type = T_Number)) Then
            Tokens(nToken).Pos_End = I - 1: nToken = nToken + 1: ReDim _Preserve Tokens(nToken) As Token: Tokens(nToken).Pos_Start = I
        End If
    Next I
    If Tokens(nToken).Pos_End = 0 Then Tokens(nToken).Pos_End = Len(text$)
    For I = 1 To nToken
        If I > nToken Then Exit For
        If Not (Tokens(I).Type = "") Then
            If Tokens(I).Type = T_Command Then
                If Tokens(I).Type = T_Command And InStr(Commands, " " + LCase$(Tokens(I).Value) + ", ") Then Tokens(I).Value = LCase$(Tokens(I).Value) Else Tokens(I).Type = T_LabelName
            End If
            If Tokens(I).Type = T_Hex Then
                Tokens(I).Value = _Trim$(Str$(Val("&H" + Mid$(Tokens(I).Value, 2))))
            End If
            If Tokens(I).Type = T_String Then
                Tokens(I).Value = Chr$(34) + Tokens(I).Value + Chr$(34)
            End If
            If Len(Tokens(I).Type) Then L$ = L$ + Tokens(I).Type
            If Len(Tokens(I).Value) Then L$ = L$ + ":" + Tokens(I).Value
            If I < nToken Then L$ = L$ + ","
        End If
    Next I: Lexer$ = L$
End Function
Function GetElementFromList$ (text$, P, C)
    Dim Tokens(1) As Token, nToken As _Unsigned Integer
    nToken = 1: Tokens(1).Pos_Start = 1: V = 0
    For I = 1 To Len(text$): c$ = Mid$(text$, I, 1)
        If c$ = "[" Then nested = nested + 1
        If c$ = "]" Then nested = nested - 1
        If c$ = Chr$(34) Then stringMode = Not stringMode
        If c$ = ":" And nested + stringMode = 0 Then
            V = -1
        ElseIf c$ = "," And nested + stringMode = 0 Then
            V = 0: Tokens(nToken).Pos_End = I - 1: nToken = nToken + 1: ReDim _Preserve Tokens(nToken) As Token: Tokens(nToken).Pos_Start = I
        Else
            If V Then Tokens(nToken).Value = Tokens(nToken).Value + c$ Else Tokens(nToken).Type = Tokens(nToken).Type + c$
        End If
    Next I
    If P = 0 Then
        GetElementFromList$ = _Trim$(Str$(nToken))
    Else
        If C = 1 Then
            GetElementFromList$ = Tokens(P).Type
        ElseIf C = 2 Then
            If Tokens(P).Type = T_String Then GetElementFromList$ = Left$(Mid$(Tokens(P).Value, 2), Len(Tokens(P).Value) - 2) Else GetElementFromList$ = Tokens(P).Value
        End If
    End If
End Function
Sub InvalidCharacterError (t$, p): Print: Print "[Invalid Character Error]": Print t$: If p = 0 Then Print String$(Len(t$), "^") Else Print Space$(p - 1); "^"
End Sub
Sub SyntaxError (t$, p): Print: Print "[Syntax Error]": Print t$: If p = 0 Then Print String$(Len(t$), "^") Else Print Space$(p - 1); "^"
End Sub
Sub IllegalLabelError (t$, p)
    Print "[Illegal Label Error: "; t$; " at line"; p; "]"
End Sub
Function GRID$ (R$)
    Select Case LCase$(R$)
        Case "ra": gri = 1
        Case "rb": gri = 2
        Case "rc": gri = 3
        Case "rd": gri = 4
        Case "re": gri = 5
        Case "rf": gri = 6
        Case "rg": gri = 7
        Case "rh": gri = 8
    End Select
    GRID$ = Chr$(gri)
End Function
Function GMID$ (M$, T$)
    If T$ = T_LabelName Then
        For I = 1 To nLabel
            If Labels(I).Name = M$ Then
                LabelFound = -1
                Exit For
            End If
        Next I
        If LabelFound = -1 Then
            GMID$ = Chr$(Labels(I).Position \ 256) + Chr$(Labels(I).Position Mod 256)
        Else
            IllegalLabelError M$, LN
        End If
    Else
        M& = Val(M$)
        GMID$ = Chr$(M& \ 256) + Chr$(M& Mod 256)
    End If
End Function
Function Value$ (T$, BC)
    Select Case GetElementFromList$(Lexer$(T$), 1, 1)
        Case T_Number:
            If BC = 1 Then
                Value$ = Chr$(Val(T$))
            ElseIf BC = 2 Then
                Value$ = Reverse$(MKI$(Val(T$)))
            ElseIf BC = 4 Then
                Value$ = Reverse$(MKL$(Val(T$)))
            End If
        Case T_LabelName:
            For I = 1 To nLabel
                If Labels(I).Name = T$ Then
                    LabelFound = -1
                    Exit For
                End If
            Next I
            If LabelFound = -1 Then
                If BC = 1 Then
                    Value$ = Chr$(Labels(I).Position)
                ElseIf BC = 2 Then
                    Value$ = Reverse$(MKI$(Labels(I).Position))
                ElseIf BC = 4 Then
                    Value$ = Reverse$(MKL$(Labels(I).Position))
                End If
            Else
                IllegalLabelError T$, LN
            End If
    End Select
End Function
Function GKC$ (A$)
    If Len(A$) = 0 Then Exit Function
    Select Case A$
        Case "nop": c = 0
        Case "add": c = 1
        Case "adm": c = 2
        Case "arv": c = 3
        Case "amv": c = 4
        Case "sub": c = 5
        Case "sbm": c = 6
        Case "srv": c = 7
        Case "smv": c = 8
        Case "mul": c = 9
        Case "mum": c = 10
        Case "mrv": c = 11
        Case "mmv": c = 12
        Case "div": c = 13
        Case "dim": c = 14
        Case "drv": c = 15
        Case "dmv": c = 16
        Case "modrr": c = 17
        Case "modrm": c = 18
        Case "modrv": c = 19
        Case "modmv": c = 20
        Case "btr": c = 32
        Case "btm": c = 33
        Case "itr": c = 34
        Case "itm": c = 35
        Case "ltr": c = 36
        Case "ltm": c = 37
        Case "rtr": c = 38
        Case "mtm": c = 39
        Case "rtm": c = 40
        Case "mtr": c = 41
        Case "sfz": c = 48
        Case "sfc": c = 49
        Case "sfe": c = 50
        Case "clz": c = 55
        Case "clc": c = 56
        Case "cfe": c = 57
        Case "hlt": c = 63
        Case "icr": c = 64
        Case "dcr": c = 65
        Case "icm": c = 66
        Case "dcm": c = 67
        Case "gk": c = 69
        Case "wk": c = 70
        Case "gmx": c = 74
        Case "gmy": c = 75
        Case "smrx": c = 76
        Case "smry": c = 77
        Case "smmx": c = 78
        Case "smmy": c = 79
        Case "crr": c = 80
        Case "crm": c = 81
        Case "cmm": c = 82
        Case "crv": c = 83
        Case "cmv": c = 84
        Case "cvv": c = 85
        Case "jiz": c = 96
        Case "jnz": c = 97
        Case "jic": c = 98
        Case "jnc": c = 99
        Case "je": c = 100
        Case "jne": c = 101
        Case "jl": c = 102
        Case "jle": c = 103
        Case "jg": c = 104
        Case "jge": c = 105
        Case "jmp": c = 111
        Case "call": c = 112
        Case "ret": c = 113
        Case "pusha": c = 128
        Case "popa": c = 129
        Case "pushb": c = 130
        Case "popb": c = 131
        Case "lct": c = 144
        Case "prn": c = 145
        Case "lod": c = 146
        Case "sto": c = 147
        Case "cls": c = 148
        Case "col": c = 149
        Case "ior": c = 160
        Case "iow": c = 161
        Case "spr": c = 176
        Case "spm": c = 177
        Case "gpr": c = 178
        Case "gpm": c = 179
        Case "sys": c = 255
    End Select
    If InStr(" db, times, ", " " + A$ + ", ") Then GKC$ = "" Else GKC$ = Chr$(c)
End Function
Function Reverse$ (IN$)
    L = Len(IN$)
    For I = 1 To Int(L / 2)
        TMP$ = Mid$(IN$, I, 1)
        Mid$(IN$, I, 1) = Mid$(IN$, L - I + 1, 1)
        Mid$(IN$, L - I + 1, 1) = TMP$
    Next I
    Reverse$ = IN$
End Function
