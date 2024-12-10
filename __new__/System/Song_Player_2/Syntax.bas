Function Tokenizer$ (L$)
    Dim Token As String
    TokenList$ = ListNew$
    For I = 1 To Len(L$)
        C$ = Mid$(L$, I, 1)
        Select Case Asc(C$)
            Case 32
                If StringMode = 0 Then
                    If Token <> "" Then
                        TokenList$ = ListAdd$(TokenList$, Token)
                        Token = ""
                    End If
                Else
                    Token = Token + C$
                End If
            Case 34: StringMode = Not StringMode
                If StringMode Then
                    If Len(Token) Then TokenList$ = ListAdd$(TokenList$, Token)
                    Token = C$
                Else
                    Token = Token + C$
                End If
            Case 33, 40 To 45, 47, 58 To 64, 91 To 94, 96, 123 To 125
                If StringMode = 0 Then
                    If Len(Token) Then
                        TokenList$ = ListAdd$(TokenList$, Token)
                        Token = ""
                    End If
                    Token = C$
                    TokenList$ = ListAdd$(TokenList$, Token)
                    Token = ""
                Else
                    Token = Token + C$
                End If
            Case 46, 48 To 57
                If StringMode = 0 Then
                    If InStr("0123456789.", Right$(Token, 1)) Then
                        Token = Token + C$
                    Else
                        If Len(Token) Then TokenList$ = ListAdd$(TokenList$, Token)
                        Token = C$
                    End If
                Else
                    Token = Token + C$
                End If
            Case Else:
                If StringMode = 0 And Len(Token) Then
                    If InStr("0123456789.", Right$(Token, 1)) Then
                        TokenList$ = ListAdd$(TokenList$, Token)
                        Token = C$
                    Else
                        Token = Token + C$
                    End If
                Else
                    Token = Token + C$
                End If
        End Select
    Next I
    If Len(Token) Then TokenList$ = ListAdd$(TokenList$, Token)
    Tokenizer$ = TokenList$
End Function
Function Lexer$ (L$)
    LexDict$ = DictNew$
    Lexer$ = LexDict$
    I = 0: While I < ListLength~&(L$): I = I + 1
        V$ = ListGet$(L$, I)
        Select Case Asc(V$, 1)
            Case 34: T$ = "String"
            Case 65 To 90, 97 To 122: T$ = "Keyword"
            Case 46, 48 To 57: T$ = "Number"
            Case 33, 42, 43, 45, 92: T$ = "Operator"
            Case 47: T$ = "Operator"
            Case 60 To 62: T$ = "Relation"
            Case 39: T$ = "Line Seperator"
            Case 38, 124, 94: T$ = "BitwiseOperator"
            Case 126: T$ = "NotOperator"
            Case 40, 91, 123: T$ = "LeftBracket"
            Case 41, 93, 125: T$ = "RightBracket"
            Case 44: T$ = "Seperator"
        End Select
        LexDict$ = DictAdd$(LexDict$, T$, V$)
    Wend
    Lexer$ = LexDict$
End Function
Sub Parse (FILE$)
    Dim As _Unsigned _Byte MODE_PLAYLIST
    __F = FreeFile
    Open FILE$ For Input As #__F
    O$ = ListNew$
    Do
        LINENUMBER = LINENUMBER + 1
        Line Input #__F, L$
        L$ = Lexer$(Tokenizer$(L$))
        Select Case DictGetKey$(L$, DictLength~&(L$))
            Case "Number": T$ = ListAdd$(ListAdd$(ListAdd$(ListAdd$(ListNew$, OneByteEncode$(DoubleTrim$(DictGetValue$(L$, 1), 1))), DictGetValue$(L$, 2)), "0"), DictGetValue$(L$, 3))
                S$ = ListAdd$(S$, T$)
                Print "    Registering Song: "; OneByteDecode$(ListGet$(T$, 1))
            Case "LeftBracket": If MODE_PLAYLIST = 0 Then MODE_PLAYLIST = 1 Else Print "Error at Line"; LINENUMBER: System
                P$ = ListAdd$(ListNew$, DictGetValue$(L$, 1))
                P$ = ListAdd$(P$, DictGetValue$(L$, 2))
                S$ = ListNew$
                Print "List Start: "; DictGetValue$(L$, 1)
            Case "RightBracket": If MODE_PLAYLIST = 1 Then MODE_PLAYLIST = 0 Else Print "Error at Line"; LINENUMBER: System
                O$ = ListAdd$(O$, ListAdd$(P$, S$))
        End Select
    Loop Until EOF(__F)
    Close #__F
    Parsed$ = O$
    'list(list(list))
End Sub
Sub SaveParse (FILE$)
    __F = FreeFile
    Open FILE$ For Binary As #__F
    Put #__F, , Parsed$
    Close #__F
End Sub
