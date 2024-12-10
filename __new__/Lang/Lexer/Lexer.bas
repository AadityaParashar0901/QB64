$Console:Only
'$Dynamic
Do
    Line Input " > "; L$
    Print "   "; Tokenizer$(L$)
Loop While _StriCmp(_Trim$(L$), "exit")
System
Function Tokenizer$ (L$)
    Dim Tokens(1) As String, nToken As _Unsigned Integer
    nToken = 1
    For I = 1 To Len(L$)
        Select Case Asc(L$, I)
            Case 32:
                If Tokens(nToken) <> "" Then
                    TokenList$ = TokenList$ + Tokens(nToken) + ","
                    nToken = nToken + 1
                    ReDim _Preserve Tokens(nToken) As String
                End If
            Case 33 To 47, 58 To 64, 91 To 96, 123 To 126
                If Tokens(nToken) <> "" Then
                    TokenList$ = TokenList$ + Tokens(nToken) + ","
                    nToken = nToken + 1
                    ReDim _Preserve Tokens(nToken) As String
                End If
                Tokens(nToken) = Mid$(L$, I, 1)
                TokenList$ = TokenList$ + Tokens(nToken) + ","
                nToken = nToken + 1
                ReDim _Preserve Tokens(nToken) As String
            Case Else: Tokens(nToken) = Tokens(nToken) + Mid$(L$, I, 1)
        End Select
    Next I
    If Tokens(nToken) = "" Then
        TokenList$ = Left$(TokenList$, Len(TokenList$) - 1)
    Else
        TokenList$ = TokenList$ + Tokens(nToken)
    End If
    Tokenizer$ = TokenList$
End Function
Function Lexer$ (L$)
    Dim Tokens(1, 1) As String, nToken As _Unsigned Integer
    nToken = 1
    T$ = Tokenizer$(L$)
    For I = 1 To nToken

    Next I
    Lexer$ = LexDict$
End Function
