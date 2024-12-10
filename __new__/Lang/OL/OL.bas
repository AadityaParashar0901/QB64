$Console:Only
Dim Shared As _Bit DEBUG
Dim Shared As String VAR_TYPES, KEYWORDS
VAR_TYPES = ListNewRaw$("[bool,byte,ubyte,word,uword,dword,udword,qword,uqword,float,double,string]")
KEYWORDS = ListAppend$(ListNewRaw$("[var]"), VAR_TYPES)
Dim Shared VariablesName$, VariablesType$, VariablesValue$
VariablesName$ = ListNew$
VariablesType$ = ListNew$
VariablesValue$ = ListNew$
10 Do
    On Error GoTo ErrHandler
    Line Input " >"; L$
    If LCase$(L$) = "exit" Then Exit Do
    If LCase$(L$) = "debug" Then DEBUG = Not DEBUG
    D$ = Lexer$(Tokenizer$(L$))
    If DEBUG Then Print "Lexer:";: DictNestPrint D$, 4: Print
    P$ = Parser$(D$)
    If DEBUG Then Print "Parser:";: DictNestPrint P$, 4: Print
    I$ = Interpreter$(P$)
    If Left$(I$, 4) = "Dict" Then DictNestPrint Mid$(I$, 6), 0 Else Print I$
Loop
System
ErrHandler:
Print "[Error:"; Err; "on line"; _ErrorLine; "]"
Resume Next
Sub DictNestPrint (P$, J)
    Print "{"
    For I = 1 To DictLength~&(P$)
        K$ = DictGetKey$(P$, I)
        V$ = DictGetValue$(P$, I)
        Print String$(J, "-"); K$;
        If Len(V$) Then Print ":";
        If K$ = "Dict" Or K$ = "Var" Then
            DictNestPrint V$, J + 1
        Else
            Print V$;
        End If
        If Len(V$) And I < DictLength~&(P$) Then Print ","
    Next I
    Print "}";
End Sub
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
        End Select
        If T$ = "Keyword" Then
            If ListSearch~&(KEYWORDS, LCase$(V$)) = 0 Then T$ = "Variable" 'Else Print V$, ListSearch~&(KEYWORDS, LCase$(V$))
        End If
        LexDict$ = DictAdd$(LexDict$, T$, V$)
    Wend
    Lexer$ = LexDict$
End Function
Function Parser$ (Dict$)
    D$ = Dict$
    For I = 1 To DictLength~&(D$)
        If DictGetValue$(D$, I) = "-" And DictGetKey$(D$, I - 1) <> "Number" And (DictGetKey$(D$, I + 1) = "Number" Or DictGetKey$(D$, I + 1) = "Variable") Then
            D$ = DictDelete$(DictEdit$(D$, I, "Number", "-" + DictGetValue$(D$, I + 1)), I + 1)
        End If
    Next I
    For I = 1 To DictLength~&(D$)
        If DictGetKey$(D$, I) <> "Variable" Then _Continue
        If _StriCmp(DictGetValue$(D$, I - 2), "var") = 0 Then _Continue
        VARIABLEID = ListSearch~&(VariablesName$, DictGetValue$(D$, I))
        T$ = ListGet$(VariablesType$, VARIABLEID)
        V$ = ListGet$(VariablesValue$, VARIABLEID)
        If T$ = "string" Then T$ = "String" Else T$ = "Number"
        D$ = DictEdit$(D$, I, T$, V$)
    Next I
    B$ = StackNew$
    P$ = StackNew$
    For I = 1 To DictLength~&(D$)
        K$ = DictGetKey$(D$, I)
        V$ = DictGetValue$(D$, I)
        If K$ = "LeftBracket" Then
            B$ = StackPush$(B$, V$)
            P$ = StackPush$(P$, MKL$(I))
        End If
        If K$ = "RightBracket" And InStr("([{", StackGet$(B$)) = InStr(")]}", V$) Then
            B$ = StackPop$(B$, "")
            J = CVL(StackGet$(P$)): P$ = StackPop$(P$, "")
            TD$ = DictNew$
            L = I - J - 1
            I = J
            For K = 1 To L
                TD$ = DictAddElement$(TD$, DictGetElement$(D$, K + J))
            Next K
            D$ = DictEdit$(D$, I, "Dict", TD$)
            For K = 0 To L
                D$ = DictDelete$(D$, J + 1)
            Next K
        End If
    Next I
    PriorityList$ = "*/\+-=!><&|~"
    For P = 1 To Len(PriorityList$)
        P$ = Mid$(PriorityList$, P, 1)
        I = 0: While I < DictLength~&(D$): I = I + 1
            If DictGetValue$(D$, I) = P$ Then
                I = I - 1
                TD$ = DictNew$: TD$ = DictAddElement$(TD$, DictGetElement$(D$, I))
                TD$ = DictAddElement$(TD$, DictGetElement$(D$, I + 1))
                TD$ = DictAddElement$(TD$, DictGetElement$(D$, I + 2))
                D$ = DictEdit$(D$, I, "Dict", TD$)
                D$ = DictDelete$(D$, I + 1): D$ = DictDelete$(D$, I + 1)
            End If
        Wend
    Next P
    I = 0: While I < DictLength~&(D$): I = I + 1
        If DictGetKey$(D$, I) <> "Keyword" Then _Continue
        Select Case LCase$(DictGetValue$(D$, I))
            Case "var"
                TD$ = DictAddElement$(DictNew$, DictGetElement$(D$, I + 1))
                TD$ = DictAddElement$(TD$, DictGetElement$(D$, I + 2))
                TD$ = DictAddElement$(TD$, DictGetElement$(D$, I + 3))
                D$ = DictEdit$(D$, I, "Var", TD$)
                D$ = DictDelete$(D$, I + 1): D$ = DictDelete$(D$, I + 1): D$ = DictDelete$(D$, I + 1)
        End Select
    Wend
    Parser$ = D$
End Function
Function Interpreter$ (Dict$)
    Dim Result As _Float
    D$ = Dict$
    I = 0: While I < DictLength~&(D$): I = I + 1
        K$ = DictGetKey$(D$, I)
        V$ = DictGetValue$(D$, I)
        If K$ = "Dict" Then
            V$ = Interpreter$(V$)
            If Len(V$) Then
                D$ = DictSetElement$(D$, I, V$)
            Else
                D$ = DictDelete$(D$, I): I = I - 1
            End If
        End If
    Wend
    I = 0: While I < DictLength~&(D$): I = I + 1
        If DictGetKey$(D$, I) = "Var" Then
            Interpreter_AssignVariable DictGetValue$(D$, I), T$, V$
            D$ = DictEdit$(D$, I, T$, V$)
        End If
    Wend
    I = 0: While I < DictLength~&(D$): I = I + 1
        K$ = DictGetKey$(D$, I)
        V$ = DictGetValue$(D$, I)
        Select Case K$
            Case "Operator", "BitwiseOperator", "Relation": A$ = DictGetValue$(D$, I - 1): B$ = DictGetValue$(D$, I + 1)
                If DictGetKey$(D$, I - 1) = "Number" Then
                    Select Case V$
                        Case "+": Result = Val(A$) + Val(B$)
                        Case "-": Result = Val(A$) - Val(B$)
                        Case "*": Result = Val(A$) * Val(B$)
                        Case "/": Result = Val(A$) / Val(B$)
                        Case "\": Result = Val(A$) \ Val(B$)
                        Case "!": Result = Val(A$) <> Val(B$)
                        Case "<": Result = Val(A$) < Val(B$)
                        Case "=": Result = Val(A$) = Val(B$)
                        Case ">": Result = Val(A$) > Val(B$)
                        Case "&": Result = Val(A$) And Val(B$)
                        Case "|": Result = Val(A$) Or Val(B$)
                    End Select
                    Result$ = _Trim$(Str$(Result))
                    I = I - 1
                    D$ = DictEdit$(D$, I, "Number", Result$)
                ElseIf DictGetKey$(D$, I - 1) = "String" Then
                    A$ = Mid$(A$, 2, Len(A$) - 2)
                    Select Case V$
                        Case "+": B$ = Mid$(B$, 2, Len(B$) - 2): Result$ = A$ + B$
                        Case "-": B$ = Mid$(B$, 2, Len(B$) - 2): Result$ = Left$(A$, InStr(A$, B$) - 1) + Mid$(A$, InStr(A$, B$) + Len(B$))
                        Case "*": Result$ = "": For J = 1 To Val(B$): Result$ = Result$ + A$: Next J
                        Case "!": B$ = Mid$(B$, 2, Len(B$) - 2): If A$ <> B$ Then Result$ = "1" Else Result$ = "0"
                        Case "=": B$ = Mid$(B$, 2, Len(B$) - 2): If A$ = B$ Then Result$ = "1" Else Result$ = "0"
                    End Select
                    I = I - 1
                    D$ = DictEdit$(D$, I, "String", Chr$(34) + Result$ + Chr$(34))
                End If
                D$ = DictDelete$(D$, I + 1): D$ = DictDelete$(D$, I + 1)
            Case "NotOperator": B$ = DictGetValue$(D$, I + 1)
                Result$ = _Trim$(Str$(Not Val(B$)))
                D$ = DictEdit$(D$, I, "Number", Result$)
                D$ = DictDelete$(D$, I + 1)
        End Select
    Wend
    If DictLength~&(D$) > 1 Then Interpreter$ = "Dict:" + D$ Else Interpreter$ = DictGetElement$(D$, 1)
End Function
Sub Interpreter_AssignVariable (D$, T$, V$)
    __T$ = DictGetValue$(D$, 1)
    If InStr(VAR_TYPES, __T$) = 0 Then Exit Sub
    If DictGetKey$(D$, 2) = "Dict" Then
        __N$ = DictGetValue$(DictGetValue$(D$, 2), 1)
        __V$ = DictGetValue$(DictGetValue$(D$, 2), 3)
    Else
        __N$ = DictGetValue$(D$, 2)
        If __T$ = "string" Then __V$ = String$(2, 34) Else __V$ = "0"
    End If
    VariablesName$ = ListAdd$(VariablesName$, __N$)
    VariablesType$ = ListAdd$(VariablesType$, __T$)
    VariablesValue$ = ListAdd$(VariablesValue$, __V$)
    V$ = __V$
    If __T$ = "string" Then T$ = "String" Else T$ = "Number"
End Sub
'$Include:'files\QBListDict.bas'
Sub UnExpectedCharactedError: Print "Unexpected Characted Error": End Sub
Function StackNew$: StackNew$ = ListNew$: End Function
Function StackPush$ (Stack$, E$): StackPush$ = ListAdd$(Stack$, E$): End Function
Function StackPop$ (Stack$, E$): E$ = ListGet$(Stack$, ListLength~&(Stack$)): StackPop$ = ListDelete$(Stack$, ListLength~&(Stack$)): End Function
Function StackGet$ (Stack$): StackGet$ = ListGet$(Stack$, ListLength~&(Stack$)): End Function
