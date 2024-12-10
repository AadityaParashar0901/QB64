'$Dynamic
$Console:Only
Type Token: Type As String: Value As String: Pos_Start As Integer: Pos_End As Integer: End Type
Type Variable: Name As String: Type As String: Value As String: End Type
Const True = -1, False = 0
Dim Shared As _Bit Debug, Debug1, Debug2, Debug3: If Command$(1) = "DEBUG" Then Debug = True: Debug1 = True: Debug2 = True: Debug3 = True
Const T_Command = "Command", T_String = "String", T_Number = "Number", T_Variable = "Variable", T_List = "List", T_Block = "Block", T_Empty = "Empty", T_NewLine = "NewLine", T_UnderScore = "_", T_BlockStart = "{", T_BlockEnd = "}"
Const T_Plus = "Plus", T_Minus = "Minus", T_Multiply = "Multiply", T_Divide = "Division", T_Power = "Power", T_LeftParenthesis = "LPR", T_RightParenthesis = "RPR", T_Equal = "Equal", T_DoubleEqual = "DoubleEqual", T_NotEqual = "NotEqual", T_Less = "Less", T_Greater = "Greater", T_LessEqual = "LessEqual", T_GreaterEqual = "GreaterEqual", T_And = "And", T_Or = "Or", T_Not = "Not"
Const T_Increment = "Increment", T_Decrement = "Decrement", T_Equate = "Equate", T_PlusEquate = "PlusEquate", T_MinusEquate = "MinusEquate", T_MultiplyEquate = "MultiplyEquate", T_DivideEquate = "DivideEquate", T_PowerEquate = "PowerEquate"
Const T_Implies = "Implies", T_Else = "Else", T_While = "While"
Const Commands = "AND, OR, NOT, TRUE, FALSE, VAR, "
Dim Shared EmptyToken As Token
Dim Shared Variables(2) As Variable, nVariable As _Unsigned Integer, Condition As _Bit
nVariable = 2
Variables(1).Name = "PI": Variables(1).Type = T_Number: Variables(1).Value = "3.141592653589793"
Variables(2).Name = "LR": Variables(2).Type = T_Number: Variables(2).Value = "0"
Dim Shared LineInput$
If _CommandCount And _FileExists(Command$(1)) Then FileMode = -1: Open Command$(1) For Input As #1
Do
    TakeInput:
    If FileMode = True Then
        Line Input #1, LineInput$
        If EOF(1) Then FileMode = False: Close
    Else
        Line Input ">>>> "; LineInput$
    End If
    If UCase$(LineInput$) = "EXIT" Then Exit Do
    If UCase$(LineInput$) = "DEBUG" Then Debug = Not Debug: GoTo TakeInput
    If UCase$(LineInput$) = "DEBUG1" Then Debug1 = Not Debug1: GoTo TakeInput
    If UCase$(LineInput$) = "DEBUG2" Then Debug2 = Not Debug2: GoTo TakeInput
    If UCase$(LineInput$) = "DEBUG3" Then Debug3 = Not Debug3: GoTo TakeInput
    If UCase$(LineInput$) = "CLV" Then ReDim _Preserve Variables(2) As Variable: GoTo TakeInput
    If UCase$(LineInput$) = "CLS" Then Cls: GoTo TakeInput
    If Debug Then
        Print "{": Lex$ = Lexer$(LineInput$): Print "}": Print "Lexer: "; Lex$
        Print "{": Parse$ = Parser$(Lex$): Print "}": Print "Parser: "; Parse$
        Print "{": Interpret$ = Interpreter$(Parse$, 1): Print "}": Print "Interpreter: "; Interpret$
    Else
        Print " >>> "; Interpreter$(Parser$(Lexer$(LineInput$)), 1)
    End If
Loop
System
ErrorHandler:
Print "Error"; Err; "on Line:"; _ErrorLine
Print _ErrorMessage$
Resume TakeInput
Function Lexer$ (text$)
    On Error GoTo ErrorHandler
    DefInt A-Z
    Dim Tokens(1) As Token, nToken As _Unsigned Integer
    nToken = 0
    For I = 1 To Len(text$)
        c$ = Mid$(text$, I, 1)
        advanceNToken = False
        Select Case c$
            Case "+": T$ = T_Plus: advanceNToken = True
            Case "-": T$ = T_Minus: advanceNToken = True
            Case "*": T$ = T_Multiply: advanceNToken = True
            Case "/": T$ = T_Divide: advanceNToken = True
            Case "^": T$ = T_Power: advanceNToken = True
            Case "(": T$ = T_LeftParenthesis: advanceNToken = True
            Case ")": T$ = T_RightParenthesis: advanceNToken = True
            Case "=": T$ = T_Equal: advanceNToken = True
            Case "!": T$ = T_NotEqual: advanceNToken = True
            Case ">": T$ = T_Greater: advanceNToken = True
            Case "<": T$ = T_Less: advanceNToken = True
            Case "&": T$ = T_And: advanceNToken = True
            Case "|": T$ = T_Or: advanceNToken = True
            Case "~": T$ = T_Not: advanceNToken = True
            Case Chr$(34): stringMode = Not stringMode
            Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9": T$ = T_Number
            Case ".":
            Case Chr$(32), Chr$(8), Chr$(9): T$ = T_Empty
            Case ";": T$ = T_NewLine: advanceNToken = True
            Case "_": T$ = T_UnderScore: advanceNToken = True
            Case "{": T$ = T_BlockStart: advanceNToken = True
            Case "}": T$ = T_BlockEnd: advanceNToken = True
            Case "@": T$ = T_While: advanceNToken = True
            Case Else
                If (Asc(UCase$(c$)) >= 65 And Asc(UCase$(c$)) <= 90) Or c$ = "_" Then
                    If stringMode Then T$ = T_String Else T$ = T_Command
                Else
                    If Not stringMode Then
                        InvalidCharacterError text$, I: If Not Debug1 Then Exit Function
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
            If T$ = T_Number Or T$ = T_String Or T$ = T_Command Then If Not c$ = Chr$(34) Then Tokens(nToken).Value = Tokens(nToken).Value + c$
        End If
        If T$ <> T_String And (T$ = T_Empty And (Tokens(nToken).Type = T_Command Or Tokens(nToken).Type = T_Number)) Then
            Tokens(nToken).Pos_End = I - 1: nToken = nToken + 1: ReDim _Preserve Tokens(nToken) As Token: Tokens(nToken).Pos_Start = I
        ElseIf T$ = T_LeftParenthesis Then
            Tokens(nToken).Type = T_List: nested = 1
            Do: I = I + 1
                If I > Len(text$) Then SyntaxError text$, I: If Not Debug1 Then Exit Function
                c$ = Mid$(text$, I, 1)
                If c$ = "(" Then nested = nested + 1
                If c$ = ")" Then nested = nested - 1
                If nested = 0 Then Exit Do
                Tokens(nToken).Value = Tokens(nToken).Value + c$
            Loop: Tokens(nToken).Value = "[" + Lexer$(Tokens(nToken).Value) + "]"
        ElseIf T$ = T_BlockStart Then
            Tokens(nToken).Type = T_Block: nested = 1
            Do: I = I + 1
                If I > Len(text$) Then SyntaxError text$, I: If Not Debug1 Then Exit Function
                c$ = Mid$(text$, I, 1)
                If c$ = "{" Then nested = nested + 1
                If c$ = "}" Then nested = nested - 1
                If nested = 0 Then Exit Do
                Tokens(nToken).Value = Tokens(nToken).Value + c$
            Loop: Tokens(nToken).Value = "[" + Lexer$(Tokens(nToken).Value) + "]"
        End If
    Next I
    If Tokens(nToken).Pos_End = 0 Then Tokens(nToken).Pos_End = Len(text$)
    If Debug Then For I = 1 To nToken: Print I; Tokens(I).Type; ":"; Tokens(I).Value: Next I
    For I = 1 To nToken
        If I > nToken Then Exit For
        If Not (Tokens(I).Type = "") Then
            If Tokens(I).Type = T_Command Then
                isVar = False
                For j = 1 To nVariable: If Variables(j).Name = Tokens(I).Value Then isVar = True: Exit For
                Next j: If isVar Then Tokens(I).Type = T_Variable Else If Tokens(I).Type = T_Command And InStr(Commands, UCase$(Tokens(I).Value) + ", ") Then Tokens(I).Value = UCase$(Tokens(I).Value)
            End If
            advanceI = 0
            If I < nToken Then
                If Tokens(I).Type = T_Plus And Tokens(I + 1).Type = T_Plus Then
                    Tokens(I).Type = T_Increment: advanceI = 1
                ElseIf Tokens(I).Type = T_Minus And Tokens(I + 1).Type = T_Minus Then
                    Tokens(I).Type = T_Decrement: advanceI = 1
                ElseIf Tokens(I).Type = T_Equal And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_Equate: advanceI = 1
                ElseIf Tokens(I).Type = T_NotEqual And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_NotEqual: advanceI = 1
                ElseIf Tokens(I).Type = T_Less And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_LessEqual: advanceI = 1
                ElseIf Tokens(I).Type = T_Greater And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_GreaterEqual: advanceI = 1
                ElseIf Tokens(I).Type = T_Plus And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_PlusEquate: advanceI = 1
                ElseIf Tokens(I).Type = T_Minus And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_MinusEquate: advanceI = 1
                ElseIf Tokens(I).Type = T_Multiply And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_MultiplyEquate: advanceI = 1
                ElseIf Tokens(I).Type = T_Divide And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_DivideEquate: advanceI = 1
                ElseIf Tokens(I).Type = T_Power And Tokens(I + 1).Type = T_Equal Then
                    Tokens(I).Type = T_PowerEquate: advanceI = 1
                ElseIf Tokens(I).Type = T_Minus And Tokens(I + 1).Type = T_Greater Then
                    Tokens(I).Type = T_Implies: advanceI = 1
                ElseIf Tokens(I).Type = T_Or And Tokens(I + 1).Type = T_Or Then
                    Tokens(I).Type = T_Else: advanceI = 1
                ElseIf Tokens(I - 1).Type <> T_Number And Tokens(I).Type = T_Minus And Tokens(I + 1).Type = T_Number Then
                    Tokens(I).Type = T_Number: Tokens(I).Value = _Trim$(Str$(-Val(Tokens(I + 1).Value))): advanceI = 1
                End If
            End If
            L$ = L$ + Tokens(I).Type
            If Len(Tokens(I).Value) Then L$ = L$ + ":" + Tokens(I).Value
            I = I + advanceI
            If I < nToken Then L$ = L$ + ","
        End If
    Next I: Lexer$ = L$
End Function
Function Parser$ (text$)
    On Error GoTo ErrorHandler
    Dim Tokens(1) As Token, nToken As _Unsigned Integer
    nToken = 1: Tokens(1).Pos_Start = 1: V = 0
    For i = 1 To Len(text$)
        c$ = Mid$(text$, i, 1)
        If c$ = "[" Then nested = nested + 1
        If c$ = "]" Then nested = nested - 1
        If c$ = ":" And nested = 0 Then
            V = -1
        ElseIf c$ = "," And nested = 0 Then
            V = 0
            Tokens(nToken).Pos_End = i - 1
            nToken = nToken + 1
            ReDim _Preserve Tokens(nToken) As Token
            Tokens(nToken).Pos_Start = i
        Else
            If V Then Tokens(nToken).Value = Tokens(nToken).Value + c$ Else Tokens(nToken).Type = Tokens(nToken).Type + c$
        End If
    Next i
    If nToken = 1 Then Parser$ = LexValue$(Tokens(1)): Exit Function
    i = 0: Do: i = i + 1
        If Tokens(i + 1).Type = T_Increment Or Tokens(i + 1).Type = T_Decrement Then
            Tokens(i).Value = "[" + LexValue$(Tokens(i)) + "," + LexValue$(Tokens(i + 1)) + "]": Tokens(i).Type = T_List
            For j = i + 2 To nToken: Tokens(j - 1) = Tokens(j): Next j: nToken = nToken - 1: i = i - 1
        End If
        If i + 1 >= nToken Then Exit Do
    Loop
    i = 0: Do: i = i + 1
        If Tokens(i).Type = T_Command And Tokens(i).Value = "VAR" Then
            Tokens(i).Value = "[" + LexValue$(Tokens(i)) + "," + LexValue$(Tokens(i + 1)) + "," + LexValue$(Tokens(i + 2)) + "," + LexValue$(Tokens(i + 3)) + "]": Tokens(i).Type = T_List
            For j = i + 4 To nToken: Tokens(j - 3) = Tokens(j): Next j: nToken = nToken - 3: i = i - 1
        End If
        If i + 1 >= nToken Then Exit Do
    Loop
    For tt = 0 To 11
        Select Case tt
            Case 0: T$ = T_While
            Case 1: T$ = T_Divide
            Case 2: T$ = T_Multiply
            Case 3: T$ = T_Minus
            Case 4: T$ = T_Plus
            Case 5: T$ = T_Power
            Case 6: T$ = T_Equal
            Case 7: T$ = T_NotEqual
            Case 8: T$ = T_Less
            Case 9: T$ = T_Greater
            Case 10: T$ = T_LessEqual
            Case 11: T$ = T_GreaterEqual
            Case Else: _Continue
        End Select
        i = 0: Do: i = i + 1
            If Tokens(i + 1).Type = T$ Then
                Tokens(i).Value = "[" + LexValue$(Tokens(i)) + "," + LexValue$(Tokens(i + 1)) + "," + LexValue$(Tokens(i + 2)) + "]": Tokens(i).Type = T_List
                For j = i + 3 To nToken: Tokens(j - 2) = Tokens(j): Next j: nToken = nToken - 2: i = i - 1
            End If
            If i + 1 >= nToken Then Exit Do
        Loop
    Next tt
    i = 0: Do: i = i + 1
        If Tokens(i).Type = T_Not Then
            Tokens(i).Value = "[" + LexValue$(Tokens(i)) + "," + LexValue$(Tokens(i + 1)) + "]": Tokens(i).Type = T_List
            For j = i + 2 To nToken: Tokens(j - 1) = Tokens(j): Next j: nToken = nToken - 1: i = i - 1
        End If
        If i + 1 >= nToken Then Exit Do
    Loop
    i = 0: Do: i = i + 1
        If Tokens(i + 1).Type = T_Implies Then
            If UBound(Tokens) >= i + 4 Then
                If Tokens(i + 3).Type = T_Else Then
                    Tokens(i).Value = "[" + LexValue$(Tokens(i)) + "," + LexValue$(Tokens(i + 1)) + "," + LexValue$(Tokens(i + 2)) + "," + LexValue$(Tokens(i + 3)) + "," + LexValue$(Tokens(i + 4)) + "]": Tokens(i).Type = T_List
                    For j = i + 5 To nToken: Tokens(j - 4) = Tokens(j): Next j: nToken = nToken - 4: i = i - 1
                Else
                    Tokens(i).Value = "[" + LexValue$(Tokens(i)) + "," + LexValue$(Tokens(i + 1)) + "," + LexValue$(Tokens(i + 2)) + "]": Tokens(i).Type = T_List
                    For j = i + 3 To nToken: Tokens(j - 2) = Tokens(j): Next j: nToken = nToken - 2: i = i - 1
                End If
            Else
                Tokens(i).Value = "[" + LexValue$(Tokens(i)) + "," + LexValue$(Tokens(i + 1)) + "," + LexValue$(Tokens(i + 2)) + "]": Tokens(i).Type = T_List
                For j = i + 3 To nToken: Tokens(j - 2) = Tokens(j): Next j: nToken = nToken - 2: i = i - 1
            End If
        End If
        If i + 1 >= nToken Then Exit Do
    Loop
    For i = 1 To nToken: L$ = L$ + Tokens(i).Type: If Len(Tokens(i).Value) Then L$ = L$ + ":" + Tokens(i).Value
        If i < nToken Then L$ = L$ + ","
    Next i: Parser$ = L$
End Function
Function Interpreter$ (text$, Nest)
    On Error GoTo ErrorHandler
    Dim Tokens(1) As Token, nToken As Integer
    Dim As Token WHILE_CONDITION_TOKEN
    Dim Result As _Float
    If Debug Then Print Nest; "Input String: "; text$
    nToken = 1: Tokens(1).Pos_Start = 1: V = 0
    For I = 1 To Len(text$): c$ = Mid$(text$, I, 1)
        If c$ = "[" Then nested = nested + 1
        If c$ = "]" Then nested = nested - 1
        If c$ = ":" And nested = 0 Then
            V = -1
        ElseIf c$ = "," And nested = 0 Then
            V = 0: Tokens(nToken).Pos_End = I - 1: nToken = nToken + 1: ReDim _Preserve Tokens(nToken) As Token: Tokens(nToken).Pos_Start = I
        Else
            If V Then Tokens(nToken).Value = Tokens(nToken).Value + c$ Else Tokens(nToken).Type = Tokens(nToken).Type + c$
        End If
    Next I
    For I = 1 To nToken: If Tokens(I).Type = T_List Then If Not (Tokens(I - 1).Type = T_Command And Tokens(I - 1).Value = "FOR") Then Tokens(I).Value = Interpreter$(Mid$(Tokens(I).Value, 2, Len(Tokens(I).Value) - 2), Nest + 1): Tokens(I).Type = T_Number
        Next I: For I = 1 To nToken
        If Debug3 Then
            Print Nest; "Interpreter Tokens: ("; nToken; ") {";
            For J = 1 To nToken
                Print LexValue$(Tokens(J));
                If J < nToken Then Print ",";
            Next J
            Print "}"
        End If
        Select Case Tokens(I).Type
            Case T_Block: Tokens(I).Type = T_Number: Tokens(I).Value = Interpreter$(Mid$(Tokens(I).Value, 2, Len(Tokens(I).Value) - 2), Nest + 1)

            Case T_Variable:
                For J = 1 To nVariable
                    If Tokens(I).Value = Variables(J).Name Then nVar = J: Exit For
                Next J
                Tokens(I).Type = Variables(nVar).Type
                Tokens(I).Value = Variables(nVar).Value

            Case T_Command: Select Case UCase$(Tokens(I).Value)
                    Case "TRUE": Tokens(I).Type = T_Number: Tokens(I).Value = "-1"
                    Case "FALSE": Tokens(I).Type = T_Number: Tokens(I).Value = "0"
                    Case "VAR":
                        VName$ = Tokens(I + 1).Value
                        VType$ = Tokens(I + 3).Type
                        nVar = 0
                        For J = 1 To nVariable
                            If VName$ = Variables(J).Name Then nVar = J: Exit For
                        Next J
                        If nVar = 0 Then nVar = nVariable + 1: nVariable = nVariable + 1: NewVar = True Else NewVar = False
                        ReDim _Preserve Variables(nVariable) As Variable
                        Select Case Tokens(I + 2).Type
                            Case T_Equate: VValue$ = Interpreter$(LexValue$(Tokens(I + 3)), Nest + 1)
                            Case T_PlusEquate: If VType$ = T_String Then VValue$ = Variables(nVar).Value + Tokens(I + 3).Value Else VValue$ = _Trim$(Str$(Val(Variables(nVar).Value) + Val(Interpreter$(LexValue$(Tokens(I + 3)), Nest + 1))))
                            Case T_MinusEquate: VValue$ = _Trim$(Str$(Val(Variables(nVar).Value) - Val(Interpreter$(LexValue$(Tokens(I + 3)), Nest + 1))))
                            Case T_MultiplyEquate: VValue$ = _Trim$(Str$(Val(Variables(nVar).Value) * Val(Interpreter$(LexValue$(Tokens(I + 3)), Nest + 1))))
                            Case T_DivideEquate: VValue$ = _Trim$(Str$(Val(Variables(nVar).Value) / Val(Interpreter$(LexValue$(Tokens(I + 3)), Nest + 1))))
                            Case T_PowerEquate: VValue$ = _Trim$(Str$(Val(Variables(nVar).Value) ^ Val(Interpreter$(LexValue$(Tokens(I + 3)), Nest + 1))))
                            Case Else: SyntaxError LineInput$, 0: If Not Debug3 Then Exit Function
                        End Select
                        Variables(nVar).Name = VName$
                        Variables(nVar).Type = VType$
                        Variables(nVar).Value = VValue$
                        Tokens(I).Type = VType$
                        Tokens(I).Value = VValue$
                        For J = I + 4 To nToken
                            Tokens(J - 3) = Tokens(J)
                        Next J
                        nToken = nToken - 3
                        I = I - 1

                End Select

            Case T_Implies:
                If UBound(Tokens) >= I + 3 Then
                    If Tokens(I + 2).Type = T_Else Then
                        If Val(Interpreter$(LexValue$(Tokens(I - 1)), Nest + 1)) Then
                            Tokens(I - 1) = Tokens(I + 1)
                            Tokens(I + 0) = Tokens(I + 1): Tokens(I + 1) = Tokens(I + 1): Tokens(I + 2) = Tokens(I + 1): Tokens(I + 3) = Tokens(I + 1)
                        Else
                            Tokens(I - 1) = Tokens(I + 3)
                            Tokens(I + 0) = Tokens(I + 3): Tokens(I + 1) = Tokens(I + 3): Tokens(I + 2) = Tokens(I + 3): Tokens(I + 3) = Tokens(I + 3)
                        End If
                        For J = I + 3 To nToken
                            Tokens(J - 3) = Tokens(J)
                            If Debug3 Then
                                Print Nest; "Interpreter Edited Tokens: {";
                                For J = 1 To nToken
                                    Print LexValue$(Tokens(J));
                                    If J < nToken Then Print ",";
                                Next J
                                Print "}"
                            End If
                        Next J
                        nToken = nToken - 4: I = I - 2
                    Else
                        If Val(Interpreter$(LexValue$(Tokens(I - 1)), Nest + 1)) Then Tokens(I - 1) = Tokens(I + 1)
                        For J = I + 1 To nToken: Tokens(J - 1) = Tokens(J): Next J: nToken = nToken - 2: I = I - 2
                    End If
                Else
                    If Val(Interpreter$(LexValue$(Tokens(I - 1)), Nest + 1)) Then Tokens(I - 1) = Tokens(I + 1)
                    For J = I + 1 To nToken: Tokens(J - 1) = Tokens(J): Next J: nToken = nToken - 2: I = I - 2
                End If

            Case T_While:
                WHILE_CONDITION_TOKEN = Tokens(I - 1)
                WHILE_STATEMENTS$ = LexValue$(Tokens(I + 1))
                WHILE_CONDITION = Val(Interpreter$(LexValue$(WHILE_CONDITION_TOKEN), Nest + 1))
                If Debug3 Then Print "While Condition: "; WHILE_CONDITION
                While WHILE_CONDITION
                    Tokens(I - 1).Value = Interpreter$(WHILE_STATEMENTS$, Nest + 1)
                Wend
                For J = I + 1 To nToken: Tokens(J - 1) = Tokens(J): Next J: nToken = nToken - 2: I = I - 2

            Case T_Increment, T_Decrement:
                Tokens(I).Type = ""

            Case T_And, T_Or: Tokens(I - 1).Type = T_Number
                Select Case Tokens(I).Type
                    Case T_And: Result = Val(Tokens(I - 1).Value) And Val(Tokens(I + 1).Value)
                    Case T_Or: Result = Val(Tokens(I - 1).Value) Or Val(Tokens(I + 1).Value)
                End Select
                Tokens(I - 1).Value = _Trim$(Str$(Result))
                For J = I + 1 To nToken: Tokens(J - 1) = Tokens(J): Next J: nToken = nToken - 2: I = I - 2

            Case T_Not: Tokens(I).Type = T_Number
                If Val(Tokens(I + 1).Value) Then Tokens(I).Value = "False" Else Tokens(I).Value = "True"
                For J = I + 2 To nToken: Tokens(J - 1) = Tokens(J): Next J: nToken = nToken - 1: I = I - 1

            Case T_Plus, T_Minus, T_Multiply, T_Divide, T_Power
                If Tokens(I - 1).Type = T_Number And Tokens(I + 1).Type = T_Number Then
                    Tokens(I - 1).Type = T_Number
                    Select Case Tokens(I).Type
                        Case T_Plus: Result = Val(Tokens(I - 1).Value) + Val(Tokens(I + 1).Value)
                        Case T_Minus: Result = Val(Tokens(I - 1).Value) - Val(Tokens(I + 1).Value)
                        Case T_Multiply: Result = Val(Tokens(I - 1).Value) * Val(Tokens(I + 1).Value)
                        Case T_Divide: Result = Val(Tokens(I - 1).Value) / Val(Tokens(I + 1).Value)
                        Case T_Power: Result = Val(Tokens(I - 1).Value) ^ Val(Tokens(I + 1).Value)
                    End Select
                    If Abs(Result) > 0 And Abs(Result) < 1 Then Tokens(I - 1).Value = "0" + _Trim$(Str$(Result)) Else Tokens(I - 1).Value = _Trim$(Str$(Result))
                    For J = I + 1 To nToken: Tokens(J - 1) = Tokens(J): Next J: nToken = nToken - 2: I = I - 2
                ElseIf Tokens(I - 1).Type <> T_Number And Tokens(I + 1).Type = T_Number Then
                    Select Case Tokens(I).Type
                        Case T_Minus: Result = -Val(Tokens(I + 1).Value)
                    End Select
                    Tokens(I).Type = T_Number
                    If Abs(Result) > 0 And Abs(Result) < 1 Then Tokens(I).Value = "0" + _Trim$(Str$(Result)) Else Tokens(I).Value = _Trim$(Str$(Result))
                    For J = I + 2 To nToken: Tokens(J - 1) = Tokens(J): Next J: nToken = nToken - 1: I = I - 1
                End If

            Case T_Equal, T_NotEqual, T_Less, T_Greater, T_LessEqual, T_GreaterEqual
                If Tokens(I - 1).Type = T_String And Tokens(I + 1).Type = T_String Then
                    Select Case Tokens(I).Type
                        Case T_Equal: Result = (Tokens(I - 1).Value = Tokens(I + 1).Value)
                        Case T_NotEqual: Result = (Tokens(I - 1).Value <> Tokens(I + 1).Value)
                        Case T_Less: Result = (Len(Tokens(I - 1).Value) < Len(Tokens(I + 1).Value))
                        Case T_Greater: Result = (Len(Tokens(I - 1).Value) > Len(Tokens(I + 1).Value))
                        Case T_LessEqual: Result = (Len(Tokens(I - 1).Value) <= Len(Tokens(I + 1).Value))
                        Case T_GreaterEqual: Result = (Len(Tokens(I - 1).Value) >= Len(Tokens(I + 1).Value))
                    End Select
                Else
                    Select Case Tokens(I).Type
                        Case T_Equal: Result = (Val(Tokens(I - 1).Value) = Val(Tokens(I + 1).Value))
                        Case T_NotEqual: Result = (Val(Tokens(I - 1).Value) <> Val(Tokens(I + 1).Value))
                        Case T_Less: Result = (Val(Tokens(I - 1).Value) < Val(Tokens(I + 1).Value))
                        Case T_Greater: Result = (Val(Tokens(I - 1).Value) > Val(Tokens(I + 1).Value))
                        Case T_LessEqual: Result = (Val(Tokens(I - 1).Value) <= Val(Tokens(I + 1).Value))
                        Case T_GreaterEqual: Result = (Val(Tokens(I - 1).Value) >= Val(Tokens(I + 1).Value))
                    End Select
                End If
                Tokens(I - 1).Type = T_Command
                If Result Then Tokens(I - 1).Value = "True" Else Tokens(I - 1).Value = "False"
                For J = I + 1 To nToken Step 1: Tokens(J - 1) = Tokens(J): Next J: nToken = nToken - 2: I = I - 1

            Case T_Number, T_String, T_NewLine:

            Case Else: If Debug3 Then Print Nest; "[Invaild Token Error: "; LexValue$(Tokens(I)); "]" Else SyntaxError LineInput$, 0: Exit Function
        End Select
    Next I
    If nToken = 1 Then
        If Tokens(1).Type = T_String Then
            Interpreter$ = Chr$(34) + Tokens(1).Value + Chr$(34)
        Else
            Interpreter$ = Tokens(1).Value
            Variables(2).Value = Tokens(1).Value
        End If
    Else
        L$ = "": For I = 1 To nToken
            Select Case Tokens(I).Type
                Case T_NewLine: N$ = "; "
                Case Else: N$ = Tokens(I).Value
            End Select
            L$ = L$ + N$
        Next I: Interpreter$ = L$
    End If
End Function
Function GetElementFromList$ (text$, P)
    Dim Tokens(1) As Token, nToken As _Unsigned Integer
    text$ = Left$(Right$(text$, Len(text$) - 1), Len(Right$(text$, Len(text$) - 1)) - 1)
    nToken = 1: Tokens(1).Pos_Start = 1: V = 0
    For i = 1 To Len(text$): c$ = Mid$(text$, i, 1)
        If c$ = "[" Then nested = nested + 1
        If c$ = "]" Then nested = nested - 1
        If c$ = ":" And nested = 0 Then
            V = -1
        ElseIf c$ = "," And nested = 0 Then
            V = 0: Tokens(nToken).Pos_End = i - 1: nToken = nToken + 1: ReDim _Preserve Tokens(nToken) As Token: Tokens(nToken).Pos_Start = i
        Else
            If V Then Tokens(nToken).Value = Tokens(nToken).Value + c$ Else Tokens(nToken).Type = Tokens(nToken).Type + c$
        End If
    Next i
    GetElementFromList$ = Tokens(P).Value
End Function
Function LexValue$ (Token As Token): L$ = Token.Type: If Len(Token.Value) Then L$ = L$ + ":" + Token.Value
LexValue$ = L$: End Function
Sub SyntaxError (t$, p): L& = Erl: Print: Print "[Syntax Error,"; L&; "]": Print t$: If p = 0 Then Print String$(Len(t$), "^") Else Print Space$(p - 1); "^"
End Sub
Sub InvalidCharacterError (t$, p): L& = Erl: Print: Print "[Invalid Character Error,"; L&; "]": Print t$: If p = 0 Then Print String$(Len(t$), "^") Else Print Space$(p - 1); "^"
End Sub
Sub ExpectedError (t$, p$): L& = Erl: Print: Print "[Expected "; p$; ","; L&; "]": Print t$: End Sub
