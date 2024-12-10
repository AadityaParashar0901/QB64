$Debug
'$Dynamic
$Console:Only
Const Tokens = "E D N H S C + - * / \ ^ ' ` ~ # & _ ! | && ( ) , . ? : ; < = > =>:="
Dim Shared DEBUG: DEBUG = 1
5 Do
    On Error GoTo ErrorHandler
    If DEBUG Then Line Input "D>"; L$ Else Line Input " >"; L$
    If UCase$(L$) = "DEBUG" Then DEBUG = Not DEBUG: _Continue
    If UCase$(L$) = "CLS" Then Cls: _Continue
    T! = Timer(0.001)
    Lex$ = Lexer$(L$)
    If DEBUG Then Print "L:";: DictPrint Lex$
    Par$ = Parser$(Lex$)
    If DEBUG Then Print "P:";: DictPrint Par$
    Itr$ = Interpreter$(Par$)
    Print "I:";: DictPrint Itr$
    Print Timer(0.001) - T!
Loop
System
ErrorHandler:
Print "[Error:"; _ErrorLine; "]"
On Error GoTo ErrorHandler
GoTo 5
Function Lexer$ (__L$)
    __Dict$ = DictNew$
    If Len(__L$) = 0 Then Lexer$ = __Dict$: Exit Function
    For __I& = 1 To Len(__L$)
        __C~%% = Asc(__L$, __I&)
        C$ = Chr$(__C~%%)
        Select Case __C~%%
            Case 32
                T$ = "E"
                _Continue
            Case 33, 35, 38, 39, 42, 43, 44, 45, 47, 58, 59, 60, 61, 62, 63, 91, 92, 93, 94, 95, 96, 123, 124, 125, 126
                If __C~%% = 44 Then
                    T$ = "C"
                Else
                    T$ = Chr$(__C~%%)
                End If
                __Dict$ = DictAdd$(__Dict$, T$, "")
            Case 40
                T$ = "D"
                nested = 1
                V$ = ""
                For __J& = __I& + 1 To Len(__L$)
                    __C~%% = Asc(__L$, __J&)
                    C$ = Chr$(__C~%%)
                    Select Case __C~%%
                        Case 40: nested = nested + 1
                        Case 41: nested = nested - 1
                            If nested = 0 Then Exit For
                    End Select
                    V$ = V$ + C$
                Next __J&
                __I& = __J&
                __Dict$ = DictAdd$(__Dict$, T$, Lexer$(V$))
            Case 46, 48 To 57 '0-9
                If T$ = "N" Then
                    V$ = DictGetValue$(__Dict$, DictLength(__Dict$))
                    __Dict$ = DictDelete$(__Dict$, DictLength(__Dict$))
                    __Dict$ = DictAdd$(__Dict$, "N", V$ + C$)
                Else
                    If __C~%% = 46 Then
                        T$ = "."
                        __Dict$ = DictAdd$(__Dict$, T$, "")
                    Else
                        __Dict$ = DictAdd$(__Dict$, "N", C$)
                        T$ = "N"
                    End If
                End If
            Case 65 To 90, 97 To 122: 'A-Z, a-z
                If T$ = "S" Then
                    V$ = DictGetValue$(__Dict$, DictLength(__Dict$))
                    __Dict$ = DictDelete$(__Dict$, DictLength(__Dict$))
                    __Dict$ = DictAdd$(__Dict$, "S", V$ + C$)
                Else
                    __Dict$ = DictAdd$(__Dict$, "S", C$)
                    T$ = "S"
                End If
            Case Else:
                InvalidCharError __L$, __I&
                Exit Function
        End Select
    Next __I&
    __L& = DictLength(__Dict$)
    For __I& = 1 To __L&
        If __I& < __L& Then
            If __I& > 1 Then
                If DictGetKey$(__Dict$, __I& - 1) <> "N" And DictGetKey$(__Dict$, __I&) = "-" And DictGetKey$(__Dict$, __I& + 1) = "N" Then
                    __Dict$ = DictEdit$(__Dict$, __I, "N", _Trim$(Str$(-Val(DictGetValue$(__Dict$, __I& + 1)))))
                    __Dict$ = DictDelete$(__Dict$, __I + 1)
                End If
            Else
                If DictGetKey$(__Dict$, __I&) = "-" And DictGetKey$(__Dict$, __I& + 1) = "N" Then
                    __Dict$ = DictEdit$(__Dict$, __I, "N", _Trim$(Str$(-Val(DictGetValue$(__Dict$, __I& + 1)))))
                    __Dict$ = DictDelete$(__Dict$, __I + 1)
                End If
            End If
        End If
    Next __I&
    Lexer$ = __Dict$
End Function
Function Parser$ (__D$)
    For __C = 1 To 11
        For __I = 1 To DictLength(__D$)
            K$ = DictGetKey$(__D$, __I)
            V$ = DictGetValue$(__D$, __I)
            If K$ = Mid$("/\*+-~=<>&| ", __C, 1) Then
                If K$ = "~" Then
                    K$ = "N"
                    If Val(DictGetValue$(__D$, __I + 1)) Then V$ = "" Else V$ = DictGetValue$(__D$, __I + 1)
                    __D$ = DictEdit$(__D$, __I - 1, K$, V$)
                    __D$ = DictDelete$(__D$, __I)
                    __I = __I - 1
                ElseIf K$ = "-" Then
                    __TMP_VAR = 0
                    If __I > 1 Then
                        If DictGetKey$(__D$, __I - 1) = "N" Then
                            V$ = DictAddElement$(DictNew$, Parser$(DictGetElementRaw$(__D$, __I - 1)))
                            V$ = DictAppend$(V$, DictGetElement$(__D$, __I))
                            V$ = DictAppend$(V$, Parser$(DictGetElementRaw$(__D$, __I + 1)))
                            K$ = "D"
                            __D$ = DictEdit$(__D$, __I - 1, K$, V$)
                            __D$ = DictDelete$(__D$, __I)
                            __D$ = DictDelete$(__D$, __I)
                            __I = __I - 1
                            __TMP_VAR = 1
                        End If
                    End If
                    If __TMP_VAR = 0 Then
                        V$ = DictAddElement$(DictNew$, DictGetElement$(__D$, __I))
                        V$ = DictAppend$(V$, Parser$(DictGetElementRaw$(__D$, __I + 1)))
                        K$ = "D"
                        __D$ = DictEdit$(__D$, __I, K$, V$)
                        __D$ = DictDelete$(__D$, __I + 1)
                    End If
                Else
                    V$ = DictAddElement$(DictNew$, Parser$(DictGetElementRaw$(__D$, __I - 1)))
                    V$ = DictAppend$(V$, DictGetElementRaw$(__D$, __I))
                    V$ = DictAppend$(V$, Parser$(DictGetElementRaw$(__D$, __I + 1)))
                    K$ = "D"
                    __D$ = DictEdit$(__D$, __I - 1, K$, V$)
                    __D$ = DictDelete$(__D$, __I)
                    __D$ = DictDelete$(__D$, __I)
                    __I = __I - 1
                End If
            End If
        Next __I
    Next __C
    Parser$ = __D$
End Function
Function Interpreter$ (__D$)
    __DL = DictLength(__D$)
    For __I = 1 To __DL
        K$ = DictGetKey$(__D$, __I)
        V$ = DictGetValue$(__D$, __I)
        Select Case K$
            Case "D":
                __I$ = Interpreter$(V$)
                __I$ = Mid$(__I$, 2, Len(__I$) - 2)
                __D$ = DictSetElement$(__D$, __I, __I$)
            Case "+", "-", "*", "/", "\", "=", "<", ">", "&", "|":
                __I1$ = Interpreter$(DictNewRaw$("{" + DictGetElement$(__D$, __I - 1) + "}"))
                __I2$ = Interpreter$(DictNewRaw$("{" + DictGetElement$(__D$, __I + 1) + "}"))
                __I1K$ = DictGetKey$(__I1$, 1)
                __I1V$ = DictGetValue$(__I1$, 1)
                __I2K$ = DictGetKey$(__I2$, 1)
                __I2V$ = DictGetValue$(__I2$, 1)
                If __I1K$ = "N" And __I2K$ = "N" Then
                    Select Case K$
                        Case "+": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) + Val(__I2V$))))
                        Case "-": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) - Val(__I2V$))))
                        Case "*": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) * Val(__I2V$))))
                        Case "/": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) / Val(__I2V$))))
                        Case "\": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) \ Val(__I2V$))))
                        Case "=": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) = Val(__I2V$))))
                        Case "<": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) < Val(__I2V$))))
                        Case ">": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) > Val(__I2V$))))
                        Case "&": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) And Val(__I2V$))))
                        Case "|": __D$ = DictEdit$(__D$, __I - 1, "N", _Trim$(Str$(Val(__I1V$) Or Val(__I2V$))))
                    End Select
                    __D$ = DictDelete$(__D$, __I): __D$ = DictDelete$(__D$, __I)
                    __I = __I - 1
                Else
                    If K$ = "-" Then
                        __I2$ = Interpreter$(DictNewRaw$("{" + DictGetElement$(__D$, __I + 1) + "}"))
                        __I2K$ = DictGetKey$(__I2$, 1)
                        __I2V$ = DictGetValue$(__I2$, 1)
                        If __I2K$ = "N" Then
                            __D$ = DictEdit$(__D$, __I, "N", _Trim$(Str$(-Val(__I2V$))))
                            __D$ = DictDelete$(__D$, __I + 1): __D$ = DictDelete$(__D$, __I + 1)
                        End If
                    Else
                        InvalidOperationError __D$, ""
                    End If
                End If
            Case "N", "S":
        End Select
        If DEBUG Then Print "__>"; __D$
        __DL = DictLength(__D$)
    Next __I
    Interpreter$ = __D$
End Function
'$Include:'files\QBListDict.bas'
Function DictGetElementRaw$ (__Dict As String, __ItemNumber As _Unsigned Long)
    DictGetElementRaw$ = DictNewRaw$("{" + DictGetElement$(__Dict, __ItemNumber) + "}")
End Function
'
Sub InvalidCharError (__Line$, __P)
    If __P = 0 Then
        Print "  " + String$(Len(__Line$), "^")
    Else
        Print String$(__P + 1, 32) + "^"
    End If
    Print "[Invalid Character Error]"
End Sub
Sub InvalidOperationError (__Line$, __P$)
    Print __Line$
    Print __P$
    Print "[Invalid Operation Error]"
End Sub
