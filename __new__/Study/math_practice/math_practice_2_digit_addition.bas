$Console:Only
$ExeIcon:'.\256.ico'
DefLng A-Z
Randomize Timer
KEYLIST$ = MKI$(&H520B) + MKI$(&H4F02) + MKI$(&H5003) + MKI$(&H5104) + MKI$(&H4B05) + MKI$(&H4C06) + MKI$(&H4D07) + MKI$(&H4708) + MKI$(&H4809) + MKI$(&H490A)

Do
    Operation = Int(Rnd * Len(OperationList)) + 1
    InputNumber = 0
    Num1 = Int(Rnd * 100) + 1
    Num2 = Int(Rnd * 100) + 1
    Ans = Num1 + Num2
    Print Num1; "+"; Num2; "= ";
    T! = Timer(0.01)
    Do
        _Limit 60
        T = _ConsoleInput
        K = _CInp
        If K = 28 Then
            If InputNumber Then
                Exit Do
            Else
                GoTo 10
            End If
        End If
        If K > 0 And K <> 14 Then
            N = InStr(KEYLIST$, Chr$(K))
            If N Then
                N = N \ 2 + Sgn(K > 11)
                Print _Trim$(Str$(N));
                X = Pos(0)
                InputNumber = InputNumber * 10 + N
            End If
            If InputNumber = Ans Then Exit Do
            If Len(_Trim$(Str$(InputNumber))) = Len(_Trim$(Str$(Ans))) Then Exit Do
        ElseIf K = 14 Then
            InputNumber = InputNumber \ 10
            X = X - 1
            Locate CsrLin, X
            Print " ";
            Locate CsrLin, X
        End If
    Loop
    T! = Timer(0.01) - T!
    TotalTime = TotalTime + T!
    If InputNumber = Ans Then
        Correct = Correct + 1
        Locate Y, 1
        Print T!; Space$(16)
    Else
        Wrong = Wrong + 1
        Print Ans; T!
    End If
Loop
10
Print
Print String$(_Width, Chr$(219))
Print "Average Time:"; TotalTime / (Correct + Wrong)
Print "Correct Answers:"; Correct
Print "Wrong Answers:"; Wrong
Print "Accuracy:"; 100 * Correct / (Correct + Wrong); "%"
_Delay 1
End
