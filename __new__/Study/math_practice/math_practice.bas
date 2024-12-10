$Console:Only
$ExeIcon:'.\256.ico'
DefLng A-Z
Type Operation
    TotalTime As Double
    Correct As _Unsigned Long
    Wrong As _Unsigned Long
End Type
Dim OperationList As String
OperationList = "+-*\"
Dim OperationData(1 To 4) As Operation
If _CommandCount Then OperationList = Command$(1)
Dim ACount, SCount, MCount, DCount
Randomize Timer
KEYLIST$ = MKI$(&H520B) + MKI$(&H4F02) + MKI$(&H5003) + MKI$(&H5104) + MKI$(&H4B05) + MKI$(&H4C06) + MKI$(&H4D07) + MKI$(&H4708) + MKI$(&H4809) + MKI$(&H490A)

Do
    Operation = Int(Rnd * Len(OperationList)) + 1
    InputNumber = 0
    Select Case Mid$(OperationList, Operation, 1)
        Case "+": Num1 = Int(Rnd * 1000) + 1
            Num2 = Int(Rnd * 1000) + 1
            Ans = Num1 + Num2
            Print Num1; "+"; Num2; "= ";
            CountMode = 1
        Case "-": Num1 = Int(Rnd * 1000) + 1
            Num2 = Int(Rnd * Num1) + 1
            Ans = Num1 - Num2
            Print Num1; "-"; Num2; "= ";
            CountMode = 2
        Case "*": Num1 = Int(Rnd * 100) + 1
            Num2 = Int(Rnd * 100) + 1
            Ans = Num1 * Num2
            Print Num1; "x"; Num2; "= ";
            CountMode = 3
        Case "/", "\": Num1 = Int(Rnd * 1000) + 1
            Num2 = Int(Rnd * Num1 / 2) + 1
            Ans = Num1 \ Num2
            Print Num1; "\"; Num2; "= ";
            CountMode = 4
    End Select
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
    OperationData(CountMode).TotalTime = OperationData(CountMode).TotalTime + T!
    If InputNumber = Ans Then
        OperationData(CountMode).Correct = OperationData(CountMode).Correct + 1
        Locate Y, 1
        Print T!; Space$(16)
    Else
        OperationData(CountMode).Wrong = OperationData(CountMode).Wrong + 1
        Print Ans; T!
    End If
Loop
10
Print
Print String$(_Width, Chr$(219))
For I = 1 To Len(OperationList)
    Select Case Mid$(OperationList, I, 1)
        Case "+":
            MODE$ = "Addition"
            CountMode = 1
        Case "-":
            MODE$ = "Subtraction"
            CountMode = 2
        Case "*":
            MODE$ = "Multiplication"
            CountMode = 3
        Case "/", "\":
            MODE$ = "Division"
            CountMode = 4
    End Select
    Print MODE$ + " Average Time:"; OperationData(CountMode).TotalTime / (OperationData(CountMode).Correct + OperationData(CountMode).Wrong)
    Print MODE$ + " Correct Answers:"; OperationData(CountMode).Correct
    Print MODE$ + " Wrong Answers:"; OperationData(CountMode).Wrong
    Print MODE$ + " Accuracy:"; 100 * OperationData(CountMode).Correct / (OperationData(CountMode).Correct + OperationData(CountMode).Wrong); "%"
Next I
_Delay 1
End
