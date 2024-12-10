$Console:Only
_ConsoleTitle "Factorial"
Do
    Input N, R
    Print "F(" + _Trim$(Str$(N)) + "):"; Factorial(N)
    Print "F(" + _Trim$(Str$(R)) + "):"; Factorial(R)
    Print "C("; _Trim$(Str$(N)) + "," + _Trim$(Str$(R)) + "):"; Combination(N, R)
    Print "P("; _Trim$(Str$(N)) + "," + _Trim$(Str$(R)) + "):"; Factorial(R) * Combination(N, R)
Loop
Function Factorial~&& (N As Integer)
    F~&& = 1
    For I = 1 To N
        F~&& = F~&& * I
    Next I
    Factorial~&& = F~&&
End Function
Function Combination~&& (N As Integer, R As Integer)
    Combination~&& = Factorial(N) / Factorial(N - R) / Factorial(R)
End Function
