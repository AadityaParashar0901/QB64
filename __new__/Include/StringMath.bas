$Console:Only
_ConsoleTitle "String Math"
Input A$, B$
Print A$ + "+" + B$ + "="; Add$(A$, B$)
Print A$ + "-" + B$ + "="; Subtract$(A$, B$)
End
Function Add$ (A As String, B As String)
    For __I = 0 To Max(Len(A), Len(B)) - 1
        If Len(A) > __I Then __V1 = Asc(A, Len(A) - __I) - 48 Else __V1 = 0
        If Len(B) > __I Then __V2 = Asc(B, Len(B) - __I) - 48 Else __V2 = 0
        __R$ = _Trim$(Str$(__V1 + __V2 + __C))
        __C = -Asc(__R$, 1) * (Len(__R$) > 1)
        __V$ = Right$(__R$, 1) + __V$
    Next __I
    Add$ = __V$
End Function
Function Max (A, B)
    If A > B Then Max = A Else Max = B
End Function
Function Subtract$ (A As String, B As String)
End Function
