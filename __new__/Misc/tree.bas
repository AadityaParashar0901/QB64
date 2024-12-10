$Console:Only
Print "."
tree_r _CWD$, 0, 0, ""
Sub tree_r (PATH$, LEVEL As _Unsigned Integer, MORE As _Byte, S$)
    HOME$ = _CWD$
    Shell "dir /A:D /B /O:N " + Chr$(34) + PATH$ + "\*" + Chr$(34) + " > " + Chr$(34) + PATH$ + "\tmp.txt" + Chr$(34)
    __F = FreeFile
    Open PATH$ + "\tmp.txt" For Input As #__F
    If LOF(__F) Then
        ChDir PATH$
        If MORE Then __S$ = S$ + Chr$(179) + Space$(3)
        Do
            Line Input #__F, L$
            If L$ = "" And EOF(1) Then Exit Do
            Print __S$ + Chr$(195 + 3 * EOF(__F)) + String$(3, 196) + L$
            tree_r PATH$ + "\" + L$, LEVEL + 1, 1 + EOF(__F), __S$
            If EOF(__F) Then Exit Do
        Loop
    Else
        Close #__F
    End If
    Close #__F
    'Kill "tmp.txt"
    ChDir HOME$
End Sub
