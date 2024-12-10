$Console:Only
Dim Shared DL$
DL$ = ListAdd$(ListNew$, _StartDir$)
Do
    If ListLength~&(DL$) > 0 Then del_r ListGet$(DL$, 1) Else Exit Do
    DL$ = ListDelete$(DL$, 1)
Loop
Sub del_r (PATH$)
    HOME$ = _CWD$
    Print "PATH$:"; PATH$
    Shell "dir /b " + PATH$ + "\* > " + PATH$ + "\tmp.txt"
    __F = FreeFile
    Open PATH$ + "\tmp.txt" For Input As #__F
    ChDir PATH$
    Y = CsrLin
    Do
        Line Input #__F, L$
        If L$ = "" Or EOF(1) Then Exit Do
        If L$ = "del_r.exe" Then _Continue
        If L$ = "tmp.txt" Then _Continue
        If _DirExists(L$) Then DL$ = ListAdd$(DL$, PATH$ + "\" + L$)
        If _FileExists(L$) Then
            Locate Y, 1: Print "Wiping File:" + L$
            __F2 = FreeFile
            Open L$ For Binary As #__F2
            A$ = String$(LOF(__F2), 0)
            Put #__F2, , A$
            Close #__F2
        End If
        If EOF(__F) Then Exit Do
    Loop
    Close #__F
    ChDir HOME$
End Sub
'$Include:'files\QBListDict.bas'
