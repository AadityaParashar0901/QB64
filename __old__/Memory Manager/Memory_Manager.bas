$Console:Only
Declare Dynamic Library "user32"
    Function ShowWindow& (ByVal hwnd As Long, Byval nCmdShow As Long)
End Declare
_ConsoleTitle "Memory Manager"
tmp = ShowWindow&(_WindowHandle, 0)
Shell _Hide "rammap -Ew"
_Delay 5
Do
    _Limit 5
    If _FileExists(".running") Then
    Else
        Open ".running" For Output As #1
        Close #1
    End If
    If _FileExists(".close") Then Exit Do
    If Timer - m! >= 30 Then
        m! = Timer
        If _FileExists(".pause") Then _Continue
        Shell "systeminfo | find " + Chr$(34) + "Available Physical Memory" + Chr$(34) + " > " + _CWD$ + "\tmp_systeminfo.txt"
        Open "tmp_systeminfo.txt" For Input As #1
        Line Input #1, A$
        Close #1
        A$ = _Trim$(Mid$(A$, 28))
        If InStr(A$, ",") Then A$ = Left$(A$, InStr(A$, ",") - 1) + Mid$(A$, InStr(A$, ",") + 1)
        FREEMEMORY = Val(A$)
        Kill "tmp_systeminfo.txt"
        Open "mm.inf" For Input As #1
        Line Input #1, A$
        Line Input #1, A$
        Close #1
        THRESHOLDMEMORY = Val(A$)
        If FREEMEMORY < THRESHOLDMEMORY Then Shell _Hide "rammap -Ew"
    End If
Loop
If _FileExists(".running") Then Kill ".running"
If _FileExists(".close") Then Kill ".close"
System
