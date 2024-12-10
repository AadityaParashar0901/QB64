$Console:Only
Declare Dynamic Library "user32"
    Function FindWindowA& (ByVal ClassName As _Offset, WindowName$)
    Function GetKeyState% (ByVal nVirtKey As Long)
    Function ShowWindow& (ByVal hwnd As Long, Byval nCmdShow As Long)
    '       0 For Hide
    '       5 For Show
End Declare
DefInt A-Z
Type __Window
    Title As String
    Handle As Long
End Type
Dim Shared WH As __Window
_ConsoleTitle "Window Manager Singular"
If _FileExists("hidden.txt") Then
    Open "hidden.txt" For Input As #1
    Line Input #1, WH.Title
    Close
    Do
        _Limit 15
        WH.Handle = FindWindowA&(0, WH.Title + Chr$(0))
        If WH.Handle Then Exit Do
    Loop
    tmp = ShowWindow&(WH.Handle, 0)
Else
    Line Input WH.Title
    WH.Handle = FindWindowA&(0, WH.Title + Chr$(0))
    If WH.Handle = 0 Then WH.Title = ""
    tmp = ShowWindow&(WH.Handle, 0)
End If
Const W = 400, H = 200
Screen _NewImage(W, H, 32)
tmp = ShowWindow&(_WindowHandle, 0)
inputText$ = Space$(10)
KEYS$ = Chr$(69) + Chr$(72) + Chr$(73) + Chr$(79) + Chr$(83) + Chr$(84) + Chr$(87) + Chr$(88) + Chr$(90)
Do
    _Limit 20
    For I = 1 To Len(KEYS$)
        If GetKeyState%(Asc(KEYS$, I)) < 0 Then
            key$ = GetKey$(Asc(KEYS$, I))
            If Right$(inputText$, 1) <> key$ Then inputText$ = inputText$ + key$
            inputText$ = Right$(inputText$, 10)
        End If
    Next I
    If Right$(UCase$(inputText$), 6) = "WHSHOW" Then
        tmp = ShowWindow&(WH.Handle, 5)
    End If
    If Right$(UCase$(inputText$), 1) = "Z" Then
        tmp = ShowWindow&(WH.Handle, 0)
    End If
    If Right$(UCase$(inputText$), 6) = "WHEXIT" Then Exit Do
    _Display
Loop
If Not _FileExists("hidden.txt") Then
    Open "hidden.txt" For Output As #1
    Print #1, WH.Title
    Close
End If
Beep
_Delay 1
System
Function GetKey$ (__I)
    Select Case __I
        Case 65 To 90: GetKey$ = Chr$(__I)
    End Select
End Function
