$Console:Only
Declare Dynamic Library "user32"
    Function ShowWindow& (ByVal hwnd As Long, Byval nCmdShow As Long)
End Declare
_ConsoleTitle "Code Time"
DefLng A-Z
tmp = ShowWindow&(_WindowHandle, 0)

FILE$ = Left$(Date$, 2) + Mid$(Date$, 4, 2) + Right$(Date$, 4) + Left$(Time$, 2) + Mid$(Time$, 4, 2) + Right$(Time$, 2)
If _DirExists("history") = 0 Then MkDir "history"
Open "history\" + FILE$ + ".txt" For Output As #1
Do
    _Limit 1
    If _FileExists("CLOSE.txt") Then
        Name "CLOSE.txt" As "CLOSED.txt"
        System
    End If
    For I = 1 To 2
        APPNAME$ = Mid$("Code.exeqb64.exe", I * 8 - 7, 8)
        Shell "tasklist /fi " + Chr$(34) + "imagename eq " + APPNAME$ + Chr$(34) + " > tmp.txt"
        Open "tmp.txt" For Binary As #2
        T$ = String$(LOF(2), 0)
        Get #2, , T$
        Close #2
        If InStr(T$, "INFO: No tasks are running which match the specified criteria.") Then APPSTARTLIST(I) = 0 Else APPSTARTLIST(I) = -1
        If APPSTARTLIST(I) <> LASTAPPSTARTLIST(I) Then If APPSTARTLIST(I) Then Print #1, "STARTTIME:" + APPNAME$ + " " + Time$ Else Print #1, "ENDTIME:" + APPNAME$ + " " + Time$
        LASTAPPSTARTLIST(I) = APPSTARTLIST(I)
    Next I
Loop
