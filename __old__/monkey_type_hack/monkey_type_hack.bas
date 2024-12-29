$Console:Only
Dim K$1(0 To 9, 0 To 3)
For Y = 0 To 3
    For X = 0 To 9
        Read K$1(X, Y)
Next X, Y
YERROR = 5
XERROR = 0
Do
    T& = _ScreenImage(866 + XERROR, 612 + YERROR, 1046 + XERROR, 680 + YERROR)
    If CLIPPED = 0 Then
        _Delay 1
        _ClipboardImage = _ScreenImage(866 + XERROR, 612 + YERROR, 1046 + XERROR, 680 + YERROR)
        CLIPPED = -1
    End If
    _Source T&
    For X = 0 To 9
        For Y = 0 To 3
            P& = Point(X * 18 + 3, Y * 18 + 3)
            If _Red32(P&) + _Green32(P&) + _Blue32(P&) > 127 Then
                If Asc(K$1(X, Y)) Then
                    TIME = 0
                    If LK$1 = K$1(X, Y) Then
                        LK$1 = Chr$(0)
                        _Delay 0.05
                    Else
                        LK$1 = K$1(X, Y)
                        Print LK$1;
                        _ScreenPrint LK$1
                    End If
                    GoTo 1
                End If
            End If
    Next Y, X
    If TIME = 0 Then TIME = Timer
    If Timer - TIME >= 10 Then _ScreenPrint Chr$(13): TIME = 0: Print ""
    1
    _Source _Console
    _FreeImage T&
    _Delay 0.02
Loop Until Inp(&H60) = 1
System
Data "q","w","e","r","t","y","u","i","o","p"
Data "a","s","d","f","g","h","j","k","l",";"
Data "z","x","c","v","b","n","m",",",".","/"
Data " "," "," "," "," "," "," "," "," "," "
