$Console:Only
Dim K$1(0 To 9, 0 To 3)
For Y = 0 To 3
    For X = 0 To 9
        Read K$1(X, Y)
Next X, Y
Do
    T& = _ScreenImage(866, 612, 1046, 680)
    _Source T&
    For X = 0 To 9
        For Y = 0 To 3
            If _Green32(Point(X * 18 + 3, Y * 18 + 3)) = 255 Then
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
