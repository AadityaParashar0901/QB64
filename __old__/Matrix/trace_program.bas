$Resize:On
Randomize Timer
Screen _NewImage(640, 480, 32)
DefLng A-Z
_PrintMode _KeepBackground
Color _RGB32(0, 255, 0), _RGB32(0)
T$ = Space$(9)
Do
    _Limit 16
    If _Resize Then
        Screen _NewImage(_ResizeWidth, _ResizeHeight, 32)
        Cls
        _PrintMode _KeepBackground
        Color _RGB32(0, 255, 0), _RGB32(0)
    End If
    Line (0, 0)-(_Width - 1, _Height - 1), _RGBA32(0, 0, 0, 191), BF
    K = 0
    For I = 0 To _Width \ _FontWidth \ 10 - 1
        For J = 1 To _Height \ _FontHeight - 1
            _PrintString (_FontWidth / 2 + I * _FontWidth * 10, J * _FontHeight), N$
            K = K + 1
    Next J, I
    _PrintString (_FontWidth / 2, 0), T$

    _Display

    If InStr(T$, " ") Then
        X = X + 1
    Else
        _Delay 1
        For J = 0 To _Height \ _FontHeight - 1
            _Limit 40
            Line (0, J * _FontHeight)-(_Width, J * _FontHeight + _FontHeight), _RGB32(0), BF
            _Display
        Next J
        Cls
        T$ = Space$(9)
    End If
    If X = 16 Then
        X = 0
        If K < 160 Then K = 160
        For J = 1 To K \ 160
            I = Int(Rnd * 9) + 1
            If Mid$(T$, I, 1) = " " Then Mid$(T$, I, 1) = _Trim$(Str$(CInt(Rnd * 9))): Exit For
        Next J
    End If
Loop Until Inp(&H60) = 41
System
Function N$
    For I = 1 To 9
        A$ = A$ + _Trim$(Str$(CInt(Rnd * 9)))
    Next I
    N$ = A$
End Function
