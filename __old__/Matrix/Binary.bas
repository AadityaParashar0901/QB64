$Console:Only
_Title "Binary"
Randomize Timer
Print "Usage: BINARY [Any Text]"
Print "Creating Image..."
IMG& = _NewImage(_DesktopWidth, _DesktopHeight, 32)
_Dest IMG&
_Font 8
W& = _Width(IMG&) \ _FontWidth
H& = _Height(IMG&) \ _FontHeight
Dim X As _Unsigned Integer
Dim Shared RANDOMTEXT$
RANDOMTEXT$ = Command$(2)
If RANDOMTEXT$ = "" Then
    RANDOMTEXT$ = "01"
End If
Cls , _RGB32(32)
_PrintMode _KeepBackground
Do
    _Limit W& * H&
    Luminance = Rnd
    Color _RGB32(0, Luminance * 64 + 127, Luminance * 128 + 127)
    If 0 Then
        _PrintString ((X Mod W&) * _FontWidth, (X \ W&) * _FontHeight), R$
    Else
        _PrintString ((X Mod W&) * _FontWidth, (X \ W&) * _FontHeight), Chr$(254)
    End If
    X = X + 1
    If X = W& * (H& + 1) + 1 Then Exit Do
Loop Until Inp(&H60) = 1
_Font 16
W& = _Width(IMG&) \ _FontWidth
H& = _Height(IMG&) \ _FontHeight
Color _RGB32(0, 127, 255), _RGB32(32)
TEXT$ = Command$(1)
If Len(TEXT$) Then
    Line ((W& * _FontWidth - _FontWidth * (Len(TEXT$) + 1)) / 2, (H& * _FontHeight - _FontHeight * 2) / 2)-((W& * _FontWidth + _FontWidth * (Len(TEXT$) + 1)) / 2 - 1, (H& * _FontHeight + _FontHeight * 2) / 2 - 1), _RGB32(32), BF
    _PrintString ((W& * _FontWidth - _FontWidth * Len(TEXT$)) / 2, (H& * _FontHeight - _FontHeight) / 2), TEXT$
End If
_Dest _Console
Print "Saving as:"; Left$(Date$, 2) + Mid$(Date$, 4, 2) + Right$(Date$, 4) + Left$(Time$, 2) + Mid$(Time$, 4, 2) + Right$(Time$, 2)
T = PNG(Left$(Date$, 2) + Mid$(Date$, 4, 2) + Right$(Date$, 4) + Left$(Time$, 2) + Mid$(Time$, 4, 2) + Right$(Time$, 2), IMG&, "")
Print "Saved!"
System
'$Include:'files\image.bm'
Function R$
    R$ = Chr$(Asc(RANDOMTEXT$, Int(Rnd * Len(RANDOMTEXT$)) + 1))
End Function
