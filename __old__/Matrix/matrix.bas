Randomize Timer
SW = _DesktopWidth
SH = _DesktopHeight
Screen _NewImage(SW, SH, 32)
_FullScreen _SquarePixels
_Title "Matrix"
Type T
    O As Integer
    M As String * 62
    L As Integer
End Type
Dim As Long W, H
FONTWIDTH = _FontWidth
FONTHEIGHT = _FontHeight
W = _Width \ FONTWIDTH
H = _Height \ FONTHEIGHT
Dim C(1 To W) As T
For I = 1 To UBound(C)
    C(I).O = Int(Rnd * H)
    C(I).L = 2 * Int(Rnd * H) - H
    For J = 1 To H
        Mid$(C(I).M, J, 1) = R$
    Next J
Next I
_PrintMode _KeepBackground
_MouseHide
Do
    _Limit 16
    If _Exit Then _Continue
    Cls
    For J = 1 To UBound(C)
        For I = 1 To C(J).L
            If I < C(J).L Then Color _RGB32(0, 255, 0) Else Color _RGB32(191, 255, 191)
            If Command$(1) = "" Then _PrintString ((J - 1) * FONTWIDTH, (I - 1 + C(J).O) * FONTHEIGHT), Mid$(C(J).M, I + C(J).O, 1) Else PrintString (J - 1) * FONTWIDTH, (I - 1 + C(J).O) * FONTHEIGHT, Mid$(C(J).M, I + C(J).O, 1), _DefaultColor
            If Rnd < 0.05 Then Mid$(C(J).M, 1 + Int(Rnd * C(J).L), 1) = R$
        Next I
        C(J).O = C(J).O + 1
        If C(J).O > H Then
            C(J).L = Int(Rnd * H)
            C(J).O = -Int(Rnd * H) - C(J).L
            For K = 1 To H
                Mid$(C(J).M, K, 1) = R$
            Next K
        End If
    Next J
    _Display
Loop Until Inp(&H60) = 1
_MouseShow
System
Function R$
    R$ = Chr$(Int(Rnd * 190) + 65)
End Function
Sub PrintString (__X, __Y, __S$, __FG&)
    FONT& = load_font&
    For I = 0 To Len(__S$) - 1
        __C = Asc(__S$, I + 1)
        __I = 8 * (__C Mod 16)
        __J = 8 * (__C \ 16)
        _PutImage (__X + I * FONTWIDTH, __Y)-(__X + FONTWIDTH - 1 + I * FONTWIDTH, __Y + FONTHEIGHT), FONT&, , (__I, __J)-(__I + 7, __J + 7)
        If __FG& <> _RGB32(255) Then
            For __X2 = __X + I * FONTWIDTH To __X + FONTWIDTH - 1 + I * FONTWIDTH
                For __Y2 = __Y To __Y + FONTHEIGHT
                    If Point(__X2, __Y2) = _RGB32(255) Then PSet (__X2, __Y2), FG
            Next __Y2, __X2
        End If
    Next I
End Sub
'$Include:'font.bm'
