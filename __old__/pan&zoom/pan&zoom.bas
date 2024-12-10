Screen _NewImage(640, 480, 32)
_Define F As DOUBLE
_Define N As INTEGER
_Define L As LONG
IMG& = _LoadImage("a.png", 32)
Dim Shared fOffsetX, fOffsetY, fScale
fOffsetX = 0 ' - _Width / 2
fOffsetY = 0 ' - _Height / 2
fScale = 1
Do
    Cls , _RGB32(15)
    _Limit 60
    fLastMouseW = 0
    While _MouseInput
        fMouseX = _MouseX
        fMouseY = _MouseY
        fMouseB = _MouseButton(1)
        fMouseW = _MouseWheel
        If fMouseW <> 0 Then fLastMouseW = fMouseW
    Wend
    If fMouseB And fLastMouseB = 0 Then
        fStartPanX = fMouseX
        fStartPanY = fMouseY
    End If
    fLastMouseB = fMouseB
    If fMouseB Then
        fOffsetX = fOffsetX - (fMouseX - fStartPanX) / fScale
        fOffsetY = fOffsetY - (fMouseY - fStartPanY) / fScale
        fStartPanX = fMouseX
        fStartPanY = fMouseY
    End If
    fMWX_BZ = 0: fMWY_BZ = 0
    S2W fMouseX, fMouseY, fMWX_BZ, fMWY_BZ
    fScale = fScale * (1 - fLastMouseW / 25)
    fMWX_AZ = 0: fMWY_AZ = 0
    S2W fMouseX, fMouseY, fMWX_AZ, fMWY_AZ
    fOffsetX = fOffsetX + fMWX_BZ - fMWX_AZ
    fOffsetY = fOffsetY + fMWY_BZ - fMWY_AZ
    Dim As Integer X1, Y1, X3, Y3
    X1 = 0: Y1 = 0: X3 = 0: Y3 = 0
    W2S 0, 0, X1, Y1
    W2S _Width(IMG&), _Height(IMG&), X3, Y3
    Print X3, Y3
    _PutImage (X1, Y1)-(X3, Y3), IMG&
    _Display
Loop Until Inp(&H60) = 1
System
Sub W2S (fWX As Double, fWY As Double, nSX As Integer, nSY As Integer)
    nSX = (fWX - fOffsetX) * fScale
    nSY = (fWY - fOffsetY) * fScale
End Sub
Sub S2W (nSX As Integer, nSY As Integer, fWX As Double, fWY As Double)
    fWX = nSX / fScale + fOffsetX
    fWY = nSY / fScale + fOffsetY
End Sub
