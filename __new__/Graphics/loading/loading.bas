Screen _NewImage(64, 64, 32)
DefLng A-Z
RED~& = &HEA4335
GREEN~& = &H34A853
BLUE~& = &H4285F4
ALPHA~& = &HFBBC05
dL = 1
Const R = 20
If _DirExists("out") Then Shell _Hide "rd /s /q out"
MkDir "out"
ChDir "out"
Do
    Cls
    _Limit 60
    L = L + dL * 3
    If L = 0 Or L = 180 Then dL = -dL
    A = A + 12
    For T = A - L To A Step 1
        FilledCircle _Width / 2 + R * Cos(_D2R(T)), _Height / 2 + R * Sin(_D2R(T)), R / 10, _RGB32(255)
    Next T
    If A = 360 Then A = 0
    _Title NewN$(File)
    T = PNG0(_Title$, 0, "")
    File = File + 1
    _Display
    If L = 0 And A = 0 Then Exit Do
Loop
System
Sub FilledCircle (X, Y, R, C&)
    Circle (X, Y), R, C&
    Paint (X, Y), C&
End Sub
Function NewN$ (X)
    H$ = _Trim$(Str$(X))
    NewN$ = String$(4 - Len(H$), 48) + H$
End Function
'$Include:'files\image.bm'
