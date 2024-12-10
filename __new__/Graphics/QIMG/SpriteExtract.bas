$Console:Only
DefLng A-Z
T = _LoadImage(Command$(1), 32)
SIZE = 32
If _CommandCount = 2 Then
    SIZE = Val(Command$(2))
    W = _Width(T) \ SIZE
    H = _Height(T) \ SIZE
ElseIf _CommandCount = 3 Then
    W = Val(Command$(2))
    H = Val(Command$(3))
End If
If _DirExists("out") Then Shell "rd /s /q out"
MkDir "out"
ChDir "out"
For I = 1 To _Width(T) \ W
    For J = 1 To _Height(T) \ H
        N = _NewImage(W, H, 32)
        _PutImage (0, 0)-(W - 1, H - 1), T, N, ((I - 1) * W, (J - 1) * H)-(I * W - 1, J * H - 1)
        L = PNG32(_Trim$(Str$(J) + Str$(I)), N, "")
        _FreeImage N
Next J, I
_FreeImage T
System
'$include:'files\image.bm'
