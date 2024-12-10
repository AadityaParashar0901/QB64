Randomize Timer
Dim Generation As _Unsigned Long
W = 256
H = 256
Screen _NewImage(W * 2, H * 2, 256)
Window (1, 1)-(W, H)
Dim Universe(1 To W, 1 To H) As _Byte
Dim Universe2(1 To W, 1 To H) As _Byte
Dim NC As _Unsigned _Byte
Dim As Integer X, Y, XX, YY
If _FileExists(Command$(1)) Then
    LUniverse& = _LoadImage(Command$(1))
    _Source LUniverse&
    For X = 1 To W: For Y = 1 To H
            If Point(X - 1, Y - 1) = _RGB32(255) Then Universe(X, Y) = -1
    Next Y, X
    _FreeImage LUniverse&
Else
    For X = 1 To W: For Y = 1 To H
            If Rnd > 0.75 Then Universe(X, Y) = -1
    Next Y, X
End If
On Timer(1) GoSub one_second
Timer On
Dim As _MEM M1, M2
M1 = _Mem(Universe())
M2 = _Mem(Universe2())
LIMIT = 30
Do
    _Limit LIMIT

    For X = 1 To W: For Y = 1 To H
            NC = 0
            If Universe(X, Y) Then
                For XX = X - 1 To X + 1: For YY = Y - 1 To Y + 1
                        If XX = X And YY = Y Then _Continue
                        If XX > 1 And XX < W And YY > 1 And YY < H Then
                            If Universe(XX, YY) Then NC = NC + 1
                        End If
                Next YY, XX
                If NC < 2 Then Universe2(X, Y) = 0
                If NC > 3 Then Universe2(X, Y) = 0
            Else
                For XX = X - 1 To X + 1: For YY = Y - 1 To Y + 1
                        If XX = X And YY = Y Then _Continue
                        If XX > 1 And XX < W And YY > 1 And YY < H Then
                            If Universe(XX, YY) Then NC = NC + 1
                        End If
                Next YY, XX
                If NC = 3 Then Universe2(X, Y) = -1
            End If
    Next Y, X

    _MemCopy M2, M2.OFFSET, M2.SIZE To M1, M1.OFFSET

    Generation = Generation + 1

    Cls
    _Title "Gen - " + _Trim$(Str$(Generation)) + "," + _Trim$(Str$(FPS&))
    For X = 1 To W: For Y = 1 To H
            If Universe(X, Y) Then PSet (X, Y), 15
    Next Y, X
    _Display
Loop Until Inp(&H60) = 1
System
one_second:
FPS& = Generation - LastGen
LastGen = Generation
_Title "Gen - " + _Trim$(Str$(Generation)) + " " + _Trim$(Str$(FPS&))
Return
