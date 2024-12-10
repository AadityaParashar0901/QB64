Screen _NewImage(640, 480, 32)
W = _Width / 16
H = _Height / 16
Dim S(0 To W - 1, 0 To H - 1) As _Unsigned _Byte
For I = 0 To W - 1
    For J = 0 To H - 1
        S(I, J) = Int(Rnd * 11)
Next J, I
Dim Assets(1 To 10) As Long
For I = 1 To 10
    If I < 10 Then Assets(I) = _LoadImage("data\000" + Chr$(I + 48) + ".png", 32) Else Assets(I) = _LoadImage("data\0010.png", 32)
Next I
Do
    _Limit 60
    Cls , _RGB32(255)
    For I = 0 To W - 1
        For J = 0 To H - 1
            T = S(I, J)
            If T = 0 Then _Continue
            _PutImage (I * 16, J * 16)-(I * 16 + 15, J * 16 + 15), Assets(T)
    Next J, I
    _Display
    S(Int(Rnd * W), Int(Rnd * H)) = Int(Rnd * 11)
Loop Until Inp(&H60) = 1
System
