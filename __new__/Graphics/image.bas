$Console:Only
Randomize Timer
IMAGE& = _NewImage(4, 4, 32)
_Dest IMAGE&
Dim USEDCOLORS(255) As _Bit
Dim C As Long
For I = 1 To 4
    For J = 1 To 4
        PSet (I - 1, J - 1), _RGB32(Int(Rnd * 256), Int(Rnd * 256), Int(Rnd * 256))
    Next J
Next I
_ClipboardImage = IMAGE&
System
