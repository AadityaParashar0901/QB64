'Very Fast Noise Function Library, by Aaditya Parashar (aadityap0901 @ qb64.boards.net)
'This code is optimised as much as I could

'$Dynamic
Dim As _Unsigned _Integer64 X, Y
Dim O As _Unsigned _Bit * 3: O = 7
Dim Shared perm(0 To 65535) As Double
Const Smoothness = 64
Const W = 640
Const H = 480
Screen _NewImage(W, H, 32)
Dim Shared SEED As _Unsigned Integer
RePERM 'Initialize the perm(utation) array
'A simple Demo
Dim R As _Unsigned _Bit: R = -1 'RGB
Do
    Cls
    If F Then _Title "Rendering: Octaves =" + Str$(O) + " Fade" Else _Title "Rendering: Octaves =" + Str$(O)
    ST! = Timer(0.01)
    For X = 0 To W - 1
        For Y = 0 To H - 1
            If R Then
                PSet (X, Y), _RGB32(255 * fractal2#(X, Y, O, Smoothness), 255 * fractal2#(X - W, Y - H, O, Smoothness), 255 * fractal2#(X + W, Y + H, O, Smoothness))
            Else
                PSet (X, Y), _RGB32(255 * fractal2#(X, Y, O, Smoothness))
            End If
    Next Y, X
    ST! = Timer(0.01) - ST!
    _Title "Octaves =" + Str$(O) + "Time:" + Str$(ST!)
    _Display
    Do
        'Controls:
        'R -> Toggle B&W, RGB
        'T -> New Seed
        'O -> Increase Octaves, values range from 0 to 7
        'C -> Copy Screen Image to Clipboard
        'Esc -> Exit
        _Limit 60
        If _KeyDown(70) Or _KeyDown(102) Then F = Not F: Exit Do
        If _KeyDown(82) Or _KeyDown(114) Then R = Not R: Exit Do
        If _KeyDown(84) Or _KeyDown(116) Then RePERM: Exit Do
        If _KeyDown(79) Or _KeyDown(111) Then O = O + 1: Exit Do
        If _KeyDown(67) Or _KeyDown(99) Then _ClipboardImage = 0
        If _KeyDown(27) Then System
    Loop
Loop Until Inp(&H60) = 1
System
'-------

Sub RePERM 'To reset the perm(utation) array with floating-point numbers between 0 and 1
    SEED = Timer
    Randomize SEED
    For I = 0 To 65535
        perm(I) = Rnd
    Next I
End Sub

Function hash# (X As _Unsigned Integer) 'Get a number from the array
    hash# = perm(X)
End Function
Function fade# (t As Double) Static 'Creates a fade effect, map this function on desmos -> 6.t ^ 5 - 15.t ^ 4 + 10.t ^ 3
    fade# = ((6 * t - 15) * t + 10) * t ^ 3
End Function
Function interpolate# (A#, B#, C#) Static 'Interpolates between two values, c = 0.5 -> gives average
    interpolate# = A# + (B# - A#) * C#
End Function
Function noise1# (X As Double) Static 'noise function 1d
    fX% = Int(X): dX# = fade#(X - fX%)
    noise1# = interpolate#(hash(fX%), hash(fX% + 1), dX#)
End Function
Function noise2# (X As Double, Y As Double) Static 'noise function 2d
    fX% = Int(X): dX# = fade#(X - fX%)
    fY% = Int(Y): dY# = fade#(Y - fY%)
    noise2# = interpolate#(interpolate#(hash(fX% + fY% * 57), hash(fX% + fY% * 57 + 1), dX#), interpolate#(hash(fX% + fY% * 57 + 57), hash(fX% + fY% * 57 + 58), dX#), dY#)
End Function
Function noise3# (X As Double, Y As Double, Z As Double) Static 'noise function 3d
    fX% = Int(X): dX# = fade#(X - fX%)
    fY% = Int(Y): dY# = fade#(Y - fY%)
    fZ% = Int(Z): dZ# = fade#(Z - fZ%)
    noise3# = interpolate#(interpolate#(interpolate#(hash(fX% + (fY% + fZ% * 57) * 57), hash(fX% + (fY% + fZ% * 57) * 57 + 1), dX#), interpolate#(hash(fX% + (fY% + fZ% * 57) * 57 + 57), hash(fX% + (fY% + fZ% * 57) * 57 + 58), dX#), dY#), interpolate#(interpolate#(hash(fX% + (fY% + fZ% * 57 + 57) * 57), hash(fX% + (fY% + fZ% * 57 + 57) * 57 + 1), dX#), interpolate#(hash(fX% + (fY% + fZ% * 57 + 57) * 57 + 57), hash(fX% + (fY% + fZ% * 57 + 57) * 57 + 58), dX#), dY#), dZ#)
End Function
'X, Y, Z, W -> Coordinates
'O          -> Octaves
'S          -> Smoothness (should be more than 1 and an integer if possible, putting 1 gives random numbers)
Function fractal1# (X As Integer, O As _Unsigned _Byte, S As _Unsigned Long) Static 'noise function 1d with octaves
    Dim As Double a, t, d, I
    a = 1: t = 0: d = 0
    For I = 0 To O
        t = t + a * noise1#(X / a / S): d = d + a: a = a / 2
    Next I
    fractal1# = t / d
End Function
Function fractal2# (X As Integer, Y As Integer, O As _Unsigned _Byte, S As _Unsigned Long) Static 'noise function 2d with octaves
    Dim As Double a, t, d, I
    a = 1: t = 0: d = 0
    For I = 0 To O
        t = t + a * noise2#(X / a / S, Y / a / S): d = d + a: a = a / 2
    Next I
    fractal2# = t / d
End Function
Function fractal3# (X As Integer, Y As Integer, Z As Integer, O As _Unsigned _Byte, S As _Unsigned Long) Static 'noise function 3d with octaves
    Dim As Double a, t, d, I
    a = 1: t = 0: d = 0
    For I = 0 To O
        t = t + a * noise3#(X / a / S, Y / a / S, Z / a / S): d = d + a: a = a / 2
    Next I
    fractal3# = t / d
End Function
