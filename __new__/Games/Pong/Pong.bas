Screen _NewImage(640, 480, 32)
Ball_PX = _Width / 2: Ball_PY = _Height / 2: Ball_R = 5: Ball_DX = 1: Ball_DY = 100
BarX = _Width / 2
BarHeight = 4
BarLength = 100
Const FPS = 60
Do
    _Limit FPS * 4
    If _WindowHasFocus = 0 Then _Continue
    Cls
    If Ball_PX + Ball_R * Sgn(Ball_DX) < 0 Or Ball_PX + Ball_R * Sgn(Ball_DX) > _Width Then Ball_DX = -Ball_DX
    If Ball_PY + Ball_R * Sgn(Ball_DY) < 0 Then Ball_DY = -Ball_DY
    If Ball_PX > BarX - BarLength / 2 And Ball_PX < BarX + BarLength / 2 And Ball_PY + Ball_R * Sgn(Ball_DY) > _Height - BarHeight Then
        Ball_DX = Ball_DX + 100 * (Ball_PX - BarX) / BarLength
        Ball_DY = -Ball_DY
        Score = Score + 1
    End If
    If Ball_PY > _Height Then Run
    Ball_PX = Ball_PX + Ball_DX / FPS
    Ball_PY = Ball_PY + Ball_DY / FPS
    If _KeyDown(19200) Then BarX = BarX + Sgn(BarLength / 2 - BarX)
    If _KeyDown(19712) Then BarX = BarX + Sgn(_Width - BarLength / 2 - BarX)
    Circle (Ball_PX, Ball_PY), Ball_R, _RGB32(255, 0, 0)
    Line (BarX - BarLength / 2, _Height - BarHeight)-(BarX + BarLength / 2, _Height - 1), _RGB32(0, 127, 255), BF
    Print Score
    _Display
Loop Until Inp(&H60) = 1
System
