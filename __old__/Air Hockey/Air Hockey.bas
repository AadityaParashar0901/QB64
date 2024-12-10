Screen _NewImage(480, 640, 32)
_PrintMode _KeepBackground
Color _RGB32(255)
_Title "Air Hockey"
Type Vec2
    As Single X, Y
End Type
Dim As Vec2 NULLVECTOR

Dim As _Unsigned Integer ScoreAI, ScorePlayer
Dim As Vec2 Ball_Position, Ball_Velocity, Mouse, AI, AITarget
Ball_Position.X = _Width / 2: Ball_Position.Y = _Height / 2
AI.X = _Width / 2: AI.Y = _Height / 4
Mouse.X = _Width / 2: Mouse.Y = _Height * 0.75
Const Ball_Radius = 10
Const Mouse_Radius = 20
Const AI_Radius = 10
Const AIMaxSpeed = 400
Dim Mouse_Goal_Width, AI_Goal_Width
Mouse_Goal_Width = _Width / 2
AI_Goal_Width = _Width / 2

_ScreenMove _DesktopWidth / 2 - _Width / 2, _DesktopHeight / 2 - _Height / 2
_MouseMove _Width / 2, _Height * 0.75
_MouseHide
Const FPS = 60 * 5
Do
    _Limit FPS
    If _WindowHasFocus = 0 Then _Continue
    While _MouseInput
        Mouse.X = Mouse.X + _MouseMovementX * 2 / 3: Mouse.Y = Mouse.Y + _MouseMovementY * 2 / 3
        Mouse.X = Bound(0, Mouse.X, _Width)
        Mouse.Y = Bound(_Height / 2, Mouse.Y, _Height)
        If Mouse.Y < _Height / 2 Then Mouse.Y = _Height / 2
        _MouseMove _Width / 2, _Height * 0.75
        CircleCollision Ball_Position, Ball_Radius, 1, Mouse, Mouse_Radius, 10, Ball_Velocity
    Wend
    If CircleTouchBox(Ball_Position, Ball_Radius, (_Width - AI_Goal_Width) / 2, 0, (_Width + AI_Goal_Width) / 2, 5) Then
        C1& = _RGB32(127, 0, 0)
        ScorePlayer = ScorePlayer + 1
        Ball_Position.X = _Width / 2
        Ball_Position.Y = _Height * 0.4
        Ball_Velocity = NULLVECTOR
        AI_Goal_Width = Max(_Width * 0.25, AI_Goal_Width - 5)
        Mouse_Goal_Width = Min(_Width * 0.75, Mouse_Goal_Width + 5)
    Else
        C1& = _RGB32(0, 127, 0)
    End If
    If CircleTouchBox(Ball_Position, Ball_Radius, (_Width - Mouse_Goal_Width) / 2, _Height - 5, (_Width + Mouse_Goal_Width) / 2, _Height) Then
        C2& = _RGB32(127, 0, 0)
        ScoreAI = ScoreAI + 1
        Ball_Position.X = _Width / 2
        Ball_Position.Y = _Height * 0.6
        Ball_Velocity = NULLVECTOR
        Mouse_Goal_Width = Max(_Width * 0.25, Mouse_Goal_Width - 5)
        AI_Goal_Width = Min(_Width * 0.75, AI_Goal_Width + 5)
    Else
        C2& = _RGB32(0, 127, 0)
    End If
    CircleCollision Ball_Position, Ball_Radius, 1, Mouse, Mouse_Radius, 10, Ball_Velocity

    AITarget.X = Ball_Position.X + Ball_Radius * 0.75 * Cos(_Atan2(Ball_Position.Y - _Height, Ball_Position.X - _Width / 2))
    AITarget.Y = Ball_Position.Y + Ball_Radius * 0.75 * Sin(_Atan2(Ball_Position.Y - _Height, Ball_Position.X - _Width / 2))
    AI.X = AI.X + Sgn(AITarget.X - AI.X) * Min(Abs(AITarget.X - AI.X) * FPS, AIMaxSpeed) / FPS
    If Ball_Position.Y >= _Height / 2 Then
        AI.Y = AI.Y + Sgn(_Height / 4 - AI.Y)
    Else
        AI.Y = AI.Y + Sgn(AITarget.Y - AI.Y) * Min(Abs(AITarget.Y - AI.Y) * FPS, AIMaxSpeed) / FPS
    End If
    CircleCollision Ball_Position, Ball_Radius, 1, AI, AI_Radius, 25, Ball_Velocity

    Ball_Position.X = Ball_Position.X + Ball_Velocity.X / FPS
    Ball_Position.Y = Ball_Position.Y + Ball_Velocity.Y / FPS
    If Ball_Position.X < Ball_Radius Then Ball_Position.X = Ball_Radius: Ball_Velocity.X = Ball_Velocity.X * -0.67
    If Ball_Position.X > _Width - Ball_Radius Then Ball_Position.X = _Width - Ball_Radius: Ball_Velocity.X = Ball_Velocity.X * -0.67
    If Ball_Position.Y < Ball_Radius Then Ball_Position.Y = Ball_Radius: Ball_Velocity.Y = Ball_Velocity.Y * -0.67
    If Ball_Position.Y > _Height - Ball_Radius Then Ball_Position.Y = _Height - Ball_Radius: Ball_Velocity.Y = Ball_Velocity.Y * -0.67
    Ball_Velocity.X = Round(Ball_Velocity.X * 0.995)
    Ball_Velocity.Y = Round(Ball_Velocity.Y * 0.995)
    If DISPLAY Mod 5 = 0 Then
        Cls , _RGB32(0, 63, 127)
        Line (0, _Height / 2 - 2)-(_Width, _Height / 2 + 2), _RGB32(255), BF

        Line ((_Width - AI_Goal_Width) / 2, 0)-((_Width + AI_Goal_Width) / 2, 5), C1&, BF
        Line ((_Width - Mouse_Goal_Width) / 2, _Height - 5)-((_Width + Mouse_Goal_Width) / 2, _Height), C2&, BF

        FillCircle Ball_Position.X, Ball_Position.Y, 0, Ball_Radius, _RGB32(255, 0, 0)

        FillCircle Mouse.X, Mouse.Y, Mouse_Radius - 4, Mouse_Radius, _RGB32(0, 255, 127)
        FillCircle AI.X, AI.Y, AI_Radius - 2, AI_Radius, _RGB32(0, 127, 255)

        _PrintString (0, _Height / 4), Str$(ScoreAI)
        _PrintString (0, _Height * 0.75), Str$(ScorePlayer)
        _Display
        DISPLAY = 0
    End If
    DISPLAY = DISPLAY + 1
Loop Until Inp(&H60) = 1
System
Sub CircleCollision (P1 As Vec2, R1, M1, P2 As Vec2, R2, M2, V1 As Vec2)
    d = R1 + R2 - distance2(P1.X, P1.Y, P2.X, P2.Y)
    If d < 0 Then Exit Sub
    d = d * M2 / M1
    r = _Atan2(P1.Y - P2.Y, P1.X - P2.X)
    V1.X = V1.X + d * Cos(r)
    V1.Y = V1.Y + d * Sin(r)
End Sub
Function distance2 (X1, Y1, X2, Y2)
    distance2 = _Hypot(Abs(X1 - X2), Abs(Y1 - Y2))
End Function
Sub FillCircle (X, Y, R1, R2, C&)
    For I = R1 To R2
        Circle (X, Y), I, C&
    Next I
End Sub
Function Bound (A, X, B)
    Bound = X
    If X < A Then Bound = A
    If X > B Then Bound = B
End Function
Function Round (X)
    If Abs(X) < 5 Then Round = 0 Else Round = X
End Function
Function inRange (A, B, C)
    If A <= B And B <= C Then inRange = -1
End Function
Function inBox (x, y, x1, y1, x2, y2)
    If inRange(x1, x, x2) And inRange(y1, y, y2) Then inBox = -1
End Function
Function Min (A, B)
    If A < B Then Min = A Else Min = B
End Function
Function Max (A, B)
    If A < B Then Max = B Else Max = A
End Function
Function CircleTouchBox (C As Vec2, R As Single, X1, Y1, X2, Y2)
    CircleTouchBox = inBox(C.X, C.Y, X1 - R, Y1 - R, X2 + R, Y2 + R)
End Function
