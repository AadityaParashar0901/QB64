Randomize Timer: Screen _NewImage(960, 540, 256): _Title "Nibbles": _Font 8: _PrintMode _KeepBackground
DefInt A-Z: Type Vec2: As Integer X, Y: End Type
Dim Snake(1 To 65536) As Vec2, Snake_Opposite_Direction As Vec2, Snake_Length As Integer, Snake_Speed As Integer
Dim As _Byte Increment_Snake_Length, Create_Food
Dim As Vec2 Food
Dim As _Unsigned Long Score
AUTOBOT = Sgn(_CommandCount)
0
Snake_Speed = 10: Snake_Length = 5
Snake(1).X = 10: Snake(1).Y = 10
If Snake_Brain_Timer = 0 Then Snake_Brain_Timer = _FreeTimer
For I = 2 To Snake_Length: Snake(I).X = Snake(1).X - I + 1: Snake(I).Y = Snake(1).Y: Next I
Create_Food = -1
On Timer(Snake_Brain_Timer, 1 / Snake_Speed) GoSub Snake_Brain
Timer(Snake_Brain_Timer) On
Do
    _Limit 60
    Cls , _RGB(32, 32, 32)
    If _WindowHasFocus = 0 Then WHF = -1 Else WHF = 1
    If AUTOBOT = 0 Then
        If WHF = -1 Then Timer(Snake_Brain_Timer) Off Else Timer(Snake_Brain_Timer) On
    End If
    Snake_Opposite_DirectionDX = Snake(1).X - Snake(2).X
    Snake_Opposite_DirectionDY = Snake(1).Y - Snake(2).Y
    If _KeyDown(18432) And Snake_Opposite_DirectionDY = 0 Then SnakeDX = 0: SnakeDY = -1
    If _KeyDown(19200) And Snake_Opposite_DirectionDX = 0 Then SnakeDX = -1: SnakeDY = 0
    If _KeyDown(20480) And Snake_Opposite_DirectionDY = 0 Then SnakeDX = 0: SnakeDY = 1
    If _KeyDown(19712) And Snake_Opposite_DirectionDX = 0 Then SnakeDX = 1: SnakeDY = 0
    If _KeyDown(83) Or _KeyDown(115) Then GoSub Snake_Brain
    If _KeyDown(76) Or _KeyDown(108) Then GoTo Lose
    If WHF <> -1 Then
        For J = 1 To Snake_Length
            Select Case J
                Case 1 To 5: Color _RGB(0, 127, 255)
                Case 5 To 15: Color _RGB(0, 255, 127)
                Case 15 To 30: Color _RGB(0, 255, 0)
                Case 30 To 50: Color _RGB(0, 0, 255)
                Case 50 To 100: Color _RGB(127, 0, 255)
                Case 100 To 150: Color _RGB(127, 127, 255)
                Case 150 To 300: Color _RGB(191, 191, 0)
                Case 300 To 500: Color _RGB(255, 127, 127)
                Case Else: Color _RGB(255, 255, 255)
            End Select
            _PrintString (Snake(J).X * _FontWidth, Snake(J).Y * _FontHeight), Chr$(219)
        Next J
    End If
    If AUTOBOT Then
        Snake_Opposite_DirectionDX = Snake(1).X - Snake(2).X
        Snake_Opposite_DirectionDY = Snake(1).Y - Snake(2).Y
        If Food.Y < Snake(1).Y And Snake_Opposite_DirectionDY = 0 Then SnakeDX = 0: SnakeDY = -1
        If Food.X < Snake(1).X And Snake_Opposite_DirectionDX = 0 Then SnakeDX = -1: SnakeDY = 0
        If Food.Y > Snake(1).Y And Snake_Opposite_DirectionDY = 0 Then SnakeDX = 0: SnakeDY = 1
        If Food.X > Snake(1).X And Snake_Opposite_DirectionDX = 0 Then SnakeDX = 1: SnakeDY = 0
    End If
    Color _RGB(255, 255, 0): _PrintString (Food.X * _FontWidth, Food.Y * _FontHeight), Chr$(219)
    _Font 16: Color _RGB(255, 255, 255): _PrintString (0, 0), "Score:" + _Trim$(Str$(Score)): _PrintString (0, 16), "Head:" + Str$(Snake(1).X) + Str$(Snake(1).Y): _Font 8
    _Display
Loop Until Inp(&H60) = 1
System

'------------------------------------------------------------------------------------------------------
Snake_Brain:
For I = Snake_Length To 1 Step -1
    If I > 1 Then Snake(I).X = Snake(I - 1).X: Snake(I).Y = Snake(I - 1).Y
Next I
Snake(1).X = Snake(1).X + SnakeDX
Snake(1).Y = Snake(1).Y + SnakeDY
If Snake(1).X > _Width \ _FontWidth Then Snake(1).X = 0
If Snake(1).X < 0 Then Snake(1).X = _Width \ _FontWidth
If Snake(1).Y > _Height \ _FontHeight Then Snake(1).Y = 0
If Snake(1).Y < 0 Then Snake(1).Y = _Height \ _FontHeight
If Snake(1).X = Food.X And Snake(1).Y = Food.Y Then Increment_Snake_Length = 1: Create_Food = -1
If (SnakeDX Or SnakeDY) And AUTOBOT = 0 Then
    For I = 2 To Snake_Length
        If Snake(1).X = Snake(I).X And Snake(1).Y = Snake(I).Y Then GoTo Lose
    Next I
End If
If Increment_Snake_Length Then
    For I = 1 To 3
        Snake_Length = Snake_Length + 1
        Snake(Snake_Length).X = Snake(Snake_Length - 1).X: Snake(Snake_Length).Y = Snake(Snake_Length - 1).Y
    Next I
    Increment_Snake_Length = Increment_Snake_Length - Sgn(Increment_Snake_Length)
    If Snake_Speed < 60 Then
        Snake_Speed = Snake_Speed + 1
        On Timer(Snake_Brain_Timer, 1 / Snake_Speed) GoSub Snake_Brain
    End If
    Score = Score + 10
End If
If Create_Food Then
    Food.X = Rnd * _Width \ _FontWidth
    Food.Y = Rnd * _Height \ _FontHeight
    Create_Food = 0
End If
Return
Lose:
Timer(Snake_Brain_Timer) Off
_AutoDisplay
_Font 16
Line ((_Width - 18 * _FontWidth) / 2, (_Height - 6 * _FontHeight) / 2)-((_Width + 18 * _FontWidth) / 2, (_Height + 6 * _FontHeight) / 2), _RGB(191, 63, 0), BF
_PrintString (_Width / 2 - 4 * _FontWidth, _Height / 2 - _FontHeight * 2), "YOU LOSE"
_PrintString (_Width / 2 - _PrintWidth("SCORE:" + _Trim$(Str$(Score))) / 2, _Height / 2 - _FontHeight), "SCORE:" + _Trim$(Str$(Score))
_PrintString (_Width / 2 - 5 * _FontWidth, _Height / 2 + _FontHeight), "PLAY AGAIN?"
_PrintString (_Width / 2 - 4 * _FontWidth, _Height / 2 + _FontHeight * 2), "Yes   No"
Clear
_Font 8
Do
    _Limit 30
    K$ = UCase$(InKey$)
    If K$ = "Y" Then GoTo 0
    If K$ = "N" Then Exit Do
Loop Until Inp(&H60) = 1
System
