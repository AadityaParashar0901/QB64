'$Dynamic
Type Alien
    As _Unsigned _Byte Type
    As _Unsigned Long Health, MaxHealth
    As _Unsigned Integer Reward, Size, Freeze
    As Single Speed
    As Single X, Y, Angle
    As Single FX, FY
End Type
Type Turret
    As _Unsigned _Byte Type, Target, Range, Level, Splash, Freeze
    As _Unsigned Integer Attack
    As Single LastAttackTime, AttackSpeed
    As _Unsigned Integer X, Y, BX, BY
    As _Unsigned Long MoneyValue
    As Single Angle
End Type
Type QIMG_Header
    As String * 4 Signature
    As _Unsigned Long Width, Height, Frames
    As _Unsigned _Byte ColorType
    As _Unsigned _Byte Compressed
    As _Unsigned Long DataLength
End Type
Type QIMG_Sprite
    As _Unsigned Long Width, Height, Frames, Frame
    As _Unsigned _Byte Delay, TotalDelay
    As String ImageHandle
End Type
Dim Shared QIMG_Sprites(0) As QIMG_Sprite
Dim Shared As Long Res_Aliens(1 To 15), Res_Turrets(1 To 4)
Dim Shared As _Byte AliensSpriteFrames(1 To 15)

'Load Resources
For I = 1 To 15
    Res_Aliens(I) = QIMG_LoadSpriteFromFile("aliens\Alien" + _Trim$(Str$(I)) + ".qimg", 15)
Next I
Res_Turrets(1) = _LoadImage("turrets\Vulcan.png", 32): Res_Turrets(2) = _LoadImage("turrets\Plasma.png", 32): Res_Turrets(3) = _LoadImage("turrets\Sam.png", 32): Res_Turrets(4) = _LoadImage("turrets\Freeze.png", 32)
'--------------
Screen _NewImage(960, 720, 32)
_PrintMode _KeepBackground
Randomize Timer

Dim Shared As _Bit isStarted, isPaused
Dim Shared TimeCount As _Unsigned Long, Time As Single
Dim Shared As _Unsigned Long Money, Kills, Level
Dim Shared SelectedTurretID As _Unsigned Long
Dim Shared Queue$
Dim Shared KEYHIT As Long

Dim StartButtonText As String: StartButtonText = "Start"

Const GRID_SIZE = 25
Const ASPECTRATIO = 720 / GRID_SIZE
Const StartPoint_X = 1, StartPoint_Y = GRID_SIZE \ 2
Const EndPoint_X = GRID_SIZE, EndPoint_Y = GRID_SIZE \ 2

Dim Shared GRID As Long: GRID = _NewImage(GRID_SIZE * 8, GRID_SIZE * 8, 32)
Dim Shared Grid2(1 To GRID_SIZE + 1, 1 To GRID_SIZE) As _Unsigned Integer
_Dest GRID
For I = 0 To GRID_SIZE - 1
    For J = 0 To GRID_SIZE - 1
        Line (I * 8 + 1, J * 8 + 1)-(I * 8 + 7, J * 8 + 7), _RGB32(64), BF
Next J, I
Line (StartPoint_X * 8 - 7, StartPoint_Y * 8 - 7)-(StartPoint_X * 8, StartPoint_Y * 8), _RGB32(64, 0, 0), BF
Line (EndPoint_X * 8 - 7, EndPoint_Y * 8 - 7)-(EndPoint_X * 8, EndPoint_Y * 8), _RGB32(64, 0, 0), BF
_Dest 0

Dim Shared Aliens(1 To 25) As Alien
Dim Shared Turrets(0) As Turret
Dim Shared PlayerHealth As Single: PlayerHealth = 20

GameTickTimer = _FreeTimer
On Timer(GameTickTimer, 1 / 6) GoSub GameTick
Timer(GameTickTimer) On

Level = 0
Money = 80
GeneratePathMap

Do
    _Limit 60
    KEYHIT = _KeyHit
    If isPaused = 0 Then
        TimeCount = TimeCount + 1
        Time = TimeCount / 60
        AliensAlive = 0
        For I = LBound(Aliens) To UBound(Aliens)
            AliensAlive = AliensAlive + Sgn(Aliens(I).Health)
            AlienPlay I
        Next I
        If isStarted And AliensAlive = 0 Then GenerateLevel
        For I = LBound(Turrets) To UBound(Turrets)
            TurretPlay I
        Next I
    End If
    Cls , _RGB32(32)
    _PutImage (0, 0)-(719, 719), GRID, 0, , _Smooth
    'Color _RGB32(255): For X = 1 To GRID_SIZE: For Y = 1 To GRID_SIZE: _PrintString ((X - 1) * ASPECTRATIO, (Y - 1) * ASPECTRATIO), Hex$(Grid2(X, Y)): Next Y, X: Color _RGB32(255)
    For I = LBound(Aliens) To UBound(Aliens)
        If Aliens(I).Health = 0 Then _Continue
        If InRange(0, Aliens(I).X, GRID_SIZE) And InRange(0, Aliens(I).Y, GRID_SIZE) Then
            QIMG_PutRotatedSprite Aliens(I).Type, AliensSpriteFrames(Aliens(I).Type), (Aliens(I).X - 0.5) * ASPECTRATIO, (Aliens(I).Y - 0.5) * ASPECTRATIO, 180 + Aliens(I).Angle, Aliens(I).Size
            If Aliens(I).Health < Aliens(I).MaxHealth Then
                Line ((Aliens(I).X - 0.5) * ASPECTRATIO - 16, (Aliens(I).Y - 0.5) * ASPECTRATIO - 32)-((Aliens(I).X - 0.5) * ASPECTRATIO - 16 + 32 * Aliens(I).Health / Aliens(I).MaxHealth, (Aliens(I).Y - 0.5) * ASPECTRATIO - 28), _RGB32(255 * (1 - Aliens(I).Health / Aliens(I).MaxHealth), 255 * Aliens(I).Health / Aliens(I).MaxHealth, 0), BF
                Line ((Aliens(I).X - 0.5) * ASPECTRATIO - 16, (Aliens(I).Y - 0.5) * ASPECTRATIO - 32)-((Aliens(I).X - 0.5) * ASPECTRATIO + 15, (Aliens(I).Y - 0.5) * ASPECTRATIO - 28), _RGB32(255), B
            End If
        End If
    Next I
    Color _RGB32(255, 0, 200)
    For I = LBound(Turrets) To UBound(Turrets)
        If Turrets(I).Type = 0 Then _Continue
        X = (Turrets(I).X - 0.5) * ASPECTRATIO
        Y = (Turrets(I).Y - 0.5) * ASPECTRATIO
        DrawRotated X, Y, Turrets(I).Angle, ASPECTRATIO, Res_Turrets(Turrets(I).Type)
        __T$ = Hex$(Turrets(I).Level): _PrintString (X - _PrintWidth(__T$) / 2, Y - 8), __T$
        If Time - Turrets(I).LastAttackTime <= Turrets(I).AttackSpeed Then
            T = (Time - Turrets(I).LastAttackTime) / Turrets(I).AttackSpeed
            If InRange(LBound(Aliens), Turrets(I).Target, UBound(Aliens)) Then
                Turrets(I).BX = (Interpolate(Turrets(I).X, Aliens(Turrets(I).Target).X, T) - 0.5) * ASPECTRATIO
                Turrets(I).BY = (Interpolate(Turrets(I).Y, Aliens(Turrets(I).Target).Y, T) - 0.5) * ASPECTRATIO
                Circle (Turrets(I).BX, Turrets(I).BY), 1, _RGB32(0)
            End If
            Line (X - 16, Y + ASPECTRATIO / 2)-(X - 16 + T * 32, Y + ASPECTRATIO / 2 - 4), _RGB32(0, 127, 255), BF
            Line (X - 16, Y + ASPECTRATIO / 2)-(X + 15, Y + ASPECTRATIO / 2 - 4), _RGB32(255), B
        End If
    Next I
    Color _RGB32(255)
    _PrintString (720, 0), "Health:" + Str$(PlayerHealth)
    _PrintString (720, 24), "Gold:" + Str$(Money) + ", Score:" + Str$(Kills)
    _PrintString (720, 48), "Level:" + Str$(Level) + ", Aliens Alive:" + Str$(AliensAlive)
    If TurretChoice Then Line (654 + TurretChoice * 64, 61)-(723 + TurretChoice * 64, 115), _RGB32(255), BF
    _PutImage (721, 64)-(768, 112), Res_Turrets(1)
    _PutImage (785, 64)-(832, 112), Res_Turrets(2)
    _PutImage (849, 64)-(896, 112), Res_Turrets(3)
    _PutImage (903, 64)-(960, 112), Res_Turrets(4)
    If InRange(LBound(Turrets), SelectedTurretID, UBound(Turrets)) Then
        _PrintString (720, 256), "Level:" + Str$(Turrets(SelectedTurretID).Level)
        _PrintString (720, 280), "Type:" + Str$(Turrets(SelectedTurretID).Type)
        _PrintString (720, 304), "Damage:" + Str$(Turrets(SelectedTurretID).Attack)
        _PrintString (720, 328), "Splash Area, Freeze:" + Str$(Turrets(SelectedTurretID).Splash) + "," + Str$(Turrets(SelectedTurretID).Freeze)
        Circle ((Turrets(SelectedTurretID).X - 0.5) * ASPECTRATIO, (Turrets(SelectedTurretID).Y - 0.5) * ASPECTRATIO), Turrets(SelectedTurretID).Range * ASPECTRATIO / 10, _RGB32(255, 0, 0)
        If Money > 10 * Turrets(SelectedTurretID).Level Then Line (720, 352)-(784, 368), _RGB32(0, 255, 0), BF Else Line (720, 352)-(784, 368), _RGB32(127), BF
        Line (720, 376)-(784, 392), _RGB32(127), BF
        _PrintString (724, 352), "Upgrade"
        _PrintString (736, 376), "Sell"
        If _MouseButton(1) Then
            If InBox(720, 352, _MouseX, _MouseY, 784, 368) Then UpgradeTurret SelectedTurretID: While _MouseInput Or _MouseButton(1): Wend
            If InBox(720, 376, _MouseX, _MouseY, 784, 392) Then SellTurret SelectedTurretID: While _MouseInput Or _MouseButton(1): Wend
        End If
    End If
    While _MouseInput: Wend
    Line (720, 700)-(959, 719), _RGB32(0), BF
    _PrintString (840 - _PrintWidth(StartButtonText) / 2, 702), StartButtonText
    If _MouseButton(1) Then
        MX = _MouseX
        MY = _MouseY
        If InBox(0, 0, MX, MY, 719, 719) Then
            SelectedTurretID = 0
            For I = LBound(Turrets) To UBound(Turrets)
                X = Int(Turrets(I).X) * ASPECTRATIO
                X1 = X - ASPECTRATIO
                Y = Int(Turrets(I).Y) * ASPECTRATIO
                Y1 = Y - ASPECTRATIO
                If InBox(X1, Y1, MX, MY, X, Y) Then SelectedTurretID = I: Exit For
            Next I
        End If
        If InBox(721, 64, MX, MY, 768, 112) Then TurretChoice = 1
        If InBox(785, 64, MX, MY, 832, 112) Then TurretChoice = 2
        If InBox(849, 64, MX, MY, 896, 112) Then TurretChoice = 3
        If InBox(903, 64, MX, MY, 960, 112) Then TurretChoice = 4
        If InBox(0, 0, MX, MY, 719, 719) Then BuildTurret TurretChoice, MX \ ASPECTRATIO + 1, MY \ ASPECTRATIO + 1: TurretChoice = 0
        If InBox(720, 700, MX, MY, 959, 719) Then
            If isStarted Then isPaused = _ToggleBit(isPaused, 0)
            isStarted = 1
            If isPaused Then StartButtonText = "Play" Else StartButtonText = "Pause"
            While _MouseInput Or _MouseButton(1): Wend
        End If
    End If
    _Display
Loop
System
GameTick:
For J = LBound(AliensSpriteFrames) To UBound(AliensSpriteFrames)
    AliensSpriteFrames(J) = AliensSpriteFrames(J) + 1
    If AliensSpriteFrames(J) > QIMG_Sprites(Res_Aliens(J)).Frames Then AliensSpriteFrames(J) = 1
Next J
Return
Sub BuildTurret (T, X, Y)
    Select Case T
        Case 1: If Money < 5 Then Exit Sub
        Case 2: If Money < 25 Then Exit Sub
        Case 3: If Money < 20 Then Exit Sub
        Case 4: If Money < 50 Then Exit Sub
    End Select
    For J = LBound(Turrets) To UBound(Turrets)
        If Turrets(J).X = X And Turrets(J).Y = Y Then Exit Sub
    Next J
    I = UBound(Turrets) + 1
    ReDim _Preserve Turrets(1 To I) As Turret
    Turrets(I).Level = 1
    Turrets(I).X = X
    Turrets(I).Y = Y
    Turrets(I).Angle = 360 * Rnd
    Turrets(I).Type = T
    Select Case T
        Case 1
            Money = Money - 5
            Turrets(I).Attack = 10
            Turrets(I).AttackSpeed = 1
            Turrets(I).Range = 50
            Turrets(I).MoneyValue = 5
        Case 2
            Money = Money - 25
            Turrets(I).Attack = 5
            Turrets(I).AttackSpeed = 0.5
            Turrets(I).Range = 50
            Turrets(I).MoneyValue = 25
        Case 3
            Money = Money - 20
            Turrets(I).Attack = 5
            Turrets(I).AttackSpeed = 1
            Turrets(I).Range = 100
            Turrets(I).Splash = 4
            Turrets(I).MoneyValue = 20
        Case 4
            Money = Money - 50
            Turrets(I).Attack = 5
            Turrets(I).AttackSpeed = 1
            Turrets(I).Range = 50
            Turrets(I).Freeze = 1
            Turrets(I).MoneyValue = 50
    End Select
    GeneratePathMap
End Sub
Sub UpgradeTurret (I)
    If Money < 10 * Turrets(I).Level Then Exit Sub
    Money = Money - 10 * Turrets(I).Level
    Turrets(I).MoneyValue = Turrets(I).MoneyValue + 10 * Turrets(I).Level
    Turrets(I).Level = Turrets(I).Level + 1
    Turrets(I).Attack = Turrets(I).Attack + 5
    Turrets(I).Range = Turrets(I).Range + 1
    Turrets(I).Splash = Turrets(I).Splash + Sgn(Turrets(I).Splash)
    If Turrets(I).Freeze < 50 Then Turrets(I).Freeze = Turrets(I).Freeze + Sgn(Turrets(I).Freeze)
End Sub
Sub SellTurret (I)
    Turrets(I).Type = 0
    Money = Money + Int(0.75 * Turrets(I).MoneyValue)
    Turrets(I).X = 0
    Turrets(I).Y = 0
    Turrets(I).Level = 0
    Turrets(I).Splash = 0
    Turrets(I).Freeze = 0
    Turrets(I).Attack = 0
    SelectedTurretID = 0
    GeneratePathMap
End Sub
Sub GeneratePathMap
    Queue$ = ListNew$
    QueueAdd EndPoint_X + 1, EndPoint_Y
    Dim As Integer X, Y
    ReDim Grid2(1 To GRID_SIZE + 1, 1 To GRID_SIZE) As _Unsigned Integer
    For I = LBound(Turrets) To UBound(Turrets)
        If Turrets(I).Type = 0 Then _Continue
        X = Turrets(I).X
        Y = Turrets(I).Y
        Grid2(X, Y) = 65535
    Next I
    Do
        If ListLength(Queue$) = 0 Then Exit Do
        QueueRemove X, Y
        If X < UBound(Grid2, 1) Then If Grid2(X + 1, Y) = 0 Then QueueAdd X + 1, Y: Grid2(X + 1, Y) = Grid2(X, Y) + 1
        If X > LBound(Grid2, 1) Then If Grid2(X - 1, Y) = 0 Then QueueAdd X - 1, Y: Grid2(X - 1, Y) = Grid2(X, Y) + 1
        If Y < UBound(Grid2, 2) Then If Grid2(X, Y + 1) = 0 Then QueueAdd X, Y + 1: Grid2(X, Y + 1) = Grid2(X, Y) + 1
        If Y > LBound(Grid2, 2) Then If Grid2(X, Y - 1) = 0 Then QueueAdd X, Y - 1: Grid2(X, Y - 1) = Grid2(X, Y) + 1
    Loop
End Sub
Sub GenerateLevel
    Level = Level + 1
    t = Int(Rnd * 15) + 1
    n = Int(Rnd * (5 + Level)) + 1
    h = Level * 10 + 1
    r = Level \ 4 + 1
    S = Level \ 25 + 5
    If UBound(Aliens) < n Then ReDim Aliens(1 To n) As Alien
    For I = 1 To n
        Aliens(I).Type = t
        Aliens(I).Health = h
        Aliens(I).MaxHealth = h
        Aliens(I).Reward = r
        Aliens(I).Speed = S
        Aliens(I).Size = ASPECTRATIO * 2
        Aliens(I).X = StartPoint_X - 10 * Rnd
        Aliens(I).Y = StartPoint_Y
        Aliens(I).Angle = 0
        Aliens(I).FX = 1
        Aliens(I).FY = Aliens(I).Y
    Next I
End Sub
Sub TurretPlay (I)
    If Turrets(I).Type = 0 Then Exit Sub
    X = Turrets(I).X: Y = Turrets(I).Y
    If InRange(LBound(Aliens), Turrets(I).Target, UBound(Aliens)) Then If Dis2(X, Y, Aliens(Turrets(I).Target).X, Aliens(Turrets(I).Target).Y) > Turrets(I).Range / 10 Then Turrets(I).Target = 0
    If Turrets(I).Target = 0 Then
        MinAlienID = 0
        MinD = Turrets(I).Range / 10
        For J = LBound(Aliens) To UBound(Aliens)
            If Aliens(J).Health = 0 Then _Continue
            If InBox(1, 1, Aliens(J).X, Aliens(J).Y, GRID_SIZE, GRID_SIZE) = 0 Then _Continue
            D = Dis2(Aliens(J).X, Aliens(J).Y, X, Y)
            If D < MinD Then
                MinD = D
                MinAlienID = J
            End If
        Next J
        Turrets(I).Target = MinAlienID
    End If
    If Turrets(I).Target = 0 Then Exit Sub
    If Aliens(Turrets(I).Target).Health = 0 Then Turrets(I).Target = 0: Exit Sub
    Turrets(I).Angle = _R2D(_Atan2(Y - Aliens(Turrets(I).Target).Y, Aliens(Turrets(I).Target).X - X))
    If Time - Turrets(I).LastAttackTime > Turrets(I).AttackSpeed Then
        If Turrets(I).Splash Then
            SR = Turrets(I).Splash
            X = Aliens(Turrets(I).Target).X
            Y = Aliens(Turrets(I).Target).Y
            For J = LBound(Aliens) To UBound(Aliens)
                If Dis2(X, Y, Aliens(J).X, Aliens(J).Y) <= SR Then Aliens(J).Health = Max(Aliens(J).Health - Turrets(I).Attack, 0)
            Next J
        Else
            Aliens(Turrets(I).Target).Health = Max(Aliens(Turrets(I).Target).Health - Turrets(I).Attack, 0)
        End If
        If Turrets(I).Freeze Then Aliens(Turrets(I).Target).Freeze = Turrets(I).Freeze
        Turrets(I).LastAttackTime = Time
    End If
    If Aliens(Turrets(I).Target).Health = 0 Then Kills = Kills + 1: Money = Money + Aliens(Turrets(I).Target).Reward
End Sub
Sub AlienPlay (I)
    Dim As Integer CPX, CPY
    If Aliens(I).Health = 0 Then Exit Sub
    If Aliens(I).Type <> 1 Then Aliens(I).Angle = _R2D(_Atan2(Aliens(I).Y - Aliens(I).FY, Aliens(I).FX - Aliens(I).X))
    dx = (Aliens(I).Speed - Aliens(I).Freeze) * Cos(_D2R(-Aliens(I).Angle)) / 100
    dy = (Aliens(I).Speed - Aliens(I).Freeze) * Sin(_D2R(-Aliens(I).Angle)) / 100
    Aliens(I).X = Aliens(I).X + dx
    Aliens(I).Y = Aliens(I).Y + dy
    If Aliens(I).X > GRID_SIZE Or Aliens(I).Y > GRID_SIZE Then Aliens(I).Health = 0: PlayerHealth = PlayerHealth - 1
    If Aliens(I).Type = 1 Then Exit Sub
    CPX = CInt(Aliens(I).X)
    CPY = CInt(Aliens(I).Y)
    If InRange(1, CPX, GRID_SIZE + 1) And InRange(1, CPY, GRID_SIZE) Then
        If CPX < UBound(Grid2, 1) Then If Grid2(CPX + 1, CPY) < Grid2(CPX, CPY) Then CPX = CPX + 1
        If CPX > LBound(Grid2, 1) Then If Grid2(CPX - 1, CPY) < Grid2(CPX, CPY) Then CPX = CPX - 1
        If CPY < UBound(Grid2, 2) Then If Grid2(CPX, CPY + 1) < Grid2(CPX, CPY) Then CPY = CPY + 1
        If CPY > LBound(Grid2, 2) Then If Grid2(CPX, CPY - 1) < Grid2(CPX, CPY) Then CPY = CPY - 1
        Aliens(I).FX = CPX
        Aliens(I).FY = CPY
    Else
        Aliens(I).FX = 1
        Aliens(I).FY = Aliens(I).Y
    End If
End Sub
Function InRange (A, B, C): InRange = (A <= B) And (B <= C): End Function
Function InBox (X1, Y1, X, Y, X2, Y2): InBox = InRange(X1, X, X2) And InRange(Y1, Y, Y2): End Function
Sub DrawRotated (X As _Unsigned Integer, Y As _Unsigned Integer, __T As _Unsigned Integer, S As _Unsigned Integer, __H&)
    Dim As Single __Theta, __CT, __ST, __X1, __Y1, __X2, __Y2, __X3, __Y3, __X4, __Y4
    __Theta = _D2R(__T): __CT = Cos(__Theta): __ST = Sin(__Theta)
    __S = S / 2
    W = _Width(__H&)
    H = _Height(__H&)
    __X1 = -__S * __CT + -__S * __ST: __Y1 = --__S * __ST + -__S * __CT
    __X2 = __S * __CT + -__S * __ST: __Y2 = -__S * __ST + -__S * __CT
    __X3 = __S * __CT + __S * __ST: __Y3 = -__S * __ST + __S * __CT
    __X4 = -__S * __CT + __S * __ST: __Y4 = --__S * __ST + __S * __CT
    _MapTriangle (0, 0)-(W - 1, 0)-(W - 1, H - 1), __H& To(X + __X1, Y + __Y1)-(X + __X2, Y + __Y2)-(X + __X3, Y + __Y3)
    _MapTriangle (0, 0)-(0, H - 1)-(W - 1, H - 1), __H& To(X + __X1, Y + __Y1)-(X + __X4, Y + __Y4)-(X + __X3, Y + __Y3)
End Sub
Function Interpolate (A!, B!, C!): Interpolate = A! + (B! - A!) * C!: End Function
Function Min (A, B): Min = -A * (A < B) - B * (A >= B): End Function
Function Max (A, B): Max = -B * (A < B) - A * (A >= B): End Function
Function Dis2 (X1, Y1, X2, Y2): Dis2 = _Hypot(X2 - X1, Y2 - Y1): End Function
'$Include:'QIMG.bas'
Sub QueueAdd (X As Integer, Y As Integer)
    Queue$ = ListAdd$(Queue$, MKI$(X) + MKI$(Y))
End Sub
Sub QueueRemove (X As Integer, Y As Integer)
    T$ = ListGet$(Queue$, 1)
    X = CVI(Left$(T$, 2))
    Y = CVI(Right$(T$, 2))
    Queue$ = ListDelete$(Queue$, 1)
End Sub
Function ListNew$
    ListNew$ = MKL$(0)
End Function
Function ListLength~& (__List As String)
    ListLength~& = CVL(Mid$(__List, 1, 4))
End Function
Function ListAdd$ (__List As String, __Item As String)
    ListAdd$ = MKL$(CVL(Mid$(__List, 1, 4)) + 1) + Mid$(__List, 5) + MKI$(Len(__Item)) + __Item
End Function
Function ListInsert$ (__List As String, __ItemNumber As _Unsigned Long, __Item As String)
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    Dim As _Unsigned Integer __LEN
    __nItems = CVL(Mid$(__List, 1, 4))
    __OFFSET = 5
    If __ItemNumber > __nItems Then
        If __ItemNumber = __nItems + 1 Then ListInsert$ = ListAdd$(__List, __Item) Else Exit Function
    End If
    For __I = 1 To __ItemNumber - 1
        __LEN = CVI(Mid$(__List, __OFFSET, 2))
        Print Mid$(__List, __OFFSET + 2, __LEN)
        __OFFSET = __OFFSET + __LEN + 2
    Next __I
    ListInsert$ = MKL$(CVL(Mid$(__List, 1, 4)) + 1) + Mid$(__List, 5, __OFFSET - 5) + MKI$(Len(__Item)) + __Item + Mid$(__List, __OFFSET)
End Function
Sub ListPrint (__List As String)
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    Dim As _Unsigned Integer __LEN
    __nItems = CVL(Mid$(__List, 1, 4))
    __OFFSET = 5
    Print "[";
    For __I = 1 To __nItems
        __LEN = CVI(Mid$(__List, __OFFSET, 2))
        Print Mid$(__List, __OFFSET + 2, __LEN);
        If __I < __nItems Then Print ",";
        __OFFSET = __OFFSET + __LEN + 2
    Next __I
    Print "]"
End Sub
Function ListGet$ (__List As String, __ItemNumber As _Unsigned Long)
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    Dim As _Unsigned Integer __LEN
    __nItems = CVL(Mid$(__List, 1, 4))
    If __ItemNumber > __nItems Then Exit Function
    __OFFSET = 5
    For __I = 1 To __nItems
        __LEN = CVI(Mid$(__List, __OFFSET, 2))
        If __I = __ItemNumber Then ListGet$ = Mid$(__List, __OFFSET + 2, __LEN): Exit Function
        __OFFSET = __OFFSET + __LEN + 2
    Next __I
End Function
Function ListDelete$ (__List As String, __ItemNumber As _Unsigned Long)
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    Dim As _Unsigned Integer __LEN
    __nItems = CVL(Mid$(__List, 1, 4))
    __OFFSET = 5
    For __I = 1 To __nItems
        __LEN = CVI(Mid$(__List, __OFFSET, 2))
        If __I = __ItemNumber Then
            ListDelete$ = MKL$(__nItems - 1) + Mid$(__List, 5, __OFFSET - 5) + Mid$(__List, __OFFSET + __LEN + 2)
            Exit Function
        End If
        __OFFSET = __OFFSET + __LEN + 2
    Next __I
End Function
