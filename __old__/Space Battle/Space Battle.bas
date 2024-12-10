'Space Battle
'By Aaditya Parashar
'
'Features: Basic Shooting System
'          Enemies Just Flying Behind
'          Basic Radar System
'Controls:
'    W - Accelerate
'    S - Decelerate
'    A - Strafe Left
'    D - Strafe Right
'    Q - Rotate Left
'    E - Rotate Right
'    Space - Shoot Bullets
'    Enter - Fire Missiles

Type Vec2: As Single X, Y: End Type
Type Entity: As Vec2 Position, Velocity: As _Unsigned Integer EXTRA, SafeDistance: As _Unsigned _Byte LIVE: HEALTH As Integer: End Type
Dim Shared W, H: W = _DesktopWidth: H = _DesktopHeight: Screen _NewImage(W, H, 32): _FullScreen _SquarePixels
DefInt A-Z: Randomize Timer

Const Simulation_Distance = 65536
Const ShipMaxVelocity = 1000, ShipMinVelocity = 25
Const EnemyMaxVelocity = 750, EnemyMinVelocity = 50
Const ShipScale = 2
Const FPS = 240

'Resources:
Dim Shared Resources(1 To 4) As Long
Print "Loading Resources"
Resources(1) = load_ship&
Resources(2) = load_ship_missile&
Resources(3) = load_blip&
Resources(4) = load_shooter&
'----------------

Dim Shared Ship As Entity: Ship.Velocity.Y = 500: Ship.HEALTH = 500 'Velocity and Size of Player's Ship
Dim Shared Bullets(1 To 10000) As Entity, BULLETID As Integer, BULLET As _Unsigned Long
Dim Shared Missiles(1 To 1000) As Entity, MISSILEID As Integer, MISSILE As _Unsigned Integer
Dim Shared Enemies(1 To 250) As Entity, TOTALENEMY As Integer: TOTALENEMY = 1
Dim Shared EnemyBullets(1 To 10000) As Entity, ENEMYBULLETID As Integer
For __I = 1 To 25
    Enemies(__I).Position.X = Rnd * Simulation_Distance / 2 - Simulation_Distance / 4
    Enemies(__I).Position.Y = Rnd * Simulation_Distance / 2 - Simulation_Distance / 4
    Enemies(__I).SafeDistance = Rnd * 100
    Enemies(__I).Velocity.X = 0
    Enemies(__I).Velocity.Y = 100
    Enemies(__I).LIVE = 1
    Enemies(__I).HEALTH = 10
Next __I
Dim Shared As Vec2 RightVector, LeftVector
NewVector RightVector, 1, 0
NewVector LeftVector, -1, 0
Stars 0 'Initialize Stars
Do
    _Limit FPS: While _MouseInput: Wend
    If _WindowHasFocus = 0 Then _Continue
    'W - Forward
    If _KeyDown(87) Or _KeyDown(119) Then If Ship.Velocity.Y < ShipMaxVelocity Then Ship.Velocity.Y = Ship.Velocity.Y + 5
    '----------------
    'S - Backward
    If _KeyDown(83) Or _KeyDown(115) Then If Ship.Velocity.Y > ShipMinVelocity Then Ship.Velocity.Y = Ship.Velocity.Y - 5
    '----------------
    'Q - Turn Left
    If _KeyDown(81) Or _KeyDown(113) Then
        Stars 2
        DrawBullets 2
        DrawEnemyBullets 2
        DrawMissiles 2
        DrawEnemies 2
    End If
    '----------------
    'E - Turn Right
    If _KeyDown(69) Or _KeyDown(101) Then
        Stars 3
        DrawBullets 3
        DrawEnemyBullets 3
        DrawMissiles 3
        DrawEnemies 3
    End If
    '----------------
    'A - Strafe Left
    If _KeyDown(65) Or _KeyDown(97) Then
        Stars 5
        DrawBullets 5
        DrawEnemyBullets 5
        DrawMissiles 5
        DrawEnemies 5
    End If
    '----------------
    'D - Strafe Right
    If _KeyDown(68) Or _KeyDown(100) Then
        Stars 6
        DrawBullets 6
        DrawEnemyBullets 6
        DrawMissiles 6
        DrawEnemies 6
    End If
    '----------------

    Stars 10 'Manage Stars

    'Shoot Bullets
    BULLET = (-BULLET - 1) * _KeyDown(32)
    If BULLET > 0 And (BULLET Mod 10 = 0) And BULLETID < UBound(Bullets) Then
        BULLETID = BULLETID + 1
        Bullets(BULLETID).Position.X = 8.5
        Bullets(BULLETID).Position.Y = -4
        Bullets(BULLETID).Velocity.Y = -6000
        Bullets(BULLETID).HEALTH = 1
        Bullets(BULLETID).LIVE = -1
        BULLETID = BULLETID + 1
        Bullets(BULLETID).Position.X = -8.5
        Bullets(BULLETID).Position.Y = -4
        Bullets(BULLETID).Velocity.Y = -6000
        Bullets(BULLETID).HEALTH = 1
        Bullets(BULLETID).LIVE = -1
    End If
    '----------------
    'Fire Missiles
    MISSILE = (-MISSILE - 1) * _KeyDown(13)
    If MISSILE = 1 And MISSILEID < UBound(Missiles) Then
        MISSILEID = MISSILEID + 1
        Missiles(MISSILEID).Position.X = 5 * 2 * (MISSILEID Mod 2 - 0.5)
        Missiles(MISSILEID).Position.Y = -3
        Missiles(MISSILEID).Velocity.Y = -1500
        Missiles(MISSILEID).HEALTH = 5
        Missiles(MISSILEID).LIVE = -1
    End If
    '----------------

    'Drawing
    Cls
    Stars 1
    Line (_Width - 225, 200)-(_Width - 25, 1), _RGB32(0), BF
    Line (_Width - 225, 200)-(_Width - 25, 1), _RGB32(0, 255, 0), B
    DrawBullets 1
    DrawEnemyBullets 1
    DrawMissiles 1
    DrawEnemies 1
    _PutImage (_Width / 2 - (8.5) * ShipScale, _Height / 2 - (8.5) * ShipScale)-(_Width / 2 + (8.5) * ShipScale, _Height / 2 + (8.5) * ShipScale), Resources(1)
    _PutImage (_Width - 125 - 5.5, 100 - 5.5)-(_Width - 125 + 5.5, 100 + 5.5), Resources(3)
    Print "Health:"; Ship.HEALTH
    Print "Speed:"; Ship.Velocity.Y
    Print "Bullets Left:"; UBound(Bullets) - BULLETID
    Print "Missiles Left:"; UBound(Missiles) - MISSILEID
    _Display
    '----------------

    On _Exit GOTO __EXIT
Loop Until Inp(&H60) = 1
__EXIT:
Close
System
FILEERROR:
Print "A Resource File Does Not Exist!"
End
Sub Stars (__WORK As _Unsigned _Byte) Static
    $Checking:Off
    Select Case __WORK
        Case 0
            __StarCount = Simulation_Distance * 2
            Dim __Stars(1 To __StarCount) As Vec2
            For __I = 1 To __StarCount
                __Stars(__I).X = Rnd * Simulation_Distance * 2 - Simulation_Distance
                __Stars(__I).Y = Rnd * Simulation_Distance * 2 - Simulation_Distance
            Next __I
        Case 1
            VisibleStarCount = 0
            For __I = 1 To __StarCount
                __Stars(__I).Y = __Stars(__I).Y + Ship.Velocity.Y / FPS
                If __Stars(__I).X > -_Width / 2 And __Stars(__I).X < _Width / 2 And __Stars(__I).Y > -_Height / 2 And __Stars(__I).Y < _Height / 2 Then
                    VisibleStarCount = VisibleStarCount + 1
                    PSet (_Width / 2 + ShipScale * __Stars(__I).X, _Height / 2 + ShipScale * __Stars(__I).Y), _RGB32(255)
                End If
            Next __I
        Case 2
            For __I = 1 To __StarCount
                RotateVector __Stars(__I), -1
            Next __I
        Case 3
            For __I = 1 To __StarCount
                RotateVector __Stars(__I), 1
            Next __I
        Case 5
            For __I = 1 To __StarCount
                AddVector __Stars(__I), RightVector
            Next __I
        Case 6
            For __I = 1 To __StarCount
                AddVector __Stars(__I), LeftVector
            Next __I
        Case 10
            For __I = 1 To __StarCount
                If __Stars(__I).Y < -_Height Then __Stars(__I).X = Rnd * Simulation_Distance * 2 - Simulation_Distance: __Stars(__I).Y = _Height - Rnd * _Height / 4
                If __Stars(__I).Y > _Height Then __Stars(__I).X = Rnd * Simulation_Distance * 2 - Simulation_Distance: __Stars(__I).Y = -_Height + Rnd * _Height / 4
            Next __I
    End Select
    $Checking:On
End Sub
Sub DrawBullets (__WORK As _Unsigned _Byte)
    Select Case __WORK
        Case 1
            BULLETCOUNT = 0
            For __I = 1 To UBound(Bullets)
                If Bullets(__I).LIVE Then
                    Bullets(__I).Position.X = Bullets(__I).Position.X + Bullets(__I).Velocity.X / FPS
                    Bullets(__I).Position.Y = Bullets(__I).Position.Y + (Bullets(__I).Velocity.Y + Ship.Velocity.Y) / FPS
                    Line (_Width / 2 + ShipScale * Bullets(__I).Position.X, _Height / 2 + ShipScale * Bullets(__I).Position.Y)-(_Width / 2 + ShipScale * (Bullets(__I).Position.X + Bullets(__I).Velocity.X / FPS), _Height / 2 + ShipScale * (Bullets(__I).Position.Y + Bullets(__I).Velocity.Y / FPS)), _RGB32(0, 127, 255)
                    If Distance(Bullets(__I).Position, Ship.Position) > Simulation_Distance Then Bullets(__I).LIVE = 0
                    For __J = 1 To UBound(Enemies)
                        If Enemies(__J).LIVE Then
                            If Distance(Enemies(__J).Position, Bullets(__I).Position) < ShipScale * 10 Then
                                Enemies(__J).HEALTH = Enemies(__J).HEALTH - Bullets(__I).HEALTH
                                If Distance(Enemies(__J).Position, Bullets(__I).Position) < ShipScale * 5 Then Enemies(__J).HEALTH = Enemies(__J).HEALTH - 5 * Bullets(__I).HEALTH
                                If Enemies(__J).HEALTH <= 0 Then Enemies(__J).LIVE = 0
                                Bullets(__I).LIVE = 0
                                Exit For
                            End If
                        End If
                    Next __J
                    BULLETCOUNT = BULLETCOUNT + Bullets(__I).LIVE
                End If
            Next __I
            If BULLETCOUNT = 0 Then DrawBullets 10
        Case 2
            For __I = 1 To UBound(Bullets)
                RotateVector Bullets(__I).Position, -1
                RotateVector Bullets(__I).Velocity, -1
            Next __I
        Case 3
            For __I = 1 To UBound(Bullets)
                RotateVector Bullets(__I).Position, 1
                RotateVector Bullets(__I).Velocity, 1
            Next __I
        Case 5
            For __I = 1 To UBound(Bullets)
                AddVector Bullets(__I).Position, RightVector
            Next __I
        Case 6
            For __I = 1 To UBound(Bullets)
                AddVector Bullets(__I).Position, LeftVector
            Next __I
        Case 10
            'BULLETID = BULLETID - Sgn(BULLETID):If BULLETID = 0 Then Erase Bullets
    End Select
End Sub
Sub DrawEnemyBullets (__WORK As _Unsigned _Byte)
    Select Case __WORK
        Case 1
            ENEMYBULLETCOUNT = 0
            For __I = 1 To UBound(EnemyBullets)
                If EnemyBullets(__I).LIVE Then
                    EnemyBullets(__I).Position.X = EnemyBullets(__I).Position.X + EnemyBullets(__I).Velocity.X / FPS
                    EnemyBullets(__I).Position.Y = EnemyBullets(__I).Position.Y + (EnemyBullets(__I).Velocity.Y + Ship.Velocity.Y) / FPS
                    Line (_Width / 2 + ShipScale * EnemyBullets(__I).Position.X, _Height / 2 + ShipScale * EnemyBullets(__I).Position.Y)-(_Width / 2 + ShipScale * (EnemyBullets(__I).Position.X + EnemyBullets(__I).Velocity.X / FPS), _Height / 2 + ShipScale * (EnemyBullets(__I).Position.Y + EnemyBullets(__I).Velocity.Y / FPS)), _RGB32(255, 127, 0)
                    If Distance(EnemyBullets(__I).Position, Ship.Position) > Simulation_Distance Then EnemyBullets(__I).LIVE = 0
                    If Distance(Ship.Position, EnemyBullets(__I).Position) < ShipScale * 10 Then
                        Ship.HEALTH = Ship.HEALTH - 1
                        EnemyBullets(__I).LIVE = 0
                    End If
                    ENEMYBULLETCOUNT = ENEMYBULLETCOUNT + EnemyBullets(__I).LIVE
                End If
            Next __I
            If ENEMYBULLETCOUNT = 0 Then DrawEnemyBullets 10
        Case 2
            For __I = 1 To UBound(EnemyBullets)
                RotateVector EnemyBullets(__I).Position, -1
                RotateVector EnemyBullets(__I).Velocity, -1
            Next __I
        Case 3
            For __I = 1 To UBound(EnemyBullets)
                RotateVector EnemyBullets(__I).Position, 1
                RotateVector EnemyBullets(__I).Velocity, 1
            Next __I
        Case 5
            For __I = 1 To UBound(EnemyBullets)
                AddVector EnemyBullets(__I).Position, RightVector
            Next __I
        Case 6
            For __I = 1 To UBound(EnemyBullets)
                AddVector EnemyBullets(__I).Position, LeftVector
            Next __I
        Case 10
            ENEMYBULLETID = ENEMYBULLETID - Sgn(ENEMYBULLETID)
            If ENEMYBULLETID = 0 Then Erase EnemyBullets
    End Select
End Sub
Sub DrawMissiles (__WORK As _Unsigned _Byte)
    Select Case __WORK
        Case 1
            MISSILECOUNT = 0
            Dim As Vec2 __V1, __V2, __V3, __V4
            For __I = 1 To UBound(Missiles)
                If Missiles(__I).LIVE > 0 Then
                    Missiles(__I).Position.X = Missiles(__I).Position.X + Missiles(__I).Velocity.X / FPS
                    Missiles(__I).Position.Y = Missiles(__I).Position.Y + (Missiles(__I).Velocity.Y + Ship.Velocity.Y) / FPS
                    NewVector __V1, 1.16 * ShipScale, -3.33 * ShipScale
                    NewVector __V2, 1.16 * ShipScale, 3.33 * ShipScale
                    NewVector __V3, -1.16 * ShipScale, 3.33 * ShipScale
                    NewVector __V4, -1.16 * ShipScale, -3.33 * ShipScale
                    __THETA = _R2D(_Atan2(Missiles(__I).Velocity.X, Missiles(__I).Velocity.Y)) + 180
                    RotateVector __V1, __THETA
                    RotateVector __V2, __THETA
                    RotateVector __V3, __THETA
                    RotateVector __V4, __THETA
                    __DX = _Width / 2 + ShipScale * Missiles(__I).Position.X
                    __DY = _Height / 2 + ShipScale * Missiles(__I).Position.Y
                    If InRange(-_Width, __DX, _Width) And InRange(-_Height, __DY, _Height) Then
                        _MapTriangle (6.5, -0.5)-(6.5, 19.5)-(-0.5, 19.5), Resources(2) To(__DX + __V1.X, __DY + __V1.Y)-(__DX + __V2.X, __DY + __V2.Y)-(__DX + __V3.X, __DY + __V3.Y)
                        _MapTriangle (6.5, -0.5)-(-0.5, -0.5)-(-0.5, 19.5), Resources(2) To(__DX + __V1.X, __DY + __V1.Y)-(__DX + __V4.X, __DY + __V4.Y)-(__DX + __V3.X, __DY + __V3.Y)
                    End If
                    DrawRadar 1, Missiles(__I).Position
                    If Distance(Missiles(__I).Position, Ship.Position) > Simulation_Distance Then Missiles(__I).LIVE = 0
                    For __J = 1 To UBound(Enemies)
                        If Enemies(__J).LIVE Then
                            If Distance(Enemies(__J).Position, Missiles(__I).Position) < ShipScale * 10 Then
                                Enemies(__J).HEALTH = Enemies(__J).HEALTH - Missiles(__I).HEALTH
                                If Distance(Enemies(__J).Position, Missiles(__I).Position) < ShipScale * 5 Then Enemies(__J).HEALTH = Enemies(__J).HEALTH - 5 * Missiles(__I).HEALTH
                                If Enemies(__J).HEALTH <= 0 Then Enemies(__J).LIVE = 0
                                Missiles(__I).LIVE = 0
                                Exit For
                            End If
                        End If
                    Next __J
                    MISSILECOUNT = MISSILECOUNT + Missiles(__I).LIVE
                End If
            Next __I
            If MISSILECOUNT = 0 Then DrawMissiles 10
        Case 2
            For __I = 1 To UBound(Missiles)
                RotateVector Missiles(__I).Position, -1
                RotateVector Missiles(__I).Velocity, -1
            Next __I
        Case 3
            For __I = 1 To UBound(Missiles)
                RotateVector Missiles(__I).Position, 1
                RotateVector Missiles(__I).Velocity, 1
            Next __I
        Case 5
            For __I = 1 To UBound(Missiles)
                AddVector Missiles(__I).Position, RightVector
            Next __I
        Case 6
            For __I = 1 To UBound(Missiles)
                AddVector Missiles(__I).Position, LeftVector
            Next __I
        Case 10
            'MISSILEID = MISSILEID - Sgn(MISSILEID):If MISSILEID = 0 Then Erase Missiles
    End Select
End Sub
Sub DrawEnemies (__WORK As _Unsigned _Byte)
    Select Case __WORK
        Case 1
            ENEMYCOUNT = 0
            Dim As Vec2 __V1, __V2, __V3, __V4
            For __I = 1 To UBound(Enemies)
                If Enemies(__I).LIVE > 0 Then
                    Enemies(__I).Position.X = Enemies(__I).Position.X + Enemies(__I).Velocity.X / FPS
                    Enemies(__I).Position.Y = Enemies(__I).Position.Y + (Enemies(__I).Velocity.Y + Ship.Velocity.Y) / FPS
                    EnemyVelocity = MagnitudeVector(Enemies(__I).Velocity) + Sgn(Sqr2(Distance(Enemies(__I).Position, Ship.Position) - 300 - Enemies(__I).SafeDistance) + MagnitudeVector2(Ship.Velocity)) * 10
                    NewVector Enemies(__I).Velocity, -Enemies(__I).Position.X, -Enemies(__I).Position.Y
                    NormalizeVector Enemies(__I).Velocity
                    MultiplyVector Enemies(__I).Velocity, Max(EnemyMinVelocity, Min(EnemyMaxVelocity, EnemyVelocity))
                    NewVector __V1, 8.5 * ShipScale, -8.5 * ShipScale
                    NewVector __V2, 8.5 * ShipScale, 8.5 * ShipScale
                    NewVector __V3, -8.5 * ShipScale, 8.5 * ShipScale
                    NewVector __V4, -8.5 * ShipScale, -8.5 * ShipScale
                    __THETA = _R2D(_Atan2(Enemies(__I).Velocity.X, Enemies(__I).Velocity.Y)) + 180
                    RotateVector __V1, __THETA
                    RotateVector __V2, __THETA
                    RotateVector __V3, __THETA
                    RotateVector __V4, __THETA
                    __DX = _Width / 2 + ShipScale * Enemies(__I).Position.X
                    __DY = _Height / 2 + ShipScale * Enemies(__I).Position.Y
                    If InRange(-_Width, __DX, _Width) And InRange(-_Height, __DY, _Height) Then
                        _MapTriangle (16.5, -0.5)-(16.5, 16.5)-(-0.5, 16.5), Resources(Enemies(__I).LIVE + 3) To(__DX + __V1.X, __DY + __V1.Y)-(__DX + __V2.X, __DY + __V2.Y)-(__DX + __V3.X, __DY + __V3.Y)
                        _MapTriangle (16.5, -0.5)-(-0.5, -0.5)-(-0.5, 16.5), Resources(Enemies(__I).LIVE + 3) To(__DX + __V1.X, __DY + __V1.Y)-(__DX + __V4.X, __DY + __V4.Y)-(__DX + __V3.X, __DY + __V3.Y)
                    End If
                    DrawRadar 2, Enemies(__I).Position
                    Enemies(__I).EXTRA = Enemies(__I).EXTRA - Sgn(Enemies(__I).EXTRA)
                    If Sin(__THETA) < 0.1 And Enemies(__I).EXTRA = 0 Then
                        ENEMYBULLETID = ENEMYBULLETID + 1
                        EnemyBullets(ENEMYBULLETID).Position.X = Enemies(__I).Position.X
                        EnemyBullets(ENEMYBULLETID).Position.Y = Enemies(__I).Position.Y
                        EnemyBullets(ENEMYBULLETID).Velocity = Enemies(__I).Velocity
                        NormalizeVector EnemyBullets(ENEMYBULLETID).Velocity
                        MultiplyVector EnemyBullets(ENEMYBULLETID).Velocity, 6000
                        EnemyBullets(ENEMYBULLETID).HEALTH = 1
                        EnemyBullets(ENEMYBULLETID).LIVE = -1
                        Enemies(__I).EXTRA = Enemies(__I).EXTRA + FPS
                    End If
                    ENEMYCOUNT = ENEMYCOUNT + Enemies(__I).LIVE
                End If
            Next __I
            If ENEMYCOUNT = 0 Then DrawEnemies 10
        Case 2
            For __I = 1 To UBound(Enemies)
                RotateVector Enemies(__I).Position, -1
                RotateVector Enemies(__I).Velocity, 1
            Next __I
        Case 3
            For __I = 1 To UBound(Enemies)
                RotateVector Enemies(__I).Position, 1
                RotateVector Enemies(__I).Velocity, -1
            Next __I
        Case 5
            For __I = 1 To UBound(Enemies)
                AddVector Enemies(__I).Position, RightVector
            Next __I
        Case 6
            For __I = 1 To UBound(Enemies)
                AddVector Enemies(__I).Position, LeftVector
            Next __I
        Case 10
            _AutoDisplay
            Cls , _RGB32(127, 255, 127): Print "YOU WIN"
            End
    End Select
End Sub
Sub DrawRadar (__ID As _Unsigned _Byte, __P As Vec2)
    Select Case __ID
        Case 1
            PSet (_Width - 125 + __P.X / Simulation_Distance * 100, 100 + __P.Y / Simulation_Distance * 100), _RGB32(255, 31, 31)
        Case 2
            Circle (_Width - 125 + __P.X / Simulation_Distance * 100, 100 + __P.Y / Simulation_Distance * 100), 2, _RGB32(255, 127, 0)
    End Select
End Sub
Function Min (__A, __B)
    If __A < __B Then Min = __A Else Min = __B
End Function
Function Max (__A, __B)
    If __A > __B Then Max = __A Else Max = __B
End Function
Sub NewVector (__A As Vec2, __X As Single, __Y As Single)
    __A.X = __X
    __A.Y = __Y
End Sub
Sub AddVector (__A As Vec2, __B As Vec2)
    __A.X = __B.X + __A.X
    __A.Y = __B.Y + __A.Y
End Sub
Sub SubVector (__A As Vec2, __B As Vec2)
    __A.X = __B.X - __A.X
    __A.Y = __B.Y - __A.Y
End Sub
Sub MultiplyVector (__A As Vec2, __M As Single)
    __A.X = __A.X * __M
    __A.Y = __A.Y * __M
End Sub
Sub RotateVector (__A As Vec2, __T As Single)
    Dim __B As Vec2
    __Theta = _D2R(__T)
    __B.X = __A.X * Cos(__Theta) + __A.Y * Sin(__Theta)
    __B.Y = -__A.X * Sin(__Theta) + __A.Y * Cos(__Theta)
    __A = __B
End Sub
Sub NormalizeVector (__A As Vec2)
    __L! = MagnitudeVector!(__A)
    __A.X = __A.X / __L
    __A.Y = __A.Y / __L
End Sub
Function MagnitudeVector! (__A As Vec2)
    MagnitudeVector! = Sqr(__A.X ^ 2 + __A.Y ^ 2)
End Function
Function MagnitudeVector2! (__A As Vec2)
    MagnitudeVector2! = Sqr(__A.X ^ 2 + __A.Y ^ 2) * Sgn(_Atan2(__A.Y, __A.X) - _Pi / 2)
End Function
Function Distance (__A As Vec2, __B As Vec2)
    Distance = Sqr((__A.X - __B.X) ^ 2 + (__A.Y - __B.Y) ^ 2)
End Function
Function Sqr2 (__X As Single)
    Select Case __X
        Case Is > 0: Sqr2 = Sqr(__X)
        Case 0: Sqr2 = 0
        Case Is < 0: Sqr2 = -Sqr(-__X)
    End Select
End Function
Function Round (__X As Single)
    Round = _Round(__X)
End Function
Function InRange (__A, __B, __C)
    If __A <= __B And __B <= __C Then InRange = -1 Else InRange = 0
End Function
Function load_ship&
    O& = _NewImage(17, 17, 32)
    __Dest = _Dest: _Dest O&
    Restore ship_data
    For __X = 0 To 16: For __Y = 0 To 17
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_ship = O&
    Exit Function
    ship_data:
    Data 0,0,0,0,0,0,&HFFE45C5C,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,0,
    Data 0,0,0,0,0,0,0,0,0,0,0,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,0,0,
    Data 0,0,0,0,0,0,0,0,0,0,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,0,0,0,
    Data 0,0,0,0,0,&HFFE45C5C,&HFFE45C5C,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&H2FFFFFF,0,0,0,
    Data 0,0,0,&H2E45C5C,0,&H1FFFFFF,0,0,&H8FFFFFF,&HFF4393EC,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFFE45C5C,&HFFE45C5C,&H80E45C5C,
    Data 0,0,0,0,0,0,0,0,&HFF4393EC,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFFE45C5C,&H80E45C5C,&H80E45C5C,
    Data 0,0,0,0,0,0,0,&HFF4393EC,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,0,0,
    Data 0,0,&H1FFFFFF,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFFE45C5C,&HFFE45C5C,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,0,0,0,
    Data &HFFE45C5C,&HFF35AEFF,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFFE45C5C,&HFFE45C5C,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,
    Data 0,0,&H2FFFFFF,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFFE45C5C,&HFFE45C5C,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,0,0,0,
    Data 0,0,0,0,0,0,0,&HFF4393EC,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,0,0,
    Data 0,0,0,0,0,0,0,0,&HFF4393EC,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFFE45C5C,&H80E45C5C,&H80E45C5C,
    Data 0,0,0,0,0,0,0,0,0,&HFF4393EC,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,&HFFE45C5C,&HFFE45C5C,&H80E45C5C,
    Data 0,0,0,&H2E45C5C,&H1E45C5C,&HFFE45C5C,&HFFE45C5C,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HAFFFFFF,0,0,0,
    Data 0,0,0,0,0,0,0,0,0,0,&HFF77DD3D,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,0,0,0,
    Data 0,0,0,0,0,0,&H6E45C5C,&H1E45C5C,&H1FFFFFF,&H1FFFFFF,&H6FFFFFF,&HFF77DD3D,&HFF35AEFF,&HFF77DD3D,&HFF77DD3D,0,0,
    Data 0,0,0,0,0,0,&HFFE45C5C,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,&HFF35AEFF,0,
End Function
Function load_ship_missile&
    O& = _NewImage(7, 20, 32)
    __Dest = _Dest: _Dest O&
    Restore ship_missile_data
    For __X = 0 To 6: For __Y = 0 To 20
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_ship_missile = O&
    Exit Function
    ship_missile_data:
    Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,0,0,
    Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&HFFFEFEFE,0,0,0,0,
    Data 0,0,0,&H72FEFEFE,&HBFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFF7F27,0,0,0,
    Data &HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFF7F27,&HFFFF7F27,&HFFFFF200,0,
    Data 0,0,0,&H72FEFEFE,&HBFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFF7F27,0,0,0,
    Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&HFFFEFEFE,0,0,0,0,
    Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,0,0,
End Function
Function load_blip&
    O& = _NewImage(11, 11, 32)
    __Dest = _Dest: _Dest O&
    Restore blip_data
    For __X = 0 To 10: For __Y = 0 To 11
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_blip = O&
    Exit Function
    blip_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFEFEFE,&HFFFEFEFE,&HFFFEFEFE,
End Function
Function load_shooter&
    O& = _NewImage(17, 17, 32)
    __Dest = _Dest: _Dest O&
    Restore shooter_data
    For __X = 0 To 16: For __Y = 0 To 17
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_shooter = O&
    Exit Function
    shooter_data:
    Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    Data 0,0,0,0,0,&HFFFF3874,&HFF4393EC,&HFFFF3874,&HFF81DD4D,&HFF4393EC,&HFF81DD4D,&HFF4393EC,&HFF81DD4D,&HFF4393EC,0,0,0,
    Data 0,0,0,0,0,0,0,0,0,&HFF81DD4D,&HFF81DD4D,&HFF81DD4D,0,0,0,0,0,
    Data 0,0,0,0,0,0,0,0,&HFF4393EC,&HFF4393EC,&HFF4393EC,0,0,0,0,0,0,
    Data 0,0,0,0,0,0,0,&HFF81DD4D,&HFF81DD4D,&HFF81DD4D,0,0,0,0,0,0,0,
    Data 0,0,0,0,0,0,&HFF4393EC,&HFF4393EC,&HFF81DD4D,0,0,&HFFFF3874,0,0,0,0,0,
    Data 0,0,0,&HFFFF3874,&HFF4393EC,&HFF4393EC,&HFF4393EC,&HFF81DD4D,&HFFFF3874,&HFF81DD4D,&HFF81DD4D,&HFFFFE644,&HFFFF3874,0,0,0,0,
    Data &HFFFFE644,&HFFFF3874,&HFFFF3874,&HFF81DD4D,&HFF81DD4D,&HFF4393EC,&HFF81DD4D,&HFF81DD4D,&HFFFF3874,&HFF81DD4D,&HFF4393EC,&HFFFFE644,&HFFFFE644,&HFFFF3874,0,0,0,
    Data 0,0,0,&HFFFF3874,&HFF4393EC,&HFF4393EC,&HFF4393EC,&HFF81DD4D,&HFFFF3874,&HFF81DD4D,&HFF81DD4D,&HFFFFE644,&HFFFF3874,0,0,0,0,
    Data 0,0,0,0,0,0,&HFF4393EC,&HFF4393EC,&HFF81DD4D,0,0,&HFFFF3874,0,0,0,0,0,
    Data 0,0,0,0,0,0,0,&HFF81DD4D,&HFF81DD4D,&HFF81DD4D,0,0,0,0,0,0,0,
    Data 0,0,0,0,0,0,0,0,&HFF4393EC,&HFF4393EC,&HFF4393EC,0,0,0,0,0,0,
    Data 0,0,0,0,0,0,0,0,0,&HFF81DD4D,&HFF81DD4D,&HFF81DD4D,0,0,0,0,0,
    Data 0,0,0,0,0,&HFFFF3874,&HFF4393EC,&HFFFF3874,&HFF81DD4D,&HFF4393EC,&HFF81DD4D,&HFF4393EC,&HFF81DD4D,&HFF4393EC,0,0,0,
    Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    Data 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
End Function
