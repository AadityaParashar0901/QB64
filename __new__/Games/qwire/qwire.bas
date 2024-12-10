Screen _NewImage(640, 480, 32)
_FullScreen _SquarePixels
Dim Shared As Double OffsetX, OffsetY, Scale, MouseX, MouseY, LastMouseB1, MouseB1, MouseB2, LastMouseW, MouseW, StartPanX, StartPanY, MWX_BZ, MWY_BZ, MWX_AZ, MWY_AZ, X0, Y0, X2, Y2
Dim As Integer X1, Y1, X3, Y3
Dim Shared As Long Images(1 To 43)
Scale = 4: OffsetX = -_Width / 16: Selected_Tile = 1
Type Tile: As _Unsigned _Byte Type, State, Value: End Type
Const W = 64, H = 48
Dim As Tile Grid(1 To W, 1 To H), NextGrid(1 To W, 1 To H), NU, ND, NL, NR
Const TILE_EMPTY = 0, TILE_WIRE = 1, TILE_POWER = 2, TILE_LAMP = 3, TILE_REPEATER = 4, TILE_GATE_AND = 5, TILE_GATE_OR = 6, TILE_GATE_NOT = 7, TILE_LATCH = 8
For I = 1 To 43: Read D$: Images(I) = _LoadImage("assets\TILE_" + D$ + ".png", 32): Next I
Do
    Cls , _RGB32(32)
    _Limit 60
    While _MouseInput
        MouseX = _MouseX
        MouseY = _MouseY
        MouseB1 = _MouseButton(1)
        MouseB2 = _MouseButton(2)
        MouseW = _MouseWheel
        If MouseW <> 0 Then LastMouseW = MouseW
    Wend
    If MouseB1 And LastMouseB1 = 0 Then StartPanX = MouseX: StartPanY = MouseY
    LastMouseB1 = MouseB1
    If MouseB1 Then
        If _KeyDown(100306) Then
            OffsetX = OffsetX - (MouseX - StartPanX) / Scale
            OffsetY = OffsetY - (MouseY - StartPanY) / Scale
            StartPanX = MouseX: StartPanY = MouseY
        End If
    End If
    If _KeyDown(18432) Then
        While _KeyDown(18432): Wend
        Selected_Tile = Max(Selected_Tile - 1, 1)
    End If
    If _KeyDown(20480) Then
        While _KeyDown(20480): Wend
        Selected_Tile = Min(Selected_Tile + 1, 7)
    End If
    Select Case Selected_Tile
        Case TILE_WIRE: SELECTED_TILE_IMG& = Images(16)
        Case TILE_POWER: SELECTED_TILE_IMG& = Images(17)
        Case TILE_LAMP: SELECTED_TILE_IMG& = Images(18)
        Case TILE_REPEATER: SELECTED_TILE_IMG& = Images(20)
        Case TILE_GATE_AND: SELECTED_TILE_IMG& = Images(28)
        Case TILE_GATE_OR: SELECTED_TILE_IMG& = Images(32)
        Case TILE_GATE_NOT: SELECTED_TILE_IMG& = Images(36)
        Case TILE_LATCH: SELECTED_TILE_IMG& = Images(40)
    End Select
    Screen2Canvas MouseX, MouseY, MWX_BZ, MWY_BZ
    Scale = Scale * (1 - LastMouseW / 25) * (1 - (_KeyDown(61) - _KeyDown(45)) / 25)
    Screen2Canvas MouseX, MouseY, MWX_AZ, MWY_AZ
    OffsetX = OffsetX + MWX_BZ - MWX_AZ
    OffsetY = OffsetY + MWY_BZ - MWY_AZ
    IMG& = _NewImage(W * 8, H * 8, 32): _Dest IMG&
    Screen2Canvas MouseX, MouseY, X0, Y0: X0 = X0 \ 8: Y0 = Y0 \ 8
    Screen2Canvas MouseX, MouseY, X2, Y2: X2 = X2 \ 8: Y2 = Y2 \ 8
    If _KeyDown(19712) Then
        While _KeyDown(19712): Wend
        Grid(X0, Y0).State = (Grid(X0, Y0).State Mod 4) + 1
    End If
    If _KeyDown(19200) Then
        While _KeyDown(19200): Wend
        Grid(X0, Y0).State = Grid(X0, Y0).State - 1
        If Grid(X0, Y0).State <= 0 Then Grid(X0, Y0).State = 4
    End If
    If MouseB1 And _KeyDown(100306) = 0 Then
        If InRange(LBound(Grid, 1), X0, UBound(Grid, 1)) And InRange(LBound(Grid, 2), Y0, UBound(Grid, 2)) Then
            Grid(X0, Y0).Type = Selected_Tile
            Grid(X0, Y0).Value = 0
            If Selected_Tile = TILE_REPEATER Or Selected_Tile = TILE_GATE_AND Or Selected_Tile = TILE_GATE_OR Or Selected_Tile = TILE_GATE_NOT Or Selected_Tile = TILE_LATCH Then Grid(X0, Y0).State = 1 Else Grid(X0, Y0).State = 0
        End If
    ElseIf MouseB2 Then
        If InRange(LBound(Grid, 1), X2, UBound(Grid, 1)) And InRange(LBound(Grid, 2), Y2, UBound(Grid, 2)) Then
            Grid(X2, Y2).Type = TILE_EMPTY
            Grid(X2, Y2).Value = 0
            Grid(X2, Y2).State = 0
        End If
    End If
    MemCopy _Offset(Grid()), _Offset(NextGrid()), Len(Grid())
    For I = LBound(Grid, 1) To UBound(Grid, 1)
        For J = LBound(Grid, 2) To UBound(Grid, 2)
            If I > LBound(Grid, 1) Then NL = Grid(I - 1, J) Else NL.Type = 0
            If I < UBound(Grid, 1) Then NR = Grid(I + 1, J) Else NR.Type = 0
            If J > LBound(Grid, 2) Then NU = Grid(I, J - 1) Else NU.Type = 0
            If J < UBound(Grid, 2) Then ND = Grid(I, J + 1) Else ND.Type = 0
            For K = 1 To 5
                If K = 1 Then TILE_K = TILE_REPEATER
                If K = 2 Then TILE_K = TILE_GATE_AND
                If K = 3 Then TILE_K = TILE_GATE_OR
                If K = 4 Then TILE_K = TILE_GATE_NOT
                If K = 5 Then TILE_K = TILE_LATCH
                If NL.Type = TILE_K Then If NL.State <> 2 Then NL.Type = 0
                If NR.Type = TILE_K Then If NR.State <> 4 Then NR.Type = 0
                If NU.Type = TILE_K Then If NU.State <> 3 Then NU.Type = 0
                If ND.Type = TILE_K Then If ND.State <> 1 Then ND.Type = 0
            Next K
            Select Case (Grid(I, J).Type And 15)
                Case TILE_WIRE
                    NextGrid(I, J).Value = Max(1, Max(powerValue(NL), Max(powerValue(NR), Max(powerValue(NU), powerValue(ND))))) - 1
                Case TILE_LAMP
                    NextGrid(I, J).Value = Max(0, Max(powerValue(NL), Max(powerValue(NR), Max(powerValue(NU), powerValue(ND)))))
                Case TILE_REPEATER
                    If Grid(I, J).State = 1 Then NextGrid(I, J).Value = Max(0, powerValue(ND))
                    If Grid(I, J).State = 2 Then NextGrid(I, J).Value = Max(0, powerValue(NL))
                    If Grid(I, J).State = 3 Then NextGrid(I, J).Value = Max(0, powerValue(NU))
                    If Grid(I, J).State = 4 Then NextGrid(I, J).Value = Max(0, powerValue(NR))
                Case TILE_GATE_AND
                    If _ReadBit(Grid(I, J).State, 1) Then
                        NextGrid(I, J).Value = Max(0, Max(powerValue(NL), powerValue(NR))) * -(powerValue(NL) > 0 And powerValue(NR) > 0)
                    ElseIf _ReadBit(Grid(I, J).State, 2) Then NextGrid(I, J).Value = Max(0, Max(powerValue(NU), powerValue(ND))) * -(powerValue(NU) > 0 And powerValue(ND) > 0)
                    End If
                Case TILE_GATE_OR
                    If Grid(I, J).State And 1 Then NextGrid(I, J).Value = Max(0, Max(powerValue(NL), powerValue(NR)))
                    If Grid(I, J).State And 2 Then NextGrid(I, J).Value = Max(0, Max(powerValue(NU), powerValue(ND)))
                Case TILE_GATE_NOT
                    If Grid(I, J).State = 1 Then NextGrid(I, J).Value = 16 - Max(0, powerValue(ND))
                    If Grid(I, J).State = 2 Then NextGrid(I, J).Value = 16 - Max(0, powerValue(NL))
                    If Grid(I, J).State = 3 Then NextGrid(I, J).Value = 16 - Max(0, powerValue(NU))
                    If Grid(I, J).State = 4 Then NextGrid(I, J).Value = 16 - Max(0, powerValue(NR))
            End Select
            Select Case (Grid(I, J).Type And 15)
                Case TILE_POWER: _PutImage (I * 8, J * 8), Images(17)
                Case TILE_WIRE: _PutImage (I * 8, J * 8), Images(1 + (Grid(I, J).Value And 31))
                Case TILE_LAMP: _PutImage (I * 8, J * 8), Images(18 + Sgn(Grid(I, J).Value))
                Case TILE_REPEATER: _PutImage (I * 8, J * 8), Images(19 + Grid(I, J).State + 4 * Sgn(Grid(I, J).Value))
                Case TILE_GATE_AND: _PutImage (I * 8, J * 8), Images(27 + Grid(I, J).State)
                Case TILE_GATE_OR: _PutImage (I * 8, J * 8), Images(31 + Grid(I, J).State)
                Case TILE_GATE_NOT: _PutImage (I * 8, J * 8), Images(35 + Grid(I, J).State)
                Case TILE_LATCH: _PutImage (I * 8, J * 8), Images(39 + Grid(I, J).State)
            End Select
    Next J, I
    _Dest 0
    MemCopy _Offset(NextGrid()), _Offset(Grid()), Len(Grid())
    Canvas2Screen 0, 0, X1, Y1
    Canvas2Screen _Width(IMG&), _Height(IMG&), X3, Y3
    Line (X1 - 1, Y1 - 1)-(X3 + 1, Y3 + 1), _RGB32(255), B
    _PutImage (X1, Y1)-(X3, Y3), IMG&
    _PutImage (_Width - 14, 2)-(_Width - 46, 34), SELECTED_TILE_IMG&
    _FreeImage IMG&
    _Display
Loop Until Inp(&H60) = 1
System
Data "WIRE_STATE_0","WIRE_STATE_1","WIRE_STATE_2","WIRE_STATE_3","WIRE_STATE_4","WIRE_STATE_5","WIRE_STATE_6","WIRE_STATE_7","WIRE_STATE_8","WIRE_STATE_9","WIRE_STATE_A","WIRE_STATE_B","WIRE_STATE_C","WIRE_STATE_D","WIRE_STATE_E","WIRE_STATE_F"
Data "POWER","LAMP_OFF","LAMP_ON","REPEATER_STATE_1_OFF","REPEATER_STATE_2_OFF","REPEATER_STATE_3_OFF","REPEATER_STATE_4_OFF","REPEATER_STATE_1_ON","REPEATER_STATE_2_ON","REPEATER_STATE_3_ON","REPEATER_STATE_4_ON"
Data "GATE_AND_STATE_1","GATE_AND_STATE_2","GATE_AND_STATE_3","GATE_AND_STATE_4"
Data "GATE_OR_STATE_1","GATE_OR_STATE_2","GATE_OR_STATE_3","GATE_OR_STATE_4"
Data "GATE_NOT_STATE_1","GATE_NOT_STATE_2","GATE_NOT_STATE_3","GATE_NOT_STATE_4"
Data "LATCH_STATE_1","LATCH_STATE_2","LATCH_STATE_3","LATCH_STATE_4"
Sub Canvas2Screen (WX As Double, WY As Double, SX As Integer, SY As Integer)
    SX = (WX - OffsetX) * Scale
    SY = (WY - OffsetY) * Scale
End Sub
Sub Screen2Canvas (SX As Integer, SY As Integer, WX As Double, WY As Double)
    WX = SX / Scale + OffsetX
    WY = SY / Scale + OffsetY
End Sub
Sub MemCopy (__S As _Offset, __D As _Offset, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _Mem(__S, __SIZE): __M2 = _Mem(__D, __SIZE)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
Function Min (A, B)
    If A < B Then Min = A Else Min = B
End Function
Function Max (A, B)
    If A > B Then Max = A Else Max = B
End Function
Function InRange (A, B, C)
    If A <= B And B <= C Then InRange = -1
End Function
Function isTilePowerSource (A As Tile)
    If A.Type = TILE_POWER Then isTilePowerSource = -1
End Function
Function powerValue (A As Tile)
    Select Case A.Type And 15
        Case TILE_WIRE: powerValue = A.Value
        Case TILE_POWER: powerValue = 16
        Case TILE_REPEATER: powerValue = 16 * Sgn(A.Value)
        Case TILE_GATE_AND: powerValue = A.Value
        Case TILE_GATE_OR: powerValue = A.Value
        Case TILE_GATE_NOT: powerValue = A.Value
        Case TILE_LATCH: powerValue = A.Value
    End Select
End Function
