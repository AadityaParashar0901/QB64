Screen _NewImage(18, 11, 0)
DefLng A-Z
Dim Grid(1 To 9, 1 To 9) As _Unsigned _Byte
Dim Omit(1 To 9, 1 To 9) As _Unsigned _Bit * 9
For I = 1 To 9: For J = 1 To 9
        Read Grid(J, I)
        If Grid(J, I) Then Grid(J, I) = _SetBit(Grid(J, I), 7)
Next J, I
Color 15, 0
PX = 1: PY = 1
Do
    Cls , 0
    _Limit 60
    K$ = InKey$: Select Case K$
        Case "0" To "9": Grid(PX, PY) = _SetBit(Val(K$), 7): If _ResetBit(Grid(PX, PY), 7) = 0 Then Grid(PX, PY) = 0
            PX = PX + 1: If PX = 10 Then PX = 1: PY = PY + 1
            If PY = 10 Then PY = 1
        Case Chr$(13): Exit Do
        Case Chr$(27): System
        Case Chr$(0) + "H": If PY > 1 Then PY = PY - 1
        Case Chr$(0) + "P": If PY < 9 Then PY = PY + 1
        Case Chr$(0) + "K": If PX > 1 Then PX = PX - 1
        Case Chr$(0) + "M": If PX < 9 Then PX = PX + 1
    End Select
    For I = 1 To 9
        For J = 1 To 9
            If PX = J And PY = I Then Color , 3 Else Color , 0
            If Grid(J, I) Then Print Str$(Grid(J, I) And 15); Else Print "  ";
            If J = 9 Then Print
    Next J, I
    _Display
Loop
_Title "Working"
Do
    S = 0
    Cls , 0
    _Limit 10
    GoSub UpdateOmit
    For I = 1 To 9
        For J = 1 To 9
            '-------Solve-------
            If Grid(J, I) = 0 Then
                S = S + 1
                If Omit(J, I) Then
                    For K = 1 To 9
                        If _SetBit(Omit(J, I), K - 1) = 511 Then
                            Grid(J, I) = K
                            Exit For
                        End If
                    Next K
                End If
            End If
            '-------------------
            '------Display------
            If Grid(J, I) And 128 Then Color 15 Else Color 2
            If Grid(J, I) And 15 Then Print Str$(Grid(J, I) And 15); Else Print "  ";
            If J = 9 Then Print
            '-------------------
    Next J, I
    _Display
Loop While S
_Title "Done"
_Delay 1
Sleep
System
UpdateOmit:
For I = 1 To 9
    For J = 1 To 9
        'If Grid(I, J) Then _Continue
        For K = 1 To 9
            If K = I Then _Continue
            If _ResetBit(Grid(K, J), 7) Then Omit(I, J) = _SetBit(Omit(I, J), _ResetBit(Grid(K, J), 7) - 1)
        Next K
        For K = 1 To 9
            If K = J Then _Continue
            If _ResetBit(Grid(I, K), 7) Then Omit(I, J) = _SetBit(Omit(I, J), _ResetBit(Grid(I, K), 7) - 1)
        Next K
        __I = 3 * ((I - 1) \ 3): __J = 3 * ((J - 1) \ 3)
        For II = __I + 1 To __I + 3
            For JJ = __J + 1 To __J + 3
                If II = I And JJ = J Then _Continue
                If _ResetBit(Grid(II, JJ), 7) Then Omit(I, J) = _SetBit(Omit(I, J), _ResetBit(Grid(II, JJ), 7) - 1)
        Next JJ, II
Next J, I
Return
UpdateOmit2:
For I = 1 To 9
    For J = 1 To 9
        If _ResetBit(Grid(I, J), 7) Then _Continue
        __I = 3 * ((I - 1) \ 3): __J = 3 * ((J - 1) \ 3)
        For K = 0 To 8
            OmitN = 0
            For II = __I + 1 To __I + 3
                For JJ = __J + 1 To __J + 3
                    If II = I And JJ = J Then _Continue
                    If _ReadBit(Omit(II, JJ), K) Or Grid(II, JJ) Then OmitN = OmitN + 1
            Next JJ, II
            If OmitN = 8 Then Grid(I, J) = K
        Next K
Next J, I
Return
Data 9,0,6,3,4,0,8,1,0
Data 0,5,1,7,0,0,3,0,0
Data 4,7,0,0,9,1,0,0,5
Data 0,0,0,9,0,3,0,0,2
Data 0,0,2,0,8,7,0,0,0
Data 1,0,7,2,0,0,6,0,0
Data 0,8,5,0,0,9,1,0,0
Data 0,3,4,0,6,0,0,0,9
Data 0,1,0,5,0,8,7,0,6

