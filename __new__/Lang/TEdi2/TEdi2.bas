'$Dynamic
DefLng A-Z
Type Vec2
    As Long X, Y
End Type
Type Cursor
    As Vec2 Start, End
End Type
Dim Shared VerticalScrollOffset, HorizontalScrollOffset
VerticalScrollOffset = 1
HorizontalScrollOffset = 1
Dim Shared FILE$(1 To 1), LINES$(1 To 1)
Dim Shared Cursors(1 To 1) As Cursor
Dim Shared As _Byte KeyShift, KeyCtrl, KeyAlt
Dim Shared VisibleLines, VisibleChars
NewScreen 960, 540
Cursors(1).Start.X = 1
Cursors(1).Start.Y = 1
Cursors(1).End.X = 1
Cursors(1).End.Y = 1
Do
    Cls , _RGB32(32)
    KH = _KeyHit
    KEY$ = InKey$
    KeyShift = _KeyDown(100303) Or _KeyDown(100304)
    KeyCtrl = _KeyDown(100305) Or _KeyDown(100306)
    KeyAlt = _KeyDown(100307) Or _KeyDown(100308)
    VerticalScrollOffset = VerticalScrollOffset + (KH = 18432) - (KH = 20480)
    iCursor = 1
    Select Case KH
        Case 8: If Len(FILE$(Cursors(iCursor).Start.Y)) = 0 Then
                DeleteLine Cursors(iCursor).Start.Y
                Cursors(iCursor).Start.Y = Cursors(iCursor).Start.Y - 1
            Else
                FILE$(Cursors(iCursor).Start.Y) = Left$(FILE$(Cursors(iCursor).Start.Y), Cursors(iCursor).Start.X - 2) + Mid$(FILE$(Cursors(iCursor).Start.Y), Cursors(iCursor).Start.X)
                Cursors(iCursor).Start.X = Cursors(iCursor).Start.X - 1
            End If
        Case 13: InsertLine Cursors(iCursor).Start.Y
            Cursors(iCursor).Start.Y = Cursors(iCursor).Start.Y + 1
            Cursors(iCursor).Start.X = 1
        Case 32 To 126: FILE$(Cursors(iCursor).Start.Y) = Left$(FILE$(Cursors(iCursor).Start.Y), Cursors(iCursor).Start.X - 1) + Chr$(KH) + Mid$(FILE$(Cursors(iCursor).Start.Y), Cursors(iCursor).Start.X)
            Cursors(iCursor).Start.X = Cursors(iCursor).Start.X + 1
        Case 19200, 19712: Cursors(iCursor).Start.X = Cursors(iCursor).Start.X + (KH = 19200) - (KH = 19712)
        'Cursors(iCursor).Start.X = Max(Min(Cursors(iCursor).Start.X, Len(FILE$(Cursors(iCursor).Start.Y))) + 1, 1)
    End Select
    J = 0
    For I = VerticalScrollOffset To VisibleLines + VerticalScrollOffset
        J = J + 1
        _PrintString (16, J * (_FontHeight + 2)), FILE$(J) 'Mid$(FILE$(J), HorizontalScrollOffset, Min(VisibleChars, Len(FILE$(J)) - HorizontalScrollOffset))
        If J = UBound(FILE$) Then Exit For
    Next I
    Line (0, 0)-(15, _Height), _RGB32(0, 32), BF
    If Timer - Int(Timer) > 0.5 Then Line (16 + (Cursors(iCursor).Start.X - 1) * _FontWidth, Cursors(iCursor).Start.Y * (_FontHeight + 2))-(16 + (Cursors(iCursor).Start.X - 1) * _FontWidth + 1, (Cursors(iCursor).Start.Y + 1) * (_FontHeight + 2)), _RGB32(255), BF
    _Display
Loop Until Inp(&H60) = 1
System
Sub InsertLine (POSITION As _Unsigned Long)
    __T = UBound(FILE$) + 1
    ReDim _Preserve FILE$(1 To __T)
    For __J = __T - 1 To POSITION
        Swap FILE$(__J), FILE$(__J + 1)
    Next __J
End Sub
Sub DeleteLine (POSITION As _Unsigned Long)
    __T = UBound(FILE$) - 1
    For __J = POSITION To __T
        Swap FILE$(__J), FILE$(__J + 1)
    Next __J
    FILE$(__T + 1) = ""
    ReDim _Preserve FILE$(1 To __T)
End Sub
Sub NewScreen (W, H)
    Screen _NewImage(W, H, 32)
    _PrintMode _KeepBackground
    VisibleLines = H \ _FontHeight - 3
End Sub
Function Max (A, B)
    Max = -A * (A > B) - B * (A <= B)
End Function
Function Min (A, B)
    Min = -A * (A < B) - B * (A >= B)
End Function
