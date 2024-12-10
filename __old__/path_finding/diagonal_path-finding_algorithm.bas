DefInt A-Z
Screen _NewImage(640, 640, 32)
Dim Shared As _Unsigned Integer Grid(1 To 40, 1 To 40), Grid2(1 To 40, 1 To 40)
Dim As _Unsigned Integer StartPoint_X, StartPoint_Y, EndPoint_X, EndPoint_Y
Dim As Integer X, Y, L
StartPoint_X = 10
StartPoint_Y = 10
EndPoint_X = 25
EndPoint_Y = 20
Dim Shared Queue$
DISPLAY = -1
Do
    _Limit 60
    While _MouseInput: Wend
    If _KeyDown(49) Then
        StartPoint_X = _MouseX \ 16 + 1
        StartPoint_Y = _MouseY \ 16 + 1
        DISPLAY = -1
    End If
    If _KeyDown(50) Then
        EndPoint_X = _MouseX \ 16 + 1
        EndPoint_Y = _MouseY \ 16 + 1
        DISPLAY = -1
    End If
    If _MouseButton(1) Then
        Grid(_MouseX \ 16 + 1, _MouseY \ 16 + 1) = 0
        DISPLAY = -1
    End If
    If _MouseButton(2) Then
        Grid(_MouseX \ 16 + 1, _MouseY \ 16 + 1) = -1
        DISPLAY = -1
    End If
    If DISPLAY = 0 Then _Continue
    Cls
    For I = LBound(Grid2, 1) To UBound(Grid2, 1): For J = LBound(Grid2, 2) To UBound(Grid, 2)
            Grid2(I, J) = Grid(I, J)
    Next J, I
    Queue$ = ListNew$
    Grid2(EndPoint_X, EndPoint_Y) = 1
    QueueAdd EndPoint_X, EndPoint_Y
    Do
        If ListLength(Queue$) = 0 Then Exit Do
        QueueRemove X, Y
        If X < UBound(Grid2, 1) Then If Grid2(X + 1, Y) = 0 Then QueueAdd X + 1, Y: Grid2(X + 1, Y) = Grid2(X, Y) + 1
        If X > LBound(Grid2, 1) Then If Grid2(X - 1, Y) = 0 Then QueueAdd X - 1, Y: Grid2(X - 1, Y) = Grid2(X, Y) + 1
        If Y < UBound(Grid2, 2) Then If Grid2(X, Y + 1) = 0 Then QueueAdd X, Y + 1: Grid2(X, Y + 1) = Grid2(X, Y) + 1
        If Y > LBound(Grid2, 2) Then If Grid2(X, Y - 1) = 0 Then QueueAdd X, Y - 1: Grid2(X, Y - 1) = Grid2(X, Y) + 1
    Loop
    For X = LBound(Grid2, 1) To UBound(Grid2, 1): For Y = LBound(Grid2, 2) To UBound(Grid2, 2)
            If Grid2(X, Y) = 65535 Then Line (X * 16 - 16, Y * 16 - 16)-(X * 16, Y * 16), _RGB32(255), BF
    Next Y, X
    CPX = StartPoint_X
    CPY = StartPoint_Y
    For I = 0 To 32767
        Line (CPX * 16 - 16, CPY * 16 - 16)-(CPX * 16, CPY * 16), _RGB32(191, 191, 0), BF
        If CPX < UBound(Grid2, 1) Then If Grid2(CPX + 1, CPY) < Grid2(CPX, CPY) Then CPX = CPX + 1
        If CPX > LBound(Grid2, 1) Then If Grid2(CPX - 1, CPY) < Grid2(CPX, CPY) Then CPX = CPX - 1
        If CPY < UBound(Grid2, 2) Then If Grid2(CPX, CPY + 1) < Grid2(CPX, CPY) Then CPY = CPY + 1
        If CPY > LBound(Grid2, 2) Then If Grid2(CPX, CPY - 1) < Grid2(CPX, CPY) Then CPY = CPY - 1
        If CPX = EndPoint_X And CPY = EndPoint_Y Then Exit For
    Next I
    Line (StartPoint_X * 16 - 16, StartPoint_Y * 16 - 16)-(StartPoint_X * 16, StartPoint_Y * 16), _RGB32(0, 255, 0), BF
    Line (EndPoint_X * 16 - 16, EndPoint_Y * 16 - 16)-(EndPoint_X * 16, EndPoint_Y * 16), _RGB32(255, 0, 0), BF
    _Display
    DISPLAY = 0
Loop Until Inp(&H60) = 1
System
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
