$Console:Only
DefLng A-Z
J = 8
For I = 1 To _CommandCount
    IMG& = _LoadImage(Command$(I), 32)
    NIMG& = _CopyImage(IMG&, 32)
    Print Command$(I)
    _Source IMG&
    _Dest NIMG&
    Cls , _RGB32(0)
    For X = 0 To _Width(IMG&) - 1
        For Y = 0 To _Height(IMG&) - 1
            RED32Sum = _Red32(Point(X, Y))
            GREEN32Sum = _Green32(Point(X, Y))
            BLUE32Sum = _Blue32(Point(X, Y))
            Count = 0
            For XX = -J To J - 1
                For YY = -J To J - 1
                    If Not (X + XX >= 0 And X + XX < _Width(IMG&) And Y + YY >= 0 And Y + YY < _Height(IMG&)) Then _Continue
                    P& = Point(X + XX, Y + YY)
                    RED32Sum = _Red32(P&) + RED32Sum
                    GREEN32Sum = _Green32(P&) + GREEN32Sum
                    BLUE32Sum = _Blue32(P&) + BLUE32Sum
                    Count = Count + 1
            Next YY, XX
            PSet (X, Y), _RGB32(RED32Sum \ Count, GREEN32Sum \ Count, BLUE32Sum \ Count, 255)
    Next Y, X
    _Source _Console
    _Dest _Console
    Print "Saving"
    T = PNG32(Command$(I) + ".png", NIMG&, "")
    _FreeImage IMG&
    _FreeImage NIMG&
Next I
System
'$Include:'files\image.bm'
