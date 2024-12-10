$Console:Only
For I = 1 To _CommandCount
    IMG& = _LoadImage(Command$(I), 32)
    _Source IMG&
    _Dest IMG&
    For K = 0 To _Width - 1
        For J = 0 To _Height - 1
            If Point(K, J) = _RGB32(34, 177, 76) Then PSet (K, J), _RGB32(255)
    Next J, K
    _Source _Console
    _Dest _Console
    T = PNG32(Command$(I), IMG&, "")
    _FreeImage IMG&
Next I
'$Include:'files\image.bm'
