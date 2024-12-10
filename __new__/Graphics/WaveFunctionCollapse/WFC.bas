Screen _NewImage(_DesktopWidth, _DesktopHeight, 32)

Type Tile
    As _Byte Omit 'BULDRA in bits
    As _Byte STATE
End Type

Const TILE_STATE_BLANK = 1, TILE_STATE_UP = 2, TILE_STATE_LEFT = 3, TILE_STATE_DOWN = 4, TILE_STATE_RIGHT = 5, TILE_STATE_ALL = 6

Dim As Tile Tile_Empty, Tiles(1 To 160, 1 To 90), Tile_Up, Tile_Down, Tile_Left, Tile_Right

Dim As Long TileImages(0 To 6): For I = 0 To 6: TileImages(I) = LoadImage(I): Next I

'Place a random tile at center'
Randomize Timer: RandomTile Tiles(UBound(Tiles, 1) \ 4 + CInt(Rnd * UBound(Tiles, 1) \ 2), UBound(Tiles, 2) \ 4 + CInt(Rnd * UBound(Tiles, 2) \ 2))

'Start the loop
S = 1
Do
    If S = 0 Or Stuck Then
        If S = 0 Then _Title "Finished" Else _Title "Stuck"
        T = PNG32(_Trim$(Str$(Timer)), 0, "")
        Do
            _Limit 60
            If _KeyDown(32) Then Run
            If _KeyDown(27) Then System
        Loop
    Else
        Cls
        S = 0
        For I = 1 To UBound(Tiles, 1)
            For J = 1 To UBound(Tiles, 2)
                If J > LBound(Tiles, 2) Then Tile_Up = Tiles(I, J - 1) Else Tile_Up = Tile_Empty
                If J < UBound(Tiles, 2) Then Tile_Down = Tiles(I, J + 1) Else Tile_Down = Tile_Empty
                If I < UBound(Tiles, 1) Then Tile_Right = Tiles(I + 1, J) Else Tile_Right = Tile_Empty
                If I > LBound(Tiles, 1) Then Tile_Left = Tiles(I - 1, J) Else Tile_Left = Tile_Empty
                UpdateTile Tiles(I, J), Tile_Up, Tile_Down, Tile_Left, Tile_Right
                If J > LBound(Tiles, 2) Then Tiles(I, J - 1) = Tile_Up
                If J < UBound(Tiles, 2) Then Tiles(I, J + 1) = Tile_Down
                If I < UBound(Tiles, 1) Then Tiles(I + 1, J) = Tile_Right
                If I > LBound(Tiles, 1) Then Tiles(I - 1, J) = Tile_Left
                If Tiles(I, J).STATE Then _Continue
                If TryTile(Tiles(I, J)) = 0 Then Stuck = -1
                S = S + 1
        Next J, I
    End If
    _Title "Working"
    For I = 1 To UBound(Tiles, 1): For J = 1 To UBound(Tiles, 2): _PutImage (I * 10 - 10, J * 10 - 10)-(I * 10 - 1, J * 10 - 1), TileImages(Tiles(I, J).STATE): Next J, I
    _Display
Loop
System
'--------------
Sub RandomTile (T As Tile)
    Dim __T As Tile
    T.STATE = 6
    UpdateTile T, __T, __T, __T, __T
End Sub
Sub UpdateTile (T As Tile, TU As Tile, TD As Tile, TL As Tile, TR As Tile)
    Select Case T.STATE
        Case TILE_STATE_BLANK
            TU.Omit = TU.Omit Or &B001111
            TD.Omit = TD.Omit Or &B011011
            TL.Omit = TL.Omit Or &B010111
            TR.Omit = TR.Omit Or &B011101
        Case TILE_STATE_UP
            TU.Omit = TU.Omit Or &B110000
            TD.Omit = TD.Omit Or &B011011
            TL.Omit = TL.Omit Or &B101000
            TR.Omit = TR.Omit Or &B100010
        Case TILE_STATE_DOWN
            TU.Omit = TU.Omit Or &B001111
            TD.Omit = TD.Omit Or &B100100
            TL.Omit = TL.Omit Or &B101000
            TR.Omit = TR.Omit Or &B100010
        Case TILE_STATE_LEFT
            TU.Omit = TU.Omit Or &B110000
            TD.Omit = TD.Omit Or &B100100
            TL.Omit = TL.Omit Or &B101000
            TR.Omit = TR.Omit Or &B011101
        Case TILE_STATE_RIGHT
            TU.Omit = TU.Omit Or &B110000
            TD.Omit = TD.Omit Or &B100100
            TL.Omit = TL.Omit Or &B010111
            TR.Omit = TR.Omit Or &B100010
        Case TILE_STATE_ALL
            TU.Omit = TU.Omit Or &B110000
            TD.Omit = TD.Omit Or &B100100
            TL.Omit = TL.Omit Or &B101000
            TR.Omit = TR.Omit Or &B100010
    End Select
End Sub
Function TryTile (T As Tile)
    Options$ = "BULDRA"
    SetOptions$ = ""
    If T.Omit And &B100000 Then Asc(Options$, 1) = 0
    If T.Omit And &B010000 Then Asc(Options$, 2) = 0
    If T.Omit And &B001000 Then Asc(Options$, 3) = 0
    If T.Omit And &B000100 Then Asc(Options$, 4) = 0
    If T.Omit And &B000010 Then Asc(Options$, 5) = 0
    If T.Omit And &B000001 Then Asc(Options$, 6) = 0
    For I = 1 To 6
        If Asc(Options$, I) > 0 Then SetOptions$ = SetOptions$ + Chr$(Asc(Options$, I))
    Next I
    If Len(SetOptions$) = 0 Then Exit Function
    If Len(SetOptions$) < 5 Then STATE = Asc(SetOptions$, Int(Rnd * Len(SetOptions$)) + 1)
    Select Case STATE
        Case 66: T.STATE = TILE_STATE_BLANK
        Case 85: T.STATE = TILE_STATE_UP
        Case 76: T.STATE = TILE_STATE_LEFT
        Case 68: T.STATE = TILE_STATE_DOWN
        Case 82: T.STATE = TILE_STATE_RIGHT
        Case 65: T.STATE = TILE_STATE_ALL
    End Select
    TryTile = -1
End Function
Function LoadImage& (I)
    LoadImage& = _LoadImage("WFC\" + _Trim$(Mid$("EmptyBlankUp   Left Down RightAll  ", I * 5 + 1, 5)) + ".png", 32)
End Function
Function PNG32 (__FILENAME As String, __IMAGE As Long, __PNGDATA As String)
    Dim As String __PNGHEADER, __IHDR, __IDAT, __IMAGDATA, __IEND
    Dim As String __EXT
    Dim As Long __WIDTH, __HEIGHT, __F, __SOURCE, __OFFSET, __X, __Y, __P
    __PNGHEADER = MKL$(&H474E5089) + MKL$(&H0A1A0A0D)
    __WIDTH = _Width(__IMAGE): __HEIGHT = _Height(__IMAGE)
    __IHDR = "IHDR" + ReverseMKL$(__WIDTH) + ReverseMKL$(__HEIGHT) + MKI$(&H0608) + String$(3, 0)
    __IMAGDATA = String$((__HEIGHT + 1) * __WIDTH * 4, 0)
    __OFFSET = 1
    __SOURCE = _Source
    _Source __IMAGE
    For __Y = 0 To __HEIGHT - 1
        __OFFSET = __OFFSET + 1
        For __X = 0 To __WIDTH - 1
            __P = Point(__X, __Y)
            Asc(__IMAGDATA, __OFFSET) = _SHR(__P, 16)
            Asc(__IMAGDATA, __OFFSET + 1) = _SHR(__P, 8)
            Asc(__IMAGDATA, __OFFSET + 2) = __P
            Asc(__IMAGDATA, __OFFSET + 3) = _SHR(__P, 24)
            __OFFSET = __OFFSET + 4
        Next __X
    Next __Y
    _Source __SOURCE
    __IDAT = "IDAT" + _Deflate$(__IMAGDATA)
    __PNGDATA = __PNGHEADER + ReverseMKL$(&H0D) + __IHDR + ReverseMKL$(crc32~&(__IHDR)) + ReverseMKL$(Len(__IDAT) - 4) + __IDAT + ReverseMKL$(crc32~&(__IDAT)) + String$(4, 0) + "IEND" + MKL$(&H826042AE)
    If Len(__FILENAME) Then
        __F = FreeFile
        If _StriCmp(Right$(__FILENAME, 4), ".png") Then __EXT = ".png" Else __EXT = ""
        If _FileExists(__FILENAME + __EXT) Then Kill __FILENAME + __EXT
        Open __FILENAME + __EXT For Binary As #__F
        Put #__F, , __PNGDATA
        Close #__F
    End If
    If _FileExists(__FILENAME) Then PNG32 = -1 Else PNG32 = 0
End Function
Function ReverseMKL$ (__L~&)
    Dim __RMKL As String
    __RMKL = String$(4, 0)
    Asc(__RMKL, 1) = _SHR(__L~&, 24)
    Asc(__RMKL, 2) = _SHR(__L~&, 16)
    Asc(__RMKL, 3) = _SHR(__L~&, 8)
    Asc(__RMKL, 4) = __L~&
    ReverseMKL$ = __RMKL
End Function
Function crc32~& (__IN$)
    Dim As _Unsigned Long __CRC32_POLY, __CRC, __I
    Dim As _Unsigned _Byte __J
    __CRC32_POLY = &HEDB88320
    __CRC = &HFFFFFFFF
    For __I = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(__IN$, __I)
        If __CRC And 1 Then __CRC = _SHR(__CRC, 1) Xor __CRC32_POLY Else __CRC = _SHR(__CRC, 1)
        If __CRC And 1 Then __CRC = _SHR(__CRC, 1) Xor __CRC32_POLY Else __CRC = _SHR(__CRC, 1)
        If __CRC And 1 Then __CRC = _SHR(__CRC, 1) Xor __CRC32_POLY Else __CRC = _SHR(__CRC, 1)
        If __CRC And 1 Then __CRC = _SHR(__CRC, 1) Xor __CRC32_POLY Else __CRC = _SHR(__CRC, 1)
        If __CRC And 1 Then __CRC = _SHR(__CRC, 1) Xor __CRC32_POLY Else __CRC = _SHR(__CRC, 1)
        If __CRC And 1 Then __CRC = _SHR(__CRC, 1) Xor __CRC32_POLY Else __CRC = _SHR(__CRC, 1)
        If __CRC And 1 Then __CRC = _SHR(__CRC, 1) Xor __CRC32_POLY Else __CRC = _SHR(__CRC, 1)
        If __CRC And 1 Then __CRC = _SHR(__CRC, 1) Xor __CRC32_POLY Else __CRC = _SHR(__CRC, 1)
    Next __I
    crc32~& = Not __CRC
End Function
