Dim Shared As Long Image(1 To 453, 1 To 150), Empty_Pixel
Empty_Pixel = _RGBA32(0, 0, 0, 0)
Screen _NewImage(452, 149, 32)
RED& = &H00FF0000
GREEN& = _RGBA32(0, 255, 0, 0)
BLUE& = &H000000FF
ALPHA& = &HFF000000
GOOGLE& = _LoadImage("Google_Dark_Dynamic.png", 32)
If _DirExists("part0") Then Shell "rd /s /q part0"
MkDir "part0"
ChDir "part0"
For X = 0 - 120 To 457 + 120 Step 8
    For XX = 1 To 453
        For YY = 1 To 150
            Image(XX, YY) = Empty_Pixel
    Next YY, XX
    _ClearColor _RGB32(0)
    _Limit 60
    _Source GOOGLE&
    For XX = 1 To 453
        For YY = 1 To 150
            Image(XX, YY) = Point(XX, YY)
    Next YY, XX
    _Source 0
    For I = X To X + 30
        DrawCircleFilled I, 150, 2, BLUE&, 0
    Next I
    For I = X + 30 To X + 60
        DrawCircleFilled I, 150, 2, RED&, 0
    Next I
    For I = X + 60 To X + 90
        DrawCircleFilled I, 150, 2, ALPHA&, 0
    Next I
    For I = X + 90 To X + 120
        DrawCircleFilled I, 150, 2, GREEN&, 0
    Next I
    FILEI = FILEI + 1
    FILE$ = _Trim$(Str$(FILEI))
    FILE$ = String$(5 - Len(FILE$), "0") + FILE$
    _Display
    T = PNG32(FILE$, "")
Next X
System
Function InRange (A, B, C)
    If A <= B And B <= C Then InRange = -1 Else InRange = 0
End Function
Sub DrawCircle (__X, __Y, __R, __C&, __D&)
    __DEST& = _Dest
    _Dest __D&
    For __T = 1 To 360 Step _R2D(_Asin(1 / __R))
        ___X = __R * Cos(_D2R(__T)) + __X
        ___Y = __R * Sin(_D2R(__T)) + __Y
        If InRange(1, ___X, 452) And InRange(1, ___Y, 149) Then Image(___X, ___Y) = __C&
    Next __T
    _Dest __DEST&
End Sub
Sub DrawCircleFilled (__X, __Y, __R, __C&, __D&)
    For __I = 1 To __R
        DrawCircle __X, __Y, __I, __C&, __D&
    Next __I
End Sub
Function PNG32 (__FILENAME As String, __PNGDATA As String)
    Dim As String __PNGHEADER, __IHDR, __IDAT, __IMAGDATA, __IEND
    Dim As String __EXT
    Dim As Long __WIDTH, __HEIGHT, __F, __SOURCE, __OFFSET, __X, __Y, __P
    Dim As _Byte __SAVEMODE
    If Len(__FILENAME) Then __SAVEMODE = -1 Else __SAVEMODE = 0
    __PNGHEADER$ = Chr$(&H89) + Chr$(&H50) + Chr$(&H4E) + Chr$(&H47) + Chr$(&H0D) + Chr$(&H0A) + Chr$(&H1A) + Chr$(&H0A)
    __WIDTH = 452: __HEIGHT = 149
    __F = FreeFile
    If LCase$(Right$(__FILENAME, 4)) <> ".png" Then __EXT = ".png" Else __EXT = ""
    If __SAVEMODE Then Open __FILENAME + __EXT For Output As #__F: Close #__F
    If __SAVEMODE Then Open __FILENAME + __EXT For Binary As #__F
    If __SAVEMODE Then Put #__F, , __PNGHEADER$
    __IHDR = "IHDR" + Reverse$(MKL$(__WIDTH)) + Reverse$(MKL$(__HEIGHT)) + Chr$(&H08) + Chr$(&H06) + String$(3, 0)
    __IHDR = Reverse$(MKL$(&H0D)) + __IHDR + Reverse$(MKL$(crc32(__IHDR)))
    If __SAVEMODE Then Put #__F, , __IHDR
    __IMAGDATA = Space$(__HEIGHT * __WIDTH * 4 + __HEIGHT)
    __OFFSET = 1
    For __Y = 1 To __HEIGHT Step 1
        Mid$(__IMAGDATA, __OFFSET, 1) = Chr$(&H00)
        __OFFSET = __OFFSET + 1
        For __X = 1 To __WIDTH Step 1
            __P = Image(__X, __Y)
            Mid$(__IMAGDATA, __OFFSET, 4) = Chr$(_Red32(__P)) + Chr$(_Green32(__P)) + Chr$(_Blue32(__P)) + Chr$(_Alpha32(__P))
            __OFFSET = __OFFSET + 4
        Next __X
    Next __Y
    __IDAT = _Deflate$(__IMAGDATA)
    __IDAT = Reverse$(MKL$(Len(__IDAT))) + "IDAT" + __IDAT + Reverse$(MKL$(crc32("IDAT" + __IDAT)))
    If __SAVEMODE Then Put #__F, , __IDAT
    __IEND = Reverse$(MKL$(&H00)) + "IEND" + Reverse$(MKL$(&HAE426082))
    If __SAVEMODE Then Put #__F, , __IEND
    If __SAVEMODE Then Close #__F
    __PNGDATA = __PNGHEADER$ + __IHDR + __IDAT + __IEND
    If _FileExists(__FILENAME) Then PNG32 = -1 Else PNG32 = 0
End Function
Function Reverse$ (IN$)
    L = Len(IN$)
    For I = 1 To Int(L / 2)
        TMP$ = Mid$(IN$, I, 1)
        Mid$(IN$, I, 1) = Mid$(IN$, L - I + 1, 1)
        Mid$(IN$, L - I + 1, 1) = TMP$
    Next I
    Reverse$ = IN$
End Function
Function crc32~& (IN$)
    Dim As _Unsigned Long CRC32_POLY, CRC
    CRC32_POLY = &HEDB88320
    CRC = &HFFFFFFFF
    For I = 1 To Len(IN$)
        CRC = CRC Xor Asc(Mid$(IN$, I, 1))
        For J = 1 To 8
            If CRC And 1 Then
                CRC = (CRC \ 2) Xor CRC32_POLY
            Else
                CRC = CRC \ 2
            End If
        Next J
    Next I
    crc32~& = Not CRC
End Function
