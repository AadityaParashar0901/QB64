Function QIMG_Save$ (IMG&(), StartFrame&, EndFrame&)
    Dim QIMG_H As QIMG_Header, M As _MEM, S As _MEM, I As _Unsigned Long
    QIMG_H.Signature = "QIMG"
    QIMG_H.Width = _Width(IMG&(StartFrame&))
    QIMG_H.Height = _Height(IMG&(StartFrame&))
    QIMG_H.Frames = EndFrame& - StartFrame& + 1
    QIMG_H.ColorType = 32
    For I = StartFrame& To EndFrame&
        If IMG&(I) >= -1 Then QIMG_H.Frames = QIMG_H.Frames - 1: _Continue
        M = _MemImage(IMG&(I))
        IMGDATA$ = String$(M.SIZE, 0)
        S = _Mem(_Offset(IMGDATA$), M.SIZE)
        _MemCopy M, M.OFFSET, M.SIZE To S, S.OFFSET
        _MemFree M
        _MemFree S
        QIMGDATA$ = QIMGDATA$ + IMGDATA$
    Next I
    IMGDATA$ = ""
    QIMGDATA$ = OneByteEncode$(QIMGDATA$)
    QIMG_H.DataLength = Len(QIMGDATA$)
    QIMG_H.Compressed = 2
    M = _Mem(QIMG_H)
    QIMG_H$ = String$(M.SIZE, 0)
    S = _Mem(_Offset(QIMG_H$), M.SIZE)
    _MemCopy M, M.OFFSET, M.SIZE To S, S.OFFSET
    _MemFree M
    _MemFree S
    QIMG_Save$ = QIMG_H$ + QIMGDATA$
    QIMG_H$ = ""
    QIMGDATA$ = ""
End Function
Sub QIMG_Load (QIMG_H As QIMG_Header, IMGDATA$, IMG&())
    Dim I As _Unsigned Long, M As _MEM, S As _MEM, __OFFSET As _Offset
    If QIMG_H.Signature <> "QIMG" Then Print "This isn't a QIMG File"
    QIMGDATA$ = OneByteDecode$(IMGDATA$)
    __OFFSET = _Offset(QIMGDATA$)
    For I = 1 To QIMG_H.Frames
        IMG&(I) = _NewImage(QIMG_H.Width, QIMG_H.Height, QIMG_H.ColorType)
        M = _MemImage(IMG&(I))
        S = _Mem(__OFFSET, M.SIZE)
        __OFFSET = __OFFSET + M.SIZE
        _MemCopy S, S.OFFSET, S.SIZE To M, M.OFFSET
        _MemFree M
        _MemFree S
    Next I
    QIMGDATA$ = ""
End Sub
Function OneByteEncode$ (__I$): Dim As _Unsigned _Byte __ONEBYTE, __C: Dim As _Unsigned Long __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET, __I, __LENA, __Frequency_Table(0 To 255): Dim __J As _Unsigned _Bit * 3: Dim As String __BYTE_BUFFER, __POSITION_BUFFER: __LENA = Len(__I$)
    For __I = 1 To __LENA: __BYTE~%% = Asc(__I$, __I): __Frequency_Table(__BYTE~%%) = __Frequency_Table(__BYTE~%%) + 1: Next __I
    For __BI~%% = 0 To 255: If __Frequency_Table(__BI~%%) > __Frequency_Table(__ONEBYTE) Then __ONEBYTE = __BI~%%
    Next __BI~%%
    __BYTE_BUFFER = String$(Len(__I$), 0): __POSITION_BUFFER = String$(Remain(Len(__I$), 8) + 1, 0)
    For __I = 1 To Len(__I$): __C = Asc(__I$, __I): If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If __C <> __ONEBYTE Then Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET) = _SetBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J): __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1: Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = __C
    __J = __J + 1: Next __I
__POSITION_BUFFER = _Deflate$(Left$(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET)): __BYTE_BUFFER = _Deflate$(Left$(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET)): OneByteEncode$ = MKL$(Len(__I$)) + MKL$(Len(__POSITION_BUFFER)) + MKL$(Len(__BYTE_BUFFER)) + Chr$(__ONEBYTE) + __POSITION_BUFFER + __BYTE_BUFFER: __POSITION_BUFFER = "": __BYTE_BUFFER = "": End Function
Function OneByteDecode$ (__I$): Dim As _Unsigned Long __I, __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET: Dim As _Unsigned _Bit * 3 __J: Dim As String __BYTE_BUFFER, __POSITION_BUFFER, __OUT_BUFFER
    __OUT_LENGTH~& = CVL(Left$(__I$, 4)): __POSITION_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 5, 4)): __BYTE_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 9, 4)): __ONEBYTE~%% = Asc(__I$, 13): __POSITION_BUFFER = _Inflate$(Mid$(__I$, 14, __POSITION_BUFFER_DEFLATE_LENGTH~&)): __BYTE_BUFFER = _Inflate$(Mid$(__I$, 14 + __POSITION_BUFFER_DEFLATE_LENGTH~&, __BYTE_BUFFER_DEFLATE_LENGTH~&)): __OUT_BUFFER = String$(__OUT_LENGTH~&, 0): __POSITION_BUFFER_OFFSET = 0: __BYTE_BUFFER_OFFSET = 0
    For __I = 1 To __OUT_LENGTH~&: If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If _ReadBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J) Then __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1: Asc(__OUT_BUFFER, __I) = Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) Else Asc(__OUT_BUFFER, __I) = __ONEBYTE~%%
    __J = __J + 1: Next __I
__POSITION_BUFFER = "": __BYTE_BUFFER = "": OneByteDecode = __OUT_BUFFER: End Function
Function Remain~& (A~&, B~&): Remain~& = A~& \ B~& + Sgn(A~& Mod B~&): End Function
Function QIMG_LoadSpriteFromFile& (FileName$, FPS): Dim QIMGH As QIMG_Header
    __F = FreeFile: Open FileName$ For Binary As #__F: Get #1, , QIMGH: IMGDATA$ = String$(QIMGH.DataLength, 0): Get #1, , IMGDATA$: Close #__F
QIMG_LoadSpriteFromFile& = QIMG_LoadSprite(QIMGH, IMGDATA$, FPS): End Function
Function QIMG_LoadSprite& (QIMG_H As QIMG_Header, IMGDATA$, FPS): Dim __I&, IMG&(1 To QIMG_H.Frames): __I& = UBound(QIMG_Sprites) + 1: ReDim _Preserve QIMG_Sprites(1 To __I&) As QIMG_Sprite
    QIMG_Sprites(__I&).Width = QIMG_H.Width: QIMG_Sprites(__I&).Height = QIMG_H.Height: QIMG_Sprites(__I&).Frames = QIMG_H.Frames: QIMG_Sprites(__I&).Frame = 1
    QIMG_Load QIMG_H, IMGDATA$, IMG&()
    QIMG_Sprites(__I&).ImageHandle = String$(QIMG_H.Frames * 4, 0): For I = 1 To QIMG_H.Frames: Mid$(QIMG_Sprites(__I&).ImageHandle, I * 4 - 3, 4) = MKL$(IMG&(I)): Next I
    QIMG_Sprites(__I&).TotalDelay = FPS / QIMG_Sprites(__I&).Frames
QIMG_LoadSprite& = __I&: End Function
Sub QIMG_PutSprite (__I As _Unsigned Long, __F As _Unsigned Integer, X As _Unsigned Integer, Y As _Unsigned Integer): If __F Then
        _PutImage (X, Y), CVL(Mid$(QIMG_Sprites(__I).ImageHandle, __F * 4 - 3, 4))
    Else
        _PutImage (X, Y), CVL(Mid$(QIMG_Sprites(__I).ImageHandle, QIMG_Sprites(__I).Frame * 4 - 3, 4))
        If QIMG_Sprites(__I).Delay = QIMG_Sprites(__I).TotalDelay Then
            If QIMG_Sprites(__I).Frame = QIMG_Sprites(__I).Frames Then QIMG_Sprites(__I).Frame = 1 Else QIMG_Sprites(__I).Frame = QIMG_Sprites(__I).Frame + 1
            QIMG_Sprites(__I).Delay = 0
        Else
            QIMG_Sprites(__I).Delay = QIMG_Sprites(__I).Delay + 1
        End If
End If: End Sub
Sub QIMG_PutRotatedSprite (__I As _Unsigned Long, __F As _Unsigned Integer, X As _Unsigned Integer, Y As _Unsigned Integer, __T As _Unsigned Integer, S As _Unsigned Integer)
    If X < 0 Or X > _Width(_Dest) Or Y < 0 Or Y > _Height(_Dest) Then Exit Sub
    Dim As Single __Theta, __CT, __ST, __X1, __Y1, __X2, __Y2, __X3, __Y3, __X4, __Y4
    __Theta = _D2R(__T): __CT = Cos(__Theta): __ST = Sin(__Theta)
    __S = S / 2
    __X1 = -__S * __CT + -__S * __ST: __Y1 = --__S * __ST + -__S * __CT
    __X2 = __S * __CT + -__S * __ST: __Y2 = -__S * __ST + -__S * __CT
    __X3 = __S * __CT + __S * __ST: __Y3 = -__S * __ST + __S * __CT
    __X4 = -__S * __CT + __S * __ST: __Y4 = --__S * __ST + __S * __CT
    If __F Then
        __H& = CVL(Mid$(QIMG_Sprites(__I).ImageHandle, __F * 4 - 3, 4))
        _MapTriangle (0, 0)-(QIMG_Sprites(__I).Width - 1, 0)-(QIMG_Sprites(__I).Width - 1, QIMG_Sprites(__I).Height - 1), __H& To(X + __X1, Y + __Y1)-(X + __X2, Y + __Y2)-(X + __X3, Y + __Y3)
        _MapTriangle (0, 0)-(0, QIMG_Sprites(__I).Height - 1)-(QIMG_Sprites(__I).Width - 1, QIMG_Sprites(__I).Height - 1), __H& To(X + __X1, Y + __Y1)-(X + __X4, Y + __Y4)-(X + __X3, Y + __Y3)
    Else
        __H& = CVL(Mid$(QIMG_Sprites(__I).ImageHandle, QIMG_Sprites(__I).Frame * 4 - 3, 4))
        _MapTriangle (0, 0)-(QIMG_Sprites(__I).Width - 1, 0)-(QIMG_Sprites(__I).Width - 1, QIMG_Sprites(__I).Height - 1), __H& To(X + __X1, Y + __Y1)-(X + __X2, Y + __Y2)-(X + __X3, Y + __Y3)
        _MapTriangle (0, 0)-(0, QIMG_Sprites(__I).Height - 1)-(QIMG_Sprites(__I).Width - 1, QIMG_Sprites(__I).Height - 1), __H& To(X + __X1, Y + __Y1)-(X + __X4, Y + __Y4)-(X + __X3, Y + __Y3)
        If QIMG_Sprites(__I).Delay = QIMG_Sprites(__I).TotalDelay Then
            If QIMG_Sprites(__I).Frame = QIMG_Sprites(__I).Frames Then QIMG_Sprites(__I).Frame = 1 Else QIMG_Sprites(__I).Frame = QIMG_Sprites(__I).Frame + 1
            QIMG_Sprites(__I).Delay = 0
        Else
            QIMG_Sprites(__I).Delay = QIMG_Sprites(__I).Delay + 1
        End If
    End If
End Sub
Sub QIMG_UnLoadSprite (__I As _Unsigned Long): For I = 1 To QIMG_Sprites(__I).Frames: _FreeImage CVL(Mid$(QIMG_Sprites(__I).ImageHandle, I * 4 - 3, 4)): Next I: End Sub
