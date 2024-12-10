'$Include:'qimg.bi'
Dim Shared QIMG_Sprites(0) As QIMG_Sprite
Dim QIMG_H As QIMG_Header
Dim IMG&(0)
If _StriCmp(Command$(1), "L") = 0 Then MODE = 1
If _StriCmp(Command$(1), "S") = 0 Then MODE = 2
If _StriCmp(Command$(1), "D") = 0 Then MODE = 3
Select Case MODE
    Case 1
        For I = 2 To _CommandCount
            Open Command$(I) For Binary As #1
            Get #1, , QIMG_H
            IMGDATA$ = String$(QIMG_H.DataLength, 0)
            Get #1, , IMGDATA$
            Close #1
            ReDim _Preserve IMG&(1 To QIMG_H.Frames)
            QIMG_Load QIMG_H, IMGDATA$, IMG&()
            For I = 1 To QIMG_H.Frames
                FILE$ = _Trim$(Str$(I))
                FILE$ = String$(4 - Len(FILE$), 48) + FILE$
                T = PNG32(FILE$ + ".png", IMG&(I), "")
                _FreeImage IMG&(I)
            Next I
        Next I
    Case 2
        Dim As _MEM M, S
        QIMG_H.Signature = "QIMG": QIMG_H.Frames = _CommandCount - 1: QIMG_H.ColorType = 32
        For I = 2 To _CommandCount
            IMG& = _LoadImage(Command$(I), 32)
            If IMG& >= -1 Then Print "Cannot load image:"; Command$(I): QIMG_H.Frames = QIMG_H.Frames - 1: _Continue
            QIMG_H.Width = _Width(IMG&): QIMG_H.Height = _Height(IMG&)
            M = _MemImage(IMG&)
            IMGDATA$ = String$(M.SIZE, 0)
            S = _Mem(_Offset(IMGDATA$), M.SIZE)
            _MemCopy M, M.OFFSET, M.SIZE To S, S.OFFSET
            _MemFree M
            _MemFree S
            _FreeImage IMG&
            QIMGDATA$ = QIMGDATA$ + IMGDATA$
        Next I
        IMGDATA$ = ""
        QIMGDATA$ = OneByteEncode$(QIMGDATA$)
        QIMG_H.DataLength = Len(QIMGDATA$)
        QIMG_H.Compressed = 2
        If _FileExists(Command$(2) + ".qimg") Then Kill Command$(I) + ".qimg"
        Open Command$(2) + ".qimg" For Binary As #1
        Put #1, , QIMG_H
        Put #1, , QIMGDATA$
        Close #1
    Case 3
        Open Command$(3) For Binary As #1
        Get #1, , QIMG_H
        IMGDATA$ = String$(QIMG_H.DataLength, 0)
        Get #1, , IMGDATA$
        Close #1
        ReDim _Preserve IMG&(1 To QIMG_H.Frames)
        T& = QIMG_LoadSprite&(QIMG_H, IMGDATA$)
        F& = Val(Command$(2))
        Screen _NewImage(QIMG_H.Width, QIMG_H.Height, QIMG_H.ColorType)
        Do
            For I = 1 To QIMG_H.Frames
                _Limit F&
                Cls
                QIMG_PutSprite T&, 0, 0
                _Display
                If Inp(&H60) = 1 Then Exit Do
            Next I
        Loop
        QIMG_UnLoadSprite T&
End Select
System
'$Include:'qimg.bm'
Function QIMG_LoadSprite& (QIMG_H As QIMG_Header, IMGDATA$)
    Dim __I&
    Dim IMG&(1 To QIMG_H.Frames)
    __I& = UBound(QIMG_Sprites) + 1
    ReDim _Preserve QIMG_Sprites(1 To __I&) As QIMG_Sprite
    QIMG_Sprites(__I&).Width = QIMG_H.Width
    QIMG_Sprites(__I&).Height = QIMG_H.Height
    QIMG_Sprites(__I&).Frames = QIMG_H.Frames
    QIMG_Sprites(__I&).Frame = 1
    QIMG_Load QIMG_H, IMGDATA$, IMG&()
    QIMG_Sprites(__I&).ImageHandle = String$(QIMG_H.Frames * 4, 0)
    For I = 1 To QIMG_H.Frames
        Mid$(QIMG_Sprites(__I&).ImageHandle, I * 4 - 3, 4) = MKL$(IMG&(I))
    Next I
    QIMG_LoadSprite& = __I&
End Function
Sub QIMG_PutSprite (__I As _Unsigned Long, X As _Unsigned Integer, Y As _Unsigned Integer)
    _PutImage (X, Y), CVL(Mid$(QIMG_Sprites(__I).ImageHandle, QIMG_Sprites(__I).Frame * 4 - 3, 4))
    If QIMG_Sprites(__I).Frame = QIMG_Sprites(__I).Frames Then QIMG_Sprites(__I).Frame = 1 Else QIMG_Sprites(__I).Frame = QIMG_Sprites(__I).Frame + 1
End Sub
Sub QIMG_UnLoadSprite (__I As _Unsigned Long)
    For I = 1 To QIMG_Sprites(__I).Frames
        _FreeImage CVL(Mid$(QIMG_Sprites(__I).ImageHandle, I * 4 - 3, 4))
    Next I
End Sub
