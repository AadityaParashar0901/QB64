'$Include:'QIMG.bm'
Function QIMG_LoadSpriteFromFile& (FileName$)
    Dim QIMGH As QIMG_Header
    __F = FreeFile
    Open FileName$ For Binary As #__F
    Get #1, , QIMGH
    IMGDATA$ = String$(QIMGH.DataLength, 0)
    Get #1, , IMGDATA$
    Close #__F
    QIMG_LoadSpriteFromFile& = QIMG_LoadSprite(QIMGH, IMGDATA$)
End Function
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
Sub QIMG_PutSprite (__I As _Unsigned Long, __F As _Unsigned Integer, X As _Unsigned Integer, Y As _Unsigned Integer)
    If __F Then
        _PutImage (X, Y), CVL(Mid$(QIMG_Sprites(__I).ImageHandle, __F * 4 - 3, 4))
    Else
        _PutImage (X, Y), CVL(Mid$(QIMG_Sprites(__I).ImageHandle, QIMG_Sprites(__I).Frame * 4 - 3, 4))
        If QIMG_Sprites(__I).Frame = QIMG_Sprites(__I).Frames Then QIMG_Sprites(__I).Frame = 1 Else QIMG_Sprites(__I).Frame = QIMG_Sprites(__I).Frame + 1
    End If
End Sub
Sub QIMG_UnLoadSprite (__I As _Unsigned Long)
    For I = 1 To QIMG_Sprites(__I).Frames
        _FreeImage CVL(Mid$(QIMG_Sprites(__I).ImageHandle, I * 4 - 3, 4))
    Next I
End Sub
