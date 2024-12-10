Screen _NewImage(_DesktopWidth, _DesktopHeight, 32)
DefLng A-Z
SIGBYTE$1 = Chr$(&B10101010)
If _CommandCount = 0 Then FILE$ = "out.rec" Else FILE$ = Command$(1)
IMG& = _NewImage(_DesktopWidth, _DesktopHeight, 32)
Open FILE$ For Binary As #1
Do
    Frame = Frame + 1
    _Title "Decoding Frame" + Str$(Frame)
    Get #1, , TMPSIGBYTE$1
    If TMPSIGBYTE$1 <> SIGBYTE$1 Then Exit Do
    Get #1, , SIZE$4
    LENGTH~& = UCVL~&(SIZE$4)
    IMAGE$ = String$(LENGTH~&, 0)
    Get #1, , IMAGE$
    IMAGE$ = OneByteDecode$(IMAGE$)
    MemCopyToImage _Offset(IMAGE$), IMG&, Len(IMAGE$)
    IMAGE$ = ""
    _PutImage (0, 0)-(_DesktopWidth - 1, _DesktopHeight - 1), IMG&
Loop
Sleep
System
Function UCVL~& (A$4)
    UCVL~& = CVL(A$4)
End Function
Sub MemCopy (__S As _Offset, __D As _Offset, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _Mem(__S, __SIZE): __M2 = _Mem(__D, __SIZE)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
Sub MemCopyToImage (__S As _Offset, __D As Long, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _Mem(__S, __SIZE): __M2 = _MemImage(__D)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
Function ZeroByteDecode$ (__I$)
    Dim As _Unsigned Long __I, __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET
    Dim As _Unsigned _Byte __C
    Dim As _Unsigned _Bit * 3 __J
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER, __OUT_BUFFER
    __OUT_LENGTH~& = UCVL~&(Left$(__I$, 4))
    __POSITION_BUFFER_LENGTH~& = UCVL~&(Mid$(__I$, 5, 4))
    __BYTE_BUFFER_LENGTH~& = UCVL~&(Mid$(__I$, 9, 4))
    __POSITION_BUFFER = Mid$(__I$, 13, __POSITION_BUFFER_LENGTH~&)
    __BYTE_BUFFER = Mid$(__I$, 13 + __POSITION_BUFFER_LENGTH~&, __BYTE_BUFFER_LENGTH~&)
    __OUT_BUFFER = String$(__OUT_LENGTH~&, 0)
    __POSITION_BUFFER_OFFSET = 0
    __BYTE_BUFFER_OFFSET = 0
    For __I = 1 To __OUT_LENGTH~&
        If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If _ReadBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J) Then
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __I) = Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET)
        End If
        __J = __J + 1
    Next __I
    ZeroByteDecode = __OUT_BUFFER
End Function
Function RLEDecode$ (__I$)
    Dim As _Unsigned _Byte __B, __C
    Dim As Long __I, __OUT_BUFFER_OFFSET
    Dim As String __OUT_BUFFER
    If Asc(__I$, 1) = 0 Then
        RLEDecode$ = Mid$(__I$, 2)
        Exit Function
    End If
    __OUT_LENGTH~& = UCVL~&(Mid$(__I$, 2, 4))
    __OUT_BUFFER = String$(__OUT_LENGTH~&, 0)
    __OUT_BUFFER_OFFSET = 1
    For __I = 6 To Len(__I$) - 1
        __B = Asc(__I$, __I): __I = __I + 1: __C = Asc(__I$, __I)
        Mid$(__OUT_BUFFER, __OUT_BUFFER_OFFSET, __C) = String$(__C, __B)
        __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + __C
    Next __I
    RLEDecode$ = __OUT_BUFFER
    __OUT_BUFFER = ""
End Function
Function OneByteDecode$ (__I$)
    Dim As _Unsigned Long __I, __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET
    Dim As _Unsigned _Bit * 3 __J
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER, __OUT_BUFFER
    __OUT_LENGTH~& = CVL(Left$(__I$, 4))
    __POSITION_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 5, 4))
    __BYTE_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 9, 4))
    __ONEBYTE~%% = Asc(__I$, 13)
    __POSITION_BUFFER = _Inflate$(Mid$(__I$, 14, __POSITION_BUFFER_DEFLATE_LENGTH~&))
    __BYTE_BUFFER = _Inflate$(Mid$(__I$, 14 + __POSITION_BUFFER_DEFLATE_LENGTH~&, __BYTE_BUFFER_DEFLATE_LENGTH~&))
    __OUT_BUFFER = String$(__OUT_LENGTH~&, 0)
    __POSITION_BUFFER_OFFSET = 0
    __BYTE_BUFFER_OFFSET = 0
    For __I = 1 To __OUT_LENGTH~&
        If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If _ReadBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J) Then
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __I) = Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET)
        Else
            Asc(__OUT_BUFFER, __I) = __ONEBYTE~%%
        End If
        __J = __J + 1
    Next __I
    __POSITION_BUFFER = ""
    __BYTE_BUFFER = ""
    OneByteDecode = __OUT_BUFFER
End Function
