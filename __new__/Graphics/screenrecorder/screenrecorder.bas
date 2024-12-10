$Console:Only
DefLng A-Z
SIGBYTE$1 = Chr$(&B10101010)
Open "out.rec" For Binary As #1
Dim As String IMG1, IMG2, IMG3
Dim As Long I
IMG1 = String$(_DesktopWidth * _DesktopHeight * 4, 0)
IMG2 = IMG1
IMG3 = IMG2
IMAGE& = _ScreenImage
Const Compress = -1
Do
    _Limit 60
    Frame = Frame + 1
    If _CommandCount Then If Frame = 61 Then System
    If Timer(0.1) - T! > 1 Then
        _ConsoleTitle Str$(Frame - OLDFrame)
        OLDFrame = Frame
        T! = Timer(0.1)
    End If
    Print "Frame"; Frame
    MemCopyFromImage IMAGE&, _Offset(IMG1), Len(IMG1)
    MemCopy _Offset(IMG1), _Offset(IMG3), Len(IMG1)
    _FreeImage IMAGE&
    IMAGE& = _ScreenImage
    For I = 1 To Len(IMG1) - 4 Step 4
        If Asc(IMG1, I) = Asc(IMG2, I) And Asc(IMG1, I + 1) = Asc(IMG2, I + 2) And Asc(IMG1, I + 3) = Asc(IMG2, I + 3) And Asc(IMG1, I + 4) = Asc(IMG2, I + 4) Then
            Asc(IMG1, I) = 0: Asc(IMG1, I + 1) = 0: Asc(IMG1, I + 2) = 0: Asc(IMG1, I + 3) = 0
        End If
    Next I
    If Compress Then
        O$ = OneByteEncode$(IMG1)
    Else
        O$ = IMG1
    End If
    S$4 = MKL$(Len(O$))
    Put #1, , SIGBYTE$1
    Put #1, , S$4
    Put #1, , O$
    MemCopy _Offset(IMG3), _Offset(IMG2), Len(IMG1)
Loop
System
Function Remain~& (A~&, B~&)
    Remain~& = A~& \ B~& + Sgn(A~& Mod B~&)
End Function
Sub MemCopy (__S As _Offset, __D As _Offset, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _Mem(__S, __SIZE): __M2 = _Mem(__D, __SIZE)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
Sub MemCopyFromImage (__S As Long, __D As _Offset, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _MemImage(__S): __M2 = _Mem(__D, __SIZE)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
Function ZeroByteEncode$ (__I$)
    Dim As _Unsigned _Byte __C
    Dim As _Unsigned Long __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET, __I
    Dim __J As _Unsigned _Bit * 3
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER
    __BYTE_BUFFER = String$(Len(__I$), 0): __POSITION_BUFFER = String$(Remain(Len(__I$), 8) + 1, 0)
    For __I = 1 To Len(__I$)
        __C = Asc(__I$, __I): If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If __C Then
            Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET) = _SetBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J)
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1: Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = __C
        End If
        __J = __J + 1
    Next __I
    ZeroByteEncode$ = MKL$(Len(__I$)) + MKL$(__POSITION_BUFFER_OFFSET) + MKL$(__BYTE_BUFFER_OFFSET) + Left$(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET) + Left$(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET)
    __POSITION_BUFFER = ""
    __BYTE_BUFFER = ""
    Exit Function
End Function
Function RLEEncode$ (__I$)
    Dim As _Unsigned _Byte __CB, __LB, __C
    Dim As Long __I
    Dim As String __OUT_BUFFER
    __OUT_BUFFER = String$(Len(__I$) * 2, 0)
    __LB = Asc(__I$, 1)
    __C = 1
    For __I = 2 To Len(__I$)
        __CB = Asc(__I$, __I)
        If __CB = __LB And __C < 255 Then
            __C = __C + 1
        Else
            __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __OUT_BUFFER_OFFSET) = __LB
            __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __OUT_BUFFER_OFFSET) = __C
            __C = 1
            __LB = __CB
        End If
    Next __I
    __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + 1
    Asc(__OUT_BUFFER, __OUT_BUFFER_OFFSET) = __LB
    __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + 1
    Asc(__OUT_BUFFER, __OUT_BUFFER_OFFSET) = __C
    If 5 + __OUT_BUFFER_OFFSET > Len(__I$) Then
        RLEEncode$ = Chr$(0) + __I$
    Else
        RLEEncode$ = Chr$(1) + MKL$(Len(__I$)) + Left$(__OUT_BUFFER, __OUT_BUFFER_OFFSET + 1)
    End If
    __OUT_BUFFER = ""
End Function
Function OneByteEncode$ (__I$)
    Dim As _Unsigned _Byte __ONEBYTE, __C
    Dim As _Unsigned Long __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET, __I, __LENA, __Frequency_Table(0 To 255)
    Dim __J As _Unsigned _Bit * 3
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER
    __LENA = Len(__I$)
    For __I = 1 To __LENA
        __BYTE~%% = Asc(__I$, __I)
        __Frequency_Table(__BYTE~%%) = __Frequency_Table(__BYTE~%%) + 1
    Next __I
    For __BI~%% = 0 To 255
        If __Frequency_Table(__BI~%%) > __Frequency_Table(__ONEBYTE) Then __ONEBYTE = __BI~%%
    Next __BI~%%
    __BYTE_BUFFER = String$(Len(__I$), 0): __POSITION_BUFFER = String$(Remain(Len(__I$), 8) + 1, 0)
    For __I = 1 To Len(__I$)
        __C = Asc(__I$, __I): If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If __C <> __ONEBYTE Then
            Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET) = _SetBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J)
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1: Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = __C
        End If
        __J = __J + 1
    Next __I
    __POSITION_BUFFER = _Deflate$(Left$(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET))
    __BYTE_BUFFER = _Deflate$(Left$(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET))
    OneByteEncode$ = MKL$(Len(__I$)) + MKL$(Len(__POSITION_BUFFER)) + MKL$(Len(__BYTE_BUFFER)) + Chr$(__ONEBYTE) + __POSITION_BUFFER + __BYTE_BUFFER
    __POSITION_BUFFER = ""
    __BYTE_BUFFER = ""
    Exit Function
End Function
