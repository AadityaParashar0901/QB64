'Function Compress$ (A$)
'    T$ = " !@#$%^&()ABCDEFGHIJKLMNOPQRSTUVWXYZ,.';{}[]+=-_1234567890`~"
'    B$ = String$(Len(A$) * 6, 0)
'    For I = 1 To Len(A$)
'        B~%% = Asc(A$, I)
'        If B~%% >= 97 And B~%% <= 122 Then B~%% = B~%% - 32
'        B~%% = InStr(T$, Chr$(B~%%))
'        If B~%% = 0 Then Print "ERROR COMPRESSING BYTE"; B~%%; "AT POSITION"; I; "OF "; A$: System
'        Mid$(B$, I * 6 - 5, 6) = Byte2Bits$6(B~%%)
'    Next I
'    I = I * 6 - 6
'    I = _SHR(I, 3) + Sgn(I - _SHL(_SHR(I, 3), 3))
'    O$ = String$(I, 0)
'    For J = 1 To I
'        Asc(O$, J) = Val("&B" + Mid$(B$, J * 8 - 7, 8))
'    Next J
'    Compress$ = O$
'    B$ = ""
'    O$ = ""
'End Function
'Function Decompress$ (A$)
'    T$ = " !@#$%^&()ABCDEFGHIJKLMNOPQRSTUVWXYZ,.';{}[]+=-_1234567890`~"
'    B$ = String$(Len(A$) * 8, 0)
'    For I = 1 To Len(A$)
'        Mid$(B$, I * 8 - 7, 8) = Byte2Bits8$8(Asc(A$, I))
'    Next I
'    L~%% = Len(B$) \ 6
'    O$ = String$(L~%%, 0)
'    For I = 1 To L~%%
'        Print Val("&B00" + Mid$(B$, I * 6 - 5, 6));
'        Asc(O$, I) = Asc(T$, Val("&B00" + Mid$(B$, I * 6 - 5, 6)))
'    Next I
'    Decompress$ = O$
'    B$ = ""
'    O$ = ""
'End Function
'Function Byte2Bits$6 (A~%%)
'    Static O$6
'    Dim I As _Unsigned _Byte
'    B~%% = A~%%
'    While B~%% And I < 6
'        I = I + 1
'        Asc(O$6, I) = 48 + (B~%% And 1)
'        B~%% = _SHR(B~%%, 1)
'    Wend
'    Byte2Bits$6 = O$6
'End Function
'Function Byte2Bits8$8 (A~%%)
'    Static O$8
'    Dim I As _Unsigned _Byte
'    B~%% = A~%%
'    While B~%% And I < 6
'        I = I + 1
'        Asc(O$8, I) = 48 + (B~%% And 1)
'        B~%% = _SHR(B~%%, 1)
'    Wend
'    Byte2Bits8$8 = O$8
'End Function
Function DoubleTrim$ (A$, N)
    DoubleTrim$ = Mid$(A$, N + 1, Len(A$) - N * 2)
End Function
Function Remain~& (A~&, B~&)
    Remain~& = A~& \ B~& + Sgn(A~& Mod B~&)
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
