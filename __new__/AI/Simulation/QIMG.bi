'$Dynamic
Type QIMG_Header
    As String * 4 Signature
    As _Unsigned Long Width, Height, Frames
    As _Unsigned _Byte ColorType
    As _Unsigned _Byte Compressed
    As _Unsigned Long DataLength
End Type
Type QIMG_Sprite
    As _Unsigned Long Width, Height, Frames, Frame
    As String ImageHandle
End Type
