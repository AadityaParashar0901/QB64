$Console
_Console On

Type Block
    As _Unsigned _Byte Type
    As _Unsigned Long Life
    As _Unsigned _Byte Corrupt
    As String Code
    As _Unsigned Integer NextPointer1, NextPointer2, NextPointer3
End Type

Dim As Block PlayerBlocks(1 To 1)

Screen _NewImage(640, 480, 32)


