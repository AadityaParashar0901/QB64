Screen _NewImage(64, 64, 32): _Title "Counter"
_ScreenMove 16, 900 - 128
Dim As Long N, LIMIT
Input LIMIT
O& = _NewImage(16, 16, 32)
_Dest O&
Color _RGB32(0), _RGB32(255)
Do
    _Dest O&
    Cls , _RGB32(255)
    N = N + 1
    _PrintString (0, 0), _Trim$(Str$(N))
    If N = LIMIT Then Sound 200, 36: Exit Do
    _PutImage (0, 0)-(63, 63), O&, 0, (0, 0)-(15, 15)
    _Dest 0
    A$ = Input$(1)
    If A$ = " " Then _Continue
    If A$ = "." Then N = N - 1
    N = N - 1
Loop Until A$ = Chr$(27)
_FreeImage O&
System
