$Console:Only
For FILEN = 1 To _CommandCount
    If _FileExists(Command$(FILEN)) = 0 Then Print "File doesn't exists.": System
    IMAGE& = _LoadImage(Command$(FILEN), 32)
    If IMAGE& = -1 Then Print "File cannot be opened.": System
    Open Command$(FILEN) + ".bi" For Output As #1
    FunctionName$ = Left$(Command$(FILEN), _InStrRev(Command$(FILEN), ".") - 1)
    FunctionName$ = Right$(FunctionName$, Len(FunctionName$) - _InStrRev(FunctionName$, "\"))
    Print #1, "FUNCTION load_" + FunctionName$ + "&"
    Print #1, "O& = _NEWIMAGE(" + _Trim$(Str$(_Width(IMAGE&))) + ", " + _Trim$(Str$(_Height(IMAGE&))) + ", 32)"
    Print #1, "__Dest = _DEST: _Dest O&"
    Print #1, "RESTORE " + FunctionName$ + "_data"
    Print #1, "FOR __X = 0 TO " + _Trim$(Str$(_Width(IMAGE&) - 1)) + ": FOR __Y = 0 TO " + _Trim$(Str$(_Height(IMAGE&)))
    Print #1, "READ __P&"
    Print #1, "PSET (__X, __Y), __P&"
    Print #1, "NEXT __Y, __X"
    Print #1, "_DEST __Dest"
    Print #1, "load_" + FunctionName$ + " = O&"
    Print #1, "EXIT FUNCTION"
    Print #1, FunctionName$ + "_data:"
    _Source IMAGE&
    For I = 0 To _Width(IMAGE&) - 1
        Line$ = "DATA "
        For J = 0 To _Height(IMAGE&) - 1
            P& = Point(I, J)
            Line$ = Line$ + "&H" + Hex$(P&)
            If J < _Height(IMAGE&) Then Line$ = Line$ + ", "
        Next J
        Print #1, Line$
    Next I
    Print #1, "END FUNCTION"
    Close #1
Next FILEN
System
