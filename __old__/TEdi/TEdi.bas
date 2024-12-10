$ExeIcon:'./TEdi.ico'
'$Dynamic
$Resize:On

Do While _Resize: Loop

Screen _NewImage(960, 540, 32)
Color -1, 0

Type Vec2
    As _Unsigned Integer X, Y
End Type
Dim As Vec2 Cursors(1 To 1)
Cursors(1).X = 1
Cursors(1).Y = 1
Cursor_Character$ = Chr$(95)
Dim Shared Lines$(1 To 1)
Dim Shared FormattedLines$(1 To 1), Colorize_StringMode As _Byte, Colorize_CommentMode As _Byte
Dim Shared Comment$, MultiLineCommentOn$, MultiLineCommentOff$
Dim Shared As Long VerticalScrollOffset, HorizontalScrollOffset, TextFrameOffset, VerticalLines, VerticalTextOffsetLines, TotalLines
Dim Shared As _Bit COLOR_MODE: COLOR_MODE = -1
Dim Shared Keywords$
Keywords$ = " alignas alignof and and_eq asm auto bitand bitor bool break case catch char char8_t char16_t char32_t class compl concept const consteval constexpr const_cast continue decltype default delete do double dynamic_cast"
Keywords$ = Keywords$ + " else enum explicit export extern false float for friend goto if inline int long mutable namespace new noexcept not not_eq nullptr operator or or_eq private protected public register reinterpret_cast"
Keywords$ = Keywords$ + " return short signed sizeof static static_assert static_cast struct switch template this thread_local throw true try typedef typeid typename union unsigned using virtual void volatile wchar_t while xor xor_eq"
Keywords$ = Keywords$ + " int8 int16 int32 int64 uint8 uint16 uint32 uint64 ifdef ifndef endif"
Comment$ = "//"
MultiLineCommentOn$ = "/*"
MultiLineCommentOff$ = "*/"
VerticalScrollOffset = 1
HorizontalScrollOffset = 1

Const LineNumbersWidth = 6
$If WIN Then
    FILESEP$ = "\"
$Else
        FILESEP$ = "/"
$End If
If _FileExists(Command$(1)) Then
    FilePath$ = Command$(1)
ElseIf _FileExists(_StartDir$ + FILESEP$ + Command$(1)) Then
    FilePath$ = _StartDir$ + FILESEP$ + Command$(1)
End If

If FilePath$ = "" Then FilePath$ = "Untitled.txt"

If _FileExists(FilePath$) Then GoSub OpenFile

If _InStrRev(FilePath$, FILESEP$) Then FileName$ = Mid$(FilePath$, _InStrRev(FilePath$, FILESEP$) + 1) Else FileName$ = FilePath$

TITLE$ = FileName$ + " - TEdi"
DirPath$ = _StartDir$ 'Left$(FilePath$, _InStrRev(FilePath$, FileName$) - 1)
If Len(DirPath$) Then If _DirExists(DirPath$) Then ChDir DirPath$

VerticalTextOffsetLines = 2
VerticalTextOffset = VerticalTextOffsetLines * _FontHeight
Height = _Height - VerticalTextOffset

DISPLAY = -1

Do
    _Limit 30
    If _Resize Then
        Screen _NewImage(_ResizeWidth, _ResizeHeight, 32)
        Color _RGB32(255), _RGB32(16)
        Height = _Height - VerticalTextOffset
    End If
    If _WindowHasFocus = 0 Then GoTo SKIPDISPLAY
    Cls , _RGB32(16)

    TextFrameOffset = 16
    HorizontalCharsVisible = (_Width - 146 - TextFrameOffset) \ _FontWidth - LineNumbersWidth - 1
    VerticalLines = Height \ _FontHeight
    TotalLines = UBound(Lines$)

    'Mouse Inputs
    LastMouseWheel = 0
    MouseWheel = 0
    While _MouseInput
        LastMouseWheel = _MouseWheel: If LastMouseWheel Then MouseWheel = MouseWheel + LastMouseWheel
        DISPLAY = -1
    Wend
    If InRange(VerticalTextOffset, _MouseY, _Height) Then
        If InRange(0, _MouseX, TextFrameOffset) And SymbolsWindow Then SymbolsWindowScrollOffset = SymbolsWindowScrollOffset + MouseWheel
        If InRange(TextFrameOffset, _MouseX, _Width - 146) Then VerticalScrollOffset = VerticalScrollOffset + MouseWheel
        If InRange(_Width - 146, _MouseX, _Width) Then VerticalScrollOffset = VerticalScrollOffset + MouseWheel * 16
        ScrollTotalLines = TotalLines - VerticalLines \ 2
        ScrollBP = Height * VerticalLines / ScrollTotalLines: ScrollBO = Height - ScrollBP: ScrollBO = ScrollBO * VerticalScrollOffset / ScrollTotalLines
        If InRange(_Width - 146, _MouseX, _Width) Then
            If _MouseButton(1) Then
                ScrollBarTemp = _MouseY - ScrollMouseY: ScrollBarTemp = ScrollBarTemp * ScrollTotalLines / Height
                VerticalScrollOffset = Max(1, Min(VerticalScrollOffset + ScrollBarTemp, ScrollTotalLines))
            End If
            ScrollMouseY = _MouseY
        End If
    End If
    '------------

    'Keyboard Inputs
    Key$ = InKey$: If Key$ <> "" Then LastKey$ = Key$
    KeyShift = _KeyDown(100304) Or _KeyDown(100303)
    KeyCtrl = _KeyDown(100306) Or _KeyDown(100305)
    KeyAlt = _KeyDown(100308) Or _KeyDown(100307)
    KeyHit = _KeyHit
    If Len(Key$) Or KeyHit Or KeyShift Or KeyCtrl Or KeyAlt Then DISPLAY = -1
    If InRange(TextFrameOffset, _MouseX, _Width - 146) And InRange(VerticalTextOffsetLines, _MouseY, _Height) And _MouseButton(1) Then
        If KeyAlt Then
            If LastMouseButton = 0 Then
                ReDim _Preserve Cursors(1 To UBound(Cursors) + 1) As Vec2
                Cursors(UBound(Cursors)).Y = Min(_MouseY \ _FontHeight + VerticalScrollOffset - VerticalTextOffsetLines, TotalLines)
                Cursors(UBound(Cursors)).X = Min((_MouseX - TextFrameOffset) \ _FontWidth - LineNumbersWidth - 1 + HorizontalScrollOffset, Len(Lines$(Cursors(UBound(Cursors)).Y)) + 1)
            Else
                Cursors(UBound(Cursors)).Y = Min(_MouseY \ _FontHeight + VerticalScrollOffset - VerticalTextOffsetLines, TotalLines)
                Cursors(UBound(Cursors)).X = Min((_MouseX - TextFrameOffset) \ _FontWidth - LineNumbersWidth - 1 + HorizontalScrollOffset, Len(Lines$(Cursors(UBound(Cursors)).Y)) + 1)
            End If
        Else
            If LastMouseButton = 0 Then
                ReDim _Preserve Cursors(1 To 1) As Vec2
                Cursors(1).Y = Min(_MouseY \ _FontHeight + VerticalScrollOffset - VerticalTextOffsetLines, TotalLines)
                Cursors(1).X = Min((_MouseX - TextFrameOffset) \ _FontWidth - LineNumbersWidth - 1 + HorizontalScrollOffset, Len(Lines$(Cursors(1).Y)) + 1)
            Else
                Cursors(1).Y = Min(_MouseY \ _FontHeight + VerticalScrollOffset - VerticalTextOffsetLines, TotalLines)
                Cursors(1).X = Min((_MouseX - TextFrameOffset) \ _FontWidth - LineNumbersWidth - 1 + HorizontalScrollOffset, Len(Lines$(Cursors(1).Y)) + 1)
            End If
        End If
    End If
    LastMouseButton = _MouseButton(1)
    Select Case KeyHit
        Case 15360 'F2
            COLOR_MODE = Not COLOR_MODE
        Case 15872 'F4
            Shell _DontWait "cmd"
        Case 16128 'F5
            GoSub SaveFile
            If _FileExists("tedi_build.bat") = 0 Then Shell "notepad " + _StartDir$ + "\tedi_build.bat"
            Shell "tedi_build.bat"
        Case 16384 'F6
            ReDim _Preserve Cursors(1 To 1) As Vec2
        Case 16640 'F7
            Shell "notepad " + _StartDir$ + "\tedi_build.bat"
        Case 16896 'F8
            Shell _DontWait "explorer " + DirPath$
    End Select
    For CursorID = LBound(Cursors) To UBound(Cursors)
        If Len(Key$) = 1 Then
            If KeyCtrl Then
                Select Case Asc(Key$)
                    Case 3: 'C
                        _Clipboard$ = Lines$(Cursors(CursorID).Y)
                    Case 11: 'K
                        If InRange(1, Cursors(CursorID).Y, TotalLines) Then
                            Lines$(Cursors(CursorID).Y) = ""
                            Cursors(CursorID).X = 1
                            RemoveLine Cursors(CursorID).Y
                            If Cursors(CursorID).Y > TotalLines - 1 Then DecrementINT Cursors(CursorID).Y
                            TotalLines = TotalLines - 1
                        End If
                    Case 12: 'L
                        Lines$(Cursors(CursorID).Y) = ""
                        Cursors(CursorID).X = 1
                    Case 13: 'M
                        Lines$(Cursors(CursorID).Y) = Lines$(Cursors(CursorID).Y - 1)
                    Case 22: 'V
                        Lines$(Cursors(CursorID).Y) = _Clipboard$
                        Cursors(CursorID).X = Len(Lines$(Cursors(CursorID).Y)) + 1
                End Select
            Else
                Select Case Asc(Key$)
                    Case 8: 'Backspace
                        If Cursors(CursorID).X > 1 Then
                            Lines$(Cursors(CursorID).Y) = Left$(Lines$(Cursors(CursorID).Y), Cursors(CursorID).X - 2) + Mid$(Lines$(Cursors(CursorID).Y), Cursors(CursorID).X)
                            DecrementINT Cursors(CursorID).X
                        ElseIf Cursors(CursorID).Y > 1 Then
                            DecrementINT Cursors(CursorID).Y
                            Cursors(CursorID).X = Len(Lines$(Cursors(CursorID).Y)) + 1
                            If Cursors(CursorID).Y < TotalLines Then Lines$(Cursors(CursorID).Y) = Lines$(Cursors(CursorID).Y) + Lines$(Cursors(CursorID).Y + 1)
                            RemoveLine Cursors(CursorID).Y + 1
                            If Cursors(CursorID).Y < VerticalScrollOffset Then VerticalScrollOffset = VerticalScrollOffset - 1
                        End If
                    Case 13: 'Enter
                        InsertLine Cursors(CursorID).Y + 1
                        Lines$(Cursors(CursorID).Y + 1) = Mid$(Lines$(Cursors(CursorID).Y), Cursors(CursorID).X)
                        Lines$(Cursors(CursorID).Y) = Left$(Lines$(Cursors(CursorID).Y), Cursors(CursorID).X - 1)
                        IncrementINT Cursors(CursorID).Y
                        If Cursors(CursorID).Y + 2 > VerticalScrollOffset + VerticalLines Then VerticalScrollOffset = VerticalScrollOffset + 1
                        Cursors(CursorID).X = 1
                        HorizontalScrollOffset = 1
                    Case 9, 32 To 126:
                        If TotalLines < Cursors(CursorID).Y Then ReDim _Preserve Lines$(1 To Cursors(CursorID).Y)
                        Lines$(Cursors(CursorID).Y) = Left$(Lines$(Cursors(CursorID).Y), Cursors(CursorID).X - 1) + Key$ + Mid$(Lines$(Cursors(CursorID).Y), Cursors(CursorID).X) 'Printable Characters
                        IncrementINT Cursors(CursorID).X
                End Select
            End If
            If CursorID = 1 Then
                VerticalScrollOffset = Max(1, Cursors(1).Y - VerticalLines \ 2)
                HorizontalScrollOffset = Max(1, Cursors(1).X - HorizontalCharsVisible \ 2)
            End If
            KeyPressTimer = Timer
            FileSaved = 0
        End If
        Select Case KeyHit
            Case 18432 'Up
                If KeyCtrl Then
                    VerticalScrollOffset = Max(VerticalScrollOffset - 1, 1)
                    If Cursors(CursorID).Y + 2 > VerticalScrollOffset + VerticalLines Then Cursors(CursorID).Y = Cursors(CursorID).Y - 1
                Else
                    If Cursors(CursorID).Y > 1 Then DecrementINT Cursors(CursorID).Y
                    Cursors(CursorID).X = Min(Len(Lines$(Cursors(CursorID).Y)) + 1, Cursors(CursorID).X)
                    If InRange(VerticalScrollOffset - 2, Cursors(CursorID).Y, VerticalScrollOffset + VerticalLines) And Cursors(CursorID).Y < VerticalScrollOffset Then VerticalScrollOffset = VerticalScrollOffset - 1
                End If
            Case 20480 'Down
                If KeyCtrl Then
                    VerticalScrollOffset = Min(VerticalScrollOffset + 1, TotalLines)
                    If Cursors(CursorID).Y < VerticalScrollOffset Then Cursors(CursorID).Y = Cursors(CursorID).Y + 1
                Else
                    If Cursors(CursorID).Y < TotalLines Then IncrementINT Cursors(CursorID).Y
                    Cursors(CursorID).X = Min(Len(Lines$(Cursors(CursorID).Y)) + 1, Cursors(CursorID).X)
                    If InRange(VerticalScrollOffset, Cursors(CursorID).Y, VerticalScrollOffset + VerticalLines) And Cursors(CursorID).Y + 2 > VerticalScrollOffset + VerticalLines Then VerticalScrollOffset = VerticalScrollOffset + 1
                End If
            Case 19200 'Left
                If KeyCtrl Then
                    HorizontalScrollOffset = Max(HorizontalScrollOffset - 1, 1)
                Else
                    If Cursors(CursorID).X > 1 Then
                        DecrementINT Cursors(CursorID).X
                    Else
                        HorizontalScrollOffset = 1
                    End If
                End If
            Case 19712 'Right
                If KeyCtrl Then
                    HorizontalScrollOffset = HorizontalScrollOffset + 1
                Else
                    Cursors(CursorID).X = Min(Len(Lines$(Cursors(CursorID).Y)) + 1, Cursors(CursorID).X + 1)
                End If
            Case 18176 'Home
                If KeyCtrl Then
                    Cursors(CursorID).X = 1
                    Cursors(CursorID).Y = 1
                    HorizontalScrollOffset = 1
                    VerticalScrollOffset = 1
                Else
                    Cursors(CursorID).X = 1
                    HorizontalScrollOffset = 1
                End If
            Case 20224 'End
                If KeyCtrl Then
                    Cursors(CursorID).Y = TotalLines
                    Cursors(CursorID).X = Len(Lines$(Cursors(CursorID).Y)) + 1
                    HorizontalScrollOffset = Max(Len(Lines$(Cursors(CursorID).Y)) - HorizontalCharsVisible + 1, 1)
                    VerticalScrollOffset = Max(TotalLines - VerticalLines + 2, 1)
                Else
                    Cursors(CursorID).X = Len(Lines$(Cursors(CursorID).Y)) + 1
                    HorizontalScrollOffset = Max(Len(Lines$(Cursors(CursorID).Y)) - HorizontalCharsVisible + 1, 1)
                End If
            Case 18688 'PgUp
                If KeyCtrl Then
                    VerticalScrollOffset = 1
                    HorizontalScrollOffset = Max(Len(Lines$(Cursors(CursorID).Y)) - HorizontalCharsVisible + 1, 1)
                Else
                    VerticalScrollOffset = Max(VerticalScrollOffset - VerticalLines, 1)
                End If
            Case 20736 'PgDn
                If KeyCtrl Then
                    VerticalScrollOffset = Max(TotalLines - VerticalLines + 2, 1)
                    HorizontalScrollOffset = Max(Len(Lines$(Cursors(CursorID).Y)) - HorizontalCharsVisible + 1, 1)
                Else
                    VerticalScrollOffset = Min(VerticalScrollOffset + VerticalLines, TotalLines - VerticalLines + 2)
                End If
            Case 21248 'Delete
                If Cursors(CursorID).X = Len(Lines$(Cursors(CursorID).Y)) + 1 Then
                    If Cursors(CursorID).Y < TotalLines Then
                        Lines$(Cursors(CursorID).Y) = Lines$(Cursors(CursorID).Y) + Lines$(Cursors(CursorID).Y + 1)
                        RemoveLine Cursors(CursorID).Y + 1
                    End If
                Else
                    Lines$(Cursors(CursorID).Y) = Left$(Lines$(Cursors(CursorID).Y), Cursors(CursorID).X - 1) + Mid$(Lines$(Cursors(CursorID).Y), Cursors(CursorID).X + 1)
                End If
                KeyPressTimer = Timer
                FileSaved = 0
        End Select
        '---------------
        If HorizontalScrollOffset > Cursors(CursorID).X Then DecrementULNG HorizontalScrollOffset
        If Cursors(CursorID).X - HorizontalScrollOffset > HorizontalCharsVisible + 1 Then IncrementULNG HorizontalScrollOffset

        If DISPLAY Then
            'Show Cursor
            Line (TextFrameOffset + (LineNumbersWidth + 1) * _FontWidth, (Cursors(CursorID).Y - VerticalScrollOffset + VerticalTextOffsetLines) * _FontHeight)-(_Width - 1, (Cursors(CursorID).Y - VerticalScrollOffset + 1 + VerticalTextOffsetLines) * _FontHeight), _RGB32(32), BF
            If 2 * Timer(0.1) - Int(2 * Timer) > 0.5 Then
                Line (TextFrameOffset + (Cursors(CursorID).X + LineNumbersWidth + 1 - HorizontalScrollOffset) * _FontWidth, (Cursors(CursorID).Y - VerticalScrollOffset + VerticalTextOffsetLines) * _FontHeight)-(TextFrameOffset + (Cursors(CursorID).X + LineNumbersWidth + 1 - HorizontalScrollOffset) * _FontWidth + 2, (Cursors(CursorID).Y - VerticalScrollOffset + VerticalTextOffsetLines + 1) * _FontHeight), _RGB32(0, 255, 0), BF
                'Line (TextFrameOffset + (Cursors(CursorID).X + LineNumbersWidth + 1 - HorizontalScrollOffset) * _FontWidth, (Cursors(CursorID).Y - VerticalScrollOffset + VerticalTextOffsetLines + 1) * _FontHeight - 2)-(TextFrameOffset + (Cursors(CursorID).X + LineNumbersWidth + 2 - HorizontalScrollOffset) * _FontWidth, (Cursors(CursorID).Y - VerticalScrollOffset + VerticalTextOffsetLines + 1) * _FontHeight), _RGB32(0, 255, 0), BF
            End If
            '-----------
        End If
    Next CursorID

    TotalLines = UBound(Lines$)

    If DISPLAY Then
        VerticalScrollOffset = Max(Min(VerticalScrollOffset, TotalLines), 1)
        'Print Text
        J = 0: For I = VerticalScrollOffset To VerticalScrollOffset + VerticalLines: If TotalLines < I Then Exit For
            Color _RGB32(255), _RGB32(32): _PrintString (TextFrameOffset, (J + VerticalTextOffsetLines) * _FontHeight), " " + _Trim$(Str$(I)) + Space$(LineNumbersWidth - Len(_Trim$(Str$(I))))
            If Len(FormattedLines$(I)) Then
                If COLOR_MODE Then
                    For K = HorizontalScrollOffset To HorizontalScrollOffset + HorizontalCharsVisible - 1
                        If K > Len(Lines$(I)) Then Exit For
                        C& = ColorListGet&(FormattedLines$(I), K)
                        If C& = 0 Then C& = _RGB32(255, 0, 0)
                        Color C&, 0
                        _PrintString (TextFrameOffset + (LineNumbersWidth + K - HorizontalScrollOffset + 1) * _FontWidth, (J + VerticalTextOffsetLines) * _FontHeight), Mid$(Lines$(I), K, 1)
                    Next K
                Else Color _RGB32(255), 0: _PrintString (TextFrameOffset + (LineNumbersWidth - HorizontalScrollOffset + 2) * _FontWidth, (J + VerticalTextOffsetLines) * _FontHeight), Mid$(Lines$(I), HorizontalScrollOffset, HorizontalCharsVisible)
                End If
            Else Color _RGB32(255), 0: _PrintString (TextFrameOffset + (LineNumbersWidth + 1) * _FontWidth, (J + VerticalTextOffsetLines) * _FontHeight), Mid$(Lines$(I), HorizontalScrollOffset, HorizontalCharsVisible)
            End If
        J = J + 1: Next I
        Line (TextFrameOffset, VerticalTextOffset)-(TextFrameOffset, _Height - 1), _RGB32(255)
        Line (TextFrameOffset, VerticalTextOffset)-(_Width, VerticalTextOffset), _RGB32(255)
        '----------
        'Show Scroll Bar
        Line (_Width - 16, VerticalTextOffset)-(_Width, _Height), _RGB32(15), BF
        Line (_Width - 16, ScrollBO)-(_Width, ScrollBO + ScrollBP), _RGB32(127), BF
        '---------------
        'Color Scroll Bar
        Line (_Width - 146, VerticalTextOffset)-(_Width - 16, _Height), _RGB32(31), BF
        S = Min(Max(1, VerticalScrollOffset - Height / 2), TotalLines - Height / 2)
        Line (_Width - 146, VerticalScrollOffset - S + VerticalTextOffset)-(_Width - 16, VerticalScrollOffset - S + VerticalLines + VerticalTextOffset), _RGB32(255, 31), BF
        For I = S To Max(1, Min(S + Height, TotalLines))
            If I < LBound(Lines$) Or I > TotalLines Then _Continue
            For J = 1 To 128
                If J > Len(Lines$(I)) Then Exit For
                If Asc(Lines$(I), J) = 32 Then _Continue
                C& = ColorListGet&(FormattedLines$(I), J)
                If C& = 0 Then C& = _RGB32(255, 0, 0)
                PSet (_Width - 146 + J, VerticalTextOffset + I - S), C&
            Next J
            For CursorID = LBound(Cursors) To UBound(Cursors)
                If Cursors(CursorID).Y = I Then Line (_Width - 146, VerticalTextOffset + I - S)-(_Width - 16, VerticalTextOffset + I - S), _RGB32(255, 0, 0, 63)
            Next CursorID
        Next I
        '----------------
        _Display
    End If

    SKIPDISPLAY:

    TotalLines = UBound(Lines$)

    If FileSaved = 0 Then ColorizeLines = -1

    If ColorizeLines And TotalLines > 1 Then
        For I = 1 To 64
            ColorizeLines_LineOffset = ColorizeLines_LineOffset + 1
            FormattedLines$(ColorizeLines_LineOffset) = Colorize$(Lines$(ColorizeLines_LineOffset))
            If ColorizeLines_LineOffset = TotalLines Then ColorizeLines = 0: ColorizeLines_LineOffset = 0: Colorize_StringMode = 0: Exit For
        Next I
    Else
        Colorize_StringMode = 0
        Colorize_CommentMode = 0
    End If

    'AutoSave after 0.5 Second of Sleep
    If Timer(0.1) - KeyPressTimer >= 0.5 And FileSaved = 0 Then
        GoSub SaveFile
        FileSaved = -1
    End If
    '------------------------

    On _Exit GOTO SaveExit

    If FileSaved <> OldFileSaved Then
        OldFileSaved = FileSaved
        If FileSaved Then _Title TITLE$ Else _Title TITLE$ + "*"
    End If
Loop
System

SaveExit:
GoSub SaveFile
System

ClearFile:
ReDim Lines$(0 To 0)
Return

OpenFile:
Cls , _RGB32(32)
_PrintString (_Width / 2 - 6 * _FontHeight, _Height / 2 - _FontHeight / 2), "Reading File"
_Display
If _FileExists(FilePath$) = 0 Then Return
ReDim Lines$(0)
Open FilePath$ For Input As #1
If LOF(1) Then
    Do
        Line Input #1, L$
        If UBound(Lines$) = 0 Then
            ReDim Lines$(1 To 1)
        Else
            ReDim _Preserve Lines$(1 To UBound(Lines$) + 1)
        End If
        Lines$(UBound(Lines$)) = L$
        If EOF(1) Then Exit Do
    Loop
Else
    ReDim Lines$(1 To 1)
End If
Close #1
ReDim FormattedLines$(1 To UBound(Lines$))
ColorizeLines = -1
Return

SaveFile:
If FilePath$ = "" Then Return
Open FilePath$ For Output As #1
For I = 1 To TotalLines
    If I = TotalLines Then Print #1, Lines$(I); Else Print #1, Lines$(I)
Next I
Close #1
Return
Sub AddLine
    ReDim _Preserve Lines$(1 To UBound(Lines$) + 1)
    ReDim _Preserve FormattedLines$(1 To UBound(Lines$))
End Sub
Sub InsertLine (__LN)
    AddLine
    For I = UBound(Lines$) - 1 To __LN Step -1
        Lines$(I + 1) = Lines$(I)
        FormattedLines$(I + 1) = FormattedLines$(I)
    Next I
End Sub
Sub RemoveLine (__LN)
    For I = __LN To UBound(Lines$) - 1
        Lines$(I) = Lines$(I + 1)
        FormattedLines$(I) = FormattedLines$(I + 1)
    Next I
    DeleteLine
End Sub
Sub DeleteLine
    ReDim _Preserve Lines$(1 To UBound(Lines$) - 1)
    ReDim _Preserve FormattedLines$(1 To UBound(Lines$))
End Sub
Sub IncrementINT (A As Integer)
    A = A + 1
End Sub
Sub DecrementINT (A As Integer)
    A = A - 1
End Sub
Sub IncrementULNG (A As _Unsigned Long)
    A = A + 1
End Sub
Sub DecrementULNG (A As _Unsigned Long)
    A = A - 1
End Sub
Function Max (A, B)
    If A > B Then Max = A Else Max = B
End Function
Function Min (A, B)
    If A < B Then Min = A Else Min = B
End Function
Function InRange (A, B, C)
    If A <= B And B <= C Then InRange = -1
End Function
Function Colorize$ (L$)
    If Colorize_CommentMode = 1 Then Colorize_CommentMode = 0
    ColorList$ = ColorListNew$
    For I = 1 To Len(L$)
        C~%% = Asc(L$, I)
        If Colorize_StringMode Then
            ColorList$ = ColorListAdd$(ColorList$, _RGB32(255, 167, 0))
            If C~%% = 34 And Colorize_StringMode = 1 Then Colorize_StringMode = 0
            If C~%% = 39 And Colorize_StringMode = 2 Then Colorize_StringMode = 0
        Else
            If InStr(L$, Comment$) = I Then Colorize_CommentMode = 1
            If InStr(L$, MultiLineCommentOn$) = I Then Colorize_CommentMode = 2
            If InStr(L$, MultiLineCommentOff$) = I And Colorize_CommentMode = 2 Then Colorize_CommentMode = 0
            If Colorize_CommentMode > 0 Then
                ColorList$ = ColorListAdd$(ColorList$, _RGB32(127))
            Else
                Select Case C~%%
                    Case 9, 32: ColorList$ = ColorListAdd$(ColorList$, _RGB32(0))
                    Case 34: Colorize_StringMode = 1: ColorList$ = ColorListAdd$(ColorList$, _RGB32(255, 127, 0))
                    Case 39: Colorize_StringMode = 2: ColorList$ = ColorListAdd$(ColorList$, _RGB32(255, 127, 0))
                    Case 33, 35 To 38, 40 To 47, 58 To 64, 91 To 96, 123 To 126: ColorList$ = ColorListAdd$(ColorList$, _RGB32(255, 180, 24))
                    Case 48 To 57
                        Select Case LC~%%
                            Case 32, 33, 35 To 38, 40 To 47, 58 To 64, 91 To 96, 123 To 126
                                ColorList$ = ColorListAdd$(ColorList$, _RGB32(95, 191, 0))
                            Case Else
                                If ColorListGet&(ColorList$, ColorListLength~&(ColorList$)) = 0 Then ColorList$ = ColorListAdd$(ColorList$, _RGB32(95, 191, 0)) Else ColorList$ = ColorListAdd$(ColorList$, ColorListGet&(ColorList$, ColorListLength~&(ColorList$)))
                        End Select
                    Case 65 To 90, 97 To 122: ColorList$ = ColorListAdd$(ColorList$, _RGB32(255))
                    Case Else: ColorList$ = ColorListAdd$(ColorList$, _RGB32(255, 0, 0))
                End Select
                Select Case C~%%
                    Case 48 To 57, 65 To 90, 95, 97 To 122: Token$ = Token$ + Chr$(C~%%)
                    Case Else: FinishedToken$ = Token$: Token$ = ""
                End Select
                If Len(FinishedToken$) Then
                    If InStr(Keywords$, " " + FinishedToken$ + " ") Then
                        T~& = ColorListGet&(ColorList$, ColorListLength~&(ColorList$))
                        For J = 0 To Len(FinishedToken$): ColorList$ = ColorListDelete$(ColorList$, ColorListLength~&(ColorList$)): Next J
                        For J = 1 To Len(FinishedToken$): ColorList$ = ColorListAdd$(ColorList$, _RGB32(86, 156, 214)): Next J
                        ColorList$ = ColorListAdd$(ColorList$, T~&)
                    End If
                    FinishedToken$ = ""
                End If
                LC~%% = C~%%
    End If: End If: Next I
Colorize$ = ColorList$: End Function
Function ColorListNew$
    ColorListNew$ = MKL$(0)
End Function
Function ColorListLength~& (__ColorList As String)
    If Len(__ColorList) < 4 Then ColorListLength~& = 0 Else ColorListLength~& = CVL(Mid$(__ColorList, 1, 4))
End Function
Function ColorListAdd$ (__ColorList As String, __Color As Long)
    If Len(__ColorList) < 4 Then __ColorList = MKL$(0)
    ColorListAdd$ = MKL$(CVL(Mid$(__ColorList, 1, 4)) + 1) + Mid$(__ColorList, 5) + MKL$(__Color)
End Function
Function ColorListGet& (__ColorList As String, __ItemNumber As _Unsigned Long)
    If Len(__ColorList) < 4 Then Exit Function
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    __nItems = CVL(Mid$(__ColorList, 1, 4))
    If __ItemNumber > __nItems Then Exit Function
    __OFFSET = 5
    For __I = 1 To __nItems
        If __I = __ItemNumber Then ColorListGet& = CVL(Mid$(__ColorList, __OFFSET, 4)): Exit Function
        __OFFSET = __OFFSET + 4
    Next __I
End Function
Function ColorListDelete$ (__ColorList As String, __ItemNumber As _Unsigned Long)
    If ColorListLength~&(__ColorList) < __ItemNumber Then Exit Function
    If __ItemNumber = 0 Then Exit Function
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    __nItems = CVL(Mid$(__ColorList, 1, 4))
    __OFFSET = 5
    For __I = 1 To __nItems
        If __I = __ItemNumber Then
            ColorListDelete$ = MKL$(__nItems - 1) + Mid$(__ColorList, 5, __OFFSET - 5) + Mid$(__ColorList, __OFFSET + 4)
            Exit Function
        End If
        __OFFSET = __OFFSET + 4
    Next __I
End Function
