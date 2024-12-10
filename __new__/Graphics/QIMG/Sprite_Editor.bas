$ExeIcon:'.\qimgse.ico'
'$Include:'QIMG.bi'
'Color Dialog Box'
Type COLORDIALOGTYPE
    lStructSize As _Integer64
    hwndOwner As _Integer64
    hInstance As _Integer64
    rgbResult As _Integer64
    lpCustColors As _Offset
    flags As _Integer64
    lCustData As _Integer64
    lpfnHook As _Integer64
    lpTemplateName As _Offset
End Type
Declare Dynamic Library "comdlg32"
    Function ChooseColorA& (DIALOGPARAMS As COLORDIALOGTYPE)
End Declare
'----------------'
Type UI_Type
    As String ID
    As _Unsigned _Byte isFrame, isBar, isCheckBox, BorderSize, HoverTime, Value, MaxValue
    As _Unsigned Integer X, Y, W, H, Length
    As Long ImageHandle, BorderColor, FColor, BColor
    As String L, T
    As _Unsigned Long Parent
End Type

Screen _NewImage(720, 320, 32)
_AcceptFileDrop

Dim Shared QIMG_H As QIMG_Header
Dim Shared IMG&(1 To 65536)
Dim Shared UI_LIST(0) As UI_Type, INFILE$, FileSaved As _Byte
Dim Shared As _Unsigned Long CurrentFrame, ForegroundColor, BackgroundColor
Dim Shared As _Byte RunMode, Tool
Dim Shared As _Unsigned _Byte ToolSize

Dim Shared RunImage&, PauseImage&

'UI'
ForegroundColor = _RGB32(255)
BackgroundColor = _RGB32(0)
CurrentFrame = 1
RunImage& = load_run
PauseImage& = load_pause
UI_Create "FRAME_FrameControls", 336, 156, 320, 32, "", "Frame Controls", 4, 0, 0, 0, 0
UI_Create "Previous", 0, 0, 32, 32, "", "Previous", 0, 0, 0, load_left, UI_GET_ID("FRAME_FrameControls")
UI_Create "Decrease", 48, 0, 32, 32, "", "Decrease", 0, 0, 0, load_minus, UI_GET_ID("FRAME_FrameControls")
UI_Create "CurrentFrame", 96, 0, 32, 32, _Trim$(Str$(CurrentFrame)), "Current Frame", 0, _RGB32(255), 0, 0, UI_GET_ID("FRAME_FrameControls")
UI_Create "TotalFrames", 96, 16, 32, 32, _Trim$(Str$(QIMG_H.Frames)), "Total Frames", 0, _RGB32(255), 0, 0, UI_GET_ID("FRAME_FrameControls")
UI_Create "Increase", 144, 0, 32, 32, "", "Increase", 0, 0, 0, load_plus, UI_GET_ID("FRAME_FrameControls")
UI_Create "Next", 192, 0, 32, 32, "", "Next", 0, 0, 0, load_right, UI_GET_ID("FRAME_FrameControls")
UI_Create "CopyPreviousFrame", 240, 0, 32, 32, "", "Copy Previous Frame", 0, 0, 0, load_copy, UI_GET_ID("FRAME_FrameControls")
UI_Create "Delete", 288, 0, 32, 32, "", "Delete", 0, 0, 0, load_delete, UI_GET_ID("FRAME_FrameControls")

UI_Create "FRAME_Tools", 336, 16, 280, 32, "", "Tools", 4, 0, 0, 0, 0
UI_Create "Run", 0, 0, 32, 32, "", "Run", 0, 0, 0, RunImage&, UI_GET_ID("FRAME_Tools")
UI_Create "Pencil", 48, 0, 32, 32, "", "Pencil", 0, 0, 0, load_pencil, UI_GET_ID("FRAME_Tools")
UI_Create "Eraser", 96, 0, 32, 32, "", "Eraser", 0, 0, 0, load_eraser, UI_GET_ID("FRAME_Tools")
UI_Create "Fill", 144, 0, 32, 32, "", "Fill", 0, 0, 0, load_fill, UI_GET_ID("FRAME_Tools")
UI_Create "Pick", 192, 0, 32, 32, "", "Pick Colour", 0, 0, 0, load_pick, UI_GET_ID("FRAME_Tools")
UI_Create "ToolSize", 240, 0, 32, 32, "1", "Tool Size", 0, _RGB32(255), 0, 0, UI_GET_ID("FRAME_Tools")

UI_Create "FRAME_ColorTools", 336, 64, 160, 64, "", "Color Tools", 4, 0, 0, 0, 0
UI_Create "ForegroundColor", 0, 0, 128, 16, "Foreground Color", "", 0, _RGB32(255), 0, 0, UI_GET_ID("FRAME_ColorTools")
UI_Create "BackgroundColor", 0, 16, 128, 16, "Background Color", "", 0, _RGB32(255), 0, 0, UI_GET_ID("FRAME_ColorTools")
UI_Create "ForegroundColorBack", 144, 0, 16, 16, "", "", 0, 0, 0, load_transparent, UI_GET_ID("FRAME_ColorTools")
UI_Create "BackgroundColorBack", 144, 16, 16, 16, "", "", 0, 0, 0, UI_LIST(UI_GET_ID("ForegroundColorBack")).ImageHandle, UI_GET_ID("FRAME_ColorTools")
UI_Create "ForegroundColorBox", 144, 0, 16, 16, "", "Foreground Color", 0, 0, ForegroundColor, 0, UI_GET_ID("FRAME_ColorTools")
UI_Create "BackgroundColorBox", 144, 16, 16, 16, "", "Background Color", 0, 0, BackgroundColor, 0, UI_GET_ID("FRAME_ColorTools")
UI_Create "BAR_ColorOpacityBar", 8, 48, 144, 16, "", "Opacity", 0, _RGB32(0, 127, 255), _RGB32(255), 0, UI_GET_ID("FRAME_ColorTools")
UI_LIST(UI_GET_ID("BAR_ColorOpacityBar")).Value = 255
UI_LIST(UI_GET_ID("BAR_ColorOpacityBar")).MaxValue = 255

UI_Create "FRAME_FileTools", 336, 206, 240, 32, "", "File Tools", 4, 0, 0, 0, 0
UI_Create "Open", 0, 0, 32, 32, "", "Open", 0, 0, 0, load_open, UI_GET_ID("FRAME_FileTools")
UI_Create "Reload", 48, 0, 32, 32, "", "Reload", 0, 0, 0, load_reload, UI_GET_ID("FRAME_FileTools")
UI_Create "New", 96, 0, 32, 32, "", "New", 0, 0, 0, load_new, UI_GET_ID("FRAME_FileTools")
UI_Create "Export", 144, 0, 32, 32, "", "Export", 0, 0, 0, load_export, UI_GET_ID("FRAME_FileTools")
UI_Create "ExportAll", 192, 0, 32, 32, "", "Export All", 0, 0, 0, load_export_all, UI_GET_ID("FRAME_FileTools")

UI_Create "CHECKBOX_EnableAlpha", 336, 252, _Width - 384, 32, "Enable Alpha", "", 2, _RGB32(255), 0, 0, 0
UI_Create "WidthHeightStat", 336, 300, _Width - 384, 16, _Trim$(Str$(QIMG_H.Width)) + "x" + _Trim$(Str$(QIMG_H.Height)), "Resolution", 4, _RGB32(255), 0, 0, 0
'--'

If _CommandCount Then
    INFILE$ = Command$(1)
    If _FileExists(INFILE$) = 0 Then
        If _FileExists(_StartDir$ + "\" + INFILE$) Then
            INFILE$ = _StartDir$ + "\" + INFILE$
        Else
            UI_DialogBox "File doesn't exists"
        End If
    End If
End If
LoadNewQIMG:
If _FileExists(INFILE$) Then
    OpenFile
Else
    INFILE$ = "Untitled.qimg"
    QIMG_H.Signature = "QIMG"
    QIMG_H.Width = 64: QIMG_H.Height = 64
    QIMG_H.Frames = 1
    UI_LIST(UI_GET_ID("TotalFrames")).L = _Trim$(Str$(QIMG_H.Frames))
    QIMG_H.ColorType = 32
End If

Tool = 1
ToolSize = 1
FileSaved = -1

Do
    Cls , _RGB32(32)
    _Limit 60
    Frame = (Frame Mod 60) + 1
    While _MouseInput: Wend
    If IMG&(CurrentFrame) = 0 Then IMG&(CurrentFrame) = _NewImage(QIMG_H.Width, QIMG_H.Height, QIMG_H.ColorType)
    If UI_LIST(UI_GET_ID("CHECKBOX_EnableAlpha")).Value Then
        TMPIMG& = _NewImage(QIMG_H.Width, QIMG_H.Height, QIMG_H.ColorType)
        __SOURCE& = _Source: __DEST& = _Dest
        _Source IMG&(CurrentFrame): _Dest TMPIMG&
        For X% = 0 To QIMG_H.Width - 1
            For Y% = 0 To QIMG_H.Height - 1
                __P& = Point(X%, Y%)
                PSet (X%, Y%), SetAlphaColor&(__P&, 255)
        Next Y%, X%
        _Source __SOURCE&: _Dest __DEST&
        _PutImage (0, 0)-(319, 319), TMPIMG&
    Else
        If RunMode = 0 And CurrentFrame > 1 Then _PutImage (0, 0)-(319, 319), IMG&(CurrentFrame - 1): Line (0, 0)-(319, 319), _RGBA32(0, 0, 0, 127), BF
        _PutImage (0, 0)-(319, 319), IMG&(CurrentFrame)
    End If
    Line (0, 0)-(319, 319), _RGB32(255), B
    If InRange(0, _MouseX, 319) And InRange(0, _MouseY, 319) Then
        ScreenToCanvas _MouseX, _MouseY, CX%, CY%
        CanvasToScreen CX%, CY%, SX1%, SY1%: CanvasToScreen CX% + 1, CY% + 1, SX2%, SY2%
        _MouseHide
        Line (SX1%, SY1%)-(SX2%, SY2%), _RGB32(255), B
        If _MouseButton(1) Or _MouseButton(2) Then
            FileSaved = 0
            If _MouseButton(1) Then CurrentColor& = ForegroundColor Else CurrentColor& = BackgroundColor
            _Source IMG&(CurrentFrame)
            _Dest IMG&(CurrentFrame)
            Select Case Tool
                Case 1: If UI_LIST(UI_GET_ID("CHECKBOX_EnableAlpha")).Value Then SetColor CX%, CY%, CurrentColor& Else Line (OCX%, OCY%)-(CX%, CY%), CurrentColor&
                Case 2: For X% = CX% - Int(ToolSize / 2 - 0.5) To CX% + Int(ToolSize / 2)
                        For Y% = CY% - Int(ToolSize / 2 - 0.5) To CY% + Int(ToolSize / 2)
                            EraseColor X%, Y%
                    Next Y%, X%
                Case 3: Paint (CX%, CY%), CurrentColor&
                Case 4: If _MouseButton(1) Then
                        ForegroundColor = Point(CX%, CY%)
                        UI_LIST(UI_GET_ID("ForegroundColorBox")).BColor = ForegroundColor
                        UI_LIST(UI_GET_ID("BAR_ColorOpacityBar")).Value = _Alpha32(ForegroundColor)
                    Else
                        BackgroundColor = Point(CX%, CY%)
                        UI_LIST(UI_GET_ID("BackgroundColorBox")).BColor = BackgroundColor
                        UI_LIST(UI_GET_ID("BAR_ColorOpacityBar")).Value = _Alpha32(BackgroundColor)
                    End If
            End Select
            _Source 0
            _Dest 0
        End If
        If Tool = 4 Then
            _Source IMG&(CurrentFrame)
            __P& = Point(CX%, CY%)
            TMP$ = _Trim$(Str$(_Red32(__P&))) + "," + _Trim$(Str$(_Green32(__P&))) + "," + _Trim$(Str$(_Blue32(__P&))) + "," + _Trim$(Str$(_Alpha32(__P&)))
            Line (_MouseX - 1, _MouseY - 1)-(_MouseX + 1 + Len(TMP$) * _FontWidth, _MouseY), _RGB32(64), BF
            _PrintString (_MouseX + 1, _MouseY), TMP$
            _Source 0
        End If
        OCX% = CX%: OCY% = CY%
    Else
        _MouseShow
    End If
    UI_LIST(UI_GET_ID("Pencil")).BorderSize = 0: UI_LIST(UI_GET_ID("Eraser")).BorderSize = 0: UI_LIST(UI_GET_ID("Fill")).BorderSize = 0: UI_LIST(UI_GET_ID("Pick")).BorderSize = 0
    Select Case Tool
        Case 1: UI_LIST(UI_GET_ID("Pencil")).BorderSize = 2
        Case 2: UI_LIST(UI_GET_ID("Eraser")).BorderSize = 2
        Case 3: UI_LIST(UI_GET_ID("Fill")).BorderSize = 2
        Case 4: UI_LIST(UI_GET_ID("Pick")).BorderSize = 2
    End Select
    If FileSaved Then _Title INFILE$ + " - QSprite Editor" Else _Title INFILE$ + " - QSprite Editor*"
    UI_LIST(UI_GET_ID("WidthHeightStat")).L = _Trim$(Str$(QIMG_H.Width)) + "x" + _Trim$(Str$(QIMG_H.Height))
    UI_Update
    If RunMode = -1 Then
        If QIMG_H.Frames < 60 Then
            If Frame Mod 5 = 0 Then
                If CurrentFrame = QIMG_H.Frames Then CurrentFrame = 0
                CurrentFrame = CurrentFrame + 1
            End If
        Else
            If CurrentFrame = QIMG_H.Frames Then CurrentFrame = 0
            CurrentFrame = CurrentFrame + 1
        End If
        UI_LIST(UI_GET_ID("CurrentFrame")).L = _Trim$(Str$(CurrentFrame))
        UI_LIST(UI_GET_ID("Run")).ImageHandle = PauseImage&
    Else
        UI_LIST(UI_GET_ID("Run")).ImageHandle = RunImage&
    End If
    If FileSaved = 0 Then AutoSaveFile
    If _KeyDown(100306) And (_KeyDown(83) Or _KeyDown(115)) Then SaveFile
    _Display
    If _TotalDroppedFiles Then
        For I = 1 To _TotalDroppedFiles
            TMP& = _LoadImage(_DroppedFile, 32)
            If IMG&(CurrentFrame + I - 1) >= -1 Then IMG&(CurrentFrame + I - 1) = _NewImage(QIMG_H.Width, QIMG_H.Height, QIMG_H.ColorType)
            _PutImage (0, 0)-(Min(QIMG_H.Width, _Width(TMP&)) - 1, Min(QIMG_H.Height, _Height(TMP&)) - 1), TMP&, IMG&(CurrentFrame + I - 1)
            If I < _TotalDroppedFiles Then QIMG_H.Frames = QIMG_H.Frames + 1
        Next
        _FinishDrop
        UI_LIST(UI_GET_ID("TotalFrames")).L = _Trim$(Str$(QIMG_H.Frames))
        FileSaved = 0
    End If
    If _Exit Then
        If FileSaved Then Exit Do
        Select Case UI_ChoiceDialogBox("Save File?")
            Case 1: SaveFile: UI_DialogBox "File Saved to " + INFILE$: Exit Do
            Case -1: Exit Do
        End Select
    End If
Loop
System
Sub SaveFile
    D$ = QIMG_Save$(IMG&(), 1, QIMG_H.Frames)
    If _FileExists(INFILE$) Then Kill INFILE$
    Open INFILE$ For Binary As #1
    Put #1, , D$
    D$ = ""
    Close #1
    FileSaved = -1
End Sub
Sub AutoSaveFile
    Static AutoSaveTime As Single
    If Timer - AutoSaveTime < 1 Then Exit Sub
    AutoSaveTime = Timer
    D$ = QIMG_Save$(IMG&(), 1, QIMG_H.Frames)
    If _FileExists("AutoSave.qimg") Then Kill "AutoSave.qimg"
    Open "AutoSave.qimg" For Binary As #1
    Put #1, , D$
    D$ = ""
    Close #1
End Sub
Sub OpenFile
    If _FileExists(INFILE$) = 0 Then UI_DialogBox "File isn't Saved!": Exit Sub
    Open INFILE$ For Binary As #1
    Get #1, , QIMG_H
    IMGDATA$ = String$(QIMG_H.DataLength, 0)
    Get #1, , IMGDATA$
    Close #1
    QIMG_H.Frames = QIMG_H.Frames
    UI_LIST(UI_GET_ID("TotalFrames")).L = _Trim$(Str$(QIMG_H.Frames))
    For I = LBound(IMG&) To UBound(IMG&)
        If IMG&(I) < -1 Then _FreeImage IMG&(I)
        IMG&(I) = 0
    Next I
    QIMG_Load QIMG_H, IMGDATA$, IMG&()
    FileSaved = -1
End Sub
Sub ScreenToCanvas (SX As Integer, SY As Integer, CX As Integer, CY As Integer): CX = SX / (320 \ QIMG_H.Width) - 0.5: CY = SY / (320 \ QIMG_H.Height) - 0.5: End Sub
Sub CanvasToScreen (CX As Integer, CY As Integer, SX As Integer, SY As Integer): SX = CX * (320 \ QIMG_H.Width): SY = CY * (320 \ QIMG_H.Height): End Sub
Sub UI_Create (ID As String, X, Y, W, H, L As String, T As String, BorderSize As _Unsigned _Byte, FColor As _Unsigned Long, BColor As _Unsigned Long, ImageHandle As Long, ParentFrame As _Unsigned Long)
    __I = UBound(UI_LIST) + 1
    ReDim _Preserve UI_LIST(1 To __I) As UI_Type
    If Left$(ID, 3) = "BAR" Then UI_LIST(__I).isBar = 1 Else UI_LIST(__I).isBar = 0
    If Left$(ID, 5) = "FRAME" Then UI_LIST(__I).isFrame = 1 Else UI_LIST(__I).isFrame = 0
    If Left$(ID, 8) = "CHECKBOX" Then UI_LIST(__I).isCheckBox = 1 Else UI_LIST(__I).isCheckBox = 0
    UI_LIST(__I).ID = ID
    UI_LIST(__I).X = X
    UI_LIST(__I).Y = Y
    UI_LIST(__I).W = W
    UI_LIST(__I).H = H
    UI_LIST(__I).L = L
    UI_LIST(__I).T = T
    UI_LIST(__I).BorderSize = BorderSize
    UI_LIST(__I).Length = W \ _FontWidth
    UI_LIST(__I).FColor = FColor
    UI_LIST(__I).BColor = BColor
    UI_LIST(__I).ImageHandle = ImageHandle
    UI_LIST(__I).BorderColor = _RGB32(255)
    UI_LIST(__I).Parent = ParentFrame
End Sub
Function UI_GET_ID (__ID$)
    For I = 1 To UBound(UI_LIST)
        If UI_LIST(I).ID = __ID$ Then
            UI_GET_ID = I
            Exit Function
        End If
    Next I
    UI_GET_ID = 0
End Function
Sub UI_Update
    For I = 1 To UBound(UI_LIST)
        PX = 0: PY = 0
        If UI_LIST(I).Parent Then PX = UI_LIST(UI_LIST(I).Parent).X: PY = UI_LIST(UI_LIST(I).Parent).Y
        X1 = UI_LIST(I).X + PX: Y1 = UI_LIST(I).Y + PY
        X2 = X1 + UI_LIST(I).W: Y2 = Y1 + UI_LIST(I).H
        If UI_LIST(I).ImageHandle < -1 Then
            _PutImage (X1, Y1)-(X2, Y2), UI_LIST(I).ImageHandle
        Else
            Line (X1, Y1)-(X2, Y2), UI_LIST(I).BColor, BF
            FG& = _DefaultColor: BG& = _BackgroundColor
            Color UI_LIST(I).FColor, UI_LIST(I).BColor
            _PrintString (X1, Y1), Left$(UI_LIST(I).L, UI_LIST(I).Length)
            Color FG&, BG&
        End If
        If UI_LIST(I).isFrame Then
            If InBox(X1, Y1, _MouseX, _MouseY, X2, Y2) = 0 And InBox(X1 - 4, Y1 - 4, _MouseX, _MouseY, X2 + 4, Y2 + 4) <> 0 Then
                ShowMoveCursor = -1
                UI_LIST(I).HoverTime = UI_LIST(I).HoverTime + Sgn(255 - UI_LIST(I).HoverTime)
                If _MouseButton(1) Then
                    MX = _MouseX
                    MY = _MouseY
                    _MouseShow
                    While _MouseButton(1) Or _MouseInput: Wend
                    UI_LIST(I).X = UI_LIST(I).X + _MouseX - MX
                    UI_LIST(I).Y = UI_LIST(I).Y + _MouseY - MY
                End If
            Else
                UI_LIST(I).HoverTime = 0
            End If
        ElseIf UI_LIST(I).isBar Then
            Line (X1, Y1 + UI_LIST(I).H / 2 - 1)-(X1 + UI_LIST(I).Value / UI_LIST(I).MaxValue * UI_LIST(I).W, Y1 + UI_LIST(I).H / 2 + 1), UI_LIST(I).BColor, BF
            Line (X1 + UI_LIST(I).Value / UI_LIST(I).MaxValue * UI_LIST(I).W - 1, Y1 + UI_LIST(I).H / 2 - 1)-(X1 + UI_LIST(I).Value / UI_LIST(I).MaxValue * UI_LIST(I).W + 1, Y1 + UI_LIST(I).H / 2 + 1), UI_LIST(I).FColor, BF
            If InBox(X1, Y1, _MouseX, _MouseY, X2, Y2) Then
                UI_LIST(I).HoverTime = 120
                If _MouseButton(1) Then
                    While _MouseInput: Wend
                    UI_LIST(I).Value = (_MouseX - X1) * UI_LIST(I).MaxValue / (X2 - X1)
                End If
                UI_OnLeftClick I
            Else
                UI_LIST(I).HoverTime = 0
            End If
        ElseIf UI_LIST(I).isCheckBox Then
            If UI_LIST(I).Value Then _PrintString (X2 - 16, Y1), Chr$(254)
            If _MouseButton(1) And InBox(X1, Y1, _MouseX, _MouseY, X2, Y2) Then
                While _MouseButton(1) Or _MouseInput: Wend
                If UI_LIST(I).Value Then UI_LIST(I).Value = 0 Else UI_LIST(I).Value = -1
            End If
        Else
            If InBox(X1, Y1, _MouseX, _MouseY, X2, Y2) Then
                UI_LIST(I).HoverTime = UI_LIST(I).HoverTime + Sgn(255 - UI_LIST(I).HoverTime)
                If _MouseButton(1) Then
                    While _MouseButton(1) Or _MouseInput: Wend
                    UI_OnLeftClick I
                ElseIf _MouseButton(2) Then
                    While _MouseButton(2) Or _MouseInput: Wend
                    UI_OnRightClick I
                End If
            Else
                UI_LIST(I).HoverTime = 0
            End If
        End If
        For J = 0 To UI_LIST(I).BorderSize - 1
            Line (X1 - J, Y1 - J)-(X2 + J, Y2 + J), UI_LIST(I).BorderColor, B
        Next J
        If InRange(30, UI_LIST(I).HoverTime, 180) And Len(UI_LIST(I).T) > 0 Then ShowToolTip = I
    Next I
    If ShowToolTip Then
        TMP$ = UI_LIST(ShowToolTip).T
        If UI_LIST(ShowToolTip).isBar Then TMP$ = TMP$ + ":" + _Trim$(Str$(UI_LIST(ShowToolTip).Value)) + "/" + _Trim$(Str$(UI_LIST(ShowToolTip).MaxValue))
        Line (_MouseX + 16, _MouseY)-(_MouseX + 16 + _FontWidth * Len(TMP$), _MouseY + _FontHeight), _RGB32(32), BF
        Line (_MouseX + 15, _MouseY - 1)-(_MouseX + 16 + _FontWidth * Len(TMP$), _MouseY + _FontHeight), _RGB32(127), B
        _PrintString (_MouseX + 16, _MouseY), TMP$
        ShowToolTip = 0
    End If
    If ShowMoveCursor Then
        Static MoveCursor&
        If MoveCursor& = 0 Then MoveCursor& = load_move_cursor
        _MouseHide
        _PutImage (_MouseX - 8, _MouseY - 8)-(_MouseX + 7, _MouseY + 7), MoveCursor&
    Else
        _MouseShow
    End If
End Sub
Sub UI_OnLeftClick (__I As _Unsigned Long)
    Select Case UI_LIST(__I).ID
        Case "Previous": If CurrentFrame > 1 Then CurrentFrame = CurrentFrame - 1: UI_LIST(UI_GET_ID("CurrentFrame")).L = _Trim$(Str$(CurrentFrame))
        Case "Decrease": If CurrentFrame < QIMG_H.Frames Then QIMG_H.Frames = QIMG_H.Frames - 1: UI_LIST(UI_GET_ID("TotalFrames")).L = _Trim$(Str$(QIMG_H.Frames)): FileSaved = 0
        Case "CurrentFrame": CurrentFrame = Val(UI_InputDialogBox("Current Frame", _Trim$(Str$(CurrentFrame)), 1))
            UI_LIST(__I).L = _Trim$(Str$(CurrentFrame))
        Case "Increase": QIMG_H.Frames = QIMG_H.Frames + 1: UI_LIST(UI_GET_ID("TotalFrames")).L = _Trim$(Str$(QIMG_H.Frames)): FileSaved = 0
        Case "Next": If CurrentFrame < QIMG_H.Frames Then CurrentFrame = CurrentFrame + 1: UI_LIST(UI_GET_ID("CurrentFrame")).L = _Trim$(Str$(CurrentFrame))
        Case "ForegroundColor", "ForegroundColorBack", "ForegroundColorBox": ForegroundColor = SetAlphaColor(ChooseColor&(ForegroundColor), UI_LIST(UI_GET_ID("BAR_ColorOpacityBar")).Value): UI_LIST(UI_GET_ID("ForegroundColorBox")).BColor = ForegroundColor
        Case "BackgroundColor", "BackgroundColorBack", "BackgroundColorBox": BackgroundColor = SetAlphaColor(ChooseColor&(BackgroundColor), UI_LIST(UI_GET_ID("BAR_ColorOpacityBar")).Value): UI_LIST(UI_GET_ID("BackgroundColorBox")).BColor = BackgroundColor
        Case "BAR_ColorOpacityBar"
            ForegroundColor = SetAlphaColor(ForegroundColor, UI_LIST(__I).Value): UI_LIST(UI_GET_ID("ForegroundColorBox")).BColor = ForegroundColor
            BackgroundColor = SetAlphaColor(BackgroundColor, UI_LIST(__I).Value): UI_LIST(UI_GET_ID("BackgroundColorBox")).BColor = BackgroundColor
        Case "Run": If RunMode Then RunMode = 0 Else RunMode = -1
        Case "Reload": If _FileExists(INFILE$) = 0 Then UI_DialogBox "File doesn't exists" Else OpenFile
        Case "Pencil": Tool = 1
        Case "Eraser": Tool = 2
        Case "Fill": Tool = 3
        Case "Pick": Tool = 4
        Case "ToolSize": ToolSize = Val(UI_InputDialogBox("Tool Size", _Trim$(Str$(ToolSize)), 1))
            UI_LIST(__I).L = _Trim$(Str$(ToolSize))
        Case "WidthHeightStat": T$ = UI_InputDialogBox("[WIDTH]x[HEIGHT]", _Trim$(Str$(QIMG_H.Width)) + "x" + _Trim$(Str$(QIMG_H.Height)), 0)
            If Len(T$) Then
                QIMG_H.Width = Val(Left$(T$, InStr(T$, "x") - 1))
                QIMG_H.Height = Val(Mid$(T$, InStr(T$, "x") + 1))
                For I = LBound(IMG&) To UBound(IMG&)
                    If IMG&(I) >= -1 Then _Continue
                    TMP& = _NewImage(QIMG_H.Width, QIMG_H.Height, QIMG_H.ColorType)
                    _PutImage (0, 0)-(QIMG_H.Width - 1, QIMG_H.Height - 1), IMG&(I), TMP&, (0, 0)-(QIMG_H.Width - 1, QIMG_H.Height - 1)
                    _FreeImage IMG&(I)
                    IMG&(I) = TMP&
                Next I
                FileSaved = 0
            End If
        Case "Export":
            FILE$ = Left$(INFILE$, Len(INFILE$) - 5) + "_" + _Trim$(Str$(CurrentFrame)) + ".png"
            If _FileExists(FILE$) Then Kill FILE$
            T = PNG32(FILE$, IMG&(CurrentFrame), "")
            UI_DialogBox "Exported to " + FILE$
        Case "ExportAll":
            If QIMG_H.Frames = 1 Then
                FILE$ = Left$(INFILE$, Len(INFILE$) - 5) + ".png"
                If _FileExists(FILE$) Then Kill FILE$
                T = PNG32(FILE$, IMG&(1), "")
                UI_DialogBox "Exported to " + FILE$
            Else
                FOLDER$ = Left$(INFILE$, Len(INFILE$) - 5)
                If _DirExists(FOLDER$) Then Shell "rd /s /q " + FOLDER$
                MkDir FOLDER$
                For I = 1 To QIMG_H.Frames
                    FILE$ = _Trim$(Str$(I))
                    FILE$ = String$(4 - Len(FILE$), 48) + FILE$
                    T = PNG32(FOLDER$ + "\" + FILE$ + ".png", IMG&(I), "")
                Next I
                UI_DialogBox "Exported to " + FOLDER$ + "\"
            End If
        Case "New":
            If FileSaved = 0 Then
                T = UI_ChoiceDialogBox("Save File?")
                If T = 0 Then Exit Sub
            End If
            If T = 1 Then
                SaveFile
            End If
            For I = 1 To 65536
                If IMG&(I) < -1 Then _FreeImage IMG&(I)
                IMG&(I) = 0
            Next I
            INFILE$ = UI_InputDialogBox("File Name", "Untitled.qimg", 1)
            CurrentFrame = 1
            QIMG_H.Frames = 1
            QIMG_H.Width = 64
            QIMG_H.Height = 64
            SaveFile
        Case "CopyPreviousFrame": If CurrentFrame > 1 Then
                _FreeImage IMG&(CurrentFrame)
                IMG&(CurrentFrame) = _CopyImage(IMG&(CurrentFrame - 1), QIMG_H.ColorType)
                FileSaved = 0
            End If
        Case "Delete":
            _FreeImage IMG&(CurrentFrame)
            IMG&(CurrentFrame) = _NewImage(QIMG_H.Width, QIMG_H.Height, QIMG_H.ColorType)
            FileSaved = 0
        Case "Open":
            If FileSaved = 0 Then
                T = UI_ChoiceDialogBox("Save File?")
                If T = 0 Then Exit Sub
                If T = 1 Then SaveFile
            End If
            For I = 1 To 65536
                If IMG&(I) < -1 Then _FreeImage IMG&(I)
                IMG&(I) = 0
            Next I
            INFILE$ = UI_InputDialogBox("File Path", _StartDir$ + "\", 1)
            CurrentFrame = 1
            OpenFile
    End Select
End Sub
Sub UI_OnRightClick (__I As _Unsigned Long)
    Select Case UI_LIST(__I).ID
        Case "ForegroundColor", "ForegroundColorBack", "ForegroundColorBox": ForegroundColor = InvertColor(ForegroundColor): UI_LIST(UI_GET_ID("ForegroundColorBox")).BColor = ForegroundColor
        Case "BackgroundColor", "BackgroundColorBack", "BackgroundColorBox": BackgroundColor = InvertColor(BackgroundColor): UI_LIST(UI_GET_ID("BackgroundColorBox")).BColor = BackgroundColor
        Case Else: UI_OnLeftClick __I
    End Select
End Sub
Sub UI_DialogBox (MSG$)
    TMP& = _CopyImage(0, 32)
    FG& = _DefaultColor
    BG& = _BackgroundColor
    L& = Len(MSG$) * _FontWidth
    Color _RGB32(255), 0
    Do
        _Limit 60
        _PutImage (0, 0)-(_Width - 1, _Height - 1), TMP&
        Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 63), BF
        While _MouseInput: Wend
        Line ((_Width - L& - 16) / 2, (_Height - 90) / 2)-((_Width + L& + 16) / 2, (_Height + 90) / 2), _RGB32(32), BF
        Line ((_Width - L& - 18) / 2, (_Height - 92) / 2)-((_Width + L& + 18) / 2, (_Height + 92) / 2), _RGB32(255), B
        _PrintString ((_Width - L&) / 2, _Height / 2 - 20), MSG$
        _PrintString (_Width / 2 - _FontWidth, _Height / 2 + 20), "OK"
        K$ = InKey$
        If Len(K$) >= 1 Then Exit Do
        If InRange(_Width / 2 - _FontWidth, _MouseX, _Width / 2 + _FontWidth) And InRange(_Height / 2 + 20, _MouseY, _Height / 2 + 20 + _FontHeight) And (_MouseButton(1) Or _MouseButton(2)) Then Exit Do
        _Display
    Loop
    Color FG&, BG&
    _FreeImage TMP&
End Sub
Function UI_InputDialogBox$ (MSG$, InitialString$, Compulsion)
    TMP& = _CopyImage(0, 32)
    S$ = InitialString$
    CursorPosition = Len(S$)
    FG& = _DefaultColor
    BG& = _BackgroundColor
    MINLENGTH = 0
    Color _RGB32(255), 0
    Do
        _Limit 60
        _PutImage (0, 0)-(_Width - 1, _Height - 1), TMP&
        Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 63), BF
        While _MouseInput: Wend
        MINLENGTH = Max(0, (Len(S$) - 16) * _FontWidth)
        Line ((_Width - 160 - MINLENGTH) / 2, (_Height - 90) / 2)-((_Width + 160 + MINLENGTH) / 2, (_Height + 90) / 2), _RGB32(32), BF
        Line ((_Width - 162 - MINLENGTH) / 2, (_Height - 92) / 2)-((_Width + 162 + MINLENGTH) / 2, (_Height + 92) / 2), _RGB32(255), B
        _PrintString ((_Width - _FontWidth * Len(MSG$)) / 2, _Height / 2 - 32), MSG$
        _PrintString (_Width / 2 - _FontWidth, _Height / 2 + 32), "OK"
        K$ = InKey$
        If InBox(_Width / 2 - _FontWidth, _Height / 2 + 32, _MouseX, _MouseY, _Width / 2 + _FontWidth, _Height / 2 + 32 + _FontHeight) And _MouseButton(1) Then Exit Do
        If InBox((_Width - 160) / 2, (_Height - 90) / 2, _MouseX, _MouseY, (_Width + 160) / 2, (_Height + 90) / 2) = 0 And _MouseButton(1) Then
            If Compulsion Then
                Line ((_Width - 162) / 2, (_Height - 92) / 2)-((_Width + 162) / 2, (_Height + 92) / 2), _RGB32(255, 0, 0), B
            Else Exit Do
            End If
        End If
        Select Case Len(K$)
            Case 1:
                Select Case Asc(K$)
                    Case 8: If Len(S$) Then S$ = Left$(S$, CursorPosition - 1) + Mid$(S$, CursorPosition + 1): CursorPosition = CursorPosition - 1
                    Case 13: Exit Do
                    Case 27: If Compulsion Then
                            Line ((_Width - 162) / 2, (_Height - 92) / 2)-((_Width + 162) / 2, (_Height + 92) / 2), _RGB32(255, 0, 0), B
                        Else
                            Color FG&, BG&: Exit Function
                        End If
                    Case 32 To 126: S$ = Left$(S$, CursorPosition) + K$ + Mid$(S$, CursorPosition + 1): CursorPosition = CursorPosition + 1
                End Select
            Case 2:
                Select Case Asc(K$, 2)
                    Case 72: CursorPosition = 1
                    Case 75: CursorPosition = Max(1, CursorPosition - 1)
                    Case 77: CursorPosition = Min(CursorPosition + 1, Len(S$) + 1)
                    Case 79: CursorPosition = Len(S$)
                    Case 83: S$ = Left$(S$, CursorPosition) + Mid$(S$, CursorPosition + 2)
                End Select
        End Select
        _PrintString ((_Width - 144 - MINLENGTH) / 2, _Height / 2), S$
        Line ((_Width - 144 - MINLENGTH) / 2 + _FontWidth * CursorPosition, _Height / 2)-((_Width - 144 - MINLENGTH) / 2 + _FontWidth * CursorPosition + 1, _Height / 2 + _FontHeight), _RGB32(255), BF
        _Display
    Loop
    Color FG&, BG&
    _FreeImage TMP&
    UI_InputDialogBox$ = S$
End Function
Function UI_ChoiceDialogBox (MSG$)
    TMP& = _CopyImage(0, 32)
    FG& = _DefaultColor
    BG& = _BackgroundColor
    L& = Max(160, Len(MSG$) * _FontWidth)
    Color _RGB32(255), 0
    Do
        _Limit 60
        _PutImage (0, 0)-(_Width - 1, _Height - 1), TMP&
        Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 63),BF
        While _MouseInput: Wend
        K$ = InKey$
        Line ((_Width - L& - 16) / 2, (_Height - 90) / 2)-((_Width + L& + 16) / 2, (_Height + 90) / 2), _RGB32(32), BF
        Line ((_Width - L& - 18) / 2, (_Height - 92) / 2)-((_Width + L& + 18) / 2, (_Height + 92) / 2), _RGB32(255), B
        _PrintString ((_Width - L&) / 2, _Height / 2 - 20), MSG$
        _PrintString (_Width / 2 - 6 * _FontWidth, _Height / 2 + 20), "Yes"
        _PrintString (_Width / 2 - 2 * _FontWidth, _Height / 2 + 20), "No"
        _PrintString (_Width / 2 + _FontWidth, _Height / 2 + 20), "Cancel"
        If Len(K$) = 1 Then
            Select Case Asc(K$)
                Case 13, 89, 121: UI_ChoiceDialogBox = 1: Exit Do
                Case 78, 110: UI_ChoiceDialogBox = -1: Exit Do
                Case 27, 67, 99: UI_ChoiceDialogBox = 0: Exit Do
            End Select
        End If
        If InRange(_Width / 2 - 6 * _FontWidth, _MouseX, _Width / 2 - 3 * _FontWidth) And InRange(_Height / 2 + 20, _MouseY, _Height / 2 + 20 + _FontHeight) And (_MouseButton(1) Or _MouseButton(2)) Then UI_ChoiceDialogBox = 1: Exit Function
        If InRange(_Width / 2 - 2 * _FontWidth, _MouseX, _Width / 2) And InRange(_Height / 2 + 20, _MouseY, _Height / 2 + 20 + _FontHeight) And (_MouseButton(1) Or _MouseButton(2)) Then UI_ChoiceDialogBox = -1: Exit Function
        If InRange(_Width / 2 + _FontWidth, _MouseX, _Width / 2 + 7 * _FontWidth) And InRange(_Height / 2 + 20, _MouseY, _Height / 2 + 20 + _FontHeight) And (_MouseButton(1) Or _MouseButton(2)) Then UI_ChoiceDialogBox = 0: Exit Function
        _Display
    Loop
    Color FG&, BG&
    _FreeImage TMP&
End Function
Function Min (A, B)
    If A < B Then Min = A Else Min = B
End Function
Function Max (A, B)
    If A > B Then Max = A Else Max = B
End Function
Function InRange (A, B, C)
    If A <= B And B <= C Then InRange = -1
End Function
Function InBox (X1, Y1, X, Y, X2, Y2)
    InBox = InRange(X1, X, X2) And InRange(Y1, Y, Y2)
End Function
'Color Dialog Box'
Function ChooseColor& (InitialColor&)
    Static COLORS As String * 64
    Dim CC As COLORDIALOGTYPE
    CC.rgbResult = _RGB32(_Blue32(InitialColor&), _Green32(InitialColor&), _Red32(InitialColor&))
    CC.lStructSize = Len(CC)
    CC.hwndOwner = _WindowHandle
    CC.flags = 3
    CC.lpCustColors = _Offset(COLORS)
    If ChooseColorA(CC) Then
        C& = _RGB32(_Blue32(CC.rgbResult), _Green32(CC.rgbResult), _Red32(CC.rgbResult))
        ChooseColor& = C&
        If InStr(COLORS, MKL$(_RGBA32(_Blue32(C&), _Green32(C&), _Red32(C&), 0))) = 0 Then COLORS = MKL$(_RGBA32(_Blue32(C&), _Green32(C&), _Red32(C&), 0)) + Left$(COLORS, 60)
    Else
        ChooseColor& = InitialColor&
    End If
End Function
'----------------'
'Erase'
Sub SetColor (X%, Y%, C&)
    Dim As _MEM S, D
    S = _MemImage(_Dest)
    IMG$ = String$(S.SIZE, 0)
    D = _Mem(_Offset(IMG$), S.SIZE)
    _MemCopy S, S.OFFSET, S.SIZE To D, D.OFFSET
    Mid$(IMG$, (Y% * _Width(_Dest) + X%) * 4 + 1, 4) = ReverseMKL$(_RGBA32(_Green32(C&), _Red32(C&), _Alpha32(C&), _Blue32(C&)))
    _MemCopy D, D.OFFSET, D.SIZE To S, S.OFFSET
    _MemFree S
    _MemFree D
    IMG$ = ""
End Sub
Sub EraseColor (X%, Y%)
    Dim As _MEM S, D
    S = _MemImage(_Dest)
    IMG$ = String$(S.SIZE, 0)
    D = _Mem(_Offset(IMG$), S.SIZE)
    _MemCopy S, S.OFFSET, S.SIZE To D, D.OFFSET
    Mid$(IMG$, (Y% * _Width(_Dest) + X%) * 4 + 1, 4) = MKL$(0)
    _MemCopy D, D.OFFSET, D.SIZE To S, S.OFFSET
    _MemFree S
    _MemFree D
    IMG$ = ""
End Sub
Function SetAlphaColor& (A&, B~%%)
    SetAlphaColor& = _RGBA32(_Red32(A&), _Green32(A&), _Blue32(A&), B~%%)
End Function
Function InvertColor& (A&)
    InvertColor& = _RGB32(255 - _Red32(A&), 255 - _Green32(A&), 255 - _Blue32(A&))
End Function
Sub ReplaceColor (A&, B&)
    Dim As _MEM S, D
    S = _MemImage(_Dest)
    IMG$ = String$(S.SIZE, 0)
    D = _Mem(_Offset(IMG$), S.SIZE)
    _MemCopy S, S.OFFSET, S.SIZE To D, D.OFFSET
    For __I% = 0 To _Width(_Dest) - 1
        For __J% = 0 To _Height(_Dest) - 1
            If Mid$(IMG$, (__J% * _Width(_Dest) + X%) * 4 + 1, 4) = ReverseMKL$(A&) Then Mid$(IMG$, (__J% * _Width(_Dest) + __I%) * 4 + 1, 4) = ReverseMKL$(B&)
    Next __J%, __I%
    _MemCopy D, D.OFFSET, D.SIZE To S, S.OFFSET
    _MemFree S
    _MemFree D
    IMG$ = ""
End Sub
'-----
'$Include:'QIMG.bm'
Function load_copy&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore copy_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_copy = O&
    Exit Function
    copy_data:
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,
    Data &H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
End Function
Function load_delete&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore delete_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_delete = O&
    Exit Function
    delete_data:
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
End Function
Function load_eraser&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore eraser_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_eraser = O&
    Exit Function
    eraser_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_export&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore export_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_export = O&
    Exit Function
    export_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_export_all&
    O& = _NewImage(32, 32, 32)
    __Dest = _Dest: _Dest O&
    Restore export_all_data
    For __X = 0 To 31: For __Y = 0 To 32
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_export_all = O&
    Exit Function
    export_all_data:
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,
    Data &H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
End Function
Function load_fill&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore fill_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_fill = O&
    Exit Function
    fill_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_left&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore left_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_left = O&
    Exit Function
    left_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_minus&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore minus_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_minus = O&
    Exit Function
    minus_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_move_cursor&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore move_cursor_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_move_cursor = O&
    Exit Function
    move_cursor_data:
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,
    Data &H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
End Function
Function load_new&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore new_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_new = O&
    Exit Function
    new_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_open&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore open_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_open = O&
    Exit Function
    open_data:
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,
    Data &H0,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,
    Data &H0,&HFFFFFFFF,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
End Function
Function load_pause&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore pause_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_pause = O&
    Exit Function
    pause_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_pencil&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore pencil_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_pencil = O&
    Exit Function
    pencil_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_pick&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore pick_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_pick = O&
    Exit Function
    pick_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_plus&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore plus_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_plus = O&
    Exit Function
    plus_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_reload&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore reload_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_reload = O&
    Exit Function
    reload_data:
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
    Data &H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,&H0,
End Function
Function load_right&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore right_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_right = O&
    Exit Function
    right_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_run&
    O& = _NewImage(16, 16, 32)
    __Dest = _Dest: _Dest O&
    Restore run_data
    For __X = 0 To 15: For __Y = 0 To 16
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_run = O&
    Exit Function
    run_data:
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
    Data &HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,&HFFFFFF,
End Function
Function load_transparent&
    O& = _NewImage(4, 4, 32)
    __Dest = _Dest: _Dest O&
    Restore transparent_data
    For __X = 0 To 3: For __Y = 0 To 4
            Read __P&
            PSet (__X, __Y), __P&
    Next __Y, __X
    _Dest __Dest
    load_transparent = O&
    Exit Function
    transparent_data:
    Data &HFFC3C3C3,&HFF7F7F7F,&HFFC3C3C3,&HFF7F7F7F,
    Data &HFF7F7F7F,&HFFC3C3C3,&HFF7F7F7F,&HFFC3C3C3,
    Data &HFFC3C3C3,&HFF7F7F7F,&HFFC3C3C3,&HFF7F7F7F,
    Data &HFF7F7F7F,&HFFC3C3C3,&HFF7F7F7F,&HFFC3C3C3,
End Function
