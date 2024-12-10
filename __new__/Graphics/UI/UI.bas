'$Dynamic
Type UI_Type
    As _Unsigned _Byte Type
    As String IDLabel, Label, Text, Date, Time
    As _Unsigned _Byte IsShown, IsVisible, IsResizable, IsNotCancellable, IsValueInteger, IsLeftClicking, IsLeftClicked, IsRightClicking, IsRightClicked, IsHovered, TimeHovered
    As _Unsigned Integer Width, Height, PositionX, PositionY, BorderSize
    As _Unsigned Long ImageHandle, FColor, BColor, SFColor, SBColor, BorderColor
    As _Unsigned Long Parent, OnLeftClick, OnRightClick, OnValueChange
    As Double Value, MinValue, MaxValue, ScrollValue
    As String ExtraProperties, ToolTip
End Type
Const UI_TYPE_BUTTON = 1, UI_TYPE_LABEL = 2, UI_TYPE_TEXTBOX = 3, UI_TYPE_IMAGEBOX = 4, UI_TYPE_LISTBOX = 5, UI_TYPE_SELECTBOX = 6, UI_TYPE_HOVERBOX = 7, UI_TYPE_RADIOGROUP = 8, UI_TYPE_CHECKBOX = 9, UI_TYPE_TABGROUP = 10, UI_TYPE_SEEKBAR = 11, UI_TYPE_PROGRESS = 12, UI_TYPE_CANVAS = 13, UI_TYPE_MENUBAR = 15, UI_TYPE_CONTEXTMENU = 15
Const UI_DIALOG_MESSAGE = 65, UI_DIALOG_TEXTBOX = 66, UI_DIALOG_BUTTON = 67, UI_DIALOG_CUSTOM = 68
Dim Shared UI_Components(0) As UI_Type, UI_KeyPressed As String, UI_Parent_Focus As _Unsigned Long, UI_Focus As _Unsigned Long, UI_DefaultForegroundColor As Long, UI_DefaultBackgroundColor As Long, UI_DefaultSelectForegroundColor As Long, UI_DefaultSelectBackgroundColor As Long
UI_DefaultForegroundColor = _RGB32(255): UI_DefaultBackgroundColor = _RGB32(32): UI_DefaultSelectForegroundColor = _RGB32(255): UI_DefaultSelectBackgroundColor = _RGB32(64)

Screen _NewImage(640, 640, 32)
UI_Create UI_TYPE_LISTBOX, "ListBox1", "List Box 1", 320, 64, 160, 160, 0, 0
UI_Components(1).Text = "List Element 1|List Element 2|LE3|LE4|LE5|LE6|LE7"
Do
    Cls , _RGB32(16)
    _Limit 60
    While _MouseInput: Wend
    UI_Update: _Title Str$(UI_Focus) + Str$(UI_Components(1).ScrollValue)
    _Display
Loop

Sub UI_Create (__Type As _Unsigned _Byte, __IDLabel As String, __Label As String, __Width As _Unsigned Integer, __Height As _Unsigned Integer, __PositionX As _Unsigned Integer, __PositionY As _Unsigned Integer, __ImageHandle As Long, __Parent As _Unsigned Long)
    Dim As _Unsigned Long __ID
    __ID = UBound(UI_Components) + 1
    ReDim _Preserve UI_Components(1 To __ID) As UI_Type
    UI_Components(__ID).Type = __Type
    UI_Components(__ID).IDLabel = __IDLabel: UI_Components(__ID).Label = __Label: UI_Components(__ID).Text = "": UI_Components(__ID).Date = "": UI_Components(__ID).Time = ""
    UI_Components(__ID).IsShown = 0: UI_Components(__ID).IsVisible = Abs(Sgn(__Type <= 64)): UI_Components(__ID).IsResizable = 0: UI_Components(__ID).IsNotCancellable = 0: UI_Components(__ID).IsValueInteger = 1: UI_Components(__ID).IsHovered = 0: UI_Components(__ID).TimeHovered = 0
    UI_Components(__ID).Width = __Width: UI_Components(__ID).Height = __Height: UI_Components(__ID).PositionX = __PositionX: UI_Components(__ID).PositionY = __PositionY: UI_Components(__ID).BorderSize = 0
    UI_Components(__ID).ImageHandle = __ImageHandle: UI_Components(__ID).FColor = UI_DefaultForegroundColor: UI_Components(__ID).BColor = UI_DefaultBackgroundColor: UI_Components(__ID).SFColor = UI_DefaultSelectForegroundColor: UI_Components(__ID).SBColor = UI_DefaultSelectBackgroundColor: UI_Components(__ID).BorderColor = _RGB32(0)
    UI_Components(__ID).Parent = __Parent: UI_Components(__ID).OnLeftClick = 0: UI_Components(__ID).OnRightClick = 0: UI_Components(__ID).OnValueChange = 0
    UI_Components(__ID).Value = 0: UI_Components(__ID).MinValue = 0: UI_Components(__ID).MaxValue = 0
    UI_Components(__ID).ExtraProperties = "": UI_Components(__ID).ToolTip = __Label
    If __Type >= UI_DIALOG_MESSAGE Then
        UI_Components(__ID).PositionX = (_Width - __Width) / 2
        UI_Components(__ID).PositionY = (_Height - __Height) / 2
        UI_Components(__ID).BorderSize = 2
        UI_Components(__ID).ToolTip = ""
    End If
    If __Type = UI_TYPE_TEXTBOX Or __Type = UI_DIALOG_TEXTBOX Then
        UI_Components(__ID).Value = Len(UI_Components(__ID).Text)
        UI_Components(__ID).ExtraProperties = "OK"
    End If
    If __Type = UI_TYPE_LISTBOX Then UI_Components(__ID).ToolTip = ""
End Sub
Sub UI_Update
    Dim As _Unsigned Long __ID, __ToolTipID
    Dim As _Unsigned Integer __PX, __PY, __X1, __Y1, __X2, __Y2, __HalfLabelLength
    Dim As _Unsigned Long __FColor, __BColor
    Dim As String __Label
    UI_KeyPressed = InKey$
    For __ID = 1 To UBound(UI_Components)
        If UI_Components(__ID).IsVisible = 0 Then _Continue
        If UI_Components(__ID).Parent Then
            If UI_Components(UI_Components(__ID).Parent).IsVisible = 0 Then Exit Sub
            __PX = UI_Components(UI_Components(__ID).Parent).PositionX
            __PY = UI_Components(UI_Components(__ID).Parent).PositionY
        End If
        __X1 = __PX + UI_Components(__ID).PositionX: __Y1 = __PX + UI_Components(__ID).PositionY: __X2 = __X1 + UI_Components(__ID).Width: __Y2 = __Y1 + UI_Components(__ID).Height
        UI_Components(__ID).IsLeftClicked = 0: UI_Components(__ID).IsRightClicked = 0
        If UI_FUNCTION_MouseInUI(__ID) And (UI_Focus = UI_Components(__ID).Parent Or UI_Parent_Focus = UI_Components(__ID).Parent) Then
            UI_Components(__ID).IsHovered = -1
            If UI_Components(__ID).IsLeftClicking And _MouseButton(1) = 0 Then UI_Components(__ID).IsLeftClicked = -1
            If UI_Components(__ID).IsLeftClicked Then UI_Focus = __ID: If UI_Components(__ID).OnLeftClick Then UI_Components(UI_Components(__ID).OnLeftClick).IsVisible = 1
            If UI_Components(__ID).IsRightClicking And _MouseButton(1) = 0 Then UI_Components(__ID).IsRightClicked = -1
            If UI_Components(__ID).IsRightClicked Then UI_Focus = __ID: If UI_Components(__ID).OnRightClick Then UI_Components(UI_Components(__ID).OnRightClick).IsVisible = 1
            UI_Components(__ID).TimeHovered = UI_Components(__ID).TimeHovered + Sgn(255 - UI_Components(__ID).TimeHovered)
        Else
            UI_Components(__ID).TimeHovered = 0
        End If
        If UI_Focus Then UI_Parent_Focus = UI_Components(UI_Focus).Parent
        UI_Components(__ID).IsLeftClicking = _MouseButton(1): UI_Components(__ID).IsRightClicking = _MouseButton(2)
        __FColor = _DefaultColor: __BColor = _BackgroundColor: Color UI_Components(__ID).FColor, UI_Components(__ID).BColor
        Line (__X1 - UI_Components(__ID).BorderSize, __Y1 - UI_Components(__ID).BorderSize)-(__X2 + UI_Components(__ID).BorderSize, __Y2 + UI_Components(__ID).BorderSize), UI_Components(__ID).BorderColor, BF
        Select Case UI_Components(__ID).Type
            Case UI_TYPE_BUTTON
                Line (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).BColor, BF
                __Label = Left$(UI_Components(__ID).Label, UI_Components(__ID).Width \ _FontWidth): _PrintString (__X1 + (UI_Components(__ID).Width - Len(__Label) * _FontWidth) / 2, __Y1 + (UI_Components(__ID).Height - _FontHeight) / 2), __Label
            Case UI_TYPE_LABEL
                Line (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).BColor, BF
                __Label = Left$(UI_Components(__ID).Label, UI_Components(__ID).Width \ _FontWidth): _PrintString (__X1 + (UI_Components(__ID).Width - Len(__Label) * _FontWidth) / 2, __Y1 + (UI_Components(__ID).Height - _FontHeight) / 2), __Label
            Case UI_TYPE_TEXTBOX
                Line (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).BColor, BF
                _PrintString (__X1, __Y1), Right$(UI_Components(__ID).Text, UI_Components(__ID).Width \ _FontWidth)
                If UI_Focus = __ID Then
                    Select Case Len(UI_KeyPressed)
                        Case 1: Select Case Asc(UI_KeyPressed)
                                Case 8: UI_Components(__ID).Text = Left$(UI_Components(__ID).Text, UI_Components(__ID).Value - 1) + Mid$(UI_Components(__ID).Text, UI_Components(__ID).Value + 1): UI_Components(__ID).Value = UI_Components(__ID).Value - 1
                                Case 9: UI_Components(__ID).Text = Left$(UI_Components(__ID).Text, UI_Components(__ID).Value) + Space$(4) + Mid$(UI_Components(__ID).Text, UI_Components(__ID).Value + 1)
                                Case 32 To 126: UI_Components(__ID).Text = Left$(UI_Components(__ID).Text, UI_Components(__ID).Value) + UI_KeyPressed + Mid$(UI_Components(__ID).Text, UI_Components(__ID).Value + 1): UI_Components(__ID).Value = UI_Components(__ID).Value + 1
                            End Select
                        Case 2: Select Case Asc(UI_KeyPressed, 2)
                            End Select
                    End Select
                    Line (__X1 + UI_Components(__ID).Value * _FontWidth, __Y1)-(__X1 + UI_Components(__ID).Value * _FontWidth, __Y1 + _FontHeight), UI_Components(__ID).FColor
                End If
            Case UI_TYPE_IMAGEBOX
                _PutImage (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).ImageHandle
            Case UI_TYPE_LISTBOX
                Line (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).BColor, BF
                Dim __STR As String
                Dim __STRA(1 To 1000) As String
                Dim As _Unsigned Long __C, __I
                __STR = UI_Components(__ID).Text
                __C = 1
                Do
                    __C = InStr(__STR, "|")
                    If __C = 0 Then Exit Do
                    __I = __I + 1: __STRA(__I) = Left$(__STR, __C - 1): __STR = Mid$(__STR, __C + 1)
                Loop: __I = __I + 1: __STRA(__I) = __STR: __STR = ""
                For __C = UI_FUNCTION_Max(1, UI_Components(__ID).ScrollValue) To UI_FUNCTION_Min(UI_Components(__ID).ScrollValue + UI_Components(__ID).Height \ (_FontHeight + 1), __I) + 1
                    If (_FontHeight + 1) * (__C - UI_FUNCTION_Max(1, UI_Components(__ID).ScrollValue)) + _FontHeight - 3 > UI_Components(__ID).Height Then _Continue
                    If UI_FUNCTION_InRange(__X1 + 1, _MouseX, __X2 - 1) And UI_FUNCTION_InRange(__Y1 + (_FontHeight + 1) * __C, _MouseY, __Y1 + (_FontHeight + 1) * __C + _FontHeight) Then
                        Line (__X1 + 1, __Y1 + (_FontHeight + 1) * __C)-(__X2 - 17, __Y1 + (_FontHeight + 1) * __C + _FontHeight), UI_Components(__ID).SBColor, BF
                        Color UI_Components(__ID).SFColor, 0
                        _PrintString (__X1 + 1, __Y1 + (_FontHeight + 1) * (__C - UI_FUNCTION_Max(1, UI_Components(__ID).ScrollValue))), Left$(__STRA(__C), (UI_Components(__ID).Width - 18) \ _FontWidth)
                        UI_Components(__ID).Value = __C
                    Else
                        _PrintString (__X1 + 1, __Y1 + (_FontHeight + 1) * (__C - UI_FUNCTION_Max(1, UI_Components(__ID).ScrollValue))), Left$(__STRA(__C), (UI_Components(__ID).Width - 18) \ _FontWidth)
                    End If
                    If __C < __I Then Line (__X1 + 1, __Y1 + (_FontHeight + 1) * (__C - UI_FUNCTION_Max(1, UI_Components(__ID).ScrollValue)) + _FontHeight)-(__X2 - 1, __Y1 + (_FontHeight + 1) * (__C - UI_FUNCTION_Max(1, UI_Components(__ID).ScrollValue)) + _FontHeight), UI_Components(__ID).BorderColor
                Next __C
                __PERCENT = UI_FUNCTION_Max((UI_Components(__ID).Height - 2) \ (__I - 1 + (UI_Components(__ID).Height - 2) / _FontHeight), 1)
                Line (__X2 - 16, __Y1 + 1 + (UI_Components(__ID).Height - 2) * UI_Components(__ID).ScrollValue / __PERCENT)-(__X2 - 1, __Y1 + 1 + (UI_Components(__ID).Height - 2) * (UI_Components(__ID).ScrollValue + 1) / __PERCENT), UI_Components(__ID).FColor, BF
                If UI_FUNCTION_InRange(__X2 - 16, _MouseX, __X2 - 1) And UI_FUNCTION_InRange(__Y1 + 1, _MouseY, __Y2 - 1) And _MouseButton(1) Then
                    While _MouseInput: Wend
                    UI_Components(__ID).ScrollValue = Int((_MouseY - __Y1 - 1) / (UI_Components(__ID).Height - 2) * __PERCENT)
                End If
                UI_Components(__ID).ScrollValue = UI_FUNCTION_Max(UI_Components(__ID).ScrollValue, 1)
                UI_Components(__ID).ScrollValue = UI_FUNCTION_Min(UI_Components(__ID).ScrollValue, __I - (UI_Components(__ID).Height - 2) \ _FontHeight)
            Case UI_TYPE_SELECTBOX
            Case UI_TYPE_HOVERBOX
            Case UI_TYPE_RADIOGROUP
            Case UI_TYPE_CHECKBOX
            Case UI_TYPE_TABGROUP
            Case UI_TYPE_SEEKBAR
            Case UI_TYPE_PROGRESS
            Case UI_TYPE_CANVAS
            Case UI_TYPE_MENUBAR
            Case UI_TYPE_CONTEXTMENU
            Case UI_DIALOG_MESSAGE
                Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 63), BF
                Line (__X1 - UI_Components(__ID).BorderSize, __Y1 - UI_Components(__ID).BorderSize)-(__X2 + UI_Components(__ID).BorderSize, __Y2 + UI_Components(__ID).BorderSize), UI_Components(__ID).BorderColor, BF
                Line (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).BColor, BF
                __HalfLabelLength = Len(UI_Components(__ID).Label) * _FontWidth / 2
                _PrintString (_Width / 2 - __HalfLabelLength, _Height / 2 - _FontHeight), UI_Components(__ID).Label
                If UI_Focus = __ID And Len(UI_KeyPressed) Then If Asc(UI_KeyPressed) = 27 Then UI_Components(__ID).IsVisible = 0: UI_Focus = UI_Components(__ID).Parent
            Case UI_DIALOG_TEXTBOX
                Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 63), BF
                Line (__X1 - UI_Components(__ID).BorderSize, __Y1 - UI_Components(__ID).BorderSize)-(__X2 + UI_Components(__ID).BorderSize, __Y2 + UI_Components(__ID).BorderSize), UI_Components(__ID).BorderColor, BF
                Line (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).BColor, BF
                __HalfLabelLength = Len(UI_Components(__ID).Label) * _FontWidth / 2
                _PrintString (_Width / 2 - __HalfLabelLength, _Height / 2 - _FontHeight * 1.5), UI_Components(__ID).Label
                Line (__X1 + UI_Components(__ID).Width / 10 - 3, __Y2 - _FontHeight * 3 - 3)-(__X2 - UI_Components(__ID).Width / 10 + 2, __Y2 - _FontHeight * 2 + 1), UI_Components(__ID).FColor, B
                _PrintString (__X1 + UI_Components(__ID).Width / 10, __Y2 - _FontHeight * 3), Right$(UI_Components(__ID).Text, (UI_Components(__ID).Width * .8) \ _FontWidth)
                If UI_Focus = __ID Then
                    Select Case Len(UI_KeyPressed)
                        Case 1: Select Case Asc(UI_KeyPressed)
                                Case 8: UI_Components(__ID).Text = Left$(UI_Components(__ID).Text, UI_Components(__ID).Value - 1) + Mid$(UI_Components(__ID).Text, UI_Components(__ID).Value + 1): UI_Components(__ID).Value = UI_Components(__ID).Value - 1
                                Case 9: UI_Components(__ID).Text = Left$(UI_Components(__ID).Text, UI_Components(__ID).Value) + Space$(4) + Mid$(UI_Components(__ID).Text, UI_Components(__ID).Value + 1)
                                Case 32 To 126: UI_Components(__ID).Text = Left$(UI_Components(__ID).Text, UI_Components(__ID).Value) + UI_KeyPressed + Mid$(UI_Components(__ID).Text, UI_Components(__ID).Value + 1): UI_Components(__ID).Value = UI_Components(__ID).Value + 1
                            End Select
                        Case 2: Select Case Asc(UI_KeyPressed, 2)
                            End Select
                    End Select
                    Line (__X1 + UI_Components(__ID).Width / 10 + UI_Components(__ID).Value * _FontWidth, __Y2 - _FontHeight * 3)-(__X1 + UI_Components(__ID).Width / 10 + UI_Components(__ID).Value * _FontWidth, __Y2 - _FontHeight * 2 - 1), UI_Components(__ID).FColor
                    If Len(UI_KeyPressed) Then If Asc(UI_KeyPressed) = 27 Then UI_Components(__ID).IsVisible = 0: UI_Focus = UI_Components(__ID).Parent
                End If
                __HalfTextLength = Len(UI_Components(__ID).ExtraProperties) * _FontWidth / 2
                Line (__X1 + UI_Components(__ID).Width / 2 - __HalfTextLength - 3, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight - 3)-(__X1 + UI_Components(__ID).Width / 2 + __HalfTextLength + 2, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight * 2 + 1), UI_Components(__ID).FColor, B
                _PrintString (__X1 + UI_Components(__ID).Width / 2 - __HalfTextLength, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight), UI_Components(__ID).ExtraProperties
                If _MouseButton(1) And UI_FUNCTION_InBox(__X1 + UI_Components(__ID).Width / 2 - __HalfTextLength, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight, _MouseX, _MouseY, __X1 + UI_Components(__ID).Width / 2 + __HalfTextLength, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight * 2 - 1) Then UI_Components(__ID).IsVisible = 0: UI_Focus = UI_Components(__ID).Parent
            Case UI_DIALOG_BUTTON
                Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 63), BF
                Line (__X1 - UI_Components(__ID).BorderSize, __Y1 - UI_Components(__ID).BorderSize)-(__X2 + UI_Components(__ID).BorderSize, __Y2 + UI_Components(__ID).BorderSize), UI_Components(__ID).BorderColor, BF
                Line (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).BColor, BF
                __HalfLabelLength = Len(UI_Components(__ID).Label) * _FontWidth / 2
                _PrintString (_Width / 2 - __HalfLabelLength, _Height / 2 - _FontHeight * 1.5), UI_Components(__ID).Label
                __HalfTextLength = Len(UI_Components(__ID).Text) * _FontWidth / 2
                Line (__X1 + UI_Components(__ID).Width / 2 - __HalfTextLength - 3, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight / 2 - 3)-(__X1 + UI_Components(__ID).Width / 2 + __HalfTextLength + 2, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight * 1.5 + 1), UI_Components(__ID).FColor, B
                _PrintString (__X1 + UI_Components(__ID).Width / 2 - __HalfTextLength, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight / 2), UI_Components(__ID).Text
                If _MouseButton(1) And UI_FUNCTION_InBox(__X1 + UI_Components(__ID).Width / 2 - __HalfTextLength, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight / 2, _MouseX, _MouseY, __X1 + UI_Components(__ID).Width / 2 + __HalfTextLength, __Y1 + UI_Components(__ID).Height / 2 + _FontHeight * 1.5 - 1) Then UI_Components(__ID).IsVisible = 0: UI_Focus = UI_Components(__ID).Parent
                If UI_Focus = __ID And Len(UI_KeyPressed) Then If Asc(UI_KeyPressed) = 27 Then UI_Components(__ID).IsVisible = 0: UI_Focus = UI_Components(__ID).Parent
            Case UI_DIALOG_CUSTOM
                Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 63), BF
                Line (__X1 - UI_Components(__ID).BorderSize, __Y1 - UI_Components(__ID).BorderSize)-(__X2 + UI_Components(__ID).BorderSize, __Y2 + UI_Components(__ID).BorderSize), UI_Components(__ID).BorderColor, BF
                Line (__X1, __Y1)-(__X2, __Y2), UI_Components(__ID).BColor, BF
                If UI_Focus = __ID And Len(UI_KeyPressed) Then If Asc(UI_KeyPressed) = 27 Then UI_Components(__ID).IsVisible = 0: UI_Focus = UI_Components(__ID).Parent
        End Select
        Color __FColor, __BColor
        If Len(UI_KeyPressed) Then If Asc(UI_KeyPressed) = 27 Then UI_Focus = UI_Components(__ID).Parent
        If UI_Components(__ID).TimeHovered > 60 Then __ToolTipID = __ID
    Next __ID
    If __ToolTipID Then UI_Show_ToolTip __ToolTipID
End Sub
Sub UI_Show_ToolTip (__ID As _Unsigned Long)
    Dim As _Unsigned Integer __PX, __PY
    If UI_Components(__ID).Parent Then
        If UI_Components(UI_Components(__ID).Parent).IsVisible = 0 Then Exit Sub
        __PX = UI_Components(UI_Components(__ID).Parent).PositionX: __PY = UI_Components(UI_Components(__ID).Parent).PositionY
    End If
    If Len(UI_Components(__ID).ToolTip) = 0 Then Exit Sub
    Dim As _Unsigned Integer __X1, __Y1, __Y2: __X1 = __PX + UI_Components(__ID).PositionX + UI_Components(__ID).Width / 2: __Y1 = __PY + UI_Components(__ID).PositionY - _FontHeight: __Y2 = __PY + UI_Components(__ID).PositionY - 1
    Dim As _Unsigned Long __FColor, __BColor: __FColor = _DefaultColor: __BColor = _BackgroundColor: Color UI_Components(__ID).FColor, 0
    Dim As _Unsigned Integer __HalfToolTipLength: __HalfToolTipLength = Len(UI_Components(__ID).ToolTip) * _FontWidth / 2
    Line (__X1 - __HalfToolTipLength, __Y1)-(__X1 + __HalfToolTipLength, __Y2), UI_Components(__ID).BColor, BF: _PrintString (__X1 - __HalfToolTipLength, __Y1), UI_Components(__ID).ToolTip
Color __FColor, __BColor: End Sub
Function UI_FUNCTION_Min (__A, __B): UI_FUNCTION_Min = -__A * (__A < __B) - __B * (__A >= __B): End Function
Function UI_FUNCTION_Max (__A, __B): UI_FUNCTION_Max = -__A * (__A > __B) - __B * (__A <= __B): End Function
Function UI_FUNCTION_InRange~%% (__A As Double, __B As Double, __C As Double): UI_FUNCTION_InRange = (__A <= __B) And (__B <= __C): End Function
Function UI_FUNCTION_InBox~%% (__X1 As _Unsigned Integer, __Y1 As _Unsigned Integer, X As _Unsigned Integer, Y As _Unsigned Integer, __X2 As _Unsigned Integer, __Y2 As _Unsigned Integer): UI_FUNCTION_InBox~%% = UI_FUNCTION_InRange(0, X - __X1, __X2 - __X1) And UI_FUNCTION_InRange(0, Y - __Y1, __Y2 - __Y1): End Function
Function UI_FUNCTION_MouseInUI~%% (__ID As _Unsigned Long): Dim As _Unsigned Integer __PX, __PY: If UI_Components(__ID).Parent Then __PX = UI_Components(UI_Components(__ID).Parent).PositionX: __PY = UI_Components(UI_Components(__ID).Parent).PositionY
UI_FUNCTION_MouseInUI~%% = UI_FUNCTION_InBox~%%(__PX + UI_Components(__ID).PositionX, __PY + UI_Components(__ID).PositionY, _MouseX, _MouseY, __PX + UI_Components(__ID).PositionX + UI_Components(__ID).Width, __PY + UI_Components(__ID).PositionY + UI_Components(__ID).Height): End Function

