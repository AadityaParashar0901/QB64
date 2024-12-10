$Console
Dim As Long rgb, a
Dim As Single red32, green32, blue32, alpha32
Dim fileList(16384) As String
Dim imageHandles(16384) As Long
Dim As Long tmpColor, tmp2Color
Dim As Long dynamicRed, dynamicGreen, dynamicBlue, dynamicAlpha
Dim As Long customdynamicRed, customdynamicGreen, customdynamicBlue, customdynamicAlpha
Dim fileN As Long
Type trimFile
    As Integer W, H, X, Y
End Type
Dim As Integer W, H
Dim As Single WidthRatio, HeightRatio
Dim trim(1024) As trimFile
Dim Shared LOG_FILE
Dim Shared SCALE, ScaleFactor As Single
SCALE = 100
_DisplayOrder _Software
For i = 1 To _CommandCount
    Select Case Command$(i)
        Case "--no-frameskip", "-nf": FRAMESKIP = 1: Write_log "FrameSkip Off"
        Case "--frameskip": FRAMESKIP = -1: Write_log "FrameSkip On"
        Case "--full-dynamic-bright", "-fdb": DYNAMIC_MODE = 1: Write_log "Full Dynamic, Increase Brightness"
        Case "--full-dynamic", "-fd": DYNAMIC_MODE = 1.5: Write_log "Full Dynamic"
        Case "--only-dynamic", "-od": DYNAMIC_MODE = 2: Write_log "Only Dynamic"
        Case "--framespeed", "-fps": FORCE_FPS = Val(Command$(i + 1)): i = i + 1: Write_log "Force FPS:" + Hex$(FORCE_FPS)
        Case "--color-red", "-cr":
            If Left$(Command$(i + 1), 1) = "#" Then customdynamicRed = Val("&HFF" + Mid$(Command$(i + 1), 2)) Else customdynamicRed = Val(Command$(i + 1))
            i = i + 1
            Write_log "Custom Red Colour:" + Hex$(customdynamicRed)
        Case "--color-green", "-cg":
            If Left$(Command$(i + 1), 1) = "#" Then customdynamicGreen = Val("&HFF" + Mid$(Command$(i + 1), 2)) Else customdynamicGreen = Val(Command$(i + 1))
            i = i + 1
            Write_log "Custom Green Colour:" + Hex$(customdynamicGreen)
        Case "--color-blue", "-cb":
            If Left$(Command$(i + 1), 1) = "#" Then customdynamicBlue = Val("&HFF" + Mid$(Command$(i + 1), 2)) Else customdynamicBlue = Val(Command$(i + 1))
            i = i + 1
            Write_log "Custom Blue Colour:" + Hex$(customdynamicBlue)
        Case "--color-alpha", "-ca":
            If Left$(Command$(i + 1), 1) = "#" Then customdynamicAlpha = Val("&HFF" + Mid$(Command$(i + 1), 2)) Else customdynamicAlpha = Val(Command$(i + 1))
            i = i + 1
            Write_log "Custom Alpha Colour:" + Hex$(customdynamicAlpha)
        Case "--fullscreen": FULLSCREEN_MODE = -1: Write_log "Fullscreen On"
        Case "--save", "-s": SAVE_MODE = 1: Write_log "Saving Displayed Images"
        Case "--save-trim", "-st": SAVE_MODE = 2: Write_log "Saving Trimmed Displayed Images"
        Case "--scale": SCALE = Val(Command$(i + 1)): i = i + 1: Write_log "Scaling Enabled"
        Case "--width", "-w": ForceWidth = Val(Command$(i + 1)): i = i + 1
        Case "--height", "-h": ForceHeight = Val(Command$(i + 1)): i = i + 1
        Case "--log": LOG_FILE = -1
            Open "log.txt" For Output As #4
        Case "--help", "-h":
            _Dest _Console
            Print "    --frameskip"
            Print "    --no-frameskip              -nf"
            Print "    --framespeed                -fps"
            Print "    --full-dynamic-bright       -fdb"
            Print "    --full-dynamic              -fd"
            Print "    --only-dynamic              -od"
            Print "    --color-red                 -cr"
            Print "    --color-green               -cg"
            Print "    --color-blue                -cb"
            Print "    --color-alpha               -ca"
            Print "    --fullscreen"
            Print "    --save                      -s"
            Print "    --save-trim                 -st"
            Print "    --scale"
            Print "    --width                     -w"
            Print "    --height                    -h"
            Print "    --log"
            Print "    --help"
            System
    End Select
Next i
ScaleFactor = SCALE / 100
If SAVE_MODE Then
    If _DirExists("out") Then Shell _Hide "rd /s /q out"
    MkDir "out"
End If
If Not _FileExists("desc.txt") Then System
Open "desc.txt" For Input As #1
If SAVE_MODE Then Open ".\out\desc.txt" For Output As #11
Input #1, file$
W = Val(Left$(file$, InStr(file$, " ")))
file$ = Right$(file$, Len(file$) - InStr(file$, " "))
H = Val(Left$(file$, InStr(file$, " ")))
file$ = Right$(file$, Len(file$) - InStr(file$, " "))
FPS = Val(file$)
If FORCE_FPS Then FPS = FORCE_FPS
If SAVE_MODE Then
    Print #11, __T$(ForceWidth) + " " + __T$(ForceHeight) + " " + __T$(FPS)
End If

If FULLSCREEN_MODE Then
    If W < _DesktopWidth Then ScreenWidth = _DesktopWidth Else ScreenWidth = W * ScaleFactor
    If H < _DesktopHeight Then ScreenHeight = _DesktopHeight Else ScreenHeight = H * ScaleFactor
    OffsetX = (_DesktopWidth - W) / 2
    OffsetY = (_DesktopHeight - H) / 2
    If OffsetX < 0 Then OffsetX = 0
    If OffsetY < 0 Then OffsetY = 0
Else
    If ForceWidth Or ForceHeight Then
        If ForceWidth = 0 Then ForceWidth = W
        If ForceHeight = 0 Then ForceHeight = H
        ScreenWidth = ForceWidth
        ScreenHeight = ForceHeight
        WidthRatio = ForceWidth / W * ScaleFactor
        HeightRatio = ForceHeight / H * ScaleFactor
        OffsetX = (ForceWidth - W) / 2
        OffsetY = (ForceHeight - H) / 2
    Else
        ScreenWidth = W
        ScreenHeight = H
    End If
End If
OffsetX = OffsetX + W * (1 - ScaleFactor) / 2
OffsetY = OffsetY + H * (1 - ScaleFactor) / 2
Write_log "Screen:" + Str$(ScreenWidth) + "," + Str$(ScreenHeight)
Write_log "Offset:" + Str$(OffsetX) + "," + Str$(OffsetY)
Screen _NewImage(ScreenWidth, ScreenHeight, 32)
If ScreenWidth >= _DesktopWidth Or ScreenHeight >= _DesktopHeight Or FULLSCREEN_MODE Then Write_log "Switching to Fullscreen Mode": _FullScreen _SquarePixels , _Smooth
_MouseHide
Do While EOF(1) = 0
    i = 0
    FILE_LINE = FILE_LINE + 1
    Line Input #1, file$
    Write_log "Reading File: " + _Trim$(Str$(FILE_LINE)) + ": " + Chr$(34) + file$ + Chr$(34)
    If _Trim$(file$) = "" Then Exit Do
    If InStr(file$, "dynamic_colors") Then
        dynamicColor = -1
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        customDynamicColourStartPath$ = _Trim$(Left$(file$, InStr(file$, " "))): Write_log "Dynamic Colours Starting Path: " + Chr$(34) + customDynamicColourStartPath$ + Chr$(34)
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        dynamicRed = Val("&HFF" + Right$(Left$(file$, InStr(file$, " ")), 7))
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        dynamicGreen = Val("&HFF" + Right$(Left$(file$, InStr(file$, " ")), 7))
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        dynamicBlue = Val("&HFF" + Right$(Left$(file$, InStr(file$, " ")), 7))
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        dynamicAlpha = Val("&HFF" + Right$(Left$(file$, InStr(file$, " ")), 7))
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        customDynamicColourStartFrame = Val(Right$(file$, Len(file$) - _InStrRev(file$, " "))): Write_log "Dynamic Colours Starting Frame:" + Hex$(customDynamicColourStartFrame)
        If FRAMESKIP = 0 Then FRAMESKIP = -1: Write_log "FrameSkip On"
        If SAVE_MODE Then FRAMESKIP = 1: Write_log "FrameSkip Off"
    Else
        mode$ = Left$(file$, 1)
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        count = Val(Left$(file$, InStr(file$, " ")))
        If count = 0 Then finalCount = 24
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        pause = Val(Left$(file$, InStr(file$, " ")))
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        path$ = _Trim$(Left$(file$, InStr(file$, " ")))
        If path$ = "" Then
            path$ = _Trim$(file$)
            file$ = ""
        End If
        file$ = Right$(file$, Len(file$) - InStr(file$, " "))
        tmp$ = Left$(file$, InStr(file$, " "))
        If tmp$ = "" Then
            tmp$ = _Trim$(file$)
            file$ = ""
        End If
        rgb = Val("&H" + Right$(tmp$, Len(tmp$) - 1)): Write_log "Background Color:" + Hex$(rgb)
        If _DirExists(path$) Then
            If SAVE_MODE Then MkDir "out\" + path$
            ChDir path$: Write_log "Changing Directory to " + path$
            If SAVE_MODE Then
                Print #11, mode$ + " " + __T$(count) + " " + __T$(pause) + " " + path$ + " #" + String$(6 - Len(Hex$(rgb)), "0") + Hex$(rgb)
            End If
        Else
            Print path$; " does not exist": Write_log path$ + " does not exist"
            GoTo SkipDisplay
        End If
        Write_log "Getting Images List Alphabetically"
        Shell _Hide "dir /b /o:n > dir.txt"
        Open "dir.txt" For Input As #2
        fileN = 0
        Do While EOF(2) = 0
            Input #2, filename$
            filename$ = _Trim$(filename$)
            If filename$ <> "dir.txt" And filename$ <> "trim.txt" Then
                fileList(fileN) = _Trim$(filename$)
                fileN = fileN + 1
            End If
        Loop
        Close #2
        Kill "dir.txt"
        If _FileExists("trim.txt") Then trimFile = -1 Else trimFile = 0
        If trimFile Then
            If SAVE_MODE = 2 Then Open "..\out\" + path$ + "\trim.txt" For Output As #10
            Write_log "Found Trim File"
            Open "trim.txt" For Input As #3
            Do While EOF(3) = 0
                Input #3, a$
                trim(i).W = Val(Left$(a$, InStr(a$, "x")))
                trimTmp$ = Right$(a$, Len(a$) - InStr(a$, "x"))
                trim(i).H = Val(Left$(trimTmp$, InStr(trimTmp$, "+")))
                a$ = trimTmp$
                trimTmp$ = Right$(a$, Len(a$) - InStr(a$, "+"))
                trim(i).X = Val(Left$(trimTmp$, InStr(trimTmp$, "+")))
                a$ = trimTmp$
                trimTmp$ = Right$(a$, Len(a$) - InStr(a$, "+"))
                trim(i).Y = Val(trimTmp$)
                If SAVE_MODE = 2 Then Print #10, __T$(Int(trim(i).W * WidthRatio)) + "x" + __T$(Int(trim(i).H * HeightRatio)) + "+" + __T$(Int(trim(i).X * WidthRatio)) + "+" + __T$(Int(trim(i).Y * HeightRatio))
                i = i + 1
            Loop
            Close #3
            If SAVE_MODE = 2 Then Close #10
        End If
        If SAVE_MODE Or EndAnimation Then finalCount = 0
        If mode$ = "p" And EndAnimation Then finalCount = -1
        If SAVE_MODE = 2 And trimFile = 0 Then Open "..\out\" + path$ + "\trim.txt" For Output As #21: Write_log "    Writing New Trim File"
        imagesLoaded = 0
        For loopN = 0 To finalCount Step 1
            Write_log "    Loop Number:" + Str$(loopN)
            If finalCount = loopN - 1 Then Exit For
            If imagesLoaded = 0 Then
                Write_log "       Loading Images"
                For i = 0 To fileN
                    If customDynamicColourStartPath$ = path$ And (customDynamicColourStartFrame = i Or customDynamicColourStartFrame = i + 1) Then
                        If customdynamicRed Then dynamicRed = customdynamicRed
                        If customdynamicGreen Then dynamicGreen = customdynamicGreen
                        If customdynamicBlue Then dynamicBlue = customdynamicBlue
                        If customdynamicAlpha Then dynamicAlpha = customdynamicAlpha
                    End If
                    If _FileExists(_Trim$(fileList(i))) Then
                        a = _LoadImage(_Trim$(fileList(i)), 32)
                        widthImage = _Width(a)
                        heightImage = _Height(a)
                        If dynamicColor Then
                            _Source a
                            _Dest a
                            For ax = 1 To widthImage
                                For ay = 1 To heightImage
                                    tmpColor = Point(ax, ay)
                                    red32 = _Red32(tmpColor) / 255
                                    green32 = _Green32(tmpColor) / 255
                                    blue32 = _Blue32(tmpColor) / 255
                                    alpha32 = _Alpha32(tmpColor) / 255
                                    If red32 > 0 And green32 = 0 And blue32 = 0 And alpha32 = 0 Then PSet (ax, ay), dynamicRed
                                    If red32 = 0 And green32 > 0 And blue32 = 0 And alpha32 = 0 Then PSet (ax, ay), dynamicGreen
                                    If red32 = 0 And green32 = 0 And blue32 > 0 And alpha32 = 0 Then PSet (ax, ay), dynamicBlue
                                    If red32 = 0 And green32 = 0 And blue32 = 0 And alpha32 > 0 Then PSet (ax, ay), dynamicAlpha
                                    Select Case DYNAMIC_MODE
                                        Case 1, 1.5:
                                            If Sgn(red32) + Sgn(green32) + Sgn(blue32) + Sgn(alpha32) <= 2 Then
                                                tmp2Color = _RGBA32(0, 0, 0, 0)
                                                If red32 > 0 Then tmp2Color = AverageColor(dynamicRed, tmp2Color)
                                                If green32 > 0 Then tmp2Color = AverageColor(dynamicGreen, tmp2Color)
                                                If blue32 > 0 Then tmp2Color = AverageColor(dynamicBlue, tmp2Color)
                                                If alpha32 > 0 Then tmp2Color = AverageColor(dynamicAlpha, tmp2Color)
                                                If DYNAMIC_MODE = 1 Then tmp2Color = _RGBA32(Min(255, _Red32(tmp2Color) * 2), Min(255, _Green32(tmp2Color) * 2), Min(255, _Blue32(tmp2Color) * 2), Min(255, _Alpha32(tmp2Color) * 2))
                                                PSet (ax, ay), tmp2Color
                                            End If
                                        Case 2:
                                            If Sgn(red32) + Sgn(green32) + Sgn(blue32) + Sgn(alpha32) <= 2 Then
                                                If red32 > 0 Then PSet (ax, ay), dynamicRed
                                                If green32 > 0 Then PSet (ax, ay), dynamicGreen
                                                If blue32 > 0 Then PSet (ax, ay), dynamicBlue
                                                If alpha32 > 0 Then PSet (ax, ay), dynamicAlpha
                                            End If
                                    End Select
                            Next ay, ax
                            _Source 0
                            _Dest 0
                        End If
                        imageHandles(i) = a
                    End If
                Next i
                Write_log "        Images Loaded"
                imagesLoaded = -1
            End If
            For i = 0 To fileN
                _Limit FPS
                If imageHandles(i) >= -1 Then Write_log "    File does not exist, skipping display": GoTo SkipDisplay
                '------Trim---------------------------------------------------------
                If trimFile Then
                    Cls , rgb
                    _PutImage (OffsetX + ScaleFactor * trim(i).X, OffsetY + ScaleFactor * trim(i).Y)-(OffsetX + ScaleFactor * trim(i).X + ScaleFactor * trim(i).W, OffsetY + ScaleFactor * trim(i).Y + ScaleFactor * trim(i).H), imageHandles(i)
                Else
                    Cls , rgb
                    _PutImage (OffsetX + ScaleFactor * (W - widthImage) / 2, OffsetY + ScaleFactor * (H - heightImage) / 2)-(OffsetX + ScaleFactor * (W + widthImage) / 2, OffsetY + ScaleFactor * (H + heightImage) / 2), imageHandles(i)
                End If
                '-------------------------------------------------------------------
                If SAVE_MODE = 1 Then PNG "..\out\" + path$ + "\" + Left$(fileList(i), _InStrRev(fileList(i), ".")) + "png", 0
                If SAVE_MODE = 2 And trimFile = 0 Then Print #21, __T$(widthImage * ScaleFactor) + "x" + __T$(heightImage * ScaleFactor) + "+" + __T$(OffsetX + ScaleFactor * (W - widthImage) / 2) + "+" + __T$(OffsetY + ScaleFactor * (H - heightImage) / 2)
                If SAVE_MODE = 2 Then PNG "..\out\" + path$ + "\" + Left$(fileList(i), _InStrRev(fileList(i), ".")) + "png", imageHandles(i)
                _Display
                Do: _Limit 60: Loop While _KeyDown(80) Or _KeyDown(112)
                K$ = InKey$: If Len(K$) = 1 And UCase$(K$) <> "P" And count = 0 Then EndAnimation = -1: finalCount = loopN
                SkipDisplay:
                If Inp(&H60) = 1 Then Exit Do
            Next i
        Next loopN
        For i = 0 To fileN
            If imageHandles(i) < -1 Then _FreeImage imageHandles(i)
            imageHandles(i) = 0
        Next i
        Write_log "    Pausing for" + Str$(pause / FPS) + " seconds"
        For i = 0 To pause Step 1
            _Limit FPS
        Next i
        Write_log "    Going Back to Parent Directory"
        ChDir ".."
        skipDescLine:
        If SAVE_MODE = 2 And trimFile = 0 Then Close #21
    End If
Loop
System
corruptedfile:
Screen _NewImage(320, 240, 256)
Print "File is Corrupted": End
Function AverageColor& (C1&, C2&)
    AverageColor& = _RGBA32((_Red32(C1&) + _Red32(C2&)) / 2, (_Green32(C1&) + _Green32(C2&)) / 2, (_Blue32(C1&) + _Blue32(C2&)) / 2, (_Alpha32(C1&) + _Alpha32(C2&)) / 2)
End Function
Function Min (A, B)
    If A < B Then Min = A Else Min = B
End Function
Sub Write_log (A$)
    _Echo "[" + Time$ + ":" + A$ + "]"
    If LOG_FILE Then Print #4, "["; Time$; ":"; A$; "]"
End Sub
Sub PNG (__FILENAME As String, __IMAGE As Long)
    Dim As String __PNGHEADER, __IHDR, __IDAT, __IMAGDATA, __IEND
    Dim As String __EXT
    Dim As Long __WIDTH, __HEIGHT, __F, __SOURCE, __OFFSET, __X, __Y, __P
    Dim As _Byte __SAVEMODE
    If Len(__FILENAME) Then __SAVEMODE = -1 Else __SAVEMODE = 0
    __PNGHEADER$ = Chr$(&H89) + Chr$(&H50) + Chr$(&H4E) + Chr$(&H47) + Chr$(&H0D) + Chr$(&H0A) + Chr$(&H1A) + Chr$(&H0A)
    __WIDTH = _Width(__IMAGE): __HEIGHT = _Height(__IMAGE)
    __F = FreeFile
    If LCase$(Right$(__FILENAME, 4)) <> ".png" Then __EXT = ".png" Else __EXT = ""
    If __SAVEMODE Then Open __FILENAME + __EXT For Output As #__F: Close #__F
    If __SAVEMODE Then Open __FILENAME + __EXT For Binary As #__F
    If __SAVEMODE Then Put #__F, , __PNGHEADER$
    __IHDR = "IHDR" + Reverse$(MKL$(__WIDTH)) + Reverse$(MKL$(__HEIGHT)) + Chr$(&H08) + Chr$(&H06) + String$(3, 0)
    __IHDR = Reverse$(MKL$(&H0D)) + __IHDR + Reverse$(MKL$(crc32(__IHDR)))
    If __SAVEMODE Then Put #__F, , __IHDR
    __SOURCE = _Source
    _Source __IMAGE
    __IMAGDATA = String$(__HEIGHT * __WIDTH * 4 + __HEIGHT, 0)
    __OFFSET = 1
    For __Y = 1 To __HEIGHT Step 1
        __OFFSET = __OFFSET + 1
        For __X = 1 To __WIDTH Step 1
            __P = Point(__X - 1, __Y - 1)
            Asc(__IMAGDATA, __OFFSET) = _Red32(__P)
            Asc(__IMAGDATA, __OFFSET + 1) = _Green32(__P)
            Asc(__IMAGDATA, __OFFSET + 2) = _Blue32(__P)
            Asc(__IMAGDATA, __OFFSET + 3) = _Alpha32(__P)
            __OFFSET = __OFFSET + 4
        Next __X
    Next __Y
    _Source __SOURCE
    __IDAT = _Deflate$(__IMAGDATA)
    __IDAT = Reverse$(MKL$(Len(__IDAT))) + "IDAT" + __IDAT + Reverse$(MKL$(crc32("IDAT" + __IDAT)))
    If __SAVEMODE Then Put #__F, , __IDAT
    __IEND = Reverse$(MKL$(&H00)) + "IEND" + Reverse$(MKL$(&HAE426082))
    If __SAVEMODE Then Put #__F, , __IEND
    If __SAVEMODE Then Close #__F
End Sub
Function Reverse$ (__IN$)
    IN$ = __IN$
    L~& = Len(IN$)
    For I~& = 1 To _SHR(L~&, 1)
        TMP~%% = Asc(IN$, I~&)
        Asc(IN$, I~&) = Asc(IN$, L~& - I~& + 1)
        Asc(IN$, L~& - I~& + 1) = TMP~%%
    Next I~&
    Reverse$ = IN$
End Function
Function crc32~& (__IN$)
    Dim As _Unsigned Long __CRC32_POLY, __CRC
    __CRC32_POLY = &HEDB88320
    __CRC = &HFFFFFFFF
    For __I = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(Mid$(__IN$, __I, 1))
        For __J = 1 To 8
            If __CRC And 1 Then
                __CRC = (__CRC \ 2) Xor __CRC32_POLY
            Else
                __CRC = __CRC \ 2
            End If
        Next __J
    Next __I
    crc32~& = Not __CRC
End Function
Function __T$ (__A)
    __T$ = _Trim$(Str$(__A))
End Function
