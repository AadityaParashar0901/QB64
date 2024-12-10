'$Dynamic
$Resize:On
Do Until _ScreenExists: Loop
While _Resize: Wend

Dim Shared As Integer W, H, hW, hH, IW, IH, hIW, hIH
Dim Shared As Single Scale
NewScreen 640, 480
Dim Shared FilesList(0) As String
F$ = Command$(1)
IMG& = _LoadImage(F$, 32)
If IMG& >= -1 Then System
ChDir _StartDir$
GetFilesList
IW = _Width(IMG&)
IH = _Height(IMG&)
hIW = IW / 2
hIH = IH / 2
DISPLAY = -1
CalculateScale
Do
    _Limit 60
    If _Resize Then
        NewScreen _ResizeWidth, _ResizeHeight
        CalculateScale
        DISPLAY = -1
    End If
    LMW = 0: MW = 0
    While _MouseInput
        LMW = _MouseWheel
        If LMW Then MW = _MouseWheel
        DISPLAY = -1
    Wend
    If InKey$ <> "" Then DISPLAY = -1
    If DISPLAY = 0 Then _Continue
    Cls
    Scale = Scale * (1 - MW / 100)
    If Scale < 0.1 Then Scale = 0.1
    If Scale > 100 Then Scale = 100
    If _MouseButton(1) Then
        offsetX = offsetX + _MouseX - MOUSEX
        offsetY = offsetY + _MouseY - MOUSEY
    End If
    MOUSEX = _MouseX
    MOUSEY = _MouseY
    _PrintString (0, 0), _Trim$(Str$(Scale))
    _PutImage (offsetX + hW - Scale * hIW, offsetY + hH - Scale * hIH)-(offsetX + hW + Scale * hIW, offsetY + hH + Scale * hIH), IMG&
    _Display
    DISPLAY = 0
Loop
Sub GetFilesList
    ReDim FilesList(0) As String
    Shell _Hide "dir " + _CWD$ + " /b /o:n > tmp_dir.txt"
    Open "tmp_dir.txt" For Input As #1
    Do
        Line Input #1, L$
        If L$ = "" And EOF(1) Then Exit Do
        If InStr(".png.jpg.jpeg.ico.bmp", Right$(L$, _InStrRev(L$, "."))) Then
            If UBound(FilesList) = 0 Then ReDim FilesList(1 To 1) As String Else ReDim _Preserve FilesList(1 To UBound(FilesList) + 1) As String
            FilesList(UBound(FilesList)) = L$
        End If
        If EOF(1) Then Exit Do
    Loop
    Close #1
    Kill "tmp_dir.txt"
End Sub
Sub NewScreen (__W, __H)
    Screen _NewImage(__W, __H, 32)
    _PrintMode _KeepBackground
    W = __W
    H = __H
    hW = W / 2
    hH = H / 2
End Sub
Sub CalculateScale
    ScaleX = W / IW
    ScaleY = H / IH
    If ScaleX > ScaleY Then Scale = ScaleY Else Scale = ScaleX
End Sub
