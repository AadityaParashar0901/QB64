$Resize:On
'$Dynamic

Screen _NewImage(640, 480, 32)
_PrintMode _KeepBackground
_Title "GL Test"

Dim Shared Texture As Long, allowGL As _Bit
TMPTexture = _NewImage(20, 16, 32)
TMPImage = _LoadImage("cape.png", 32)
_Source TMPImage
_Dest TMPTexture
For X = 0 To 9
    For Y = 0 To 15
        PSet (X, Y), Point(X + 1, Y + 1)
Next Y, X
For X = 0 To 9
    For Y = 0 To 15
        PSet (X + 10, Y), Point(X + 12, Y + 1)
Next Y, X
_Source 0
_Dest 0
_FreeImage TMPImage

Texture = _CopyImage(TMPTexture, 32)
_FreeImage TMPTexture
While Texture: Wend

Dim Shared As _Unsigned Integer LFPS, GFPS, LFPSCount, GFPSCount: LFPS = 60

F = _FreeTimer
On Timer(F, 1) GoSub FPSCounter
Timer(F) On

Cls
_GLRender _Behind
allowGL = -1
Do
    _Limit 60
    LFPSCount = LFPSCount + 1
    If _Resize Then
        Screen _NewImage(_ResizeWidth, _ResizeHeight, 32)
        _PrintMode _KeepBackground
    End If
    Cls 2, 0
    Print "FPS (G/L):"; GFPS; "/"; LFPS
    _Display
Loop Until Inp(&H60) = 1
System

FPSCounter:
If LFPSCount Then LFPS = LFPSCount
LFPSCount = 0
GFPS = GFPSCount
GFPSCount = 0
Return

Sub _GL
    Static As Long TextureHandle
    Static As Integer T
    If Texture < -1 Then GL_Generate_Texture TextureHandle, Texture
    If allowGL = 0 Then Exit Sub

    _glViewport 0, 0, _Width - 1, _Height - 1
    _glEnable _GL_DEPTH_TEST
    _glClearColor 0, 0.5, 1, 0
    _glClear _GL_DEPTH_BUFFER_BIT Or _GL_COLOR_BUFFER_BIT
    _glTranslatef 0, 0, -25
    T = (T Mod 360) + 1
    _glRotatef T, 0, 1, 0
    _glMatrixMode _GL_PROJECTION
    _glLoadIdentity
    _gluPerspective 70, _Width / _Height, 0.1, 256
    _glMatrixMode _GL_MODELVIEW
    _glEnable _GL_TEXTURE_2D
    _glBindTexture _GL_TEXTURE_2D, TextureHandle
    _glBegin _GL_QUADS
    _glVertex3f -5, 8, 0.5
    _glTexCoord2f 0.5, 1
    _glVertex3f -5, -8, 0.5
    _glTexCoord2f 0, 1
    _glVertex3f 5, -8, 0.5
    _glTexCoord2f 0, 0
    _glVertex3f 5, 8, 0.5
    _glTexCoord2f 0.5, 0
    _glEnd
    _glBegin _GL_QUADS
    _glVertex3f -5, 8, -0.5
    _glTexCoord2f 1, 1
    _glVertex3f -5, -8, -0.5
    _glTexCoord2f 0.5, 1
    _glVertex3f 5, -8, -0.5
    _glTexCoord2f 0.5, 0
    _glVertex3f 5, 8, -0.5
    _glTexCoord2f 1, 0
    _glEnd
    _glDisable _GL_TEXTURE_2D
    _glDisable _GL_DEPTH_TEST
    _glFlush
    GFPSCount = GFPSCount + 1
End Sub
Sub GL_Generate_Texture (Handle As Long, Image As Long)
    Dim M As _MEM
    _glGenTextures 1, _Offset(Handle)
    _glBindTexture _GL_TEXTURE_2D, Handle
    M = _MemImage(Image)
    _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGBA, _Width(Image), _Height(Image), 0, _GL_BGRA_EXT, _GL_UNSIGNED_BYTE, M.OFFSET
    _MemFree M
    _FreeImage Image
    Image = 0
    _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
    _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_NEAREST
End Sub
