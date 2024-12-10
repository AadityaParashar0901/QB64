'$Dynamic
$Resize:On

Type Vec2_Float
    As Single X, Y
End Type
Type Vec2_Int
    As Integer X, Y
End Type
Type Vec3_Float
    As Single X, Y, Z
End Type
Type Vec3_Int
    As Integer X, Y, Z
End Type
Type BlockPropertiesType
    As String * 16 Name
    As _Unsigned _Byte Model, isTransparent, isFluid
End Type
Type ChunkType
    As Integer X, Z
    As _Unsigned Long Count, TCount, ShowCount, ShowTCount
    As Integer MinimumHeight, MaximumHeight
    As _Byte LoadedChunkData, LoadedRenderData, ShowRenderData
End Type

Screen _NewImage(960, 540, 32)
_PrintMode _KeepBackground

Do Until _ScreenExists: Loop
Do While _Resize: Loop

_Title "QB Blocks 3"

Randomize Timer
Const True = -1, False = 0
Const DEBUG = False: If DEBUG Then _Title "QB Blocks 3 - Debug"
Const Noise_3D = False

Dim Shared As _Bit FLYMODE, ZOOM, ShowDebugInfo, isPaused: FLYMODE = 0: ShowDebugInfo = DEBUG
Dim Shared As _Byte FOV, FOG, RenderDistance

Dim Shared GUI_ASSETS&(1 To 21), GUI_ASSETS_ID: GUI_ASSETS_ID = 1
Dim Shared WORLDFOLDER$, WorldFlat As _Unsigned _Byte

Const ChunkHeight = 256
Const GenerationChunkHeight = 64
Const WaterLevel = GenerationChunkHeight * 2 / 5
Const Chunk_Cave_Generate_Threshold = 0.3 'Debug
Const NoiseSmoothness = 64

Const PlayerHeight = 1.75
Const PlayerObesity = 0.5

FOV = 90

'---------------------
Const FONTWIDTH = 16, FONTHEIGHT = 16
Const ChunkSectionSize = 192 * ChunkHeight
Const ChunkTSectionSize = 256 * ChunkHeight
'---------------------
Dim Shared TotalChunks As _Unsigned Integer
Const MAXCHUNKS = 1089 '16 Chunks

Dim Shared TMPCHUNKDATA(0 To 17, 0 To ChunkHeight + 1, 0 To 17) As _Unsigned _Byte
Dim Shared Chunk(1 To MAXCHUNKS) As ChunkType
Dim Shared ChunkData(1 To MAXCHUNKS, 0 To 17, -1 To ChunkHeight + 1, 0 To 17) As _Unsigned _Byte
Dim Shared As Integer ChunkLoadTime
Dim Shared As _Unsigned Long LoadedChunks
Dim Shared CubeVertices(23) As Vec3_Int, CubeTexCoords(23) As Vec2_Float
For I = 0 To 23
    Read CubeVertices(I).X, CubeVertices(I).Y, CubeVertices(I).Z, CubeTexCoords(I).X, CubeTexCoords(I).Y
Next I
'--------------------------------
'+X
Data 1,1,1,0,0: Data 1,1,0,1,0: Data 1,0,0,1,1: Data 1,0,1,0,1
'-X
Data 0,0,0,0,1: Data 0,1,0,0,0: Data 0,1,1,1,0: Data 0,0,1,1,1
'+Y
Data 0,1,0,0,0: Data 1,1,0,1,0: Data 1,1,1,1,1: Data 0,1,1,0,1
'-Y
Data 0,0,0,0,0: Data 1,0,0,1,0: Data 1,0,1,1,1: Data 0,0,1,0,1
'+Z
Data 0,0,1,0,1: Data 1,0,1,1,1: Data 1,1,1,1,0: Data 0,1,1,0,0
'-Z
Data 0,0,0,0,1: Data 1,0,0,1,1: Data 1,1,0,1,0: Data 0,1,0,0,0
'--------------------------------
MAXVERTICES = MAXCHUNKS * ChunkSectionSize
MAXTVERTICES = MAXCHUNKS * ChunkTSectionSize
Dim Shared Vertices(1 To MAXVERTICES) As Vec3_Int, TexCoords(1 To MAXVERTICES) As Vec2_Float
Dim Shared TVertices(1 To MAXTVERTICES) As Vec3_Float, TTexCoords(1 To MAXTVERTICES) As Vec2_Float

Dim Shared Camera As Vec3_Float, CameraAngle As Vec2_Float, PlayerVelocity As Vec3_Float, BlockOnCamera As _Unsigned _Byte
Dim Shared RayPos As Vec3_Float, RayDir As Vec3_Float, RayBlockPos As Vec3_Int, RayPreviousBlockPos As Vec3_Int, BlockSelected As _Unsigned _Byte
Dim Shared As Vec2_Float CameraAngleSine, CameraAngleCoSine

CameraAngleSine.X = Sin(_D2R(CameraAngle.X)): CameraAngleSine.Y = Sin(_D2R(CameraAngle.Y))
CameraAngleCoSine.X = Cos(_D2R(CameraAngle.X)): CameraAngleCoSine.Y = Cos(_D2R(CameraAngle.Y))
'Game Mechanics
Const Gravity = -20
Dim Shared Time As Long, SkyColor As Long, SkyLight As _Byte, oldSkyLight As _Byte: Time = 7200 'Sunrise
Dim As Long PSkyColor, NSkyColor
Dim Shared SELECTED_BLOCK As Long: SELECTED_BLOCK = 1
'--------------
'--------------------------------
Print "Generating Textures"
Const TOTALTEXTURES = 29
Const BLOCK_AIR = 0
Const BLOCK_GRASS = 1
Const BLOCK_DIRT = 2
Const BLOCK_STONE = 3
Const BLOCK_WATER = 4
Const BLOCK_SAND = 5
Const BLOCK_SANDSTONE = 6
Const BLOCK_SNOW = 7
Const BLOCK_OAK_LOG = 8
Const BLOCK_OAK_LEAVES = 9
Const BLOCK_OAK_PLANKS = 10
Const BLOCK_CRAFTING_TABLE = 11
Const BLOCK_FURNACE_OFF = 12
Const BLOCK_FURNACE_ON = 13
Const BLOCK_BLACK_WOOL = 14
Const BLOCK_BLUE_WOOL = 15
Const BLOCK_BROWN_WOOL = 16
Const BLOCK_CYAN_WOOL = 17
Const BLOCK_GRAY_WOOL = 18
Const BLOCK_GREEN_WOOL = 19
Const BLOCK_LIGHT_BLUE_WOOL = 20
Const BLOCK_LIGHT_GRAY_WOOL = 21
Const BLOCK_LIME_WOOL = 22
Const BLOCK_MAGENTA_WOOL = 23
Const BLOCK_ORANGE_WOOL = 24
Const BLOCK_PINK_WOOL = 25
Const BLOCK_PURPLE_WOOL = 26
Const BLOCK_RED_WOOL = 27
Const BLOCK_WHITE_WOOL = 28
Const BLOCK_YELLOW_WOOL = 29

Const TEXTURESIZE = 16
Const IMAGEHEIGHT = 6 * TOTALTEXTURES
Dim Shared FILESEP$
$If WIN Then
    FILESEP$ = "\"
$Else
        FILESEP$ = "/"
$End If

Dim Shared As Long Texture, SunTexture, MoonTexture, CloudTexture, Cross, FontImage
Texture = _NewImage(TEXTURESIZE * 20, TEXTURESIZE * IMAGEHEIGHT, 32)
FontImage = LoadImage("assets" + FILESEP$ + "font" + FILESEP$ + "ascii.png")
GUI_ASSETS&(GUI_ASSETS_ID) = _LoadImage("assets" + FILESEP$ + "gui" + FILESEP$ + "background_loading.png", 32)
_PutImage (0, 0)-(_Width - 1, _Height - 1), GUI_ASSETS&(1)
_Display
Open "assets" + FILESEP$ + "gui_assets.list" For Input As #1
Do
    Line Input #1, IMGFILE$
    If IMGFILE$ = "end" Then Exit Do
    GUI_ASSETS_ID = GUI_ASSETS_ID + 1
    GUI_ASSETS&(GUI_ASSETS_ID) = LoadImage("assets" + FILESEP$ + _Trim$(IMGFILE$))
Loop
Close #1
Open "assets" + FILESEP$ + "assets.list" For Input As #1
I = 0
Do
    Line Input #1, I$
    If Left$(I$, 2) = "//" Then _Continue
    If Left$(I$, 2) = "/*" Then MULTILINECOMMENT = True: _Continue
    If Left$(I$, 2) = "*/" Then MULTILINECOMMENT = False: _Continue
    If MULTILINECOMMENT Then _Continue
    If _Trim$(I$) = "" Then _Continue
    I = I + 1
    If _Trim$(I$) = "/o" Then I$ = L$ Else L$ = I$
    If _Trim$(I$) = "/n" Then
    Else
        IMG& = LoadImage("assets" + FILESEP$ + "blocks" + FILESEP$ + I$)
        For J = 1 To 20
            _PutImage ((J - 1) * TEXTURESIZE, (I - 1) * TEXTURESIZE)-(J * TEXTURESIZE - 1, I * TEXTURESIZE - 1), IMG&, Texture ', (0, 0)-(TEXTURESIZE - 1, TEXTURESIZE - 1)
        Next J
        _FreeImage IMG&
    End If
    If I >= TOTALTEXTURES * 6 Then Exit Do
Loop
Close #1
Cross = _LoadImage("assets" + FILESEP$ + "gui" + FILESEP$ + "cross.png", 32)
SunTexture = _LoadImage("assets" + FILESEP$ + "environment" + FILESEP$ + "sun.png", 32)
MoonTexture = _LoadImage("assets" + FILESEP$ + "environment" + FILESEP$ + "moon.png", 32)
CloudTexture = _LoadImage("assets" + FILESEP$ + "environment" + FILESEP$ + "clouds.png", 32)
_Source Texture: _Dest Texture
For I = 1 To 20
    For X = I * TEXTURESIZE To _Width
        For Y = 0 To _Height - 1
            If _Alpha32(Point(X, Y)) = 255 Then PSet (X, Y), _RGBA32(0, 0, 0, 17)
    Next Y, X
Next I
_Source 0: _Dest 0
__GL_Generate_Texture = -1: While __GL_Generate_Texture = -1: Wend

HOMEDIR$ = _CWD$
START:
'Start GUI
ChDir HOMEDIR$
If _DirExists("saves") = 0 Then MkDir "saves"
Settings True
If FOV = 0 Then FOV = 90
If RenderDistance = 0 Then RenderDistance = 8
Settings False
WORLDFOLDER$ = ""
WorldSeed$ = ""
WorldFlat = 0
PAGE = 0
Do
    Cls
    If _Resize Then Screen _NewImage(_ResizeWidth, _ResizeHeight, 32): _PrintMode _KeepBackground
    _PutImage (0, 0)-(_Width - 1, _Height - 1), GUI_ASSETS&(2)
    While _MouseInput: Wend
    Select Case PAGE
        Case 0: If Button(1, _Width / 2, _Height * 0.4, "Play") Then PAGE = 1
            If Button(1, _Width / 2, _Height / 2, "Settings") Then Settings_Dialog
            If Button(1, _Width / 2, _Height * 0.6, "Quit") Then System
        Case 1:
            If _MouseButton(1) Then
                If MouseInBox(_Width / 2 - 100, _Height * 0.4 - 20, _Width / 2 + 99, _Height * 0.4 + 19) Then InputBoxFocus = 1
                If MouseInBox(_Width / 2 - 100, _Height / 2 - 20, _Width / 2 + 99, _Height / 2 + 19) Then InputBoxFocus = 2
            End If
            KeyHit = _KeyHit
            InputBox _Width / 2, _Height * 0.4, WORLDFOLDER$, "World Name", InputBoxFocus = 1, KeyHit
            InputBox _Width / 2, _Height / 2, WorldSeed$, "World Seed", InputBoxFocus = 2, KeyHit
            CheckBox _Width * 0.4, _Height * 0.6, "Super Flat", WorldFlat
            If Button(2, _Width / 10, _Height / 10, "") Then PAGE = 0
            If Button(1 - 8 * (Len(WORLDFOLDER$) = 0), _Width / 2, _Height * 0.7, "Create") Then Exit Do
    End Select
    _Display
Loop

Dim Shared SeedRatio As Single, Seed As _Unsigned Integer

If _DirExists("saves" + FILESEP$ + WORLDFOLDER$) = 0 Then MkDir "saves" + FILESEP$ + WORLDFOLDER$
If _DirExists("saves" + FILESEP$ + WORLDFOLDER$ + FILESEP$ + "chunks") = 0 Then MkDir "saves" + FILESEP$ + WORLDFOLDER$ + FILESEP$ + "chunks"
If Len(WorldSeed$) Then SeedRatio = Val(WorldSeed$) / 65536 Else SeedRatio = Rnd
LoadPlayerData
SavePlayerData
Seed = SeedRatio * 65536
Randomize Seed
Dim Shared perm(0 To 65535) As Double
For I = 0 To 65535
    perm(I) = Rnd
Next I

_PutImage (0, 0)-(_Width - 1, _Height - 1), GUI_ASSETS&(2)
PrintString (_Width - FONTWIDTH * Len(T$)) / 2, (_Height - FONTHEIGHT) / 2, "Loading World"
ReDim Shared Chunk(1 To MAXCHUNKS) As ChunkType: LoadedChunks = 0
ChunkX = Int(Camera.X / 16): ChunkZ = Int(Camera.Z / 16)
T$ = "Generating Chunks": TotalChunks = (2 * RenderDistance + 1) ^ 2
For R = 0 To 4: For X = ChunkX - R To ChunkX + R: For Z = ChunkZ - R To ChunkZ + R
            If X > ChunkX - R And X < ChunkX + R And Z > ChunkZ - R And Z < ChunkZ + R Then _Continue
            T = LoadChunkFile(X, Z)
            If _Resize Then Screen _NewImage(_ResizeWidth, _ResizeHeight, 32): _PrintMode _KeepBackground
            _PutImage (0, 0)-(_Width - 1, _Height - 1), GUI_ASSETS&(2)
            PrintString (_Width - FONTWIDTH * Len(T$)) / 2, (_Height - FONTHEIGHT) / 2, T$
            Line (_Width / 2 - 128, (_Height + FONTHEIGHT) / 2)-(_Width / 2 + 128, (_Height + FONTHEIGHT) / 2 + 4), _RGB32(0, 32), BF
            Line (_Width / 2 - 128, (_Height + FONTHEIGHT) / 2)-(_Width / 2 - 128 + 256 * LoadedChunks / 81, (_Height + FONTHEIGHT) / 2 + 4), _RGB32(0, 127, 0), BF
            _Display
Next Z, X: Next R
'--------------------------------

Dim Shared As _Unsigned Integer LFPS, GFPS, LFPSCount, GFPSCount: LFPS = 60
FPSCounterTimer = _FreeTimer: On Timer(FPSCounterTimer, 1) GoSub FPSCounter: Timer(FPSCounterTimer) On
GameTickTimer = _FreeTimer: On Timer(GameTickTimer, 1 / 20) GoSub GameTick: Timer(GameTickTimer) On

LoadChunkX = 0
LoadChunkZ = 0
LoadChunkDirection = 1
LoadChunkLength = 1

_Title "QB Blocks 3 - " + WORLDFOLDER$

Cls: _MouseHide: _GLRender _Behind
Do
    _Limit 60
    LFPSCount = LFPSCount + 1
    If _WindowHasFocus = 0 Or isPaused = -1 Then
        allowGL = 0
        isPaused = -1
        _MouseShow
        Timer(GameTickTimer) Off
    Else
        allowGL = -1
        _MouseHide
        Timer(GameTickTimer) On
    End If
    If _Resize Then
        Screen _NewImage(_ResizeWidth, _ResizeHeight, 32): _PrintMode _KeepBackground
        aspectRatio = _Width / _Height
    End If

    If isPaused Then
        While _MouseInput: Wend
        If _KeyDown(27) Then
            While _KeyDown(27): Wend
            isPaused = 0
        End If
        Cls , SkyColor
        If Button(1, _Width / 2, _Height * 0.4, "Back to Game") Then isPaused = 0
        If Button(1, _Width / 2, _Height / 2, "Settings") Then Settings_Dialog
        If Button(1, _Width / 2, _Height * 0.6, "Save & Exit") Then
            Timer(FPSCounterTimer) Free
            Timer(GameTickTimer) Free
            SavePlayerData
            GoTo START
        End If
        SavePlayerData
        LoadChunkX = 0
        LoadChunkZ = 0
        LoadChunkDirection = 1
        LoadChunkLength = 1
        InitialLoadChunkX = 0
        InitialLoadChunkZ = 0
        _Display
    Else
        If 1 Then
            ChunkX = Int(Camera.X / 16): ChunkZ = Int(Camera.Z / 16)
            For I = LBound(Chunk) To UBound(Chunk)
                If Chunk(I).LoadedChunkData = 0 Then _Continue
                If InRange(-RenderDistance, Chunk(I).X - ChunkX, RenderDistance) = 0 Or InRange(-RenderDistance, Chunk(I).Z - ChunkZ, RenderDistance) = 0 Then
                    Chunk(I).X = 0
                    Chunk(I).Z = 0
                    Chunk(I).Count = 0
                    Chunk(I).TCount = 0
                    Chunk(I).LoadedChunkData = 0
                    Chunk(I).LoadedRenderData = 0
                    LoadedChunks = LoadedChunks - 1
                    Exit For
                End If
            Next I
            For I = 0 To 63
                ChunkLoadingStartTime = Timer(0.001)
                CL = 0
                EXITFOR = 0
                If InRange(-RenderDistance, LoadChunkX, RenderDistance) And InRange(-RenderDistance, LoadChunkZ, RenderDistance) Then CL = LoadChunkFile(ChunkX + LoadChunkX, ChunkZ + LoadChunkZ)
                If CL Then EXITFOR = -1
                If CL Then ChunkLoadTime = Int(1000 * (Timer(0.001) - ChunkLoadingStartTime)) Else ChunkLoadTime = 0
                E = 0
                Select Case LoadChunkDirection
                    Case 1: LoadChunkZ = LoadChunkZ + 1
                        If LoadChunkZ - InitialLoadChunkZ = LoadChunkLength + 1 Then E = -1
                    Case 2: LoadChunkX = LoadChunkX + 1
                        If LoadChunkX - InitialLoadChunkX = LoadChunkLength + 1 Then E = -1
                    Case 3: LoadChunkZ = LoadChunkZ - 1
                        If InitialLoadChunkZ - LoadChunkZ = LoadChunkLength + 1 Then E = -1
                    Case 4: LoadChunkX = LoadChunkX - 1
                        If InitialLoadChunkX - LoadChunkX = LoadChunkLength + 1 Then E = -1
                End Select
                If E Then
                    LoadChunkDirection = LoadChunkDirection + 1
                    If LoadChunkDirection = 5 Then LoadChunkDirection = 1
                    If LoadChunkDirection = 1 Or LoadChunkDirection = 3 Then LoadChunkLength = LoadChunkLength + 1
                    If LoadChunkLength = 2 * RenderDistance + 1 Then LoadChunkLength = 0: LoadChunkX = 0: LoadChunkZ = 0
                    InitialLoadChunkX = LoadChunkX
                    InitialLoadChunkZ = LoadChunkZ
                End If
                If EXITFOR Then Exit For
            Next I
        End If

        SELECTED_BLOCK = Max(1, Min(TOTALTEXTURES, SELECTED_BLOCK + MW + _KeyDown(18432) - _KeyDown(20480) + _KeyDown(19200) - _KeyDown(19712)))
        While _KeyDown(18432) Or _KeyDown(20480) Or _KeyDown(19200) Or _KeyDown(19712): _Limit 30: Wend

        _KeyClear
        If InRange(1, Camera.Y, ChunkHeight) = 0 Then FLYMODE = -1
        If _KeyDown(87) Or _KeyDown(119) Then 'W
            If FLYMODE Then
                Camera.X = Camera.X + CameraAngleSine.X * Movespeed / LFPS
                Camera.Z = Camera.Z - CameraAngleCoSine.X * Movespeed / LFPS
            Else
                If isTransparent(BlockExists(Camera.X + Sgn(CameraAngleSine.X) * PlayerObesity, Camera.Y, Camera.Z)) And isTransparent(BlockExists(Camera.X + Sgn(CameraAngleSine.X) * PlayerObesity, Camera.Y - 1, Camera.Z)) Then Camera.X = Camera.X + CameraAngleSine.X * Movespeed / LFPS
                If isTransparent(BlockExists(Camera.X, Camera.Y, Camera.Z - Sgn(CameraAngleCoSine.X) * PlayerObesity)) And isTransparent(BlockExists(Camera.X, Camera.Y - 1, Camera.Z - Sgn(CameraAngleCoSine.X) * PlayerObesity)) Then Camera.Z = Camera.Z - CameraAngleCoSine.X * Movespeed / LFPS
            End If
        End If
        If _KeyDown(83) Or _KeyDown(115) Then 'S
            If FLYMODE Then
                Camera.X = Camera.X - CameraAngleSine.X * Movespeed / LFPS
                Camera.Z = Camera.Z + CameraAngleCoSine.X * Movespeed / LFPS
            Else
                If isTransparent(BlockExists(Camera.X - Sgn(CameraAngleSine.X) * PlayerObesity, Camera.Y, Camera.Z)) And isTransparent(BlockExists(Camera.X - Sgn(CameraAngleSine.X) * PlayerObesity, Camera.Y - 1, Camera.Z)) Then Camera.X = Camera.X - CameraAngleSine.X * Movespeed / LFPS
                If isTransparent(BlockExists(Camera.X, Camera.Y, Camera.Z + Sgn(CameraAngleCoSine.X) * PlayerObesity)) And isTransparent(BlockExists(Camera.X, Camera.Y - 1, Camera.Z + Sgn(CameraAngleCoSine.X) * PlayerObesity)) Then Camera.Z = Camera.Z + CameraAngleCoSine.X * Movespeed / LFPS
            End If
        End If
        If _KeyDown(65) Or _KeyDown(97) Then 'A
            If FLYMODE Then
                Camera.X = Camera.X - CameraAngleCoSine.X * Movespeed / LFPS
                Camera.Z = Camera.Z - CameraAngleSine.X * Movespeed / LFPS
            Else
                If isTransparent(BlockExists(Camera.X - Sgn(CameraAngleCoSine.X) * PlayerObesity, Camera.Y, Camera.Z)) And isTransparent(BlockExists(Camera.X - Sgn(CameraAngleCoSine.X) * PlayerObesity, Camera.Y - 1, Camera.Z)) Then Camera.X = Camera.X - CameraAngleCoSine.X * Movespeed / LFPS
                If isTransparent(BlockExists(Camera.X, Camera.Y, Camera.Z - Sgn(CameraAngleSine.X) * PlayerObesity)) And isTransparent(BlockExists(Camera.X, Camera.Y - 1, Camera.Z - Sgn(CameraAngleSine.X) * PlayerObesity)) Then Camera.Z = Camera.Z - CameraAngleSine.X * Movespeed / LFPS
            End If
        End If
        If _KeyDown(68) Or _KeyDown(100) Then 'D
            If FLYMODE Then
                Camera.X = Camera.X + CameraAngleCoSine.X * Movespeed / LFPS
                Camera.Z = Camera.Z + CameraAngleSine.X * Movespeed / LFPS
            Else
                If isTransparent(BlockExists(Camera.X + Sgn(CameraAngleCoSine.X) * PlayerObesity, Camera.Y, Camera.Z)) And isTransparent(BlockExists(Camera.X + Sgn(CameraAngleCoSine.X) * PlayerObesity, Camera.Y - 1, Camera.Z)) Then Camera.X = Camera.X + CameraAngleCoSine.X * Movespeed / LFPS
                If isTransparent(BlockExists(Camera.X, Camera.Y, Camera.Z + Sgn(CameraAngleSine.X) * PlayerObesity)) And isTransparent(BlockExists(Camera.X, Camera.Y - 1, Camera.Z + Sgn(CameraAngleSine.X) * PlayerObesity)) Then Camera.Z = Camera.Z + CameraAngleSine.X * Movespeed / LFPS
            End If
        End If
        If _KeyDown(67) Or _KeyDown(99) Then 'C
            ZOOM = -1
        Else
            ZOOM = 0
        End If
        If _KeyDown(27) Then 'Esc
            While _KeyDown(27): Wend
            isPaused = -1
        End If
        If _KeyDown(84) Or _KeyDown(116) Then Time = Time + 10
        If (_KeyDown(70) Or _KeyDown(102)) And InRange(1, Camera.Y, ChunkHeight) Then 'F
            While _KeyDown(70) Or _KeyDown(102): _Limit 30: Wend
            FLYMODE = Not FLYMODE
        End If
        If _KeyDown(82) Or _KeyDown(114) Then 'R
            For I = 1 To TotalChunks
                Chunk(I).X = 0
                Chunk(I).Z = 0
                Chunk(I).Count = 0
                Chunk(I).TCount = 0
                Chunk(I).LoadedChunkData = 0
                Chunk(I).LoadedRenderData = 0
                Chunk(I).ShowCount = 0
                Chunk(I).ShowTCount = 0
            Next I
            LoadedChunks = 0
        End If
        If _KeyDown(71) Or _KeyDown(103) Then 'G
            While _KeyDown(71) Or _KeyDown(103): _Limit 30: Wend
            FOG = Not FOG
        End If
        If _KeyDown(15616) Then 'F3
            While _KeyDown(15616): _Limit 30: Wend
            ShowDebugInfo = Not ShowDebugInfo
        End If
        If _KeyDown(100304) Then 'LShift
            If FLYMODE Then
                Camera.Y = Camera.Y - Movespeed / LFPS
            Else
                If BlockExists(Camera.X, Camera.Y - PlayerHeight, Camera.Z) = 0 Then Camera.Y = Camera.Y - Movespeed / LFPS
            End If
        End If
        If _KeyDown(32) Then 'Space
            If FLYMODE Then
                Camera.Y = Camera.Y + Movespeed / LFPS
            Else
                If BlockExists(Camera.X, Camera.Y - PlayerHeight, Camera.Z) <> 0 Then PlayerVelocity.Y = 6
            End If
        End If
        If _KeyDown(100306) Then 'LCtrl
            If FLYMODE Then
                Movespeed = 64
            Else
                If isBlockFluid(BlockExists(Camera.X, Camera.Y - 1, Camera.Z)) Then Movespeed = 3 Else Movespeed = 6
            End If
        Else
            If isBlockFluid(BlockExists(Camera.X, Camera.Y - 1, Camera.Z)) Then Movespeed = 2 Else Movespeed = 4
        End If
        If FLYMODE = 0 Then
            Camera.Y = Camera.Y + PlayerVelocity.Y / LFPS
            'If isNotBlock(BlockExists(Camera.X, Camera.Y, Camera.Z)) = 0 Then PlayerVelocity.Y = 0
            If isTransparent(BlockExists(Camera.X, Camera.Y - PlayerHeight, Camera.Z)) = 0 Then
                PlayerVelocity.Y = 0
            Else
                PlayerVelocity.Y = PlayerVelocity.Y + Gravity / LFPS
            End If
        End If
        BlockOnCamera = BlockExists(Camera.X, Camera.Y, Camera.Z)
    End If
Loop
System

FPSCounter:
If GFPSCount Then GFPS = GFPSCount
GFPSCount = 0
If LFPSCount Then LFPS = LFPSCount
LFPSCount = 0
Return

GameTick:
Time = Time + 1
If Time >= 28800 Then Time = 0
HOUR = Time \ 1200
MINUTEPER60 = ((Time \ 20) Mod 60) / 60
'SkyLight = Abs(HOUR - 12)
Select Case HOUR: Case 0: PSkyColor = _RGB32(0, 0, 0): Case 1 To 4: PSkyColor = _RGB32(0, 0, 51): Case 5: PSkyColor = _RGB32(255, 153, 102): Case 6: PSkyColor = _RGB32(255, 204, 153): Case 7, 8: PSkyColor = _RGB32(102, 204, 255): Case 9 To 14: PSkyColor = _RGB32(51, 153, 255): Case 15 To 17: PSkyColor = _RGB32(255, 153, 102): Case 18: PSkyColor = _RGB32(255, 102, 51): Case 19: PSkyColor = _RGB32(102, 51, 153): Case 20 To 23: PSkyColor = _RGB32(0, 0, 51): End Select
Select Case HOUR + 1: Case 0: NSkyColor = _RGB32(0, 0, 0): Case 1 To 4: NSkyColor = _RGB32(0, 0, 51): Case 5: NSkyColor = _RGB32(255, 153, 102): Case 6: NSkyColor = _RGB32(255, 204, 153): Case 7, 8: NSkyColor = _RGB32(102, 204, 255): Case 9 To 14: NSkyColor = _RGB32(51, 153, 255): Case 15 To 17: NSkyColor = _RGB32(255, 153, 102): Case 18: NSkyColor = _RGB32(255, 102, 51): Case 19: NSkyColor = _RGB32(102, 51, 153): Case 20 To 23: NSkyColor = _RGB32(0, 0, 51): End Select
SkyColor = _RGB32(interpolate#(_Red32(PSkyColor), _Red32(NSkyColor), MINUTEPER60), interpolate#(_Green32(PSkyColor), _Green32(NSkyColor), MINUTEPER60), interpolate#(_Blue32(PSkyColor), _Blue32(NSkyColor), MINUTEPER60))
If SkyLight <> oldSkyLight Then
    CRLM = SkyLight - oldSkyLight: oldSkyLight = SkyLight
    For I = 1 To TotalChunks: ChunkReloadLight I, CRLM: Next I
End If
Return

Sub _GL
    Dim As _Byte CX, CZ, CPX, CPY, CPZ
    Shared allowGL, __GL_Generate_Texture, __GL_Generate_Sun_Texture, __GL_Generate_Chunks
    Static As Long TextureHandle, SunTextureHandle, MoonTextureHandle, CloudTextureHandle
    Dim M As _MEM
    If __GL_Generate_Texture Then
        _glGenTextures 1, _Offset(TextureHandle)
        _glBindTexture _GL_TEXTURE_2D, TextureHandle
        M = _MemImage(Texture)
        _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGBA, _Width(Texture), _Height(Texture), 0, _GL_BGRA_EXT, _GL_UNSIGNED_BYTE, M.OFFSET
        _MemFree M
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_NEAREST

        _glGenTextures 1, _Offset(SunTextureHandle)
        _glBindTexture _GL_TEXTURE_2D, SunTextureHandle
        M = _MemImage(SunTexture)
        _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGBA, _Width(SunTexture), _Height(SunTexture), 0, _GL_BGRA_EXT, _GL_UNSIGNED_BYTE, M.OFFSET
        _MemFree M
        _FreeImage SunTexture
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_NEAREST

        _glGenTextures 1, _Offset(MoonTextureHandle)
        _glBindTexture _GL_TEXTURE_2D, MoonTextureHandle
        M = _MemImage(MoonTexture)
        _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGBA, _Width(MoonTexture), _Height(MoonTexture), 0, _GL_BGRA_EXT, _GL_UNSIGNED_BYTE, M.OFFSET
        _MemFree M
        _FreeImage MoonTexture
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_NEAREST

        _glGenTextures 1, _Offset(CloudTextureHandle)
        _glBindTexture _GL_TEXTURE_2D, CloudTextureHandle
        M = _MemImage(CloudTexture)
        _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGBA, _Width(CloudTexture), _Height(CloudTexture), 0, _GL_BGRA_EXT, _GL_UNSIGNED_BYTE, M.OFFSET
        _MemFree M
        _FreeImage CloudTexture
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_NEAREST

        __GL_Generate_Texture = 0
    End If

    If allowGL = 0 Then Exit Sub

    _glViewport 0, 0, _Width - 1, _Height - 1
    _glEnable _GL_BLEND
    _glDisable _GL_MULTISAMPLE
    _glEnable _GL_DEPTH_TEST

    _glClearColor _Red32(SkyColor) / 255, _Green32(SkyColor) / 255, _Blue32(SkyColor) / 255, 0
    _glClear _GL_DEPTH_BUFFER_BIT Or _GL_COLOR_BUFFER_BIT

    _glTranslatef 0, 0, -0.25
    _glRotatef -CameraAngle.Y, 1, 0, 0
    _glRotatef CameraAngle.X, 0, 1, 0
    _glTranslatef 0, -Camera.Y, 0

    _glMatrixMode _GL_PROJECTION
    _glLoadIdentity
    _gluPerspective FOV + ZOOM * (FOV - 30), _Width / _Height, 0.1, Max(256, ChunkHeight * 8)

    _glMatrixMode _GL_MODELVIEW

    _glEnable _GL_TEXTURE_2D

    'Draw Sun and Moon
    _glBindTexture _GL_TEXTURE_2D, SunTextureHandle
    X = 0: Y = ChunkHeight * 4.5: Z = 0: S = ChunkHeight * 8
    _glRotatef Time / 80 - 180, 1, 0, 0
    _glBegin _GL_QUADS: For I = 8 To 11
        _glVertex3f X + (CubeVertices(I).X - 0.5) * S, Y + (CubeVertices(I).Y - 0.5), Z + (CubeVertices(I).Z - 0.5) * S
        _glTexCoord2f CubeTexCoords(I).X, CubeTexCoords(I).Y
    Next I: _glEnd
    _glBindTexture _GL_TEXTURE_2D, MoonTextureHandle
    X = 0: Y = ChunkHeight * 4.5: Z = 0: S = ChunkHeight * 4
    _glRotatef 180, 1, 0, 0
    _glBegin _GL_QUADS: For I = 8 To 11
        _glVertex3f X + (CubeVertices(I).X - 0.5) * S, Y + (CubeVertices(I).Y - 0.5), Z + (CubeVertices(I).Z - 0.5) * S
        _glTexCoord2f CubeTexCoords(I).X, CubeTexCoords(I).Y
    Next I: _glEnd
    _glRotatef -Time / 80, 1, 0, 0
    '----------------------------

    _glTranslatef -Camera.X, 0, -Camera.Z
    _glBindTexture _GL_TEXTURE_2D, TextureHandle
    If FOG Then
        _glEnable _GL_FOG
        _glFogi _GL_FOG_MODE, _GL_LINEAR
        _glFogf _GL_FOG_END, 16 * (RenderDistance - 1)
        If BlockOnCamera = BLOCK_WATER Then
            _glFogf _GL_FOG_START, 8
            _glFogfv _GL_FOG_COLOR, glVec4(0.25, 0.12, 1, 0.5)
            _glFogf _GL_FOG_DENSITY, 0.1
        Else
            _glFogf _GL_FOG_START, 8 * RenderDistance
            _glFogfv _GL_FOG_COLOR, glVec4(0.75, 0.88, 1, 1)
            _glFogf _GL_FOG_DENSITY, 0.1
        End If
    End If
    _glEnableClientState _GL_VERTEX_ARRAY
    _glEnableClientState _GL_TEXTURE_COORD_ARRAY
    For I = LBound(Chunk) - 1 To TotalChunks - 1
        If Chunk(I + 1).ShowRenderData = 0 Or Chunk(I + 1).ShowCount = 0 Then _Continue
        _glVertexPointer 3, _GL_SHORT, 0, _Offset(Vertices(I * ChunkSectionSize + 1))
        _glTexCoordPointer 2, _GL_FLOAT, 0, _Offset(TexCoords(I * ChunkSectionSize + 1))
        _glDrawArrays _GL_QUADS, 0, Chunk(I + 1).ShowCount
    Next I
    For I = LBound(Chunk) - 1 To TotalChunks - 1
        If Chunk(I + 1).ShowRenderData = 0 Or Chunk(I + 1).ShowTCount = 0 Then _Continue
        _glVertexPointer 3, _GL_FLOAT, 0, _Offset(TVertices(I * ChunkSectionSize + 1))
        _glTexCoordPointer 2, _GL_FLOAT, 0, _Offset(TTexCoords(I * ChunkSectionSize + 1))
        _glDrawArrays _GL_QUADS, 0, Chunk(I + 1).ShowTCount
    Next I
    If BlockSelected Then DrawOutlineBox
    _glDisableClientState _GL_VERTEX_ARRAY
    _glDisableClientState _GL_TEXTURE_COORD_ARRAY
    'Draw Clouds
    _glTranslatef Camera.X, 0, Camera.Z
    _glBindTexture _GL_TEXTURE_2D, CloudTextureHandle
    X = 0: Y = ChunkHeight * 0.8: Z = Time / 80: S = ChunkHeight * 64
    _glBegin _GL_QUADS: For I = 8 To 11
        _glVertex3f X + (CubeVertices(I).X - 0.5) * S, Y + (CubeVertices(I).Y - 0.5), Z + (CubeVertices(I).Z - 0.5) * S
        _glTexCoord2f CubeTexCoords(I).X, CubeTexCoords(I).Y
    Next I: _glEnd
    '-----------
    If FOG Then _glDisable _GL_FOG
    _glDisable _GL_TEXTURE_2D
    _glDisable _GL_DEPTH_TEST
    _glDisable _GL_BLEND
    _glFlush
    Cls 2, 0
    Print "FPS(G/L):"; GFPS; "/"; LFPS;: Print "Seed:"; Seed;: Print "Time:"; GameTime$
    If ShowDebugInfo Then
        Print Using "PP: ###.## ###.## ###.##"; Camera.X; Camera.Y; Camera.Z
        Print Using "PA: ####.## ###.##"; CameraAngle.X; CameraAngle.Y
        Print "Chunks:"; LoadedChunks
        ChunkRelativeCameraPosition Camera, CX, CZ, CPX, CPY, CPZ
        Print Using "CR: ### ### ## ### ##"; CX; CZ, CPX; CPY; CPZ
        Print "SB:"; Int(RayBlockPos.X); Int(RayBlockPos.Y); Int(RayBlockPos.Z)
        If ChunkLoadTime Then Print "Chunk Load Time:"; ChunkLoadTime
    End If
    _PutImage (_Width / 2, _Height / 2), Cross&
    _PutImage (_Width / 2 - TEXTURESIZE, _Height - TEXTURESIZE * 2)-(_Width / 2 + TEXTURESIZE, _Height - 1), Texture, , (0, (SELECTED_BLOCK * 6 - 5) * TEXTURESIZE)-(TEXTURESIZE - 1, (SELECTED_BLOCK * 6 - 4) * TEXTURESIZE - 1)
    _Display
    GFPSCount = GFPSCount + 1
    'Mouse
    LMW = 0: MW = 0: While _MouseInput
        CameraAngle.X = CameraAngle.X + _MouseMovementX / 8
        CameraAngle.Y = CameraAngle.Y - _MouseMovementY / 8
        If CameraAngle.Y < -90 Then CameraAngle.Y = -90
        If CameraAngle.Y > 90 Then CameraAngle.Y = 90
        If CameraAngle.X > 180 Then CameraAngle.X = CameraAngle.X - 360
        If CameraAngle.X < -180 Then CameraAngle.X = CameraAngle.X + 360
        CameraAngleSine.X = Sin(_D2R(CameraAngle.X)): CameraAngleSine.Y = Sin(_D2R(CameraAngle.Y))
        CameraAngleCoSine.X = Cos(_D2R(CameraAngle.X)): CameraAngleCoSine.Y = Cos(_D2R(CameraAngle.Y))
        LMW = _MouseWheel
        If LMW Then MW = LMW
        _MouseMove _Width / 2, _Height / 2
    Wend
    RayPos = Camera: RayDir.X = CameraAngleSine.X * CameraAngleCoSine.Y: RayDir.Y = CameraAngleSine.Y: RayDir.Z = -CameraAngleCoSine.X * CameraAngleCoSine.Y: BlockSelected = 0
    Vec3_FloatToInt RayPos, RayBlockPos
    For I = 1 To 5
        If InRange(1, RayPos.Y, ChunkHeight) Then
            If isTransparent(BlockExists(RayPos.X, RayPos.Y, RayPos.Z)) = 0 Then BlockSelected = -1: Exit For
        End If
        Vec3_FloatToInt RayPos, RayPreviousBlockPos
        RayPos.X = RayPos.X + RayDir.X
        RayPos.Y = RayPos.Y + RayDir.Y
        RayPos.Z = RayPos.Z + RayDir.Z
    Next I
    Vec3_FloatToInt RayPos, RayBlockPos
    If BlockSelected And _MouseButton(1) And InRange(1, RayBlockPos.Y, ChunkHeight) And Timer(0.01) - LastMouse1Time > 0.25 Then SetBlockReloadChunk RayBlockPos.X, RayBlockPos.Y, RayBlockPos.Z, BLOCK_AIR: LastMouse1Time = Timer(0.01)
    If BlockSelected And _MouseButton(2) And InRange(1, RayPreviousBlockPos.Y, ChunkHeight) And Timer(0.01) - LastMouse2Time > 0.25 Then If (Abs(Camera.X - RayPreviousBlockPos.X) > PlayerObesity Or Abs(Camera.Z - RayPreviousBlockPos.Z) > PlayerObesity) Or Abs(Camera.Y - RayPreviousBlockPos.Y) > PlayerHeight Then SetBlockReloadChunk RayPreviousBlockPos.X, RayPreviousBlockPos.Y, RayPreviousBlockPos.Z, SELECTED_BLOCK: LastMouse2Time = Timer(0.01)
End Sub

Sub DrawOutlineBox: X = RayBlockPos.X: Y = RayBlockPos.Y: Z = RayBlockPos.Z: _glBegin _GL_LINES: For I = 0 To 23: _glVertex3f X + CubeVertices(I).X, Y + CubeVertices(I).Y, Z + CubeVertices(I).Z: Next I: _glEnd: End Sub

Function GameTime$
    Static oldGT$, oldTime As Long
    If oldTime <> Time \ 20 Then
        T& = Time \ 20
        H$ = _Trim$(Str$(T& \ 60))
        M$ = _Trim$(Str$(T& Mod 60))
        oldGT$ = String$(2 - Len(H$), 48) + _Trim$(Str$(T& \ 60)) + ":" + String$(2 - Len(M$), 48) + M$
        oldTime = T&
    End If
    GameTime$ = oldGT$
End Function

Function hash# (X As _Unsigned Integer)
    hash# = perm(X)
End Function
Function fade# (t As Double) Static
    fade# = ((6 * t - 15) * t + 10) * t ^ 3
End Function
Function interpolate# (A#, B#, C#) Static
    interpolate# = A# + (B# - A#) * C#
End Function
Function noise1# (X As Double) Static
    fX% = Int(X): dX# = fade#(X - fX%)
    noise1# = interpolate#(hash(fX%), hash(fX% + 1), dX#)
End Function
Function noise2# (X As Double, Y As Double) Static
    fX% = Int(X): dX# = fade#(X - fX%)
    fY% = Int(Y): dY# = fade#(Y - fY%)
    noise2# = interpolate#(interpolate#(hash(fX% + fY% * 57), hash(fX% + fY% * 57 + 1), dX#), interpolate#(hash(fX% + fY% * 57 + 57), hash(fX% + fY% * 57 + 58), dX#), dY#)
End Function
Function noise3# (X As Double, Y As Double, Z As Double) Static
    fX% = Int(X): dX# = fade#(X - fX%)
    fY% = Int(Y): dY# = fade#(Y - fY%)
    fZ% = Int(Z): dZ# = fade#(Z - fZ%)
    noise3# = interpolate#(interpolate#(interpolate#(hash(fX% + (fY% + fZ% * 57) * 57), hash(fX% + (fY% + fZ% * 57) * 57 + 1), dX#), interpolate#(hash(fX% + (fY% + fZ% * 57) * 57 + 57), hash(fX% + (fY% + fZ% * 57) * 57 + 58), dX#), dY#), interpolate#(interpolate#(hash(fX% + (fY% + fZ% * 57 + 57) * 57), hash(fX% + (fY% + fZ% * 57 + 57) * 57 + 1), dX#), interpolate#(hash(fX% + (fY% + fZ% * 57 + 57) * 57 + 57), hash(fX% + (fY% + fZ% * 57 + 57) * 57 + 58), dX#), dY#), dZ#)
End Function
Function fractal1# (X As Integer, O As _Unsigned _Byte, S As _Unsigned Long) Static
    Dim As Double a, t, d, I
    a = 1: t = 0: d = 0
    For I = 0 To O
        t = t + a * noise1#(X / a / S): d = d + a: a = a / 2
    Next I
    fractal1# = t / d
End Function
Function fractal2# (X As Integer, Y As Integer, O As _Unsigned _Byte, S As _Unsigned Long) Static
    Dim As Double a, t, d, I
    a = 1: t = 0: d = 0
    For I = 0 To O
        t = t + a * noise2#(X / a / S, Y / a / S): d = d + a: a = a / 2
    Next I
    fractal2# = t / d
End Function
Function fractal3# (X As Integer, Y As Integer, Z As Integer, O As _Unsigned _Byte, S As _Unsigned Long) Static
    Dim As Double a, t, d, I
    a = 1: t = 0: d = 0
    For I = 0 To O
        t = t + a * noise3#(X / a / S, Y / a / S, Z / a / S): d = d + a: a = a / 2
    Next I
    fractal3# = t / d
End Function

Function glVec4%& (X!, Y!, Z!, W!) Static
    If firstRun` = 0 Then
        Dim VEC4(3) As Single
        firstRun` = -1
    End If
    VEC4(0) = X!: VEC4(1) = Y!: VEC4(2) = Z!: VEC4(3) = W!
glVec4%& = _Offset(VEC4()): End Function

Function ChunkLoader (FoundI, CX As Long, CZ As Long)
    Dim Block As _Unsigned _Byte
    If FoundI = 0 Then Exit Function
    Chunk(FoundI).X = CX
    Chunk(FoundI).Z = CZ
    If Noise_3D Then
        Chunk(FoundI).MinimumHeight = 1
    Else
        Chunk(FoundI).MinimumHeight = GenerationChunkHeight
    End If
    Chunk(FoundI).MaximumHeight = 1
    For X = 0 To 17: For Z = 0 To 17: For Y = 1 To ChunkHeight: ChunkData(FoundI, X, Y, Z) = 0: Next Y, Z, X
    CX16 = CX * 16
    CZ16 = CZ * 16
    If WorldFlat = 0 Then
        For X = 0 To 17: For Z = 0 To 17
                Biome = Int(4 * fractal2(CX16 + X, CZ16 + Z, 7, NoiseSmoothness * 4))
                H = Int(GenerationChunkHeight * fractal2(CX16 + X, CZ16 + Z, 3, NoiseSmoothness))
                If Chunk(FoundI).MinimumHeight > H - 1 Then Chunk(FoundI).MinimumHeight = H - 1
                If Chunk(FoundI).MaximumHeight < H Then Chunk(FoundI).MaximumHeight = Max(H, WaterLevel)
                ChunkData(FoundI, X, 1, Z) = BLOCK_STONE
                For Y = 1 To Max(H, WaterLevel)
                    If Y < H Then Block = BLOCK_DIRT
                    If Y = H Then Block = BLOCK_GRASS
                    If Y > H Then Block = BLOCK_WATER
                    If Y <= H And Noise_3D Then
                        If fractal3(CX16 + X, Y, CZ16 + Z, 0, NoiseSmoothness) > Chunk_Cave_Generate_Threshold Then ChunkData(FoundI, X, Y, Z) = Block
                    Else
                        ChunkData(FoundI, X, Y, Z) = Block
                    End If
                Next Y
        Next Z, X
    Else
        Chunk(FoundI).MinimumHeight = 1
        Chunk(FoundI).MaximumHeight = GenerationChunkHeight / 2
        For X = 0 To 17: For Z = 0 To 17
                For Y = 0 To GenerationChunkHeight / 2 - 1
                    ChunkData(FoundI, X, Y, Z) = BLOCK_DIRT
                Next Y
                ChunkData(FoundI, X, Y, Z) = BLOCK_GRASS
                ChunkData(FoundI, X, 1, Z) = BLOCK_STONE
        Next Z, X
    End If
    Chunk(FoundI).LoadedChunkData = -1
    ChunkLoader = -1
End Function

Function ChunkReloader (FoundI, CX, CZ)
    If FoundI = 0 Then Exit Function
    Chunk(FoundI).LoadedRenderData = -1
    Dim As _Unsigned Long LV, LTV
    Dim As _Unsigned _Byte Block, Visibility, Light
    LV = (FoundI - 1) * ChunkSectionSize
    LTV = (FoundI - 1) * ChunkSectionSize
    For X = 1 To 16
        For Z = 1 To 16
            For Y = Chunk(FoundI).MinimumHeight To Chunk(FoundI).MaximumHeight
                Block = ChunkData(FoundI, X, Y, Z)
                If Block = BLOCK_AIR Then _Continue
                Visibility = isTransparent(ChunkData(FoundI, X + 1, Y, Z)) + 2 * isTransparent(ChunkData(FoundI, X - 1, Y, Z)) + 4 * isTransparent(ChunkData(FoundI, X, Y + 1, Z)) + 8 * isTransparent(ChunkData(FoundI, X, Y - 1, Z)) + 16 * isTransparent(ChunkData(FoundI, X, Y, Z + 1)) + 32 * isTransparent(ChunkData(FoundI, X, Y, Z - 1))
                If ChunkData(FoundI, X, Y + 1, Z) = BLOCK_WATER Then
                    If Block = BLOCK_WATER Then
                        Visibility = 0
                    Else
                        If Visibility = 0 Then Visibility = 63
                    End If
                End If
                Light = 0
                For YY = Y + 1 To Chunk(FoundI).MaximumHeight
                    If ChunkData(FoundI, X, YY, Z) Then Light = 5: Exit For
                Next YY
                If isTransparent(Block) Then
                    For I = 0 To 23
                        FACE%% = _SHL(1, _SHR(I, 2))
                        If (FACE%% And Visibility) = 0 Then _Continue
                        If Block = BLOCK_WATER Then If FACE%% <> 4 Then _Continue
                        LTV = LTV + 1
                        TVertices(LTV).X = CubeVertices(I).X + CX * 16 + X
                        TVertices(LTV).Y = CubeVertices(I).Y + Y + 0.125 * (Block = BLOCK_WATER)
                        TVertices(LTV).Z = CubeVertices(I).Z + CZ * 16 + Z
                        TTexCoords(LTV).X = (CubeTexCoords(I).X + SkyLight + Light * Sgn(FACE%% And 4) + 4 * Sgn(FACE%% And 48) + 6 * Sgn(FACE%% And 3) + 8 * Sgn(FACE%% And 8)) / 20
                        TTexCoords(LTV).Y = (CubeTexCoords(I).Y + _SHR(I, 2) + 6 * Block - 6) / IMAGEHEIGHT
                        Chunk(FoundI).TCount = Chunk(FoundI).TCount + 1
                    Next I
                Else
                    For I = 0 To 23
                        FACE%% = _SHL(1, _SHR(I, 2))
                        If (FACE%% And Visibility) = 0 Then _Continue
                        LV = LV + 1
                        Vertices(LV).X = CubeVertices(I).X + CX * 16 + X
                        Vertices(LV).Y = CubeVertices(I).Y + Y
                        Vertices(LV).Z = CubeVertices(I).Z + CZ * 16 + Z
                        TexCoords(LV).X = (CubeTexCoords(I).X + SkyLight + Light * Sgn(FACE%% And 4) + 4 * Sgn(FACE%% And 48) + 6 * Sgn(FACE%% And 3) + 8 * Sgn(FACE%% And 8)) / 20
                        TexCoords(LV).Y = (CubeTexCoords(I).Y + _SHR(I, 2) + 6 * Block - 6) / IMAGEHEIGHT
                        Chunk(FoundI).Count = Chunk(FoundI).Count + 1
                    Next I
                End If
    Next Y, Z, X
    Chunk(FoundI).ShowCount = Chunk(FoundI).Count
    Chunk(FoundI).ShowTCount = Chunk(FoundI).TCount
    Chunk(FoundI).ShowRenderData = -1
    LoadedChunks = LoadedChunks + 1
    ChunkReloader = -1
End Function

Sub ChunkReloadLight (FoundI, L As _Byte)
    T = L / 20
    I = (FoundI - 1) * ChunkSectionSize
    For J = 1 To Chunk(FoundI).ShowCount
        I = I + 1
        TexCoords(I).X = TexCoords(I).X + T
    Next J
    I = (FoundI - 1) * ChunkSectionSize
    For J = 1 To Chunk(FoundI).ShowTCount
        I = I + 1
        TTexCoords(I).X = TTexCoords(I).X + T
    Next J
End Sub

Sub SetBlockReloadChunk (X, Y, Z, B)
    Dim As Integer __CX, __CZ, __CPX, __CPY, __CPZ
    Dim As _Unsigned Long TMPLoadedChunks
    __CX = Int((X - 1) / 16): __CZ = Int((Z - 1) / 16)
    __CPX = Int(X - __CX * 16): __CPY = Int(Y): __CPZ = Int(Z - __CZ * 16)
    For I = LBound(Chunk) To UBound(Chunk)
        If Chunk(I).X = __CX And Chunk(I).Z = __CZ And Chunk(I).LoadedChunkData Then FoundI = I: Exit For
    Next I
    If FoundI = 0 Then Exit Sub
    ChunkData(FoundI, __CPX, __CPY, __CPZ) = B
    If __CPX = 1 Then SetAnotherBlockReloadChunk __CX - 1, __CZ, 17, __CPY, __CPZ, B
    If __CPX = 16 Then SetAnotherBlockReloadChunk __CX + 1, __CZ, 0, __CPY, __CPZ, B
    If __CPZ = 1 Then SetAnotherBlockReloadChunk __CX, __CZ - 1, __CPX, __CPY, 17, B
    If __CPZ = 16 Then SetAnotherBlockReloadChunk __CX, __CZ + 1, __CPX, __CPY, 0, B
    Chunk(FoundI).MinimumHeight = Max(0, Min(Chunk(FoundI).MinimumHeight, __CPY - 1))
    Chunk(FoundI).MaximumHeight = Max(Chunk(FoundI).MaximumHeight, __CPY)
    Chunk(FoundI).LoadedRenderData = 0
    Chunk(FoundI).Count = 0
    Chunk(FoundI).TCount = 0
    TMPLoadedChunks = LoadedChunks
    T = ChunkReloader(FoundI, __CX, __CZ)
    LoadedChunks = TMPLoadedChunks
    SaveChunkFile __CX, __CZ
End Sub

Sub SetAnotherBlockReloadChunk (__CX As Integer, __CZ As Integer, __CPX As Integer, __CPY As Integer, __CPZ As Integer, B)
    Dim As _Unsigned Long TMPLoadedChunks
    For I = LBound(Chunk) To UBound(Chunk)
        If Chunk(I).X = __CX And Chunk(I).Z = __CZ And Chunk(I).LoadedChunkData Then FoundI = I: Exit For
    Next I
    If FoundI = 0 Then Exit Sub
    ChunkData(FoundI, __CPX, __CPY, __CPZ) = B
    Chunk(FoundI).MinimumHeight = Max(0, Min(Chunk(FoundI).MinimumHeight, __CPY - 1))
    Chunk(FoundI).MaximumHeight = Max(Chunk(FoundI).MaximumHeight, __CPY)
    Chunk(FoundI).LoadedRenderData = 0
    Chunk(FoundI).Count = 0
    Chunk(FoundI).TCount = 0
    TMPLoadedChunks = LoadedChunks
    T = ChunkReloader(FoundI, __CX, __CZ)
    LoadedChunks = TMPLoadedChunks
    SaveChunkFile __CX, __CZ
End Sub

Sub SetBlock (X, Y, Z, B)
    __CX = Int((X - 1) / 16): __CZ = Int((Z - 1) / 16)
    __CPX = Int(X - __CX * 16): __CPY = Int(Y): __CPZ = Int(Z - __CZ * 16)
    For I = LBound(Chunk) To UBound(Chunk)
        If Chunk(I).X = __CX And Chunk(I).Z = __CZ Then FoundI = I: Exit For
    Next I
    If FoundI = 0 Then Exit Sub
    ChunkData(FoundI, __CPX, __CPY, __CPZ) = B
End Sub

Function LoadChunkFile (CX, CZ)
    For I = LBound(Chunk) To UBound(Chunk)
        If Chunk(I).X = CX And Chunk(I).Z = CZ And Chunk(I).LoadedChunkData = -1 Then Exit Function
        If Chunk(I).LoadedChunkData = 0 And FoundI = 0 Then FoundI = I
    Next I
    If FoundI = 0 Then Exit Function
    FILEX = Int(CX / 16): FILEZ = Int(CZ / 16)
    SEEKX = CX - FILEX * 16: SEEKZ = CZ - FILEZ * 16
    SIZE& = Len(Chunk(FoundI)) + Len(TMPCHUNKDATA())
    FILE$ = "saves" + FILESEP$ + WORLDFOLDER$ + FILESEP$ + "chunks" + FILESEP$ + "region_" + TS$(FILEX) + "_" + TS$(FILEZ) + ".chunkdata"
    If _FileExists(FILE$) Then
        F = FreeFile
        Open FILE$ For Binary As #F
        Seek #F, (SEEKX * 16 + SEEKZ) * SIZE& + 1
        Get #F, , Chunk(FoundI)
        If Chunk(FoundI).LoadedChunkData Then
            Get #F, , TMPCHUNKDATA()
            For X = 0 To 17: For Z = 0 To 17: For Y = 0 To ChunkHeight + 1: ChunkData(FoundI, X, Y, Z) = TMPCHUNKDATA(X, Y, Z): Next Y, Z, X
            Close #F
            LoadChunkFile = ChunkReloader(FoundI, CX, CZ)
        Else
            LoadChunkFile = ChunkLoader(FoundI, CX, CZ) And ChunkReloader(FoundI, CX, CZ)
        End If
    Else
        LoadChunkFile = ChunkLoader(FoundI, CX, CZ) And ChunkReloader(FoundI, CX, CZ)
    End If
End Function

Sub SaveChunkFile (CX, CZ)
    For I = LBound(Chunk) To UBound(Chunk)
        If Chunk(I).X = CX And Chunk(I).Z = CZ And Chunk(I).LoadedChunkData Then FoundI = I: Exit For
    Next I
    If FoundI = 0 Then Exit Sub
    F = FreeFile
    FILEX = Int(CX / 16): FILEZ = Int(CZ / 16)
    SEEKX = CX - FILEX * 16: SEEKZ = CZ - FILEZ * 16
    SIZE& = Len(Chunk(FoundI)) + Len(TMPCHUNKDATA())
    Open "saves" + FILESEP$ + WORLDFOLDER$ + FILESEP$ + "chunks" + FILESEP$ + "region_" + TS$(FILEX) + "_" + TS$(FILEZ) + ".chunkdata" For Binary As #F
    Seek #F, (SEEKX * 16 + SEEKZ) * SIZE& + 1
    Put #F, , Chunk(FoundI)
    For X = 0 To 17: For Z = 0 To 17: For Y = 0 To ChunkHeight + 1: TMPCHUNKDATA(X, Y, Z) = ChunkData(FoundI, X, Y, Z): Next Y, Z, X
    Put #F, , TMPCHUNKDATA()
    Close #F
End Sub

Sub LoadPlayerData
    Camera.X = 0.5: Camera.Y = GenerationChunkHeight: Camera.Z = 0.5
    CameraAngle.X = 0: CameraAngle.Y = 0
    If _FileExists("saves" + FILESEP$ + WORLDFOLDER$ + FILESEP$ + "world.dat") = 0 Then Exit Sub
    F = FreeFile
    Open "saves" + FILESEP$ + WORLDFOLDER$ + FILESEP$ + "world.dat" For Binary As #F
    Get #F, , Seed
    SeedRatio = Seed / 65536
    Get #F, , WorldFlat
    Get #F, , Camera
    Get #F, , CameraAngle
    Get #F, , Time
    Close #F
End Sub
Sub SavePlayerData
    Static SaveTime As Single
    If Timer - SaveTime < 1 Then Exit Sub
    SaveTime = Timer
    F = FreeFile
    Open "saves" + FILESEP$ + WORLDFOLDER$ + FILESEP$ + "world.dat" For Binary As #F
    Put #F, , Seed
    Put #F, , WorldFlat
    Put #F, , Camera
    Put #F, , CameraAngle
    Put #F, , Time
    Close #F
End Sub

Function LoadImage& (FP$)
    If _FileExists(FP$) Then LoadImage& = _LoadImage(FP$, 32): Exit Function
    If _FileExists(FP$ + ".png") Then LoadImage& = _LoadImage(FP$ + ".png", 32): Exit Function
    If _FileExists(FP$ + ".jpg") Then LoadImage& = _LoadImage(FP$ + ".jpg", 32): Exit Function
    If _FileExists(FP$ + ".jpeg") Then LoadImage& = _LoadImage(FP$ + ".jpeg", 32): Exit Function
    Print "Cannot load "; FP$
End Function

Function TS$ (A)
    TS$ = _Trim$(Str$(A))
End Function

Function isTransparent (B)
    If B = BLOCK_AIR Or B = BLOCK_WATER Then isTransparent = 1
End Function

Function isBlockFluid (B)
    If B = BLOCK_WATER Then isBlockFluid = 1
End Function

Function isNotBlock (B)
    If B = BLOCK_AIR Or B = BLOCK_WATER Then isNotBlock = 1
End Function

Function Dis2 (X1, Y1, X2, Y2)
    Dis2 = _Hypot(X1 - X2, Y1 - Y2) 'Got this _hypot() idea from bplus
End Function

Function Min (A, B)
    If A < B Then Min = A Else Min = B
End Function
Function Max (A, B)
    If A < B Then Max = B Else Max = A
End Function

Function InRange (A, B, C)
    If A <= B And B <= C Then InRange = -1
End Function

Function MouseInBox (X1, Y1, X2, Y2)
    MouseInBox = InRange(X1, _MouseX, X2) And InRange(Y1, _MouseY, Y2)
End Function

Sub ChunkRelativeCameraPosition (__Camera As Vec3_Float, __CX As _Byte, __CZ As _Byte, __CPX As _Byte, __CPY As _Byte, __CPZ As _Byte)
    __CX = Int((__Camera.X - 1) / 16)
    __CZ = Int((__Camera.Z - 1) / 16)
    __CPX = Int(__Camera.X - __CX * 16)
    __CPY = Int(__Camera.Y)
    __CPZ = Int(__Camera.Z - __CZ * 16)
End Sub

Function BlockExists (X, Y, Z)
    If InRange(1, Y, ChunkHeight) = 0 Then Exit Function
    __CX = Int((X - 1) / 16): __CZ = Int((Z - 1) / 16)
    __CPX = Int(X - __CX * 16): __CPY = Int(Y): __CPZ = Int(Z - __CZ * 16)
    For I = LBound(Chunk) To UBound(Chunk): If Chunk(I).X = __CX And Chunk(I).Z = __CZ And Chunk(I).LoadedChunkData Then FoundI = I: Exit For
    Next I: If FoundI = 0 Then Exit Function
    BlockExists = ChunkData(FoundI, __CPX, __CPY, __CPZ)
End Function

Sub Settings (__LOAD)
    If __LOAD And _FileExists("saves" + FILESEP$ + "settings.dat") = 0 Then Exit Sub
    __F = FreeFile
    Open "saves" + FILESEP$ + "settings.dat" For Binary As #__F
    If __LOAD Then
        Get #__F, , FOV
        Get #__F, , RenderDistance
        Get #__F, , FOG
    Else
        Put #__F, , FOV
        Put #__F, , RenderDistance
        Put #__F, , FOG
    End If
    Close #__F
End Sub

Sub Settings_Dialog
    Do
        Cls: _Limit 60
        If _Resize Then Screen _NewImage(_ResizeWidth, _ResizeHeight, 32): _PrintMode _KeepBackground
        While _MouseInput: Wend
        _PutImage (0, 0)-(_Width - 1, _Height - 1), GUI_ASSETS&(2)
        Slider RenderDistance, _Width / 2, _Height * 0.4, "Render Distance", 1, 16
        TotalChunks = (2 * RenderDistance + 1) ^ 2
        Slider FOV, _Width / 2, _Height * 0.5, "FOV", 70, 110
        If Button(2, _Width / 10, _Height / 10, "") Then Settings False: Exit Do
        _Display
    Loop
End Sub

Function Button (T~%%, X, Y, S$)
    Select Case T~%%
        Case 1:
            If InRange(X - 200, _MouseX, X + 199) And InRange(Y - 20, _MouseY, Y + 19) Then
                _PutImage (X - 200, Y - 20)-(X + 199, Y + 19), GUI_ASSETS&(4)
                If _MouseButton(1) Then Button = -1
                While _MouseButton(1) Or _MouseInput: Wend
            Else
                _PutImage (X - 200, Y - 20)-(X + 199, Y + 19), GUI_ASSETS&(3)
            End If
            PrintString X - Len(S$) * FONTWIDTH / 2, Y - FONTHEIGHT / 2, S$
        Case 9:
            _PutImage (X - 200, Y - 20)-(X + 199, Y + 19), GUI_ASSETS&(5)
            PrintString X - Len(S$) * FONTWIDTH / 2, Y - FONTHEIGHT / 2, S$
        Case 2:
            If InRange(X - 11, _MouseX, X + 11) And InRange(Y - 6, _MouseY, Y + 6) Then
                _PutImage (X - 11, Y - 6)-(X + 11, Y + 6), GUI_ASSETS&(15)
                If _MouseButton(1) Then Button = -1
                While _MouseButton(1) Or _MouseInput: Wend
            Else
                _PutImage (X - 11, Y - 6)-(X + 11, Y + 6), GUI_ASSETS&(14)
            End If
        Case 3:
            If InRange(X - 11, _MouseX, X + 11) And InRange(Y - 6, _MouseY, Y + 6) Then
                _PutImage (X - 11, Y - 6)-(X + 11, Y + 6), GUI_ASSETS&(17)
                If _MouseButton(1) Then Button = -1
                While _MouseButton(1) Or _MouseInput: Wend
            Else
                _PutImage (X - 11, Y - 6)-(X + 11, Y + 6), GUI_ASSETS&(16)
            End If
    End Select
End Function

Sub InputBox (X, Y, S$, H$, isInFocus, KeyHit)
    _PutImage (X - 200, Y - 10)-(X + 199, Y + 9), GUI_ASSETS&(11)
    If isInFocus And 2 * Timer(0.1) - Int(2 * Timer) > 0.5 Then C$ = "_"
    T$ = Right$(S$, 400 \ FONTWIDTH)
    PrintString X - 200, Y - FONTHEIGHT / 2, T$ + C$
    If Len(T$) = 0 Then PrintString X - Len(H$) * FONTWIDTH / 2, Y - FONTHEIGHT / 2, H$
    If isInFocus = 0 Then Exit Sub
    Select Case KeyHit
        Case 8: S$ = Left$(S$, Len(S$) - 1)
        Case 32 To 126: S$ = S$ + Chr$(KeyHit)
    End Select
End Sub

Sub CheckBox (X, Y, S$, C As _Unsigned _Byte)
    If C Then
        _PutImage (X - 10, Y - 10)-(X + 9, Y + 9), GUI_ASSETS&(19)
        If InRange(X - 10, _MouseX, X + 9) And InRange(Y - 10, _MouseY, Y + 9) Then
            _PutImage (X - 10, Y - 10)-(X + 9, Y + 9), GUI_ASSETS&(21)
            If _MouseButton(1) Then
                C = 0
                While _MouseButton(1) Or _MouseInput: Wend
            End If
        End If
    Else
        _PutImage (X - 10, Y - 10)-(X + 9, Y + 9), GUI_ASSETS&(18)
        If InRange(X - 10, _MouseX, X + 9) And InRange(Y - 10, _MouseY, Y + 9) Then
            _PutImage (X - 10, Y - 10)-(X + 9, Y + 9), GUI_ASSETS&(20)
            If _MouseButton(1) Then
                C = -1
                While _MouseButton(1) Or _MouseInput: Wend
            End If
        End If
    End If
    PrintString X + 18, Y - FONTHEIGHT / 2, S$
End Sub

Sub Slider (B As _Unsigned _Byte, X, Y, S$, A!, C!)
    T! = (B - A!) / (C! - A!) * 384 - 192
    If InRange(X - 200, _MouseX, X + 199) And InRange(Y - 20, _MouseY, Y + 19) Then
        _PutImage (X - 200, Y - 20)-(X + 199, Y + 19), GUI_ASSETS&(11)
        _PutImage (X + T - 8, Y - 20)-(X + T + 7, Y + 19), GUI_ASSETS&(13)
        If _MouseButton(1) Then
            B = A! + (_MouseX - X + 192) * (C! - A!) / 384
        End If
    Else
        _PutImage (X - 200, Y - 20)-(X + 199, Y + 19), GUI_ASSETS&(10)
        _PutImage (X + T - 8, Y - 20)-(X + T + 7, Y + 19), GUI_ASSETS&(12)
    End If
    T$ = S$ + ": " + _Trim$(Str$(B))
    PrintString X - Len(T$) * FONTWIDTH / 2, Y - FONTHEIGHT / 2, T$
End Sub

Sub PrintString (X, Y, S$)
    For I = 1 To Len(S$)
        A = Asc(S$, I)
        B = A Mod 16
        A = _SHR(A, 4)
        _PutImage (X + I * FONTWIDTH - FONTWIDTH + 1, Y)-(X + I * FONTWIDTH, Y + FONTHEIGHT - 1), FontImage, , (B * 8, A * 8)-(B * 8 + 7, A * 8 + 7)
    Next I
End Sub

Sub Vec3_FloatToInt (A As Vec3_Float, B As Vec3_Int)
    B.X = Int(A.X)
    B.Y = Int(A.Y)
    B.Z = Int(A.Z)
End Sub
