'$Dynamic
Type EntityBrain
    As Single BehavingFactors_Extrovert, BehavingFactors_Courage
    As _Unsigned _Byte LevelOfIntelligence
    As String Goals, DataMemory, GoalMemory, ExperienceMemory, Actions
    As Single EmotionalState_Happy, EmotionalState_Angry, EmotionalState_Love
    As String DecisionSlot_A, DecisionSlot_B
    As _Unsigned _Byte DecisionSlot_A_Score, DecisionSlot_B_Score
End Type
Type Vec2: As Integer X, Z: End Type
Type Entity
    As _Unsigned _Byte Live
    As Vec2 Position, Target
    As _Unsigned _Byte Face, Age, Health, MaxHealth
    As EntityBrain Brain
    As String Inventory
End Type

Const octaves = 7

Const UP = 1, DOWN = 2, LEFT = 4, RIGHT = 8
Dim Shared PlayerPosX, PlayerPosY, WALK_STATE_SPRITE, Entities(1) As Entity

ChDir "res"

'$Include:'QIMG.bi'
Dim Shared QIMG_Sprites(0) As QIMG_Sprite, WalkDown&, WalkUp&, WalkLeft&, WalkRight&
WalkDown& = QIMG_LoadSpriteFromFile&("Walk_Down.qimg")
WalkUp& = QIMG_LoadSpriteFromFile&("Walk_Up.qimg")
WalkLeft& = QIMG_LoadSpriteFromFile&("Walk_Left.qimg")
WalkRight& = QIMG_LoadSpriteFromFile&("Walk_Right.qimg")
WALK_STATE_SPRITE = WalkDown&

Dim Shared As Long TILE_GRASS, TILE_TREE, TILE_WATER, TILE_PLANKS
If _FileExists("grass.png") Then TILE_GRASS = _LoadImage("grass.png", 32) Else TILE_GRASS = _NewImage(1, 1, 32): _Dest TILE_GRASS: Cls , _RGB32(0, 191, 0)
If _FileExists("tree.png") Then TILE_TREE = _CopyImage(TILE_GRASS, 32): _Dest TILE_TREE: T& = _LoadImage("tree.png", 32): _PutImage (0, 0), T&: _FreeImage T&: Else TILE_TREE = _NewImage(1, 1, 32): _Dest TILE_TREE: Cls , _RGB32(127, 31, 0)
If _FileExists("water.png") Then TILE_WATER = _LoadImage("water.png", 32) Else TILE_WATER = _NewImage(1, 1, 32): _Dest TILE_WATER: Cls , _RGB32(0, 127, 255)
If _FileExists("planks.png") Then TILE_PLANKS = _LoadImage("planks.png", 32) Else TILE_PLANKS = _NewImage(1, 1, 32): _Dest TILE_PLANKS: Cls , _RGB32(188, 188, 128)

Screen _NewImage(800, 450, 32)
Randomize Timer
Dim Shared World(-1024 To 1023, -1024 To 1023) As Long, WorldViewOffsetX, WorldViewOffsetZ
Print "Generating World": GenerateWorld
Do: Cls: _Limit 20: While _MouseInput: Wend
    For X = 0 To _Width \ 16 - 1: For Z = 0 To _Height \ 32 - 1
            If InRange(LBound(World, 1), Int(WorldViewOffsetX) + X, UBound(World, 1)) = 0 Or InRange(LBound(World, 2), Int(WorldViewOffsetZ) + Z, UBound(World, 2)) = 0 Then _Continue
            _PutImage (X * 16, Z * 32)-(X * 16 + 15, Z * 32 + 31), World(Int(WorldViewOffsetX) + X, Int(WorldViewOffsetZ) + Z)
    Next Z, X
    QIMG_PutSprite WALK_STATE_SPRITE, 1, (-Int(WorldViewOffsetX) + PlayerPosX) * 16, (-Int(WorldViewOffsetZ) + PlayerPosY) * 32
    Move -UP * _KeyDown(18432) - DOWN * _KeyDown(20480) - LEFT * _KeyDown(19200) - RIGHT * _KeyDown(19712)
    _PrintString (0, 0), Str$(Int(PlayerPosX)) + Str$(Int(PlayerPosY))
    _Display
Loop Until Inp(&H60) = 1
System
Sub Move (D)
    If D And UP Then PlayerPosY = PlayerPosY - 1: WALK_STATE_SPRITE = WalkUp&
    If D And DOWN Then PlayerPosY = PlayerPosY + 1: WALK_STATE_SPRITE = WalkDown&
    If D And LEFT Then PlayerPosX = PlayerPosX - 1: WALK_STATE_SPRITE = WalkLeft&
    If D And RIGHT Then PlayerPosX = PlayerPosX + 1: WALK_STATE_SPRITE = WalkRight&
    WorldViewOffsetX = PlayerPosX - _Width \ 32: WorldViewOffsetZ = PlayerPosY - _Height \ 64
End Sub
Function InRange (A!, B!, C!): InRange = ((A! <= B!) And (B! <= C!)): End Function
Function NH# (X%)
    Static Perm(0 To 65535) As Double
    If Perm(0) = 0 Then
        For __I = 0 To 65535
            Perm(__I) = Rnd
        Next __I
    End If
    NH# = Perm(X% And 65535)
End Function
Function LI! (A!, B!, C!)
    LI! = A! + (B! - A!) * C!
End Function
Function N1D! (X!)
    N1D! = LI!(NH#(Int(X!)), NH#(Int(X!) + 1), X! - Int(X!))
End Function
Function N2D! (X!, Y!)
    fX% = Int(X!)
    fY% = Int(Y!)
    A! = LI!(NH#(fX% + fY% * 57), NH#(fX% + fY% * 57 + 1), X! - fX%)
    B! = LI!(NH#(fX% + fY% * 57 + 57), NH#(fX% + fY% * 57 + 58), X! - fX%)
    N2D! = LI!(A!, B!, Y! - fY!)
End Function
Function Noise2D! (X%, Y%)
    Dim As Single totalNoise, totalAmplitude, amplitude
    Dim As Integer frequency
    amplitude = 1
    frequency = 1
    X! = X% / 16
    Y! = Y% / 16
    For __I = 0 To octaves
        totalNoise = totalNoise + amplitude * N2D!(X! * frequency, Y! * frequency)
        totalAmplitude = totalAmplitude + amplitude
        frequency = frequency * 2
        amplitude = amplitude / 2
    Next __I
    Noise2D! = totalNoise / totalAmplitude
End Function
Sub GenerateWorld: For X = LBound(World, 1) To UBound(World, 1): For Z = LBound(World, 2) To UBound(World, 2): T = Int(3 * Noise2D!(X, Z)): Select Case T: Case 0: Tile = TILE_TREE: Case 1: Tile = TILE_GRASS: Case 2: Tile = TILE_WATER: End Select: World(X, Z) = Tile: Next Z, X: End Sub
'$Include:'.\Sprite_Subs.bas'
