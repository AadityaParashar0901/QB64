'Song Player
'By - Aaditya Parashar
'Version - 1.4.4
'
'Features -
'1. Upto 25 Playlists with upto 100 Songs each
'2. Simple UI:
'    Playlist Tabs
'    Songs List
'    Seek Bar
'    Can be Hidden
'    Custom Font (Cannot be changed by user)
'3. When App is in focus:
'    Space - Pauses or Plays Song
'4. Mouse:
'    Seek Bar
'    PlayList Control with NEW Hover Support
'    SongList Control with NEW Scroll Support
'    Volume Control
'5. Keyboard: (Keys { 0-9, +,-, *, /, . } only on Keypad with NumLock On}
'    App Controls:
'        To Hide App: H
'        To Show App: LCtrl LShift `
'        Song Navigation with Arrow Keys
'    Song Controls:
'        Play: 0 2
'        Pause: 2
'        Restart: 0 1
'        Stop: 0 3
'        Jump 5 Seconds Backward: 0 2 4
'        Jump 5 Seconds Forward: 0 2 5
'        Set Repetition Point A: 0 [
'        Set Repetition Point B: 0 ]
'        ReSet Repetition Points: 1 6
'        Reset Song Played Count: 0 Delete
'    Volume Controls:
'        Increase Volume by 1%: 0 +
'        Decrease Volume by 1%: 0 -
'        Increase Volume by 5%: 1 +
'        Decrease Volume by 5%: 1 -
'        Increase Volume by 50%: 3 +
'        Decrease Volume by 50%: 3 -
'        Increase Volume to 200% of Current Volume: 6 +
'        Decrease Volume to 50% of Current Volume: 6 -
'        Set Default Volume to PlayList's Current Volume: 0 *
'        Toggle Dynamic Volume: 0 /
'    Playlist Controls:
'        First Song: 0 Home
'        Last Song: 0 End
'        Previous Song: 0 4
'        Next Song: 0 5
'        Toggle Repeation: 0 6
'        Change Volume to Songs Specific: 0 7
'        Reload Song and Set Volume to 100%: 0 8
'        Exit Program: 9 or Esc when in focus
'        Reload Program: 0 9

$ExeIcon:'./icon.ico'
'$Dynamic
$Resize:On
Declare Dynamic Library "user32"
    Function GetKeyState% (ByVal nVirtKey As Long)
End Declare

Type CONFIGURE
    Current_PlayList As _Byte
    Default_Volume As _Unsigned _Byte
    Dynamic_Volume As _Byte
    Total_PlayLists As _Unsigned _Byte
End Type
Type PLAYLIST
    Name As String * 64
    Current_Song As _Byte
    Current_Volume As _Unsigned _Byte
    Total_Songs As _Byte
End Type
Type SONG
    Name As String * 64
    Path As String * 256
    Specific_Volume As _Unsigned _Byte
    Song_Handle As Long
    Times_To_Play As _Byte
    Times_Played As _Byte
    Repeat_On As _Byte
    Repeat_Start As Single
    Repeat_End As Single
End Type
Type UI
    ID As Integer
    Label As String * 16
    Text As String * 64
    Compact As _Byte
    SX As Integer
    SY As Integer
    EX As Integer
    EY As Integer
    FG As Long
    BG As Long
    MFG As Long
    MBG As Long
End Type
Dim Shared Config As CONFIGURE, PlayLists(1 To 25) As PLAYLIST, Songs(1 To 25, 1 To 100) As SONG
Dim Shared UI(1 To 100) As UI, nUI As _Byte, DisplayPlayList As Integer, SelectedSong As Integer
Dim Shared As Integer MX, MY, MB(1 To 3), MouseMoved
Dim Shared FONT&, FONTHEIGHT As _Unsigned _Byte, FONTWIDTH As _Unsigned _Byte
FONT& = _LoadFont("consola.ttf", 16, "MONOSPACE"): FONTWIDTH = _FontWidth(FONT&): FONTHEIGHT = _FontHeight(FONT&)
Dim Shared SONGFILE$
If _CommandCount Then SONGFILE$ = Command$(1) Else SONGFILE$ = "Song_Player.dat"
If _FileExists(SONGFILE$) = 0 Then System
Open SONGFILE$ For Binary As #1
If LOF(1) <> 841679 Then System
Get #1, , Config
Get #1, , PlayLists()
Get #1, , Songs()
Close
Dim Shared ScrollOffset As Integer
sx = 1
For I = 1 To Config.Total_PlayLists
    AddUIElement "PLAYLIST_TAB", " " + PlayLists(I).Name, 0, sx, 1, sx + Len(_Trim$(PlayLists(I).Name) + " ") * FONTWIDTH - 1, FONTHEIGHT, _RGB32(255), _RGB32(32), _RGB32(255), _RGB32(77)
    sx = sx + Len(_Trim$(PlayLists(I).Name) + " ") * FONTWIDTH - 1
Next I
MINWIDTH = Max(sx, 400)
MINHEIGHT = 164
Screen _NewImage(MINWIDTH, MINHEIGHT, 32)
_Font FONT&
Do While _Resize: Loop
_ScreenMove _DesktopWidth - 20 - MINWIDTH, 10
_PrintMode _KeepBackground
KeyMapTimer = _FreeTimer
On Timer(KeyMapTimer, 0.1) GoSub KeyMap
VolumeKeyMapTimer = _FreeTimer
On Timer(VolumeKeyMapTimer, 0.2) GoSub VolumeKeyMap
HideShowKeyMapTimer = _FreeTimer
On Timer(HideShowKeyMapTimer, 0.05) GoSub HideShowKeyMap
SleepModeKeyTimer = _FreeTimer
On Timer(SleepModeKeyTimer, 1) GoSub SleepModeKey
Timer(KeyMapTimer) On
Timer(VolumeKeyMapTimer) On
Timer(HideShowKeyMapTimer) On
Timer(SleepModeKeyTimer) On
DisplayPlayList = Config.Current_PlayList
SelectedSong = PlayLists(DisplayPlayList).Current_Song
ScrollOffset = SelectedSong
PlaySong DisplayPlayList, SelectedSong
Const FPS = 30
Do
    _Limit FPS
    FPSCount = (FPSCount + 1) Mod FPS
    If _Resize Then Screen _NewImage(Max(MINWIDTH, _ResizeWidth), Max(MINHEIGHT, _ResizeHeight), 32)
    If _Width < MINWIDTH Then Screen _NewImage(MINWIDTH, _Height, 32)
    If _Height < MINHEIGHT Then Screen _NewImage(_Width, MINHEIGHT, 32)
    Cls , _RGB32(0, 31, 63)
    MouseMoved = 0: LMW = 0
    While _MouseInput
        MX = _MouseX: MY = _MouseY
        MB(1) = _MouseButton(1): MB(2) = _MouseButton(2): MB(3) = _MouseButton(3)
        MW = _MouseWheel
        If MW Then LMW = MW
        MouseMoved = -1
    Wend
    MW = LMW

    If Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_On And _SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) >= Int(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_End) Then _SndSetPos Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_Start

    If _SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) >= _SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) Then
        If Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_On And Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_End >= Int(_SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)) Then _SndSetPos Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_Start
        If Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_Played < Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_To_Play - 1 Then
            PlaySong Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song
            Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_Played = Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_Played + 1
        Else
            Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_Played = 0
            __TMP = PlayLists(Config.Current_PlayList).Current_Song
            If PlayLists(Config.Current_PlayList).Current_Song = PlayLists(Config.Current_PlayList).Total_Songs Then PlayLists(Config.Current_PlayList).Current_Song = 1
            PlaySong Config.Current_PlayList, __TMP + 1
        End If
    End If

    If _KeyDown(32) Then
        If SpacePressed = 0 Then If _SndPlaying(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) Then _SndPause Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle Else _SndPlay Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
        SpacePressed = -1
    Else
        SpacePressed = 0
    End If
    DrawUIElements
    For I = 1 To Config.Total_PlayLists
        If MouseUIElement(I) = -1 Then PlaySong I, PlayLists(I).Current_Song
        If MouseUIElement(I) = 1 Then DisplayPlayList = I: SelectedSong = PlayLists(DisplayPlayList).Current_Song
        If I = Config.Current_PlayList Then
            UI(I).FG = _RGB32(255): UI(I).MFG = _RGB32(255)
            UI(I).BG = _RGB32(77): UI(I).MBG = _RGB32(122)
        Else
            UI(I).FG = _RGB32(255): UI(I).MFG = _RGB32(255)
            UI(I).BG = _RGB32(32): UI(I).MBG = _RGB32(77)
        End If
    Next I

    SelectedSong = SelectedSong + MW: ScrollOffset = ScrollOffset + MW
    If InRange(0, MX, _Width - 64) And InRange(FONTHEIGHT, MY, _Height - FONTHEIGHT * 1.25 - 1) Then 'Song
        If MB(1) Then PlaySong DisplayPlayList, MY \ FONTHEIGHT + ScrollOffset - 2
        If MouseMoved Then SelectedSong = MY \ FONTHEIGHT + ScrollOffset - 2
    End If
    If FPSCount Mod 3 = 0 Then
        SelectedSong = SelectedSong - _KeyDown(20480) + _KeyDown(18432)
        ScrollOffset = ScrollOffset - _KeyDown(20480) + _KeyDown(18432)
        If _KeyDown(19200) Or _KeyDown(19712) Then
            DisplayPlayList = (DisplayPlayList - _KeyDown(19712) + _KeyDown(19200)) Mod Config.Total_PlayLists
            If DisplayPlayList = 0 Then DisplayPlayList = Config.Total_PlayLists
            SelectedSong = PlayLists(DisplayPlayList).Current_Song
            ScrollOffset = SelectedSong - 2
        End If
        If _KeyDown(13) Then PlaySong DisplayPlayList, SelectedSong
    End If
    SelectedSong = Limit(1, PlayLists(DisplayPlayList).Total_Songs, SelectedSong)
    ScrollOffset = Limit(0, PlayLists(DisplayPlayList).Total_Songs - _Height \ FONTHEIGHT + 5, ScrollOffset)

    If InRange(_Width - 64, MX, _Width) And InRange(FONTHEIGHT, MY, _Height - FONTHEIGHT * 1.25 - 1) Then 'Volume
        If MB(1) Then PlayLists(Config.Current_PlayList).Current_Volume = 100 - Int(100 * (MY - FONTHEIGHT * 1.25) / (_Height - FONTHEIGHT * 2.5 - 1)): _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
    End If
    Line (_Width - 8, _Height - FONTHEIGHT * 1.25 - 1)-(_Width - 7, FONTHEIGHT * 1.25), _RGB32(255), BF
    Line (_Width - 10, _Height - FONTHEIGHT * 1.25 - 1 - PlayLists(Config.Current_PlayList).Current_Volume * (_Height - FONTHEIGHT * 2.5) / 100 - 2)-(_Width - 5, _Height - FONTHEIGHT * 1.25 - 1 - PlayLists(Config.Current_PlayList).Current_Volume * (_Height - FONTHEIGHT * 2.5) / 100 + 2), _RGB32(255), BF

    Line (UI(DisplayPlayList).SX, FONTHEIGHT)-(UI(DisplayPlayList).EX, FONTHEIGHT + 2), _RGB32(0, 127, 255), BF
    For I = ScrollOffset To ScrollOffset + _Height \ FONTHEIGHT - 4
        If InRange(1, I, PlayLists(DisplayPlayList).Total_Songs) Then
            If I = PlayLists(DisplayPlayList).Current_Song Then
                PrintString _Trim$(Songs(DisplayPlayList, I).Name), 1, (I + 2 - ScrollOffset) * FONTHEIGHT, _RGB32(0, 127, 255)
            ElseIf I = SelectedSong Then
                PrintString _Trim$(Songs(DisplayPlayList, I).Name), 1, (I + 2 - ScrollOffset) * FONTHEIGHT, _RGB32(255)
                Line (1, (I + 2 - ScrollOffset) * FONTHEIGHT)-(_Width - 1, (I + 3 - ScrollOffset) * FONTHEIGHT), _RGB32(0, 127, 255), B
            Else
                PrintString _Trim$(Songs(DisplayPlayList, I).Name), 1, (I + 2 - ScrollOffset) * FONTHEIGHT, _RGB32(255)
            End If
        End If
    Next I
    If MB(1) And InRange(_Height - 4 - FONTHEIGHT, MY, _Height) Then
        _SndSetPos (Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle), _SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) * MX / _Width
        _SndPause Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
        LMB = -1
    Else
        If LMB Then _SndPlay Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle: LMB = 0
    End If
    PrintString _Trim$(Str$(PlayLists(Config.Current_PlayList).Current_Volume)) + ", " + _Trim$(Str$(Config.Default_Volume)), _Width - 8 * FONTWIDTH, FONTHEIGHT * (_Height \ FONTHEIGHT - 1), _RGB32(255)
    PrintString T$(_SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)) + "/" + T$(_SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)), _Width \ 2 - FONTWIDTH * 2.5, FONTHEIGHT * (_Height \ FONTHEIGHT - 1), _RGB32(255)
    PrintString _Trim$(Str$(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_Played + 1)) + "/" + _Trim$(Str$(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_To_Play)), 1, FONTHEIGHT * (_Height \ FONTHEIGHT - 1), _RGB32(255)
    Repeat_Start = Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_Start / _SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)
    Repeat_End = Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_End / _SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)
    Current_Song_Progress = _SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) / _SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)
    Line (0, _Height - 4)-(Current_Song_Progress * _Width, _Height - 1), _RGB32(255), BF
    If Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_On Then
        Line (Repeat_Start * _Width, _Height - 4)-(Repeat_Start * _Width + 1, _Height - 1), _RGB32(0, 255, 0), BF
        Line (Repeat_End * _Width, _Height - 4)-(Repeat_End * _Width + 1, _Height - 1), _RGB32(255, 0, 0), BF
    Else
        Line (Repeat_Start * _Width, _Height - 4)-(Repeat_Start * _Width + 1, _Height - 1), _RGB32(0, 127, 0), BF
        Line (Repeat_End * _Width, _Height - 4)-(Repeat_End * _Width + 1, _Height - 1), _RGB32(127, 0, 0), BF
    End If
    If Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_On Then PrintString "Repeat: On", _Width - FONTWIDTH * 12, FONTHEIGHT * (_Height \ FONTHEIGHT - 3), _RGB32(255) Else PrintString "Repeat: Off", _Width - FONTWIDTH * 12, FONTHEIGHT * (_Height \ FONTHEIGHT - 3), _RGB32(255)
    If Config.Dynamic_Volume Then PrintString "Dynamic", _Width - FONTWIDTH * 8, FONTHEIGHT * (_Height \ FONTHEIGHT - 2), _RGB32(255) Else PrintString "Static", _Width - FONTWIDTH * 8, FONTHEIGHT * (_Height \ FONTHEIGHT - 2), _RGB32(255)
    SONGNAME$ = _Trim$(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Name)
    If _SndPlaying(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) Then _Title T$(_SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)) + "/" + T$(_SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)) + " " + SONGNAME$ Else _Title "Paused - " + SONGNAME$
    _Display
    On _Exit GOTO Exited
Loop Until Inp(&H60) = 1
Exited:
Open SONGFILE$ For Binary As #1
Put #1, , Config
Put #1, , PlayLists()
Put #1, , Songs()
Close
System
SleepModeKey:
If GetKeyState%(110) < 0 Then SleepModeCount = SleepModeCount + 1 Else SleepModeCount = 0
If SleepModeCount = 2 Then
    SleepMode = Not SleepMode
    Sound2 SleepMode
End If
Return
HideShowKeyMap:
If GetKeyState%(72) < 0 Then _ScreenHide
If GetKeyState%(17) < 0 Then If GetKeyState%(16) < 0 Then If GetKeyState%(192) < 0 Then _ScreenShow: DisplayPlayList = Config.Current_PlayList
Return
KeyMap:
Timer(KeyMapTimer) Off
KeyPressCount = KeyPressCount - Sgn(KeyPressCount)
If SleepMode Then
    SleepModeKeyMapTimeCount = SleepModeKeyMapTimeCount - Sgn(SleepModeKeyMapTimeCount)
    If SleepModeKeyMapTimeCount > 0 Then
        Timer(KeyMapTimer) On
        Return
    End If
End If
Key$ = InKey$
If SleepMode = -1 Then
    If GetKeyState%(98) < 0 Then '2
        If GetKeyState%(96) < 0 Then '0
            If _SndPlaying(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) = 0 Then _SndPlay Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
        Else
            _SndPause Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
            _Title "Paused - " + SONGNAME$
        End If
        SleepModeKeyMapTimeCount = 20
    End If
Else
    If GetKeyState%(96) < 0 Then '0
        If GetKeyState%(97) < 0 Then '1
            PlaySong Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song
        End If
        If GetKeyState%(98) < 0 Then '2
            If _SndPlaying(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) Then
                If GetKeyState%(100) < 0 Then '4
                    _SndSetPos Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, _SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) - 5
                End If
                If GetKeyState%(101) < 0 Then '5
                    _SndSetPos Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, _SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle) + 5
                End If
            Else
                _SndPlay Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
            End If
        Else
            If KeyPressCount = 0 Then
                If GetKeyState%(100) < 0 Then '4
                    PlaySong Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song - 1
                    KeyPressCount = 2
                End If
                If GetKeyState%(101) < 0 Then '5
                    PlaySong Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song + 1
                    KeyPressCount = 2
                End If
            End If
        End If
        If GetKeyState%(99) < 0 Then '3
            _SndStop Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
        End If
        If KeyPressCount = 0 Then
            If GetKeyState%(102) < 0 Then '6
                Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_On = Not Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_On
                Sound2 Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_On
                KeyPressCount = 5
            End If
        End If
        If GetKeyState%(219) < 0 Then '[
            If Key219Pressed = 0 Then Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_Start = _SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)
            Key219Pressed = -1
        Else
            Key219Pressed = 0
        End If
        If GetKeyState%(221) < 0 Then ']
            If Key221Pressed = 0 Then Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_End = _SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)
            Key221Pressed = -1
        Else
            Key221Pressed = 0
        End If
        If GetKeyState%(103) < 0 Then '7
            PlayLists(Config.Current_PlayList).Current_Volume = Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Specific_Volume
            _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
        End If
        If GetKeyState%(104) < 0 Then '8
            _SndPause Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
            SongPosition = _SndGetPos(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)
            _SndClose Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
            Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle = _SndOpen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Path)
            _SndSetPos Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, SongPosition
            _SndPlay Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
            PlayLists(Config.Current_PlayList).Current_Volume = 100
        End If
        If GetKeyState%(105) < 0 Then '9
            Shell _Hide "start " + Command$(0) + " " + Command$(1) + " " + Command$(2)
            GoTo Exited
        End If
        If KeyPressCount = 0 Then
            If GetKeyState%(106) < 0 Then '*
                Config.Default_Volume = PlayLists(Config.Current_PlayList).Current_Volume
                Sound2 -1
                KeyPressCount = 5
            End If
        End If

        For __I = 49 To 57 '1 - 9
            If KeyPressCount = 0 Then
                If GetKeyState%(__I) < 0 And PlayLists(Config.Current_PlayList).Current_Song + __I - 53 <= PlayLists(Config.Current_PlayList).Total_Songs Then
                    PlaySong Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song + __I - 53
                    KeyPressCount = 2
                End If
            End If
        Next __I

        If KeyPressCount = 0 Then
            If PlayLists(Config.Current_PlayList).Current_Song > 1 Then
                If GetKeyState%(36) < 0 Then 'Home
                    PlaySong Config.Current_PlayList, 1
                    KeyPressCount = 2
                End If
            End If
            If PlayLists(Config.Current_PlayList).Current_Song < PlayLists(Config.Current_PlayList).Total_Songs Then
                If GetKeyState%(35) < 0 Then 'End
                    PlaySong Config.Current_PlayList, PlayLists(Config.Current_PlayList).Total_Songs
                    KeyPressCount = 2
                End If
            End If
        End If
        If Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_Played Then
            If GetKeyState%(46) < 0 Then 'Delete
                Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Times_Played = 0
            End If
        End If
    Else
        If GetKeyState%(97) < 0 Then '1
            If GetKeyState%(100) < 0 Then '4
                If Config.Current_PlayList > 1 Then PlaySong Config.Current_PlayList - 1, PlayLists(Config.Current_PlayList - 1).Current_Song Else PlaySong Config.Total_PlayLists, PlayLists(Config.Total_PlayLists).Current_Song
            End If
            If GetKeyState%(101) < 0 Then '5
                If Config.Current_PlayList < Config.Total_PlayLists Then PlaySong Config.Current_PlayList + 1, PlayLists(Config.Current_PlayList + 1).Current_Song Else PlaySong 1, PlayLists(1).Current_Song
            End If
            If GetKeyState%(102) < 0 Then '6
                Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_Start = 0
                Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Repeat_End = _SndLen(Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle)
            End If
            For __I = 49 To 57 '1 - 9
                If KeyPressCount = 0 Then
                    If GetKeyState%(__I) < 0 And Config.Current_PlayList + __I - 53 <= Config.Total_PlayLists Then
                        PlaySong Config.Current_PlayList + __I - 53, PlayLists(Config.Current_PlayList + __I - 53).Current_Song
                        KeyPressCount = 2
                    End If
                End If
            Next __I
            If KeyPressCount = 0 Then
                If Config.Current_PlayList > 1 Then
                    If GetKeyState%(36) < 0 Then 'Home
                        PlaySong 1, PlayLists(1).Current_Song
                        KeyPressCount = 2
                    End If
                End If
                If Config.Current_PlayList < Config.Total_PlayLists Then
                    If GetKeyState%(35) < 0 Then 'End
                        PlaySong Config.Total_PlayLists, PlayLists(Config.Total_PlayLists).Current_Song
                        KeyPressCount = 2
                    End If
                End If
            End If
        End If
        If GetKeyState%(98) < 0 Then '2
            _SndPause Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle
            _Title "Paused - " + SONGNAME$
        End If
        If GetKeyState%(99) < 0 Then '3
            If GetKeyState%(100) < 0 Then '4
                PlaySong Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song - 5
            End If
            If GetKeyState%(101) < 0 Then '5
                PlaySong Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song + 5
            End If
        End If
        If GetKeyState%(105) < 0 Then '9
            GoTo Exited
        End If
    End If
End If
Timer(KeyMapTimer) On
Return
VolumeKeyMap:
Timer(VolumeKeyMapTimer) Off
KeyPressCount2 = KeyPressCount2 - Sgn(KeyPressCount2)
If SleepMode = 0 Then
    If GetKeyState%(96) < 0 Then '0
        If GetKeyState%(107) < 0 Then '+
            If PlayLists(Config.Current_PlayList).Current_Volume < 200 Then PlayLists(Config.Current_PlayList).Current_Volume = PlayLists(Config.Current_PlayList).Current_Volume + 1
            _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
        End If
        If GetKeyState%(109) < 0 Then '-
            If PlayLists(Config.Current_PlayList).Current_Volume > 0 Then PlayLists(Config.Current_PlayList).Current_Volume = PlayLists(Config.Current_PlayList).Current_Volume - 1
            _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
        End If
        If KeyPressCount2 = 0 Then
            If GetKeyState%(111) < 0 Then '/
                Config.Dynamic_Volume = Not Config.Dynamic_Volume
                Sound2 Config.Dynamic_Volume
                KeyPressCount2 = 5
            End If
        End If
    Else
        If GetKeyState%(97) < 0 Then '1
            If GetKeyState%(107) < 0 Then '+
                If PlayLists(Config.Current_PlayList).Current_Volume <= 195 Then PlayLists(Config.Current_PlayList).Current_Volume = PlayLists(Config.Current_PlayList).Current_Volume + 5
                _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
            End If
            If GetKeyState%(109) < 0 Then '-
                If PlayLists(Config.Current_PlayList).Current_Volume >= 5 Then PlayLists(Config.Current_PlayList).Current_Volume = PlayLists(Config.Current_PlayList).Current_Volume - 5
                _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
            End If
        End If
        If GetKeyState%(99) < 0 Then '3
            If GetKeyState%(107) < 0 Then '+
                If PlayLists(Config.Current_PlayList).Current_Volume <= 150 Then PlayLists(Config.Current_PlayList).Current_Volume = PlayLists(Config.Current_PlayList).Current_Volume + 50
                _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
            End If
            If GetKeyState%(109) < 0 Then '-
                If PlayLists(Config.Current_PlayList).Current_Volume >= 50 Then PlayLists(Config.Current_PlayList).Current_Volume = PlayLists(Config.Current_PlayList).Current_Volume - 50
                _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
            End If
        End If
        If GetKeyState%(102) < 0 Then '6
            If GetKeyState%(107) < 0 Then '+
                If PlayLists(Config.Current_PlayList).Current_Volume <= 100 Then PlayLists(Config.Current_PlayList).Current_Volume = PlayLists(Config.Current_PlayList).Current_Volume * 2 Else PlayLists(Config.Current_PlayList).Current_Volume = 200
                _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
            End If
            If GetKeyState%(109) < 0 Then '-
                If PlayLists(Config.Current_PlayList).Current_Volume >= 2 Then PlayLists(Config.Current_PlayList).Current_Volume = PlayLists(Config.Current_PlayList).Current_Volume \ 2 Else PlayLists(Config.Current_PlayList).Current_Volume = 1
                _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
            End If
        End If
        If GetKeyState%(103) < 0 Then '7
            PlayLists(Config.Current_PlayList).Current_Volume = Config.Default_Volume
            _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
        End If
    End If
Else
    If GetKeyState%(96) >= 0 Then '0
        If GetKeyState%(103) < 0 Then '7
            PlayLists(Config.Current_PlayList).Current_Volume = Config.Default_Volume
            _SndVol Songs(Config.Current_PlayList, PlayLists(Config.Current_PlayList).Current_Song).Song_Handle, PlayLists(Config.Current_PlayList).Current_Volume / 100
        End If
    End If
End If
Timer(VolumeKeyMapTimer) On
Return

Sub LoadSong (A As _Byte, B As _Byte)
    Songs(A, B).Song_Handle = _SndOpen(Songs(A, B).Path)
End Sub
Function PlayListID (PID As _Byte)
    If PID > Config.Total_PlayLists Then PID = 1
    If PID < 1 Then PID = Config.Total_PlayLists
    PlayListID = PID
End Function
Function SongID (PID As _Byte, SID As _Byte)
    If SID = 0 Then
        PID = PlayListID(PID - 1)
        SID = PlayLists(PID).Current_Song
        SongID = -1
    ElseIf SID > PlayLists(PID).Total_Songs Then
        PID = PlayListID(PID + 1)
        SID = PlayLists(PID).Current_Song
        SongID = -1
    Else
        PID = PlayListID(PID)
    End If
End Function
Sub PlaySong (NextPlayList As _Byte, NextSong As _Byte) Static
    If __LastPlayList > 0 And __LastSong > 0 Then
        If _SndPlaying(Songs(__LastPlayList, __LastSong).Song_Handle) Then _SndStop Songs(__LastPlayList, __LastSong).Song_Handle
        _SndClose Songs(__LastPlayList, __LastSong).Song_Handle
    End If
    __TMP = SongID(NextPlayList, NextSong)
    If __LastPlayList > 0 And __LastSong > 0 Then
        If NextPlayList <> __LastPlayList Then
            Config.Current_PlayList = NextPlayList
        End If
    End If
    LoadSong NextPlayList, NextSong
    If Config.Dynamic_Volume Then PlayLists(Config.Current_PlayList).Current_Volume = Songs(Config.Current_PlayList, NextSong).Specific_Volume
    _SndVol Songs(NextPlayList, NextSong).Song_Handle, PlayLists(NextPlayList).Current_Volume / 100
    _SndPlay Songs(NextPlayList, NextSong).Song_Handle
    PlayLists(NextPlayList).Current_Song = NextSong
    If Songs(NextPlayList, NextSong).Repeat_End = 0 Then Songs(NextPlayList, NextSong).Repeat_End = _SndLen(Songs(NextPlayList, NextSong).Song_Handle)
    Open SONGFILE$ For Binary As #1
    Put #1, , Config
    Put #1, , PlayLists()
    Put #1, , Songs()
    Close
    DisplayPlayList = NextPlayList
    SelectedSong = NextSong
    ScrollOffset = NextSong - 2
    __LastPlayList = NextPlayList
    __LastSong = NextSong
End Sub
Sub Sound2 (A)
    If A Then
        Sound 250, 2
        Sound 500, 4
    Else
        Sound 500, 2
        Sound 250, 4
    End If
End Sub

Sub AddUIElement (LBL As String * 16, TXT As String * 64, CMP, SX, SY, EX, EY, FG As Long, BG As Long, MFG As Long, MBG As Long)
    If CMP Then TXT = _Trim$(TXT)
    nUI = nUI + 1
    UI(nUI).ID = nUI
    UI(nUI).Label = LBL
    UI(nUI).Text = TXT
    UI(nUI).Compact = CMP
    UI(nUI).SX = SX
    UI(nUI).SY = SY
    UI(nUI).EX = EX
    UI(nUI).EY = EY
    UI(nUI).FG = FG
    UI(nUI).BG = BG
    UI(nUI).MFG = MFG
    UI(nUI).MBG = MBG
End Sub
Sub DrawUIElements ()
    For I = 1 To nUI
        If UI(nUI).Label <> String$(16, Chr$(0)) Then DrawUIElement I
    Next I
End Sub
Sub DrawUIElement (ID)
    Dim u As UI
    u = UI(ID)
    If MouseUIElement(ID) Then
        Line (u.SX, u.SY)-(u.EX, u.EY), u.MBG, BF
        PrintString u.Text, u.SX, u.SY, u.MFG
    Else
        Line (u.SX, u.SY)-(u.EX, u.EY), u.BG, BF
        PrintString u.Text, u.SX, u.SY, u.FG
    End If
End Sub
Function MouseUIElement (ID)
    Dim u As UI
    u = UI(ID)
    If MX >= u.SX And MX <= u.EX And MY >= u.SY And MY <= u.EY Then
        If MB(1) Then MouseUIElement = -1 Else MouseUIElement = 1
    End If
End Function
Sub PrintString (__S$, __X, __Y, __FG As Long)
    __OFG& = _DefaultColor
    __OBG& = _BackgroundColor
    Color __FG, _RGBA32(0, 0, 0, 0)
    _PrintString (__X, __Y), __S$
    Color __OFG&, __OBG&
End Sub

Function T$ (__A As Long)
    __A = Abs(__A)
    __m$ = _Trim$(Str$(__A \ 60))
    __s$ = _Trim$(Str$(__A Mod 60))
    If Len(__m$) = 0 Then __m$ = "00"
    If Len(__m$) = 1 Then __m$ = "0" + __m$
    If Len(__s$) = 0 Then __s$ = "00"
    If Len(__s$) = 1 Then __s$ = "0" + __s$
    T$ = __m$ + ":" + __s$
End Function
Function Min (__A, __B)
    If __A < __B Then Min = __A Else Min = __B
End Function
Function Max (__A, __B)
    If __A > __B Then Max = __A Else Max = __B
End Function
Function InRange (__A, __B, __C)
    If __A <= __B And __B <= __C Then InRange = -1 Else InRange = 0
End Function
Function Limit (__A, __B, __C)
    Limit = Min(Max(__A, __C), __B)
End Function
