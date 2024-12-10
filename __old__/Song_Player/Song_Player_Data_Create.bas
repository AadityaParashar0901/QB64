'Song Player
'By - Aaditya Parashar
'Creates Playlist File for Song Player from CSV File

$Console:Only
'$Dynamic
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
Dim As CONFIGURE Config, Config0
Dim As PLAYLIST PlayLists(1 To 25), PlayLists0(1 To 25)
Dim As SONG Songs(1 To 25, 1 To 100), Songs0(1 To 25, 1 To 100)
If _FileExists(Command$(1)) Then
    INFILE$ = Command$(1)
    OUTFILE$ = Left$(Command$(1), _InStrRev(Command$(1), ".") - 1) + ".dat"
Else
    INFILE$ = "Song_Player.txt"
    OUTFILE$ = "Song_Player.dat"
End If
If _FileExists(INFILE$) Then
    Open INFILE$ For Input As #1
    If _FileExists(OUTFILE$) Then Print "Editing Song Playlist File" Else Print "Creating Song Playlist File"
    Config.Current_PlayList = 1
    Config.Default_Volume = 10
    Config.Dynamic_Volume = -1
    Do While EOF(1) = 0
        nPlayList = nPlayList + 1
        Input #1, PlayLists(nPlayList).Name, PlayLists(nPlayList).Total_Songs
        _Echo _Trim$(PlayLists(nPlayList).Name)
        PlayLists(nPlayList).Current_Song = 1
        PlayLists(nPlayList).Current_Volume = Config.Default_Volume
        For nSongs = 1 To PlayLists(nPlayList).Total_Songs
            Input #1, Songs(nPlayList, nSongs).Name, Songs(nPlayList, nSongs).Path, Songs(nPlayList, nSongs).Specific_Volume, Songs(nPlayList, nSongs).Times_To_Play
            _Echo "    " + _Trim$(Songs(nPlayList, nSongs).Name)
            If _FileExists(Songs(nPlayList, nSongs).Path) = 0 Then
                Print "File does not exists."
                Do While _ConsoleInput <> 1: _Limit 60: Loop
                System
            End If
        Next nSongs
        If EOF(1) Then Exit Do
    Loop
    Config.Total_PlayLists = nPlayList
    Close #1
    If _FileExists(OUTFILE$) Then
        Open OUTFILE$ For Binary As #1
        Get #1, , Config0
        Get #1, , PlayLists0()
        Get #1, , Songs0()
        Close #1
        If Config0.Current_PlayList <= Config.Total_PlayLists Then Config.Current_PlayList = Config0.Current_PlayList
        For I = 1 To Config.Total_PlayLists
            If PlayLists0(I).Current_Song <= PlayLists(I).Total_Songs Then PlayLists(I).Current_Song = PlayLists0(I).Current_Song
            If PlayLists0(I).Name = PlayLists(I).Name Then
                PlayLists(I).Current_Volume = PlayLists0(I).Current_Volume
                For J = 1 To PlayLists(I).Total_Songs
                    If Songs(I, J).Name = Songs0(I, J).Name Then
                        Songs(I, J).Times_Played = Songs0(I, J).Times_Played
                        Songs(I, J).Repeat_On = Songs0(I, J).Repeat_On
                        Songs(I, J).Repeat_Start = Songs0(I, J).Repeat_Start
                        Songs(I, J).Repeat_End = Songs0(I, J).Repeat_End
                    End If
                Next J
            End If
        Next I
    End If
    Open OUTFILE$ For Binary As #1
    Put #1, , Config
    Put #1, , PlayLists()
    Put #1, , Songs()
    Close #1
End If
System
