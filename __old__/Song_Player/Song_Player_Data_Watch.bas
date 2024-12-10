'Song Player
'By - Aaditya Parashar
'Creates Info File from Playlist File of Song Player

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
Dim As CONFIGURE Config
Dim As PLAYLIST PlayLists(1 To 25)
Dim As SONG Songs(1 To 25, 1 To 100)
If _FileExists(Command$(1)) Then
    INFILE$ = Left$(Command$(1), _InStrRev(Command$(1), ".") - 1) + ".info.txt"
    OUTFILE$ = Command$(1)
Else
    INFILE$ = "Song_Player.info.txt"
    OUTFILE$ = "Song_Player.dat"
End If
If _FileExists(OUTFILE$) Then
    Open OUTFILE$ For Binary As #1
    Get #1, , Config
    Get #1, , PlayLists()
    Get #1, , Songs()
    Close #1
    Open INFILE$ For Output As #1
    Print #1, "Config {"
    Print #1, "    Playlist:"; Config.Current_PlayList; "/"; Config.Total_PlayLists
    Print #1, "    Default Volume:"; Config.Default_Volume
    Print #1, "    Dynamic Volume:"; Config.Dynamic_Volume
    Print #1, "}"
    Print #1, "Playlists {"
    For I = 1 To 25
        If PlayLists(I).Name <> String$(64, 0) Or PlayLists(I).Total_Songs > 0 Then
            Print #1, "    Playlist(" + NewINT$(I) + "):"; _Trim$(PlayLists(I).Name); " {"
            Print #1, "        Total Songs:"; PlayLists(I).Total_Songs
            Print #1, "        Current Song:"; PlayLists(I).Current_Song
            Print #1, "        Current Volume:"; PlayLists(I).Current_Volume
            For J = 1 To 100
                If Songs(I, J).Name <> String$(64, 0) Or Songs(I, J).Path <> String$(256, 0) Then
                    Print #1, "        Song(" + NewINT$(J) + "):"; _Trim$(Songs(I, J).Name); " {"
                    Print #1, "            Path:"; Songs(I, J).Path
                    Print #1, "            Specific Volume:"; Songs(I, J).Specific_Volume
                    Print #1, "            Times:"; Songs(I, J).Times_Played + 1; "/"; Songs(I, J).Times_To_Play
                    If Songs(I, J).Repeat_On Then Print #1, "            Repeat On:"; Songs(I, J).Repeat_Start; "~"; Songs(I, J).Repeat_End Else Print #1, "            Repeat Off:"; Songs(I, J).Repeat_Start; "~"; Songs(I, J).Repeat_End
                    Print #1, "        }"
                End If
            Next J
            Print #1, "    }"
        End If
    Next I
    Print #1, "}"
    Close
End If
System
Function NewINT$ (__A As Integer)
    __I$ = _Trim$(Str$(__A))
    NewINT$ = String$(3 - Len(__I$), "0") + __I$
End Function
