'$Dynamic
Type UI_Element
    As Integer X1, Y1, X2, Y2
    As String Text
    As Long BG, HBG, CBG
    As _Unsigned _Byte Clicked
    As Integer TextScroll
    As Long LastScroll
    As _Byte ScrollDirection
End Type
Dim Shared FILE$, Parsed$, P$, S$
Dim Shared Song_Handle As Long, Song_Volume As Single
Dim Shared SELECTED_PLAYLIST_ID As Long, SELECTED_SONG_ID As Long, SELECTED_SONG_NAME$
Dim Shared VERTICALSCROLLOFFSET As Long, PLAYLIST_ID As Long
Dim Shared UI_Elements(0) As UI_Element
If _CommandCount Then
    If Command$(1) = "--create" Then
        If _FileExists(Command$(2)) Then Parse Command$(2): SaveParse Command$(3): FILE$ = Command$(3)
        If _FileExists(_StartDir$ + "\" + Command$(2)) Then Parse _StartDir$ + "\" + Command$(2): SaveParse _StartDir$ + "\" + Command$(3): FILE$ = _StartDir$ + "\" + Command$(3)
    Else
        __F = FreeFile
        FILE$ = Command$(1)
        Open FILE$ For Binary As #__F
        Parsed$ = String$(LOF(__F), 0)
        Get #__F, , Parsed$
        Close #__F
    End If
Else
    If _FileExists("Song_Player.dat") = 0 Then System
    FILE$ = "Song_Player.dat"
    __F = FreeFile
    Open FILE$ For Binary As #__F
    Parsed$ = String$(LOF(__F), 0)
    Get #__F, , Parsed$
    Close #__F
End If
Dim OFFSET As Long
OFFSET = 0
For I = 1 To ListLength~&(Parsed$)
    P$ = ListGet$(ListGet$(Parsed$, I), 1)
    t = Len(P$) * _FontWidth
    UI_Add OFFSET, 1, OFFSET + t, _FontHeight, P$, _RGB32(31), _RGB32(63), _RGB32(127)
    OFFSET = OFFSET + t + _FontWidth
Next I
Screen _NewImage(Max(OFFSET, 640), 240, 32)
_PrintMode _KeepBackground
_Title "Song Player 2"
PLAYLIST_ID = 1
VERTICALSCROLLOFFSET = 1
Song_Handle = 0
OFFSET = 0
P$ = ListGet$(Parsed$, PLAYLIST_ID)
S$ = ListGet$(P$, 3)
Do
    Cls
    LMW = 0
    MW = 0
    While _MouseInput
        LMW = _MouseWheel
        If LMW Then MW = LMW
    Wend
    VERTICALSCROLLOFFSET = Max(1, Min(VERTICALSCROLLOFFSET + MW, ListLength~&(S$) - _Height \ _FontHeight + 5))
    _Limit 30
    Color _RGB32(0, 255, 127), 0
    UI_Draw_All
    Line (0, _FontHeight)-(_Width, _FontHeight), -1, B
    Line (0, _Height - 3 * _FontHeight)-(_Width, _Height), _RGB32(0, 127, 255), BF
    Color -1, 0
    If OFFSET = 0 Or OFFSET = _Width Or OFFSET = -_Width Then
        OLD_PLAYLIST_ID = PLAYLIST_ID
        OP$ = P$
        OS$ = S$
        OFFSET = 0
    End If
    For I = 1 To UBound(UI_Elements)
        If UI_Elements(I).Clicked Then
            VERTICALSCROLLOFFSET = 1
            PLAYLIST_ID = I
            P$ = ListGet$(Parsed$, PLAYLIST_ID)
            S$ = ListGet$(P$, 3)
        End If
        If PLAYLIST_ID = I Then
            UI_Elements(I).BG = _RGB32(127)
            UI_Elements(I).HBG = _RGB32(127)
            UI_Elements(I).CBG = _RGB32(127)
        Else
            UI_Elements(I).BG = _RGB32(31)
            UI_Elements(I).HBG = _RGB32(63)
            UI_Elements(I).CBG = _RGB32(127)
        End If
    Next I
    PLAYLIST_ID = Min(Max(1, PLAYLIST_ID), ListLength~&(Parsed$))
    If _MouseButton(1) And InBox(0, _FontHeight, _MouseX, _MouseY, _Width, _Height - 3 * _FontHeight) Then
        PlaySong PLAYLIST_ID, VERTICALSCROLLOFFSET + _MouseY \ _FontHeight - 1
    End If
    If OLD_PLAYLIST_ID <> PLAYLIST_ID Then
        If OLD_PLAYLIST_ID > PLAYLIST_ID Then
            For I = 1 To Min(_Height \ _FontHeight - 4, ListLength~&(S$) - VERTICALSCROLLOFFSET + 1)
                If VERTICALSCROLLOFFSET + I - 1 = SELECTED_SONG_ID And SELECTED_PLAYLIST_ID = PLAYLIST_ID Then Line (OFFSET - _Width, I * _FontHeight)-(OFFSET, (I + 1) * _FontHeight), _RGB32(0, 63, 127), BF
                _PrintString (OFFSET - _Width, I * _FontHeight), OneByteDecode$(ListGet$(ListGet$(S$, VERTICALSCROLLOFFSET + I - 1), 1))
            Next I
            For I = 1 To Min(_Height \ _FontHeight - 4, ListLength~&(OS$) - VERTICALSCROLLOFFSET + 1)
                If VERTICALSCROLLOFFSET + I - 1 = SELECTED_SONG_ID And SELECTED_PLAYLIST_ID = OLD_PLAYLIST_ID Then Line (OFFSET, I * _FontHeight)-(OFFSET + _Width, (I + 1) * _FontHeight), _RGB32(0, 63, 127), BF
                _PrintString (OFFSET, I * _FontHeight), OneByteDecode$(ListGet$(ListGet$(OS$, VERTICALSCROLLOFFSET + I - 1), 1))
            Next I
            OFFSET = OFFSET + ease(_Width - OFFSET)
        Else
            For I = 1 To Min(_Height \ _FontHeight - 4, ListLength~&(S$) - VERTICALSCROLLOFFSET + 1)
                If VERTICALSCROLLOFFSET + I - 1 = SELECTED_SONG_ID And SELECTED_PLAYLIST_ID = PLAYLIST_ID Then Line (OFFSET + _Width, I * _FontHeight)-(OFFSET + 2 * _Width, (I + 1) * _FontHeight), _RGB32(0, 63, 127), BF
                _PrintString (OFFSET + _Width, I * _FontHeight), OneByteDecode$(ListGet$(ListGet$(S$, VERTICALSCROLLOFFSET + I - 1), 1))
            Next I
            For I = 1 To Min(_Height \ _FontHeight - 4, ListLength~&(OS$) - VERTICALSCROLLOFFSET + 1)
                If VERTICALSCROLLOFFSET + I - 1 = SELECTED_SONG_ID And SELECTED_PLAYLIST_ID = OLD_PLAYLIST_ID Then Line (OFFSET, I * _FontHeight)-(OFFSET + _Width, (I + 1) * _FontHeight), _RGB32(0, 63, 127), BF
                _PrintString (OFFSET, I * _FontHeight), OneByteDecode$(ListGet$(ListGet$(OS$, VERTICALSCROLLOFFSET + I - 1), 1))
            Next I
            OFFSET = OFFSET - ease(OFFSET + _Width)
        End If
    Else
        For I = 1 To Min(_Height \ _FontHeight - 4, ListLength~&(S$) - VERTICALSCROLLOFFSET + 1)
            If VERTICALSCROLLOFFSET + I - 1 > ListLength~&(S$) Then _Continue
            If VERTICALSCROLLOFFSET + I - 1 = SELECTED_SONG_ID And SELECTED_PLAYLIST_ID = PLAYLIST_ID Then Line (1, I * _FontHeight)-(_Width, (I + 1) * _FontHeight), _RGB32(0, 63, 127), BF
            _PrintString (1, I * _FontHeight), OneByteDecode$(ListGet$(ListGet$(S$, VERTICALSCROLLOFFSET + I - 1), 1))
        Next I
    End If
    If Song_Handle Then
        SongSeek = _SndGetPos(Song_Handle) / _SndLen(Song_Handle)
        _PrintString ((_Width - (11 + Len(SELECTED_SONG_NAME$)) * _FontWidth) / 2, _Height - 2 * _FontHeight - 4), SELECTED_SONG_NAME$ + " " + Seconds2Time$(_SndGetPos(Song_Handle)) + "/" + Seconds2Time$(_SndLen(Song_Handle))
        Line (0, _Height - 4)-(_Width * SongSeek, _Height), -1, BF
        If _MouseButton(1) And InBox(0, _Height - 4, _MouseX, _MouseY, _Width, _Height) And Timer - LastPress > 0.1 Then
            _SndSetPos Song_Handle, _MouseX / _Width * _SndLen(Song_Handle)
            LastPress = Timer
        End If
        _PrintString (_Width - 4 * _FontWidth, _Height - _FontHeight - 4), _Trim$(Str$(Song_Volume))
        If _SndPlaying(Song_Handle) = 0 And _SndGetPos(Song_Handle) = _SndLen(Song_Handle) Then
            PlayNextSong
        End If
        If _KeyHit = 32 Then If _SndPaused(Song_Handle) Then _SndPlay Song_Handle Else _SndPause Song_Handle
    End If
    _Display
Loop Until Inp(&H60) = 1
System
Sub PlaySong (P, S)
    If PLAYLIST_ID <> P Or P$ = "" Then P$ = ListGet$(Parsed$, P): S$ = ListGet$(P$, 3)
    If S$ = "" Then S$ = ListGet$(P$, 3)
    If InRange(1, S, ListLength~&(S$)) = 0 Then Exit Sub
    If Song_Handle Then
        _SndStop Song_Handle
        _SndClose Song_Handle
    End If
    PLAYLIST_ID = P
    SELECTED_PLAYLIST_ID = PLAYLIST_ID
    SELECTED_SONG_ID = S
    SELECTED_SONG_NAME$ = OneByteDecode$(ListGet$(ListGet$(S$, SELECTED_SONG_ID), 1))
    Song_Handle = _SndOpen(OneByteDecode$(ListGet$(ListGet$(S$, SELECTED_SONG_ID), 2)))
    Song_Volume = Val(ListGet$(ListGet$(S$, SELECTED_SONG_ID), 3))
    _SndVol Song_Handle, Song_Volume / 100
    _SndPlay Song_Handle
End Sub
Sub PlayNextSong
    N_S_ID = SELECTED_SONG_ID + 1
    N_P_ID = SELECTED_PLAYLIST_ID
    If InRange(1, N_S_ID, ListLength~&(S$)) = 0 Then N_P_ID = N_P_ID + 1: N_S_ID = 1
    If InRange(1, N_P_ID, ListLength~&(Parsed$)) = 0 Then N_P_ID = 1
    PlaySong N_P_ID, N_S_ID
End Sub
Sub UI_Add (X1, Y1, X2, Y2, T$, BG&, HBG&, CBG&)
    I = UBound(UI_Elements) + 1
    ReDim _Preserve UI_Elements(1 To I) As UI_Element
    UI_Elements(I).X1 = X1: UI_Elements(I).Y1 = Y1: UI_Elements(I).X2 = X2: UI_Elements(I).Y2 = Y2: UI_Elements(I).Text = T$: UI_Elements(I).BG = BG&: UI_Elements(I).HBG = HBG&: UI_Elements(I).CBG = CBG&
    UI_Elements(I).TextScroll = 1: UI_Elements(I).ScrollDirection = 1
End Sub
Sub UI_Draw_All
    For I = LBound(UI_Elements) To UBound(UI_Elements)
        UI_Draw I
    Next I
End Sub
Sub UI_Draw (I)
    UI_Elements(I).Clicked = 0
    Select Case 2 * _MouseButton(1) + InBox(UI_Elements(I).X1, UI_Elements(I).Y1, _MouseX, _MouseY, UI_Elements(I).X2, UI_Elements(I).Y2)
        Case 0: C& = UI_Elements(I).BG
        Case -1: C& = UI_Elements(I).HBG
        Case -3: C& = UI_Elements(I).CBG: UI_Elements(I).Clicked = -1
    End Select
    Line (UI_Elements(I).X1, UI_Elements(I).Y1)-(UI_Elements(I).X2, UI_Elements(I).Y2), C&, BF
    T$ = Mid$(UI_Elements(I).Text, UI_Elements(I).TextScroll, (UI_Elements(I).X2 - UI_Elements(I).X1) \ _FontWidth)
    If Len(UI_Elements(I).Text) > Len(T$) Then
        If Int(Timer) <> UI_Elements(I).LastScroll Then
            UI_Elements(I).TextScroll = UI_Elements(I).TextScroll + UI_Elements(I).ScrollDirection
            UI_Elements(I).LastScroll = Timer
            If UI_Elements(I).TextScroll = Len(UI_Elements(I).Text) - Len(T$) + 1 Then UI_Elements(I).ScrollDirection = -1
            If UI_Elements(I).TextScroll = 1 Then UI_Elements(I).ScrollDirection = 1
        End If
    End If
    _PrintString ((UI_Elements(I).X1 + UI_Elements(I).X2 - Len(T$) * _FontWidth) / 2, (UI_Elements(I).Y1 + UI_Elements(I).Y2 - _FontHeight) / 2), T$
End Sub
Function InRange (A, B, C)
    InRange = (A <= B) And (B <= C)
End Function
Function InBox (X1, Y1, X, Y, X2, Y2)
    InBox = InRange(X1, X, X2) And InRange(Y1, Y, Y2)
End Function
Function Tokenizer$ (L$)
    Dim Token As String
    TokenList$ = ListNew$
    For I = 1 To Len(L$)
        C$ = Mid$(L$, I, 1)
        Select Case Asc(C$)
            Case 32
                If StringMode = 0 Then
                    If Token <> "" Then
                        TokenList$ = ListAdd$(TokenList$, Token)
                        Token = ""
                    End If
                Else
                    Token = Token + C$
                End If
            Case 34: StringMode = Not StringMode
                If StringMode Then
                    If Len(Token) Then TokenList$ = ListAdd$(TokenList$, Token)
                    Token = C$
                Else
                    Token = Token + C$
                End If
            Case 33, 40 To 45, 47, 58 To 64, 91 To 94, 96, 123 To 125
                If StringMode = 0 Then
                    If Len(Token) Then
                        TokenList$ = ListAdd$(TokenList$, Token)
                        Token = ""
                    End If
                    Token = C$
                    TokenList$ = ListAdd$(TokenList$, Token)
                    Token = ""
                Else
                    Token = Token + C$
                End If
            Case 46, 48 To 57
                If StringMode = 0 Then
                    If InStr("0123456789.", Right$(Token, 1)) Then
                        Token = Token + C$
                    Else
                        If Len(Token) Then TokenList$ = ListAdd$(TokenList$, Token)
                        Token = C$
                    End If
                Else
                    Token = Token + C$
                End If
            Case Else:
                If StringMode = 0 And Len(Token) Then
                    If InStr("0123456789.", Right$(Token, 1)) Then
                        TokenList$ = ListAdd$(TokenList$, Token)
                        Token = C$
                    Else
                        Token = Token + C$
                    End If
                Else
                    Token = Token + C$
                End If
        End Select
    Next I
    If Len(Token) Then TokenList$ = ListAdd$(TokenList$, Token)
    Tokenizer$ = TokenList$
End Function
Function Lexer$ (L$)
    LexDict$ = DictNew$
    Lexer$ = LexDict$
    I = 0: While I < ListLength~&(L$): I = I + 1
        V$ = ListGet$(L$, I)
        Select Case Asc(V$, 1)
            Case 34: T$ = "String"
            Case 65 To 90, 97 To 122: T$ = "Keyword"
            Case 46, 48 To 57: T$ = "Number"
            Case 33, 42, 43, 45, 92: T$ = "Operator"
            Case 47: T$ = "Operator"
            Case 60 To 62: T$ = "Relation"
            Case 39: T$ = "Line Seperator"
            Case 38, 124, 94: T$ = "BitwiseOperator"
            Case 126: T$ = "NotOperator"
            Case 40, 91, 123: T$ = "LeftBracket"
            Case 41, 93, 125: T$ = "RightBracket"
            Case 44: T$ = "Seperator"
        End Select
        LexDict$ = DictAdd$(LexDict$, T$, V$)
    Wend
    Lexer$ = LexDict$
End Function
Sub Parse (FILE$)
    Dim As _Unsigned _Byte MODE_PLAYLIST
    __F = FreeFile
    Open FILE$ For Input As #__F
    O$ = ListNew$
    Do
        LINENUMBER = LINENUMBER + 1
        Line Input #__F, L$
        L$ = Lexer$(Tokenizer$(L$))
        Select Case DictGetKey$(L$, DictLength~&(L$))
            Case "Number"
                T$ = ListAdd$(ListNew$, OneByteEncode$(DoubleTrim$(DictGetValue$(L$, 1), 1)))
                T$ = ListAdd$(T$, OneByteEncode$(DoubleTrim$(DictGetValue$(L$, 2), 1)))
                T$ = ListAdd$(T$, DictGetValue$(L$, 3))
                T$ = ListAdd$(T$, "0")
                T$ = ListAdd$(T$, DictGetValue$(L$, 4))
                S$ = ListAdd$(S$, T$)
                Print "    Registering Song: "; OneByteDecode$(ListGet$(T$, 1))
            Case "LeftBracket": If MODE_PLAYLIST = 0 Then MODE_PLAYLIST = 1 Else Print "Error at Line"; LINENUMBER: System
                P$ = ListAdd$(ListNew$, DoubleTrim$(DictGetValue$(L$, 1), 1))
                P$ = ListAdd$(P$, DoubleTrim$(DictGetValue$(L$, 2), 1))
                S$ = ListNew$
                Print "List Start: "; DictGetValue$(L$, 1)
            Case "RightBracket": If MODE_PLAYLIST = 1 Then MODE_PLAYLIST = 0 Else Print "Error at Line"; LINENUMBER: System
                O$ = ListAdd$(O$, ListAdd$(P$, S$))
        End Select
    Loop Until EOF(__F)
    Close #__F
    Parsed$ = O$
    'list(list(list))
End Sub
Sub SaveParse (FILE$)
    If _FileExists(FILE$) Then Kill FILE$
    __F = FreeFile
    Open FILE$ For Binary As #__F
    Put #__F, , Parsed$
    Close #__F
End Sub
Function Min (A, B)
    Min = -A * (A < B) - B * (A >= B)
End Function
Function Max (A, B)
    Max = -A * (A > B) - B * (A <= B)
End Function
Function ease& (N)
    If Abs(N) >= 4 Then ease = N \ 4 Else ease = Sgn(N)
End Function
Function Seconds2Time$ (T As Integer)
    Minutes$ = _Trim$(Str$(T \ 60))
    Minutes$ = String$(2 - Len(Minutes$), 48) + Minutes$
    Seconds$ = _Trim$(Str$(T Mod 60))
    Seconds$ = String$(2 - Len(Seconds$), 48) + Seconds$
    Seconds2Time$ = Minutes$ + ":" + Seconds$
End Function
'$Include:'OneByteEncoder.bas'
'$Include:'files\QBListDict.bas'
