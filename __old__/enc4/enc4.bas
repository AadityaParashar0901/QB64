$Console:Only
If _CommandCount = 0 Then System
ENCRYPTEDFILENAME$ = Command$(1)
Open ENCRYPTEDFILENAME$ For Binary As #1
Password$ = Command$(2)
If _CommandCount = 2 Or Command$(3) = "l" Then GoTo LISTFILES
If Command$(3) = "a" Then GoTo ADDFILES
If Command$(3) = "e" Then GoTo EXTRACTFILES
If Command$(3) = "t" Then GoTo TESTFILES
If Command$(3) = "cp" Then GoTo CHANGEPASSWORD
HELP:
Print "ENC4 [ARCHIVE NAME] [PASSWORD] [OPTION] [FILES]"
Print "OPTIONS:"
Print "a    Add"
Print "e    Extract"
Print "l    List"
Print "t    Test"
Print "cp   Change Password"
System
ADDFILES:
For I = 4 To _CommandCount
    FS = _InStrRev(Command$(I), "\")
    FN$ = Command$(I)
    If FS Then FN$ = Mid$(Command$(I), FS + 1)
    Print "Adding File:"; Chr$(34); FN$; Chr$(34)
    Open Command$(I) For Binary As #2
    F$ = String$(LOF(2), 0)
    Get #2, , F$
    Seek #1, LOF(1) + 1
    FN$ = ED$(FN$, Password$)
    E$ = Chr$(Len(FN$)) + FN$ + MKL$(CRC32(F$)) + MKL$(LOF(2)) + ED$(F$, Password$)
    Put #1, , E$
    F$ = ""
    Close #2
Next I
System
LISTFILES:
If LOF(1) = 0 Then Print "No Files": System
Do
    Get #1, , FNL$1
    If FNL$1 = Chr$(0) Then Exit Do
    FN$ = String$(Asc(FNL$1), 0)
    Get #1, , FN$
    FN$ = ED$(FN$, Password$)
    Print FN$
    Get #1, , FCRC32$4
    Get #1, , FS$4
    Seek #1, Seek(1) + CVL(FS$4)
Loop
System
EXTRACTFILES:
If LOF(1) = 0 Then Print "No Files": System
Do
    Get #1, , FNL$1
    If FNL$1 = Chr$(0) Then Exit Do
    FN$ = String$(Asc(FNL$1), 0)
    Get #1, , FN$
    FN$ = ED$(FN$, Password$)
    If _CommandCount = 3 Then EXTRACT = -1 Else EXTRACT = 0
    For I = 1 To _CommandCount
        If FN$ = Command$(I) Then EXTRACT = -1: Exit For
    Next I
    If EXTRACT Then Print "Extracting:"; FN$;
    Get #1, , FCRC32$4
    Get #1, , FS$4
    F$ = String$(CVL(FS$4), 0)
    Get #1, , F$
    F$ = ED$(F$, Password$)
    If EXTRACT Then
        If CVL(FCRC32$4) <> CRC32(F$) Then Print ": Corrupted File" Else Print ": Healthy File"
        Do
            If _FileExists(_StartDir$ + "\" + FN$) Then
                FN$ = "_" + FN$
            Else
                Exit Do
            End If
        Loop
        Open _StartDir$ + "\" + FN$ For Binary As #2
        Put #2, , F$
        Close #2
    End If
Loop
System
TESTFILES:
If LOF(1) = 0 Then Print "No Files": System
Do
    Get #1, , FNL$1
    If FNL$1 = Chr$(0) Then Exit Do
    FN$ = String$(Asc(FNL$1), 0)
    Get #1, , FN$
    FN$ = ED$(FN$, Password$)
    Print "Testing:"; FN$;
    Get #1, , FCRC32$4
    Get #1, , FS$4
    F$ = String$(CVL(FS$4), 0)
    Get #1, , F$
    F$ = ED$(F$, Password$)
    If CVL(FCRC32$4) <> CRC32(F$) Then Print ": Corrupted File" Else Print ": Healthy File"
Loop
System
CHANGEPASSWORD:
If LOF(1) = 0 Then Print "No Files": System
Do
    Get #1, , FNL$1
    If FNL$1 = Chr$(0) Then Exit Do
    FN$ = String$(Asc(FNL$1), 0)
    Get #1, , FN$
    FN$ = ED$(FN$, Password$)
    Print "Testing:"; FN$;
    Get #1, , FCRC32$4
    Get #1, , FS$4
    F$ = String$(CVL(FS$4), 0)
    Get #1, , F$
    F$ = ED$(F$, Password$)
    If CVL(FCRC32$4) <> CRC32(F$) Then
        Print ": Corrupted File"
        CF = CF + 1
    Else Print ": Healthy File"
    End If
Loop
If CF Then Print "CANNOT CHANGE PASSWORD": System
NewPassword$ = Command$(4)
Seek #1, 1
Do
    Get #1, , FNL$1
    If FNL$1 = Chr$(0) Then Exit Do
    LASTSEEK& = Seek(1)
    FN$ = String$(Asc(FNL$1), 0)
    Get #1, , FN$
    FN$ = ED$(FN$, Password$)
    Print "Changing Password:"; FN$
    Get #1, , FCRC32$4
    Get #1, , FS$4
    F$ = String$(CVL(FS$4), 0)
    Get #1, , F$
    F$ = ED$(F$, Password$)
    Seek #1, LASTSEEK&
    FN$ = ED$(FN$, NewPassword$)
    E$ = FN$ + MKL$(CRC32(F$)) + FS$4 + ED$(F$, NewPassword$)
    Put #1, , E$
    F$ = ""
    Close #2
Loop
System
Function ED$ (I$, P$)
    O$ = I$
    P~& = CRC32(P$): PCRC$ = MKL$(P~&)
    For I& = 1 To Len(I$)
        Asc(O$, I&) = Asc(I$, I&) Xor Asc(PCRC$, I& Mod 4 + 1)
        If I& Mod 4 = 0 Then P~& = P~& + 1: PCRC$ = MKL$(P~&)
    Next I&
    ED$ = O$
End Function
Function CRC32~& (__IN$)
    Dim As _Unsigned Long __CRC32_POLY, __CRC
    __CRC32_POLY = &HEDB88320
    __CRC = &HFFFFFFFF
    For __I& = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(Mid$(__IN$, __I&, 1))
        For __J = 1 To 8
            If __CRC And 1 Then
                __CRC = (__CRC \ 2) Xor __CRC32_POLY
            Else
                __CRC = __CRC \ 2
            End If
        Next __J
    Next __I&
    CRC32~& = Not __CRC
End Function
