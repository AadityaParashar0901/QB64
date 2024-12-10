$Console:Only
_Define A-Z As _UNSIGNED LONG
Type FileType '329 Bytes
    Name As String * 256
    Type As String * 1
    Exists As _Unsigned _Byte
    Directory As String * 64
    Directory_Hash As _Unsigned Long
    Password_Hash As _Unsigned Long
    File_Hash As _Unsigned Long
    Length As _Unsigned Long
End Type
Dim Shared File As FileType, FileCount As _Unsigned Long, FileName As String, FileDirectory As String
Dim Shared As String Password, SVFile, Mode, Directory

If _CommandCount < 2 Then GoTo HELP
Y = CsrLin
Password = Command$(1)
SVFile = Command$(2)
Mode = Command$(3)
Directory = Command$(4)
Select Case LCase$(Mode$)
    Case "cd", "create-dir": GoTo SV_CREATE_DIR
    Case "dd", "delete-dir": GoTo SV_DELETE_DIR
    Case "a", "add": GoTo SV_ADD
    Case "l", "list": GoTo SV_LIST
    Case "e", "extract": GoTo SV_EXTRACT
    Case "t", "test": GoTo SV_TEST
    Case "rl", "recover-list": GoTo SV_RECOVER_LIST
    Case "rd", "recover-dir": GoTo SV_RECOVER_DIR
    Case "rf", "recover-file": GoTo SV_RECOVER_FILE
    Case "d", "delete": GoTo SV_DELETE
    Case Else: GoTo HELP
End Select
HELP:
Print "SV"
Print "Usage: " + Command$(0) + " [Password] [SV File] [Mode]"
Print "Mode: Default = List Files"
Print "1.  cd create-dir   - Create Directory"
Print "    cd [Directory Name]"
Print "2.  dd delete-dir   - Delete Directory"
Print "    dd [Directory Name]"
Print "3.  a add           - Add Files"
Print "    a [Directory Name] [Files]"
Print "4.  l list          - List Files"
Print "    l [Directory Name"
Print "5.  e extract       - Extract Files"
Print "    e [Directory Name]"
Print "6.  t test          - Test Files"
Print "    t [Directory Name] [Files]"
Print "7.  d delete        - Delete Files"
Print "    d [Directory Name] [Files]"
Print "8.  rl recover-list - List Deleted Files"
Print "    rl [Directory Name]"
Print "9.  rd recover-dir  - Recover Directory"
Print "    rl [Directory Name]"
Print "10. rf recover-file - Recover File"
Print "    rf [Directory Name] [Files]"
System

SV_CREATE_DIR:
If Directory = "root" Then Print "Directory Exists": System
CheckSignature SVFile$
For I = 1 To FileCount
    Get #1, , File
    If File.Type = "D" Then
        If DecryptFileDetails And _StriCmp(Directory, FileName) = 0 And File.Exists Then
            Print "Directory Exists"
            System
        End If
    End If
    Seek #1, Seek(1) + Rem64~&(File.Length)
Next I
IncrementFileCount
Seek #1, LOF(1) + 1
File.Name = Directory
File.Type = "D"
File.Exists = 255
File.Directory = "root"
File.Directory_Hash = &H16F4F95B
File.Length = 0
EncryptFileDetails
Put #1, , File
Print "Created Directory "; Directory
System

SV_DELETE_DIR:
CheckSignature SVFile$
For I = 1 To FileCount
    S~& = Seek(1)
    Get #1, , File
    If File.Type = "D" Then
        If DecryptFileDetails And Directory = FileName And &H16F4F95B = File.Directory_Hash And File.Exists Then
            EncryptFileDetails
            File.Exists = 0
            Seek #1, S~&
            Put #1, , File
            Print "Deleted Directory "; Directory
            System
        End If
    End If
    Seek #1, Seek(1) + Rem64~&(File.Length)
Next I
Print "Directory "; Directory; " doesn't exists!"
System

SV_ADD:
CheckSignature SVFile$
If Directory <> "root" Then
    For I = 1 To FileCount
        Get #1, , File
        If File.Type = "D" Then
            If DecryptFileDetails And _StriCmp(Directory, FileName) = 0 And File.Exists Then
                Exit For
            End If
        End If
        Seek #1, Seek(1) + Rem64~&(File.Length)
    Next I
    If _StriCmp(Directory, FileName) Then Print "Directory "; Directory; " doesn't exists!": System
End If
For I = 5 To _CommandCount
    Y = CsrLin
    Print "Adding File:"; Command$(I)
    IncrementFileCount
    Seek #1, LOF(1) + 1
    S~& = Seek(1)
    File.Name = Command$(I)
    File.Type = "F"
    File.Exists = 255
    File.Directory = Directory
    File.Directory_Hash = crc32~&(Directory)
    Open Command$(I) For Binary Lock Write As #2
    File.Length = LOF(2)
    EncryptFileDetails
    Put #1, , File
    CRC = -1
    Do
        If LOF(2) - Seek(2) >= 1048575 Then FI$ = String$(1048576, 0) Else FI$ = String$(Rem64~&(LOF(2) - Seek(2) + 1), 0)
        Get #2, , FI$
        'Calculate CRC32
        For K = 1 To Len(FI$)
            CRC = CRC Xor Asc(FI$, K)
            For J = 1 To 8
                If CRC And 1 Then CRC = _SHR(CRC, 1) Xor &HEDB88320 Else CRC = _SHR(CRC, 1)
            Next J
        Next K
        '---------------
        FO$ = ENC$(FI$, Password)
        Put #1, , FO$
        If LOF(2) < Seek(2) Then Exit Do 'If LOF(2) <= Seek(2) - 1 Then Exit Do
    Loop
    File.File_Hash = Not (CRC)
    Seek #1, S~&
    Put #1, , File
    Close #2
    Print "Added File "; Command$(I)
Next I
System

SV_LIST:
CheckSignature SVFile$
For I = 1 To FileCount
    Get #1, , File
    If InStr("FD", File.Type) And File.Exists Then
        If crc32~&(Directory) = File.Directory_Hash Then
            If DecryptFileDetails Then
                If File.Type = "D" Then Color 2, 0
                Print File.Type; ","; File.Length; ":"; FileName
                Color 15, 0
            Else
                Color 7, 4
                Print "Wrong Password"
                Color 15, 0
            End If
        End If
    End If
    Seek #1, Seek(1) + Rem64~&(File.Length)
Next I
System

SV_EXTRACT:
CheckSignature SVFile$
For I = 1 To FileCount
    Get #1, , File
    If File.Type = "F" And crc32~&(Directory) = File.Directory_Hash And File.Exists Then
        If DecryptFileDetails Then
            FileExtract = 0
            If _CommandCount > 4 Then
                For J = 5 To _CommandCount
                    If FileName = Command$(J) Then FileExtract = -1: Exit For
                Next J
            Else
                FileExtract = -1
            End If
            If FileExtract Then
                OUTFILE$ = FileName
                Do
                    If _FileExists(_StartDir$ + "\" + OUTFILE$) Then OUTFILE$ = "_" + OUTFILE$ Else Exit Do
                Loop
                Print "Extracting:"; FileName; " to "; OUTFILE$; ":";
                Open OUTFILE$ For Binary Lock Write As #2
                CRC = -1
                II = 1
                Do
                    If Rem64~&(File.Length) - II + 1 >= 1048576 Then FI$ = String$(1048576, 0) Else FI$ = String$(Rem64~&(File.Length) - II + 1, 0)
                    II = II + Len(FI$)
                    Get #1, , FI$
                    FO$ = DEC$(FI$, Password)
                    'Calculate CRC32
                    For K = 1 To Len(FO$)
                        CRC = CRC Xor Asc(FO$, K)
                        For J = 1 To 8
                            If CRC And 1 Then CRC = _SHR(CRC, 1) Xor &HEDB88320 Else CRC = _SHR(CRC, 1)
                        Next J
                    Next K
                    '---------------
                    If File.Length - LOF(2) < 1048575 Then FO$ = Left$(FO$, File.Length Mod 1048576)
                    Put #2, , FO$
                    If Rem64~&(File.Length) < II Then Exit Do 'If File.Length <= II - 1 Then Exit Do
                Loop
                Close #2
                If File.File_Hash = Not (CRC) Then Print "Healthy File" Else Print "Corrupted File"
            End If
        Else
            Seek #1, Seek(1) + Rem64~&(File.Length)
        End If
    Else
        Seek #1, Seek(1) + Rem64~&(File.Length)
    End If
Next I
System

SV_TEST:
CheckSignature SVFile$
For I = 1 To FileCount
    Get #1, , File
    If File.Type = "F" And crc32~&(Directory) = File.Directory_Hash And File.Exists Then
        If DecryptFileDetails Then
            FileTest = 0
            If _CommandCount > 4 Then
                For J = 5 To _CommandCount
                    If FileName = Command$(J) Then FileTest = -1: Exit For
                Next J
            Else
                FileTest = -1
            End If
            Print "Testing:"; FileName; ":";
            If FileTest Then
                CRC = -1
                II = 1
                Do
                    If Rem64~&(File.Length) - II + 1 >= 1048576 Then FI$ = String$(1048576, 0) Else FI$ = String$(Rem64~&(File.Length - II + 1), 0)
                    II = II + Len(FI$)
                    Get #1, , FI$
                    FO$ = DEC$(FI$, Password)
                    'Calculate CRC32
                    For K = 1 To Len(FO$)
                        CRC = CRC Xor Asc(FO$, K)
                        For J = 1 To 8
                            If CRC And 1 Then CRC = _SHR(CRC, 1) Xor &HEDB88320 Else CRC = _SHR(CRC, 1)
                        Next J
                    Next K
                    '---------------
                    If Rem64~&(File.Length) < II Then Exit Do 'If File.Length <= II - 1 Then Exit Do
                Loop
                If File.File_Hash = Not (CRC) Then Print "Healthy File" Else Print "Corrupted File"
            End If
        Else
            Seek #1, Seek(1) + Rem64~&(File.Length)
        End If
    Else
        Seek #1, Seek(1) + Rem64~&(File.Length)
    End If
Next I
System

SV_RECOVER_LIST:
CheckSignature SVFile$
For I = 1 To FileCount
    Get #1, , File
    If InStr("FD", File.Type) And File.Exists = 0 Then
        If crc32~&(Directory) = File.Directory_Hash Then
            If DecryptFileDetails Then
                If File.Type = "D" Then Color 2, 0
                Print File.Type; ","; File.Length; ":"; FileName
                Color 15, 0
            Else
                Color 7, 4
                Print "Wrong Password"
                Color 15, 0
            End If
        End If
    End If
    Seek #1, Seek(1) + Rem64~&(File.Length)
Next I
System

SV_RECOVER_DIR:
CheckSignature SVFile$
For I = 1 To FileCount
    S~& = Seek(1)
    Get #1, , File
    If File.Type = "D" Then
        If DecryptFileDetails And Directory = FileName And &H16F4F95B = File.Directory_Hash And File.Exists = 0 Then
            EncryptFileDetails
            File.Exists = 255
            Seek #1, S~&
            Put #1, , File
            Print "Recovered Directory "; Directory
            System
        End If
    End If
    Seek #1, Seek(1) + Rem64~&(File.Length)
Next I
Print "Deleted Directory "; Directory; " doesn't exists!"
System

SV_RECOVER_FILE:
CheckSignature SVFile$
For I = 1 To FileCount
    S~& = Seek(1)
    Get #1, , File
    If File.Type = "F" And File.Exists = 0 Then
        If crc32~&(Directory) = File.Directory_Hash Then
            If DecryptFileDetails Then
                FileRecover = 0
                If _CommandCount > 4 Then
                    For J = 5 To _CommandCount
                        If FileName = Command$(J) Then FileRecover = -1: Exit For
                    Next J
                Else
                    FileRecover = -1
                End If
                If FileRecover Then
                    File.Exists = 255
                    EncryptFileDetails
                    Seek #1, S~&
                    Put #1, , File
                    Print "Recovered File "; FileName
                End If
            End If
        End If
    End If
    Seek #1, Seek(1) + Rem64~&(File.Length)
Next I
System

SV_DELETE:
CheckSignature SVFile$
For I = 1 To FileCount
    S~& = Seek(1)
    Get #1, , File
    If File.Type = "F" And File.Exists Then
        If crc32~&(Directory) = File.Directory_Hash Then
            If DecryptFileDetails Then
                FileDelete = 0
                If _CommandCount > 4 Then
                    For J = 5 To _CommandCount
                        If FileName = Command$(J) Then FileDelete = -1: Exit For
                    Next J
                Else
                    FileDelete = -1
                End If
                If FileDelete Then
                    File.Exists = 0
                    EncryptFileDetails
                    Seek #1, S~&
                    Put #1, , File
                    Print "Deleted File "; FileName
                End If
            End If
        End If
    End If
    Seek #1, Seek(1) + Rem64~&(File.Length)
Next I
System

Sub CheckSignature (FILE$)
    Open FILE$ For Binary Lock Write As #1
    SIGN$4 = "SV" + Chr$(0) + Chr$(1)
    If LOF(1) > 4 Then
        Get #1, , S$4
        If S$4 <> SIGN$4 Then
            Print "File is not SV File."
            System
        Else
            GetFileCount
        End If
    ElseIf LOF(1) = 0 Then
        Put #1, , SIGN$4
        FileCount = 0: SetFileCount
    End If
    Seek #1, 9
End Sub
Sub EncryptFileDetails
    File.Name = ENC$(File.Name, Password)
    File.Directory = ENC$(File.Directory, Password)
    File.Password_Hash = crc32~&(Password)
End Sub
Function DecryptFileDetails
    If crc32~&(Password) = File.Password_Hash Then
        File.Name = DEC$(File.Name, Password)
        File.Directory = DEC$(File.Directory, Password)
        FileName = _Trim$(File.Name)
        FileDirectory = _Trim$(File.Directory)
        DecryptFileDetails = -1
    Else
        FileName = ""
        FileDirectory = ""
    End If
End Function
Sub GetFileCount
    __S~& = Seek(1)
    Seek #1, 5
    Get #1, , FileCount
    Seek #1, __S~&
End Sub
Sub SetFileCount
    __S~& = Seek(1)
    Seek #1, 5
    Put #1, , FileCount
    Seek #1, __S~&
End Sub
Sub IncrementFileCount
    GetFileCount
    FileCount = FileCount + 1
    SetFileCount
End Sub
Sub DecrementFileCount
    GetFileCount
    FileCount = FileCount - 1
    SetFileCount
End Sub
Function ENC$ (I$, P$)
    PFC$64 = FC$64(P$)
    PARTS = _SHR(Rem64~&(Len(I$)), 6)
    O$ = I$ + String$(PARTS * 64 - Len(I$), 0)
    For I~& = 1 To PARTS: Mid$(O$, I~& * 64 - 63, 64) = Encrypt$(Mid$(O$, I~& * 64 - 63, 64), PFC$64): Next I~&
    ENC$ = O$
    O$ = ""
End Function
Function DEC$ (I$, P$)
    PFC$64 = FC$64(P$)
    PARTS = Len(I$) \ 64
    O$ = I$
    For I~& = 1 To PARTS: Mid$(O$, I~& * 64 - 63, 64) = Decrypt$(Mid$(O$, I~& * 64 - 63, 64), PFC$64): Next I~&
    DEC$ = O$
    O$ = ""
End Function
Function Encrypt$64 (__I$64, __P$64)
    Static As Long I, II, IJ, JI, JJ, J
    Static DataOut$64
    Static As _Unsigned _Byte DataMatrix(1 To 8, 1 To 8), PasswordMatrix(1 To 8, 1 To 8), HashMatrix(1 To 8, 1 To 8), ConstantMatrix(1 To 8, 1 To 8), OutMatrix(1 To 8, 1 To 8)
    Static FirstRun As _Bit
    MemCopy _Offset(__I$64), _Offset(DataMatrix()), 64
    MemCopy _Offset(__P$64), _Offset(PasswordMatrix()), 64
    If FirstRun = 0 Or __OLDP$64 <> __P$64 Then
        For I = 1 To 8: For J = 1 To 8
                ConstantMatrix(I, J) = Hash~%%(PasswordMatrix(I, J) + 127): HashMatrix(I, J) = Hash~%%(ConstantMatrix(I, J) Xor PasswordMatrix(I, J)) + 127
        Next J, I
        __OLDP$64 = __P$64
        FirstRun = -1
    End If
    'Shuffle Data Matrix
    For I = 0 To 63
        II = 1 + I \ 8: IJ = 1 + (I Mod 8)
        JI = 1 + (63 - I) \ 8: JJ = 1 + ((63 - I) Mod 8)
        Swap DataMatrix(II, IJ), DataMatrix(JI, JJ)
    Next I
    'Generate Constant, Hash Matrices & Encrypt
    For I = 1 To 8: For J = 1 To 8
            OutMatrix(I, J) = (DataMatrix(I, J) Xor HashMatrix(I, J)) + ConstantMatrix(I, J)
    Next J, I
    MemCopy _Offset(OutMatrix()), _Offset(DataOut$64), 64
    Encrypt$64 = DataOut$64
End Function
Function Decrypt$64 (__I$64, __P$64)
    Static As Long I, II, IJ, JI, JJ, J
    Static DataOut$64
    Static As _Unsigned _Byte DataMatrix(1 To 8, 1 To 8), PasswordMatrix(1 To 8, 1 To 8), HashMatrix(1 To 8, 1 To 8), ConstantMatrix(1 To 8, 1 To 8), OutMatrix(1 To 8, 1 To 8)
    Static FirstRun As _Bit
    MemCopy _Offset(__I$64), _Offset(OutMatrix()), 64
    MemCopy _Offset(__P$64), _Offset(PasswordMatrix()), 64
    If FirstRun = 0 Or __OLDP$64 <> __P$64 Then
        For I = 1 To 8: For J = 1 To 8
                ConstantMatrix(I, J) = Hash~%%(PasswordMatrix(I, J) + 127): HashMatrix(I, J) = Hash~%%(ConstantMatrix(I, J) Xor PasswordMatrix(I, J)) + 127
        Next J, I
        __OLDP$64 = __P$64
        FirstRun = -1
    End If
    'Generate Constant, Hash Matrices & Decrypt
    For I = 1 To 8: For J = 1 To 8
            DataMatrix(I, J) = (OutMatrix(I, J) - ConstantMatrix(I, J)) Xor HashMatrix(I, J)
    Next J, I
    'UnShuffle Data Matrix
    For I = 0 To 63
        II = 1 + I \ 8: IJ = 1 + (I Mod 8)
        JI = 1 + (63 - I) \ 8: JJ = 1 + ((63 - I) Mod 8)
        Swap DataMatrix(II, IJ), DataMatrix(JI, JJ)
    Next I
    MemCopy _Offset(DataMatrix()), _Offset(DataOut$64), 64
    Decrypt$64 = DataOut$64
End Function
Function Hash~%% (X As _Unsigned _Integer64)
    Hash~%% = _SHL(X And 15, 4) Or _SHR(X And 240, 4)
End Function
Function Rem64~& (A~&)
    B~%% = A~& Mod 64
    Rem64~& = A~& + 64 * Sgn(B~%%) - (B~%%)
End Function
Sub MemCopy (__S As _Offset, __D As _Offset, __SIZE As _Unsigned Long)
    Dim As _MEM __M1, __M2
    __M1 = _Mem(__S, __SIZE): __M2 = _Mem(__D, __SIZE)
    _MemCopy __M1, __M1.OFFSET, __M1.SIZE To __M2, __M2.OFFSET
    _MemFree __M1: _MemFree __M2
End Sub
Function FC$64 (__I$)
    Restore HASHCONSTANTS
    For __I = 1 To 16
        Read __POLY~&
        Mid$(H$64, __I * 4 - 3, 4) = MKL$(c32~&(__I$, __POLY~&))
    Next __I
    FC$64 = H$64
    Exit Function
    HASHCONSTANTS:
    Data &H04c11db7,&Hedb88320,&Hdb710641,&H82608edb,&H1edc6f41,&H82f63b78,&H05ec76f1,&H8f6e37a0
    Data &H741b8cd7,&Heb31d82e,&Hd663b05d,&Hba0dc66b,&H814141ab,&Hd5828281,&Hab050503,&Hc0a0a0d5
End Function
Function c32~& (__IN$, __CRC32_POLY As _Unsigned Long)
    Dim As _Unsigned Long __CRC, __I
    Dim As _Unsigned _Byte __J
    __CRC = &HFFFFFFFF
    For __I = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(__IN$, __I)
        For __J = 1 To 8
            If __CRC And 1 Then __CRC = (__CRC \ 2) Xor __CRC32_POLY Else __CRC = __CRC \ 2
        Next __J
    Next __I
    c32~& = Not __CRC
End Function
Function LongToHex$8 (A As _Unsigned Long)
    H$ = Hex$(A)
    LongToHex$8 = String$(8 - Len(H$), "0") + H$
End Function
Function crc32~& (__IN$)
    Dim As _Unsigned Long __CRC32_POLY, __CRC, __I
    Dim As _Unsigned _Byte __J
    __CRC32_POLY = &HEDB88320
    __CRC = &HFFFFFFFF
    For __I = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(__IN$, __I)
        For __J = 1 To 8
            If __CRC And 1 Then __CRC = (__CRC \ 2) Xor __CRC32_POLY Else __CRC = __CRC \ 2
        Next __J
    Next __I
    crc32~& = Not __CRC
End Function
