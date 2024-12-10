$Console:Only
Dim Shared PWD_CRC~&
Type DATTYPE
    CRC As _Unsigned Long
    Identifier As String * 128
    Password As String * 128
End Type

Dim DAT(1 To 64) As DATTYPE
Dim Identifier As String * 128, Password As String * 128

If _CommandCount = 0 Then GoTo HELP

Select Case Command$(2)
    Case "--add": MODE = 1: Identifier = Command$(3): Password = Command$(4)
    Case "--edit": MODE = 2: Position = Val(Command$(3)): Identifier = Command$(4): Password = Command$(5)
    Case "--insert": MODE = 3: Position = Val(Command$(3)): Identifier = Command$(4): Password = Command$(5)
    Case "--list": MODE = 4
    Case "--delete": MODE = 5: Position = Val(Command$(3))
End Select

PWD_CRC~& = CRC32(Command$(1))

Print "Reading Password Database"
Open "pwd_data" For Binary As #1
FILEPOSITION = Seek(1)
Get #1, , DAT()

Empty_Position = 0
For I = 1 To 64
    If DAT(I).Identifier = String$(128, 0) And DAT(I).Password = String$(128, 0) Then Empty_Position = I: Exit For
Next I

Select Case MODE
    Case 1: 'Add
        If Empty_Position = 0 Then Print "No More Passwords can be added.": System
        For J = 1 To 8
            Mid$(DAT(Empty_Position).Identifier, J * 16 - 15, 16) = ENCRYPT$(Mid$(Identifier, J * 16 - 15, 16))
            Mid$(DAT(Empty_Position).Password, J * 16 - 15, 16) = ENCRYPT$(Mid$(Password, J * 16 - 15, 16))
        Next J
        DAT(Empty_Position).CRC = PWD_CRC~&
        Print "Added at Position:"; Empty_Position
    Case 2: 'Edit
        If DAT(Position).CRC <> PWD_CRC~& Then Print "Wrong Password": System
        For J = 1 To 8
            Mid$(DAT(Position).Identifier, J * 16 - 15, 16) = ENCRYPT$(Mid$(Identifier, J * 16 - 15, 16))
            Mid$(DAT(Position).Password, J * 16 - 15, 16) = ENCRYPT$(Mid$(Password, J * 16 - 15, 16))
        Next J
        DAT(Position).CRC = PWD_CRC~&
        Print "Edited at Position:"; Position
    Case 3: 'Insert
        For J = 63 To Position Step -1
            Swap DAT(J), DAT(J + 1)
        Next J
        For J = 1 To 8
            Mid$(DAT(Position).Identifier, J * 16 - 15, 16) = ENCRYPT$(Mid$(Identifier, J * 16 - 15, 16))
            Mid$(DAT(Position).Password, J * 16 - 15, 16) = ENCRYPT$(Mid$(Password, J * 16 - 15, 16))
        Next J
        DAT(Position).CRC = PWD_CRC~&
        Print "Inserted at Position:"; Position
    Case 4: 'List
        For I = 1 To 64
            If DAT(I).Identifier <> String$(128, 0) Or DAT(I).Password <> String$(128, 0) Then Total_Entries = Total_Entries + 1 Else _Continue
            For J = 1 To 8
                Mid$(Identifier, J * 16 - 15, 16) = DECRYPT$(Mid$(DAT(I).Identifier, J * 16 - 15, 16))
                Mid$(Password, J * 16 - 15, 16) = DECRYPT$(Mid$(DAT(I).Password, J * 16 - 15, 16))
            Next J
            If DAT(I).CRC = PWD_CRC~& Then Print I; ":", _Trim$(Identifier); ":"; _Trim$(Password) Else Print I; ": Incorrect Password"
        Next I
        Print "Total Entries:"; Total_Entries
    Case 5: 'Delete
        If DAT(Position).CRC <> PWD_CRC~& Then Print "Wrong Password": System
        For J = Position + 1 To 64
            Swap DAT(J), DAT(J - 1)
        Next J
        DAT(64).CRC = 0
        DAT(64).Identifier = String$(128, 0)
        DAT(64).Password = String$(128, 0)
        Print "Deleted from Position:"; Position
End Select
Seek #1, FILEPOSITION
Put #1, , DAT()
Close
System
HELP:
Print "pwdmgr [Password] [Options] (Position) (Identifier) (Password)"
Print "Options:"
Print "    --add (Identifier) (Password)                 Add Entry"
Print "    --edit (Position) (Identifier) (Password)     Edit Entry"
Print "    --insert (Position) (Identifier) (Password)   Insert Entry"
Print "    --list                                        List All Entries"
Print "    --delete (Position)                           Delete Entry"
System
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
'$Include:'EncryptDecrypt.bm'
