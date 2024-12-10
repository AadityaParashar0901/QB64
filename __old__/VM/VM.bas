$Console
Screen _NewImage(640, 480, 32)
If _CommandCount = 0 Then System
Open "VM_log.txt" For Output As #10
Print #10, "RA", "RB", "RC", "RD", "RE", "RF", "RG", "RH", "R1", "R2", "SA", "SB", "SC", "Flags"
Dim Shared Memory As String * 65536
Dim Shared As _Unsigned Long R(1 To 8), R1, R2, R3
Dim Shared As String SA, SB, SC, SD
Dim Shared As _Bit FZ, FC, FE, FL, FG, FMB1, FMB2, FMB3, FMSU, FMSD
'RA, RB         - 32_Bit Unsigned Numbers
'RC, RD         - 32_Bit Unsigned Numbers, Mouse Pointers
'RE, RF         - String Pointers
'RG             - Instruction Pointers
'RH             - Key Pointers
R(7) = 0

Dim As _Unsigned _Byte memByte
Open Command$(1) For Binary As 1
Get #1, , Memory
If _CommandCount > 8 Then MaxFiles = 8 Else MaxFiles = _CommandCount
For I = 2 To MaxFiles
    Open Command$(I) For Binary As I
Next I
_Title "VM"
Do
    _Limit 2 ^ 16
    R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
    Print #10, Hex$(R(1)), Hex$(R(2)), Hex$(R(3)), Hex$(R(4)), Hex$(R(5)), Hex$(R(6)), Hex$(R(7)), Hex$(R(8)), Hex$(R1), Hex$(R2), Chr$(34); SA; Chr$(34), Chr$(34); SB; Chr$(34), Chr$(34); SC; Chr$(34), FZ, FC, FE, FL, FG
    On Error GoTo eH
    FMSU = 0
    FMSD = 0
    While _MouseInput
        FMSU = _MouseWheel > 0
        FMSD = _MouseWheel < 0
    Wend
    Select Case memByte
        Case &H00:
        Case &H01:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) + R(R2)
        Case &H02:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(R1) = R(R1) + Asc(Memory, R2)
        Case &H03:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) + R2
        Case &H04:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            memByte = Asc(Memory, R1) + memByte
            If memByte < R2 Then FC = -1
            Asc(Memory, R1) = memByte
        Case &H05:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) - R(R2)
        Case &H06:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(R1) = R(R1) - Asc(Memory, R2)
        Case &H07:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) - R2
        Case &H08:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            memByte = Asc(Memory, R1) - memByte
            If memByte > R2 Then FC = -1
            Asc(Memory, R1) = memByte
        Case &H09:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) * R(R2)
        Case &H0A:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(R1) = R(R1) * Asc(Memory, R2)
        Case &H0B:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) * R2
        Case &H0C:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            memByte = Asc(Memory, R1) * memByte
            If memByte < R2 Then FC = -1
            Asc(Memory, R1) = memByte
        Case &H0D:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) \ R(R2)
        Case &H0E:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(R1) = R(R1) \ Asc(Memory, R2)
        Case &H0F:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) \ R2
        Case &H10:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            memByte = Asc(Memory, R1) \ memByte
            If memByte < R2 Then FC = -1
            Asc(Memory, R1) = memByte
        Case &H11:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R1) Mod R(R2)
        Case &H12:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(R1) = R(R1) Mod Asc(Memory, R2)
        Case &H13:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R(R1) = R(R1) Mod memByte
        Case &H14:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            Asc(Memory, R1) = Asc(Memory, R1) Mod memByte
        Case &H20:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: R(R1) = Asc(Memory, R(7))
        Case &H21:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            Asc(Memory, R1) = memByte
        Case &H22:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: R(R1) = R2 + Asc(Memory, R(7))
        Case &H23:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            Mid$(Memory, R1, 2) = Reverse$(MKI$(R2))
        Case &H24:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 16777216
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte * 65536
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(R1) = R2
        Case &H25:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 16777216
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte * 65536
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            Mid$(Memory, R1, 4) = Reverse$(MKL$(R2))
        Case &H26:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R(R1) = R(R2)
        Case &H27:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            Mid$(Memory, R1, 2) = Mid$(Memory, R2, 2)
        Case &H28:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            Asc(Memory, R1) = R(R2) Mod 256
        Case &H29:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(R1) = Asc(Memory, R2)
        Case &H30:
            FZ = -1
        Case &H31:
            FC = -1
        Case &H32:
            FE = -1
        Case &H37:
            FZ = 0
        Case &H38:
            FC = 0
        Case &H39:
            FE = 0
        Case &H3F:
            Exit Do
        Case &H40:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R(memByte) = R(memByte) + 1
        Case &H41:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R(memByte) = R(memByte) - 1
        Case &H42:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            memByte = Asc(Memory, R1) + 1
            Asc(Memory, R1) = memByte
        Case &H43:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            memByte = Asc(Memory, R1) - 1
            Asc(Memory, R1) = memByte
        Case &H45:
            Key$ = InKey$
            If Len(Key$) = 1 Then R(8) = Asc(Key$)
            If Len(Key$) = 2 Then R(8) = CVI(Key$)
        Case &H46:
            Sleep
            Key$ = InKey$
            If Len(Key$) = 1 Then R(8) = Asc(Key$)
            If Len(Key$) = 2 Then R(8) = CVI(Key$)
        Case &H4A:
            R(3) = _MouseX
        Case &H4B:
            R(4) = _MouseY
        Case &H4C:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            _MouseMove R(R1), _MouseY
        Case &H4D:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            _MouseMove _MouseX, R(R1)
        Case &H4E:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            _MouseMove Asc(Memory, R1), _MouseY
        Case &H4F:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            _MouseMove _MouseX, Asc(Memory, R1)
        Case &H50:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R1 = R(R1)
            R2 = R(R2)
            FG = R1 > R2
            FL = R1 < R2
            FE = R1 = R2
        Case &H51:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R1 = R(R1)
            R2 = Asc(Memory, R2)
            FG = R1 > R2
            FL = R1 < R2
            FE = R1 = R2
        Case &H52:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R1 = Asc(Memory, R1)
            R2 = Asc(Memory, R2)
            FG = R1 > R2
            FL = R1 < R2
            FE = R1 = R2
        Case &H53:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R1 = R(R1)
            FG = R1 > R2
            FL = R1 < R2
            FE = R1 = R2
        Case &H54:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            R1 = Asc(Memory, R1)
            FG = R1 > R2
            FL = R1 < R2
            FE = R1 = R2
        Case &H55:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte
            FG = R1 > R2
            FL = R1 < R2
            FE = R1 = R2
        Case &H60:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FZ Then R(7) = R1 - 1
        Case &H61:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FZ = 0 Then R(7) = R1 - 1
        Case &H62:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FC Then R(7) = R1 - 1
        Case &H63:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FC = 0 Then R(7) = R1 - 1
        Case &H64:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FE Then R(7) = R1 - 1
        Case &H65:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FE = 0 Then R(7) = R1 - 1
        Case &H66:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FL Then R(7) = R1 - 1
        Case &H67:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FL And FE Then R(7) = R1 - 1
        Case &H68:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FG Then R(7) = R1 - 1
        Case &H69:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            If FG And FE Then R(7) = R1 - 1
        Case &H6F:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R(7) = R1 + memByte - 1
        Case &H70:
            SC = SC + MKL$(R(7))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R(7) = R1 - 1
        Case &H71:
            R(7) = CVL(Right$(SC, 4)) + 2
            SC = Left$(SC, Len(SC) - 4)
        Case &H80:
            SA = SA + MKL$(R(1)) + MKL$(R(2)) + MKL$(R(3)) + MKL$(R(4))
        Case &H81:
            If Len(SA) Then
                R(4) = CVL(Right$(SA, 4))
                SA = Left$(SA, Len(SA) - 4)
                R(3) = CVL(Right$(SA, 4))
                SA = Left$(SA, Len(SA) - 4)
                R(2) = CVL(Right$(SA, 4))
                SA = Left$(SA, Len(SA) - 4)
                R(1) = CVL(Right$(SA, 4))
                SA = Left$(SA, Len(SA) - 4)
            Else
                R(1) = 0
                R(2) = 0
                R(3) = 0
                R(4) = 0
            End If
        Case &H82:
            SB = SB + MKL$(R(5)) + MKL$(R(6))
        Case &H83:
            If Len(SB) Then
                R(6) = CVL(Right$(SB, 4))
                SB = Left$(SB, Len(SB) - 4)
                R(5) = CVL(Right$(SB, 4))
                SB = Left$(SB, Len(SB) - 4)
            Else
                R(5) = 0
                R(6) = 0
            End If
        Case &H84:
            SD = SD + MKL$(R(8))
        Case &H85:
            If Len(SD) Then
                R(8) = CVL(Right$(SD, 4))
                SD = Left$(SD, Len(SD) - 4)
            Else
                R(8) = 0
            End If
        Case &H90:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R1 + memByte
            R1 = Asc(Memory, R1)
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R2 = Asc(Memory, R2)
            Locate R1, R2
        Case &H91:
            Print Chr$(R(1));
        Case &H92:
            R(1) = Asc(Memory, R(5))
            R(5) = R(5) + 1
        Case &H93:
            Asc(Memory, R(6)) = R(1) mod 256
            R(6) = R(6) + 1
        Case &H94:
            Cls , _BackgroundColor
        Case &H95:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R(memByte)
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R(memByte)
            Color R1, R2
        Case &HA0:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R(memByte)
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R3 = R(memByte)
            readSectors$ = String$(512 * R3, 0)
            Seek R1, R(3) * 65536 + R(4) + 1
            Get R1, , readSectors$
            Mid$(Memory, R2, R3 * 512) = readSectors$
        Case &HA1:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = R(memByte)
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = R2 + memByte
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R3 = R(memByte)
            Seek R1, R(3) * 65536 + R(4) + 1
            readSectors$ = Mid$(Memory, R2, R3 * 512)
            Put R1, , readSectors$
        Case &HB0:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = CVI(Reverse$(Mid$(Memory, R1 + memByte, 2)))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = CVI(Reverse$(Mid$(Memory, R2 + memByte, 2)))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R3 = memByte
            PSet (R1, R2), R(R3)
        Case &HB1:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = CVI(Reverse$(Mid$(Memory, R1 + memByte, 2)))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = CVI(Reverse$(Mid$(Memory, R2 + memByte, 2)))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R3 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R3 = R3 + memByte
            PSet (R1, R2), CVL(Mid$(Memory, R3, 4))
        Case &HB2:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = CVI(Reverse$(Mid$(Memory, R1 + memByte, 2)))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = CVI(Reverse$(Mid$(Memory, R2 + memByte, 2)))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R3 = R(memByte)
            R(memByte) = Point(R1, R2)
        Case &HB3:
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R1 = CVI(Reverse$(Mid$(Memory, R1 + memByte, 2)))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R2 = CVI(Reverse$(Mid$(Memory, R2 + memByte, 2)))
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R3 = memByte * 256
            R(7) = R(7) + 1: memByte = Asc(Memory, R(7))
            R3 = R3 + memByte
            Mid$(Memory, R3, 4) = Reverse$(MKL$(Point(R1, R2)))
        Case &HFF:
            System
    End Select
Loop
_Title "Halt"
Sleep
System
eH:
_Echo "Error:{" + _ErrorMessage$ + " at line " + _Trim$(Str$(_ErrorLine)) + "}"
_Echo "RG:" + Hex$(R(7)) + ", Code:" + Hex$(memByte)
System
Function Reverse$ (IN$)
    L = Len(IN$)
    For I = 1 To Int(L / 2)
        TMP$ = Mid$(IN$, I, 1)
        Mid$(IN$, I, 1) = Mid$(IN$, L - I + 1, 1)
        Mid$(IN$, L - I + 1, 1) = TMP$
    Next I
    Reverse$ = IN$
End Function
