$Console:Only
'$Dynamic
DefLng A-Z
If _CommandCount = 0 Then System
Open Command$(1) For Binary As #1
If LOF(1) > 2 ^ 26 Then Print "File too large": System
D$ = String$(LOF(1), 0)
Get #1, , D$
Print "Allocating Memory"
O$ = String$(LOF(1) * 10, 0)
Print "Converting"
NEWLINE$ = MKI$(&H0A0D)
CR$10 = "1111111111"
For I = 1 To LOF(1)
    BYTE~%% = Asc(D$, I)
    Mid$(O$, (I - 1) * 10 + 1, 10) = ByteToBits$(BYTE~%%, 8) + NEWLINE$
Next I
Open Command$(1) + ".txt" For Binary As #2
Put #2, , O$
Close
System
Function ByteToBits$ (__BYTE As _Unsigned _Byte, __MAX_LEN As _Unsigned _Byte) Static
    Dim __I As _Unsigned _Byte
    Dim __O$
    __O$ = Space$(__MAX_LEN)
    For __I = 0 To __MAX_LEN - 1
        If __BYTE And 2 ^ __I Then Mid$(__O$, __MAX_LEN - __I, 1) = "1" Else Mid$(__O$, __MAX_LEN - __I, 1) = "0"
    Next __I
    ByteToBits$ = __O$
End Function
