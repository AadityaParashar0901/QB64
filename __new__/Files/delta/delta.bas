$Console:Only
If _CommandCount = 0 Then System
Open Command$(1) For Binary As #1
D$ = String$(LOF(1), 0)
Get #1, , D$
E$ = deltaEncode$(D$)
Open Command$(1) + ".delta" For Binary As #2
Put #2, , E$
Open Command$(1) + ".deltad" For Binary As #3
D$ = deltaDecode$(E$)
Put #3, , D$
Close
System
Function deltaEncode$ (__IN$)
    Dim As _Unsigned _Byte __A
    __I$ = String$(Len(__IN$), 0)
    Asc(__I$, 1) = Asc(__IN$, 1)
    For __I& = 2 To Len(__IN$)
        __A = Asc(__IN$, __I&) - Asc(__IN$, __I& - 1)
        Asc(__I$, __I&) = __A
    Next __I&
    deltaEncode$ = __I$
End Function
Function deltaDecode$ (__IN$)
    Dim As _Unsigned _Byte __A
    __I$ = String$(Len(__IN$), 0)
    Asc(__I$, 1) = Asc(__IN$, 1)
    For __I& = 2 To Len(__IN$)
        __A = Asc(__IN$, __I&) + Asc(__I$, __I& - 1)
        Asc(__I$, __I&) = __A
    Next __I&
    deltaDecode$ = __I$
End Function
