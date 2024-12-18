$Console:Only
'$Dynamic
DefInt A-Z
Type Connection
    As Integer HostID
    As Long Handle
End Type
Dim As Long Hosts(1), Clients(1)
Dim Connections(1) As Connection
Dim nHost, nClient, nConnection
Do Until _ScreenExists: Loop
Color 15, 0: Print "Networking Utility"
_ConsoleTitle "Networking Utility"
10
Do
    On Error GoTo ErrHandler
    Line Input "> "; C$
    If InStr(C$, " ") = 0 Then C$ = C$ + " "
    Select Case UCase$(Left$(C$, InStr(C$, " ") - 1))
        Case "OPENHOST"
            tmpHost = _OpenHost(Mid$(C$, InStr(C$, " ") + 1))
            If tmpHost Then
                Print "[Started Host: "; _ConnectionAddress(tmpHost); "]"
                nHost = nHost + 1
                ReDim _Preserve Hosts(1 To nHost)
                Hosts(nHost) = tmpHost
            Else
                Print "[Error]"
            End If
        Case "LISTHOST", "LISTHOSTS"
            For I = 1 To nHost
                If Hosts(I) Then Print I; ":"; _ConnectionAddress(Hosts(I))
            Next I
        Case "CLOSEHOST"
            idHost = Val(Mid$(C$, InStr(C$, " ") + 1))
            If idHost = 0 Then
                For I = 1 To nHost
                    If Hosts(I) Then Close Hosts(I)
                Next I
                nHost = 0
                ReDim Hosts(1)
            Else
                If Hosts(idHost) = 0 Then Exit Select
                Close Hosts(idHost): Hosts(idHost) = 0
                For I = idHost To nHost - 1
                    Swap Hosts(I + 1), Hosts(I)
                Next I
                nHost = nHost - 1
                If nHost Then ReDim _Preserve Hosts(1 To nHost)
            End If
        Case "OPENCONNECTION"
            idHost = Val(Mid$(C$, InStr(C$, " ") + 1))
            If idHost > nHost Then Print "No Host Opened": Exit Select
            If Hosts(idHost) = 0 Then Print "No Host Opened": Exit Select
            tmpConnection = _OpenConnection(Hosts(idHost))
            If tmpConnection Then
                Print "[Started Connection: "; _ConnectionAddress(tmpConnection); "]"
                nConnection = nConnection + 1
                ReDim _Preserve Connections(1 To nConnection)
                Connections(nConnection).HostID = idHost
                Connections(nConnection).Handle = tmpConnection
            End If
        Case "TESTCONNECTION", "TESTCONNECTIONS"
            For I = 1 To nConnection
                If Connections(I).Handle = 0 Then _Continue
                If _Connected(Connections(I).Handle) = 0 Then
                    Connections(I).Handle = 0
                    For J = I To nConnection - 1
                        Swap Connections(J + 1), Connections(J)
                    Next J
                    nConnection = nConnection - 1
                    If nConnection Then ReDim _Preserve Connections(1 To nConnection)
                End If
            Next I
        Case "LISTCONNECTION", "LISTCONNECTIONS"
            For I = 1 To nConnection
                If Connections(I).Handle Then Print I; ":"; _ConnectionAddress(Connections(I).Handle)
            Next I
        Case "CLOSECONNECTION"
            idConnection = Val(Mid$(C$, InStr(C$, " ") + 1))
            If idConnection = 0 Then
                For I = 1 To nConnection
                    If Connections(I).Handle Then Close Connections(I).Handle
                Next I
                nConnection = 0
                ReDim Connections(1)
            Else
                If Connections(idConnection).Handle = 0 Then Exit Select
                Close Connections(idConnection).Handle: Connections(idConnection).Handle = 0
                For I = idConnection To nConnection - 1
                    Swap Connections(I + 1), Connections(I)
                Next I
                nConnection = nConnection - 1
                If nConnection Then ReDim _Preserve Connections(1 To nConnection)
            End If
        Case "OPENCLIENT"
            tmpClient = _OpenClient(Mid$(C$, InStr(C$, " ") + 1))
            If tmpClient Then
                Print "[Started Client: "; _ConnectionAddress(tmpClient); "]"
                nClient = nClient + 1
                ReDim _Preserve Clients(1 To nClient)
                Clients(nClient) = tmpClient
            Else
                Print "[Error]"
            End If
        Case "TESTCLIENT", "TESTCLIENTS"
            For I = 1 To nClient
                If Clients(I) = 0 Then _Continue
                If _Connected(Clients(I)) = 0 Then
                    Clients(I) = 0
                    For J = I To nClients - 1
                        Swap Clients(J + 1), Clients(J)
                    Next J
                    nClient = nClient - 1
                    If nClient Then ReDim _Preserve Clients(1 To nClient)
                End If
            Next I
        Case "LISTCLIENT", "LISTCLIENTS"
            For I = 1 To nClient
                If Clients(I) Then Print I; ":"; _ConnectionAddress(Clients(I))
            Next I
        Case "CLOSECLIENT"
            idClient = Val(Mid$(C$, InStr(C$, " ") + 1))
            If idClient = 0 Then
                For I = 1 To nClient
                    If Clients(I) Then Close Clients(I)
                Next I
                nClient = 0
                ReDim Clients(1)
            Else
                If Clients(idClient) = 0 Then Exit Select
                Close Clients(idClient): Clients(idClient) = 0
                For I = idClient To nClient - 1
                    Swap Clients(I + 1), Clients(I)
                Next I
                nClient = nClient - 1
                If nClient Then ReDim _Preserve Clients(1 To nClient)
            End If
        Case "CLOSE"
            Print "Closing All Connections"
            For I = 1 To nConnection
                If Connections(I).Handle Then Close Connections(I).Handle
                Connections(I).Handle = 0
            Next I
            nConnection = 0
            Print "Closing All Clients"
            For I = 1 To nClient
                If Clients(I) Then Close Clients(I)
                Clients(I) = 0
            Next I
            nClient = 0
            Print "Closing All Hosts"
            For I = 1 To nHost
                If Hosts(I) Then Close Hosts(I)
                Hosts(I) = 0
            Next I
            nHost = 0
        Case "CLS"
            Cls
            Print "Networking Utility"
        Case "HELP"
            Print "Commands: openhost, listhost(s), closehost, openconnection, testconnection(s), listconnection(s), closeconnection, openclient, testclient(s), listclient(s), closeclient, close, cls, help, exit"
        Case "EXIT"
            Print "Closing All Connections"
            For I = 1 To nConnection
                If Connections(I).Handle Then Close Connections(I).Handle
                Connections(I).Handle = 0
            Next I
            Print "Closing All Clients"
            For I = 1 To nClient
                If Clients(I) Then Close Clients(I)
                Clients(I) = 0
            Next I
            Print "Closing All Hosts"
            For I = 1 To nHost
                If Hosts(I) Then Close Hosts(I)
                Hosts(I) = 0
            Next I
            Exit Do
        Case Else
            Print "No such command"
    End Select
Loop
System
ErrHandler:
If Err Then Print "["; _ErrorMessage$; "]"
Resume 10
