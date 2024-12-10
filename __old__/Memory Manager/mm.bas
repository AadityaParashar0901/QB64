$Console:Only
If Not _FileExists("mm.inf") Then
    Print "Information File does not exists!"
    System
End If
Open "mm.inf" For Input As #1
Line Input #1, folderPath$
Close
ChDir folderPath$
Select Case Command$(1)
    Case "-c", "--close": Print "Closing Memory Manager"
        Open ".close" For Output As #1
	Case "-p", "--pause": Print "Pausing Memory Manager"
		Open ".pause" For Output As #1
    Case "-r", "--resume": Print "Resuming Memory Manager"
        If _FileExists(".pause") Then Kill ".pause"
    Case "-s", "--start": Print "Starting Memory Manager"
        Shell _Hide "start memory_manager.exe"
    Case Else: Print "Checking Memory Manager Status"
        If _FileExists(".running") Then
            Kill ".running"
            _Delay 0.5: If _FileExists(".running") Then Print "Memory Manager is running" Else Print "Memory Manager is not running"
        Else
            Print "Memory Manager is not running"
        End If
End Select
Close
System