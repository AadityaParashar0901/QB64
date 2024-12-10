Screen _NewImage(640, 480, 256)
DefDbl A-Z
Randomize Timer
Type Node
    As _Unsigned Long nInputs
    As String InputID, Weights
    As Double Bias, Value
End Type

Const learningRate = 0.1
Dim E As _Bit * 2

Dim Shared As Node Nodes(1 To 4), OutputNode
Nodes(1).nInputs = 2: Nodes(1).InputID = NumberListAdd$(NumberListAdd$(NumberListNew$, -1), -2): Nodes(1).Weights = NumberListNew$: For I = 1 To Nodes(1).nInputs: Nodes(1).Weights = NumberListAdd$(Nodes(1).Weights, Rnd): Next I: Nodes(1).Bias = 0 'Rnd * 2 - 1
Nodes(2).nInputs = 2: Nodes(2).InputID = NumberListAdd$(NumberListAdd$(NumberListNew$, -1), -2): Nodes(2).Weights = NumberListNew$: For I = 1 To Nodes(2).nInputs: Nodes(2).Weights = NumberListAdd$(Nodes(2).Weights, Rnd): Next I: Nodes(2).Bias = 0 'Rnd * 2 - 1
Nodes(3).nInputs = 2: Nodes(3).InputID = NumberListAdd$(NumberListAdd$(NumberListNew$, 1), 2): Nodes(3).Weights = NumberListNew$: For I = 1 To Nodes(3).nInputs: Nodes(3).Weights = NumberListAdd$(Nodes(3).Weights, Rnd): Next I: Nodes(3).Bias = 0 'Rnd * 2 - 1
Nodes(4).nInputs = 2: Nodes(4).InputID = NumberListAdd$(NumberListAdd$(NumberListNew$, 1), 2): Nodes(4).Weights = NumberListNew$: For I = 1 To Nodes(4).nInputs: Nodes(4).Weights = NumberListAdd$(Nodes(4).Weights, Rnd): Next I: Nodes(4).Bias = 0 'Rnd * 2 - 1
OutputNode.nInputs = 2: OutputNode.InputID = NumberListAdd$(NumberListAdd$(NumberListNew$, 3), 4): OutputNode.Weights = NumberListNew$: For I = 1 To OutputNode.nInputs: OutputNode.Weights = NumberListAdd$(OutputNode.Weights, Rnd): Next I: OutputNode.Bias = 0 'Rnd * 2 - 1

Do Until _ScreenExists: Loop
For I = 0 To 3
    A = I And 1
    B = _SHR(I And 2, 1)
    O = A Xor B
    Print A; B, O; ForwardPropagate(A, B)
Next I
Y = CsrLin
X = Pos(0)
Do
    Epoch = Epoch + 1
    E = E + 1
    If E = 0 Then Locate Y, X
    A = E And 1
    B = _SHR(E And 2, 1)
    O = A Xor B
    Train A, B, O
    Print Epoch; ":"; A; B, O; ForwardPropagate(A, B)
Loop

Function ForwardPropagate (A, B)
    Dim As Double K
    For I = LBound(Nodes) To UBound(Nodes)
        Nodes(I).Value = 0
        For J = 1 To Nodes(I).nInputs
            K = NumberListGet(Nodes(I).InputID, J)
            If K = -1 Then
                K = A
            ElseIf K = -2 Then
                K = B
            Else K = Nodes(K).Value
            End If
            Nodes(I).Value = Nodes(I).Value + K * NumberListGet(Nodes(I).Weights, J)
        Next J
        Nodes(I).Value = ReLU(Nodes(I).Value + Nodes(I).Bias)
    Next I
    OutputNode.Value = 0
    For J = 1 To OutputNode.nInputs
        OutputNode.Value = OutputNode.Value + Nodes(NumberListGet(OutputNode.InputID, J)).Value * NumberListGet(OutputNode.Weights, J)
    Next J
    OutputNode.Value = ReLU(OutputNode.Value + OutputNode.Bias)
    ForwardPropagate = OutputNode.Value
End Function

Sub Train (A, B, O)
    P = ForwardPropagate(A, B)
    deltaO = (O - P) * dReLU(P)
    For I = 1 To OutputNode.nInputs
        UpdateNodeInputs NumberListGet(OutputNode.InputID, I), deltaO, A, B
        OutputNode.Weights = NumberListEdit$(OutputNode.Weights, I, NumberListGet(OutputNode.Weights, I) * (1 + deltaO * learningRate))
    Next I
    OutputNode.Bias = OutputNode.Bias * (1 + deltaO * learningRate)
End Sub
Sub UpdateNodeInputs (NodeID As _Unsigned Long, deltaOutput As Double, A, B)
    For I = 1 To Nodes(NodeID).nInputs
        K = NumberListGet(Nodes(NodeID).InputID, I)
        If K > 0 Then
            UpdateNodeInputs K, deltaOutput, A, B
            deltaH = deltaOutput * NumberListGet(Nodes(NodeID).Weights, I) * dReLU(Nodes(K).Value)
        Else
            If K = -1 Then K = A
            If K = -2 Then K = B
            deltaH = deltaOutput * NumberListGet(Nodes(NodeID).Weights, I) * dReLU(K)
        End If
    Next I
    Nodes(NodeID).Bias = Nodes(NodeID).Bias * (1 + deltaOutput * learningRate)
End Sub

Function ReLU (X)
    ReLU = -X * (X > 0)
End Function
Function dReLU (X)
    dReLU = -(X > 0)
End Function
'$Include:'files\QBListDict.bas'
