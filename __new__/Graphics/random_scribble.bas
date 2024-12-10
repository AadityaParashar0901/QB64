Screen _NewImage(640, 480, 32)
Type Vec2d
    As Single X, Y
End Type
Dim As Vec2d NormalVector, PositionVector, LastPositionVector, RandomVector
Do
    Randomize Timer
    Cls
    NewRandomVector NormalVector
    NewVector PositionVector, _Width / 2, _Height / 2
    LastPositionVector = PositionVector
    For Y = 1 To _Height
        NewVector RandomVector, Rnd - 0.5, Rnd - 0.5
        NormalizeVector RandomVector
        MultiplyVector RandomVector, DotProduct(RandomVector, NormalVector) * 10
        'NewParallelVector NormalVector, RandomVector
        'RotateVector NormalVector, _Pi / 2
        AddVector PositionVector, RandomVector, PositionVector
        Line (LastPositionVector.X, LastPositionVector.Y)-(PositionVector.X, PositionVector.Y), _RGB32(0, 127, 255)
        LastPositionVector = PositionVector
    Next Y
    Sleep
Loop Until Inp(&H60) = 1
System
Sub NewVector (A As Vec2d, X, Y)
    A.X = X
    A.Y = Y
End Sub
Sub NewRandomVector (A As Vec2d)
    A.X = Rnd - 0.5
    A.Y = Rnd - 0.5
    NormalizeVector A
End Sub
Sub NewParallelVector (A As Vec2d, B As Vec2d)
    A = B
    NormalizeVector A
End Sub
Function DotProduct (A As Vec2d, B As Vec2d)
    DotProduct = A.X * B.X + A.Y * B.Y
End Function
Sub NormalizeVector (A As Vec2d)
    L = Sqr(DotProduct(A, A))
    A.X = A.X / L
    A.Y = A.Y / L
End Sub
Sub AddVector (A As Vec2d, B As Vec2d, C As Vec2d)
    C.X = A.X + B.X
    C.Y = A.Y + B.Y
End Sub
Sub SubVector (A As Vec2d, B As Vec2d, C As Vec2d)
    C.X = A.X - B.X
    C.Y = A.Y - B.Y
End Sub
Sub MultiplyVector (A As Vec2d, B As Single)
    A.X = A.X * B
    A.Y = A.Y * B
End Sub
Sub RotateVector (A As Vec2d, T As Single)
    Dim B As Vec2d
    B.X = A.X * Cos(T) + A.Y * Sin(T)
    B.Y = -A.X * Sin(T) + A.Y * Cos(T)
    A = B
End Sub
