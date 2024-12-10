Screen _NewImage(_DesktopWidth, _DesktopHeight, 32)
Randomize Timer
_FullScreen _SquarePixels
Open "wallpaper_settings" For Binary As #1
Dim As _Unsigned _Byte red, green, blue, AVG
Get #1, , red
Get #1, , green
Get #1, , blue
Get #1, , AVG
Cls , _RGB32(red, green, blue)
For I = 1 To 16 * 9
    Line (Rnd * _Width, Rnd * _Height)-(Rnd * _Width, Rnd * _Height), _RGB32(red + AVG * rr * Sgn(red), green + AVG * rr * Sgn(green), blue + AVG * rr * Sgn(blue)), BF
Next I
T = PNG32("wallpaper" + _Trim$(Str$(Timer)), 0, "")
System
Function rr
    rr = (Rnd - .5)
End Function
Sub FillCircle (__X, __Y, __R, __C As Long)
    Circle (__X, __Y), __R, __C
    Paint (__X, __Y), __C
End Sub
'$include:'image.bm'
