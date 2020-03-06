'Public Class GUI_Group
'    Public Visable As Boolean
'    Public Location As Point
'    Public Size As Size
'    Public Name As String
'End Class
'操作介面的控件
Public Class GUI_TextBox
    Public Visable As Boolean
    Public Location As Point
    Public Size As Size
    Public Text As String
    Public PassWord As Boolean
    'Public ENonly As Boolean
End Class
Public Class GUI_Lable
    Public Visable As Boolean
    Public Text As String
    Public Location As Point
    Public Font As Font
End Class
Public Class GUI_Button
    Public Visable As Boolean
    Public Location As Point
    Public Size As Size
    Public Text As String
End Class
Public Class GUI_NumberSelect
    Public Visable As Boolean
    Public Location As Point
    Public Value As Integer
End Class
Public Class GUI_Bar
    Public Visable As Boolean
    Public Location As Point
    Public Size As Size
    Public Max As Integer
    Public Value As Integer
End Class
Public Class GUI_GButton
    Public Visable As Boolean
    Public Location As Point
    Public Size As Size
    Public Graphic() As Point
End Class
Public Class GUI_MsgBox
    Public Visable As Boolean
    Public Location As Point
    Public Size As Size
    Public Title As String
    Public Msg As String
    Public Button As GUI_Button
End Class

'三維繪圖的控件
Public Class GD_Camera
    Public X As Single
    Public Y As Single
    Public Z As Single
    Public Angle_W As Double
    Public Angle_A As Double
End Class
Public Class GD_Polygon
    'Public Visable As Boolean
    'Public Color As Color
    Public Point(3)() As Single
    Public Function Location_X() As Single
        Location_X = (Point(0)(0) + Point(1)(0) + Point(2)(0) + Point(3)(0)) / 4
    End Function
    Public Function Location_Y() As Single
        Location_Y = (Point(0)(1) + Point(1)(1) + Point(2)(1) + Point(3)(1)) / 4
    End Function
    Public Function Location_Z() As Single
        Location_Z = (Point(0)(2) + Point(1)(2) + Point(2)(2) + Point(3)(2)) / 4
    End Function
End Class
