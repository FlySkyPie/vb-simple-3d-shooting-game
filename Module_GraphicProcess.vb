Module Module_GraphicProcess
    '繪製視窗按鈕
    Public Sub Draw_GUI_Objects(ByVal _bitmap As Bitmap)
        Dim drawFont As New Font("Arial", 9)
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        Dim g As Graphics
        g = Graphics.FromImage(_bitmap)
        '繪製文字
        For i = 0 To MAX_Lable
            'Dim _Font As New Font("Arial", 9)
            Dim _Brush As New SolidBrush(Color.FromArgb(64, 225, 225))
            If G_Lable(i).Visable Then
                Dim _Font = G_Lable(i).Font
                g.DrawString(G_Lable(i).Text, _Font, _Brush, G_Lable(i).Location)
            End If
        Next
        '繪製文字方塊
        For i = 0 To MAX_TextBox
            Dim _Font As New Font("Arial", 9)
            Dim _Brush As New SolidBrush(Color.FromArgb(64, 225, 225))
            Dim _pen As New Pen(_Brush)
            If G_Text(i).Visable Then
                _Brush.Color = Color.FromArgb(12, 64, 255, 255)
                g.FillRectangle(_Brush, G_Text(i).Location.X, G_Text(i).Location.Y, G_Text(i).Size.Width, G_Text(i).Size.Height)
                _Brush.Color = Color.FromArgb(64, 255, 255)
                g.DrawRectangle(_pen, G_Text(i).Location.X, G_Text(i).Location.Y, G_Text(i).Size.Width, G_Text(i).Size.Height)
                If G_Text(i).PassWord Then
                    Dim _str As String = ""
                    For j = 1 To G_Text(i).Text.Length
                        _str += "*"
                    Next
                    g.DrawString(_str, _Font, _Brush, G_Text(i).Location)
                Else
                    g.DrawString(G_Text(i).Text, _Font, _Brush, G_Text(i).Location)
                End If
            End If
        Next
        '繪製按鈕
        For i = 0 To MAX_Button
            If G_Button(i).Visable Then
                Dim _Brush As New SolidBrush(Color.FromArgb(64, 225, 225))
                Dim _pen As New Pen(_Brush)
                Dim _Font As New Font("Arial", 12)
                _Brush.Color = Color.FromArgb(30, 64, 255, 255)
                g.FillRectangle(_Brush, G_Button(i).Location.X, G_Button(i).Location.Y, G_Button(i).Size.Width, G_Button(i).Size.Height)
                _Brush.Color = Color.FromArgb(64, 255, 255)
                _pen.Width = 2 : g.DrawRectangle(_pen, G_Button(i).Location.X, G_Button(i).Location.Y, G_Button(i).Size.Width, G_Button(i).Size.Height)
                Dim Lx, Ly As Integer
                Lx = G_Button(i).Location.X + 5
                Ly = G_Button(i).Location.Y + G_Button(i).Size.Height / 2 - 9
                g.DrawString(G_Button(i).Text, _Font, _Brush, Lx, Ly)
            End If
        Next

        If TypeTaging Then  '繪製文字標記框
            If TypeShiny <= 12 Then
                TypeShiny += 1
                If TypeShiny <= 8 Then
                    drawpen.Color = Color.FromArgb(96, 250, 250)
                    drawpen.Width = 2
                    g.DrawRectangle(drawpen, TypeTag.Location.X, TypeTag.Location.Y, TypeTag.Size.Width, TypeTag.Size.Height)
                End If
            Else
                TypeShiny = 0
            End If
        End If
    End Sub







    '3D繪圖
    '繪製四方形
    Public Sub Draw_Polygon(ByVal _bitmap As Bitmap, ByVal _Camera As GD_Camera, ByVal _Polygon As GD_Polygon)
        Dim g As Graphics
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        g = Graphics.FromImage(_bitmap)
        Dim _rang, Xpc, Ypc, Zpc, Xd, Yd, Zd As Single
        Xpc = _Camera.X
        Ypc = _Camera.Y
        Zpc = _Camera.Z

        Xd = _Polygon.Location_X - Xpc
        Yd = _Polygon.Location_Y - Ypc
        Zd = _Polygon.Location_Z - Zpc
        _rang = Math.Sqrt(Xd ^ 2 + Yd ^ 2 + Zd ^ 2)
        'g.DrawString("range=" & _rang, New Font("Arial", 9), drawBrush, New Point(300, 320))
        If _rang < 200 Then      '小於50公尺
            Dim thdW, thdA, _thdW, _thdA, thpcW, thpcA, DW, DA, _dd1, _dd2 As Double
            thpcW = _Camera.Angle_W
            thpcA = _Camera.Angle_A
            thdW = Math.Atan2(Zd, Xd)
            thdA = Math.Atan2(Yd, Math.Sqrt(Xd ^ 2 + Zd ^ 2))
            _dd1 = Math.Sqrt(Xd ^ 2 + Zd ^ 2)
            _thdW = thdW - thpcW
            Xd = _dd1 * Math.Cos(_thdW)
            Zd = _dd1 * Math.Sin(_thdW)
            thdA = Math.Atan2(Yd, Xd)
            _dd2 = Math.Sqrt(Xd ^ 2 + Yd ^ 2)
            _thdA = thdA - thpcA
            Xd = _dd2 * Math.Cos(_thdA)
            _thdW = Math.Atan2(Zd, Xd)
            If _thdW > 3.14 Then _thdW = _thdW - 6.28
            If _thdW < -3.14 Then _thdW = _thdW + 6.28
            If _thdA > 3.14 Then _thdA = _thdA - 6.28
            If _thdA < -3.14 Then _thdA = _thdA + 6.28
            DW = Math.Atan2(0.02, Camera_ViewD)
            DA = Math.Atan2(0.015, Camera_ViewD)
            If _thdW < DW And _thdW > -DW And _thdA < DA And _thdA > -DA Then
                Dim _point(3) As Point
                For j = 0 To 3
                    Dim Xd2, Yd2, Zd2, _d1, _d2 As Single
                    Dim thdW2, thdA2, _thdW2, _thdA2 As Double
                    Xd2 = _Polygon.Point(j)(0) - Xpc
                    Yd2 = _Polygon.Point(j)(1) - Ypc
                    Zd2 = _Polygon.Point(j)(2) - Zpc
                    thdW2 = Math.Atan2(Zd2, Xd2)
                    _d1 = Math.Sqrt(Xd2 ^ 2 + Zd2 ^ 2)
                    _thdW2 = thdW2 - thpcW
                    Xd2 = _d1 * Math.Cos(_thdW2)
                    Zd2 = _d1 * Math.Sin(_thdW2)
                    thdA2 = Math.Atan2(Yd2, Xd2)
                    _d2 = Math.Sqrt(Xd2 ^ 2 + Yd2 ^ 2)
                    _thdA2 = thdA2 - thpcA
                    Xd2 = _d2 * Math.Cos(_thdA2)
                    _thdW2 = Math.Atan2(Zd2, Xd2)
                    Dim _x, _y As Single
                    _x = -Camera_ViewD * Math.Tan(_thdW2)
                    _y = Camera_ViewD * Math.Tan(_thdA2)
                    _point(j).X = (_x + 0.02) * 800 / 0.04
                    _point(j).Y = 600 - ((_y + 0.015) * 600 / 0.03)
                Next
                g.DrawPolygon(drawpen, _point)
            End If
        End If
    End Sub
    '繪製地平線
    Public Sub Draw_Horizon(ByVal _bitmap As Bitmap, ByVal _Camera As GD_Camera)
        Dim g As Graphics
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        g = Graphics.FromImage(_bitmap)
        Dim _i As Integer = 0
        For j = -45 To 45
            For k = -45 To 45
                Dim _r As Single = Math.Sqrt(j ^ 2 + k ^ 2)
                If _r < 45 Then
                    Const _width = 1
                    Dim _Spoint(3)() As Single
                    Dim _x, _z As Single
                    Dim _boo(3) As Boolean
                    _x = Int(Client_Camera.X \ _width) * _width + j * _width : _z = Int(Client_Camera.Z \ _width) * _width + k * _width
                    _Spoint(0) = {_x - _width / 2, 0, _z - _width / 2}
                    _Spoint(1) = {_x - _width / 2, 0, _z + _width / 2}
                    _Spoint(2) = {_x + _width / 2, 0, _z + _width / 2}
                    _Spoint(3) = {_x + _width / 2, 0, _z - _width / 2}
                    Dim Xpc, Ypc, Zpc, Xd, Yd, Zd, _d1, _d2 As Single
                    Dim thdW, thdA, _thdW, _thdA, thpcW, thpcA, DW, DA As Double
                    Xpc = _Camera.X
                    Ypc = _Camera.Y
                    Zpc = _Camera.Z
                    thpcW = _Camera.Angle_W
                    thpcA = _Camera.Angle_A
                    DW = Math.Atan2(0.02, Camera_ViewD)
                    DA = Math.Atan2(0.015, Camera_ViewD)
                    Dim _point(3) As Point
                    For p = 0 To 3
                        Xd = _Spoint(p)(0) - Xpc
                        Yd = _Spoint(p)(1) - Ypc
                        Zd = _Spoint(p)(2) - Zpc
                        thpcW = _Camera.Angle_W
                        thpcA = _Camera.Angle_A
                        thdW = Math.Atan2(Zd, Xd)
                        thdA = Math.Atan2(Yd, Math.Sqrt(Xd ^ 2 + Zd ^ 2))
                        _d1 = Math.Sqrt(Xd ^ 2 + Zd ^ 2)
                        _thdW = thdW - thpcW
                        Xd = _d1 * Math.Cos(_thdW)
                        Zd = _d1 * Math.Sin(_thdW)
                        thdA = Math.Atan2(Yd, Xd)
                        _d2 = Math.Sqrt(Xd ^ 2 + Yd ^ 2)
                        _thdA = thdA - thpcA
                        Xd = _d2 * Math.Cos(_thdA)
                        _thdW = Math.Atan2(Zd, Xd)
                        _boo(p) = _thdW < DW + 0.4 And _thdW > -DW - 0.4 And _thdA < DA + 0.4 And _thdA > -DA - 0.4
                        If _boo(p) Then
                            Dim _x2, _y2 As Single
                            _x2 = -Camera_ViewD * Math.Tan(_thdW)
                            _y2 = Camera_ViewD * Math.Tan(_thdA)
                            _point(p).X = (_x2 + 0.02) * 800 / 0.04
                            _point(p).Y = 600 - ((_y2 + 0.015) * 600 / 0.03)
                        End If
                    Next
                    If _boo(0) And _boo(1) And _boo(2) And _boo(3) Then
                        drawpen.Color = Color.FromArgb(64, 64, 225, 225)
                        g.DrawPolygon(drawpen, _point)
                    End If
                End If
            Next
        Next
    End Sub
    '繪製FPS介面
    Public Sub Draw_Game_HUD(ByVal _bitmap As Bitmap)
        Dim g As Graphics
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawFont As New Font("Tahoma", 9, FontStyle.Bold)
        Dim drawpen As New Pen(drawBrush)
        g = Graphics.FromImage(_bitmap)
        If UserObj.HP > 0 Then
            '血量與體力
            Dim _hp, _mp As Integer
            Dim _c(1)() As String
            _hp = UserObj.HP * 100 / 100
            _mp = UserObj.MP * 100 / 1000
            If _hp < 10 Then
                g.DrawString(_hp & "%", drawFont, drawBrush, 761, 545)
            ElseIf _hp < 100 Then
                g.DrawString(_hp & "%", drawFont, drawBrush, 753, 545)
            Else
                g.DrawString(_hp & "%", drawFont, drawBrush, 745, 545)
            End If
            If _mp < 10 Then
                g.DrawString(_mp & "%", drawFont, drawBrush, 761, 570)
            ElseIf _mp < 100 Then
                g.DrawString(_mp & "%", drawFont, drawBrush, 753, 570)
            Else
                g.DrawString(_mp & "%", drawFont, drawBrush, 745, 570)
            End If
            drawpen.Width = 2
            drawBrush.Color = Color.FromArgb(155, 64, 255, 255)
            g.FillRectangle(drawBrush, 640, 575, _mp, 6)            'Bar
            drawpen.Color = Color.FromArgb(64, 255, 255)
            g.DrawRectangle(drawpen, 640, 575, 100, 6)              'Bar 外框
            drawBrush.Color = Color.FromArgb(155, 64, 255, 255)
            g.FillRectangle(drawBrush, 640, 550, _hp, 6)            'Bar
            drawpen.Color = Color.FromArgb(64, 255, 255)
            g.DrawRectangle(drawpen, 640, 550, 100, 6)              'Bar 外框
            '準心
            drawpen.Width = 1
            g.DrawLine(drawpen, New Point(390, 300), New Point(410, 300))
            g.DrawLine(drawpen, New Point(400, 290), New Point(400, 310))
        Else '你死惹
            Dim _r, _g, _b As Single
            _r = 48 + 16 * Math.Sin(UserObj.SpawnCount / 4)
            _g = 168.75 + 56.25 * Math.Sin(UserObj.SpawnCount / 4)
            _b = 168.75 + 56.25 * Math.Sin(UserObj.SpawnCount / 4)
            drawBrush.Color = Color.FromArgb(_r, _g, _b)
            g.DrawString("You will respawn in " & Format(UserObj.SpawnCount * 50 / 1000, "0.00") & " second", drawFont, drawBrush, 40, 545)
        End If


    End Sub
    '繪製雨滴
    Public Sub Draw_Rain(ByVal _bitmap As Bitmap, ByVal _Camera As GD_Camera)
        Dim g As Graphics
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        g = Graphics.FromImage(_bitmap)
        For i = 0 To Max_Rain
            If RainObj(i).Enable Then

                Dim _rang, Xpc, Ypc, Zpc, Xd, Yd, Zd As Single
                Xpc = _Camera.X
                Ypc = _Camera.Y
                Zpc = _Camera.Z

                Xd = RainObj(i).X - Xpc
                Yd = RainObj(i).Y - Ypc
                Zd = RainObj(i).Z - Zpc
                _rang = Math.Sqrt(Xd ^ 2 + Yd ^ 2 + Zd ^ 2)
                If _rang < 50 Then      '小於50公尺
                    Dim thdW, thdA, _thdW, _thdA, thpcW, thpcA, DW, DA, _dd1, _dd2 As Double
                    thpcW = _Camera.Angle_W
                    thpcA = _Camera.Angle_A
                    thdW = Math.Atan2(Zd, Xd)
                    thdA = Math.Atan2(Yd, Math.Sqrt(Xd ^ 2 + Zd ^ 2))
                    _dd1 = Math.Sqrt(Xd ^ 2 + Zd ^ 2)
                    _thdW = thdW - thpcW
                    Xd = _dd1 * Math.Cos(_thdW)
                    Zd = _dd1 * Math.Sin(_thdW)
                    thdA = Math.Atan2(Yd, Xd)
                    _dd2 = Math.Sqrt(Xd ^ 2 + Yd ^ 2)
                    _thdA = thdA - thpcA
                    Xd = _dd2 * Math.Cos(_thdA)
                    _thdW = Math.Atan2(Zd, Xd)

                    If _thdW > 3.14 Then _thdW = _thdW - 6.28
                    If _thdW < -3.14 Then _thdW = _thdW + 6.28
                    If _thdA > 3.14 Then _thdA = _thdA - 6.28
                    If _thdA < -3.14 Then _thdA = _thdA + 6.28
                    DW = Math.Atan2(0.02, Camera_ViewD)
                    DA = Math.Atan2(0.015, Camera_ViewD)
                    If _thdW < DW And _thdW > -DW And _thdA < DA And _thdA > -DA Then
                        Dim _point(1) As Point


                        Dim Xd2, Yd2, Zd2, _d1, _d2 As Single
                        Dim thdW2, thdA2, _thdW2, _thdA2 As Double
                        Xd2 = RainObj(i).X + RainObj(i).Fx / 3 - Xpc
                        Yd2 = RainObj(i).Y + RainObj(i).Fy / 3 - Ypc
                        Zd2 = RainObj(i).Z + RainObj(i).Fz / 3 - Zpc
                        thdW2 = Math.Atan2(Zd2, Xd2)

                        _d1 = Math.Sqrt(Xd2 ^ 2 + Zd2 ^ 2)
                        _thdW2 = thdW2 - thpcW

                        Xd2 = _d1 * Math.Cos(_thdW2)
                        Zd2 = _d1 * Math.Sin(_thdW2)

                        thdA2 = Math.Atan2(Yd2, Xd2)

                        _d2 = Math.Sqrt(Xd2 ^ 2 + Yd2 ^ 2)
                        _thdA2 = thdA2 - thpcA

                        Xd2 = _d2 * Math.Cos(_thdA2)
                        _thdW2 = Math.Atan2(Zd2, Xd2)

                        Dim _x, _y As Single
                        _x = -Camera_ViewD * Math.Tan(_thdW2)
                        _y = Camera_ViewD * Math.Tan(_thdA2)
                        _point(0).X = (_x + 0.02) * 800 / 0.04
                        _point(0).Y = 600 - ((_y + 0.015) * 600 / 0.03)


                        _x = -Camera_ViewD * Math.Tan(_thdW)
                        _y = Camera_ViewD * Math.Tan(_thdA)
                        _point(1).X = (_x + 0.02) * 800 / 0.04
                        _point(1).Y = 600 - ((_y + 0.015) * 600 / 0.03)

                        drawpen.Color = Color.FromArgb(255 - (_dd1 * 255 / 60), 64, 225, 225)
                        g.DrawLine(drawpen, _point(0), _point(1))
                        'g.DrawPolygon(drawpen, _point)
                    End If
                End If
            End If
        Next


    End Sub
    '繪製子彈(曳光彈)
    Public Sub Draw_Bullect(ByVal _bitmap As Bitmap, ByVal _Camera As GD_Camera)
        Dim g As Graphics
        Dim drawBrush As New SolidBrush(Color.FromArgb(255, 236, 172))
        Dim drawpen As New Pen(drawBrush)
        g = Graphics.FromImage(_bitmap)
        For i = 0 To Max_Bullet
            If BulletObj(i).Enable Then
                Dim _rang, _rang2, Xpc, Ypc, Zpc, Xd, Yd, Zd, Xd2, Yd2, Zd2 As Single
                Xpc = _Camera.X
                Ypc = _Camera.Y
                Zpc = _Camera.Z

                Xd = BulletObj(i).X - Xpc
                Yd = BulletObj(i).Y - Ypc
                Zd = BulletObj(i).Z - Zpc

                Dim _A, _W As Double
                _W = Math.Atan2(BulletObj(i).FZ, BulletObj(i).FX)
                _A = Math.Atan2(BulletObj(i).FY, Math.Sqrt(BulletObj(i).FZ ^ 2 + BulletObj(i).FX ^ 2))
                Xd2 = BulletObj(i).X - 5 * Math.Cos(_A) * Math.Cos(_W) - Xpc
                Yd2 = BulletObj(i).Y - 5 * Math.Sin(_A) - Ypc
                Zd2 = BulletObj(i).Z - 5 * Math.Cos(_A) * Math.Sin(_W) - Zpc

                _rang = Math.Sqrt(Xd ^ 2 + Yd ^ 2 + Zd ^ 2)
                _rang2 = Math.Sqrt(Xd2 ^ 2 + Yd2 ^ 2 + Zd2 ^ 2)
                If _rang < 200 And _rang > Camera_ViewD And _rang2 < 200 And _rang2 > Camera_ViewD Then      '小於50公尺
                    Dim thdW, thdA, _thdW, _thdA, thpcW, thpcA, DW, DA, _dd1, _dd2 As Double
                    thpcW = _Camera.Angle_W
                    thpcA = _Camera.Angle_A
                    thdW = Math.Atan2(Zd, Xd)
                    thdA = Math.Atan2(Yd, Math.Sqrt(Xd ^ 2 + Zd ^ 2))
                    _dd1 = Math.Sqrt(Xd ^ 2 + Zd ^ 2)
                    _thdW = thdW - thpcW
                    Xd = _dd1 * Math.Cos(_thdW)
                    Zd = _dd1 * Math.Sin(_thdW)
                    thdA = Math.Atan2(Yd, Xd)
                    _dd2 = Math.Sqrt(Xd ^ 2 + Yd ^ 2)
                    _thdA = thdA - thpcA
                    Xd = _dd2 * Math.Cos(_thdA)
                    _thdW = Math.Atan2(Zd, Xd)

                    If _thdW > 3.14 Then _thdW = _thdW - 6.28
                    If _thdW < -3.14 Then _thdW = _thdW + 6.28
                    If _thdA > 3.14 Then _thdA = _thdA - 6.28
                    If _thdA < -3.14 Then _thdA = _thdA + 6.28
                    DW = Math.Atan2(0.02, Camera_ViewD)
                    DA = Math.Atan2(0.015, Camera_ViewD)
                    If _thdW < DW And _thdW > -DW And _thdA < DA And _thdA > -DA Then
                        Dim _point(1) As Point
                        Dim _d1, _d2 As Single
                        Dim thdW2, thdA2, _thdW2, _thdA2 As Double

                        thdW2 = Math.Atan2(Zd2, Xd2)

                        _d1 = Math.Sqrt(Xd2 ^ 2 + Zd2 ^ 2)
                        _thdW2 = thdW2 - thpcW

                        Xd2 = _d1 * Math.Cos(_thdW2)
                        Zd2 = _d1 * Math.Sin(_thdW2)

                        thdA2 = Math.Atan2(Yd2, Xd2)

                        _d2 = Math.Sqrt(Xd2 ^ 2 + Yd2 ^ 2)
                        _thdA2 = thdA2 - thpcA

                        Xd2 = _d2 * Math.Cos(_thdA2)
                        _thdW2 = Math.Atan2(Zd2, Xd2)

                        If _thdW2 < DW And _thdW2 > -DW And _thdA2 < DA And _thdA2 > -DA Then
                            '平面繪圖計算
                            Dim _x, _y As Single
                            _x = -Camera_ViewD * Math.Tan(_thdW2)
                            _y = Camera_ViewD * Math.Tan(_thdA2)
                            _point(0).X = (_x + 0.02) * 800 / 0.04
                            _point(0).Y = 600 - ((_y + 0.015) * 600 / 0.03)

                            _x = -Camera_ViewD * Math.Tan(_thdW)
                            _y = Camera_ViewD * Math.Tan(_thdA)
                            _point(1).X = (_x + 0.02) * 800 / 0.04
                            _point(1).Y = 600 - ((_y + 0.015) * 600 / 0.03)

                            '根據遠近調整繪圖顏色亮度
                            Dim _r, _g, _b As Integer
                            _r = 255 - _rang * 255 / 200
                            _g = 236 - _rang * 236 / 200
                            _b = 172 - _rang * 172 / 200
                            drawBrush.Color = Color.FromArgb(_r, _g, _b)
                            If _point(0).X = _point(1).X And _point(0).Y = _point(1).Y Then
                                g.FillEllipse(drawBrush, _point(0).X - 1, _point(0).Y - 1, 2, 2)
                            Else
                                drawpen.Width = 1.5
                                drawpen.Color = Color.FromArgb(_r, _g, _b)
                                g.DrawLine(drawpen, _point(0), _point(1))
                            End If
                        End If '點二在角度內
                    End If
                End If '點一和點二距離都小於200而且大於相機成像距離
            End If
        Next
    End Sub
    '繪製方塊
    Public Sub Draw_Cube(ByVal _bitmap As Bitmap, ByVal _Camera As GD_Camera)
        Dim g As Graphics
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        g = Graphics.FromImage(_bitmap)
        For i = 0 To Max_Cube
            If CubeObj(i).Enable Then
                Dim _rang, Xpc, Ypc, Zpc, Xd, Yd, Zd As Single
                Xpc = _Camera.X
                Ypc = _Camera.Y
                Zpc = _Camera.Z

                Xd = CubeObj(i).X - Xpc
                Yd = CubeObj(i).Y - Ypc
                Zd = CubeObj(i).Z - Zpc
                _rang = Math.Sqrt(Xd ^ 2 + Yd ^ 2 + Zd ^ 2)
                'g.DrawString("range=" & _rang, New Font("Arial", 9), drawBrush, New Point(300, 320))
                If _rang < 200 And _rang > Camera_ViewD Then      '小於50公尺
                    Dim thdW, thdA, _thdW, _thdA, thpcW, thpcA, DW, DA, _dd1, _dd2 As Double
                    thpcW = _Camera.Angle_W
                    thpcA = _Camera.Angle_A
                    thdW = Math.Atan2(Zd, Xd)
                    thdA = Math.Atan2(Yd, Math.Sqrt(Xd ^ 2 + Zd ^ 2))

                    _dd1 = Math.Sqrt(Xd ^ 2 + Zd ^ 2)
                    _thdW = thdW - thpcW

                    Xd = _dd1 * Math.Cos(_thdW)
                    Zd = _dd1 * Math.Sin(_thdW)

                    thdA = Math.Atan2(Yd, Xd)
                    _dd2 = Math.Sqrt(Xd ^ 2 + Yd ^ 2)
                    _thdA = thdA - thpcA

                    Xd = _dd2 * Math.Cos(_thdA)
                    _thdW = Math.Atan2(Zd, Xd)

                    If _thdW > 3.14 Then _thdW = _thdW - 6.28
                    If _thdW < -3.14 Then _thdW = _thdW + 6.28
                    If _thdA > 3.14 Then _thdA = _thdA - 6.28
                    If _thdA < -3.14 Then _thdA = _thdA + 6.28
                    DW = Math.Atan2(0.02, Camera_ViewD)
                    DA = Math.Atan2(0.015, Camera_ViewD)
                    If _thdW < DW + 0.2 And _thdW > -DW - 0.2 And _thdA < DA + 0.2 And _thdA > -DA - 0.2 Then
                        Dim _point(7) As Point  '方塊的8個點
                        Dim _x1(7), _y1(7), _z1(7), _XX, _YY, _ZZ, _SX, _SY, _SZ As Single
                        Dim bool(7) As Boolean
                        _XX = CubeObj(i).X : _YY = CubeObj(i).Y : _ZZ = CubeObj(i).Z
                        _SX = CubeObj(i).Size_X / 2 : _SY = CubeObj(i).Size_Y / 2 : _SZ = CubeObj(i).Size_Z / 2
                        _x1 = {_XX - _SX, _XX - _SX, _XX + _SX, _XX + _SX, _XX - _SX, _XX - _SX, _XX + _SX, _XX + _SX}
                        _y1 = {_YY + _SY, _YY + _SY, _YY + _SY, _YY + _SY, _YY - _SY, _YY - _SY, _YY - _SY, _YY - _SY}
                        _z1 = {_ZZ - _SZ, _ZZ + _SZ, _ZZ + _SZ, _ZZ - _SZ, _ZZ - _SZ, _ZZ + _SZ, _ZZ + _SZ, _ZZ - _SZ}
                        For j = 0 To 7
                            _point(j) = Spoint_to_Ipoint(_Camera, _x1(j), _y1(j), _z1(j))
                        Next
                        g.DrawLine(drawpen, _point(0), _point(1))
                        g.DrawLine(drawpen, _point(1), _point(2))
                        g.DrawLine(drawpen, _point(2), _point(3))
                        g.DrawLine(drawpen, _point(3), _point(0))
                        g.DrawLine(drawpen, _point(4), _point(5))
                        g.DrawLine(drawpen, _point(5), _point(6))
                        g.DrawLine(drawpen, _point(6), _point(7))
                        g.DrawLine(drawpen, _point(7), _point(4))
                        g.DrawLine(drawpen, _point(0), _point(4))
                        g.DrawLine(drawpen, _point(1), _point(5))
                        g.DrawLine(drawpen, _point(2), _point(6))
                        g.DrawLine(drawpen, _point(3), _point(7))
                    End If ' _thdW < DW And _thdW > -DW And _thdA < DA And _thdA > -DA 
                End If


            End If
        Next
    End Sub
    Public Sub Draw_Player(ByVal _bitmap As Bitmap, ByVal _Camera As GD_Camera)
        Dim g As Graphics
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        g = Graphics.FromImage(_bitmap)
        For i = 0 To Max_Player
            If PlayerObj(i).Enable Then
                If PlayerObj(i).NickName <> UserObj.NickName And PlayerObj(i).HP > 0 Then '如果不是玩家自身就進行繪圖-繪製除了自己以外的人
                    Dim _rang, Xpc, Ypc, Zpc, Xd, Yd, Zd As Single
                    Xpc = _Camera.X
                    Ypc = _Camera.Y
                    Zpc = _Camera.Z

                    Xd = PlayerObj(i).X - Xpc
                    Yd = PlayerObj(i).Y - Ypc
                    Zd = PlayerObj(i).Z - Zpc
                    _rang = Math.Sqrt(Xd ^ 2 + Yd ^ 2 + Zd ^ 2)
                    'g.DrawString("range=" & _rang, New Font("Arial", 9), drawBrush, New Point(300, 320))
                    If _rang < 200 Then      '小於50公尺
                        Dim thdW, thdA, _thdW, _thdA, thpcW, thpcA, DW, DA, _dd1, _dd2 As Double
                        thpcW = _Camera.Angle_W
                        thpcA = _Camera.Angle_A
                        thdW = Math.Atan2(Zd, Xd)
                        thdA = Math.Atan2(Yd, Math.Sqrt(Xd ^ 2 + Zd ^ 2))

                        _dd1 = Math.Sqrt(Xd ^ 2 + Zd ^ 2)
                        _thdW = thdW - thpcW

                        Xd = _dd1 * Math.Cos(_thdW)
                        Zd = _dd1 * Math.Sin(_thdW)

                        thdA = Math.Atan2(Yd, Xd)
                        _dd2 = Math.Sqrt(Xd ^ 2 + Yd ^ 2)
                        _thdA = thdA - thpcA

                        Xd = _dd2 * Math.Cos(_thdA)
                        _thdW = Math.Atan2(Zd, Xd)

                        If _thdW > 3.14 Then _thdW = _thdW - 6.28
                        If _thdW < -3.14 Then _thdW = _thdW + 6.28
                        If _thdA > 3.14 Then _thdA = _thdA - 6.28
                        If _thdA < -3.14 Then _thdA = _thdA + 6.28
                        DW = Math.Atan2(0.02, Camera_ViewD)
                        DA = Math.Atan2(0.015, Camera_ViewD)
                        If _thdW < DW + 0.4 And _thdW > -DW - 0.4 And _thdA < DA + 0.4 And _thdA > -DA - 0.4 Then
                            Dim _point(15) As Point
                            For _p = 0 To 15
                                Dim _x, _y, _z As Single
                                _x = PlayerObj(i).X + 0.2 * Math.Cos(Pi / 4 * _p)
                                If _p >= 0 And _p <= 7 Then
                                    _y = PlayerObj(i).Y + 0.7
                                Else
                                    _y = PlayerObj(i).Y - 1
                                End If
                                _z = PlayerObj(i).Z + 0.2 * Math.Sin(Pi / 4 * _p)
                                _point(_p) = Spoint_to_Ipoint(_Camera, _x, _y, _z)
                            Next
                            If PlayerObj(i).Clan = UserObj.Clan Then    '同陣營用藍色畫
                                drawpen.Color = Color.FromArgb(64, 225, 225)
                            Else    '不同陣營用紅色
                                drawpen.Color = Color.FromArgb(255, 64, 64)
                            End If
                            For _t = 0 To 6
                                g.DrawLine(drawpen, _point(_t), _point(_t + 1))
                            Next
                            g.DrawLine(drawpen, _point(7), _point(0))
                            For _t = 0 To 7
                                g.DrawLine(drawpen, _point(_t), _point(_t + 8))
                            Next
                            For _t = 8 To 14
                                g.DrawLine(drawpen, _point(_t), _point(_t + 1))
                            Next
                            g.DrawLine(drawpen, _point(15), _point(8))

                        End If '_thdW < DW + 0.4 And _thdW > -DW - 0.4 And _thdA < DA + 0.4 And _thdA > -DA - 0.4 
                    End If '_rang < 200
                End If ' PlayerObj(i).NickName <> UserObj.NickName
            End If 'PlayerObj(i).Enable 
        Next
    End Sub
    Function Spoint_to_Ipoint(ByVal _Camera As GD_Camera, ByVal _x As Single, ByVal _y As Single, ByVal _z As Single) As Point
        Dim thpcW, thpcA As Double
        Dim Xpc, Ypc, Zpc As Single
        Xpc = _Camera.X
        Ypc = _Camera.Y
        Zpc = _Camera.Z
        thpcW = _Camera.Angle_W
        thpcA = _Camera.Angle_A

        Dim Xd2, Yd2, Zd2, _d1, _d2 As Single
        Dim thdW2, thdA2, _thdW2, _thdA2 As Double
        Xd2 = _x - Xpc
        Yd2 = _y - Ypc
        Zd2 = _z - Zpc
        thdW2 = Math.Atan2(Zd2, Xd2)

        _d1 = Math.Sqrt(Xd2 ^ 2 + Zd2 ^ 2)
        _thdW2 = thdW2 - thpcW

        Xd2 = _d1 * Math.Cos(_thdW2)
        Zd2 = _d1 * Math.Sin(_thdW2)

        thdA2 = Math.Atan2(Yd2, Xd2)

        _d2 = Math.Sqrt(Xd2 ^ 2 + Yd2 ^ 2)
        _thdA2 = thdA2 - thpcA

        Xd2 = _d2 * Math.Cos(_thdA2)
        _thdW2 = Math.Atan2(Zd2, Xd2)
        Dim _point As Point
        Dim _xx, _yy As Single
        'If Math.Tan(_thdW2) = Single.NaN Then
        'Else
        '    _xx = -Camera_ViewD * Math.Tan(_thdW2)
        'End If

        _xx = -Camera_ViewD * Math.Tan(_thdW2)
        _yy = Camera_ViewD * Math.Tan(_thdA2)
        _point = New Point(0, 0)
        _point.X = (_xx + 0.02) * 800 / 0.04
        _point.Y = 600 - ((_yy + 0.015) * 600 / 0.03)
        Return _point
    End Function
End Module
