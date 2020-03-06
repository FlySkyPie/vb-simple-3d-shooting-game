Option Explicit On
Imports System.Timers

Module Module_Timer_Tick
    'Public Declare Function SetCursorPos Lib "user32" (ByVal x As Integer, ByVal y As Integer) As Integer
    Public Sub Timer_Game_Tick(ByVal sender As System.Object, ByVal e As ElapsedEventArgs)
        Timer_Game.Enabled = False
        '控制
        If Gaming And Not Menuing Then
            UserObj.Moving = (key_up Or key_down Or key_right Or key_left)
            If UserObj.Moving Then
                Dim _move_x, _move_y As Integer
                _move_x = 0 : _move_y = 0
                If key_up Then _move_x = _move_x + 1
                If key_down Then _move_x = _move_x - 1
                If key_right Then _move_y = _move_y - 1
                If key_left Then _move_y = _move_y + 1
                UserObj.Move_direct = UserObj.Angle_W + Math.Atan2(_move_y, _move_x)
            Else
            End If

            '滑鼠游標相對操作
            GetCursorPos(Mouse_Loact(0))
            If Gaming And Not Menuing Then
                Dim _A, _W As Single
                _W = -(Mouse_Loact(0).X - Mouse_Loact(1).X) * 0.004
                _A = -(Mouse_Loact(0).Y - Mouse_Loact(1).Y) * 0.004

                UserObj.Angle_W += _W
                UserObj.Angle_A += _A
                If UserObj.Angle_A > 3.14 / 2 Then
                    UserObj.Angle_A = 3.14 / 2
                ElseIf UserObj.Angle_A < -3.14 / 2 Then
                    UserObj.Angle_A = -3.14 / 2
                End If
                If UserObj.Angle_W > 3.14 Then UserObj.Angle_W = UserObj.Angle_W - 6.28
                If UserObj.Angle_W < -3.14 Then UserObj.Angle_W = UserObj.Angle_W + 6.28
            End If
            Mouse_Loact(1) = New Point(Mouse_Loact(0).X, Mouse_Loact(0).Y)

            '滑鼠游標絕對操作
            'Dim _A, _W As Single
            '_W = -(Mouse_Loact.X - 400) * 0.004
            '_A = -(Mouse_Loact.Y - 300) * 0.004
            'SetCursorPos(Form1.Location.X + 400, Form1.Location.Y + 300)
            'UserObj.Angle_W += _W
            'UserObj.Angle_A += _A
            'If UserObj.Angle_A > 3.14 / 2 Then
            '    UserObj.Angle_A = 3.14 / 2
            'ElseIf UserObj.Angle_A < -3.14 / 2 Then
            '    UserObj.Angle_A = -3.14 / 2
            'End If
            'If UserObj.Angle_W > 3.14 Then UserObj.Angle_W = UserObj.Angle_W - 6.28
            'If UserObj.Angle_W < -3.14 Then UserObj.Angle_W = UserObj.Angle_W + 6.28
        End If


        '遊戲運算
        '-BOT的AI運算
        For i = 0 To Max_Player
            If PlayerObj(i).Enable Then
                Dim _n As String
                _n = PlayerObj(i).NickName
                If Mid(_n, 1, 3) = "BOT" Then   '這是一個BOT
                    PlayerObj(i).Angle_W = Math.Atan2(UserObj.Z - PlayerObj(i).Z, UserObj.X - PlayerObj(i).X)
                    PlayerObj(i).Move_direct = PlayerObj(i).Angle_W
                    PlayerObj(i).Moving = True
                    If PlayerObj(i).MP > 700 Then
                        PlayerObj(i).Running = True
                    ElseIf PlayerObj(i).MP < 300 Then
                        PlayerObj(i).Running = False
                    End If
                    'If Math.Sqrt((UserObj.Z - PlayerObj(i).Z) ^ 2 + (UserObj.X - PlayerObj(i).X) ^ 2) < 150 Then
                    '    PlayerObj(i).Attacking = True
                    'Else
                    '    PlayerObj(i).Attacking = False
                    'End If
                End If
            End If
        Next


        '-所有玩家的運算
        For i = 0 To Max_Player
            If PlayerObj(i).Enable Then
                If PlayerObj(i).HP > 0 Then '活著
                    '--移動操作
                    If PlayerObj(i).Running And PlayerObj(i).MP >= 5 Then
                        PlayerObj(i).MP += -5
                        Dim speed As Single = 0.17 'M per .0m Sec(M/10mS)  '考慮系統運算作出的妥協
                        PlayerObj(i).FX = speed * Math.Cos(PlayerObj(i).Angle_W)
                        PlayerObj(i).FZ = speed * Math.Sin(PlayerObj(i).Angle_W)
                    ElseIf PlayerObj(i).Moving Then
                        Dim speed As Single = 0.09 'M per 20m Sec(M/10mS)  '考慮系統運算作出的妥協
                        PlayerObj(i).FX = speed * Math.Cos(PlayerObj(i).Move_direct)
                        PlayerObj(i).FZ = speed * Math.Sin(PlayerObj(i).Move_direct)
                    End If


                    '--位移運算
                    '---重力
                    PlayerObj(i).FY += -0.49
                    '---碰撞以及位移
                    Dim _r As Single
                    _r = Math.Sqrt(PlayerObj(i).FX ^ 2 + PlayerObj(i).FY ^ 2 + PlayerObj(i).FZ ^ 2) / 0.1
                    '----切割向量
                    Dim _fx, _fy, _fz As Single
                    _fx = PlayerObj(i).FX / Int(_r)
                    _fy = PlayerObj(i).FY / Int(_r)
                    _fz = PlayerObj(i).FZ / Int(_r)
                    For tt = 0 To Int(_r)


                        '----碰撞地面
                        If PlayerObj(i).Y + _fy <= 1 Then
                            PlayerObj(i).FY = 0 : _fy = 0
                            PlayerObj(i).Y = 1
                        End If
                        '----碰撞方塊
                        For cb = 0 To Max_Cube
                            If CubeObj(cb).Enable Then
                                '使用"矩形碰撞"作為人物碰撞偵測的方法
                                '演算法參考 http://ten-dimensional.blogspot.tw/2014/05/blog-post_31.html


                                Dim _dx, _dy, _dz, _dmx, _dmy, _dmz As Single
                                _dmx = 0.2 + CubeObj(cb).Size_X / 2
                                _dmy = 0.7 + CubeObj(cb).Size_Y / 2
                                _dmz = 0.2 + CubeObj(cb).Size_Z / 2
                                _dx = Math.Abs(PlayerObj(i).X + _fx - CubeObj(cb).X)
                                _dy = Math.Abs(PlayerObj(i).Y + _fy - CubeObj(cb).Y)
                                _dz = Math.Abs(PlayerObj(i).Z + _fz - CubeObj(cb).Z)
                                If _dx < _dmx And _dy < _dmy And _dz < _dmz Then    '出現碰撞
                                    PlayerObj(i).FX = 0 : _fx = 0
                                    PlayerObj(i).FY = 0 : _fy = 0
                                    PlayerObj(i).FZ = 0 : _fz = 0
                                    Exit For
                                Else    '沒有發生碰撞 走樓梯用的對小格子進行碰撞判斷
                                    _dmx = 0.2 + CubeObj(cb).Size_X / 2
                                    _dmy = 0.15 + CubeObj(cb).Size_Y / 2
                                    _dmz = 0.2 + CubeObj(cb).Size_Z / 2
                                    _dx = Math.Abs(PlayerObj(i).X + _fx - CubeObj(cb).X)
                                    _dy = Math.Abs(PlayerObj(i).Y - 0.85 + _fy - CubeObj(cb).Y)
                                    _dz = Math.Abs(PlayerObj(i).Z + _fz - CubeObj(cb).Z)
                                    If _dx < _dmx And _dy < _dmy And _dz < _dmz Then
                                        PlayerObj(i).FY = 0 : _fy = 0
                                        PlayerObj(i).Y = CubeObj(cb).Y + CubeObj(cb).Size_Y / 2 + 1
                                        'Exit For
                                    End If
                                End If ' _dx < _dmx And _dy < _dmy And _dz < _dmz Then    '出現碰撞
                            End If 'CubeObj(cb).Enable
                        Next 'cb = 0 To Max_Cube



                        PlayerObj(i).X += _fx
                        PlayerObj(i).Y += _fy
                        PlayerObj(i).Z += _fz

                    Next 'tt = 1 To Int(_r / 0.1)
                    '玩家站在東西上面
                    If PlayerObj(i).FY = 0 Then
                        '摩擦力
                        Dim _d, _rw As Single
                        _d = Math.Atan2(PlayerObj(i).FZ, PlayerObj(i).FX)
                        _rw = Math.Sqrt(PlayerObj(i).FZ ^ 2 + PlayerObj(i).FX ^ 2)
                        If _rw > 0.02 Then _rw = _rw - 0.02 Else _rw = 0
                        PlayerObj(i).FX = _rw * Math.Cos(_d)
                        PlayerObj(i).FZ = _rw * Math.Sin(_d)
                    End If






                    '--開槍運算
                    If PlayerObj(i).Attack_count > 0 Then
                        PlayerObj(i).Attack_count += -1
                    End If
                    If PlayerObj(i).Reloading Then
                        If PlayerObj(i).Reload_Count > 0 Then
                            PlayerObj(i).Reload_Count += -1
                        Else
                            PlayerObj(i).Reloading = False
                            If PlayerObj(i).Ammo_Amount(PlayerObj(i).Weaponing_code) > 0 Then
                                PlayerObj(i).Ammo_Amount(PlayerObj(i).Weaponing_code) = 31
                            Else
                                PlayerObj(i).Ammo_Amount(PlayerObj(i).Weaponing_code) = 30
                            End If
                        End If
                    End If

                    If PlayerObj(i).Attacking And Not PlayerObj(i).Reloading Then
                        If PlayerObj(i).Attack_count = 0 Then '冷卻完畢 開槍
                            Dim _X = PlayerObj(i).X + 0.4 * Math.Cos(PlayerObj(i).Angle_W)
                            Dim _Y = PlayerObj(i).Y + 0.4 * Math.Sin(PlayerObj(i).Angle_W)

                            Create_BulletObj(PlayerObj(i))
                            PlayerObj(i).Attack_count = PlayerObj(i).Weaponing.Delay
                            PlayerObj(i).Ammo_Amount(PlayerObj(i).Weaponing_code) += -1
                            If PlayerObj(i).Ammo_Amount(PlayerObj(i).Weaponing_code) <= 0 Then
                                PlayerObj(i).Reload_Count = PlayerObj(i).Weaponing.ReloadTime
                                PlayerObj(i).Reloading = True
                            End If
                        End If
                    End If
                    '--體力回覆
                    If PlayerObj(i).MP < 1000 Then
                        PlayerObj(i).MP += 2
                        If PlayerObj(i).MP > 1000 Then PlayerObj(i).MP = 1000
                    End If

                Else '你已經死惹
                    PlayerObj(i).SpawnCount -= 1
                    If PlayerObj(i).SpawnCount <= 0 Then
                        Select Case PlayerObj(i).Clan
                            Case "A"
                                PlayerObj(i).X = 0 : PlayerObj(i).Y = 0.9 : PlayerObj(i).Z = -1000
                            Case "B"
                                PlayerObj(i).X = -866.025 : PlayerObj(i).Y = 0.9 : PlayerObj(i).Z = 500
                            Case "C"
                                PlayerObj(i).X = 866.025 : PlayerObj(i).Y = 0.9 : PlayerObj(i).Z = 500
                        End Select
                        PlayerObj(i).HP = 100
                        PlayerObj(i).MP = 1000
                        PlayerObj(i).Angle_A = 0 : PlayerObj(i).Angle_W = 0
                        PlayerObj(i).FX = 0 : PlayerObj(i).FY = 0 : PlayerObj(i).FZ = 0
                    End If
                End If

            End If 'PlayerObj(i).Enable
        Next

        '飛行子彈運算
        For i = 0 To Max_Bullet
            If BulletObj(i).Enable Then
                '飛太遠啦~銷毀子彈
                BulletObj(i).Count += 1
                If BulletObj(i).Count * BulletObj(i).Master.Speed > BulletObj(i).Master.MaxRange Then
                    BulletObj(i).Enable = False
                End If

                '重力
                BulletObj(i).FY -= 0.001

                Dim _t As Single
                _t = Math.Sqrt(BulletObj(i).FX ^ 2 + BulletObj(i).FY ^ 2 + BulletObj(i).FZ ^ 2) / 0.2
                For k = 1 To Int(_t) '將一個向量切割成很多次運算,避免飛行向量過大,直接穿過碰撞物件
                    ''方塊碰撞運算
                    'For cb = 0 To Max_Cube
                    '    If CubeObj(cb).Enable Then
                    '        Dim bool(2) As Boolean
                    '        bool(0) = (BulletObj(i).X + BulletObj(i).FX / Int(_t) < CubeObj(cb).X + CubeObj(cb).Size_X / 2) And (BulletObj(i).X + BulletObj(i).FX / Int(_t) > CubeObj(cb).X - CubeObj(cb).Size_X / 2)
                    '        bool(1) = (BulletObj(i).Y + BulletObj(i).FY / Int(_t) < CubeObj(cb).Y + CubeObj(cb).Size_Y / 2) And (BulletObj(i).Y + BulletObj(i).FY / Int(_t) > CubeObj(cb).Y - CubeObj(cb).Size_Y / 2)
                    '        bool(2) = (BulletObj(i).Z + BulletObj(i).FX / Int(_t) < CubeObj(cb).Z + CubeObj(cb).Size_Z / 2) And (BulletObj(i).Z + BulletObj(i).FZ / Int(_t) > CubeObj(cb).Z - CubeObj(cb).Size_Z / 2)
                    '        If bool(0) And bool(1) And bool(2) Then
                    '            BulletObj(i).Enable = False
                    '        End If
                    '    End If
                    'Next
                    'If BulletObj(i).Enable = False Then
                    '    Exit For
                    'End If
                    '讓子彈飛~~
                    BulletObj(i).X = BulletObj(i).X + BulletObj(i).FX / Int(_t)
                    BulletObj(i).Y = BulletObj(i).Y + BulletObj(i).FY / Int(_t)
                    BulletObj(i).Z = BulletObj(i).Z + BulletObj(i).FZ / Int(_t)
                    '命中判定
                    For j = 0 To Max_Player
                        If PlayerObj(j).Enable Then
                            If PlayerObj(j).HP > 0 Then
                                If BulletObj(i).Owner.Clan <> PlayerObj(j).Clan Then    '不同陣營才需要命中
                                    Dim _rang1, _rang2 As Single
                                    _rang1 = Math.Sqrt((PlayerObj(j).X - BulletObj(i).X) ^ 2 + (PlayerObj(j).Z - BulletObj(i).Z) ^ 2)
                                    _rang2 = Math.Abs(PlayerObj(j).Y - BulletObj(i).Y)
                                    If _rang1 <= 0.3 And _rang2 <= 0.85 Then    '命中玩家   '註:此範圍為圓柱體

                                        If PlayerObj(j).HP - BulletObj(i).Damage > 0 Then
                                            PlayerObj(j).HP = PlayerObj(j).HP - BulletObj(i).Damage
                                            BulletObj(i).Owner.Exp += BulletObj(i).Damage   '獲得經驗值
                                        Else '死亡判定
                                            BulletObj(i).Owner.Kills += 1                '獲得擊殺數
                                            BulletObj(i).Owner.Exp += PlayerObj(j).HP  '獲得經驗值
                                            BulletObj(i).Owner.Exp += 100   '殺敵獎勵-獲得經驗值
                                            PlayerObj(j).SpawnCount = 260
                                            PlayerObj(j).HP = 0
                                            PlayerObj(j).Deaths += 1                    '死亡數

                                        End If
                                        BulletObj(i).Enable = False
                                        Exit For
                                    End If
                                End If 'BulletObj(i).Owner.Clan <> PlayerObj(j).Clan
                            End If 'PlayerObj(j).HP > 0 
                        End If 'PlayerObj(j).Enable 
                    Next 'j = 0 To Max_Player
                    If BulletObj(i).Enable = False Then
                        Exit For
                    End If
                    If BulletObj(i).Y <= 0 Then '掉落地面
                        BulletObj(i).Enable = False
                        Exit For
                    End If
                Next



            End If 'BulletObj(i).Enable
        Next
        '下雨運算
        '-創造雨滴
        For j = 0 To 100
            For i = 0 To Max_Rain
                If RainObj(i).Enable = False Then
                    RainObj(i).X = Rnd() * 100 - 50 + UserObj.X
                    RainObj(i).Y = UserObj.Y + 50
                    RainObj(i).Z = Rnd() * 100 - 50 + UserObj.Z
                    RainObj(i).Fx = 0
                    RainObj(i).Fy = -0.7
                    RainObj(i).Fz = 0
                    RainObj(i).Enable = True
                    Exit For
                End If
            Next
        Next
        '-雨滴運動
        For i = 0 To Max_Rain
            If RainObj(i).Enable Then
                RainObj(i).X += RainObj(i).Fx
                RainObj(i).Y += RainObj(i).Fy
                RainObj(i).Z += RainObj(i).Fz
                If RainObj(i).Y < 0 Then
                    RainObj(i).Enable = False
                End If
            End If
        Next
        Timer_Game.Enabled = True
    End Sub
    Public Sub Timer_GUI_Tick(ByVal sender As System.Object, ByVal e As ElapsedEventArgs)
        Timer_GUI.Enabled = False

        '繪圖
        Dim GUI As Bitmap
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        GUI = New Bitmap(800, 600)
        Dim g As Graphics
        g = Graphics.FromImage(GUI)
        g.Clear(Color.FromArgb(32, 32, 32))

        If Gaming Then
            Draw_Game(GUI)
            If Menuing Then
                Dim g2 As Graphics
                Dim GUI2 As Bitmap
                GUI2 = New Bitmap(800, 600)
                g2 = Graphics.FromImage(GUI2)
                g2.Clear(Color.FromArgb(200, 32, 32, 32))
                g.DrawImage(GUI2, New Point(0, 0))
                Draw_GUI_Objects(GUI)
            End If
        Else
            Draw_GUI_Objects(GUI)
        End If
        If Loading Then
            Draw_Loading_Animation(GUI)
        End If

        Screen.Image = GUI
        Timer_GUI.Enabled = True
    End Sub

    Public Sub Draw_Loading_Animation(ByVal _bitmap As Bitmap)
        Dim teeths As Integer = 8
        If Animation_frame < 10 Then
            Animation_frame += 1
        Else
            Animation_frame = 0
        End If
        Dim drawFont As New Font("Arial", 9)
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        Dim g As Graphics
        g = Graphics.FromImage(_bitmap)
        drawBrush.Color = Color.FromArgb(64, 225, 225)
        'draw a gear
        For i = 0 To teeths * 5 - 1
            Dim angle As Double = 2 * 3.1415926 / (teeths * 5) * i + Animation_frame * 2 * 3.1415926 / teeths / 11
            Dim x, y, R As Integer
            Dim _d As Double = 0.05
            If i Mod 5 = 0 Then
                R = 120
                angle -= _d
            ElseIf (i - 1) Mod 5 = 0 Then
                R = 120
                angle += _d
            Else
                R = 80
            End If
            x = Int(R * Math.Cos(angle)) + 390
            y = Int(R * Math.Sin(angle)) + 325
            LOGO_Gear(i) = New Point(x, y)
        Next
        g.DrawPolygon(drawpen, LOGO_Gear)
        g.DrawEllipse(drawpen, 390 - 40, 325 - 40, 80, 80)
        g.DrawPolygon(drawpen, LOGO_Cat)
    End Sub
    Public Sub Draw_Game(ByVal _bitmap As Bitmap)
        '幀數運算
        If DrawTime >= 20 Then
            If (Now.Minute > Tq.Minute) Then
                fps = 1 / (60 + Now.Second + Now.Millisecond / 1000 - Tq.Second - Tq.Millisecond / 1000)
            Else
                fps = 1 / (Now.Second + Now.Millisecond / 1000 - Tq.Second - Tq.Millisecond / 1000)
            End If
            fps = Int(fps * 20 * 10) / 10
            Tq = Now
            DrawTime = 1
        Else
            DrawTime += 1
        End If
        '繪圖
        Dim drawFont As New Font("Tahoma", 9, FontStyle.Bold)
        Dim drawBrush As New SolidBrush(Color.FromArgb(64, 225, 225))
        Dim drawpen As New Pen(drawBrush)
        Dim g As Graphics
        g = Graphics.FromImage(_bitmap)
        drawBrush.Color = Color.FromArgb(64, 225, 225)
        '-圖像
        '--定位攝影機
        Client_Camera.X = UserObj.X
        Client_Camera.Y = UserObj.Y + 0.6 '(1+0.6)
        Client_Camera.Z = UserObj.Z
        Client_Camera.Angle_W = UserObj.Angle_W
        Client_Camera.Angle_A = UserObj.Angle_A

        '---繪製地圖線
        Draw_Horizon(_bitmap, Client_Camera)
        '---繪製玩家
        Draw_Player(_bitmap, Client_Camera)
        '---繪製子彈
        Draw_Bullect(_bitmap, Client_Camera)
        '---繪製方塊
        Draw_Cube(_bitmap, Client_Camera)
        '---繪製雨滴
        Draw_Rain(_bitmap, Client_Camera)
        '---繪製介面
        Draw_Game_HUD(_bitmap)

        '---輸出測試用數據
        drawBrush.Color = Color.FromArgb(64, 255, 255)
        drawFont = New Font("Arial", 9)
        Dim Stmp As String
        Stmp = "=====GUI=====" & vbNewLine
        Stmp += "FPS: " & fps & vbNewLine
        Stmp += "(" & Mouse_Loact(0).X & "," & Mouse_Loact(0).Y & ")" & vbNewLine
        Stmp += key_up & key_right & key_down & key_left & vbNewLine
        Stmp += "=====Players=====" & vbNewLine
        For i = 0 To Max_Player
            'PlayerObj(i)
            If PlayerObj(i).Enable Then
                Stmp += "Name:" & PlayerObj(i).NickName & vbNewLine
                Stmp += "HP:" & PlayerObj(i).HP & vbTab & "||MP:" & PlayerObj(i).MP & vbTab & "Exp:" & PlayerObj(i).Exp & vbNewLine
                Stmp += "K/D:" & PlayerObj(i).Kills & "/" & PlayerObj(i).Deaths & vbNewLine
                Stmp += "Location:(" & PlayerObj(i).X & "," & PlayerObj(i).Y & "," & PlayerObj(i).Z & ")" & vbNewLine
                Stmp += "F:(" & PlayerObj(i).FX & "," & PlayerObj(i).FY & "," & PlayerObj(i).FZ & ")" & vbNewLine
                Stmp += "Moving Direcion:" & PlayerObj(i).Move_direct & vbNewLine
                Stmp += "Speed:" & Math.Sqrt(PlayerObj(i).FX ^ 2 + PlayerObj(i).FY ^ 2 + PlayerObj(i).FZ ^ 2) & vbNewLine
                Stmp += "Moving:" & PlayerObj(i).Moving & "Running:" & PlayerObj(i).Running & vbNewLine
                Stmp += "Attacking:" & PlayerObj(i).Attacking & "/" & PlayerObj(i).Attack_count & vbNewLine
                Stmp += "Ammo:" & PlayerObj(i).Ammo_Amount(0) & vbNewLine
                Stmp += "Reload Time:" & PlayerObj(i).Reload_Count & vbNewLine
                Stmp += "Main Weapon:" & PlayerObj(i).Weapon(0).Name & vbNewLine
                Stmp += "=================" & vbNewLine
            End If
            'Stmp += "=====Camera=====" & vbNewLine
            'Stmp += "Location:(" & Client_Camera.X & "," & Client_Camera.Y & "," & Client_Camera.Z & ")" & vbNewLine
            'Stmp += "Angle_W:" & Client_Camera.Angle_W & vbNewLine
            'Stmp += "Angle_A:" & Client_Camera.Angle_A & vbNewLine
        Next
        g.DrawString(Stmp, drawFont, drawBrush, 1, 1)
    End Sub
   
End Module
