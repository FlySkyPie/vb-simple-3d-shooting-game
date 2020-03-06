Module Module_Screen
    Public Sub Screen_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        'Mouse_Loact(0) = New Point(e.X, e.Y)
        'If Gaming And Not Menuing Then
        '    Dim _A, _W As Single
        '    _W = -(Mouse_Loact(0).X - Mouse_Loact(1).X) * 0.008
        '    _A = -(Mouse_Loact(0).Y - Mouse_Loact(1).Y) * 0.008

        '    UserObj.Angle_W += _W
        '    UserObj.Angle_A += _A
        '    If UserObj.Angle_A > 3.14 / 2 Then
        '        UserObj.Angle_A = 3.14 / 2
        '    ElseIf UserObj.Angle_A < -3.14 / 2 Then
        '        UserObj.Angle_A = -3.14 / 2
        '    End If
        '    If UserObj.Angle_W > 3.14 Then UserObj.Angle_W = UserObj.Angle_W - 6.28
        '    If UserObj.Angle_W < -3.14 Then UserObj.Angle_W = UserObj.Angle_W + 6.28
        'End If
        'Mouse_Loact(1) = New Point(Mouse_Loact(0).X, Mouse_Loact(0).Y)
    End Sub
    Public Sub Screen_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        Dim gameclik As Boolean = True
        '打字方塊判定
        TypeTaging = False
        For i = 0 To MAX_TextBox
            If G_Text(i).Visable Then
                If e.X > G_Text(i).Location.X And e.Y > G_Text(i).Location.Y And e.X < G_Text(i).Location.X + G_Text(i).Size.Width And e.Y < G_Text(i).Location.Y + G_Text(i).Size.Height Then
                    TypeTag = G_Text(i)
                    TypeTaging = True : gameclik = False
                    TypeShiny = 2
                    Exit For
                End If
            End If
        Next
        '按鈕方塊判定
        For i = 0 To MAX_Button
            If G_Button(i).Visable Then
                If e.X > G_Button(i).Location.X And e.Y > G_Button(i).Location.Y And e.X < G_Button(i).Location.X + G_Button(i).Size.Width And e.Y < G_Button(i).Location.Y + G_Button(i).Size.Height Then
                    Button_Click(i) : gameclik = False
                    Exit For
                End If
            End If

        Next
        If gameclik Then    '如果不是按在控件上 就是屬於遊戲的操作
            If Gaming Then
                UserObj.Attacking = True
            End If
        End If
    End Sub
    Public Sub Screen_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        If Gaming Then
            UserObj.Attacking = False
        End If
    End Sub
    Public Sub Button_Click(ByVal _k As Integer)
        Select Case _k
            Case 0 '登入按鈕
                '帳密驗證寫這在這裡
                Login(G_Text(0).Text, G_Text(1).Text)
            Case 1 '畫面 登入->註冊
                Visable_LoginMenu(False)
                Visable_Register(True)
            Case 2  '最小化
                Form1.WindowState = 1
            Case 3  '離開
                Timer_GUI.Enabled = False
                Timer_Game.Enabled = False
                Form1.Dispose()
            Case 4 '註冊A
                Register(G_Text(2).Text, G_Text(3).Text, G_Text(4).Text, G_Text(5).Text, "A")
            Case 5 '註冊B
                Register(G_Text(2).Text, G_Text(3).Text, G_Text(4).Text, G_Text(5).Text, "B")
            Case 6 '註冊C
                Register(G_Text(2).Text, G_Text(3).Text, G_Text(4).Text, G_Text(5).Text, "C")
            Case 7 '畫面 註冊->登入
                Visable_LoginMenu(True)
                Visable_Register(False)
            Case 8  '返回遊戲 
                Menuing = False
                Cursor.Hide() '隱藏鼠標
                Visable_GameMenu(False)
            Case 9  '角色資料
            Case 10 '領取獎勵
                Visable_GameMenu(False)
                Visable_CodeGift(True)
            Case 11 '遊戲設定
        End Select
    End Sub
    Public Sub Visable_LoginMenu(ByVal _b As Boolean)
        '   帳號
        G_Lable(0).Visable = _b
        '   密碼
        G_Lable(1).Visable = _b
        '   帳號
        G_Text(0).Visable = _b
        '   密碼
        G_Text(1).Visable = _b
        '   登入按鈕
        G_Button(0).Visable = _b
        '   註冊按鈕
        G_Button(1).Visable = _b
        For i = 0 To 1
            G_Text(i).Text = "" '出於安全考量 清除登入資料
        Next
    End Sub
    Public Sub Visable_Register(ByVal _b As Boolean)
        '   帳號
        G_Lable(2).Visable = _b
        '   密碼
        G_Lable(3).Visable = _b
        '   重複密碼
        G_Lable(4).Visable = _b
        '   角色暱稱
        G_Lable(5).Visable = _b
        '   遊戲章規
        G_Lable(6).Visable = _b
        '   帳號
        G_Text(2).Visable = _b
        '   密碼
        G_Text(3).Visable = _b
        '   確認密碼
        G_Text(4).Visable = _b
        '   暱稱
        G_Text(5).Visable = _b
        '   註冊按鈕
        G_Button(4).Visable = _b
        '   註冊按鈕
        G_Button(5).Visable = _b
        '   註冊按鈕
        G_Button(6).Visable = _b
        '   返回主畫面
        G_Button(7).Visable = _b
        If Not _b Then
            For i = 2 To 5
                G_Text(i).Text = "" '出於安全考量 清除註冊資料
            Next
        End If
    End Sub
    Public Sub Visable_GameMenu(ByVal _b As Boolean)
        '遊戲選單
        '-返回遊戲
        G_Button(8).Visable = _b
        '-角色資料
        G_Button(9).Visable = _b
        '-索取獎勵
        G_Button(10).Visable = _b
        '-遊戲設定
        G_Button(11).Visable = _b
    End Sub
    Public Sub Visable_CodeGift(ByVal _b As Boolean)
        '-輸入序號
        G_Lable(7).Visable = _b
        '-輸入序號的地方
        G_Text(6).Visable = _b
    End Sub
End Module
