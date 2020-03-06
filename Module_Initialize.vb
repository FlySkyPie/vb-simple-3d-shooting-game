Module Module_Initialize
    Public Sub Start_Game(ByVal _account As String)
        Loading = True              '告訴Timer正在載入中 讓它去跑載入動畫
        Visable_LoginMenu(False)    '隱藏登入選單
        Initialize_Guns()           '載入槍枝資料
        Initialize_Items()          '載入物品資料
        Initialize_Bullet()           '初始化子彈陣列

        Initialize_PlayerObj()      '初始化人物陣列
        Load_PlayerData(_account)   '載入玩家資料
        Initialize_Rain()           '初始化雨滴陣列
        Initialize_CubeObj()        '初始化方塊物件
        Load_CubeObj()              '載入方塊

        GetCursorPos(Mouse_Loact(0)) : GetCursorPos(Mouse_Loact(1))


        Create_BOT(200, -1000)
        'Create_TestBOT()            '載入BOT

        ' SetCursorPos(Form1.Location.X + 400, Form1.Location.Y + 300)
        Menuing = False : Form1.Test002()
        Gaming = True
        Timer_Game.Enabled = True   '
        Loading = False
    End Sub

    '初始化顯示picturebox控件
    Public Sub Initialize_Screen()
        Screen = New PictureBox
        Screen.BackColor = Color.FromArgb(32, 32, 32)
        Screen.Location = New Point(0, 0)
        'Screen.Name = "TerminalBox"
        Screen.Size = New System.Drawing.Size(832, 600)
        Screen.Image = New Bitmap(832, 600)
        Screen.TabIndex = 1
        Screen.TabStop = False
        AddHandler Screen.MouseMove, AddressOf Screen_MouseMove
        AddHandler Screen.MouseDown, AddressOf Screen_MouseDown
        AddHandler Screen.MouseUp, AddressOf Screen_MouseUp
        Form1.Controls.Add(Screen)
    End Sub
    Public Sub Initialize_Timer() '初始化遊戲時鐘
        Timer_Game = New System.Timers.Timer
        AddHandler Timer_Game.Elapsed, AddressOf Timer_Game_Tick
        Timer_Game.Interval = 50
        Timer_Game.SynchronizingObject = Form1
        Timer_Game.Enabled = False

        Timer_GUI = New System.Timers.Timer
        AddHandler Timer_GUI.Elapsed, AddressOf Timer_GUI_Tick
        Timer_GUI.Interval = 35
        Timer_GUI.SynchronizingObject = Form1
        Timer_GUI.Enabled = True
    End Sub
    Public Sub Initialize_GUI_Object()
        For i = 0 To 20
            G_Lable(i) = New GUI_Lable
            G_Button(i) = New GUI_Button
            G_NumberSelect(i) = New GUI_NumberSelect
            G_Text(i) = New GUI_TextBox

            G_Lable(i).Visable = False
            G_Button(i).Visable = False
            G_NumberSelect(i).Visable = False
            G_Text(i).Visable = False
        Next
    End Sub
    Public Sub Load_GUI_Objects()
        '最上方
        '   最小化
        G_Button(2).Visable = True
        G_Button(2).Location = New Point(730, 3)
        G_Button(2).Size = New Size(26, 26)
        G_Button(2).Text = "_"
        '   離開
        G_Button(3).Visable = True
        G_Button(3).Location = New Point(770, 3)
        G_Button(3).Size = New Size(26, 26)
        G_Button(3).Text = "X"
        '登入介面
        '   帳號
        G_Lable(0).Visable = True
        G_Lable(0).Text = "帳號:"
        G_Lable(0).Location = New Point(280, 248)
        G_Lable(0).Font = New Font("Arial", 9)
        '   密碼
        G_Lable(1).Visable = True
        G_Lable(1).Text = "密碼:"
        G_Lable(1).Location = New Point(280, 272)
        G_Lable(1).Font = New Font("Arial", 9)
        '   帳號
        G_Text(0).Visable = True
        G_Text(0).Location = New Point(315, 245)
        G_Text(0).Size = New Size(200, 20)
        G_Text(0).Text = ""
        G_Text(0).PassWord = False
        '   密碼
        G_Text(1).Visable = True
        G_Text(1).Location = New Point(315, 270)
        G_Text(1).Size = New Size(200, 20)
        G_Text(1).Text = ""
        G_Text(1).PassWord = True
        '   登入按鈕
        G_Button(0).Visable = True
        G_Button(0).Location = New Point(280, 300)
        G_Button(0).Size = New Size(110, 40)
        G_Button(0).Text = "     登入"
        '   註冊按鈕
        G_Button(1).Visable = True
        G_Button(1).Location = New Point(410, 300)
        G_Button(1).Size = New Size(110, 40)
        G_Button(1).Text = "     註冊"
        '註冊介面
        '   帳號
        G_Lable(2).Visable = False
        G_Lable(2).Text = "帳號:"
        G_Lable(2).Location = New Point(100, 120)
        G_Lable(2).Font = New Font("Arial", 9)
        '   密碼
        G_Lable(3).Visable = False
        G_Lable(3).Text = "密碼:"
        G_Lable(3).Location = New Point(100, 155)
        G_Lable(3).Font = New Font("Arial", 9)
        '   重複密碼
        G_Lable(4).Visable = False
        G_Lable(4).Text = "確認您的密碼:"
        G_Lable(4).Location = New Point(100, 190)
        G_Lable(4).Font = New Font("Arial", 9)
        '   角色暱稱
        G_Lable(5).Visable = False
        G_Lable(5).Text = "角色暱稱:"
        G_Lable(5).Location = New Point(100, 225)
        G_Lable(5).Font = New Font("Arial", 9)
        '   遊戲章規
        G_Lable(6).Visable = False
        G_Lable(6).Text = "章規(作者的廢話):" & vbNewLine & "才沒有那種東西勒=3=,本軟體僅本人興趣製作," & vbNewLine & "開服時間隨本人高興,遊戲內的物品及角色資料(也就是電磁資料)皆沒有任何保障," & vbNewLine & "如果發現BUG歡迎提出,開發出外掛也請和本人交流交流xd.總之,祝各位遊戲愉快:)"
        G_Lable(6).Text += vbNewLine & "喔對了,因為懶得做其他GUI物件,直接用3個大按鈕來選擇陣營xd." & vbNewLine & "選擇陣營只會影響所屬勢力以及重生點,未來玩家還是可以自己創立或是加入/退出勢力," & vbNewLine & "這裡的ABC只是系統預設的3個勢力,所以這裡的選擇不影響日後遊戲"
        'G_Lable(6).Text += "其他注意事項:" & vbNewLine & ""
        G_Lable(6).Font = New Font("Arial", 9)
        G_Lable(6).Location = New Point(100, 260)
        '   帳號
        G_Text(2).Visable = False
        G_Text(2).Location = New Point(150, 118)
        G_Text(2).Size = New Size(200, 20)
        G_Text(2).Text = ""
        G_Text(2).PassWord = False
        '   密碼
        G_Text(3).Visable = False
        G_Text(3).Location = New Point(150, 153)
        G_Text(3).Size = New Size(200, 20)
        G_Text(3).Text = ""
        G_Text(3).PassWord = True
        '   確認密碼
        G_Text(4).Visable = False
        G_Text(4).Location = New Point(200, 188)
        G_Text(4).Size = New Size(200, 20)
        G_Text(4).Text = ""
        G_Text(4).PassWord = True
        '   暱稱
        G_Text(5).Visable = False
        G_Text(5).Location = New Point(170, 223)
        G_Text(5).Size = New Size(200, 20)
        G_Text(5).Text = ""
        G_Text(5).PassWord = False
        '   註冊按鈕
        G_Button(4).Visable = False
        G_Button(4).Location = New Point(100, 480)
        G_Button(4).Size = New Size(180, 40)
        G_Button(4).Text = "註冊為A陣營玩家"
        '   註冊按鈕
        G_Button(5).Visable = False
        G_Button(5).Location = New Point(300, 480)
        G_Button(5).Size = New Size(180, 40)
        G_Button(5).Text = "註冊為B陣營玩家"
        '   註冊按鈕
        G_Button(6).Visable = False
        G_Button(6).Location = New Point(500, 480)
        G_Button(6).Size = New Size(180, 40)
        G_Button(6).Text = "註冊為C陣營玩家"
        '   返回主畫面
        G_Button(7).Visable = False
        G_Button(7).Location = New Point(650, 540)
        G_Button(7).Size = New Size(130, 40)
        G_Button(7).Text = "返回登入畫面"
        '遊戲選單
        '-返回遊戲
        G_Button(8).Visable = False
        G_Button(8).Location = New Point(340, 400)
        G_Button(8).Size = New Size(130, 40)
        G_Button(8).Text = "    返回遊戲"
        '-角色資料
        G_Button(9).Visable = False
        G_Button(9).Location = New Point(340, 160)
        G_Button(9).Size = New Size(130, 40)
        G_Button(9).Text = "    角色資料"
        '-索取獎勵
        G_Button(10).Visable = False
        G_Button(10).Location = New Point(340, 220)
        G_Button(10).Size = New Size(130, 40)
        G_Button(10).Text = "    序號兌獎"
        '-遊戲設定
        G_Button(11).Visable = False
        G_Button(11).Location = New Point(340, 280)
        G_Button(11).Size = New Size(130, 40)
        G_Button(11).Text = "    遊戲設定"
        '序號兌獎
        '-輸入序號
        G_Lable(7).Visable = False
        G_Lable(7).Text = "請輸入序號:"
        G_Lable(7).Location = New Point(350, 260)
        G_Lable(7).Font = New Font("Arial", 9)
        '-輸入序號的地方
        G_Text(6).Visable = False
        G_Text(6).Location = New Point(200, 300)
        G_Text(6).Size = New Size(400, 20)
        G_Text(6).Text = ""
        G_Text(6).PassWord = False

    End Sub
    Public Sub Initialize_Guns()
        For i = 0 To 99
            WeaponData(i) = New Weapon
        Next
        '沒有武器 也就是赤手空拳xd
        WeaponData(0).Name = "Hand"           '名稱
        WeaponData(0).Weapon_Type = 0         '阿就空手唄
        WeaponData(0).Ammo_Type = "none"      '彈藥類型
        WeaponData(0).Delay = 10            '擊發延遲
        WeaponData(0).Shift = 0             '彈道飄移量 (標準差)
        WeaponData(0).ReloadTime = 0        '填裝時間
        WeaponData(0).Damage = 3            '對人傷害
        WeaponData(0).Speed = 0             '子彈飛行速度
        WeaponData(0).MaxRange = 100         '最大射程

        WeaponData(1).Name = "AK-47"        '名稱
        WeaponData(1).Weapon_Type = 1       '步槍
        WeaponData(1).Ammo_Type = "7.62x39" '彈藥類型
        WeaponData(1).Delay = 2             '擊發延遲
        WeaponData(1).Shift = 0.5 / 180 * Pi    '彈道飄移量 (標準差)   '5度
        WeaponData(1).ReloadTime = 45       '填裝時間 45*50ms=2.25 S
        WeaponData(1).Damage = 11           '對人傷害
        WeaponData(1).Speed = 40.5         ' m/per 50ms子彈飛行速度 (810M/s =40.5 M/50mS)
        WeaponData(1).MaxRange = 500         '最大射程

    End Sub
    Public Sub Initialize_Bullet()  '初始化飛行子彈陣列
        Dim i As Integer
        For i = 0 To Max_Bullet
            BulletObj(i) = New WorldObject_Bullet
            BulletObj(i).Enable = False
        Next
    End Sub
    Public Sub Initialize_Rain()  '初始化雨滴陣列
        Dim i As Integer
        For i = 0 To Max_Rain
            RainObj(i) = New WorldObject_Rain
            RainObj(i).Enable = False
        Next
        For j = 0 To 5000
            For i = 0 To Max_Rain
                If RainObj(i).Enable = False Then
                    RainObj(i).X = UserObj.X + Rnd() * 100 - 50
                    RainObj(i).Y = UserObj.Y + Rnd() * 50
                    RainObj(i).Z = UserObj.Z + Rnd() * 100 - 50
                    RainObj(i).Fx = 0
                    RainObj(i).Fy = -0.7
                    RainObj(i).Fz = 0
                    RainObj(i).Enable = True
                    Exit For
                End If
            Next
        Next
    End Sub
    Public Sub Initialize_Items()
        For i = 0 To Max_Item_Type
            ItemData(i) = New Item
        Next
        ItemData(0).Name = "7.62x39彈匣"          '物品名稱
        ItemData(0).Type = 0         '物品類型 (先用字串存,以後需要優化再改成編號)
        ItemData(0).Value = 30       '物品值
        ItemData(0).Explanation = "裝有30發7.62x39mm北約彈淡的彈匣"    '物品的說明文字
    End Sub

    '註冊
    Public Sub Register(ByVal _account As String, ByVal _password As String, ByVal _password2 As String, ByVal _nickname As String, ByVal _clan As String)
        If _account = "" Or _password = "" Or _password2 = "" Or _nickname = "" Then
            MsgBox("請填妥資料", MsgBoxStyle.OkOnly + MsgBoxStyle.Critical, "錯誤")
        ElseIf _password <> _password2 Then
            MsgBox("確認密碼必須和密碼相同", MsgBoxStyle.OkOnly + MsgBoxStyle.Critical, "錯誤")

        Else
            Dim fileExists As Boolean
            fileExists = My.Computer.FileSystem.FileExists(MonLocl & "\Data\User\" & _account & ".txt")
            If fileExists Then
                MsgBox("名為" & _account & "的帳號已經存在,再想一個吧!", MsgBoxStyle.OkOnly + MsgBoxStyle.Critical, "錯誤")
            Else
                Dim goodnickname As Boolean = True
                Dim h As String = ""
                Dim r = My.Computer.FileSystem.OpenTextFileReader(MonLocl & "\Data\Index\NickName.txt")
                Dim U As Integer = 0
                While Not r.EndOfStream
                    r.ReadLine()
                    U += 1
                End While
                r.Close()
                Dim r2 = My.Computer.FileSystem.OpenTextFileReader(MonLocl & "\Data\Index\NickName.txt")
                Dim playerd(U) As String
                U = 0
                While Not r2.EndOfStream
                    playerd(U) = r2.ReadLine()
                    If playerd(U) = _nickname Then
                        goodnickname = False
                        Exit While
                    End If

                    U += 1
                End While
                r2.Close()

                If goodnickname Then '填入的資料符合條件 開始創立帳號
                    Dim userdata As String
                    userdata = _password & vbNewLine    '密碼
                    userdata += _nickname & vbNewLine   '暱稱
                    userdata += "1" & vbNewLine           '等級
                    userdata += "0" & vbNewLine           '經驗值
                    userdata += "0" & vbNewLine           '剩餘技能點數
                    userdata += "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0" & vbNewLine       '技能
                    userdata += _clan & vbNewLine           '所屬陣營
                    userdata += "0 0" & vbNewLine           '擊殺數/死亡數
                    userdata += "1 0 0" & vbNewLine           '裝備武器
                    userdata += "30 0 0" & vbNewLine           '殘彈
                    userdata += "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0" & vbNewLine       '物品類型
                    userdata += "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0" & vbNewLine       '物品數量
                    userdata += "100" & vbNewLine           'HP
                    userdata += "1000" & vbNewLine           'MP
                    userdata += "0 0" & vbNewLine           '每日領$加權 /每日領$加權 歸零基準
                    userdata += "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0" & vbNewLine '成就 不要問 你會怕
                    Select Case _clan   '初始座標 x/ y
                        Case "A"
                            userdata += "0 0.85 -1000" & vbNewLine
                        Case "B"
                            userdata += "-866.025 0.85 500" & vbNewLine
                        Case "C"
                            userdata += "866.025 0.85 500" & vbNewLine
                    End Select
                    My.Computer.FileSystem.WriteAllText(MonLocl & "\Data\User\" & _account & ".txt", userdata, False)   '建立資料檔
                    My.Computer.FileSystem.WriteAllText(MonLocl & "\Data\Index\NickName.txt", _nickname & vbNewLine, True) '建立暱稱索引
                    '畫面 註冊->登入
                    Visable_LoginMenu(True)
                    Visable_Register(False)
                    MsgBox("註冊成功!", MsgBoxStyle.OkOnly + MsgBoxStyle.Information, "恭喜")
                Else
                    MsgBox("暱稱為" & _nickname & "的角色已經存在,再想一個吧!", MsgBoxStyle.OkOnly + MsgBoxStyle.Critical, "錯誤")
                End If
            End If
        End If
    End Sub
    '登入
    Public Sub Login(ByVal _account As String, ByVal _password As String)
        Dim fileExists As Boolean
        fileExists = My.Computer.FileSystem.FileExists(MonLocl & "\Data\User\" & _account & ".txt")
        If fileExists Then
            Dim playerdata As String
            Dim playerd() As String
            playerdata = My.Computer.FileSystem.ReadAllText(MonLocl & "\Data\User\" & _account & ".txt")
            playerd = playerdata.Split(vbNewLine)
            If playerd(0) <> _password Then
                MsgBox("請輸入正確的帳號密碼!", MsgBoxStyle.OkOnly + MsgBoxStyle.Critical, "錯誤")
            Else
                'MsgBox("密碼正確!", MsgBoxStyle.OkOnly + MsgBoxStyle.Information, "測試")
                Start_Game(_account) '開始啟動遊戲
            End If
        Else
            MsgBox("請輸入正確的帳號密碼!", MsgBoxStyle.OkOnly + MsgBoxStyle.Critical, "錯誤")
        End If
        G_Text(1).Text = "" '出於安全考量 消除密碼
    End Sub
    '從資料庫載入玩家資料
    Public Sub Load_PlayerData(ByVal _account As String)
        Dim playerd(18) As String
        Dim r = My.Computer.FileSystem.OpenTextFileReader(MonLocl & "\Data\User\" & _account & ".txt")
        Dim U As Integer = 0
        While Not r.EndOfStream
            playerd(U) = r.ReadLine()
            U += 1
        End While
        r.Close()

        '初始化玩家物件
        For i = 0 To Max_Player
            If PlayerObj(i).Enable = False Then
                UserObj = PlayerObj(i)
                Exit For
            End If
        Next
        UserObj.NickName = playerd(1)       '角色暱稱

        UserObj.Level = playerd(2)          '等級
        UserObj.Exp = playerd(3)            '經驗值
        UserObj.SkillPoint = playerd(4)     '技能點數
        Dim _Skill(31) As String
        _Skill = playerd(5).Split(" ")
        For i = 0 To 31
            UserObj.Skill(i) = _Skill(i)    '技能 
        Next
        UserObj.Clan = playerd(6)           '所屬陣營   (預設 A B C
        Dim KD(1) As String
        KD = playerd(7).Split(" ")
        UserObj.Kills = KD(0)               '擊殺數
        UserObj.Deaths = KD(1)              '死亡數
        Dim weapon(2) As String
        weapon = playerd(8).Split(" ")
        For i = 0 To 2
            UserObj.Weapon(i) = WeaponData(weapon(i))       '一次可以裝備三種武器
        Next
        Dim ammo_amount(2) As String
        ammo_amount = playerd(9).Split(" ")
        For i = 0 To 2
            UserObj.Ammo_Amount(i) = ammo_amount(i)         '殘彈
        Next
        Dim _Item_Type(31), _Item_Amount(31) As String
        _Item_Type = playerd(10).Split(" ")
        _Item_Amount = playerd(11).Split(" ")
        For i = 0 To 31
            UserObj.Item_Type(i) = _Item_Type(i)            '物品欄 (32格)
            UserObj.Item_Amount(i) = _Item_Amount(i)        '物品數量
        Next
        UserObj.HP = playerd(12)        '血量
        UserObj.MP = playerd(13)        '體力
        Dim _money(1) As String
        _money = playerd(14).Split(" ")
        UserObj.Money_Count = _money(0) '每日領$$加權
        UserObj.Money_Get = _money(1)   '每日領$$加權 歸零基準
        Dim _Achievement(50) As String
        _Achievement = playerd(15).Split(" ")
        For i = 0 To 50
            UserObj.Achievement(i) = _Achievement(i)   '成就
        Next
        '行為
        UserObj.Attacking = False       '是否攻擊
        UserObj.Attack_count = 0        '攻擊延遲計時器
        UserObj.Reload_Count = 0        '填裝延遲計時器
        UserObj.Moving = False          '是否移動
        UserObj.Running = False         '是否奔跑
        UserObj.Reloading = False         '是否奔跑
        UserObj.Driving = False         '是否駕駛載具
        '物理運算   '注意:物理運算距離單位統一使用m(公尺)
        Dim _Location(1) As String
        _Location = playerd(16).Split(" ")
        UserObj.X = _Location(0)            '所在座標x
        UserObj.Y = _Location(1)            '所在座標y
        UserObj.Z = _Location(2)            '所在座標y
        UserObj.FX = 0              '運動矢量x
        UserObj.FY = 0              '運動矢量y
        UserObj.Move_direct = 0     '移動角度
        UserObj.Angle_W = 0         '視角
        UserObj.Angle_A = 0         '視角
        UserObj.Weaponing = UserObj.Weapon(0)
        UserObj.Enable = True
        '初始化攝影機
        Client_Camera = New GD_Camera
    End Sub
    Public Sub Initialize_PlayerObj() '初始化BOT
        For i = 0 To Max_Player
            PlayerObj(i) = New WorldObject_Player
            PlayerObj(i).Enable = False
        Next
    End Sub
    Public Sub Initialize_CubeObj() '初始化方塊物件
        For i = 0 To Max_Cube
            CubeObj(i) = New WorldObject_Cube
        Next
    End Sub
    Public Sub Load_CubeObj()
        Dim j As Integer = 0

        For i = 0 To 13
            CubeObj(j).X = 9.5
            CubeObj(j).Y = 0.1 + i * 0.2
            CubeObj(j).Z = -1000 + i * 0.5
            CubeObj(j).Size_X = 2
            CubeObj(j).Size_Y = 0.2
            CubeObj(j).Size_Z = 0.5
            CubeObj(j).Enable = True
            j += 1
        Next
        CubeObj(j).X = 9.5
        CubeObj(j).Y = 0.1 + 14 * 0.2
        CubeObj(j).Z = -1000 + 13 * 0.5 + 0.625
        CubeObj(j).Size_X = 2
        CubeObj(j).Size_Y = 0.2
        CubeObj(j).Size_Z = 0.75
        CubeObj(j).Enable = True
        j += 1
        For i = 1 To 3
            CubeObj(j).X = 9.5
            CubeObj(j).Y = 2.9
            CubeObj(j).Z = -1000 + 14 * 0.5 + 1 * i
            CubeObj(j).Size_X = 2
            CubeObj(j).Size_Y = 0.2
            CubeObj(j).Size_Z = 1
            CubeObj(j).Enable = True
            j += 1
        Next

        For i = 0 To 10 '天花板
            For k = -10 To 10
                CubeObj(j).X = 8 - i
                CubeObj(j).Y = 2.9
                CubeObj(j).Z = -1000 + k
                CubeObj(j).Size_X = 1
                CubeObj(j).Size_Y = 0.2
                CubeObj(j).Size_Z = 1
                CubeObj(j).Enable = True
                j += 1
            Next
        Next




        For i = -10 To 10 '牆壁A
            For k = 0 To 2
                CubeObj(j).X = 10.9
                CubeObj(j).Y = 0.5 + k
                CubeObj(j).Z = -1000 + i
                CubeObj(j).Size_X = 0.8
                CubeObj(j).Size_Y = 1
                CubeObj(j).Size_Z = 1
                CubeObj(j).Enable = True
                j += 1
            Next
        Next
        For i = -10 To 10 '牆壁B
            For k = 0 To 2
                CubeObj(j).X = -2.9
                CubeObj(j).Y = 0.5 + k
                CubeObj(j).Z = -1000 + i
                CubeObj(j).Size_X = 0.8
                CubeObj(j).Size_Y = 1
                CubeObj(j).Size_Z = 1
                CubeObj(j).Enable = True
                j += 1
            Next
        Next

    End Sub

End Module
