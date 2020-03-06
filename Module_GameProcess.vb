Module Module_GameProcess
    Public Sub Create_TestBOT()
        For i = 0 To 10
            Create_BOT(UserObj.X + 1000 * Rnd() - 500, UserObj.Z + 1000 * Rnd() - 500)
        Next
    End Sub

    Public Sub Create_BOT(ByVal _x As Single, ByVal _z As Single)
        For i = 0 To Max_Player
            If PlayerObj(i).Enable = False Then
                PlayerObj(i).NickName = "BOT" & Format(i, "000")
                PlayerObj(i).Level = 1         '等級
                PlayerObj(i).Exp = 0          '經驗值
                PlayerObj(i).SkillPoint = 0   '技能點數
                For j = 0 To 31
                    PlayerObj(i).Skill(j) = False   '技能 
                    PlayerObj(i).Item_Type(j) = 0 '物品欄 (32格)
                    PlayerObj(i).Item_Amount(j) = 0  '物品數量
                Next

                PlayerObj(i).Clan = "C"     '所屬陣營   (預設 A B C
                PlayerObj(i).Kills = 0          '擊殺數
                PlayerObj(i).Deaths = 0         '死亡數


                '一次可以裝備三種武器
                PlayerObj(i).Weapon(0) = WeaponData(1) 'AK-47
                PlayerObj(i).Weapon(1) = WeaponData(0)
                PlayerObj(i).Weapon(2) = WeaponData(0)
                PlayerObj(i).Weaponing = PlayerObj(i).Weapon(0)

                PlayerObj(i).Ammo_Amount = {31, 0, 0}

                PlayerObj(i).HP = 100           '血量
                PlayerObj(i).MP = 1000           '體力

                PlayerObj(i).Money_Count = 0   '每日領$$加權(累積天數)
                PlayerObj(i).Money_Get = False    '每日領$$加權 歸零基準(今天領了沒)

                PlayerObj(i).Achievement(50) = False   '成就

                '行為
                PlayerObj(i).Attacking = False     '是否攻擊
                PlayerObj(i).Reloading = False     '是否填裝
                PlayerObj(i).Attack_count = 0  '攻擊延遲計時器
                PlayerObj(i).Reload_Count = 0 '填裝延遲計時器
                PlayerObj(i).Moving = False        '是否移動
                PlayerObj(i).Running = False       '是否奔跑
                PlayerObj(i).Driving = False      '是否駕駛載具
                'Public Vehicle As WorldObject_Vehicle       '所在載具

                '物理運算   '注意:物理運算距離單位統一使用m(公尺)
                PlayerObj(i).X = _x             '所在座標x
                PlayerObj(i).Y = 0.85            '所在座標y
                PlayerObj(i).Z = _z            '所在座標Z
                'PlayerObj(i).FX As Single              '運動矢量x
                'PlayerObj(i).FY As Single              '運動矢量y
                'PlayerObj(i).FZ As Single              '運動矢量z

                PlayerObj(i).Move_direct = 0    '移動角度
                'PlayerObj(i).Const(R = 0.3)            '半徑
                PlayerObj(i).Angle_W = 0        '視角
                PlayerObj(i).Angle_A = 0     '視角
                PlayerObj(i).Enable = True
                Exit For
            End If
        Next
    End Sub
    Public Sub Create_BulletObj(ByVal _owner As WorldObject_Player)
        Dim i As Integer
        For i = 0 To Max_Bullet
            If BulletObj(i).Enable = False Then

                BulletObj(i).Owner = _owner
                BulletObj(i).Master = _owner.Weaponing
                Dim _gunwidth As Single
                _gunwidth = 0.5 * (1 / Math.Cos(_owner.Angle_A))
                BulletObj(i).X = _owner.X + _gunwidth * Math.Cos(_owner.Angle_A) * Math.Cos(_owner.Angle_W)
                BulletObj(i).Y = _owner.Y + 0.5 + _gunwidth * Math.Sin(_owner.Angle_A)
                BulletObj(i).Z = _owner.Z + _gunwidth * Math.Cos(_owner.Angle_A) * Math.Sin(_owner.Angle_W)

                Dim _AngleW, _AngleA As Double '
                _AngleW = _owner.Angle_W + _owner.Weaponing.Shift * NormSInv(Rnd) '常態分布著彈
                _AngleA = _owner.Angle_A + _owner.Weaponing.Shift * NormSInv(Rnd) '常態分布著彈
                BulletObj(i).FX = _owner.Weaponing.Speed * Math.Cos(_AngleA) * Math.Cos(_AngleW)
                BulletObj(i).FY = _owner.Weaponing.Speed * Math.Sin(_AngleA)
                BulletObj(i).FZ = _owner.Weaponing.Speed * Math.Cos(_AngleA) * Math.Sin(_AngleW)

                BulletObj(i).Count = 0
                BulletObj(i).Damage = _owner.Weaponing.Damage
                BulletObj(i).Enable = True
                Exit For        '跳脫回圈
            End If
        Next
    End Sub
    Public Function NormSInv(ByVal p As Double) As Double   '符合常態分布的亂數
        Const a1 = -39.6968302866538, a2 = 220.946098424521, a3 = -275.928510446969
        Const a4 = 138.357751867269, a5 = -30.6647980661472, a6 = 2.50662827745924
        Const b1 = -54.4760987982241, b2 = 161.585836858041, b3 = -155.698979859887
        Const b4 = 66.8013118877197, b5 = -13.2806815528857, c1 = -0.00778489400243029
        Const c2 = -0.322396458041136, c3 = -2.40075827716184, c4 = -2.54973253934373
        Const c5 = 4.37466414146497, c6 = 2.93816398269878, d1 = 0.00778469570904146
        Const d2 = 0.32246712907004, d3 = 2.445134137143, d4 = 3.75440866190742
        Const p_low = 0.02425, p_high = 1 - p_low
        Dim q As Double, r As Double
        If p < 0 Or p > 1 Then
            Err.Raise(vbObjectError, , "NormSInv: Argument out of range.")
        ElseIf p < p_low Then
            q = (-2 * Math.Log(p)) ^ 0.5
            NormSInv = (((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6) / _
               ((((d1 * q + d2) * q + d3) * q + d4) * q + 1)
        ElseIf p <= p_high Then
            q = p - 0.5 : r = q * q
            NormSInv = (((((a1 * r + a2) * r + a3) * r + a4) * r + a5) * r + a6) * q / _
               (((((b1 * r + b2) * r + b3) * r + b4) * r + b5) * r + 1)
        Else
            q = (-2 * Math.Log(1 - p)) ^ 0.5
            NormSInv = -(((((c1 * q + c2) * q + c3) * q + c4) * q + c5) * q + c6) / _
               ((((d1 * q + d2) * q + d3) * q + d4) * q + 1)
        End If
    End Function
End Module
