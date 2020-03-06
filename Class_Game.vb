'動態物件
Public Class WorldObject_Player    '資料庫的玩家物件
    Public Enable As Boolean
    '這些屬於長期保存的資料
    'Public Account As String        '帳號
    'Public Password As String       '密碼 (以後考慮要加密?
    Public NickName As String       '角色暱稱
    Public Level As Integer         '等級
    Public Exp As Integer           '經驗值
    Public SkillPoint As Integer    '技能點數
    Public Skill(31) As Boolean     '技能 

    Public Clan As String           '所屬陣營   (預設 A B C
    Public Kills As Integer          '擊殺數
    Public Deaths As Integer         '死亡數

    Public Weaponing_code As Integer
    Public Weaponing As Weapon
    Public Weapon(2) As Weapon      '一次可以裝備三種武器
    Public Ammo_Amount(2) As Integer
    Public Item_Type(31) As Integer '物品欄 (32格)
    Public Item_Amount(31) As Integer    '物品數量

    Public SpawnCount As Integer = 0
    Public HP As Integer            '血量
    Public MP As Integer            '體力

    Public Money_Count As Integer   '每日領$$加權(累積天數)
    Public Money_Get As Boolean     '每日領$$加權 歸零基準(今天領了沒)

    Public Achievement(50) As Boolean   '成就

    '行為
    Public Attacking As Boolean     '是否攻擊
    Public Reloading As Boolean     '是否填裝
    Public Attack_count As Integer  '攻擊延遲計時器
    Public Reload_Count As Integer  '填裝延遲計時器
    Public Moving As Boolean        '是否移動
    Public Running As Boolean       '是否奔跑
    Public Driving As Boolean       '是否駕駛載具
    Public Vehicle As WorldObject_Vehicle       '所在載具

    '物理運算   '注意:物理運算距離單位統一使用m(公尺)
    Public X As Single              '所在座標x
    Public Y As Single              '所在座標y
    Public Z As Single              '所在座標Z
    Public FX As Single              '運動矢量x
    Public FY As Single              '運動矢量y
    Public FZ As Single              '運動矢量z

    Public Move_direct As Single    '移動角度
    Public Const R = 0.3            '半徑
    Public Angle_W As Double        '視角
    Public Angle_A As Double        '視角
    Function KD() As Single
        Return Kills / Deaths
    End Function
End Class

Public Class WorldObject_Vehicle                '載具
    Public Type As Integer          '載具類型     
    'Public Name As String          '載具名稱
    Public AP As Integer            '護甲值
    Public X As Single              '座標
    Public Y As Single              '座標
    Public FX As Single             '運動矢量
    Public FY As Single             '運動矢量
    Public Angle_D As Double        '駕駛角度
    Public Angle_A As Double        '攻擊手角度
    Public Members(1) As WorldObject_Player    '載具成員    通常 0-駕駛 1-攻擊手
    '汗馬車/吉普車
    'APC
    '戰車
    '直昇機
    'F16(輕戰鬥機)
    'A-10(重戰鬥機)
End Class

Public Class WorldObject_Bullet '子彈-飛行物件
    Public Enable As Boolean
    Public Owner As WorldObject_Player 'String        '發射者
    Public Master As Weapon
    Public X As Single              '座標x
    Public Y As Single              '座標y
    Public Z As Single              '座標y
    Public FX As Single              '矢量x
    Public FY As Single              '矢量y
    Public FZ As Single              '矢量y
    Public Count As Integer         '計時器 (用來決定飛行距離)
    Public Damage As Integer        '傷害
    'Public Sub Move(ByVal _d As Double)
    '    X = X + Speed * Math.Cos(Angle)
    '    Y = Y + Speed * Math.Sin(Angle)
    'End Sub
End Class

Public Class WorldObject_Rain
    Public Enable As Boolean
    Public X, Y, Z As Single
    Public Fx, Fy, Fz As Single
End Class

Public Class WorldObject_Cube
    Public Enable As Boolean
    Public X As Single
    Public Y As Single
    Public Z As Single
    Public Size_X As Single
    Public Size_Y As Single
    Public Size_Z As Single
End Class
'靜態物件 通常用來當做資料、設定讀取
Public Class Weapon
    Public Name As String           '名稱
    Public Weapon_Type As Integer   '武器類型 0近戰 1步槍 2手槍 3霰彈槍 4火砲
    Public Ammo_Type As String      '彈藥類型
    Public Delay As Integer         '擊發延遲
    Public Shift As Single          '彈道飄移量 (標準差)
    Public ReloadTime As Integer    '填裝時間
    Public Damage As Integer        '對人傷害
    Public Speed As Single          '子彈飛行速度
    Public MaxRange As Single       '最大射程
End Class

Public Class Item
    Public Name As String           '物品名稱
    Public Type As String           '物品類型 (先用字串存,以後需要優化再改成編號)
    Public Value As Integer         '物品值
    Public Explanation As String    '物品的說明文字
End Class