Imports System.Timers
Module Module_Declaration
    Public Declare Function GetCursorPos Lib "user32" (ByRef point As Point) As Integer
    Public Declare Function SetCursorPos Lib "user32" (ByVal x As Integer, ByVal y As Integer) As Integer
    '變數宣告都放這好了=w= 看過學長用這種方法整理 我來也試試
    Public Const Pi = 3.141592653589
    Public Const Max_Bullet = 599
    Public Const Max_Item_Type = 99
    Public Const Max_Weapon_Type = 99
    Public Const Max_Rain = 10000
    Public Const Max_Player = 63
    Public Const Max_Cube = 1000

    Public Const MAX_TextBox = 20
    Public Const MAX_Lable = 20
    Public Const MAX_NumberSelect = 20
    Public Const MAX_Button = 20
    Public Const Version = "0.0.3"
    Public MonLocl As String = Application.StartupPath

    '圖形化用戶介面類的變數都放這裡
    Public TypeTag As GUI_TextBox
    Public TypeTaging As Boolean = False
    Public TypeShiny As Integer = 0

    Public Screen As PictureBox
    Public G_Text(MAX_TextBox) As GUI_TextBox
    Public G_Lable(MAX_Lable) As GUI_Lable
    Public G_NumberSelect(MAX_NumberSelect) As GUI_NumberSelect
    Public G_Button(MAX_Button) As GUI_Button

    'Public CursorP() As Point = {New Point(0, 0), New Point(16, 8), New Point(10, 10), New Point(8, 16)}
    Public Timer_Game, Timer_GUI As System.Timers.Timer


    '圖形化用戶介面變數
    Public Tq As Date = Now
    Public fps As Single
    Public Mouse_Loact(1) As Point

    Public key_up As Boolean
    Public key_right As Boolean
    Public key_down As Boolean
    Public key_left As Boolean

    '攝影機
    Public Client_Camera As GD_Camera
    Public Const Camera_ViewD = 0.05 '0.085 '0.0169


    Public DrawTime As Integer = 0
    '啟動LOGO動畫
    Public Animation_frame As Integer
    Public LOGO_Gear(39) As Point
    Public LOGO_Cat() As Point = {New Point(221, 75), New Point(238, 85), New Point(242, 58), New Point(250, 58), New Point(271, 85), New Point(300, 104), New Point(355, 103), New Point(391, 110), New Point(465, 121), New Point(534, 150), New Point(584, 176), New Point(638, 204), New Point(674, 214), New Point(681, 219), New Point(674, 234), New Point(658, 236), New Point(626, 226), New Point(595, 216), New Point(566, 204), New Point(546, 195), New Point(545, 205), New Point(560, 237), New Point(579, 263), New Point(597, 277), New Point(595, 307), New Point(594, 304), New Point(587, 333), New Point(574, 339), New Point(565, 333), New Point(564, 316), New Point(570, 298), New Point(564, 288), New Point(544, 286), New Point(527, 280), New Point(514, 299), New Point(501, 303), New Point(490, 303), New Point(482, 298), New Point(483, 292), New Point(493, 282), New Point(503, 266), New Point(492, 253), New Point(479, 242), New Point(463, 230), New Point(445, 218), New Point(418, 204), New Point(394, 193), New Point(366, 198), New Point(339, 204), New Point(311, 222), New Point(292, 242), New Point(274, 262), New Point(261, 279), New Point(253, 286), New Point(245, 290), New Point(237, 283), New Point(237, 267), New Point(247, 247), New Point(260, 217), New Point(238, 231), New Point(219, 249), New Point(206, 255), New Point(197, 246), New Point(198, 230), New Point(216, 212), New Point(250, 181), New Point(226, 180), New Point(207, 172), New Point(200, 162), New Point(201, 150), New Point(206, 146), New Point(206, 126), New Point(209, 104), New Point(225, 91)}

    Public Loading As Boolean = False  '載入中
    Public Gaming As Boolean = False  '遊戲進行中
    Public Menuing As Boolean = False    '選單操作
    '遊戲宣告
    Public PlayerObj(Max_Player) As WorldObject_Player
    Public UserObj As WorldObject_Player
    Public BulletObj(Max_Bullet) As WorldObject_Bullet
    Public RainObj(Max_Rain) As WorldObject_Rain
    Public CubeObj(Max_Cube) As WorldObject_Cube

    Public WeaponData(Max_Weapon_Type) As Weapon
    Public ItemData(Max_Item_Type) As Item
End Module
'======================================================================
'2015/08/18                 版本:0.0.3 
'   -提升為3D引擎,包含物理引擎變數、3D繪圖和滑鼠操作FPS化(注意:因為使用了修改滑鼠位置的API,在模擬沙盒中的windows中執行可能會造成部分異常)
'   -按下ESC鍵,背景半透明遮蔽,並且進入遊戲選單
'   -調整物件繪圖方式(提高視覺感受)
'2015/08/20
'   -優化地平線/網格繪圖
'   -血量以及體力條顯示
'2015/08/21
'   -新增雨水特效
'   -新增子彈繪圖
'2015/08/22
'   -修正註冊時NickName.txt讀取中而跳出錯誤視窗(此BUG由 阿歲 回報)
'   -新增BOT
'2015/08/23
'   -修正第一次載入沒有雨水問題
'   -優化彈道繪圖
'   -AK-47參數調整
'   -物理引擎微調
'   -死亡重生
'   -修改滑鼠操作程式讀取方式(固定鼠標改為相對鼠標,為了方便在Linux測試)
'   -隱藏鼠標(美化遊戲內鼠標)
'2015/08/25
'   -繪製方塊(尚未加入碰撞偵測)
'2015/08/26
'   -碰撞偵測(players to cubes玩家對方塊)
'   -優化碰撞偵測(可以站在方塊上)
'2015/08/28
'   -新增繪製玩家
'   -新增三維繪圖 三維點->二維點 轉換函式