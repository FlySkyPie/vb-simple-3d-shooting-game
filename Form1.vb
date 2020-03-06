Public Class Form1
    'Public Declare Function SetCursorPos Lib "user32" (ByVal x As Integer, ByVal y As Integer) As Integer


    Private Sub Form1_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        If Gaming Then
            e.Handled = True
            Select Case e.KeyValue
                Case Keys.Escape
                    If Menuing Then
                        'SetCursorPos(Me.Location.X + 400, Me.Location.Y + 300)
                        Menuing = False : Test002()
                        GetCursorPos(Mouse_Loact(0)) : GetCursorPos(Mouse_Loact(1))
                        '隱藏其他menu物件
                        Visable_GameMenu(False)
                        Visable_CodeGift(False)
                    Else
                        Menuing = True : Test001()
                        Visable_GameMenu(True)
                    End If
            End Select
        End If
        '遊戲操作類指令(鍵盤操作)
        If Gaming And Not Menuing Then
            e.Handled = True
            Select Case e.KeyValue
                Case Keys.W
                    key_up = True
                Case Keys.A
                    key_left = True
                Case Keys.D
                    key_right = True
                Case Keys.S
                    key_down = True
                Case Keys.R
                    If Not UserObj.Reloading Then
                        UserObj.Reload_Count = UserObj.Weapon(0).ReloadTime
                        UserObj.Reloading = True
                    End If
                Case Keys.ShiftKey
                    UserObj.Running = True
                Case Keys.Up
                    UserObj.Angle_A += 0.02
                    If UserObj.Angle_A > 3.14 / 2 Then UserObj.Angle_A = 3.14 / 2
                Case Keys.Down
                    UserObj.Angle_A -= 0.02
                    If UserObj.Angle_A < -3.14 / 2 Then UserObj.Angle_A = -3.14 / 2
                Case Keys.Right
                    UserObj.Angle_W -= 0.02
                Case Keys.Left
                    UserObj.Angle_W += 0.02
                    If UserObj.Angle_W > 3.14 Then UserObj.Angle_W = UserObj.Angle_W - 6.28
                    If UserObj.Angle_W < -3.14 Then UserObj.Angle_W = UserObj.Angle_W + 6.28
            End Select
        End If
    End Sub
    Private Sub Form1_KeyUp(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyUp
        If Gaming Then
            e.Handled = True
            If e.KeyValue = Keys.W Then key_up = False
            If e.KeyValue = Keys.A Then key_left = False
            If e.KeyValue = Keys.D Then key_right = False
            If e.KeyValue = Keys.S Then key_down = False
            If e.KeyValue = Keys.ShiftKey Then UserObj.Running = False
        End If
    End Sub
    Private Sub Form1_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles Me.KeyPress
        If TypeTaging Then
            If Asc(e.KeyChar) >= 32 And Asc(e.KeyChar) <= 126 Then
                TypeTag.Text += e.KeyChar

            ElseIf e.KeyChar = vbBack Then
                Dim _str As String = TypeTag.Text
                If _str.Length >= 1 Then
                    TypeTag.Text = Mid(_str, 1, _str.Length - 1)
                End If
            End If
        End If
    End Sub
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Initialize_GUI_Object()
        Load_GUI_Objects()
        Initialize_Screen()
        Initialize_Timer()
        Mouse_Loact = {New Point(0, 0), New Point(0, 0)}

        Test001()

        ''Dim _b As Bitmap = New Bitmap(16, 16)
        ''_b = Image.FromFile(MonLocl & "\Cursor\3dgarro.cur")
        ''Me.Cursor = New Cursor(_b.GetHicon)
        ''Me.Cursor = New Cursor(MonLocl & "\Cursor\3dgarro.cur")
        'Dim bmp As New Bitmap(16, 16)
        'Dim g As Graphics = Graphics.FromImage(bmp)
        'g.SmoothingMode = Drawing2D.SmoothingMode.HighQuality

        'g.FillEllipse(Brushes.Aquamarine, New Rectangle(0, 0, 15, 15))
        'Dim CursorP() As Point = {New Point(0, 0), New Point(16, 8), New Point(10, 10), New Point(8, 32)}
        ''Dim drawpen As New Pen(Brushes.Aquamarine)
        ''g.DrawPolygon(drawpen, CursorP)
        'g.Dispose()
        'Me.Cursor = New Cursor(bmp.GetHicon)
        'bmp.Dispose()
    End Sub
    Private Sub Form1_Deactivate(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Deactivate
        If Gaming Then
            Menuing = True : Test001()
            Visable_GameMenu(True)
        End If
    End Sub


    '一切都是為了鼠標
    'The code that below are copy form:
    'https://www.daniweb.com/software-development/vbnet/threads/394082/color-custom-cursor-appears-to-be-black-in-vb-net
    'very thanks this guy :)

    Private Structure IconInfo
        Public fIcon As Boolean
        Public xHotspot As Integer
        Public yHotspot As Integer
        Public hbmMask As IntPtr
        Public hbmColor As IntPtr
    End Structure
    <System.Runtime.InteropServices.DllImport("user32.dll")> _
    Private Shared Function GetIconInfo(ByVal hIcon As IntPtr, ByRef pIconInfo As IconInfo) As Boolean
    End Function
    <System.Runtime.InteropServices.DllImport("user32.dll")> _
    Private Shared Function CreateIconIndirect(ByRef icon As IconInfo) As IntPtr
    End Function

    Public Sub Test001()
        ' This call is required by the Windows Form Designer.
        InitializeComponent()
        ' Add any initialization after the InitializeComponent() call.
        'Get the Hicon from the bitmap

        Dim _b As Bitmap = New Bitmap(16, 16)
        Dim g As Graphics = Graphics.FromImage(_b)
        g.SmoothingMode = Drawing2D.SmoothingMode.HighQuality
        Dim CursorP() As Point = {New Point(0, 0), New Point(16, 8), New Point(10, 10), New Point(8, 16)}
        Dim drawpen As New Pen(Brushes.Aquamarine)
        drawpen.Width = 2 : drawpen.Color = Color.FromArgb(64, 225, 225)
        g.DrawPolygon(drawpen, CursorP)

        Dim BitmapPtr As IntPtr = _b.GetHicon
        Dim IconInfo As IconInfo = New IconInfo()
        Dim CursorPtr As IntPtr
        'This will set the hbmMask and Color fields of the
        'IconInfo structure
        GetIconInfo(BitmapPtr, IconInfo)
        'Now you can set the x and y hotspot
        IconInfo.xHotspot = 0
        IconInfo.yHotspot = 0
        IconInfo.fIcon = False 'True for Icon; False for cursor
        'Get a handle for the cursor
        CursorPtr = CreateIconIndirect(IconInfo)
        'Now create the cursor from the handle
        Me.Cursor = New Cursor(CursorPtr)
        g.Dispose()
    End Sub
    Public Sub Test002()
        ' This call is required by the Windows Form Designer.
        InitializeComponent()
        ' Add any initialization after the InitializeComponent() call.
        'Get the Hicon from the bitmap

        Dim _b As Bitmap = New Bitmap(16, 16)
        Dim g As Graphics = Graphics.FromImage(_b)
        g.SmoothingMode = Drawing2D.SmoothingMode.HighQuality

        Dim BitmapPtr As IntPtr = _b.GetHicon
        Dim IconInfo As IconInfo = New IconInfo()
        Dim CursorPtr As IntPtr
        'This will set the hbmMask and Color fields of the
        'IconInfo structure
        GetIconInfo(BitmapPtr, IconInfo)
        'Now you can set the x and y hotspot
        IconInfo.xHotspot = 0
        IconInfo.yHotspot = 0
        IconInfo.fIcon = False 'True for Icon; False for cursor
        'Get a handle for the cursor
        CursorPtr = CreateIconIndirect(IconInfo)
        'Now create the cursor from the handle
        Me.Cursor = New Cursor(CursorPtr)
        g.Dispose()
    End Sub
End Class
