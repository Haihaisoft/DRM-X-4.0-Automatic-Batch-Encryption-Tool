' Haihaisoft Open Source DRM Provider: https://www.haihaisoft.com
' DRM-X Content Protection Platform: https://www.drm-x.com
' Project Page: https://www.drm-x.com/DRM-X-4.0-Automatic-Batch-Encryption-Tool.aspx

Imports System.ServiceModel
Imports System.Data.SQLite
Public Class LoginForm
    Inherits System.Windows.Forms.Form

    Private lblServer As New Label With {
        .Text = "DRM-X 4.0 Server:",
        .Location = New Point(30, 25),
        .Width = 150
    }
    Private rbInternational As New RadioButton With {
        .Text = "International",
        .Location = New Point(190, 25),
        .Width = 115,
        .Checked = True
    }

    Private rbChina As New RadioButton With {
        .Text = "China",
        .Location = New Point(310, 25),
        .Width = 80
    }

    Private lblEmail As New Label With {
        .Text = "DRM-X 4.0 Account:",
        .Location = New Point(30, 65),
        .Width = 150
    }

    Private txtAdminEmail As New TextBox With {
        .Location = New Point(190, 63),
        .Width = 215,
        .Text = "" 
    }

    Private lblAuthCode As New Label With {
        .Text = "Web Service Auth:",
        .Location = New Point(30, 105),
        .Width = 145
    }

    Private txtAuthString As New TextBox With {
        .Location = New Point(190, 105),
        .Width = 215,
        .UseSystemPasswordChar = True,
        .PasswordChar = "*"c,
        .Text = "" 
    }

    Private chkRememberMe As New CheckBox With {
        .Text = If(rbInternational.Checked, "Remember Me", "记住我"),
        .Location = New Point(90, 145), 
        .Width = 120
    }

    Private lblTutorialLink As New LinkLabel With {
        .Text = "Tutorial",
        .Location = New Point(340, 145),
        .Width = 270,
        .Height = 20,
        .LinkColor = Color.Blue,
        .ActiveLinkColor = Color.Red,
        .VisitedLinkColor = Color.Purple,
        .LinkBehavior = LinkBehavior.NeverUnderline
    }

    Private btnLogin As New Button With {
        .Text = "Sign In",
        .Location = New Point(80, 180),
        .Width = 130,
        .Height = 35
    }

    Private btnSignUp As New Button With {
        .Text = "Sign Up",
        .Location = New Point(220, 180),
        .Width = 130,
        .Height = 35
    }
    Private isProcessing As Boolean = False

    Public Sub New()
        InitializeForm()
        Me.AutoScaleMode = AutoScaleMode.Dpi 
        Me.Font = New Font("微软雅黑", 10) 
        LoadLoginInfo() 
    End Sub

    Private Sub InitializeForm()
        ' 窗体设置
        Me.Text = "DRM-X 4.0 Authentication"
        Me.Size = New Size(450, 300)
        Me.FormBorderStyle = FormBorderStyle.FixedDialog
        Me.StartPosition = FormStartPosition.CenterScreen
        Me.MaximizeBox = False
        Me.MinimizeBox = True  '保留最小化按钮

        Dim iconPath As String = "drmxIcon.ico"
        Dim myIcon As Icon = New Icon(iconPath)
        Me.Icon = myIcon

        ' 添加控件
        Me.Controls.AddRange({
            lblEmail, txtAdminEmail,
            lblAuthCode, txtAuthString, lblServer,
            rbInternational, rbChina,
            btnLogin, btnSignUp, lblTutorialLink, chkRememberMe
        })

        ' 事件绑定
        AddHandler btnLogin.Click, AddressOf btnLogin_Click
        AddHandler btnSignUp.Click, AddressOf btnSignUp_Click
        AddHandler lblTutorialLink.LinkClicked, AddressOf TutorialLink_Clicked
        ' 在RadioButton的CheckedChanged事件中调用：
        AddHandler rbChina.CheckedChanged, AddressOf ServerChanged
        AddHandler rbInternational.CheckedChanged, AddressOf ServerChanged
    End Sub
    Private Sub ServerChanged(sender As Object, e As EventArgs)
        If rbInternational.Checked OrElse rbChina.Checked Then
            SaveRegion() ' 独立保存区域
            SetLanguage(rbChina.Checked)
        End If
    End Sub
    Private Sub SaveRegion()
        My.Settings.SelectedRegion = If(rbInternational.Checked, "International", "China")
        My.Settings.Save()
    End Sub
    ' 根据选择的区域设置界面语言：
    Private Sub SetLanguage(isChina As Boolean)
        If isChina Then
            lblEmail.Text = "DRM-X 4.0 账号："
            lblAuthCode.Text = "Web 服务验证码："
            rbChina.Text = "中国版"
            rbInternational.Text = "国际版"
            Me.Text = "DRM-X 4.0 身份验证"
            btnLogin.Text = "登录"
            btnSignUp.Text = "注册"
            lblTutorialLink.Text = "使用教程"
            chkRememberMe.Text = "记住我"
            lblServer.Text = "DRM-X 4.0 服务器："
            rbChina.Checked = True
        Else
            lblEmail.Text = "DRM-X 4.0 Account:"
            lblAuthCode.Text = "Web Service Auth:"
            rbChina.Text = "China"
            rbInternational.Text = "International"
            Me.Text = "DRM-X 4.0 Authentication"
            btnLogin.Text = "Sign In"
            btnSignUp.Text = "Sign Up"
            lblTutorialLink.Text = "Tutorial"
            chkRememberMe.Text = "Remember Me"
            lblServer.Text = "DRM-X 4.0 Server:"
            rbInternational.Checked = True
        End If
    End Sub


    Private Sub ExecuteNonQuery(conn As SQLiteConnection, sql As String)
        Using cmd As New SQLiteCommand(sql, conn)
            cmd.ExecuteNonQuery()
        End Using
    End Sub

    ' 保存登录信息
    Private Sub SaveLoginInfo()
        My.Settings.RememberMe = chkRememberMe.Checked
        My.Settings.SelectedRegion = If(rbInternational.Checked, "International", "China")

        If chkRememberMe.Checked Then
            My.Settings.AdminEmail = txtAdminEmail.Text.Trim()
            My.Settings.EncryptedAuthString = SecurityHelper.Encrypt(txtAuthString.Text.Trim())
        Else
            My.Settings.AdminEmail = ""
            My.Settings.EncryptedAuthString = ""
        End If
        My.Settings.Save()
    End Sub

    ' 加载登录信息
    Private Sub LoadLoginInfo()
        Try
            ' 优先加载区域选择
            If Not String.IsNullOrEmpty(My.Settings.SelectedRegion) Then
                If My.Settings.SelectedRegion = "International" Then
                    rbInternational.Checked = True
                Else
                    rbChina.Checked = True
                End If
            End If

            ' 加载账号信息
            If My.Settings.RememberMe AndAlso
           Not String.IsNullOrEmpty(My.Settings.AdminEmail) AndAlso
           Not String.IsNullOrEmpty(My.Settings.EncryptedAuthString) Then

                txtAdminEmail.Text = My.Settings.AdminEmail
                txtAuthString.Text = SecurityHelper.Decrypt(My.Settings.EncryptedAuthString)
                chkRememberMe.Checked = True
            End If
        Catch ex As Exception
            My.Settings.Reset()
        End Try
    End Sub

    Private Sub btnLogin_Click(sender As Object, e As EventArgs)

        ' 邮箱格式验证
        If Not IsValidEmail(txtAdminEmail.Text.Trim()) Then
            MessageBox.Show("Invalid email format!")
            Return
        End If

        If String.IsNullOrWhiteSpace(txtAuthString.Text) Then
            MessageBox.Show("Please enter the Web Auth String!")
            Return
        End If

        If isProcessing Then Return
        isProcessing = True

        Dim AdminID As Integer = ValidateCredentials()
        If AdminID > 0 Then
            SaveLoginInfo() ' 新增：登录成功后保存信息
            ' 登录成功后打开主窗体
            Dim encryptionForm As New DRMX4.EncryptionTool.EncryptionForm(Me.IsInternationalVersion, txtAdminEmail.Text.Trim(), txtAuthString.Text.Trim())
            encryptionForm.Show()
            Me.Hide()
        ElseIf AdminID = -1 Then
            MessageBox.Show(If(Me.IsInternationalVersion, "WebServiceAuthStr can not be empty, please set WebServiceAuthStr before login.", "Web服务验证码不能为空，请先设置验证码。"))
            isProcessing = False
        ElseIf AdminID = -2 Then
            MessageBox.Show(If(Me.IsInternationalVersion, "Admin type requires Premier account or above.", "DRM-X账号类型须使用高级版或以上！"))
            isProcessing = False
        Else
            MessageBox.Show(If(Me.IsInternationalVersion, "AdminEmail or WebServiceAuthStr is wrong.", "DRM-X账号或者Web服务验证码错误！"))
            isProcessing = False
        End If

    End Sub
    Private Sub btnSignUp_Click(sender As Object, e As EventArgs)
        Dim url As String = If(rbInternational.Checked,
                            "https://4.drm-x.com/Register.aspx",
                            "https://4.drm-x.cn/Register.aspx")
        Process.Start(url)
    End Sub

    Private Sub TutorialLink_Clicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Try
            Process.Start("https://www.drm-x.com/DRM-X-4.0-Automatic-Batch-Encryption-Tool.aspx")
            lblTutorialLink.LinkVisited = True ' 标记为已访问
        Catch ex As Exception
            MessageBox.Show(If(IsInternationalVersion,
                "Failed to open tutorial page",
                "无法打开教程页面"))
        End Try
    End Sub
    Private Function IsValidEmail(email As String) As Boolean
        Try
            Dim addr = New System.Net.Mail.MailAddress(email)
            Return addr.Address = email
        Catch
            Return False
        End Try
    End Function

    Private Function ValidateCredentials() As String
        Using drm = CreateServiceClient() ' 使用工厂方法
            Try

                Return drm.WebServiceLogin(txtAdminEmail.Text.Trim(), txtAuthString.Text)

            Catch ex As TimeoutException
                Throw New Exception("Service timeout, please check network")
            Catch ex As FaultException
                Throw New Exception("Invalid service request")
            Catch ex As CommunicationException
                Throw New Exception("Communication error")
            End Try
        End Using
    End Function

    Private Function CreateServiceClient() As Object
        Return If(rbInternational.Checked,
        New DRMX_EN.HaihaisoftLicenseServiceSoapClient(),
        New DRMX_CN.HaihaisoftLicenseServiceSoapClient())
    End Function

    Public ReadOnly Property IsInternationalVersion As Boolean
        Get
            Return rbInternational.Checked
        End Get
    End Property
End Class
