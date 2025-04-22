' Haihaisoft Open Source DRM Provider: https://www.haihaisoft.com
' DRM-X Content Protection Platform: https://www.drm-x.com
' Project Page: https://www.drm-x.com/DRM-X-4.0-Automatic-Batch-Encryption-Tool.aspx

Imports System.IO
Imports System.Threading
Imports System.Runtime.InteropServices
Imports System.Collections.Concurrent
Imports System.ServiceModel
Imports System.Data.SQLite
Imports System.Security.Cryptography
Imports System.Text
Imports System.Timers

Namespace DRMX4.EncryptionTool
    Public Module NativeMethods
        <DllImport("shlwapi.dll", CharSet:=CharSet.Unicode)>
        Public Function StrCmpLogicalW(psz1 As String, psz2 As String) As Integer
        End Function
    End Module
    Public Class EncryptionForm

        Inherits Form

        Private manualTab As New TabPage
        Private autoTab As New TabPage

        Private WithEvents btnStart As New Button
        Private WithEvents btnStop As New Button
        Private licensePanel As New Panel
        Private inputPanel As New Panel
        Private outputPanel As New Panel
        Private AutoInputPanel As New Panel
        Private AutoOutputPanel As New Panel

        Private WithEvents btnAutoStart As New Button
        Private WithEvents btnAutoStop As New Button
        Private WithEvents btnClearList As Button

        Private manualInputTextBox As TextBox
        Private manualOutputTextBox As TextBox
        Private autoInputTextBox As TextBox
        Private autoOutputTextBox As TextBox
        Private lblStatus As Label
        Private statusLabel As Label


        Private WithEvents folderBrowserDialog As New FolderBrowserDialog()

        Private tabControl As New TabControl

        Private _isInternational As Boolean
        Private _adminEmail As String
        Private _authString As String

        Private profileDict As New Dictionary(Of String, Integer)
        Private cmbProfile As New ComboBox

        '文件列表控件
        Private WithEvents manualFileListView As New ListView
        '过滤文件的后缀
        Private ReadOnly allowedExtensions As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase) From {
        ".mp4", ".webm", ".mp3", ".pdf", ".html", ".htm",
        ".bmp", ".gif", ".png", ".jpg", ".jpeg", ".webp",
        ".svg", ".js", ".css"}

        Private createFolderCheckBox As CheckBox
        Private convertPDFCheckBox As CheckBox
        Private convertPDFCheckBoxAuto As CheckBox
        Private cancellationTokenSource As New CancellationTokenSource()
        Private runningProcesses As New Concurrent.ConcurrentBag(Of Process)()

        Private profileID As String

        ' 自动扫描加密
        Private fileSystemWatcher As FileSystemWatcher
        Private processingQueue As Concurrent.ConcurrentQueue(Of String)
        Private autoCts As CancellationTokenSource


        Private isProcessing As Boolean = False
        Private isSutoProcessing As Boolean = False

        Private WithEvents autoFileListView As New ListView
        Private processedFiles As New Concurrent.ConcurrentDictionary(Of String, Boolean)

        ' 重试队列（线程安全）
        Private retryQueue As New ConcurrentQueue(Of RetryFileInfo)
        ' 重试配置
        Private Const MaxRetryAttempts As Integer = 5 ' 最大重试次数
        Private Const InitialRetryDelay As Integer = 5000 ' 初始重试间隔5秒
        Private ReadOnly retryDelays As Integer() = {5000, 10000, 20000, 40000, 60000} ' 指数退避策略

        Private _templateCreationProgress As IProgress(Of String)

        Private folderProcessingQueue As New Concurrent.ConcurrentQueue(Of String)
        Private folderWatcher As FileSystemWatcher

        Private pdfConversionSemaphore As New SemaphoreSlim(1, 1) ' 同时只允许一个PDF转换


        '根据所选择的服务器，使用不同的接口地址
        Private Function CreateServiceClient() As Object
            Return If(_isInternational,
                    New DRMX_EN.HaihaisoftLicenseServiceSoapClient(),
                    New DRMX_CN.HaihaisoftLicenseServiceSoapClient())
        End Function

        ' 步骤2：修改排序比较器
        Private Class NaturalFileComparer
            Implements IComparer(Of String)

            Public Function Compare(x As String, y As String) As Integer _
                Implements IComparer(Of String).Compare
                Return NativeMethods.StrCmpLogicalW(
                    Path.GetFileName(x),
                    Path.GetFileName(y)
                )
            End Function
        End Class

        Public Sub New(isInternational As Boolean, adminEmail As String, authString As String)

            _isInternational = isInternational
            _adminEmail = adminEmail
            _authString = authString

            InitializeForm()
            InitializeTabs()
            Me.AutoScaleMode = AutoScaleMode.Dpi ' 设置DPI自动缩放
            Me.Font = New Font("微软雅黑", 10) ' 使用清晰字体
            SetLanguage(_isInternational)

        End Sub

        Private Sub InitializeForm()
            Me.Text = "DRM-X 4.0 Automatic Batch Encryption Tool"
            Me.Size = New Size(900, 580)
            Me.StartPosition = FormStartPosition.CenterScreen

            ' 添加以下三行禁用窗口缩放
            Me.FormBorderStyle = FormBorderStyle.FixedDialog
            Me.MaximizeBox = False
            Me.MinimizeBox = True  '保留最小化按钮

            Dim iconPath As String = "drmxIcon.ico"
            Dim myIcon As Icon = New Icon(iconPath)
            Me.Icon = myIcon

            ' 主布局
            tabControl.Dock = DockStyle.Fill
            Me.Controls.Add(tabControl)

        End Sub


        Private Sub InitializeTabs()
            ' 手动加密选项卡
            manualTab = New TabPage With {.Text = "Manual Encryption"}
            InitializeManualTab(manualTab)
            tabControl.TabPages.Add(manualTab)

            ' 自动加密选项卡
            autoTab = New TabPage With {.Text = "Auto Encryption"}
            InitializeAutoTab(autoTab)
            tabControl.TabPages.Add(autoTab)
            InitializeDatabase()
        End Sub

        Private Sub InitializeManualTab(container As Control)
            ' 输入输出目录
            inputPanel = CreateFilePanel("Input Directory:", "Browse", 20, "Input")
            manualInputTextBox = CType(inputPanel.Controls.OfType(Of TextBox)().First(), TextBox)


            outputPanel = CreateFilePanel("Output Directory:", "Browse", 60, "Output")
            manualOutputTextBox = CType(outputPanel.Controls.OfType(Of TextBox)().First(), TextBox)

            ' 许可证模板选择
            licensePanel = New Panel With {
            .Height = 40,
            .Top = 110,
            .Width = 470
        }
            licensePanel.Controls.Add(New Label With {
            .Text = "License Profile:",
            .Left = 20,
            .Top = 2,
            .Width = 130
        })
            cmbProfile = New ComboBox With {
            .Left = 150,
            .Width = 300,
            .DropDownStyle = ComboBoxStyle.DropDownList
        }
            licensePanel.Controls.Add(cmbProfile)



            '-----------------------------------
            Using drmClient = CreateServiceClient()
                Try
                    Dim result As String = drmClient.ListLicenseProfilesAsString(_adminEmail, _authString, -1)

                    Console.WriteLine(result)
                    ' 数据解析与填充
                    If Not String.IsNullOrEmpty(result) Then
                        ' 分割所有条目（过滤空条目）
                        Dim allEntries = result.Split(New String() {";;"}, StringSplitOptions.RemoveEmptyEntries)

                        For Each entry In allEntries
                            ' 分割ID和Name（使用2段式分割确保安全）
                            Dim parts = entry.Split(New String() {"||"}, StringSplitOptions.None)

                            ' 验证数据有效性
                            If parts.Length >= 2 Then
                                cmbProfile.Items.Add($"{parts(0)}|{parts(1)}") ' 名称|ID
                                profileDict($"{parts(0)}|{parts(1)}") = CInt(parts(0))
                            End If
                        Next

                        ' 配置ComboBox显示方式
                        cmbProfile.DisplayMember = "DisplayName"
                        cmbProfile.ValueMember = "ID"

                        ' 默认选择第一项（如果有数据）
                        If cmbProfile.Items.Count > 0 Then
                            cmbProfile.SelectedIndex = 0
                        End If
                    End If
                    cmbProfile.EndUpdate()

                Catch ex As TimeoutException
                    Throw New Exception("Service timeout, please check network")
                Catch ex As FaultException
                    Throw New Exception("Invalid service request")
                Catch ex As CommunicationException
                    Throw New Exception("Communication error")
                End Try
            End Using

            '------------------------------



            ' 选项面板
            Dim optionsPanel As New Panel With {.Height = 40, .Top = 140, .Width = 450}

            createFolderCheckBox = New CheckBox With {
                .Text = "Create folder in output dir",
                .Left = 25,
                .Top = 8,
                .Width = 200,
                .Checked = True
            }
            optionsPanel.Controls.Add(createFolderCheckBox)

            convertPDFCheckBox = New CheckBox With {
                .Text = "Convert PDF to HTML",
                .Left = 250,
                .Top = 8,
                .Width = 200
            }
            optionsPanel.Controls.Add(convertPDFCheckBox)

            ' Start按钮
            btnStart = New Button With {
                .Text = "Start",
                .Width = 100,
                .Top = 450,
                .Left = 30,
                .Height = 50,
                .Enabled = True
            }

            ' Stop按钮
            btnStop = New Button With {
                .Text = "Stop",
                .Width = 100,
                .Top = 450,
                .Left = 150,
                .Height = 50,
                .Enabled = False
            }

            container.Controls.Add(btnStart)
            container.Controls.Add(btnStop)

            ' 文件列表初始化
            manualFileListView = New ListView With {
                .View = View.Details,
                .Top = 180,
                .Height = 260,
                .Anchor = AnchorStyles.Left Or AnchorStyles.Top,
                .Width = 870,
                .FullRowSelect = True,
                .GridLines = True,
                .Scrollable = True
            }
            manualFileListView.Columns.Add(If(_isInternational, "File Name", "文件名"), 250)
            manualFileListView.Columns.Add(If(_isInternational, "Input Path", "输入目录"), 220)
            manualFileListView.Columns.Add(If(_isInternational, "Output Path", "输出目录"), 200)
            manualFileListView.Columns.Add(If(_isInternational, "Size", "文件大小"), 90)
            manualFileListView.Columns.Add(If(_isInternational, "Status", "状态"), 100)

            ' 添加右键菜单
            Dim contextMenu As New ContextMenuStrip
            contextMenu.Items.Add(If(_isInternational, "🔄 Refresh", "🔄 刷新"), Nothing, AddressOf RefreshFileList)
            contextMenu.Items.Add(If(_isInternational, "📂 Open Input File Path", "📂 打开输入文件目录"), Nothing, AddressOf OpenFileLocation)
            contextMenu.Items.Add(If(_isInternational, "📁 Open Output File Path", "📁 打开输出文件目录"), Nothing, AddressOf OpenOutputLocation)
            manualFileListView.ContextMenuStrip = contextMenu

            ' 添加双击事件
            AddHandler manualFileListView.DoubleClick, AddressOf OpenSelectedFile

            container.Controls.AddRange({inputPanel, outputPanel, licensePanel, optionsPanel, manualFileListView, btnStart, btnStop})
        End Sub


        ' Start按钮点击事件
        Private Async Sub btnStart_Click(sender As Object, e As EventArgs) Handles btnStart.Click

            If Not Directory.Exists(manualInputTextBox.Text) Or Not Directory.Exists(manualOutputTextBox.Text) Then
                If _isInternational Then
                    MessageBox.Show("Please select the input and output Path first.")
                Else
                    MessageBox.Show("请先选择输入和输出目录。")
                End If
                Return
            End If

            If cmbProfile.SelectedItem Is Nothing Then
                If _isInternational Then
                    MessageBox.Show("Please select License Profile！")
                Else
                    MessageBox.Show("请选择许可证模板!")
                End If
                Return
            Else
                Dim key = cmbProfile.SelectedItem.ToString()
                Dim id = profileDict(key)
                profileID = id
            End If


            If isProcessing Then Return
            isProcessing = True

            ' 切换按钮状态
            btnStart.Enabled = False
            btnStop.Enabled = True


            cancellationTokenSource = New CancellationTokenSource()
            Dim token = cancellationTokenSource.Token


            ' 获取输入文件夹信息
            Dim inputFolder As New DirectoryInfo(manualInputTextBox.Text)
            Dim outputRoot As New DirectoryInfo(manualOutputTextBox.Text)

            ' 处理勾选创建操作
            If createFolderCheckBox.Checked Then
                CreateProfileFolder(inputFolder, outputRoot)
            End If

            Try
                Await Task.Run(Async Function()

                                   If convertPDFCheckBox.Checked Then
                                       ' 步骤1：PDF转换
                                       Await ConvertPDFToHTML(inputFolder, outputRoot, token)

                                   End If
                                   ' 步骤2：仅在转换成功时继续
                                   token.ThrowIfCancellationRequested()

                                   ' 步骤3：加载文件列表
                                   BeginInvoke(Sub() LoadFilesToListView(inputFolder.FullName))

                                   Await Task.Delay(100, token) ' 确保文件系统更新

                                   ' 步骤4：加密文件
                                   Await EncryptFilesAsync(inputFolder, outputRoot, token)

                               End Function, token)

            Catch ex As OperationCanceledException
                UpdateUI(Sub() LogError("Operation Cancelled" & vbCrLf))
            Catch ex As Exception
                UpdateUI(Sub() MessageBox.Show($"The operation failed: {ex.Message}"))
            Finally
                UpdateUI(Sub()
                             btnStart.Enabled = True
                             btnStop.Enabled = False
                             isProcessing = False
                         End Sub)
            End Try

        End Sub

        ' 创建配置文件夹方法
        Private Sub CreateProfileFolder(inputFolder As DirectoryInfo, outputRoot As DirectoryInfo)
            Dim profileFolderName = $"{profileID}_{inputFolder.Name}"
            Dim outputPath = Path.Combine(outputRoot.FullName, profileFolderName)

            If Not Directory.Exists(outputPath) Then
                Directory.CreateDirectory(outputPath)
            End If
        End Sub

        ' PDF转换方法
        Private Async Function ConvertPDFToHTML(inputFolder As DirectoryInfo, outputRoot As DirectoryInfo, token As CancellationToken) As Task(Of Boolean)

            Try

                Dim pdfFiles = Await Task.Run(Function()
                                                  Dim validFiles As New List(Of String)
                                                  ' 使用Invoke确保线程安全
                                                  manualFileListView.Invoke(Sub()
                                                                                For Each item As ListViewItem In manualFileListView.Items
                                                                                    Try
                                                                                        ' 检查所有可能为空的属性
                                                                                        If item IsNot Nothing AndAlso
                                                                                 item.Tag IsNot Nothing AndAlso
                                                                                 Not String.IsNullOrWhiteSpace(item.Tag.ToString()) AndAlso
                                                                                 File.Exists(item.Tag.ToString()) AndAlso
                                                                                 Path.GetExtension(item.Tag.ToString()).Equals(".pdf", StringComparison.OrdinalIgnoreCase) Then

                                                                                            validFiles.Add(item.Tag.ToString())

                                                                                        End If
                                                                                    Catch ex As Exception
                                                                                        ' 记录异常但不中断流程
                                                                                        LogError($"Item verification failed: {ex.Message}")
                                                                                    End Try
                                                                                Next
                                                                            End Sub)
                                                  Return validFiles
                                              End Function)

                'If Not pdfFiles.Any() Then
                '   UpdateUI(Sub() MessageBox.Show("未找到PDF文件"))
                '   Return False
                'End If

                ' 并行处理每个PDF文件（带错误捕获）
                Await Task.WhenAll(pdfFiles.Select(Function(file) ProcessSinglePDF(file, token)).ToArray())

                'UpdateUI(Sub() MessageBox.Show("PDF转换完成，点击【确定】继续加密！"))
                Return True

            Catch ex As OperationCanceledException
                ' 取消操作时的处理
                'BeginInvoke(Sub() MessageBox.Show("转换已取消"))
                LogError("The user cancelled the conversion")
                Return False
            Catch ex As Exception
                LogError($"The PDF conversion process is abnormal: {ex}")
                UpdateUI(Sub() MessageBox.Show($"Convert failed: {ex.Message}"))
                Return False
            End Try


        End Function

        Private Async Function ProcessSinglePDF(filePath As String, token As CancellationToken) As Task
            Using process As New Process()
                Try
                    token.ThrowIfCancellationRequested()

                    ' 路径验证
                    If String.IsNullOrWhiteSpace(filePath) Then
                        UpdateStatus(filePath, "Empty file path", Color.Red)
                        Return
                    End If

                    If Not File.Exists(filePath) Then
                        UpdateStatus(filePath, "File not exist", Color.Red)
                        Return
                    End If

                    ' 构建输出文件名
                    Dim outputFile = Path.Combine(Path.GetDirectoryName(filePath), $"{Path.GetFileNameWithoutExtension(filePath)}.html")

                    '构建转换参数
                    Dim Arg As String = $"--fit-width 1024 ""{filePath}"" --dest-dir ""{Path.GetDirectoryName(filePath)}"" --page-filename ""{Path.GetFileNameWithoutExtension(filePath)}.html"" "

                    ' 执行转换命令
                    Dim ConvertProcessInfo As New ProcessStartInfo("./pdf2htmlEX/pdf2htmlEX.exe", Arg)

                    process.StartInfo = ConvertProcessInfo
                    With process.StartInfo
                        .Arguments = Arg
                        .UseShellExecute = False
                        .CreateNoWindow = True

                    End With

                    runningProcesses.Add(process)  ' 添加到线程安全集合

                    '  进程启动前最后检查
                    If token.IsCancellationRequested Then
                        If _isInternational Then
                            UpdateStatus(filePath, "Cancelled", Color.Orange)
                        Else
                            UpdateStatus(filePath, "已取消", Color.Orange)
                        End If

                        Return
                    End If

                    If _isInternational Then
                        UpdateFileStatus(filePath, "Converting", Color.Blue)
                    Else
                        UpdateFileStatus(filePath, "正在转换", Color.Blue)
                    End If

                    Try
                        process.Start() '开始执行
                        Await process.WaitForExitAsync(token)
                    Finally
                        runningProcesses.TryTake(process)
                    End Try


                    If process.ExitCode = 0 Then
                        Debug.WriteLine(filePath)

                        If _isInternational Then
                            UpdateStatus(filePath, "Converted", Color.Green)
                        Else
                            UpdateStatus(filePath, "已转换", Color.Green)
                        End If
                    Else
                        UpdateStatus(filePath, $"Failed({process.ExitCode})", Color.Red)
                    End If

                Catch ex As OperationCanceledException
                    If _isInternational Then
                        UpdateStatus(filePath, "Cancelled", Color.Orange)
                    Else
                        UpdateStatus(filePath, "已取消", Color.Orange)
                    End If
                Catch ex As Exception
                    LogError($"File processing exception [{filePath}]: {ex}")
                    UpdateStatus(filePath, $"Error: {ex.Message}", Color.Red)
                End Try
            End Using
        End Function

        Private Function GetOutputDirectory(inputFolder As DirectoryInfo, outputRoot As DirectoryInfo) As String
            If createFolderCheckBox.Checked Then
                Dim profileFolderName = $"{profileID}_{inputFolder.Name}"
                Return Path.Combine(outputRoot.FullName, profileFolderName)
            Else
                Return outputRoot.FullName
            End If
        End Function

        Private Async Function EncryptFilesAsync(inputFolder As DirectoryInfo, outputRoot As DirectoryInfo, token As CancellationToken) As Task
            Try
                ' 使用线程安全方式获取有效文件列表
                Dim filePaths As List(Of String) = Await GetValidFilePathsAsync()
                ' 加密文件的输出目录
                Dim finalOutputDir As String = GetOutputDirectory(inputFolder, outputRoot)
                Directory.CreateDirectory(finalOutputDir)

                ' 并行处理文件（限制最大并发数）
                Dim options = New ParallelOptions With {
                        .CancellationToken = token,
                        .MaxDegreeOfParallelism = Environment.ProcessorCount
                    }

                ' 在后台线程执行并行操作
                Await Task.Run(Sub()
                                   Try
                                       Parallel.ForEach(filePaths, options, Sub(filePath)
                                                                                Dim unused = ProcessFileEncryption(filePath, inputFolder.FullName, finalOutputDir, token)
                                                                            End Sub)
                                   Catch ex As OperationCanceledException
                                       ' 预期内的取消操作，不处理错误
                                   Catch ex As AggregateException
                                       LogError($"Error in parallel processing: {ex.Flatten().Message}")
                                   End Try
                               End Sub, token)

                'UpdateUI(Sub()
                'btnStart.Enabled = True
                'btnStop.Enabled = False
                'If _isInternational Then
                'MessageBox.Show($"The encryption has been completed!")
                'Else
                'MessageBox.Show($"加密已完成!")
                'End If
                ' MessageBox.Show($"加密完成，共加密了 {filePaths.Count} 个文件")
                'End Sub)

            Catch ex As OperationCanceledException
                UpdateUI(Sub() MessageBox.Show("The operation has been cancelled."))
                btnStart.Enabled = True
                btnStop.Enabled = False
            Catch ex As Exception
                LogError($"Encryption process anomaly: {ex}")
                UpdateUI(Sub() MessageBox.Show($"Encryption failed: {ex.Message}"))
            End Try

        End Function

        '文件加密操作
        Private Async Function ProcessFileEncryption(inputFile As String, inputRoot As String, outputDir As String, token As CancellationToken) As Task

            ' 监控取消状态的标志
            Dim isUserCanceled = False
            Try
                ' 检查点1：开始处理前
                token.ThrowIfCancellationRequested()

                ' 加密文件的输出路径
                Dim outputFile = GenerateOutputPath(inputFile, inputRoot, outputDir)

                If _isInternational Then
                    UpdateStatus(inputFile, "Preparing to Encrypt...", Color.DarkBlue)
                Else
                    UpdateStatus(inputFile, "准备加密...", Color.DarkBlue)
                End If

                ' 构建加密参数
                Dim arguments = $"-ServerDomain {If(_isInternational, "COM", "CN")} " &
                      $"-AdminEmail ""{_adminEmail}"" " &
                      $"-WebServiceAuthStr ""{_authString}"" " &
                      $"-ID ""{profileID}"" " &
                      $"-Input ""{inputFile}"" " &
                      $"-Output ""{outputFile}"""
                Await Task.Run(Sub()
                                   ' 进程管理
                                   Using process As New Process()
                                       With process.StartInfo
                                           .FileName = "Packager.exe"
                                           .Arguments = arguments
                                           .UseShellExecute = False
                                           .CreateNoWindow = True
                                           .RedirectStandardOutput = True
                                           .RedirectStandardError = True
                                       End With

                                       ' 注册进程到监控列表
                                       SyncLock runningProcesses
                                           runningProcesses.Add(process)
                                       End SyncLock

                                       process.Start()
                                       'process.WaitForExit()


                                       ' 循环检查进程状态和取消请求
                                       Do While Not process.HasExited
                                           If token.IsCancellationRequested Then
                                               process.Kill()
                                               isUserCanceled = True ' 标记为用户主动取消
                                               token.ThrowIfCancellationRequested()
                                           End If
                                           Thread.Sleep(100) ' 避免CPU占用过高
                                       Loop

                                       ' 结果处理
                                       If isUserCanceled Then
                                           If _isInternational Then
                                               UpdateStatus(inputFile, "Cancelled", Color.Orange)
                                           Else
                                               UpdateStatus(inputFile, "已取消", Color.Orange)
                                           End If
                                       ElseIf process.ExitCode = 0 Then
                                           If _isInternational Then
                                               UpdateStatus(inputFile, "Successful", Color.Green)
                                           Else
                                               UpdateStatus(inputFile, "已完成", Color.Green)
                                           End If
                                           UpdateOutputPath(inputFile, outputFile)
                                       Else
                                           Dim errorMsg = process.StandardError.ReadToEnd()
                                           LogError($"Failed encryption [{process.ExitCode}]: {inputFile}{vbCrLf}{errorMsg}")
                                           UpdateStatus(inputFile, $"Failed ({process.ExitCode})", Color.Red)
                                       End If

                                   End Using
                               End Sub, token)

            Catch ex As OperationCanceledException
                ' 仅在没有标记的情况下更新状态
                If Not isUserCanceled Then
                    UpdateStatus(inputFile, "Cancelled", Color.Orange)
                End If
            Catch ex As Exception
                LogError($"File processing exception [{inputFile}]: {ex}")
                UpdateStatus(inputFile, $"Error: {ex.Message}", Color.Red)
            End Try
        End Function
        Private Sub UpdateOutputPath(inputFile As String, outputFile As String)
            UpdateUI(Sub()
                         Dim fileName = Path.GetFileName(inputFile)
                         Dim item = manualFileListView.Items.Cast(Of ListViewItem)().FirstOrDefault(Function(i) i.Text.Equals(fileName, StringComparison.OrdinalIgnoreCase))
                         If item IsNot Nothing Then
                             item.SubItems(2).Text = outputFile
                             manualFileListView.Refresh()
                         End If
                     End Sub)
        End Sub

        '获取有效文件列表
        Private Async Function GetValidFilePathsAsync() As Task(Of List(Of String))
            Return Await Task.Run(
            Function()
                ' 线程安全获取有效路径
                Dim paths As New List(Of String)
                If manualFileListView.InvokeRequired Then
                    manualFileListView.Invoke(
                    Sub()
                        paths = (From item In manualFileListView.Items.Cast(Of ListViewItem)()
                                 Let path = GetValidPath(item)
                                 Where path IsNot Nothing
                                 Select path).ToList()
                    End Sub)
                Else
                    paths = (From item In manualFileListView.Items.Cast(Of ListViewItem)()
                             Let path = GetValidPath(item)
                             Where path IsNot Nothing
                             Select path).ToList()
                End If
                Return paths
            End Function)
        End Function

        ' 验证路径的有效性
        Private Function GetValidPath(item As ListViewItem) As String
            Try
                If item Is Nothing Then Return Nothing
                If item.Tag Is Nothing Then Return Nothing
                Dim path = TryCast(item.Tag, String)
                If String.IsNullOrWhiteSpace(path) Then Return Nothing
                If Not File.Exists(path) Then Return Nothing

                Return path
            Catch
                Return Nothing
            End Try
        End Function

        ' 生成加密文件的输出路径
        Private Function GenerateOutputPath(inputFile As String, inputRoot As String, outputDir As String) As String
            If String.IsNullOrWhiteSpace(inputFile) Then
                Throw New ArgumentException("Input path cannot be empty", NameOf(inputFile))
            End If

            If String.IsNullOrWhiteSpace(inputRoot) Then
                Throw New ArgumentException("Input root path cannot be empty", NameOf(inputRoot))
            End If

            If String.IsNullOrWhiteSpace(outputDir) Then
                Throw New ArgumentException("Output path cannot be empty", NameOf(outputDir))
            End If

            ' 获取输入文件相对于输入根目录的相对路径
            Dim relativePath As String = GetRelativePath(inputRoot, inputFile)

            ' 获取文件名和扩展名
            Dim fileName As String = Path.GetFileNameWithoutExtension(relativePath)
            Dim extension As String = Path.GetExtension(relativePath)

            ' 生成带后缀的新文件名
            Dim newFileName As String = $"{fileName}_P{extension}"

            ' 获取相对路径中的目录部分
            Dim directoryPart As String = Path.GetDirectoryName(relativePath)

            ' 组合完整的输出路径
            Dim fullOutputPath As String

            If String.IsNullOrEmpty(directoryPart) Then
                fullOutputPath = Path.Combine(outputDir, newFileName)
            Else
                fullOutputPath = Path.Combine(outputDir, directoryPart, newFileName)
            End If

            ' 确保输出目录存在
            Directory.CreateDirectory(Path.GetDirectoryName(fullOutputPath))

            Return fullOutputPath
        End Function
        ' 在类中添加这个自定义路径工具方法
        Public Shared Function GetRelativePath(fromPath As String, toPath As String) As String
            ' 统一路径格式
            fromPath = Path.GetFullPath(fromPath).TrimEnd(Path.DirectorySeparatorChar)
            toPath = Path.GetFullPath(toPath).TrimEnd(Path.DirectorySeparatorChar)

            ' 分解路径为段
            Dim fromParts = fromPath.Split(Path.DirectorySeparatorChar)
            Dim toParts = toPath.Split(Path.DirectorySeparatorChar)

            ' 找到共同前缀的长度
            Dim length = 0
            For i = 0 To Math.Min(fromParts.Length, toParts.Length) - 1
                If Not String.Equals(fromParts(i), toParts(i), StringComparison.OrdinalIgnoreCase) Then Exit For
                length += 1
            Next

            ' 构建向上回溯的路径
            Dim builder As New System.Text.StringBuilder()
            For i = length To fromParts.Length - 1
                builder.Append("..")
                builder.Append(Path.DirectorySeparatorChar)
            Next

            ' 添加目标路径的剩余部分
            For i = length To toParts.Length - 1
                builder.Append(toParts(i))
                If i < toParts.Length - 1 Then
                    builder.Append(Path.DirectorySeparatorChar)
                End If
            Next

            Return builder.ToString()
        End Function

        Private Sub UpdateUI(action As Action)
            If Me.InvokeRequired Then
                Me.BeginInvoke(action)
            Else
                action()
            End If
        End Sub

        Private Sub UpdateStatus(filePath As String, status As String, color As Color)
            UpdateUI(Sub()
                         Dim fileName = Path.GetFileName(filePath)
                         Dim item = manualFileListView.Items.Cast(Of ListViewItem)().FirstOrDefault(Function(i) i.Text.Equals(fileName, StringComparison.OrdinalIgnoreCase))
                         If item IsNot Nothing Then
                             If item.SubItems(4).Text.StartsWith("Error") Then Return
                             If item.SubItems(4).Text.StartsWith("Failed") AndAlso color <> Color.Orange Then Return

                             item.SubItems(4).Text = status
                             item.ForeColor = color
                             manualFileListView.Refresh()
                         End If
                     End Sub)
        End Sub

        ' 更新文件状态的方法
        Private Sub UpdateFileStatus(filePath As String, status As String, color As Color)
            If manualFileListView.InvokeRequired Then
                manualFileListView.BeginInvoke(Sub() UpdateFileStatus(filePath, status, color))
            Else
                Dim fileName = Path.GetFileName(filePath)
                'Dim item = manualFileListView.Items.Cast(Of ListViewItem).FirstOrDefault(Function(i) i.Text = fileName)
                Dim item = manualFileListView.Items.Cast(Of ListViewItem)().FirstOrDefault(Function(i) i.Text.Equals(fileName, StringComparison.OrdinalIgnoreCase))
                If item IsNot Nothing Then
                    item.SubItems(4).Text = status
                    item.ForeColor = color
                    manualFileListView.Refresh()
                End If
            End If

        End Sub

        ' 错误日志记录方法
        Private Sub LogError(message As String)
            Try
                Dim logPath = Path.Combine(Application.StartupPath, "FileProcessErrors.log")
                File.AppendAllText(logPath, $"[{DateTime.Now:yyyy-MM-dd HH:mm:ss.fff}] " &
                                    $"[Thread:{Thread.CurrentThread.ManagedThreadId}] " &
                                    $"{message}{vbCrLf}{vbCrLf}")
            Catch ex As Exception
                ' 防止日志记录导致二次崩溃
            End Try
        End Sub

        ' Stop按钮点击事件，停止按钮事件以终止所有进程
        Private Sub btnStop_Click(sender As Object, e As EventArgs) Handles btnStop.Click

            ' 显示确认对话框
            Dim result
            If _isInternational Then

                result = MessageBox.Show("Are you sure you want to stop the current operation?",
                                    "Confirm Stop",
                                    MessageBoxButtons.OKCancel,
                                    MessageBoxIcon.Question)
            Else

                result = MessageBox.Show("您确定要停止当前操作吗?",
                                    "确认停止",
                                    MessageBoxButtons.OKCancel,
                                    MessageBoxIcon.Question)
            End If

            If result = DialogResult.OK Then
                ' 触发取消
                cancellationTokenSource.Cancel()

                KillAllProcesses()

                ' 强制终止所有相关进程
                BeginInvoke(Sub()
                                For Each item In manualFileListView.Items.Cast(Of ListViewItem)()
                                    If item.SubItems(4).Text.Contains("...") Then
                                        If _isInternational Then
                                            item.SubItems(4).Text = "Cancelled"
                                        Else
                                            item.SubItems(4).Text = "已取消"
                                        End If
                                        item.ForeColor = Color.Orange
                                    End If
                                Next
                            End Sub)
                ' 恢复按钮状态
                btnStart.Enabled = True
                btnStop.Enabled = False
            End If
        End Sub

        Private Function CreateFilePanel(labelText As String, buttonText As String, top As Integer, panelType As String) As Panel
            Dim panel As New Panel With {
                .Height = 40,
                .Top = top,
                .Width = 900
            }

            panel.Controls.Add(New Label With {
                .Text = labelText,
                .Left = 20,
                .Top = 6,
                .Width = 130
            })

            Dim txtBox As New TextBox With {
                .Left = 150,
                .Width = 500,
                .Top = 5,
                .Tag = panelType, ' 添加标识用于区分
                .ReadOnly = True,
                .BackColor = SystemColors.Window, ' 默认背景色
                .Cursor = Cursors.Arrow' 强制设置为只读
            }

            Dim btnBrowse As New Button With {
                .Text = buttonText,
                .Left = 650,
                .Top = 4,
                .Width = 80,
                .Height = 28,
                .Tag = txtBox ' 关联文本框
            }

            ' 添加浏览按钮点击事件
            AddHandler btnBrowse.Click, AddressOf BrowseButton_Click

            txtBox.ForeColor = Color.Gray

            If _isInternational Then
                txtBox.Text = "Please select a directory..."
            Else
                txtBox.Text = "请选择目录..."
            End If

            panel.Controls.Add(txtBox)
            panel.Controls.Add(btnBrowse)

            Return panel

        End Function

        ' 浏览按钮点击事件处理
        Private Sub BrowseButton_Click(sender As Object, e As EventArgs)
            Dim targetTextBox As TextBox = CType(CType(sender, Button).Tag, TextBox)

            ' 配置对话框
            folderBrowserDialog.Description = If(_isInternational, "Please select a directory...", "请选择目录...")
            folderBrowserDialog.ShowNewFolderButton = True
            folderBrowserDialog.RootFolder = Environment.SpecialFolder.Desktop

            If folderBrowserDialog.ShowDialog() = DialogResult.OK Then
                targetTextBox.Text = folderBrowserDialog.SelectedPath

                If targetTextBox.Tag.ToString() = "Input" Then
                    LoadFilesToListView(targetTextBox.Text)
                End If

            End If
        End Sub

        ' 文件加载方法
        Private Sub LoadFilesToListView(directoryPath As String)
            UpdateUI(Sub()
                         Try
                             manualFileListView.BeginUpdate()
                             manualFileListView.Items.Clear()

                             Dim files = Directory.EnumerateFiles(directoryPath, "*.*", SearchOption.AllDirectories).Where(Function(f) allowedExtensions.Contains(Path.GetExtension(f))).OrderBy(Function(f) f, New NaturalFileComparer()).ToList()

                             For Each filePath In files
                                 Try
                                     Dim fileInfo = New FileInfo(filePath)
                                     Dim item As New ListViewItem(Path.GetFileName(filePath))

                                     item.SubItems.Add(filePath)
                                     Dim outputPath = "......"
                                     item.SubItems.Add(outputPath)
                                     item.SubItems.Add(FormatFileSize(fileInfo.Length))
                                     If _isInternational Then
                                         item.SubItems.Add("Pending")
                                     Else
                                         item.SubItems.Add("等待加密")
                                     End If
                                     item.Tag = filePath  ' 存储完整路径

                                     manualFileListView.Items.Add(item)
                                 Catch ex As UnauthorizedAccessException
                                     ' 跳过无权限访问的文件
                                 End Try
                             Next


                         Catch ex As Exception
                             MessageBox.Show($"Failed to load the file: {ex.Message}",
                          "Error",
                          MessageBoxButtons.OK,
                          MessageBoxIcon.Error)
                         Finally
                             manualFileListView.EndUpdate()
                         End Try
                     End Sub)
        End Sub

        ' 文件大小格式化方法
        Private Function FormatFileSize(bytes As Long) As String
            Dim units() As String = {"B", "KB", "MB", "GB"}
            Dim size As Double = bytes
            Dim unitIndex = 0

            While size >= 1024 AndAlso unitIndex < units.Length - 1
                size /= 1024
                unitIndex += 1
            End While

            Return $"{size:0.##} {units(unitIndex)}"
        End Function

        ' 右键菜单功能
        Private Sub RefreshFileList(sender As Object, e As EventArgs)
            If Directory.Exists(manualInputTextBox.Text) Then
                LoadFilesToListView(manualInputTextBox.Text)
            End If
        End Sub

        '文件上右键打开文件目录
        Private Sub OpenFileLocation(sender As Object, e As EventArgs)
            If IsManualEncryptTab() Then
                If manualFileListView.SelectedItems.Count > 0 Then
                    Dim filePath = manualFileListView.SelectedItems(0).Tag.ToString()
                    Process.Start("explorer.exe", $"/select, ""{filePath}""")
                End If
            Else
                If autoFileListView.SelectedItems.Count > 0 Then
                    Dim filePath = autoFileListView.SelectedItems(0).SubItems(1).Text
                    Process.Start("explorer.exe", $"/select, ""{filePath}""")
                End If
            End If
        End Sub

        Private Sub OpenOutputLocation(sender As Object, e As EventArgs)
            If IsManualEncryptTab() Then
                If manualFileListView.SelectedItems.Count > 0 Then
                    Dim outputPath = manualFileListView.SelectedItems(0).SubItems(2).Text
                    If Directory.Exists(Path.GetDirectoryName(outputPath)) Then
                        Process.Start("explorer.exe", $"/select, ""{outputPath}""")
                    Else
                        MessageBox.Show("Output directory does not exist.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End If
                End If
            Else
                If autoFileListView.SelectedItems.Count > 0 Then
                    Dim outputPath = autoFileListView.SelectedItems(0).SubItems(2).Text
                    If Directory.Exists(Path.GetDirectoryName(outputPath)) Then
                        Process.Start("explorer.exe", $"/select, ""{outputPath}""")
                    Else
                        MessageBox.Show("Output directory does not exist.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End If
                End If
            End If

        End Sub

        '双击文件列表里的文件打开文件
        Private Sub OpenSelectedFile(sender As Object, e As EventArgs)
            If IsManualEncryptTab() Then
                If manualFileListView.SelectedItems.Count > 0 Then
                    Dim filePath = manualFileListView.SelectedItems(0).Tag.ToString()
                    Try
                        Process.Start(filePath)
                    Catch ex As Exception
                        MessageBox.Show($"Unable to open the file: {ex.Message}")
                    End Try
                End If
            Else
                If autoFileListView.SelectedItems.Count > 0 Then
                    Dim filePath = autoFileListView.SelectedItems(0).SubItems(1).Text
                    Try
                        Process.Start(filePath)
                    Catch ex As Exception
                        MessageBox.Show($"Unable to open the file: {ex.Message}")
                    End Try
                End If
            End If
        End Sub

        '检查当前界面是否为“手动加密”
        Public Function IsManualEncryptTab() As Boolean
            Return tabControl.SelectedTab Is manualTab
        End Function


        ' 自动加密选项卡
        Private Sub InitializeAutoTab(container As Control)
            ' 自动加密选项卡初始化时保留控件引用
            AutoInputPanel = CreateFilePanel("Scan Directory:", "Browse", 20, "Inout")
            autoInputTextBox = CType(AutoInputPanel.Controls.OfType(Of TextBox)().First(), TextBox)

            AutoOutputPanel = CreateFilePanel("Output Directory:", "Browse", 60, "Output")
            autoOutputTextBox = CType(AutoOutputPanel.Controls.OfType(Of TextBox)().First(), TextBox)

            ' 控制按钮
            btnAutoStart = New Button With {
                .Text = "Start Scanning",
                .Left = 20,
                .Top = 100,
                .Width = 130,
                .Height = 35,
                .Enabled = True}

            btnAutoStop = New Button With {
                .Text = "Stop Scanning",
                .Left = 180,
                .Top = 100,
                .Width = 130,
                .Height = 35,
                .Enabled = False}

            ' 添加清空列表按钮
            btnClearList = New Button With {
            .Text = "Clear List", ' 默认中文，在SetLanguage中会根据语言切换
            .Left = 340,
            .Top = 100,
            .Width = 130,
            .Height = 35,
            .Enabled = True}


            ' 状态显示改为Label
            statusLabel = New Label With {
                .Text = "Status:",
                .Left = 20,
                .Top = 145}

            lblStatus = New Label With {
                .Left = 120,
                .Top = 145,
                .Width = 400,
                .AutoSize = False,
                .BorderStyle = BorderStyle.FixedSingle,
                .BackColor = SystemColors.Info,
                .ForeColor = Color.DarkSlateGray
            }

            ' 在自动加密选项卡中添加 ListView
            autoFileListView = New ListView With {
                .View = View.Details,
                .Top = 180,
                .Width = 870,
                .Height = 320,
                .FullRowSelect = True,
                .GridLines = True,
                .Scrollable = True
            }
            With autoFileListView.Columns
                .Add(If(_isInternational, "File Name", "文件名"), 250)
                .Add(If(_isInternational, "Input Path", "输入目录"), 200)
                .Add(If(_isInternational, "Output Path", "输出目录"), 200)
                .Add(If(_isInternational, "Size", "文件大小"), 90)
                .Add(If(_isInternational, "Status", "状态"), 120)
            End With

            convertPDFCheckBoxAuto = New CheckBox With {
            .Text = "Convert PDF to HTML",
            .Left = 500,
            .Top = 108,
            .Width = 185
        }

            ' 添加右键菜单
            Dim contextMenu As New ContextMenuStrip

            contextMenu.Items.Add(If(_isInternational, "📂 Open Input File Path", "📂 打开输入文件目录"), Nothing, AddressOf OpenFileLocation)
            contextMenu.Items.Add(If(_isInternational, "📁 Open Output File Path", "📁 打开输出文件目录"), Nothing, AddressOf OpenOutputLocation)

            autoFileListView.ContextMenuStrip = contextMenu

            ' 添加双击事件
            AddHandler autoFileListView.DoubleClick, AddressOf OpenSelectedFile

            container.Controls.AddRange({AutoInputPanel, AutoOutputPanel, btnAutoStart, btnAutoStop, btnClearList， convertPDFCheckBoxAuto, statusLabel, lblStatus, autoFileListView})

            ' 添加按钮事件
            AddHandler btnAutoStart.Click, AddressOf btnAutoStart_Click
            AddHandler btnAutoStop.Click, AddressOf btnAutoStop_Click
            AddHandler btnClearList.Click, AddressOf btnClearList_Click


        End Sub


        Private Async Sub btnAutoStart_Click(sender As Object, e As EventArgs)

            Try
                ' 切换按钮状态
                btnAutoStart.Enabled = False
                btnAutoStop.Enabled = True
                btnClearList.Enabled = False
                convertPDFCheckBoxAuto.Enabled = False

                If _isInternational Then
                    lblStatus.Text = "Creating License Profile..."
                Else
                    lblStatus.Text = "正在创建许可证模板..."
                End If


                If isProcessing Then Return
                isProcessing = True

                ' 初始化关键变量
                processingQueue = New Concurrent.ConcurrentQueue(Of String)()
                processedFiles = New Concurrent.ConcurrentDictionary(Of String, Boolean)()

                If Not Directory.Exists(autoInputTextBox.Text) Then
                    If _isInternational Then
                        MessageBox.Show("Please select input directory first")
                    Else
                        MessageBox.Show("请先选择输入目录")
                    End If
                    Return
                End If

                ' 创建许可证模板
                Await CreateLicenseProfilesAsync(autoInputTextBox.Text)

                ' 启动文件监控
                'InitializeFileWatcher()


                ' 扫描现有文件并加入队列
                ' 递归扫描所有现有文件（包括子目录）
                Dim existingFiles = Directory.EnumerateFiles(
                    autoInputTextBox.Text,
                    "*.*",
                    SearchOption.AllDirectories
                ).Where(Function(f) allowedExtensions.Contains(Path.GetExtension(f)))

                For Each file In existingFiles
                    Try
                        If Not processedFiles.ContainsKey(file) Then
                            processingQueue.Enqueue(file)
                            If _isInternational Then
                                UpdateAutoFileStatus(file, "Waiting for processing", Color.Gray)
                            Else
                                UpdateAutoFileStatus(file, "等待处理", Color.Gray)
                            End If
                        End If
                    Catch ex As UnauthorizedAccessException
                        LogError($"无权限访问文件:  {file}")
                    End Try
                Next

                ' 启动队列处理
                autoCts = New CancellationTokenSource()
                Dim unused = Task.Run(Sub() ProcessQueueAsync(autoCts.Token), autoCts.Token)

                If _isInternational Then
                    UpdateStatusLabel($"Scanning...", Color.DarkBlue)
                Else
                    UpdateStatusLabel($"正在扫描...", Color.DarkBlue)
                End If


                ' 初始化文件监控
                fileSystemWatcher = New FileSystemWatcher() With {
                    .Path = autoInputTextBox.Text,
                    .IncludeSubdirectories = True,
                    .EnableRaisingEvents = True,
                    .NotifyFilter = NotifyFilters.FileName,
                    .Filter = "*.*"
                }

                AddHandler fileSystemWatcher.Created, AddressOf OnFileCreated
                AddHandler fileSystemWatcher.Changed, AddressOf OnFileCreated 'Joseph 2025-3-26
                AddHandler fileSystemWatcher.Renamed, AddressOf OnFileCreated 'Joseph 2025-3-26

            Catch ex As Exception
                LogError($"Start monitoring failed: {ex}")
                UpdateUI(Sub()
                             btnAutoStart.Enabled = True
                             btnAutoStop.Enabled = False
                             btnClearList.Enabled = True
                             convertPDFCheckBoxAuto.Enabled = True
                         End Sub)
            Finally
                isProcessing = False
            End Try

        End Sub


        ' 更新自动加密文件状态的方法
        Private Sub UpdateAutoFileStatus(filePath As String, status As String, color As Color)
            If autoFileListView.InvokeRequired Then
                autoFileListView.BeginInvoke(Sub() UpdateAutoFileStatus(filePath, status, color))
            Else
                Dim fileName = Path.GetFileName(filePath)
                Dim fileSize = If(File.Exists(filePath), New FileInfo(filePath).Length, -1)

                Dim existingItem = autoFileListView.Items.Cast(Of ListViewItem)().FirstOrDefault(
                    Function(i) i.SubItems(1).Text.Equals(filePath, StringComparison.OrdinalIgnoreCase)
                )
                If existingItem Is Nothing Then
                    Dim item = New ListViewItem(fileName)
                    item.SubItems.Add(filePath)
                    item.SubItems.Add("...") '
                    'item.SubItems.Add(GetLicenseOutputPath(filePath).outputPath)
                    item.SubItems.Add(FormatFileSize(fileSize))
                    item.SubItems.Add(status)
                    item.ForeColor = color
                    autoFileListView.Items.Add(item)
                Else
                    existingItem.SubItems(4).Text = status
                    existingItem.ForeColor = color
                End If
                ' 滚动到最新项
                ' autoFileListView.EnsureVisible(autoFileListView.Items.Count - 1)
            End If
        End Sub

        Private Sub UpdateStatusLabel(text As String, Optional color As Color = Nothing)
            If lblStatus.InvokeRequired Then
                lblStatus.BeginInvoke(Sub() UpdateStatusLabel(text, color))
            Else
                lblStatus.Text = text
                lblStatus.ForeColor = If(color = Nothing, SystemColors.ControlText, color)
                lblStatus.BackColor = Color.FromArgb(&HE0, &HE0, &HE0)
            End If
        End Sub

        Private Sub OnFileCreated(sender As Object, e As FileSystemEventArgs)

            WaitForFileReady(e.FullPath)

            If allowedExtensions.Contains(Path.GetExtension(e.FullPath)) Then
                ' 延迟检查确保文件可访问
                Task.Run(Async Function()
                             Await Task.Delay(1000)
                             If IsFileAccessible(e.FullPath) Then
                                 processingQueue.Enqueue(e.FullPath)
                                 If _isInternational Then
                                     UpdateAutoFileStatus(e.FullPath, "New files detected", Color.Blue)
                                 Else
                                     UpdateAutoFileStatus(e.FullPath, "检测到新文件", Color.Blue)
                                 End If
                             End If
                         End Function)
            End If
        End Sub
        Private Sub WaitForFileReady(filePath As String)
            Const maxRetries As Integer = 20 ' 增加到20次重试
            Dim delay As Integer = 100 ' 初始延迟500ms
            Dim retryCount As Integer = 0

            While retryCount < maxRetries
                Try
                    ' 尝试以非独占方式打开文件
                    Using fs = File.Open(filePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                        If fs.Length > 0 Then Exit Sub ' 文件就绪
                    End Using
                Catch ex As IOException When IsFileLocked(ex)
                    ' 明确捕获文件锁定异常
                    ' LogError($"文件被锁定: {filePath} (重试 {retryCount + 1}/{maxRetries})")
                Catch ex As Exception
                    LogError($"文件访问异常: {filePath} - {ex.Message}")
                    Throw
                End Try

                ' 指数退避策略
                Thread.Sleep(delay)
                delay = Math.Min(delay * 2, 5000) ' 最大延迟5秒
                retryCount += 1
            End While

            Throw New TimeoutException($"文件未就绪: {filePath}")
        End Sub
        Private Async Sub ProcessQueueAsync(token As CancellationToken)
            Try
                While Not token.IsCancellationRequested
                    Dim filePath As String = Nothing

                    ' 优先处理重试队列
                    If Not retryQueue.IsEmpty Then
                        Await ProcessRetryQueue(token)
                        Continue While
                    End If

                    ' 处理正常队列中的文件
                    If processingQueue.TryDequeue(filePath) Then
                        Try
                            Dim fileExistsInDb As Boolean = False

                            ' 检查数据库中的文件记录
                            Using dbConnection As New SQLiteConnection("Data Source=FileProcessLogDB.db;Version=3;Pooling=True;Cache=Shared;Journal Mode=WAL;BusyTimeout=5000;")
                                dbConnection.Open()
                                fileExistsInDb = QueryProcessedFiles(dbConnection, filePath)

                            End Using

                            If fileExistsInDb Then
                                ' 如果文件已存在，移除列表项并跳过处理
                                RemoveFromListView(filePath)
                                Continue While
                            End If

                            If convertPDFCheckBoxAuto.Checked AndAlso Path.GetExtension(filePath).Equals(".pdf", StringComparison.OrdinalIgnoreCase) Then
                                Await pdfConversionSemaphore.WaitAsync(token)
                                Try
                                    Await ConvertPDFAutoAsync(filePath, token)
                                    'processingQueue.Enqueue(convertedFile) ' 将生成的HTML加入队列
                                Finally
                                    pdfConversionSemaphore.Release()
                                End Try
                            End If

                            ' 执行文件处理（加密/转换）
                            Dim success As Boolean = Await EncryptSingleFileAsync(filePath, token)

                            ' 仅在处理成功时移除列表项
                            'If success Then
                            'RemoveFromListView(filePath)
                            'End If

                        Catch ex As OperationCanceledException
                            LogError($"加密取消: {filePath}")
                        Catch ex As Exception
                            LogError($"加密失败: {filePath} - {ex.Message}")
                        End Try
                    Else
                        ' 处理重试队列
                        Await ProcessRetryQueue(token)
                    End If
                End While
            Catch ex As OperationCanceledException
                LogError("扫描手动停止")
            Finally
                UpdateUI(Sub() lblStatus.Text = If(_isInternational, "Ready", "已就绪"))
            End Try
        End Sub

        ' 辅助方法：从ListView中移除指定文件路径的项
        Private Sub RemoveFromListView(filePath As String)
            If autoFileListView.InvokeRequired Then
                autoFileListView.BeginInvoke(Sub() RemoveFromListView(filePath))
            Else
                For i As Integer = autoFileListView.Items.Count - 1 To 0 Step -1
                    Dim item As ListViewItem = autoFileListView.Items(i)
                    If String.Equals(item.SubItems(1).Text, filePath, StringComparison.OrdinalIgnoreCase) Then
                        autoFileListView.Items.RemoveAt(i)
                    End If
                Next
            End If
        End Sub

        Private Async Function ProcessRetryQueue(token As CancellationToken) As Task
            If Not retryQueue.Any() Then
                Await Task.Delay(1000, token) ' 无任务时轻度休眠
                Return
            End If

            Dim now = DateTime.Now
            Dim eligibleFiles = retryQueue.Where(Function(f) f.NextRetryTime <= now).ToList()

            For Each fileInfo In eligibleFiles
                If fileInfo.RetryCount >= MaxRetryAttempts Then
                    LogError($"放弃处理: {fileInfo.FilePath} (超过最大重试次数)")
                    retryQueue = New ConcurrentQueue(Of RetryFileInfo)(retryQueue.Except({fileInfo}))
                    Continue For
                End If

                ' 临时移出队列防止重复处理
                retryQueue = New ConcurrentQueue(Of RetryFileInfo)(retryQueue.Except({fileInfo}))
                Await ProcessSingleFile(fileInfo.FilePath, token)

                ' 更新重试信息
                fileInfo.RetryCount += 1
                fileInfo.NextRetryTime = CalculateNextRetry(fileInfo.RetryCount)
                retryQueue.Enqueue(fileInfo)
            Next

            If eligibleFiles.Count = 0 Then
                Await Task.Delay(500, token) ' 无符合条件文件时短暂等待
            End If
        End Function

        Private Async Function ProcessSingleFile(filePath As String, token As CancellationToken) As Task
            Try
                If Not IsFileReady(filePath) Then
                    ScheduleRetry(filePath, isNewFile:=True)
                    Return
                End If

                If convertPDFCheckBoxAuto.Checked AndAlso Path.GetExtension(filePath).Equals(".pdf", StringComparison.OrdinalIgnoreCase) Then
                    Await pdfConversionSemaphore.WaitAsync(token)
                    Try
                        Await ConvertPDFAutoAsync(filePath, token)
                        'processingQueue.Enqueue(convertedFile) ' 将生成的HTML加入队列
                    Finally
                        pdfConversionSemaphore.Release()
                    End Try
                End If

                ' 正常加密流程
                Await EncryptSingleFileAsync(filePath, token)


                LogError($"成功加密: {filePath}")

            Catch ex As FileNotFoundException
                LogError($"文件已消失: {filePath}")
            Catch ex As IOException When IsFileLocked(ex)
                ScheduleRetry(filePath, isNewFile:=False)
            Catch ex As Exception
                LogError($"处理失败: {filePath} - {ex.Message}")
            End Try
        End Function
        Private Async Function ConvertPDFAutoAsync(pdfPath As String, token As CancellationToken) As Task
            Try
                ' 双重检查文件可用性
                Await WaitForFileReady(pdfPath, token)

                ' 生成输出路径（同目录）
                Dim outputDir = Path.GetDirectoryName(pdfPath)
                Dim htmlFile = Path.Combine(outputDir, $"{Path.GetFileNameWithoutExtension(pdfPath)}.html")

                ' 更新状态
                UpdateAutoFileStatus(pdfPath, If(_isInternational, "Converting PDF...", "正在转换PDF..."), Color.DarkCyan)

                ' 转换进程管理
                Using process As New Process()
                    process.StartInfo = New ProcessStartInfo() With {
                        .FileName = "./pdf2htmlEX/pdf2htmlEX.exe",
                        .Arguments = $"--fit-width 1024 ""{pdfPath}"" --dest-dir ""{outputDir}""",
                        .UseShellExecute = False,
                        .CreateNoWindow = True,
                        .RedirectStandardOutput = True
                    }

                    ' 启动进程并异步等待
                    process.Start()
                    Dim processTask = process.WaitForExitAsync(token)

                    ' 创建超时保护（5分钟）
                    Dim timeoutTask = Task.Delay(TimeSpan.FromMinutes(5), token)

                    Dim completedTask = Await Task.WhenAny(processTask, timeoutTask)

                    If completedTask Is timeoutTask Then
                        process.Kill()
                        Throw New TimeoutException("PDF conversion timed out")
                    End If

                    ' 验证输出文件
                    If process.ExitCode = 0 Then
                        UpdateAutoFileStatus(pdfPath, If(_isInternational, "Converted", "已转换"), Color.DarkGreen)
                    End If
                End Using
            Catch ex As Exception
                LogError($"PDF Conversion Failed: {pdfPath} - {ex.Message}")
            End Try
        End Function

        ' 文件状态检测模块
        Private Function IsFileReady(filePath As String) As Boolean
            Try
                ' 通过尝试打开文件检测可用性
                Using fs As New FileStream(filePath, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
                    Return fs.Length > 0 ' 可根据需要调整判断逻辑
                End Using
            Catch ex As Exception
                Return False
            End Try
        End Function

        Private Function IsFileLocked(ex As IOException) As Boolean
            ' 通过错误代码判断文件锁定状态
            Dim errorCode As Integer = Marshal.GetHRForException(ex) And &HFFFF
            Return errorCode = 32 OrElse errorCode = 33 ' ERROR_SHARING_VIOLATION / ERROR_LOCK_VIOLATION
        End Function

        Private Sub ScheduleRetry(filePath As String, isNewFile As Boolean)
            Dim existing = retryQueue.FirstOrDefault(Function(x) x.FilePath = filePath)

            If existing.FilePath IsNot Nothing Then
                ' 已有记录则更新重试时间
                existing.NextRetryTime = CalculateNextRetry(existing.RetryCount)
                retryQueue = New ConcurrentQueue(Of RetryFileInfo)(retryQueue.Except({existing}))
                retryQueue.Enqueue(existing)
            Else
                ' 新条目加入队列
                Dim delayIndex = If(isNewFile, 0, 1) ' 新文件立即重试，锁定文件等待更久
                retryQueue.Enqueue(New RetryFileInfo With {
            .FilePath = filePath,
            .RetryCount = 0,
            .NextRetryTime = DateTime.Now.AddMilliseconds(retryDelays(delayIndex))
        })
            End If
        End Sub

        Private Function CalculateNextRetry(attempt As Integer) As DateTime
            Dim index = Math.Min(attempt, retryDelays.Length - 1)
            Return DateTime.Now.AddMilliseconds(retryDelays(index))
        End Function

        '重试文件
        Private Structure RetryFileInfo
            Public FilePath As String
            Public RetryCount As Integer
            Public NextRetryTime As DateTime
        End Structure

        Private Async Function WaitForFileReady(filePath As String, token As CancellationToken) As Task
            Const timeoutSeconds As Integer = 60 ' 最大等待时间60秒'

            ' 组合等待任务：文件就绪检测 + 超时控制 '
            Await Task.WhenAny(
                CheckFileAccessLoop(filePath, token),
                Task.Delay(timeoutSeconds * 1000, token)
            ).ConfigureAwait(False)

            ' 最终验证文件是否可用 '
            If Not IsFileAccessible(filePath) Then
                Throw New TimeoutException($"File not ready.")
            End If
        End Function

        Private Async Function CheckFileAccessLoop(filePath As String, token As CancellationToken) As Task
            Do While Not token.IsCancellationRequested
                Dim needsRetry As Boolean = False

                Try
                    ' 尝试访问文件 '
                    Using New FileStream(filePath, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
                        Exit Do ' 成功访问时退出循环 '
                    End Using
                Catch ex As IOException
                    needsRetry = True ' 标记需要重试 '
                End Try

                ' 在Catch块外执行异步等待 '
                If needsRetry Then
                    Await Task.Delay(1000, token)
                End If
            Loop
        End Function

        Private Function IsFileAccessible(filePath As String) As Boolean
            Try
                Using New FileStream(filePath, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
                    Return True
                End Using
            Catch
                Return False
            End Try
        End Function

        Private Async Function EncryptSingleFileAsync(inputFile As String, token As CancellationToken) As Task(Of Boolean)
            Try
                Await WaitForFileReady(inputFile, token)

                ' 更新状态为 "处理中"
                If _isInternational Then
                    UpdateAutoFileStatus(inputFile, "Encrypting...", Color.DarkBlue)
                Else
                    UpdateAutoFileStatus(inputFile, "加密中...", Color.DarkBlue)
                End If

                ' 获取输出路径和 profileId
                Dim result = GetLicenseOutputPath(inputFile)

                ' 生成输出目录（根据输入目录结构）
                Dim outputDir = result.outputPath
                Dim profileId = result.profileId

                Await Task.Run(Sub() EncryptFile(inputFile, outputDir, profileId, token), token)

                ' 更新状态为 "成功"

                If _isInternational Then
                    UpdateAutoFileStatus(inputFile, "Successful ✓", Color.Green)
                Else
                    UpdateAutoFileStatus(inputFile, "加密成功 ✓", Color.Green)
                End If
                Return True

            Catch ex As Exception
                ' 更新状态为 "失败"
                If _isInternational Then
                    UpdateAutoFileStatus(inputFile, $"Failed: {ex.Message}", Color.Red)
                Else
                    UpdateAutoFileStatus(inputFile, $"失败： {ex.Message}", Color.Red)
                End If

                LogError($"Encryption failed: [{inputFile}]: {ex.Message}")

                Return False
            End Try
        End Function

        Private Sub EncryptFile(inputFile As String, outputFile As String, profileId As String, token As CancellationToken)
            Try

                ' 创建目标目录
                Directory.CreateDirectory(Path.GetDirectoryName(outputFile))

                ' 加密参数构建
                Dim arguments = $"-ServerDomain {If(_isInternational, "COM", "CN")} " &
                      $"-AdminEmail ""{_adminEmail}"" " &
                      $"-WebServiceAuthStr ""{_authString}"" " &
                      $"-ID ""{profileId}"" " &
                      $"-Input ""{inputFile}"" " &
                      $"-Output ""{outputFile}"""

                ' 调用加密工具（示例代码）
                Dim process As New Process()
                process.StartInfo.FileName = "Packager.exe"
                process.StartInfo.Arguments = arguments
                process.StartInfo.UseShellExecute = False
                process.StartInfo.CreateNoWindow = True
                process.StartInfo.RedirectStandardOutput = True
                process.StartInfo.RedirectStandardError = True

                process.Start()
                process.WaitForExit()

                ' 检查退出代码
                If process.ExitCode <> 0 Then
                    If _isInternational Then
                        Throw New Exception($"Encryption failed:{process.ExitCode}")
                    Else
                        Throw New Exception($"加密失败，错误代码：{process.ExitCode}")
                    End If
                End If


                ' 插入文件记录到SQLite数据库
                InsertFileRecord(inputFile, profileId)

                UpdateAutoOutputPath(inputFile, outputFile)

            Catch ex As OperationCanceledException
                Throw ' 取消操作时传递异常
            Catch ex As Exception
                LogError($"Encryption failed: [{inputFile}]: {ex.Message}")
                Throw
            End Try
        End Sub
        Private Sub UpdateAutoOutputPath(inputFile As String, outputFile As String)
            Try
                ' 标准化路径格式
                Dim normalizedInput = Path.GetFullPath(inputFile).TrimEnd(Path.DirectorySeparatorChar)
                Dim normalizedOutput = Path.GetFullPath(outputFile).TrimEnd(Path.DirectorySeparatorChar)

                ' 调试日志
                'Console.WriteLine($"[Update] 开始处理 | 输入: {normalizedInput} | 输出: {normalizedOutput}")

                Dim updateAction As Action = Sub()
                                                 Try
                                                     ' 确保在UI线程执行
                                                     If autoFileListView.InvokeRequired Then
                                                         autoFileListView.Invoke(Sub() UpdateListView(normalizedInput, normalizedOutput))
                                                     Else
                                                         UpdateListView(normalizedInput, normalizedOutput)
                                                     End If
                                                 Catch ex As Exception
                                                     Console.WriteLine($"[Error] UI更新异常: {ex}")
                                                 End Try
                                             End Sub

                ' 异步调用模式
                If autoFileListView.InvokeRequired Then
                    autoFileListView.BeginInvoke(updateAction)
                Else
                    updateAction.Invoke()
                End If

            Catch ex As Exception
                Console.WriteLine($"[Error] 预处理异常: {ex}")
            End Try
        End Sub

        Private Sub UpdateListView(inputPath As String, outputPath As String)
            Try
                ' 暂停界面绘制
                autoFileListView.BeginUpdate()

                Dim targetItem = autoFileListView.Items.Cast(Of ListViewItem)() _
            .FirstOrDefault(Function(i)
                                Dim itemInputPath = i.SubItems(1).Text.Trim()
                                Return String.Equals(itemInputPath, inputPath, StringComparison.OrdinalIgnoreCase)
                            End Function)

                If targetItem IsNot Nothing Then
                    ' 更新输出路径
                    targetItem.SubItems(2).Text = outputPath

                    ' 精确刷新指定区域
                    Dim bounds = targetItem.Bounds
                    autoFileListView.Invalidate(Rectangle.Inflate(bounds, 0, 2))
                    autoFileListView.Update()

                    ' 自动滚动到可见区域
                    targetItem.EnsureVisible()
                End If
            Catch ex As Exception
                Console.WriteLine($"[Error] 列表更新异常: {ex}")
            Finally
                ' 恢复界面绘制
                autoFileListView.EndUpdate()
            End Try
        End Sub

        '初始化数据库，如果不存在数据则创建
        Private Sub InitializeDatabase()
            Const dbName As String = "FileProcessLogDB.db"
            Dim dbPath As String = Path.Combine(Application.StartupPath, dbName)
            Dim connectionString As String = $"Data Source={dbPath};Version=3;"

            Try
                ' 验证数据库文件路径
                If Not File.Exists(dbPath) Then
                    SQLiteConnection.CreateFile(dbPath)
                    Debug.WriteLine($"数据库文件已创建：{dbPath}")
                End If

                Using connection As New SQLiteConnection(connectionString)
                    connection.Open()
                    Debug.WriteLine("数据库连接成功打开")

                    ' 创建Folder_License_Map表
                    Using cmd As New SQLiteCommand(connection)
                        cmd.CommandText = "CREATE TABLE IF NOT EXISTS Folder_License_Map (
                            folder_id INTEGER PRIMARY KEY AUTOINCREMENT,
                            folder_path TEXT NOT NULL UNIQUE CHECK(LENGTH(folder_path) > 0),
                            folder_hash TEXT NOT NULL UNIQUE CHECK(LENGTH(folder_hash) = 64),
                            profile_id TEXT NOT NULL,
                            profile_name TEXT NOT NULL COLLATE NOCASE,
                            product_id TEXT NOT NULL DEFAULT '0',
                            created_time DATETIME DEFAULT (datetime('now','localtime'))
                        )"
                        cmd.ExecuteNonQuery()
                        Debug.WriteLine("Folder_License_Map表已创建/验证")
                    End Using

                    ' 创建File_Process_Details表
                    Using cmd As New SQLiteCommand(connection)
                        cmd.CommandText = "CREATE TABLE IF NOT EXISTS File_Process_Details (
                            file_id INTEGER PRIMARY KEY AUTOINCREMENT,
                            profile_id INTEGER NOT NULL REFERENCES Folder_License_Map(folder_id),
                            file_path TEXT NOT NULL UNIQUE CHECK(LENGTH(file_path) > 0),
                            file_hash TEXT NOT NULL CHECK(LENGTH(file_hash) = 64),
                            status INTEGER NOT NULL DEFAULT 0 CHECK(status IN (0, 1, 2)),
                            process_time DATETIME DEFAULT (datetime('now','localtime'))
                        )"
                        cmd.ExecuteNonQuery()
                        Debug.WriteLine("File_Process_Details表已创建/验证")
                    End Using

                    ' 验证表结构
                    VerifyTableStructure(connection)

                End Using
            Catch ex As SQLiteException
                Debug.WriteLine($"SQL错误 [{ex.ErrorCode}]：{ex.Message}")
                MessageBox.Show($"数据库初始化失败：{ex.Message}")
            Catch ex As Exception
                Debug.WriteLine($"初始化异常：{ex.Message}")
                MessageBox.Show($"系统错误：{ex.Message}")
            End Try
        End Sub

        Private Sub VerifyTableStructure(conn As SQLiteConnection)
            Dim tables As New List(Of String) From {"Folder_License_Map", "File_Process_Details"}

            Using cmd As New SQLiteCommand("SELECT name FROM sqlite_master WHERE type='table'", conn)
                Using reader = cmd.ExecuteReader()
                    While reader.Read()
                        tables.Remove(reader.GetString(0))
                    End While
                End Using
            End Using

            If tables.Count > 0 Then
                Throw New Exception($"缺失表：{String.Join(", ", tables)}")
            End If
        End Sub

        Private Sub InsertFileRecord(inputFile As String, profileId As String)
            Const MaxRetries As Integer = 5
            Dim retryDelay As Integer = 100 ' 毫秒

            For retry As Integer = 1 To MaxRetries
                Try
                    ' 创建数据库连接
                    Using connection As New SQLiteConnection("Data Source=FileProcessLogDB.db;Version=3;Pooling=True;Cache=Shared;Journal Mode=WAL;BusyTimeout=5000;")
                        connection.Open()

                        ' 插入文件记录
                        Dim fileHash = GetFileHash(inputFile)
                        Dim status = 1 ' 1表示已处理
                        Dim insertFileQuery = "INSERT OR IGNORE INTO File_Process_Details (profile_id, file_path, file_hash, status) VALUES (@profile_id, @file_path, @file_hash, @status)"
                        Using insertFileCommand As New SQLiteCommand(insertFileQuery, connection)
                            insertFileCommand.Parameters.AddWithValue("@profile_id", profileId)
                            insertFileCommand.Parameters.AddWithValue("@file_path", inputFile)
                            insertFileCommand.Parameters.AddWithValue("@file_hash", fileHash)
                            insertFileCommand.Parameters.AddWithValue("@status", status)
                            insertFileCommand.ExecuteNonQuery()
                        End Using

                    End Using
                Catch ex As SQLiteException When ex.Message.Contains("database is locked")
                    ' 如果是数据库锁定错误，等待并重试
                    If retry = MaxRetries Then
                        LogError($"数据库插入文件记录失败：{ex.Message}")
                        Throw
                    End If
                    System.Threading.Thread.Sleep(retryDelay)
                Catch ex As Exception
                    LogError($"数据库操作异常：{ex.Message}")
                Throw
                End Try
            Next
        End Sub
        Private Sub InsertFolderRecord(inputFile As String, profileId As String)
            Const MaxRetries As Integer = 5
            Dim retryDelay As Integer = 100 ' 毫秒

            For retry As Integer = 1 To MaxRetries
                Try
                    ' 创建数据库连接
                    Using connection As New SQLiteConnection("Data Source=FileProcessLogDB.db;Version=3;Pooling=True;Cache=Shared;Journal Mode=WAL;BusyTimeout=5000;")
                        connection.Open()

                        ' 插入文件夹记录，使用事务
                        Using transaction As SQLiteTransaction = connection.BeginTransaction()
                            Try
                                Dim folderPath = inputFile
                                Dim folderHash = GetFolderHash(folderPath)

                                Dim folderPathInfo As New DirectoryInfo(folderPath)
                                Dim profileName = folderPathInfo.Name

                                Dim productId = "0"
                                Dim parts = profileName.Split(New Char() {"_"}, 2, StringSplitOptions.RemoveEmptyEntries)
                                If parts.Length >= 2 Then
                                    productId = parts(0).Trim()
                                End If

                                Dim insertFolderQuery = "INSERT OR IGNORE INTO Folder_License_Map (folder_path, folder_hash, profile_id, profile_name, product_id) VALUES (@folder_path, @folder_hash, @profile_id, @profile_name, @product_id)"
                                Using insertFolderCommand As New SQLiteCommand(insertFolderQuery, connection)
                                    insertFolderCommand.Parameters.AddWithValue("@folder_path", folderPath)
                                    insertFolderCommand.Parameters.AddWithValue("@folder_hash", folderHash)
                                    insertFolderCommand.Parameters.AddWithValue("@profile_id", profileId)
                                    insertFolderCommand.Parameters.AddWithValue("@profile_name", profileName)
                                    insertFolderCommand.Parameters.AddWithValue("@product_id", productId)
                                    insertFolderCommand.ExecuteNonQuery()
                                End Using

                                ' 提交事务
                                transaction.Commit()
                                Return ' 成功插入后退出方法
                            Catch ex As Exception
                                ' 回滚事务
                                transaction.Rollback()
                                Throw
                            End Try
                        End Using
                    End Using
                Catch ex As SQLiteException When ex.Message.Contains("database is locked")
                    ' 如果是数据库锁定错误，等待并重试
                    If retry = MaxRetries Then
                        LogError($"数据库插入失败：{ex.Message}")
                        Throw
                    End If
                    System.Threading.Thread.Sleep(retryDelay)
                Catch ex As Exception
                    LogError($"数据库操作异常：{ex.Message}")
                    Throw
                End Try
            Next
        End Sub
        Function QueryProcessedFolders(connection As SQLiteConnection, folderPath As String) As String
            Dim selectFolderQuery = "SELECT profile_id FROM Folder_License_Map WHERE folder_path = @folder_path"
            Using selectFolderCommand As New SQLiteCommand(selectFolderQuery, connection)
                selectFolderCommand.Parameters.AddWithValue("@folder_path", folderPath)
                Using reader As SQLiteDataReader = selectFolderCommand.ExecuteReader()
                    If reader.Read() Then
                        Return reader.GetString(0)
                    End If
                End Using
            End Using
            Return Nothing
        End Function

        Function QueryProcessedFiles(connection As SQLiteConnection, filePath As String) As Boolean
            Dim selectFileQuery = "SELECT * FROM File_Process_Details WHERE file_path = @file_path"
            Using selectFileCommand As New SQLiteCommand(selectFileQuery, connection)
                selectFileCommand.Parameters.AddWithValue("@file_path", filePath)
                Using reader As SQLiteDataReader = selectFileCommand.ExecuteReader()
                    Return reader.Read()
                End Using
            End Using
        End Function

        Private Function GetLicenseOutputPath(inputFile As String) As (outputPath As String, profileId As String)
            Try
                Dim inputRoot As String = Path.GetFullPath(autoInputTextBox.Text)

                Dim fullInputPath As String = Path.GetFullPath(inputFile)

                Dim folderPath = GetFirstLevelFolder(inputRoot, fullInputPath)

                Dim baseFolder = Path.GetFileName(folderPath)

                Dim parsed = ParseFolderName(baseFolder)


                Using conn As New SQLiteConnection("Data Source=FileProcessLogDB.db;Version=3;Pooling=True;Cache=Shared;Journal Mode=WAL;BusyTimeout=5000;")
                    conn.Open()

                    Using cmd As New SQLiteCommand("SELECT profile_id FROM Folder_License_Map WHERE folder_path = @path", conn)

                        cmd.Parameters.AddWithValue("@path", folderPath)
                        Dim profileId = cmd.ExecuteScalar()?.ToString()

                        If String.IsNullOrEmpty(profileId) Then
                            ' 记录更详细的日志信息
                            ' LogError($"找不到路径 {folderPath} 对应的许可证模板")

                            ' 尝试重新创建许可证模板
                            profileId = AddLicenseProfile(folderPath, baseFolder, parsed.productID)

                            If String.IsNullOrEmpty(profileId) Then
                                Throw New KeyNotFoundException(If(_isInternational, $"Profile not found for: {baseFolder}", $"找不到 {baseFolder} 对应的许可证模板"))
                            End If
                        End If


                        Dim outputBaseDir As String = Path.Combine(autoOutputTextBox.Text, $"{profileId}_{parsed.displayName}")

                        ' 保留子目录结构
                        Dim relativePath As String = fullInputPath.Substring(Path.Combine(inputRoot, baseFolder).Length + 1)

                        Dim outputPath = Path.Combine(outputBaseDir, Path.GetDirectoryName(relativePath), $"{Path.GetFileNameWithoutExtension(inputFile)}_P{Path.GetExtension(inputFile)}")

                        Return (outputPath, profileId)
                    End Using
                End Using

            Catch ex As Exception
                LogError($"路径生成失败: {ex.Message}")
                Throw
            End Try
        End Function

        Private Function GetFirstLevelFolder(inputRoot As String, fullPath As String) As String
            Dim currentDir As String = Path.GetDirectoryName(fullPath)

            ' 处理根目录直接包含文件的情况
            If currentDir.Equals(inputRoot, StringComparison.OrdinalIgnoreCase) Then
                Return New DirectoryInfo(inputRoot).Name
            End If

            While Not Directory.GetParent(currentDir).FullName.Equals(inputRoot)
                currentDir = Directory.GetParent(currentDir).FullName
            End While

            Return currentDir
        End Function

        ' 
        Private Async Function CreateLicenseProfilesAsync(folderPath As String) As Task
            Try
                ' 1. 获取所有一级子目录
                Dim dirs As String() = Directory.GetDirectories(folderPath)

                ' 2. 并行处理每个子目录
                Await Task.Run(Async Function()
                                   For Each dirPath In dirs
                                       Try
                                           Dim folderName = Path.GetFileName(dirPath)
                                           Dim parsed = ParseFolderName(folderName)

                                           'LogError($"文件名: {folderName}")
                                           'LogError($" ProductID: {parsed.productID} 文件名: {parsed.displayName}")

                                           Using conn As New SQLiteConnection("Data Source=FileProcessLogDB.db;Version=3;Pooling=True;Cache=Shared;Journal Mode=WAL;BusyTimeout=5000;")
                                               Await conn.OpenAsync()

                                               Using cmd As New SQLiteCommand("SELECT profile_id FROM Folder_License_Map WHERE folder_path = @path", conn)

                                                   cmd.Parameters.AddWithValue("@path", dirPath)
                                                   Dim existingId = Await cmd.ExecuteScalarAsync()

                                                   If existingId IsNot Nothing Then Continue For ' 跳过已存在记录
                                               End Using
                                           End Using

                                           ' 添加许可证模板
                                           AddLicenseProfile(dirPath, folderName, parsed.productID)

                                       Catch ex As Exception
                                           LogError($"子目录处理失败: {dirPath} - {ex.Message}")
                                       End Try
                                   Next
                               End Function)
            Catch ex As Exception
                LogError($"模板创建异常: {ex.Message}")
                Throw
            End Try
        End Function

        Private Function AddLicenseProfile(folderPath As String, folderName As String, productID As String) As String

            Try
                Using Connection As New SQLiteConnection("Data Source=FileProcessLogDB.db;Version=3;Pooling=True;Cache=Shared;Journal Mode=WAL;BusyTimeout=5000;")
                    Connection.Open()
                    Dim resultProfileId As String = QueryProcessedFolders(Connection, folderPath)

                    If resultProfileId IsNot Nothing Then
                        Return resultProfileId
                    End If
                End Using

                Using drm = CreateServiceClient()
                    Dim ResultID = drm.AddLicenseProfile(_adminEmail, _authString, folderName, productID, "False")
                    Dim ProfileID As Integer

                    If Integer.TryParse(ResultID, ProfileID) Then

                        ' 插入文件记录到SQLite数据库
                        InsertFolderRecord(folderPath, ProfileID.ToString())

                        ' 立即验证是否成功插入
                        Using conn As New SQLiteConnection("Data Source=FileProcessLogDB.db;Version=3;Pooling=True;Cache=Shared;Journal Mode=WAL;BusyTimeout=5000;")
                            conn.Open()
                            Using cmd As New SQLiteCommand("SELECT profile_id FROM Folder_License_Map WHERE folder_path = @path", conn)
                                cmd.Parameters.AddWithValue("@path", folderPath)
                                Dim verifiedProfileId = cmd.ExecuteScalar()?.ToString()

                                If String.IsNullOrEmpty(verifiedProfileId) Then
                                    LogError($"插入文件夹记录失败: {folderPath}")
                                    Throw New Exception("无法验证文件夹记录")
                                End If
                            End Using
                            conn.Close()
                        End Using
                        Return ProfileID.ToString()
                    Else
                        ' 记录详细错误信息
                        LogError($"添加许可证模板失败：{ResultID}")
                        MessageBox.Show(ResultID)
                        Return Nothing
                    End If
                End Using
            Catch ex As Exception
                ' 详细记录异常信息
                LogError($"创建许可证模板异常：{ex.Message}")
                Throw
            End Try
        End Function

        Private Function GetFolderHash(folderPath As String) As String
            If Not Directory.Exists(folderPath) Then Return String.Empty

            Using sha As SHA256 = SHA256.Create()
                ' 1. 规范化路径
                Dim normalizedPath = Path.GetFullPath(folderPath).ToLowerInvariant()

                ' 2. 包含文件夹元数据
                Dim metaData = New StringBuilder()
                Dim dirInfo = New DirectoryInfo(normalizedPath)

                metaData.Append($"Name:{dirInfo.Name}|")
                metaData.Append($"Created:{dirInfo.CreationTimeUtc.Ticks}|")
                metaData.Append($"Modified:{dirInfo.LastWriteTimeUtc.Ticks}|")

                ' 3. 包含所有文件内容哈希
                For Each file In dirInfo.EnumerateFiles("*", SearchOption.AllDirectories)
                    metaData.Append($"{file.Name}:{GetFileHash(file.FullName)}|")
                Next

                ' 4. 生成最终哈希
                Dim hashBytes = sha.ComputeHash(Encoding.UTF8.GetBytes(metaData.ToString()))
                Return BitConverter.ToString(hashBytes).Replace("-", "")
            End Using
        End Function

        Private Function GetFileHash(filePath As String) As String
            Using sha As SHA256 = SHA256.Create()
                Using stream = File.OpenRead(filePath)
                    Dim hash = sha.ComputeHash(stream)
                    Return BitConverter.ToString(hash).Replace("-", "")
                End Using
            End Using
        End Function


        Private Function ParseFolderName(folderName As String) As (productID As String, displayName As String)
            ' 分割第一个下划线前的部分作为ProductID，剩余部分作为DisplayName
            Dim parts = folderName.Split(New Char() {"_"}, 2, StringSplitOptions.RemoveEmptyEntries)

            Dim productID = "0"
            Dim displayName = folderName ' 默认使用完整名称

            If parts.Length >= 2 Then
                If Integer.TryParse(parts(0), Nothing) Then
                    productID = parts(0).Trim()
                Else
                    LogError($"非数字产品ID: {parts(0)}，使用默认值0")
                End If
            ElseIf parts.Length = 1 Then
                If Integer.TryParse(parts(0), Nothing) Then
                    productID = parts(0).Trim()
                End If
            End If

            Return (productID, displayName)
        End Function


        Private Sub btnClearList_Click(sender As Object, e As EventArgs)
            ' 确认操作
            Dim msgText As String = If(_isInternational,
                             "Are you sure you want to clear the file list?",
                             "确定要清空文件列表吗？")

            Dim result = MessageBox.Show(msgText,
                               If(_isInternational, "Confirm", "确认"),
                               MessageBoxButtons.OKCancel,
                               MessageBoxIcon.Question)

            If result = DialogResult.OK Then
                ClearFileList()
            End If
        End Sub

        Private Sub btnAutoStop_Click(sender As Object, e As EventArgs)

            ' 显示确认对话框
            Dim result
            If _isInternational Then

                result = MessageBox.Show("Are you sure you want to stop scanning the files?",
                                    "Confirm Stop",
                                    MessageBoxButtons.OKCancel,
                                    MessageBoxIcon.Question)
            Else

                result = MessageBox.Show("您确定要停止扫描文件吗?",
                                    "确认停止",
                                    MessageBoxButtons.OKCancel,
                                    MessageBoxIcon.Question)
            End If
            If result = DialogResult.OK Then
                autoCts?.Cancel()
                fileSystemWatcher?.Dispose()

                ' 恢复按钮状态
                UpdateUI(Sub()
                             btnAutoStart.Enabled = True
                             btnAutoStop.Enabled = False
                             convertPDFCheckBoxAuto.Enabled = True
                             btnClearList.Enabled = True
                         End Sub)
            End If
        End Sub

        Protected Overrides Sub OnFormClosing(e As FormClosingEventArgs)

            Dim result
            If _isInternational Then
                result = MessageBox.Show("Are you sure you want to exit the program?",
                                "Confirm Exit",
                                MessageBoxButtons.OKCancel,
                                MessageBoxIcon.Question)
            Else
                result = MessageBox.Show("您确定要退出该程序吗？",
                                "是否确定退出",
                                MessageBoxButtons.OKCancel,
                                MessageBoxIcon.Question)
            End If
            ' 根据用户选择处理关闭操作
            If result = DialogResult.Cancel Then
                e.Cancel = True ' 取消关闭
            Else
                ' 执行清理操作
                KillAllProcesses()
            End If
            MyBase.OnFormClosing(e)
        End Sub

        Private Sub KillAllProcesses()
            Do While Not runningProcesses.IsEmpty
                Dim process As Process = Nothing
                If runningProcesses.TryTake(process) Then
                    Try
                        If Not process.HasExited Then
                            process.Kill()
                            process.WaitForExit(1000) ' 等待进程终止
                        End If
                    Catch ex As Exception
                        ' 忽略已退出的进程
                    Finally
                        process.Dispose()
                    End Try
                End If
            Loop
        End Sub


        ' 根据选择的区域设置界面语言：
        Private Sub SetLanguage(_isInternational As Boolean)
            If _isInternational Then

                Me.Text = "DRM-X 4.0 Automatic Batch Encryption Tool"

                btnStart.Text = "Start"
                btnStop.Text = "Stop"
                createFolderCheckBox.Text = "Create folder in output dir"
                convertPDFCheckBox.Text = "Convert PDF to HTML"
                convertPDFCheckBoxAuto.Text = "Convert PDF to HTML"
                manualTab.Text = "Manual Encryption"
                autoTab.Text = "Auto Encryption"
                Dim inputlabel As Label = TryCast(inputPanel.Controls(0), Label)
                inputlabel.Text = "Input Directory:"
                Dim inputbtn As Button = TryCast(inputPanel.Controls(2), Button)
                inputbtn.Text = "Browse..."
                Dim outputlabel As Label = TryCast(outputPanel.Controls(0), Label)
                outputlabel.Text = "Output Directory:"
                Dim outputbtn As Button = TryCast(outputPanel.Controls(2), Button)
                outputbtn.Text = "Browse..."

                Dim licenselabel As Label = TryCast(licensePanel.Controls(0), Label)
                licenselabel.Text = "License Profile:"

                Dim autoInputlabel As Label = TryCast(AutoInputPanel.Controls(0), Label)
                autoInputlabel.Text = "Scan Directory:"
                Dim autoInputbtn As Button = TryCast(AutoInputPanel.Controls(2), Button)
                autoInputbtn.Text = "Browse..."

                Dim autoOutputlabel As Label = TryCast(AutoOutputPanel.Controls(0), Label)
                autoOutputlabel.Text = "Output Directory:"
                Dim autoOutputbtn As Button = TryCast(AutoOutputPanel.Controls(2), Button)
                autoOutputbtn.Text = "Browse..."

                btnAutoStart.Text = "Start Scanning"
                btnAutoStop.Text = "Stop Scanning"
                btnClearList.Text = "Clear List"

                statusLabel.Text = "Status"
                lblStatus.Text = "Ready"
            Else
                Me.Text = "DRM-X 4.0 自动批量加密工具"

                btnStart.Text = "开始"
                btnStop.Text = "停止"
                createFolderCheckBox.Text = "在输出目录下创建文件夹"
                convertPDFCheckBox.Text = "将PDF转换成HTML"
                convertPDFCheckBoxAuto.Text = "将PDF转换为HTML"
                manualTab.Text = "手动加密"
                autoTab.Text = "自动加密"
                Dim inputlabel As Label = TryCast(inputPanel.Controls(0), Label)
                inputlabel.Text = "输入目录："
                Dim inputbtn As Button = TryCast(inputPanel.Controls(2), Button)
                inputbtn.Text = "浏览..."
                Dim outputlabel As Label = TryCast(outputPanel.Controls(0), Label)
                outputlabel.Text = "输出目录："
                Dim outputbtn As Button = TryCast(outputPanel.Controls(2), Button)
                outputbtn.Text = "浏览..."

                Dim licenselabel As Label = TryCast(licensePanel.Controls(0), Label)
                licenselabel.Text = "许可证模板："

                Dim autoInputlabel As Label = TryCast(AutoInputPanel.Controls(0), Label)
                autoInputlabel.Text = "扫描目录："
                Dim autoInputbtn As Button = TryCast(AutoInputPanel.Controls(2), Button)
                autoInputbtn.Text = "浏览..."

                Dim autoOutputlabel As Label = TryCast(AutoOutputPanel.Controls(0), Label)
                autoOutputlabel.Text = "输出目录："
                Dim autoOutputbtn As Button = TryCast(AutoOutputPanel.Controls(2), Button)
                autoOutputbtn.Text = "浏览..."

                btnAutoStart.Text = "开始扫描"
                btnAutoStop.Text = "停止扫描"
                btnClearList.Text = "清空列表"

                statusLabel.Text = "状态"
                lblStatus.Text = "已就绪"
            End If
        End Sub
        Private Sub ClearFileList()
            If autoFileListView.InvokeRequired Then
                autoFileListView.BeginInvoke(Sub() ClearFileList())
            Else
                ' 清空列表视图
                autoFileListView.Items.Clear()

                ' 清空处理队列
                processingQueue = New Concurrent.ConcurrentQueue(Of String)()
                processedFiles = New Concurrent.ConcurrentDictionary(Of String, Boolean)()

                ' 清空重试队列
                retryQueue = New Concurrent.ConcurrentQueue(Of RetryFileInfo)()

                ' 更新状态
                If _isInternational Then
                    UpdateStatusLabel("File list cleared", Color.DarkGreen)
                Else
                    UpdateStatusLabel("文件列表已清空", Color.DarkGreen)
                End If
            End If
        End Sub

        Private Sub InitializeFileWatcher()
            folderWatcher = New FileSystemWatcher() With {
                .Path = autoInputTextBox.Text,
                .IncludeSubdirectories = False, ' 仅监控根目录
                .NotifyFilter = NotifyFilters.DirectoryName,
                .EnableRaisingEvents = True
            }
            AddHandler folderWatcher.Created, AddressOf OnNewFolderCreated
        End Sub

        ' 文件夹创建事件处理
        Private Async Sub OnNewFolderCreated(sender As Object, e As FileSystemEventArgs)
            Dim needDelayReset As Boolean = False
            Try
                Dim newFolderPath = e.FullPath
                Dim folderName = Path.GetFileName(newFolderPath)

                Dim profileId As String = Nothing

                ' 使用事务确保数据一致性
                Using conn As New SQLiteConnection("FileProcessLogDB.db;Version=3;Pooling=True;Cache=Shared;Journal Mode=WAL;BusyTimeout=5000;")
                    conn.Open()
                    Using transaction = conn.BeginTransaction()
                        Try
                            ' 检查是否已存在记录
                            Using cmd As New SQLiteCommand("SELECT profile_id FROM Folder_License_Map WHERE folder_path = @path", conn)
                                cmd.Parameters.AddWithValue("@path", newFolderPath)
                                profileId = Convert.ToString(cmd.ExecuteScalar())
                            End Using

                            ' 如果不存在profileId，则创建
                            If String.IsNullOrEmpty(profileId) Then
                                Dim parsed = ParseFolderName(folderName)

                                ' 在此处调用AddLicenseProfile并获取profileId
                                profileId = AddLicenseProfile(newFolderPath, folderName, parsed.productID)

                                ' 如果仍然为空，则记录日志并抛出异常
                                If String.IsNullOrEmpty(profileId) Then
                                    LogError($"无法为文件夹 {newFolderPath} 创建许可证模板")
                                    Throw New Exception("Failed to create license profile")
                                End If
                            End If

                            ' 提交事务
                            transaction.Commit()
                        Catch ex As Exception
                            ' 回滚事务
                            transaction.Rollback()
                            LogError($"处理文件夹 {newFolderPath} 时出错: {ex.Message}")
                            Throw
                        End Try
                    End Using
                End Using

                If _isInternational Then
                    UpdateStatusLabel($"Detected new folder: {Path.GetFileName(e.FullPath)}", Color.DarkBlue)
                Else
                    UpdateStatusLabel($"检测到新目录: {Path.GetFileName(e.FullPath)}", Color.DarkBlue)
                End If

                ' 扫描新文件夹中的现有文件
                Dim newFiles = Directory.EnumerateFiles(newFolderPath, "*.*", SearchOption.AllDirectories).Where(Function(f) allowedExtensions.Contains(Path.GetExtension(f)))

                For Each file In newFiles
                    WaitForFileReady(file)

                    If Not processedFiles.ContainsKey(file) Then
                        processingQueue.Enqueue(file)
                        If _isInternational Then
                            UpdateAutoFileStatus(file, "New files have been detected", Color.Blue)
                        Else
                            UpdateAutoFileStatus(file, "检测到新文件", Color.Blue)
                        End If
                    End If
                Next

                ' 更新完成状态
                If _isInternational Then
                    UpdateStatusLabel($"Folder processing completed: {folderName}", Color.Green)
                Else
                    UpdateStatusLabel($"目录处理完成: {folderName}", Color.Green)
                End If

            Catch ex As Exception
                LogError($"自动创建模板失败: {ex.Message}")
                needDelayReset = True
            End Try
            If needDelayReset Then
                Await Task.Delay(3000)
                UpdateStatusLabel(If(_isInternational, "Scanning...", "正在扫描..."), Color.Blue)
            End If
        End Sub


    End Class

End Namespace
