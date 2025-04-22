Imports System.Runtime.CompilerServices
Imports System.Diagnostics
Imports System.Threading
Imports System.Threading.Tasks

Public Module ProcessExtensions

    ''' <summary>
    ''' 异步等待进程退出
    ''' </summary>
    <Extension()>
    Public Function WaitForExitAsync(process As Process, Optional cancellationToken As CancellationToken = Nothing) As Task
        ' 如果进程已退出，直接返回完成的任务
        If process.HasExited Then
            Return Task.CompletedTask
        End If

        ' 创建一个 TaskCompletionSource 来跟踪异步状态
        Dim tcs As New TaskCompletionSource(Of Boolean)

        ' 注册进程退出事件
        AddHandler process.Exited, Sub(sender, e)
                                       tcs.TrySetResult(True)
                                   End Sub

        ' 启用进程的事件触发（默认禁用）
        process.EnableRaisingEvents = True

        ' 如果取消令牌被触发，终止等待
        If cancellationToken <> Nothing Then
            cancellationToken.Register(Sub()
                                           tcs.TrySetCanceled()
                                       End Sub)
        End If

        ' 返回异步任务
        Return tcs.Task
    End Function

End Module