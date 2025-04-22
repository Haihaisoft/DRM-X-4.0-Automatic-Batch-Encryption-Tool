' Haihaisoft Open Source DRM Provider: https://www.haihaisoft.com
' DRM-X Content Protection Platform: https://www.drm-x.com
' Project Page: https://www.drm-x.com/DRM-X-4.0-Automatic-Batch-Encryption-Tool.aspx

Imports System.Security.Cryptography
Imports System.Text

Public Class SecurityHelper
    Public Shared Function Encrypt(text As String) As String
        Dim bytes() As Byte = Encoding.UTF8.GetBytes(text)
        bytes = ProtectedData.Protect(bytes, Nothing, DataProtectionScope.CurrentUser)
        Return Convert.ToBase64String(bytes)
    End Function

    Public Shared Function Decrypt(encryptedText As String) As String
        Try
            Dim bytes() As Byte = Convert.FromBase64String(encryptedText)
            bytes = ProtectedData.Unprotect(bytes, Nothing, DataProtectionScope.CurrentUser)
            Return Encoding.UTF8.GetString(bytes)
        Catch
            Return String.Empty
        End Try
    End Function
End Class
