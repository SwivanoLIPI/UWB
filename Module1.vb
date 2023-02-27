Imports MySql.Data.MySqlClient
Module Module1
    Public conn As MySqlConnection
    Public da As MySqlDataAdapter
    Public ds As DataSet
    Public queri As String
    Public cmd As MySqlCommand
    ' Sub koneksi()
    'Try
    '  queri = "datasource=localhost;port=3307;username=root;password=password;database=runmeter;"
    ' conn = New MySqlConnection(queri)
    ' If conn.State = ConnectionState.Closed Then
    '   conn.Open()
    '     Form1.Label12.Text = "Connection Successful"
    '    Form1.Label12.ForeColor = Color.Green


    '   conn.Close()

    '  End If
    'Catch ex As Exception

    'End Try
    ' End Sub

End Module
