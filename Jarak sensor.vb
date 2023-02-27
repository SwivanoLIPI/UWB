Public Class Jarak_sensor

    Private Sub Jarak_sensor_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        SerialPort1.BaudRate = 115200 'CmbBaud.SelectedItem
        SerialPort1.PortName = "COM12" 'CmbScanPort.SelectedItem
        SerialPort1.Open()
        Timer1.Enabled = True
        Timer1.Interval = 1000
        Timer1.Start()


    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Form1.TextBox12.Text = Val(Me.TextBox2.Text) / 100
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        TextBox1.Text = (SerialPort1.ReadLine)
        If Not TextBox1.Text = 0 Then
            TextBox2.Text = Val(Val(TextBox1.Text) * 100)
        End If
    End Sub
End Class