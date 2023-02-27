Imports System.Drawing
Imports System.Globalization
Imports System.Windows
Imports System.Windows.Forms.DataVisualization.Charting
Imports System.Data.OleDb
Imports System.Linq
Imports MySql.Data.MySqlClient
Imports System.Data.Sql
Imports System
Imports System.Data


Public Class Form1
    Dim myPort As Array
    Dim baris As Integer
    Dim iterasi As Integer
    Dim TrendLine As New System.Windows.Forms.DataVisualization.Charting.Series("TrendLine")
    Dim vi As Integer
    Dim Rand As New Random
    Dim curve As Integer
    Dim FileName As String
    Dim header As String
    Dim tipeA As Integer = 3
    Dim l As ListViewItem
    Dim P_Stack As String
    Dim N As Integer
    Dim x As String
    Dim Tc As String
    Dim P_H2O As String
    Dim LV As ListView
    Dim skb As String

    Dim j_o2 As String
    Dim z As Integer
    Dim v As String
    Dim c1 As String
    Dim c2 As String
    Dim c3 As String
    Dim c4 As String
    Dim i As Integer
    Dim lin As Integer = 0
    Dim vdec As String


    Dim Delimiter As String
    'Dim sw As StreamWriter
    Dim legend As String
    Dim sfDialog As New SaveFileDialog

    Dim w As String
    Dim k As Integer
    Dim da As MySqlDataAdapter
    Dim ds As DataSet
    Dim queri As String
    Dim cm As MySqlCommand
    Dim conn As MySqlConnection
    Delegate Sub SetTextCallBack(ByVal [text] As String)
    Dim DisplaySeriesTrendLine As Boolean = False
    Private Const MAX_RECURSIVE_CALLS As Integer = 1000000000
    Dim ga As String
    Dim al As String
    Dim b As Integer
    Dim a As Integer
    Dim r As Integer
    Dim sa As String
    Dim sb As String
    Dim p As String
    Dim q As String
    Dim chk As Boolean
    Dim xs As Integer = 0
    Dim ys As Integer = 0
    Dim j As String = 0
    Dim h As String = 0
    Dim ag As String = 0
    Dim mal As String = 0
    Dim swi As String
    Dim mel As String
    Dim si As String
    Dim ha As String = 0


    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
      
        Try
            TextBox7.Text = SerialPort1.ReadLine.ToString() '.Substring(0, 4)
            TextBox8.Text = SerialPort2.ReadLine.ToString() '.Substring(0, 4)
            ' chk = MessageTimeOut(CStr(SerialPort1.ReadLine) & " AND " & CStr(SerialPort2.ReadLine), "AMS production", 4)
        Catch ex As Exception
            TextBox7.Text = TextBox7.Text
            TextBox8.Text = TextBox8.Text
        End Try
        If Not CStr(TextBox7.Text.ToString().Substring(0, 1)) = "" And Not CStr(TextBox8.Text.ToString().Substring(0, 1)) = "" Then
            If Char.IsNumber(CStr(TextBox7.Text.ToString().Substring(0, 1))) = True And Char.IsNumber(CStr(TextBox8.Text.ToString().Substring(0, 1))) = True Then
                'If Not CStr(TextBox7.Text.ToString().Substring(0, 4)) = "" Or Not CStr(TextBox8.Text.ToString().Substring(0, 4)) = "" Then
                ' chk = MessageTimeOut(TextBox7.Text.ToString().Substring(0, 4) & " AND " & TextBox8.Text.ToString().Substring(0, 4), "AMS production", 4)

                '& " AND " & TextBox7.Text
                'If Not Convert.ToString(TextBox7.Text) = 0 Then
                'If Not Convert.ToString(TextBox8.Text) = 0 Then
                'MsgBox("d")
                'Label2.Text = TextBox7.Text
                ' Label4.Text = TextBox8.Text
                'if TextBox7.Text =
                '

                b = CStr(TextBox7.Text) 'port15
                r = CStr(TextBox8.Text)  'port14
                'chk = MessageTimeOut(a, "AMS production", 4)
                'b yang ini di rekayasa, hanya untuk eksperimen
                a = (CStr(TextBox12.Text) * 100)
                Label52.Text = r / b
                Label53.Text = b / r
                If TextBox7.Text = "0" Or TextBox8.Text = "0" Then
                    ' wait(1)
                    Return

                End If
                '(Convert.ToString(TextBox12.Text))
                'chk = MessageTimeOut(a, "pesan", 4)
                'Waspadai, sistem koma dan titik!!!!!! pada laptop anda

                'al = ((Convert.ToSingle(2 ^ 2) + Convert.ToSingle(2 ^ 2) - Convert.ToSingle(2 ^ 2)) / (Convert.ToSingle(2 * 2 * 2)))
                ' al = ((Convert.ToString(2 ^ 2) + Convert.ToString(2 ^ 2) - Convert.ToString(2 ^ 2)) / (Convert.ToString(2 * 2 * 2)))
                '                           A
                '                          / \
                '               r = com14 /   \ b = com15
                '                       B/_____\C

                'cos sudut alpha(<B)

                Dim sud As Single
                Dim dut As Single
                sud = b / r
                dut = r / b
                'If Not dut=1 then
                If Not sud = 1 Or dut = 1 Then
                    'If sud > 0.9999 And sud < 0.99999 Then
                    'TextBox9.Text = CInt(Convert.ToSingle(Convert.ToSingle(TextBox3.Text) / 2) + Convert.ToSingle(Convert.ToSingle(TextBox12.Text) / 2))
                    'Else

                    al = Math.Round(Convert.ToSingle((Convert.ToSingle(a ^ 2) + Convert.ToSingle(r ^ 2) - Convert.ToSingle(b ^ 2)) / ((2 * Convert.ToSingle(a) * Convert.ToSingle(r)))), 4) '* 180 / Math.PI
                    'cos sudut gama (<C)

                    ga = Math.Round(Convert.ToSingle((Convert.ToSingle(b ^ 2) + Convert.ToSingle(a ^ 2) - Convert.ToSingle(r ^ 2)) / ((2 * Convert.ToSingle(b) * Convert.ToSingle(a)))), 4) '* 180 / Math.PI

                    If al > 1 Or ga > 1 Then
                        al = 1
                        ga = 1
                    Else
                        al = al
                        ga = ga
                    End If
                    Label54.Text = al
                    Label55.Text = ga
                    Dim aca As Single
                    Dim abe As Single
                    Dim te As Single
                    Dim bete As Single


                    'pikirkan jika sudut tumpul

                    aca = Math.Round(Convert.ToSingle((Math.Acos(ga)) * 180 / Math.PI), 4) 'Acos gamma = (<C)
                    Label50.Text = aca
                    abe = Math.Round(Convert.ToSingle((Math.Acos(al)) * 180 / Math.PI), 4) 'Acos alpha = (<B)
                    Label51.Text = abe
                    te = Math.Round(Convert.ToSingle(180 - aca), 4)
                    bete = Math.Round(Convert.ToSingle(Math.Cos(te) * Math.PI / 180), 4)
                    If aca < 5 And abe > 5 Then
                        sa = Convert.ToSingle(0)
                        TextBox5.Text = Math.Round(Convert.ToSingle(0), 4) '/ 100
                        sb = Math.Round(Convert.ToSingle(Math.Sin(abe * Math.PI / 180)), 4) 'sin(<B)
                    ElseIf abe < 5 And aca > 5 Then
                        sa = Math.Round(Convert.ToSingle(Math.Sin(aca * Math.PI / 180)), 4) 'sin(<C)
                        sb = Convert.ToSingle(0)
                    ElseIf abe < 5 And aca < 5 Then
                        sa = Convert.ToSingle(0)
                        sb = Convert.ToSingle(0)
                    Else
                        sa = Math.Round(Convert.ToSingle(Math.Sin(aca * Math.PI / 180)), 4) 'sin(<C)
                        sb = Math.Round(Convert.ToSingle(Math.Sin(abe * Math.PI / 180)), 4) 'sin(<B)
                        TextBox5.Text = Math.Round(Convert.ToSingle(sb), 4) '/ 100
                    End If

                    Dim rd As Single
                    If aca > 90 And abe < 90 Then
                        rd = Math.Round(Convert.ToSingle(Convert.ToSingle(TextBox3.Text) / 2) + Convert.ToSingle(Convert.ToSingle(TextBox12.Text) / 2) + Convert.ToSingle(Convert.ToSingle(b) * Convert.ToSingle(bete) / 100), 4)
                        If Not rd = "NaN" Then
                            TextBox9.Text = Math.Round(Convert.ToSingle(rd), 4)
                        Else
                            TextBox9.Text = Math.Round(Convert.ToSingle(TextBox9.Text), 4)
                        End If
                        'ElseIf aca = 90 And abe < 90 Then

                    ElseIf aca = 90 And abe < 90 Then
                        rd = Math.Round(Convert.ToSingle(Convert.ToSingle(TextBox3.Text) / 2) + Convert.ToSingle(Convert.ToSingle(TextBox12.Text) / 2), 4)
                        If Not rd = "NaN" Then
                            TextBox9.Text = Math.Round(Convert.ToSingle(rd), 4)
                        Else
                            TextBox9.Text = Math.Round(Convert.ToSingle(TextBox9.Text), 4)
                        End If
                    ElseIf aca < 90 And abe = 90 Then
                        rd = Math.Round((Convert.ToSingle(TextBox3.Text) / 2) - Convert.ToSingle(Convert.ToSingle(TextBox12.Text) / 2), 4)
                        If Not rd = "NaN" Then
                            TextBox9.Text = Math.Round(Convert.ToSingle(rd), 4)
                        Else
                            TextBox9.Text = Math.Round(Convert.ToSingle(TextBox9.Text), 4)
                        End If
                    ElseIf abe > 90 And aca < 90 Then
                        rd = Math.Round((Convert.ToSingle(Convert.ToSingle(TextBox3.Text) / 2) + Convert.ToSingle(Convert.ToSingle(TextBox12.Text) / 2) - Convert.ToSingle(Convert.ToSingle(b) * Convert.ToSingle(ga) / 100)), 4)
                        If Not rd = "NaN" Then
                            TextBox9.Text = Math.Round(Convert.ToSingle(rd), 4)
                        Else
                            TextBox9.Text = Math.Round(Convert.ToSingle(TextBox9.Text), 4)
                        End If
                    ElseIf abe < 90 And aca < 90 Then
                        rd = Math.Round(Convert.ToSingle(Convert.ToSingle(TextBox3.Text) / 2) - Convert.ToSingle(Convert.ToSingle(r) / 100 * Convert.ToSingle(al)), 4)
                        If Not rd = "NaN" Then
                            TextBox9.Text = Math.Round(Convert.ToSingle(rd), 4)
                        Else
                            TextBox9.Text = Math.Round(Convert.ToSingle(TextBox9.Text), 4)
                        End If

                    End If
                    'End If
                Else
                    TextBox9.Text = Math.Round(Convert.ToSingle(Convert.ToSingle(TextBox3.Text) / 2) + Convert.ToSingle(Convert.ToSingle(TextBox12.Text) / 2), 4)
                End If


                'chk = MessageTimeOut(sb, "pesan", 4)

                TextBox6.Text = Math.Round(Convert.ToSingle(al), 4) '/ 100
                Dim es As Single
                If Not sa = "NaN" Then
                    es = Math.Round(Convert.ToSingle(((Convert.ToSingle(r) * Math.Round(Convert.ToSingle(sa))) / 100)), 4)
                    'va = Val(b * sa)
                    'no = Val(a * sb)
                    If Not es = "NaN" Then

                        TextBox10.Text = Math.Round(Convert.ToSingle(es), 4)
                    Else
                        TextBox10.Text = Math.Round(Convert.ToSingle(TextBox10.Text), 4)

                    End If
                Else
                    'Dim vg As String
                    'Dim cal As Single
                    ' vg = 1 / 2
                    chk = MessageTimeOut(r / b & " And " & b / r & " And " & Label50.Text & " And " & Label51.Text, "AMS production", 4)
                    MsgBox(r / b & " And " & b / r)
                    Timer1.Stop()
                    Timer1.Enabled = False
                    Timer2.Stop()
                    Timer1.Enabled = False
                    'End
                    'MsgBox(sa)
                End If
                'If Not TextBox11.Text = "" Then
                ' If va > no Then
                ' TextBox11.Text = (va - no)

                'TextBox11.Text = Convert.ToSingle(Math.Sqrt((Val(TextBox10.Text) ^ 2 + Val(TextBox9.Text) ^ 2)))
                ' Else
                ' TextBox11.Text = (no - va)
                ' TextBox10.Text = (a * sb)
                ' End If
                'End If
                'Catch s As Exception
                ' MsgBox("Data stopped!")
                '  Exit Sub
                'End Try
                ''queri = "datasource=localhost;port=3307;username=root;password=password;database=runmeter;"
                '' conn = New MySqlConnection(queri)
                ' If conn.State = ConnectionState.Closed Then
                '' conn.Open()
                ''  cm = New MySqlCommand("INSERT INTO `runmonitor` (`Waktu`, `Jarak`) VALUES (current_timestamp(),'" & Label2.Text & "');", conn)

                ''cm.ExecuteNonQuery()
                'ListView1.Sorting = SortOrder.Ascending
                Label25.Text = Convert.ToSingle(Label25.Text) + 1
                ListView1.Items.Add(ListView1.Items.Count + 1)
                ListView1.Items(i).SubItems.Add(Date.Now.ToString("HH:mm:ss:fff"))
                Label37.Text = ListView1.Items(i).SubItems(1).Text
                If Not Math.Round(Convert.ToSingle(TextBox9.Text), 0) = 0 Then
                    If Not Math.Round(Convert.ToSingle(TextBox10.Text), 0) = 0 Then




                        ListView1.Items(i).SubItems.Add(Math.Round(Convert.ToSingle(TextBox9.Text), 2))
                        ListView1.Items(i).SubItems.Add(Math.Round(Convert.ToSingle(TextBox10.Text), 2))
                        'Label31.Text = Val(ListView1.Items(i).SubItems(3).Text) - Val(ListView1.Items(i - 1).SubItems(3).Text)
                        If ListView1.Items.Count > 1 Then
                            'va = Val(ListView1.Items(i - 1).SubItems(2).Text)
                            'no = Val(ListView1.Items(i - 1).SubItems(3).Text)
                            ' 
                            ' Label33.Text = Convert.ToSingle(TextBox9.Text) - Convert.ToSingle(ListView1.Items(i - 1).SubItems(2).Text)
                            ' Label34.Text = Convert.ToSingle(TextBox10.Text) - Convert.ToSingle(ListView1.Items(i - 1).SubItems(3).Text)

                            Label33.Text = Convert.ToSingle(Math.Round(Convert.ToSingle(TextBox9.Text), 0) - Math.Round(Convert.ToSingle(ListView1.Items(i - 1).SubItems(2).Text), 2))

                            If Math.Abs(Convert.ToSingle(Label33.Text)) < 2 Then
                                'Label33.Text = "0"
                                ListView1.Items(i).SubItems(2).Text = ListView1.Items(i - 1).SubItems(2).Text
                                If Not Val(TextBox9.Text) = 0 And Not Val(TextBox10.Text) = 0 Then
                                    Label33.Text = Convert.ToSingle(Math.Round(Convert.ToSingle(ListView1.Items(i).SubItems(2).Text), 2) - Math.Round(Convert.ToSingle(ListView1.Items(i - 1).SubItems(2).Text), 2))
                                Else
                                    Label33.Text = Label33.Text
                                End If
                            Else
                                Label33.Text = Label33.Text
                            End If
                            Label34.Text = Convert.ToSingle(Math.Round(Convert.ToSingle(TextBox10.Text), 0) - Math.Round(Convert.ToSingle(ListView1.Items(i - 1).SubItems(3).Text), 2))
                            If Math.Abs(Convert.ToSingle(Label34.Text)) < 2 Then
                                'Label34.Text = "0"
                                ListView1.Items(i).SubItems(3).Text = ListView1.Items(i - 1).SubItems(3).Text
                                If Not Val(TextBox9.Text) = 0 And Not Val(TextBox10.Text) = 0 Then
                                    Label34.Text = Convert.ToSingle(Math.Round(Convert.ToSingle(ListView1.Items(i - 1).SubItems(3).Text), 2) - Math.Round(Convert.ToSingle(ListView1.Items(i - 1).SubItems(3).Text), 2))
                                Else
                                    Label34.Text = Label34.Text
                                End If
                            Else
                                Label34.Text = Label34.Text
                            End If

                            mel = Math.Round(Math.Sqrt(Convert.ToSingle(Convert.ToSingle(Label33.Text) ^ 2) + Convert.ToSingle(Convert.ToSingle(Label34.Text) ^ 2)), 1)
                            'If Not Label25.Text = 0 Then
                            Label36.Text = Convert.ToSingle(mel) / Convert.ToSingle(Label25.Text)
                            'Else
                            'Label36.Text = 0
                            'End If
                            Label2.Text = Convert.ToSingle(Label33.Text)
                            Label4.Text = Convert.ToSingle(Label34.Text)
                            'mal = Val(TextBox10.Text) - no
                            Dim sel As Single
                            Dim sely As Single
                            sel = Math.Abs(Math.Round(Convert.ToSingle(ListView1.Items(i).SubItems(2).Text), 1) - Math.Round(Convert.ToSingle(ListView1.Items(i - 1).SubItems(2).Text), 1))
                            sely = Math.Abs(Math.Round(Convert.ToSingle(ListView1.Items(i).SubItems(3).Text), 1) - Math.Round(Convert.ToSingle(ListView1.Items(i - 1).SubItems(3).Text), 1))
                            'mengukur path
                            If Not sel >= 1 Then
                                Label35.Text = Math.Round(Convert.ToSingle(mel), 1)
                            Else
                                Label35.Text = 0
                                'mengukur jarak
                            End If

                            Label38.Text = Label35.Text

                            ListView1.Items(i).SubItems.Add(Math.Round(Convert.ToSingle(mel), 1))
                            Label44.Text = CInt(Math.Round(Convert.ToSingle(ListView1.Items(i).SubItems(4).Text), 1)) + CInt(Math.Round(Convert.ToSingle(Label44.Text), 1))
                            TextBox11.Text = Math.Round(Convert.ToSingle(Label44.Text), 1)
                            'Else
                            ListView1.Items(i).SubItems.Add(Convert.ToSingle(Label36.Text))

                            'ListView1.Items(ListView1.Items.Count - 1).EnsureVisible()

                            'ListView1.Items(i).SubItems.Add(si)
                        Else
                            ListView1.Items(i).SubItems.Add(0)
                            ListView1.Items(i).SubItems.Add(0)
                        End If
                        ListView1.Items(i).SubItems.Add(TextBox7.Text)
                        ListView1.Items(i).SubItems.Add(TextBox8.Text)


                        i = i + 1
                        Label26.Text = i
                        j = Val(j + Convert.ToSingle(TextBox9.Text))
                        h = Val(h + Convert.ToSingle(TextBox10.Text))
                        Label27.Text = Convert.ToSingle(j)
                        Label28.Text = Convert.ToSingle(j / ListView1.Items.Count)
                        Label29.Text = Convert.ToSingle(h)
                        Label30.Text = Convert.ToSingle(h / ListView1.Items.Count)
                        '

                        ' ha = ha + Convert.ToSingle(ListView1.Items(i).SubItems(4).Text)
                        'TextBox11.Text = ha

                        Chart2.Series(0).ChartType = DataVisualization.Charting.SeriesChartType.Point
                        'Chart2.ChartAreas("ChartArea1").AxisY.Minimum = 0
                        '.ChartAreas("ChartArea1").AxisX.Minimum = 0
                        'Chart2.ChartAreas("ChartArea1").AxisX.Maximum = 5 
                        ' Chart2.ChartAreas("ChartArea1").AxisY.Maximum = 10
                        Chart2.Series(0).Points.AddXY(Math.Round(Val(TextBox10.Text), 1), Math.Round(Val(TextBox9.Text), 1))

                        Chart3.Series(0).ChartType = DataVisualization.Charting.SeriesChartType.Spline
                        ' Chart3.ChartAreas("ChartArea1").AxisY.Minimum = 0
                        ' Chart3.ChartAreas("ChartArea1").AxisX.Minimum = 0
                        ' Chart3.ChartAreas("ChartArea1").AxisX.Maximum = Val(Label36.Text) + Val(Label36.Text / 10)
                        ' Chart3.ChartAreas("ChartArea1").AxisY.Maximum = Val(Label35.Text) + Val(Label35.Text / 10)
                        'Chart3.Series(0).Points.AddXY(1, i)
                        Chart3.Series(0).Points.AddXY(CStr(Label37.Text), Convert.ToSingle(Label35.Text))

                        ' Label4.Text = lin
                        ' Label7.Text = i
                        'If ListView1.Items.Count > 0 Then
                        'If Label2.Text Then
                        'End If
                        'Catch ex As Exception
                        'End Try

                        'End If
                        'End If


                        'SerialPort1.Close()
                        'SerialPort2.Close()
                        'End If
                        'End If

                    End If
                    ' Loop


                    'Label44.Text = CStr(Label44.Text) + CStr(ListView1.Items(i).SubItems(4).Text)
                    'End If
                    'Dim ka As String = CStr(SerialPort1.ReadExisting.ToString) 'SerialPort1.ReadLine.ToString)
                    ' wait(1)
                    'Label1.Text = ka
                    ' Dim be As String = 
                    'If SerialPort1.ReadExisting = True And SerialPort2.ReadExisting Then
                    'End If

                    'Catch ex As Exception

                    ' End Try
                    'End If
                Else
                    ' Return

                End If
            End If
            Label47.Text = CStr(Math.Round(Val(TextBox9.Text), 2))
            Label48.Text = CStr(Math.Round(Val(TextBox10.Text), 2))
        End If
        ' End If
    End Sub
    Function MessageTimeOut(ByVal sMessage As String, ByVal sTitle As String, ByVal iSeconds As Integer) As Boolean
        Dim Shell
        Shell = CreateObject("WScript.Shell")
        Shell.Run("mshta.exe vbscript:close(CreateObject(""WScript.shell"").Popup(""" & sMessage & """," & iSeconds & ",""" & sTitle & """))")
        MessageTimeOut = True
    End Function
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Button5.PerformClick()

        ' Call koneksi()
        RadioButton1.Checked = True
        Timer2.Enabled = False
        SerialPort1.BaudRate = 115200 'CmbBaud.SelectedItem
        SerialPort1.PortName = "COM15" 'CmbScanPort.SelectedItem
        SerialPort1.Close()

        SerialPort2.BaudRate = 115200 'CmbBaud.SelectedItem
        SerialPort2.PortName = "COM14" 'CmbScanPort.SelectedItem
        SerialPort2.Close()
        Dim vg As String
        Dim cal As Single
        vg = 1 / 2
        chk = MessageTimeOut("Welcome to Swivano Software", "AMS production", 4)
        cal = Math.Sin(0 * Math.PI / 180)
        chk = MessageTimeOut(Convert.ToSingle(cal), "AMS production", 4)
        'MsgBox(vg)
        If Char.IsNumber(TextBox4.Text) = False Then
            MsgBox("f")
        End If
        SerialPort1.Close()
        SerialPort2.Close()
        Timer1.Enabled = False
        Timer1.Stop()
        'rumus acos
        'TextBox4.Text = Math.Cos(60 * Math.PI / 180)

    End Sub
    Public Sub wait(ByVal Dt As Double)
        Dim IDay As Double = Date.Now.DayOfYear
        Dim CDay As Double
        Dim ITime As Double = Date.Now.TimeOfDay.TotalSeconds
        Dim CTime As Double
        Dim DiffDay As Double
        'Try
        Do
            Application.DoEvents()
            CDay = Date.Now.DayOfYear
            CTime = Date.Now.TimeOfDay.TotalSeconds
            DiffDay = CDay - IDay
            CTime = CTime + 86400 * DiffDay
            If CTime >= ITime + Dt Then Exit Do
        Loop
        'Catch e As Exception
        'End Try
    End Sub



    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        'wait(1000)
        If ListView2.Items.Count = 0 Then
            MsgBox("Peserta belum ada")
        Else
            If SerialPort1.IsOpen Or SerialPort2.IsOpen Then
                SerialPort1.Close()
                SerialPort2.Close()
            Else
                SerialPort1.BaudRate = 115200 'CmbBaud.SelectedItem
                SerialPort1.PortName = "COM15" 'CmbScanPort.SelectedItem
                SerialPort1.Open()

                SerialPort2.BaudRate = 115200 'CmbBaud.SelectedItem
                SerialPort2.PortName = "COM14" 'CmbScanPort.SelectedItem
                SerialPort2.Open()

                Timer1.Interval = 100
                Timer1.Enabled = True
                Timer1.Start()

                Timer2.Interval = 1000
                Timer2.Enabled = True
                Timer2.Start()
                Button1.Enabled = True
                Button2.Enabled = False

                Timer3.Interval = 1000
                Timer3.Enabled = True
                Timer3.Start()

            End If

        End If



    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        queri = "datasource=localhost;port=3307;username=root;password=password;database=runmeter;"
        conn = New MySqlConnection(queri)
        ' If conn.State = ConnectionState.Closed Then
        conn.Open()
        cm = New MySqlCommand("INSERT INTO `runmonitor` (`Kecepatan`) VALUES ('70');", conn)

        cm.ExecuteNonQuery()
        'MsgBox("ok")
        ' End If

    End Sub

    Private Sub ListView2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ListView2.SelectedIndexChanged

    End Sub

    Private Sub Chart1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Chart1.Click

    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click

    End Sub

    Private Sub Label3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub Button1_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Timer1.Stop()
        Timer1.Enabled = False
        Timer2.Stop()
        Timer2.Enabled = False
        Label7.Text = 0
        SerialPort1.Close()
        SerialPort2.Close()

        Button2.Enabled = True

    End Sub

    Private Sub Label9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label9.Click

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        TextBox1.Enabled = False
        TextBox2.Enabled = False

    End Sub

    Private Sub TextBox8_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox8.TextChanged

    End Sub

    Private Sub TextBox12_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox12.TextChanged

    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick

        Dim xa As Integer
        Dim ya As Integer
        Dim w As Integer
        w = ListView3.Items.Count
        Label7.Text = CInt(CInt(Label7.Text) + 1)
        If ListView1.Items.Count > 0 Then

            If Label7.Text > 2 Then
                ListView3.Items.Add(ListView3.Items.Count + 1)
                ListView3.Items(w).SubItems.Add(Date.Now.ToString("HH:mm:ss"))
                ListView3.Items(w).SubItems.Add(Convert.ToSingle(Label28.Text))
                ListView3.Items(w).SubItems.Add(Convert.ToSingle(Label30.Text))
                Chart1.Series(0).ChartType = DataVisualization.Charting.SeriesChartType.Point
                ' Chart1.ChartAreas("ChartArea1").AxisY.Minimum = 0
                'Chart1.ChartAreas("ChartArea1").AxisX.Minimum = 0
                ' Chart1.ChartAreas("ChartArea1").AxisX.Maximum = 2
                'Chart1.ChartAreas("ChartArea1").AxisY.Maximum = 2
                Chart1.Series(0).Points.AddXY(Convert.ToSingle(Label30.Text), Convert.ToSingle(Label28.Text))
                'ListView3.Items(w).SubItems.Add(ya)

                'ListView1.Items.Clear()
                'Label32.Text = Val(TextBox10.Text) - Val(ListView1.Items(i - 1).SubItems(4).Text)

                If ListView3.Items.Count > 1 Then
                    'va = Val(ListView1.Items(i - 1).SubItems(2).Text)
                    'no = Val(ListView1.Items(i - 1).SubItems(3).Text)
                    ' (
                    Label31.Text = Convert.ToSingle(Math.Round(Convert.ToSingle(Label28.Text), 4) - Math.Round(Convert.ToSingle(ListView3.Items(CInt(ListView3.Items.Count - 2)).SubItems(2).Text), 4))
                    Label32.Text = Convert.ToSingle(Math.Round(Convert.ToSingle(Label30.Text), 4) - Math.Round(Convert.ToSingle(ListView3.Items(CInt(ListView3.Items.Count - 2)).SubItems(3).Text), 4))
                    If Not Label31.Text = "NaN" And Not Label32.Text = "NaN" Then
                        'Label31.Text = Convert.ToSingle(Convert.ToSingle(Label28.Text) - Convert.ToSingle(ListView1.Items(i - 1).SubItems(2).Text))
                        If Math.Abs(Convert.ToSingle(Label31.Text)) < 1 Then
                            Label31.Text = Convert.ToSingle(0)
                            ListView3.Items(w).SubItems(2).Text = Convert.ToSingle(Math.Round(Convert.ToSingle(ListView3.Items(w - 1).SubItems(2).Text), 4))
                            'Label33.Text = Convert.ToSingle(Convert.ToSingle(ListView1.Items(i).SubItems(2).Text) - Convert.ToSingle(ListView1.Items(i - 1).SubItems(2).Text))
                        Else
                            Label31.Text = Convert.ToSingle(Label31.Text)
                        End If
                        'Label32.Text = Convert.ToSingle(Convert.ToSingle(Label30.Text) - Convert.ToSingle(ListView1.Items(w - 1).SubItems(3).Text))
                        If Math.Abs(Convert.ToSingle(Label32.Text)) < 1 Then
                            'Label34.Text = "0"
                            Label32.Text = Convert.ToSingle(0)
                            ListView3.Items(w).SubItems(3).Text = Convert.ToSingle(Math.Round(Convert.ToSingle(ListView3.Items(w - 1).SubItems(3).Text), 4))
                            'Label32.Text = Convert.ToSingle(Convert.ToSingle(ListView1.Items(i - 1).SubItems(3).Text) - Convert.ToSingle(ListView1.Items(i - 1).SubItems(3).Text))
                        Else
                            Label32.Text = Convert.ToSingle(Label32.Text)
                        End If
                    Else
                        Label31.Text = Label31.Text
                        Label32.Text = Label32.Text
                    End If
                    'mal = Val(TextBox10.Text) - no
                    swi = Math.Sqrt(((Label31.Text) ^ 2) + ((Label32.Text) ^ 2))
                    mal = swi / (Label7.Text)
                    ' ListView1.Items(i).SubItems.Add(swi)
                    'Else
                    'TextBox11.Text = CInt(TextBox11.Text) + CInt(swi)

                    If Not swi = "NaN" Then
                        ListView3.Items(w).SubItems.Add(Convert.ToSingle(swi))
                        'TextBox11.Text = CInt(TextBox11.Text) + CInt(swi)
                    Else
                        ListView3.Items(w).SubItems.Add(Convert.ToSingle(0))
                        'TextBox11.Text = CInt(TextBox11.Text)
                    End If
                    ListView3.Items(w).SubItems.Add(Convert.ToSingle(mal))
                    ListView3.Items(ListView3.Items.Count - 1).EnsureVisible()
                    'skb = swi-

                    'TextBox11.Text = Convert.ToSingle(ListView3.Items(0).SubItems(4).Text) + Convert.ToSingle(ListView3.Items(Convert.ToSingle(ListView3.Items.Count - 1)).SubItems(3).Text)
                Else
                    ListView3.Items(w).SubItems.Add(0)
                    ListView3.Items(w).SubItems.Add(0)
                End If
                w = w + 1
                Chart4.Series(0).ChartType = DataVisualization.Charting.SeriesChartType.Spline
                Chart4.Series(0).Points.AddXY(CStr(ListView3.Items(CInt(ListView3.Items.Count - 1)).SubItems(1).Text), Convert.ToSingle(swi))
            End If
        End If
        If RadioButton1.Checked = True Then
            If Label7.Text >= CInt(TextBox2.Text) Then
                Timer1.Stop()
                Timer2.Enabled = False
                Timer2.Stop()
                Timer1.Enabled = False
                SerialPort1.Close()
                SerialPort2.Close()

            End If
        Else
            If TextBox11.Text >= TextBox1.Text Then
                Timer1.Stop()
                Timer1.Enabled = False
                Timer2.Stop()
                Timer2.Enabled = False
                SerialPort1.Close()
                SerialPort2.Close()
            End If
        End If


    End Sub

    Private Sub ListView1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ListView1.SelectedIndexChanged

    End Sub

    Private Sub ListView3_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ListView3.SelectedIndexChanged

    End Sub

    Private Sub Label33_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label33.Click

    End Sub

    Private Sub Label35_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label35.Click

    End Sub

    Private Sub TextBox11_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox11.TextChanged

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        ListView1.Items.Clear()
        ListView3.Items.Clear()
        ListView2.Items.Clear()
        Button2.Enabled = True
        'SerialPort1.BaudRate = 115200 'CmbBaud.SelectedItem
        'SerialPort1.PortName = "COM15" 'CmbScanPort.SelectedItem
        SerialPort1.Close()

        'SerialPort2.BaudRate = 115200 'CmbBaud.SelectedItem
        'SerialPort2.PortName = "COM14" 'CmbScanPort.SelectedItem
        SerialPort2.Close()
        Chart1.Series(0).Points.Clear()
        Chart2.Series(0).Points.Clear()
        Chart3.Series(0).Points.Clear()
        Chart4.Series(0).Points.Clear()
        TextBox5.Text = ""
        TextBox6.Text = ""
        TextBox7.Text = 0
        TextBox8.Text = 0
        TextBox9.Text = 0
        TextBox10.Text = 0
        TextBox11.Text = 0

        Label2.Text = 0
        Label4.Text = 0
        Label25.Text = 0
        Label26.Text = 0
        Label27.Text = 0
        Label28.Text = 0
        Label29.Text = 0
        Label30.Text = 0
        Label31.Text = 0
        Label32.Text = 0
        Label33.Text = 0
        Label34.Text = 0
        Label35.Text = 0
        Label36.Text = 0
        Label37.Text = "00:00:00"
        Label38.Text = 0
        Label44.Text = 0
        Label47.Text = 0
        Label48.Text = 0
    End Sub

    Private Sub Label6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label6.Click

    End Sub

    Private Sub Chart4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Chart4.Click

    End Sub

    Private Sub Label2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label2.Click

    End Sub

    Private Sub Label4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label4.Click

    End Sub

    Private Sub Chart3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Chart3.Click

    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        ListView2.Items.Add(TextBox13.Text)
        ListView2.Items(ListView2.Items.Count - 1).SubItems.Add(TextBox14.Text)
        If RadioButton1.Checked = True Then
            ListView2.Items(ListView2.Items.Count - 1).SubItems.Add(RadioButton1.Text)
        Else
            ListView2.Items(ListView2.Items.Count - 1).SubItems.Add(RadioButton2.Text)
        End If


    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        TextBox13.Text = ""
        TextBox14.Text = ""

        'ComboBox1.Text = ""

    End Sub

    Private Sub RadioButton1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton1.CheckedChanged

    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        ' Dim baris As Integer
        'Try
        Dim SaveFile As New SaveFileDialog

        SaveFile.FileName = ""
        SaveFile.Filter = "Text Files (*.txt)|*.txt"
        SaveFile.Title = "Save"
        SaveFile.ShowDialog()
        Dim Write As New System.IO.StreamWriter(SaveFile.FileName)
        Dim col As ColumnHeader
        Dim columnnames As String = ""
        For Each col In ListView2.Columns
            If String.IsNullOrEmpty(columnnames) Then
                columnnames = col.Text
            Else
                columnnames &= "|" & col.Text
            End If
        Next
        Write.Write(columnnames & vbCrLf)
        For Me.baris = 1 To ListView2.Items.Count
            Write.Write(ListView2.Items(baris - 1).SubItems(0).Text & "|" & ListView2.Items(baris - 1).SubItems(1).Text & "|" & ListView2.Items(baris - 1).SubItems(2).Text & "|" & ListView2.Items(baris - 1).SubItems(3).Text & "|" & ListView2.Items(baris - 1).SubItems(4).Text & "|" & ListView2.Items(baris - 1).SubItems(5).Text & vbCrLf)
            'Next baris
        Next baris
        Write.Close()
        'Catch p As Exception
        ' Exit Sub
        'End Try
    End Sub

    Private Sub Label16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label16.Click

    End Sub

    Private Sub Label25_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label25.Click

    End Sub

    Private Sub Label26_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label26.Click

    End Sub

    Private Sub Label27_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label27.Click

    End Sub

    Private Sub Label28_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label28.Click

    End Sub

    Private Sub Label29_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label29.Click

    End Sub

    Private Sub Label30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label30.Click

    End Sub

    Private Sub Label31_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label31.Click

    End Sub

    Private Sub Label32_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label32.Click

    End Sub

    Private Sub Label34_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label34.Click

    End Sub

    Private Sub Label36_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label36.Click

    End Sub

    Private Sub Label37_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label37.Click

    End Sub

    Private Sub Label44_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label44.Click

    End Sub

    Private Sub Chart2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Chart2.Click

    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        Jarak_sensor.Show()


    End Sub

    Private Sub Label42_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label42.Click

    End Sub

    Private Sub RadioButton2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton2.CheckedChanged

    End Sub

    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick

    End Sub
End Class
