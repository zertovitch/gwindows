VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "VBtoAda"
   ClientHeight    =   1395
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   1395
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton VBtoAdaBtn 
      Caption         =   "Send"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   960
      Width           =   1095
   End
   Begin VB.TextBox VBtoAdaTxt 
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   480
      Width           =   4335
   End
   Begin VB.Label Label1 
      Caption         =   "Text to send to Ada program"
      Height          =   255
      Left            =   120
      TabIndex        =   1
      Top             =   120
      Width           =   2535
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim AdaCode As VBtoAdaClass

Private Sub Form_Load()
    Set AdaCode = New VBtoAdaClass
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Set AdaCode = Nothing
End Sub

Private Sub VBtoAdaBtn_Click()
    AdaCode.Display VBtoAdaTxt.Text
End Sub
