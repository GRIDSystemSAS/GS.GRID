object flogview: Tflogview
  Left = 0
  Top = 0
  Caption = 'flogview'
  ClientHeight = 656
  ClientWidth = 907
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    907
    656)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 32
    Top = 0
    Width = 65
    Height = 65
    Brush.Color = clMoneyGreen
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 891
    Height = 609
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
