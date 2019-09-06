object Form1: TForm1
  Left = 218
  Top = 18
  Caption = 'Form1'
  ClientHeight = 745
  ClientWidth = 674
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 657
    Height = 273
  end
  object Button1: TButton
    Left = 9
    Top = 288
    Width = 97
    Height = 25
    Caption = 'Open Picture...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 197
    Top = 288
    Width = 105
    Height = 25
    Caption = 'Execute'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 9
    Top = 423
    Width = 657
    Height = 281
    Lines.Strings = (
      'import io'
      'import PIL'
      'from PIL import Image'
      'import base64'
      ''
      'def ProcessImage(data):'
      '  stream = io.BytesIO(data)'
      '  im = Image.open(stream)'
      
        '  print ("Processing image %s of %d bytes" % (im.format, len(dat' +
        'a)))'
      '  new_im = im.rotate(15)'
      '  new_im.format = im.format'
      '  return new_im'
      '  '
      'def ImageToString(image):'
      
        '  """ Not woking : try VarArray delphi side. But return python l' +
        'ist object...'
      '  st = io.BytesIO()'
      '  image.save(st, image.format)'
      '  st.seek(0)'
      '  return [st.getvalue()];'
      '  """'
      
        '  """ that is working... but encoding base64 is not optimized. "' +
        '""'
      '  st = io.BytesIO()'
      '  image.save(st, image.format)'
      '  st.seek(0)'
      '  return base64.b64encode(st.getvalue());'
      '  #return st.getvalue()')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Memo2: TMemo
    Left = 9
    Top = 328
    Width = 657
    Height = 113
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object chkUseDC: TCheckBox
    Left = 337
    Top = 292
    Width = 193
    Height = 17
    Caption = 'Use Device Context'
    TabOrder = 4
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 105
    Top = 272
  end
  object PythonEngine1: TPythonEngine
    AutoUnload = False
    DllName = 'python36.dll'
    DllPath = 'C:\Users\Vincent\AppData\Local\Programs\Python\Python36-32\'
    APIVersion = 1013
    RegVersion = '3.4'
    UseLastKnownVersion = False
    IO = PythonGUIInputOutput1
    Left = 193
    Top = 512
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 441
    Top = 504
  end
end
