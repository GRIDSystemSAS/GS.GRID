unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, StdCtrls, ExtCtrls, PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    Memo1: TMemo;
    Memo2: TMemo;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    chkUseDC: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  VarPyth, jpeg,
  System.NetEncoding;

{$R *.dfm}

// Old code not compatible with Unicode
//
//function ImageToString(AGraphic : TGraphic) : String;
//var
//  _stream : TStringStream;
//begin
//  _stream := TStringStream.Create('');
//  try
//    AGraphic.SaveToStream(_stream);
//    Result := _stream.DataString;
//  finally
//    _stream.Free;
//  end;
//end;
//
//function BinStrToPyStr(const AString : String) : Variant;
//var
//  _str : PPyObject;
//begin
//  _str := GetPythonEngine.PyString_FromStringAndSize(PAnsiChar(AString), Length(AString)*SizeOf(Char));
//  Result := VarPythonCreate(_str);
//  GetPythonEngine.Py_DECREF(_str);
//end;

function ImageToPyStr(AGraphic : TGraphic) : Variant;
var
  _stream : TMemoryStream;
  _str : PPyObject;
begin
  _stream := TMemoryStream.Create();
  try
    AGraphic.SaveToStream(_stream);
    _str := GetPythonEngine.PyString_FromStringAndSize(_stream.Memory, _stream.Size);
    Result := VarPythonCreate(_str);
    GetPythonEngine.Py_DECREF(_str);
  finally
    _stream.Free;
  end;
end;

function PyToImage(AV : PPYObject) : TGraphic;
var
  _stream : TMemoryStream;
  _str : Variant;
begin
   _str := GetPythonEngine.PyObjectAsVariant(AV);
  _stream := TMemoryStream.Create();
   Result.SaveToStream(_stream);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;


Procedure StreamVariant( Var aVariant: Variant; aStream: TStream );
Var
  p: PByte;
  size: Integer;
Begin
  Assert( Assigned( aStream ));
  if VarArrayDimCount( aVariant )  <> 1 Then
    raise Exception.Create(
     'StreamVariant: passed variant does not contain a '+
     '1-dimensional array!' );
  p:= VarArrayLock( aVariant );
  If p <> Nil Then
  try
    size := (VarArrayHighBound( aVariant, 1 ) -
             VarArrayLowBound( aVariant, 1 ) + 1 ) *
            TVarData( aVariant ).VArray^.ElementSize;
    aStream.WriteBuffer( p^, size );
  finally
    VarArrayUnlock( aVariant );
  end;
End;

function GiveMeThatAnsiCharFromVariant( const v: Variant; var a: AnsiChar): Boolean;
// Accepts UnicodeString,AnsiString,WideString,byte for conversion to AnsiChar;
begin
  Result := False;
  if VarIsStr(v) then
  begin
    case VarType(v) of
    varString :
      if (Length(AnsiString(v)) = 1) then
      begin
        a := AnsiChar(AnsiString(v)[1]);
        Result := True;
      end;
    varUString :
      if (Length(String(v)) = 1) then
      begin
        a := AnsiChar(String(v)[1]);
        Result := True;
      end;
    varOleStr :
      if (Length(Widestring(v)) = 1) then
      begin
        a := AnsiChar(Widestring(v)[1]);
        Result := True;
      end;
    end;
  end
  else
  begin
    if (VarType(v) = varByte) then
    begin
      a := AnsiChar(Byte(v));
      Result := True;
    end;
  end;
end;

function VarToBytes(const Value: Variant): TBytes;
var
  I: Integer;
begin
  if not (VarIsArray(Value) and (VarArrayDimCount(Value) = 1) and
     ((VarType(Value) and VarTypeMask) = varByte)) then
    raise Exception.Create('bam');

  SetLength(Result, VarArrayHighBound(Value, 1) + 1);
  for I := 0 to VarArrayHighBound(Value, 1) do
    Result[I] := Value[I];
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  _im : Variant;
  _stream : TMemoryStream;
  _hdc : Variant;
  _dib : Variant;
  S,SS,s3 : AnsiString;
  SR : RawByteString;
  Sb : TBytes;
  s2,vp : variant;
  SafeArray: PVarArray;
  im2 : PPYObject;
  aaa : ansichar;
  ssb : TBytes;
begin
  if (Image1.Picture.Graphic = nil) or Image1.Picture.Graphic.Empty then
    raise Exception.Create('You must first select an image');
  PythonEngine1.ExecStrings(Memo1.Lines);
  _im := MainModule.ProcessImage(ImageToPyStr(Image1.Picture.Graphic));
  if not chkUseDC.Checked then
  begin
//     TODO :
//        Needs fixing.  Probably has to do with images containing null bytes which
//        cannot be converted to ansistrings.
//    ShowMessage('This does not work and needs fixing');


{
/////--------//
    _stream := TMemoryStream.Create();
    try
      s2 := MainModule.ImageToString(_im);
      s := PAnsiChar(AnsiString(s2));
      SetLength(s,93408);

      s3 := copy(s,1,length(ss)-1);
      _stream.WriteBuffer(pointer(S3)^, Length(S3));
      _stream.Position := 0;
      Image1.Picture.Graphic.LoadFromStream(_stream);
    finally
      _stream.Free;
    end;
}


   {
   //VG Attempt 1 : var types..
    _stream := TMemoryStream.Create();
    try
    s2 := MainModule.ImageToString(_im);
    SR := s2;
    _stream.SetSize(Length(SR));


    Move(SR,_stream.Memory^, _stream.Size);
    _stream.Position := 0;
    Image1.Picture.Graphic.LoadFromStream(_stream);
    finally

    end;
}

   //VG Attempt2 : base64 :/
   _stream := TMemoryStream.Create();
   try
     s := MainModule.ImageToString(_im);
     sb := TNetEncoding.Base64.DecodeStringToBytes(s);
     _stream.WriteBuffer(sb[0], length(sb));
     _stream.Position := 0;
     Image1.Picture.Graphic.LoadFromStream(_stream);
   finally
     FreeAndNil(_Stream);
   end;

  end
  else
  begin
    Image1.Picture.Bitmap.Width := Image1.Width;
    Image1.Picture.Bitmap.Height := Image1.Height;
    _hdc := Import('ImageWin').HDC(Image1.Picture.Bitmap.Canvas.Handle);
    _dib := Import('ImageWin').Dib(_im);
    _dib.expose(_hdc);
  end;
end;

end.
