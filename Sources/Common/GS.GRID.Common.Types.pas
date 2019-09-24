unit GS.GRID.Common.Types;

//
///
///  *** LICENSE ****
///  Version: MPL 2
///
///  The contents of this file are subject to the Mozilla Public License Version
///  1.1 (the "License"); you may not use this file except in compliance with
///  the License. You may obtain a copy of the License at
///  http://www.mozilla.org/MPL
///
///  Software distributed under the License is distributed on an "AS IS" basis,
///  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
///  for the specific language governing rights and limitations under the License.
///
///  Portions created by the Initial Developer are Copyright (C) 2019
///  the Initial Developer. All Rights Reserved.
///
///  This license applies only to files bearing this header and does not apply
///  to files under "third party" directories (which are the creation of other
///  authors and may be under other licenses) or if file has not,
///  or has another, license header.
///  *** LICENSE ****
//

//
///
/// Promoted and sponsored by GRID SYSTEM S.A.S, France
/// VincentOnGitHub (at) grids.systems
///
//

{$I GSCore.inc}

interface

Uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 {$IFDEF USE_GENERIC}
 Generics.Collections,
 {$ENDIF}
 SyncObjs,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 {$IFDEF USE_GENERIC}
 System.Generics.Collections,
 {$ENDIF}
 System.SyncObjs,
 {$ENDIF}
 GS.Common,
 GS.Bus,
 GS.Bus.Services,
 GS.Stream,
 GS.CPUUsage;


Const
  CST_CHANNELNAME_LOGROOT                 = '.GRIDServer.System.Log';
  CST_CHANNELNAME_SEPARATOR               = '.';


Type

//------------------------------------------------------------------------------
// Log format message
//------------------------------------------------------------------------------
  TGridLogCategory = (glcInfo, glcWarning, glcException, glcFatal);
  TGridLogCategories = set of TGridLogCategory;
  TGRIDLogChunk = Packed Record
    DateTime : TDateTime;
    ThreadID : UInt64;
    LogText : String;
    LoggerClassName : String;
    Category : TGridLogCategory;
    Module : String;

    Procedure Serialize(var aStream : TStream);
    Procedure Deserialize(FromStream : TStream);
  end;



implementation

{ TGRIDLogChunk }

procedure TGRIDLogChunk.Deserialize(FromStream: TStream);
begin
  DateTime := ReadDateTime(FromStream);
  ThreadID := ReadUInt64(FromStream);
  LogText := ReadString(FromStream);
  LoggerClassName := ReadString(FromStream);
  Module := ReadString(FromStream);
  Category := TGridLogCategory(ReadByte(FromStream));
end;

procedure TGRIDLogChunk.Serialize(var aStream: TStream);
begin
  WriteDateTime(aStream,DateTime);
  WriteUInt64(aStream,ThreadID);
  WriteString(aStream,LogText);
  WriteString(aStream,LoggerClassName);
  WriteString(aStream,Module);
  WriteByte(aStream,Byte(Category));
end;


end.
