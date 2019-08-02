{
This MQTT Implementation rely on pjde nice work. Due to GRID architecture,
we used only the Parser : all the connection and server stuff is GRID one.
}
unit GS.GRID.Common.Protocols.MQTT;

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

interface

{$I GSCore.inc}

uses SysUtils,
     Classes,
     GS.Stream,
     GS.GRID.Common.Protocols,
     uMQTTex {https://github.com/pjde/ultibo-mqtt};

Type

TGRIDProtocol_MQTT = class(TGRIDProtocol)
protected
  FParser : TMQTTParser;
public
  constructor Create; override;
  destructor Destroy; override;
  //Needed for inheritance.
  class function Negociate(aStream : TStream) : Boolean; Override;

  function AllowByteCountPrefix : Boolean; override;
  function AllowEmptyInputData : boolean; override;


  function ProtocolName : String; Override;
  function ProtocolNativeFormat : TGRIDProtocolFormat; Override;
  function ProtocolDescription : String; Override;

  //Parser objet handle all side of the protocol : Client and Server/Broker.
  property Parser : TMQTTParser read FParser;
end;

Implementation


{ TGRIDProtocol_MQTT }

function TGRIDProtocol_MQTT.AllowByteCountPrefix: Boolean;
begin
  result := false; //MQTT not indicate the stream size on header.
end;

function TGRIDProtocol_MQTT.AllowEmptyInputData: boolean;
begin
  //We need to check, in the server "client" thread, if there are no data
  //available, without "real" client ping. This parameter let the server loop
  //check client state without incoming data event.
  result := true;
end;

constructor TGRIDProtocol_MQTT.Create;
begin
  Inherited Create;
  FParser := TMQTTParser.Create;
end;

destructor TGRIDProtocol_MQTT.Destroy;
begin
  FreeAndNil(FParser);
  inherited;
end;

class function TGRIDProtocol_MQTT.Negociate(aStream: TStream): Boolean;
var FNegociateParser : TMQTTParser;
begin
  result := false;
  if aStream.Size>0 then
  begin
    FNegociateParser := TMQTTParser.Create;
    try
      try
        FNegociateParser.Parse(aStream);
        result := FNegociateParser.RxMsg = TMQTTMessageType.mtCONNECT;
      Except
        raise EExceptionProtocolNegotiation.Create(ClassName + ' - Protocol negociation failure');
      end;
    finally
      FreeAndNil(FNegociateParser);
    end;
  end;
end;

function TGRIDProtocol_MQTT.ProtocolDescription: String;
begin
  result := 'MQTT 3.11 compliant <beta>';
end;

function TGRIDProtocol_MQTT.ProtocolName: String;
begin
  result := 'MQTT';
end;

function TGRIDProtocol_MQTT.ProtocolNativeFormat: TGRIDProtocolFormat;
begin
  result := TGRIDProtocolFormat.Binary
end;

Initialization

GridProtocolManager.Add(TGRIDProtocol_MQTT);

end.
