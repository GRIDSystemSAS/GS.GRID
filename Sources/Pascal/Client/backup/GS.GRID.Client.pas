unit GS.GRID.Client;

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
     GS.GRID.Client.Transport;

const
  CST_NOT_CONNECTED = 'client not connected.';

Type
TGRIDMessage = Packed record  //Only for response.
  From : String;
  Channel : String;
  PayLoad : TBytes;
  Ticks : Uint64;
  function PayloadAsString : String;
end;
TGRIDMessages = Array of TGRIDMessage;

TGRIDClient = class
private
protected
  FProtocol : TGRIDProtocol;
  FTransport : TGRIDTransport;
  function GetConnected: Boolean;
public
  Procedure Disconnect; Virtual;

  Property Protocol : TGRIDProtocol read FProtocol;
  Property Transport : TGRIDTransport read FTransport;

  Property Connected : Boolean read GetConnected;
end;


implementation

{ TGSClient }
procedure TGRIDClient.Disconnect;
begin
  Assert(assigned(Transport));
  FTransport.Disconnect;
end;

function TGRIDClient.GetConnected: Boolean;
begin
  Assert(assigned(Transport));
  result := FTransport.connected;
end;

{ TGRIDMessage }

function TGRIDMessage.PayloadAsString: String;
var i : integer;
begin
  result := '';
  for I := 0 to Length(PayLoad)-1 do
  begin
    result := result + InTtoStr(PayLoad[i])+' ';
  end;
end;

end.
