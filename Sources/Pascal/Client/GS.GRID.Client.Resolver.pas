unit GS.GRID.Client.Resolver;
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

Uses
   SysUtils, Classes,
   GS.Common,
   IdSocketHandle, IdGlobal,
   IdComponent, IdBaseComponent,
   IdUDPBase, IdUDPClient, IdStack;

Const
  CST_SSDP_ROOTDEVICE     = 'upnp:rootdevice';
  CST_SSDP_HTTPOK         = 'HTTP/1.1 200 OK';

type
TGridClientResolver = Class
Public
  Procedure Resolve; Virtual; Abstract;
End;

TGridClientDevice = Record
   DevType, ServerName, UNS, Location : String
End;

TGridClientSSDPLikeResolver = Class(TGridClientResolver)
  private
    function GetItem(Index: UInt32): TGridClientDevice;
    procedure SetItem(Index: UInt32; const Value: TGridClientDevice);
protected
  FArray : Array of TGridClientDevice;
  Function TextStartsWith(S,SubS : String) : Boolean;

Public
  Procedure Add(aType, aServer, aUNS, aLocation : String);
  Function DeviceCount : Uint32;
  Procedure Resolve; Override;
  Property Items[Index : UInt32] : TGridClientDevice read GetItem Write SetItem; Default;
End;

TGridServerInstanceItem = Class
public
  GridServerIP : String;
  GridServerPort : String;
  GridServerName : String;
  GridServerDesc : String;
  GridServerHost : String;
End;

TGridServerInstanceItems = Class(TList_ObjectArray)
  private
    function GetServer(Index: UInt32): TGridServerInstanceItem;
    procedure SetServer(Index: UInt32; const Value: TGridServerInstanceItem);
Public
  Procedure Add(aItem : TGridServerInstanceItem);
  Property Items[Index : UInt32] : TGridServerInstanceItem read GetServer Write SetServer; Default;
End;

//Perform complete resolution, with named data.
TGridClientGRIDResolver = Class(TGridClientSSDPLikeResolver)
Private
  FInternalList : TGridServerInstanceItems;

  function GetServer(Index: UINT32): TGridServerInstanceItem;
  function GetServerCount: UINT32;
Public
  Constructor Create; virtual;
  Destructor Destroy; Override;
  Procedure Resolve; Override;

  Property GRIDServers[Index : UINT32] : TGridServerInstanceItem read GetServer;
  Property GRIDServerCount : UINT32 read GetServerCount;
End;

implementation

{ TGridClientSSDPLikeResolver }
procedure TGridClientSSDPLikeResolver.Add(aType, aServer, aUNS,
  aLocation: String);
var l : UInt32;
begin
  l := Length(Farray);
  SetLEngth(FArray,l+1);
  FArray[l].DevType := aType;
  FArray[l].ServerName := aServer;
  FArray[l].UNS := aUNS;
  FArray[l].Location := aLocation;
end;

function TGridClientSSDPLikeResolver.DeviceCount: Uint32;
begin
  result := Length(FArray);
end;

function TGridClientSSDPLikeResolver.GetItem(Index: UInt32): TGridClientDevice;
begin
  result := TGridClientDevice(FArray[Index]);
end;

procedure TGridClientSSDPLikeResolver.Resolve;
var
    i : integer;
    s,obb : integer;
    strAn : TStringList;

    anServerName : String;
    anUID : String;
    anType : String;
    anLocation : String;
    lStr : String;
    lCli : TIdUDPClient;

    Function ExtractValue(FromString : String) : String;
    var p : Integer;
    begin
      Result := EmptyStr;
      p := Pos(':',FromString);
      if p>0 then
        Result := trim(Copy(FromString,p+1,Length(FromString)-p));
    end;

    Procedure Eval;
    begin
      if (anType <> '') And (anserverName <>'') And (anUID<>'') and (anLocation<>'') then
      begin
        //if (Length(anUID)>0) then
        begin
          //We should have all the info here.
          if anType = CST_SSDP_ROOTDEVICE then
          begin

            Add(anType,anServerName,anUID,anLocation);
{
            if Not(Data.IsDeviceExistsByUID(anUID,obb)) then
            begin
              //log(' BuildSnapShotDataModel : New root device detected : '+anServerName);

              aDevice := TGRIDSSDPServerSideDevice.Create(FDataM);
              aDevice.ServerName := anServerName;
              aDevice.UNS := anUID;
              aDevice.Location := anLocation;
              Data.AddDeviceList(aDevice);

            end
            else
            begin
              //log(' BuildSnapShotDataModel : Root device "'+anServerName+'" exists yet.');
            end;
}
          end
          else
          begin
            //TODO OTHER TYPE. (SERVICE...)
          end;
        end;

        anServerName := EmptyStr;
        anUID := EmptyStr;
        anType := EmptyStr;
        anLocation := EmptyStr;
      end;
    end;

    Function ReadAsString : String;
    var rb : TidBytes;
        s : Integer;
    begin
      Result := EmptyStr;

      while True do
      begin
        setlength(rb,lCli.BufferSize);
        s := lCli.ReceiveBuffer(rb,1000);
        if s = 0  then
          Break;
        setlength(rb,s);
        Result := Result + BytesToString(rb);
      end;
    end;

begin
  //Log('BuildSnapShotDataModel : Broadcasting M-SEARCH...');
  //Note / should be sent if we had a "real" server listen en 239.255.255.250:1900.
  //But currently, our server listen on other interface on port 1900.
  //This is mainly for 2 reason :
  //- Our SSDP server is just four our usage, it is not open.
  //- We do not want to do the job of the OS.
  lCli := TIdUDPClient.Create(nil);
  try
    lCli.ReceiveTimeout := 6000;
    lCli.BroadcastEnabled := True;
    lStr := 'M-SEARCH * HTTP/1.1' + #13#10 +
                      'HOST: 239.255.255.250:1900' + #13#10 +
                      'MAN: "ssdp:discover"'+ #13#10 +
                      'MX: 3'+ #13#10 +
                       // 'ST: urn:dial-multiscreen-org:service:dial:1'+ #13#10 +
                       // 'ST: urn:schemas-udap:service:smartText:1'+ #13#10 +
                       // 'ST: udap:all'+ #13#10 +
                       // 'ST: urn:all'+ #13#10 +
                      'ST: ssdp:all'+ #13#10;
    lCli.Broadcast(lStr,1900);
    lStr := ReadAsString;
  Finally
    FreeAndNil(lCli);
  end;

  strAn := TStringList.Create;
  try
    stran.Text := lstr;
    //Log('BuildSnapShotDataModel : Analysing data...');

   anServerName := EmptyStr;
   anUID := EmptyStr;
   anType := EmptyStr;
   anLocation := EmptyStr;

    for i := 0 to strAn.Count-1 do
    begin
      if TextStartsWith(lowercase(strAn[i]),'st:') then
      begin
        anType := ExtractValue(strAn[i]);
      end
      else
      if TextStartsWith(lowercase(strAn[i]),'location') then
      begin
        anLocation := ExtractValue(strAn[i]);
      end
      else
      if TextStartsWith(lowercase(strAn[i]),'usn') then
      begin
        anUID := ExtractValue(strAn[i]);
      end
      else
      if TextStartsWith(lowercase(strAn[i]),'server') then
      begin
        anServerName := ExtractValue(strAn[i]);
      end;

      Eval;
    end;
  //Log('BuildSnapShotDataModel : Done.');
  finally
    FreeAndNil(strAn);
  end;
end;

procedure TGridClientSSDPLikeResolver.SetItem(Index: UInt32;
  const Value: TGridClientDevice);
begin
  FArray[Index] := Value;
end;

function TGridClientSSDPLikeResolver.TextStartsWith(S,SubS : String): Boolean;
var LLen : Integer;
begin
  LLen := Length(SubS);
  Result := LLen <= Length(S);
  if Result then
    Result := System.String.Compare(S, 0, SubS, 0, LLen, True) = 0;
end;

{ TGridClientGRIDResolver }

constructor TGridClientGRIDResolver.Create;
begin
  inherited;
  FInternalList := TGridServerInstanceItems.Create(True);
end;

destructor TGridClientGRIDResolver.Destroy;
begin
  FreeAndNil(FInternalList); //Owned ObjectList
  inherited;
end;

function TGridClientGRIDResolver.GetServer(
  Index: UINT32): TGridServerInstanceItem;
begin
  result := FInternalList[Index];
end;

function TGridClientGRIDResolver.GetServerCount: UINT32;
begin
  Result := FInternalList.Count;
end;

procedure TGridClientGRIDResolver.Resolve;
Const CST_UNABLE_TO_RETRIEVE = 'Unknown';
var a : TGridClientSSDPLikeResolver;
    i : integer;
    temp, ip,port : string;
    aItem : TGridServerInstanceItem;
begin
  FInternalList.Clear;
  a := TGridClientSSDPLikeResolver.Create;
  try
    a.Resolve;

    for I := 0 to a.DeviceCount-1 do
    begin
      temp :=a[i].Location;
      if pos('grid',temp)>0 then
      begin
        temp := lowercase(temp);
        temp := trim(Stringreplace(temp,'grid://','',[]));
        port := Copy(temp,Pos(':',temp)+1,length(temp)-Pos(':',temp));
        Ip := Copy(temp,1,Pos(':',temp)-1);

        aItem := TGridServerInstanceItem.Create;
        aItem.GridServerIP := Ip;
        aItem.GridServerPort := port;
        aItem.GridServerName := CST_UNABLE_TO_RETRIEVE;
        aItem.GridServerDesc := CST_UNABLE_TO_RETRIEVE;
        aItem.GridServerHost := CST_UNABLE_TO_RETRIEVE;
{
        try

          c.Connect(IP,StrToInt(Port));
          c.ServerDefinition(cc);
          c.Disconnect;
          if cc.GetFirstEntryByClassName(TSOGRIDCentralDataModel,TObject(ccc)) then
          begin
            aItem.GridServerName := ccc.GRIDServerName;
            aItem.GridServerDesc := ccc.GRIDServerDesc;
            aItem.GridServerHost := ccc.GRIDServerHost;
          end;
          cc.StoredObject.ClearAllData(True);
        Except
          //Silent exception. Resolution should not be fatal if one of thos server is not up-to-date with the client protocol.
        end;
}
        FInternalList.Add(aItem);
       end;
    end;
  finally
    FreeAndNil(a);
  end;
end;

{ TGridServerInstanceItems }

procedure TGridServerInstanceItems.Add(aItem: TGridServerInstanceItem);
begin
  ManagedAdd(aItem);
end;

function TGridServerInstanceItems.GetServer(
  Index: UInt32): TGridServerInstanceItem;
begin
  result := TGridServerInstanceItem(FArray[Index]);
end;

procedure TGridServerInstanceItems.SetServer(Index: UInt32;
  const Value: TGridServerInstanceItem);
begin
  ManagedSet(Index,Value);
end;

end.
