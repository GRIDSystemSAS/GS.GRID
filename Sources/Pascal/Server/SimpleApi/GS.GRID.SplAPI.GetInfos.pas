unit GS.GRID.SplAPI.GetInfos;
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
 SysUtils,
 {$ELSE}
 System.SysUtils,
 {$ENDIF}
 GS.Bus,
 GS.GRID.Common.Protocols.KissB,
 GS.GRID.Server.Service.Server.BasedProtocols,
 GS.GRID.Server.Service.Types,
 GS.GRID.SplAPI.Base;

type

 TGRIDSimpleAPIGetInfos = Class(TGRIDSimpleAPI)
 public
   Constructor Create(const Server: TGRIDServiceServerBasedProtocol); Reintroduce;

   Function GetInfos : TGRIDProtocol_KB_SRV_PROCESS_API_INFO;
   Function CPUUsagePercent : Double;
 End;


implementation

function TGRIDSimpleAPIGetInfos.CPUUsagePercent: Double;
var l : TBusClientDataRepo;
begin
  l := TBusClientDataRepo.Create(FServer.MasterThread.Bus,CST_BUSDATAREPO_SERVERINFO);
  try
    l.GetValue('GS.GRID.CPU.UsagePercent',result);
  finally
    FreeAndNil(l);
  end;
end;

constructor TGRIDSimpleAPIGetInfos.Create(
  const Server: TGRIDServiceServerBasedProtocol);
begin
  Inherited Create(Server,nil,nil,nil,nil);
end;

Function TGRIDSimpleAPIGetInfos.GetInfos : TGRIDProtocol_KB_SRV_PROCESS_API_INFO;
var l : TBusClientDataRepo;
begin
  result.ServerGenuineName := '';
  result.ServerHostCPUArchitecture := '';
  result.GRIDVersion := '';
  result.GRIDServerName := '';
  result.GRIDServices := '';
  result.GRIDArch := '';
  result.GRIDCompiler := '';

  //As private data repo, we use masterthread bus, because it is very much less used than gridbus.
  if FServer.MasterThread.Bus.IsDataRepositoryExists(CST_BUSDATAREPO_SERVERINFO) then
  begin
    l := TBusClientDataRepo.Create(FServer.MasterThread.Bus,CST_BUSDATAREPO_SERVERINFO);
    try
      l.GetValue('GRIDServerName',result.GridServerName);
      l.GetValue('OSAndArchAsString',result.ServerHostCPUArchitecture);
      l.GetValue('OSMajorMinorBuild',result.ServerHostOSBuild);
      l.GetValue('OSArchitecture',result.ServerHostArchitecture);
      l.GetValue('OSName',result.ServerHostOS);
      l.GetValue('OSGenuineName',result.ServerGenuineName);
      l.GetValue('GRIDArch',result.GRIDArch);
      l.GetValue('GRIDCompiler',result.GRIDCompiler);
    finally
      FreeAndNil(l);
    end;
  end;
end;

end.
