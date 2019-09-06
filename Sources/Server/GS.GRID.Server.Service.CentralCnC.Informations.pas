unit GS.GRID.Server.Service.CentralCnC.Informations;
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

uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 SyncObjs,
 {$ELSE}
 System.Classes,
 System.SysUtils,
 System.SyncObjs,
 {$ENDIF}
 GS.Common,
 GS.CPUUsage,
 GS.GRID.Server.Service.Types,
 GS.GRID.Server.Service.Server;

const
{$ifdef fpc}
  CST_Platform : Array[0..5] of string = ('Windows','Mac OS','iOS','Android','WinRT','Linux');
  CST_Archi : Array[0..3] of string = ('X86','X64','Arm32','Arm64');
{$else}
  CST_Platform : Array of string = ['Windows','Mac OS','iOS','Android','WinRT','Linux'];
  CST_Archi : Array of string = ['X86','X64','Arm32','Arm64'];
{$endif}

//Compiler and compiled version.

{$IFDEF DCC}

  {$IFDEF WIN32}
  CST_Archi_ServerCompiler = 'DCC32';
  {$ELSE}
    {$IFDEF WIN64}
      CST_Archi_ServerCompiler = 'DCC64';
    {$ELSE}
      {$IFDEF LINUX64}
        CST_Archi_ServerCompiler = 'DCCLINUX64';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

{$ELSE}

  {$IFDEF FPC}
    CST_Archi_ServerCompiler = 'FPC'; //+IntToStr(FPC_FULLVERSION);
  {$ENDIF}
{$ENDIF}

//Compiled...
{$IFDEF WIN32}
CST_Archi_ServerCompiled = '32 bits';
{$ELSE}
  {$IFDEF WIN64}
    CST_Archi_ServerCompiled = '64 bits';
  {$ELSE}
    {$IFDEF LINUX}
      {$IFDEF CPU64}
        CST_Archi_ServerCompiled = '64 bits';
      {$ELSE}
        CST_Archi_ServerCompiled = '32 bits';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


Type
TGRIDCentralCNCInformation = class
protected
  FServerName : String;
public
  Function GetOSAndArchAsString : String;
  Function GetOSMajorMinorBuild : String;
  Function GetOSArchitecture : String;
  Function GetOSName : String;
  Function GetOSGenuineName : String;
  Function GetServerArch : String;
  function GetCompiler : String;

  Function GetGridServerName : String;
  Procedure SetGridServerName(const aValue : String);
end;

implementation

{ TGRIDCentralCNCInformation }

function TGRIDCentralCNCInformation.GetCompiler: String;
begin
  result := CST_Archi_ServerCompiler;
end;

function TGRIDCentralCNCInformation.GetGridServerName: String;
begin
  result := FServerName;
  if trim(result) = '' then
    result := 'Unamed Grid Server';
end;

function TGRIDCentralCNCInformation.GetOSAndArchAsString: String;
begin
  Result :=  GetOSGenuineName + ' ('+GetOSMajorMinorBuild+') - '+ GetOSName +' - '+ GetOSArchitecture;
end;

function TGRIDCentralCNCInformation.GetOSArchitecture: String;
begin
  {$IFDEF FPC}
    {$IFDEF CPUARM}
    Result := CST_Archi[2];
    {$ELSE}
    Result := CST_Archi[0];
    {$ENDIF}
  {$ELSE}  Result := CST_Archi[integer(TOSVersion.Architecture)]
{$ENDIF}
end;

function TGRIDCentralCNCInformation.GetOSGenuineName: String;
begin
  {$IFDEF FPC}
    {$IFDEF LINUX}
    Result := CST_Platform[5];
    {$ELSE}
    Result := CST_Platform[0];
    {$ENDIF}
  {$ELSE}
  Result := TOSVersion.Name;
  {$ENDIF}
end;

function TGRIDCentralCNCInformation.GetOSMajorMinorBuild: String;
begin
{$IFDEF FPC}
  Result := '0.0'; //?
{$ELSE}
  Result := IntToStr(TOSVersion.Major)+'.'+IntToStr(TOSVersion.Minor)+'.'+IntToStr(TOSVersion.Build);
{$ENDIF}
end;

function TGRIDCentralCNCInformation.GetOSName: String;
begin
  {$IFDEF FPC}
    {$IFDEF LINUX}
    Result := CST_Platform[5];
    {$ELSE}
    Result := CST_Platform[0];
    {$ENDIF}
  {$ELSE}
  Result := CST_Platform[Integer(TOSVersion.Platform)]
{$ENDIF}
end;

function TGRIDCentralCNCInformation.GetServerArch: String;
begin
  result := CST_Archi_ServerCompiled;
end;

procedure TGRIDCentralCNCInformation.SetGridServerName(const aValue: String);
begin
  FServerName := trim(aValue);
end;

end.
