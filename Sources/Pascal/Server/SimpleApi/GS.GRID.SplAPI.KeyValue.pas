unit GS.GRID.SplAPI.KeyValue;
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

 TGRIDSimpleAPIKeyValue = Class(TGRIDSimpleAPI)
 public

 End;


implementation

end.
