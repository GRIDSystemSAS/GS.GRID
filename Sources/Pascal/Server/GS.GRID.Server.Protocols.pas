unit GS.GRID.Server.Protocols;
interface

{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
{$ENDIF}

uses SysUtils,
     Classes,
     Generics.Collections,
     GS.GRID.Common.Protocols;

Type

TGRIDServerProtocol = class(TGRIDProtocol)
protected
public
  //When a new connection occurs, server begin to negociate with a non empty incoming stream.
  //this negociation constits of try to read signature, and compare with a one GPCommand.
  //For example, this GP command could be a connection negotiation.
  class function Negotiate(aStream : TStream) : Boolean; virtual;

  function NegotiateCommand : TGPCommand; virtual; abstract;
end;
TGRIDServerProtocolClass = class of TGRIDServerProtocol;

{ TGridServerProtocolManager }
TGridServerProtocolManager = Class
protected
  FProtocolList : TList<TGRIDServerProtocolClass>;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;
  property Protocols : TList<TGRIDServerProtocolClass> read FProtocolList;
End;

Var
  GridServerProtocolManager : TGridServerProtocolManager;


implementation


{ TGRIDServerProtocol }

class function TGRIDServerProtocol.Negotiate(aStream: TStream): Boolean;
begin
  result := false;
end;


{ TGridServerProtocolManager }

constructor TGridServerProtocolManager.Create;
begin
  FProtocolList := TList<TGRIDServerProtocolClass>.Create;
end;

destructor TGridServerProtocolManager.Destroy;
begin
  FreeAndNil(FProtocolList);
  inherited;
end;

Initialization
 GridServerProtocolManager := TGridServerProtocolManager.Create;
Finalization
 FreeAndNil(GridServerProtocolManager);

end.
