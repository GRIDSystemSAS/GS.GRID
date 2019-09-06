//General common types (data)
unit gridlog.types;

interface

type
  TGridServerItem = class
  public
    ServerName : string;
    ServerDesc : string;
    ServerHost : string;
    ServerPort : integer;

    //app usage only, not persisted. (link beween gui elem and backe elem.)
    externalId : string;

    //Do not pass thought backend.
    user : string;
    pass : string;

    //communicate with its own GRID socket thread.
    ConnectedChan : string;
  end;


implementation

end.
