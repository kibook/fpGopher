uses
  GopherServer,
  GopherCaps;

{ ... }

var
  Server: TGopherServer;
begin
  Server := TGopherServer.Create('localhost');
  GopherCaps.Expire := 3600; { 1 hour }
  Server.StartAccepting
end.
