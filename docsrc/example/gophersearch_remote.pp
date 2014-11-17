type
  TSearchServer = class(TGopherRemoteSearch)
  public
    procedure Respond(Sender: TObject); override;
  end;

procedure TSearchServer.Respond(Sender: TObject);
begin
  Host := 'foobar.com';
  inherited
end;

var
  Server: TGopherServer;
begin
  TSearchServer.Register('search-foobar');

  Server := TGopherServer.Create('localhost');
  Server.RootDir := '/var/gopher';

  Server.StartAccepting
end.
