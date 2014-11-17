type
  TSearchSubDir = class(TGopherLocalSearch)
  public
    procedure Respond(Sender: TObject); override;
  end;

procedure TSearchSubDir.Respond(Sender: TObject);
begin
  SearchDir := 'subdir';
  Recursive := False;
  inherited
end;

var
  Server: TGopherServer;
begin
  { Search entire root dir and all subdirectories }
  TGopherLocalSearch.Register('search');

  { Search only this subdirectory }
  TSearchSubDir.Register('search-subdir');

  Server := TGopherServer.Create('localhost');
  Server.RootDir := '/var/gopher';

  Server.StartAccepting
end.
