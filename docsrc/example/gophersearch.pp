uses
  GopherServer,
  GopherSearch;

var
  Server: TGopherServer;
begin
  Server := TGopherServer.Create('localhost');
  Server.RootDir := '/var/gopher';

  { gopher://localhost/search foo bar
    gopher://localhost/search?foo bar

    will both initiate a search for files containing "foo" and "bar"}
  TGopherSearch.Register('search');

  Server.StartAccepting
end.
