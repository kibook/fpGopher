var
  Server: TGopherServer;
begin
  Server := TGopherServer.Create('localhost');
  Server.RootDir := '/var/gopher';
  Server.AllowMole := True;
  Server.StartAccepting
end.

{ file: /var/gopher/demo.cgi

  #!/bin/bash

  # says "Hello" to the client's IP
  echo "Hello $REMOTE_ADDR!"

}
