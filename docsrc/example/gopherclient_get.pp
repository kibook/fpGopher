var
  Client: TGopherClient;
begin
  Client := TGopherClient.Create;

  { Set properties manually }
  Client.Host := 'foobar.com';
  Client.Port := 70;
  Client.Selector := '/file.txt';
  WriteLn(Client.Get);

  { Use a Gopher URI }
  WriteLn(Client.Get('gopher://foobar.com/file.txt'));
  WriteLn(Client.Get('foobar.com:7000/file.txt'));

  Client.Free
end.
