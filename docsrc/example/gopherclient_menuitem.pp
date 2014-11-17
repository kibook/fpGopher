var
  Client: TGopherClient;
begin
  Client := TGopherClient.Create;
  Client.Get('gopher://foobar.com');
  
  { List all menu item display strings }
  for i := 0 to Client.Contents.Count do
    WriteLn(Client.MenuItems[i].Text);

  Client.Free
end.
