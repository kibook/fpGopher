var
  Client: TGopherClient;
begin
  Client := TGopherClient.Create;
  Client.Get('gopher://foobar.com');
  
  { List all menu item display strings }
  for i := 0 to Client.Contents.Count - 1 do
    WriteLn(Client.MenuItems[i].Text);

  Client.Free
end.
