procedure TAction1.Respond(Sender: TObject);
begin
  { ex: gopher://server.com/action1/foo/bar }
  WriteInfo(Params[1]); // 'foo'
  WriteInfo(Params[2])  // 'bar'
end;
