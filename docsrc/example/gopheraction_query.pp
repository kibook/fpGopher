procedure TAction1.Respond(Sender: TObject);
begin
  { ex: gopher://server.com/action1?foo=bar }
  WriteInfo(Query.Values['foo']) // 'bar'
end;
