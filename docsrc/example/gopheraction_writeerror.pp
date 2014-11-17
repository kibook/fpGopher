procedure TAction1.Respond(Sender: TObject);
begin
  if FileExists(Params[1]) then
    WriteInfo('It exists')
  else
    WriteError('It doesn''t exist')
end;
