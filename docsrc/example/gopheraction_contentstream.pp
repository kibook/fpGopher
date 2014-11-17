procedure TAction1.Respond(Sender: TObject);
var
  F: TFileStream;
begin
  if FileExists(Params[1]) then
  begin
    F := TFileStream.Create(Params[1], fmOpenRead);
    ContentStream.CopyFrom(F, F.Size);
    F.Free
  end
end;
