procedure TAction1.Respond(Sender: TObject);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create('menu.gopher', fmOpenRead);
  ContentStream.CopyFrom(FileStream, FileStream.Size);
  FileStream.Free;
  UpdateGopherFile
end;
