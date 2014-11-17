procedure TAction1.Respond(Sender: TObject);
begin
  Write('<html>');
  Write('<body>');
  Write('You are connected from: <b>' + RemoteAddress + '</b>');
  Write('</body>');
  Write('</html>')
end;
