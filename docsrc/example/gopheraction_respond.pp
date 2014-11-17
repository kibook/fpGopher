type
  TAction1 = class(TGopherAction)
  public
    procedure Respond(Sender: TObject); override;
  end;

procedure TAction1.Respond(Sender: TObject);
begin
  WriteInfo('Hello world')
end;
