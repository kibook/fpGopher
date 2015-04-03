(* /// template.gopher file ///

  [*** Dynamic menu with template example ***]
  [You are: {RemoteAddress}]
  [Server time: {CurrentTime}]

*)

type
  TAction1 = class(TGopherAction)
  public
    procedure Respond(Sender: TObject); override;
    procedure ReplaceTag(Sender: TObject; const TagString: string;
      TagParams: TStringList; out ReplaceText: string);
  end;

{ ... }

procedure TAction1.Respond(Sender: TObject);
begin
  Template.FileName := 'template.gopher';
  Template.AllowTagParams := True;
  Template.OnReplaceTag := @ReplaceTag
end;

procedure TAction1.ReplaceTag(Sender: TObject; const TagString: string;
  TagParams: TStringList; out ReplaceText: string);
begin
  case TagString of
    'RemoteAddress': ReplaceText := RemoteAddress;
    'CurrentTime': ReplaceText := DateTimeToStr(Now)
  end
end;

{ /// The resulting menu file will look like this: ///

  *** Dynamic menu with template example
  You are: 127.0.0.1
  Server time: 01-01-01 00:00:00

}
