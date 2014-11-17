unit GopherLog;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

const
  SLogFormat = ' [%s %s] %s' + LineEnding;

type
  TGopherLog = class
  private
    FFileName: string;
    FEnabled: Boolean;
    FStream: TFileStream;
    procedure SetFileName(Value: string);
    procedure Log(MessageType: string; Message: string);
  public
    constructor Create;
    procedure Info(Message: string);
    procedure Info(Message: string; Fmt: array of const);
    procedure Error(Message: string);
    procedure Error(Message: string; Fmt: array of const);
    property FileName: string read FFileName write SetFileName;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

procedure TGopherLog.SetFileName(Value: string);
begin
  FFileName := Value;

  if Assigned(FStream) then
  begin
    FStream.Free
  end;
  FStream := TFileStream.Create(FFileName, fmCreate);
  FStream.Seek(0, soFromEnd);

  FEnabled := True
end;

procedure TGopherLog.Log(MessageType, Message: string);
var
  Msg: string;
begin
  if FEnabled then
  begin
    Msg := Format(SLogFormat, [
      DateTimeToStr(Now),
      MessageType,
      Message
    ]);
    FStream.Write(Msg[1], Length(Msg) * SizeOf(Msg[1]))
  end
end;

constructor TGopherLog.Create;
begin
  FEnabled := False
end;

procedure TGopherLog.Info(Message: string);
begin
  Log('Info', Message)
end;

procedure TGopherLog.Info(Message: string; Fmt: array of const);
begin
  Info(Format(Message, Fmt))
end;

procedure TGopherLog.Error(Message: string);
begin
  Log('Error', Message)
end;

procedure TGopherLog.Error(Message: string; Fmt: array of const);
begin
  Error(Format(Message, Fmt))
end;

end.
