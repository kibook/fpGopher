unit GopherMoleAction;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GopherAction,
  GopherServer;

type
  TMoleAction = class(TGopherAction)
  private class var
    FMole: string;
  public
    procedure Respond(Sender: TObject); override;
    class property Mole: string read FMole write FMole;
  end;

implementation

uses
  Process;

procedure TMoleAction.Respond(Sender: TObject);
var
  Exec: TProcess;
  ExitStatus: Integer;
  Server: TGopherServer;
begin
  Server := Sender as TGopherServer;

  if Server.AllowMole then
  begin
    Exec := TProcess.Create(nil);
    Exec.Options := [poUsePipes, poWaitOnExit, poStdErrToOutput];
    Exec.Executable := FMole;

    with Exec.Environment do
    begin
      Values['DOCUMENT_ROOT'] := Server.RootDir;
      Values['QUERY_STRING'] := Query.DelimitedText;
      Values['REMOTE_ADDR'] := RemoteAddress;
      Values['REMOTE_HOST'] := Server.Host;
      Values['REMOTE_PORT'] := IntToStr(Server.Port);
      Values['PATH'] := GetCurrentDir;
      Values['SERVER_PORT'] := IntToStr(Server.Port);
      Values['SERVER_ADMIN'] := Server.AdminEmail;
      Values['SERVER_NAME'] := Server.Host;
      Values['SERVER_SOFTWARE'] := Server.Version
    end;

    Exec.Parameters.AddStrings(Params);

    try
      Exec.Execute;
      ExitStatus := Exec.ExitStatus
    except
      ExitStatus := 1
    end;

    if ExitStatus = 0 then
    begin
      IsMenu := False;
      ContentStream.CopyFrom(Exec.Output, Exec.Output.NumBytesAvailable)
    end else
    begin
      IsMenu := True;
      WriteError(sErrServer)
    end;

    Exec.Free
  end
end;

end.
